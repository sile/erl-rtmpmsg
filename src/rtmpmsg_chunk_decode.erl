-module(rtmpmsg_chunk_decode).
%% -compile(inline).

-export([init/0, get_chunk_size/1, set_chunk_size/2, decode/2]).

-include("../include/internal/rtmpmsg_internal.hrl").

-define(STATE, ?MODULE).

-record(?STATE,
        {
          chunk_size  = ?CHUNK_SIZE_DEFAULT :: rtmpmsg:chunk_size(),
          last_chunks = splay_tree:new()    :: splay_tree:tree()
        }).

-record(last_chunk,
        {
          timestamp       = 0  :: non_neg_integer(),
          timestamp_delta = 0  :: non_neg_integer(),
          msg_type_id     = 0  :: non_neg_integer(),
          msg_stream_id   = 0  :: non_neg_integer(),
          msg_length      = 0  :: non_neg_integer(),
          acc_payload_len = 0  :: non_neg_integer(),
          acc_payload     = [] :: iolist()
        }).

init() ->
    #?STATE{}.

get_chunk_size(State) ->
    State#?STATE.chunk_size.

set_chunk_size(State, Size) ->
    State#?STATE{chunk_size = Size}.

decode(State, Bin) ->
    {State1, Bin1, Chunk} = decode_chunk(decode_chunk_basic_header(Bin), State, Bin),
    {Chunk, State1, Bin1}.

decode_chunk(partial, State, Bin) ->
    {State, Bin, partial};
decode_chunk(Header, State, Bin) ->
    {Fmt, ChunkId, _} = Header,
    LastChunk = case {get_last_chunk(State, ChunkId), Fmt} of
                    {undefined, ?CHUNK_FMT_0} -> #last_chunk{};
                    {undefined, _}            -> error({first_chunk_format_id_must_be_0, ChunkId, Fmt});
                    {Chunk, _}                -> Chunk
                end,
    decode_next_chunk(Header, State, Bin, ChunkId, LastChunk).

decode_next_chunk(partial, State, Bin, ChunkId, LastChunk) ->
    {save_last_chunk(State, ChunkId, LastChunk), Bin, partial};
decode_next_chunk({Fmt,ChunkId,Bin1}, State, Bin, ChunkId, LastChunk) ->
    case decode_message(Bin1, Fmt, LastChunk, State#?STATE.chunk_size) of
        partial                       -> {save_last_chunk(State,ChunkId,LastChunk), Bin, partial};
        {LastChunk1, Bin2, undefined} -> decode_next_chunk(decode_chunk_basic_header(Bin2), State, Bin2, ChunkId, LastChunk1);
        {LastChunk1, Bin2, Payload}   ->
            #last_chunk{timestamp=Timestamp, msg_type_id=TypeId, msg_stream_id=StreamId} = LastChunk1,
            Chunk = #chunk{
              id = ChunkId,
              msg_stream_id = StreamId,
              msg_type_id = TypeId,
              timestamp = Timestamp,
              payload = Payload
             },
            {save_last_chunk(State,ChunkId,LastChunk1), Bin2, Chunk}
    end;
decode_next_chunk(Header, State, Bin, ChunkId, LastChunk) ->
    decode_chunk(Header, save_last_chunk(State,ChunkId,LastChunk), Bin).

get_last_chunk(State, ChunkId) ->
    splay_tree:get_value(ChunkId, State#?STATE.last_chunks, undefined).

save_last_chunk(State, ChunkId, LastChunk) ->
    State#?STATE{last_chunks = splay_tree:store(ChunkId, LastChunk, State#?STATE.last_chunks)}.

decode_chunk_basic_header(<<Fmt:2, 0:6, ChunkId:08, Bin/binary>>) -> {Fmt, ChunkId+64, Bin};
decode_chunk_basic_header(<<Fmt:2, 1:6, ChunkId:16, Bin/binary>>) -> {Fmt, ChunkId+64, Bin};
decode_chunk_basic_header(<<Fmt:2,      ChunkId:06, Bin/binary>>) when ChunkId > 1 -> {Fmt, ChunkId, Bin};
decode_chunk_basic_header(<<_/binary>>)                           -> partial.

decode_message(Bin, Fmt, LastChunk, ChunkSize) ->
    case decode_message_header(Bin, Fmt, LastChunk) of
        partial            ->
            partial;
                {LastChunk1, Bin1} -> decode_message_payload(Bin1, LastChunk1, ChunkSize)
    end.

decode_message_payload(Bin, LastChunk, MaxPayloadSize) ->
    #last_chunk{msg_length=MsgLen, acc_payload=AccPayload, acc_payload_len=AccLen} = LastChunk,
    ChunkPayloadSize = min(MsgLen - AccLen, MaxPayloadSize),
    case Bin of
        <<Payload:ChunkPayloadSize/binary, Bin1/binary>> ->
            AccPayload1 = [Payload|AccPayload],
            case AccLen + ChunkPayloadSize of
                MsgLen ->
                    Timestamp = LastChunk#last_chunk.timestamp + LastChunk#last_chunk.timestamp_delta,
                    LastChunk1 = LastChunk#last_chunk{timestamp=Timestamp, acc_payload=[], acc_payload_len=0},
                    MsgPayload = list_to_binary(lists:reverse(AccPayload1)),
                    {LastChunk1, Bin1, MsgPayload};
                Len ->
                    LastChunk1 = LastChunk#last_chunk{acc_payload=AccPayload1, acc_payload_len=Len},
                    {LastChunk1, Bin1, undefined}
            end;
        _ ->
            partial
    end.

decode_message_header(<<Bin/binary>>, ?CHUNK_FMT_0, _Prev) ->
    case Bin of
        <<16#FFFFFF:24, Len:24, Type:8, Stream:32/little, Time:32, Rest/binary>> ->
            {#last_chunk{timestamp=Time, msg_type_id=Type, msg_stream_id=Stream, msg_length=Len}, Rest};
        <<Time:24, Len:24, Type:8, Stream:32/little, Rest/binary>> ->
            {#last_chunk{timestamp=Time, msg_type_id=Type, msg_stream_id=Stream, msg_length=Len}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, ?CHUNK_FMT_1, Prev) ->
    case Bin of
        <<16#FFFFFF:24, Len:24, Type:8, TimeDelta:32, Rest/binary>> ->
            {Prev#last_chunk{timestamp_delta=TimeDelta, msg_type_id=Type, msg_length=Len}, Rest};
        <<TimeDelta:24, Len:24, Type:8, Rest/binary>> ->
            {Prev#last_chunk{timestamp_delta=TimeDelta, msg_type_id=Type, msg_length=Len}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, ?CHUNK_FMT_2, Prev) ->
    case Bin of
        <<16#FFFFFF:24, TimeDelta:32, Rest/binary>> ->
            {Prev#last_chunk{timestamp_delta=TimeDelta}, Rest};
        <<TimeDelta:24, Rest/binary>> ->
            {Prev#last_chunk{timestamp_delta=TimeDelta}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, ?CHUNK_FMT_3, Prev) ->
    %% TODO: ここでのextended-timestampの扱いは要確認 (その他の境界条件も)
    {Prev, Bin}.
