-module(rtmpmsg_chunk_encode).

-export([init/0, get_chunk_size/1, set_chunk_size/2, encode/2]).

-include("../include/internal/rtmpmsg_internal.hrl").

-define(STATE, ?MODULE).

-record(?STATE,
        {
          chunk_size  = ?CHUNK_SIZE_DEFAULT :: rtmpmsg:chunk_size(),
          last_chunks = splay_tree:new()    :: splay_tree:tree()
        }).

-record(last_chunk,
        {
          base_fmt        = 0 :: chunk_format_id(),
          timestamp       = 0 :: non_neg_integer(),
          timestamp_delta = 0 :: non_neg_integer(),
          msg_type_id     = 0 :: non_neg_integer(),
          msg_stream_id   = 0 :: non_neg_integer(),
          msg_length      = 0 :: non_neg_integer()
        }).

init() ->
    #?STATE{}.

get_chunk_size(State) ->
    State#?STATE.chunk_size.

set_chunk_size(State, Size) ->
    State#?STATE{chunk_size=Size}.

encode(State, Chunk) ->
    {LastChunk, Fmt} = get_last_chunk(State, Chunk),

    ChunkBasicHeaderBytes = encode_chunk_basic_header(Fmt, Chunk#chunk.id),
    MessageHeaderBytes    = encode_message_header(Fmt, LastChunk),
    encode_message_payload(Chunk#chunk.payload,
                           save_last_chunk(State, Chunk#chunk.id, LastChunk),
                           Chunk#chunk.id,
                           [MessageHeaderBytes, ChunkBasicHeaderBytes]).

encode_chunk_basic_header(_Fmt, ChunkId) when ChunkId < 2     -> error({too_small_chunk_stream_id, ChunkId});
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 64    -> (Fmt bsl 6) + ChunkId;      % NOTE: 最適化
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 320   -> <<Fmt:2, 0:6, (ChunkId-64):08>>;
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 65600 -> <<Fmt:2, 1:6, (ChunkId-64):16>>.

encode_message_header(0, #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Time}) ->
    if 
        Time < 16#FFFFFF -> <<     Time:24, Len:24, Type:8, Stream:32/little>>;
        true             -> <<16#FFFFFF:24, Len:24, Type:8, Stream:32/little, Time:32>>
    end;
encode_message_header(1, #last_chunk{msg_type_id=Type, msg_length=Len, timestamp_delta=Delta}) ->
    if
        Delta < 16#FFFFFF -> <<    Delta:24, Len:24, Type:8>>;
        true              -> <<16#FFFFFF:24, Len:24, Type:8, Delta:32>>
    end;
encode_message_header(2, #last_chunk{timestamp_delta=Delta}) ->
    if
        Delta < 16#FFFFFF -> <<    Delta:24>>;
        true              -> <<16#FFFFFF:24, Delta:32>>
    end;
encode_message_header(3, _LastChunk) -> 
    [].

encode_message_payload(<<Payload/binary>>, State, ChunkId, Acc) ->
    MaxChunkPayloadSize = State#?STATE.chunk_size,
    case Payload of
        <<ChunkPayload:MaxChunkPayloadSize/binary, Rest/binary>> when byte_size(Rest) > 0 ->
            encode_message_payload(Rest, State, ChunkId, [encode_chunk_basic_header(3, ChunkId), ChunkPayload | Acc]);
        _ ->
            {State, lists:reverse([Payload | Acc])}
    end.

save_last_chunk(State, ChunkId, LastChunk) ->
    State#?STATE{last_chunks = splay_tree:store(ChunkId, LastChunk, State#?STATE.last_chunks)}.

get_last_chunk(State, Chunk) ->
    expand_last_chunk(Chunk, byte_size(Chunk#chunk.payload), splay_tree:get_value(Chunk#chunk.id, State#?STATE.last_chunks, undefined)).

expand_last_chunk(Chunk, Len, undefined) ->
    {#last_chunk{base_fmt      = 0,
                 timestamp     = Chunk#chunk.timestamp,
                 msg_type_id   = Chunk#chunk.msg_type_id,
                 msg_stream_id = Chunk#chunk.msg_stream_id,
                 msg_length    = Len},
     0};
expand_last_chunk(#chunk{timestamp=Now}=Chunk, Len, #last_chunk{timestamp=Prev}) when Now < Prev ->
    {#last_chunk{base_fmt      = 0,
                 timestamp     = Chunk#chunk.timestamp,
                 msg_type_id   = Chunk#chunk.msg_type_id,
                 msg_stream_id = Chunk#chunk.msg_stream_id,
                 msg_length    = Len},
     0};
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Time}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Time, base_fmt=0}=LastChunk) ->
    {LastChunk,
     3};
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Now}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Prev, timestamp_delta=Delta} = LastChunk) 
  when (Now-Prev) =:= Delta ->
    {LastChunk#last_chunk{timestamp=Now},
     3};
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Now}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Prev} = LastChunk) ->
    {LastChunk#last_chunk{base_fmt        = 2,
                          timestamp       = Now,
                          timestamp_delta = Now - Prev},
     2};
expand_last_chunk(#chunk{msg_stream_id=Stream, timestamp=Now} = Chunk, Len,
                  #last_chunk{msg_stream_id=Stream, timestamp=Prev} = LastChunk) ->
    {LastChunk#last_chunk{base_fmt        = 1,
                          timestamp       = Now,
                          timestamp_delta = Now - Prev,
                          msg_type_id     = Chunk#chunk.msg_type_id,
                          msg_length      = Len},
     1};
expand_last_chunk(Chunk, Len, _LastChunk) ->
    {#last_chunk{base_fmt      = 0,
                 timestamp     = Chunk#chunk.timestamp,
                 msg_type_id   = Chunk#chunk.msg_type_id,
                 msg_stream_id = Chunk#chunk.msg_stream_id,
                 msg_length    = Len},
     0}.
