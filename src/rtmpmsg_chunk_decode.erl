%%% @doc RTMP chunk decoding module
%%% @private
%%% @end
%%%
%%%
%%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmpmsg_chunk_decode).
-compile(inline).
-include("../include/internal/rtmpmsg_internal.hrl").

%% Exported API
-export([init/0, get_chunk_size/1, set_chunk_size/2, decode/2]).

%% Exported Types
-export_type([state/0, decode_result/0]).

%% Macro
-define(STATE, ?MODULE).

%% Records
-record(?STATE,
        {
          chunk_size  = ?CHUNK_SIZE_DEFAULT :: rtmpmsg:chunk_size(),
          last_chunks = splay_tree:new()    :: splay_tree:tree()
        }).

-record(last_chunk,
        {
          timestamp       = 0  :: rtmpmsg:message_timestamp(),
          timestamp_delta = 0  :: rtmpmsg:milliseconds(),
          msg_type_id     = 0  :: rtmpmsg:message_type_id(),
          msg_stream_id   = 0  :: rtmpmsg:message_stream_id(),
          msg_length      = 0  :: non_neg_integer(),
          acc_payload_len = 0  :: non_neg_integer(),
          acc_payload     = [] :: iolist()
        }).

%%================================================================================
%% Types
%%================================================================================
-type state() :: #?STATE{}.
-type decode_result() :: {rtmpmsg:chunk(), state(), Unconsumed::binary()} | {partial, state(), Unconsumed::binary()}.

-type format_id() :: 0 | 1 | 2 | 3.
-type basic_header_decode_result() :: partial | {format_id(), rtmpmsg:chunk_stream_id(), Unconsumed::binary()}.

-type last_chunk() :: #last_chunk{}.

%%================================================================================
%% Exported API
%%================================================================================

%% @doc Return initial state
-spec init() -> state().
init() -> #?STATE{}.

%% @doc Return current chunk size
-spec get_chunk_size(state()) -> rtmpmsg:chunk_size().
get_chunk_size(State) -> State#?STATE.chunk_size.
    
%% @doc Set new chunk size
-spec set_chunk_size(state(), rtmpmsg:chunk_size()) -> state().
set_chunk_size(State, Size) when 1 =< Size andalso Size =< ?CHUNK_SIZE_MAX ->
    State#?STATE{chunk_size = Size}.

%% @doc Decode chunk
-spec decode(state(), binary()) -> decode_result().
decode(State, Bin) ->
    decode_chunk(decode_chunk_basic_header(Bin), State, Bin).

%%================================================================================
%% Internal Functions
%%================================================================================
-spec decode_chunk(basic_header_decode_result(), state(), binary()) -> decode_result().
decode_chunk(partial, State, Bin) ->
    {partial, State, Bin};
decode_chunk(Header, State, Bin) ->
    {Fmt, ChunkId, _} = Header,
    LastChunk = case {get_last_chunk(State, ChunkId), Fmt} of
                    {undefined, 0} -> #last_chunk{};
                    {undefined, _} -> error({first_chunk_format_id_must_be_0, ChunkId, Fmt});
                    {Chunk, _}     -> Chunk
                end,
    decode_next_chunk(Header, State, Bin, ChunkId, LastChunk).

-spec decode_next_chunk(basic_header_decode_result(), state(), binary(), rtmpmsg:chunk_stream_id(), last_chunk()) -> decode_result().
decode_next_chunk(partial, State, Bin, ChunkId, LastChunk) ->
    {partial, save_last_chunk(State, ChunkId, LastChunk), Bin};
decode_next_chunk({Fmt,ChunkId,Bin1}, State, Bin, ChunkId, LastChunk) ->
    case decode_message(Bin1, Fmt, LastChunk, State#?STATE.chunk_size) of
        partial                       -> {partial, save_last_chunk(State,ChunkId,LastChunk), Bin};
        {LastChunk1, Bin2, undefined} -> decode_next_chunk(decode_chunk_basic_header(Bin2), State, Bin2, ChunkId, LastChunk1);
        {LastChunk1, Bin2, Payload}   ->
            #last_chunk{timestamp=Timestamp, msg_type_id=TypeId, msg_stream_id=StreamId} = LastChunk1,
            Chunk = #chunk{id            = ChunkId,
                           msg_stream_id = StreamId,
                           msg_type_id   = TypeId,
                           timestamp     = Timestamp,
                           payload       = Payload},
            {Chunk, save_last_chunk(State,ChunkId,LastChunk1), Bin2}
    end;
decode_next_chunk(Header, State, Bin, ChunkId, LastChunk) ->
    decode_chunk(Header, save_last_chunk(State,ChunkId,LastChunk), Bin).

-spec get_last_chunk(state(), rtmpmsg:chunk_stream_id()) -> last_chunk() | undefined.
get_last_chunk(State, ChunkId) -> 
    splay_tree:get_value(ChunkId, State#?STATE.last_chunks, undefined).

-spec save_last_chunk(state(), rtmpmsg:chunk_stream_id(), last_chunk()) -> state().
save_last_chunk(State, ChunkId, LastChunk) -> 
    State#?STATE{last_chunks = splay_tree:store(ChunkId, LastChunk, State#?STATE.last_chunks)}.

-spec decode_chunk_basic_header(binary()) -> basic_header_decode_result().
decode_chunk_basic_header(<<Fmt:2, 0:6, ChunkId:08, Bin/binary>>) -> {Fmt, ChunkId+64, Bin};
decode_chunk_basic_header(<<Fmt:2, 1:6, ChunkId:16, Bin/binary>>) -> {Fmt, ChunkId+64, Bin};
decode_chunk_basic_header(<<Fmt:2,      ChunkId:06, Bin/binary>>) when ChunkId > 1 -> {Fmt, ChunkId, Bin};
decode_chunk_basic_header(<<_/binary>>)                           -> partial.

-spec decode_message(binary(), format_id(), last_chunk(), rtmpmsg:chunk_size()) -> partial | {last_chunk(), UnconsumedBin, Payload} when
      UnconsumedBin :: binary(),
      Payload       :: binary() | undefined.
decode_message(Bin, Fmt, LastChunk, ChunkSize) ->
    case decode_message_header(Bin, Fmt, LastChunk) of
        partial            -> partial;
        {LastChunk1, Bin1} -> decode_message_payload(Bin1, LastChunk1, ChunkSize)
    end.

-spec decode_message_payload(binary(), last_chunk(), rtmpmsg:chunk_size()) -> partial | {last_chunk(), UnconsumedBin, Payload} when
      UnconsumedBin :: binary(),
      Payload       :: binary() | undefined.
decode_message_payload(Bin, LastChunk, MaxPayloadSize) ->
    #last_chunk{msg_length=MsgLen, acc_payload=AccPayload, acc_payload_len=AccLen} = LastChunk,
    ChunkPayloadSize = min(MsgLen - AccLen, MaxPayloadSize),
    case Bin of
        <<Payload:ChunkPayloadSize/binary, Bin1/binary>> ->
            AccPayload1 = [Payload|AccPayload],
            case AccLen + ChunkPayloadSize of
                MsgLen ->
                    Timestamp  = LastChunk#last_chunk.timestamp + LastChunk#last_chunk.timestamp_delta,
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

-spec decode_message_header(binary(), format_id(), last_chunk()) -> partial | {last_chunk(), Unconsumed::binary()}.
decode_message_header(<<Bin/binary>>, 0, _Last) ->
    case Bin of
        <<16#FFFFFF:24, Len:24, Type:8, Stream:32/little, Time:32, Rest/binary>> ->
            {#last_chunk{timestamp=Time, msg_type_id=Type, msg_stream_id=Stream, msg_length=Len}, Rest};
        <<Time:24, Len:24, Type:8, Stream:32/little, Rest/binary>> ->
            {#last_chunk{timestamp=Time, msg_type_id=Type, msg_stream_id=Stream, msg_length=Len}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, 1, Last) ->
    case Bin of
        <<16#FFFFFF:24, Len:24, Type:8, TimeDelta:32, Rest/binary>> ->
            {Last#last_chunk{timestamp_delta=TimeDelta, msg_type_id=Type, msg_length=Len}, Rest};
        <<TimeDelta:24, Len:24, Type:8, Rest/binary>> ->
            {Last#last_chunk{timestamp_delta=TimeDelta, msg_type_id=Type, msg_length=Len}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, 2, Last) ->
    case Bin of
        <<16#FFFFFF:24, TimeDelta:32, Rest/binary>> ->
            {Last#last_chunk{timestamp_delta=TimeDelta}, Rest};
        <<TimeDelta:24, Rest/binary>> ->
            {Last#last_chunk{timestamp_delta=TimeDelta}, Rest};
        _ ->
            partial
    end;
decode_message_header(<<Bin/binary>>, 3, Last) ->
    {Last, Bin}.
