%%% @doc RTMP chunk encoding module
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
-module(rtmpmsg_chunk_encode).
-compile(inline).
-include("../include/internal/rtmpmsg_internal.hrl").

%% Exported API
-export([init/0, get_chunk_size/1, set_chunk_size/2, encode/2]).

%% Exported Types
-export_type([state/0]).

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
          base_fmt        = 0 :: format_id(),
          timestamp       = 0 :: rtmpmsg:message_timestamp(),
          timestamp_delta = 0 :: rtmpmsg:milliseconds(),
          msg_type_id     = 0 :: rtmpmsg:message_type_id(),
          msg_stream_id   = 0 :: rtmpmsg:message_stream_id(),
          msg_length      = 0 :: non_neg_integer()
        }).

%%================================================================================
%% Types
%%================================================================================
-type state() :: #?STATE{}.

-type format_id() :: 0 | 1 | 2 | 3.
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
    State#?STATE{chunk_size=Size}.

%% @doc Encode chunk
-spec encode(state(), rtmpmsg:chunk()) -> {NewState, EncodedData} when
      NewState    :: state(),
      EncodedData :: iolist().
encode(State, Chunk) ->
    {LastChunk, Fmt} = get_last_chunk(State, Chunk),

    ChunkBasicHeaderBytes = encode_chunk_basic_header(Fmt, Chunk#chunk.id),
    MessageHeaderBytes    = encode_message_header(Fmt, LastChunk),
    encode_message_payload(Chunk#chunk.payload,
                           save_last_chunk(State, Chunk#chunk.id, LastChunk),
                           Chunk#chunk.id,
                           [MessageHeaderBytes, ChunkBasicHeaderBytes]).

%%================================================================================
%% Internal Functions
%%================================================================================
-spec encode_chunk_basic_header(format_id(), rtmpmsg:chunk_stream_id()) -> binary()|byte().
encode_chunk_basic_header(_Fmt, ChunkId) when ChunkId < 2     -> error({too_small_chunk_stream_id, ChunkId});
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 64    -> (Fmt bsl 6) + ChunkId;
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 320   -> <<Fmt:2, 0:6, (ChunkId-64):08>>;
encode_chunk_basic_header( Fmt, ChunkId) when ChunkId < 65600 -> <<Fmt:2, 1:6, (ChunkId-64):16>>.

-spec encode_message_header(format_id(), last_chunk()) -> iolist().
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

-spec encode_message_payload(binary(), state(), rtmpmsg:chunk_stream_id(), iolist()) -> {state(), iolist()}.
encode_message_payload(<<Payload/binary>>, State, ChunkId, Acc) ->
    MaxChunkPayloadSize = State#?STATE.chunk_size,
    case Payload of
        <<ChunkPayload:MaxChunkPayloadSize/binary, Rest/binary>> when byte_size(Rest) > 0 ->
            encode_message_payload(Rest, State, ChunkId, [encode_chunk_basic_header(3, ChunkId), ChunkPayload | Acc]);
        _ ->
            {State, lists:reverse([Payload | Acc])}
    end.

-spec save_last_chunk(state(), rtmpmsg:chunk_stream_id(), last_chunk()) -> state().
save_last_chunk(State, ChunkId, LastChunk) ->
    State#?STATE{last_chunks = splay_tree:store(ChunkId, LastChunk, State#?STATE.last_chunks)}.

-spec get_last_chunk(state(), rtmpmsg:chunk()) -> {last_chunk(), format_id()}.
get_last_chunk(State, Chunk) ->
    MaybeLastChunk = splay_tree:get_value(Chunk#chunk.id, State#?STATE.last_chunks, undefined),
    expand_last_chunk(Chunk, byte_size(Chunk#chunk.payload), MaybeLastChunk).

-spec expand_last_chunk(rtmpmsg:chunk(), MessagePayloadLength, MaybeLastChunk) -> {last_chunk(), format_id()} when
      MessagePayloadLength :: non_neg_integer(),
      MaybeLastChunk       :: last_chunk() | undefined.
expand_last_chunk(Chunk, Len, undefined) ->
    {
      #last_chunk{base_fmt      = 0,
                  timestamp     = Chunk#chunk.timestamp,
                  msg_type_id   = Chunk#chunk.msg_type_id,
                  msg_stream_id = Chunk#chunk.msg_stream_id,
                  msg_length    = Len},
      0
    };
expand_last_chunk(#chunk{timestamp=Now}=Chunk, Len, #last_chunk{timestamp=Prev}) when Now < Prev ->
    {
      #last_chunk{base_fmt      = 0,
                  timestamp     = Chunk#chunk.timestamp,
                  msg_type_id   = Chunk#chunk.msg_type_id,
                  msg_stream_id = Chunk#chunk.msg_stream_id,
                  msg_length    = Len},
      0
    };
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Time}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Time, base_fmt=0}=LastChunk) ->
    {
      LastChunk, 
      3
    };
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Now}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Prev, timestamp_delta=Delta} = LastChunk) 
  when (Now-Prev) =:= Delta ->
    {
      LastChunk#last_chunk{timestamp=Now}, 
      3
    };
expand_last_chunk(#chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Now}, Len,
                  #last_chunk{msg_stream_id=Stream, msg_type_id=Type, msg_length=Len, timestamp=Prev} = LastChunk) ->
    {
      LastChunk#last_chunk{base_fmt        = 2,
                           timestamp       = Now,
                           timestamp_delta = Now - Prev},
      2
    };
expand_last_chunk(#chunk{msg_stream_id=Stream, timestamp=Now} = Chunk, Len,
                  #last_chunk{msg_stream_id=Stream, timestamp=Prev} = LastChunk) ->
    {
      LastChunk#last_chunk{base_fmt        = 1,
                           timestamp       = Now,
                           timestamp_delta = Now - Prev,
                           msg_type_id     = Chunk#chunk.msg_type_id,
                           msg_length      = Len},
      1
    };
expand_last_chunk(Chunk, Len, _LastChunk) ->
    {
      #last_chunk{base_fmt      = 0,
                  timestamp     = Chunk#chunk.timestamp,
                  msg_type_id   = Chunk#chunk.msg_type_id,
                  msg_stream_id = Chunk#chunk.msg_stream_id,
                  msg_length    = Len},
      0
    }.
