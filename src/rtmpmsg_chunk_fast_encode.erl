%%% @doc RTMP chunk encoding module (faster encode speed, larger size overhead)
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
-module(rtmpmsg_chunk_fast_encode).
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
          chunk_size  = ?CHUNK_SIZE_DEFAULT :: rtmpmsg:chunk_size()
        }).

%%================================================================================
%% Types
%%================================================================================
-type state() :: #?STATE{}.

-type format_id() :: 0 | 3.

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
    ChunkBasicHeaderBytes = encode_chunk_basic_header(0, Chunk#chunk.id),
    MessageHeaderBytes    = encode_message_header(byte_size(Chunk#chunk.payload), Chunk),
    encode_message_payload(Chunk#chunk.payload,
                           State,
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

-spec encode_message_header(non_neg_integer(), #chunk{}) -> binary().
encode_message_header(Len, #chunk{msg_stream_id=Stream, msg_type_id=Type, timestamp=Time}) ->
    if 
        Time < 16#FFFFFF -> <<     Time:24, Len:24, Type:8, Stream:32/little>>;
        true             -> <<16#FFFFFF:24, Len:24, Type:8, Stream:32/little, Time:32>>
    end.

-spec encode_message_payload(binary(), state(), rtmpmsg:chunk_stream_id(), iolist()) -> {state(), iolist()}.
encode_message_payload(<<Payload/binary>>, State, ChunkId, Acc) ->
    MaxChunkPayloadSize = State#?STATE.chunk_size,
    case Payload of
        <<ChunkPayload:MaxChunkPayloadSize/binary, Rest/binary>> when byte_size(Rest) > 0 ->
            encode_message_payload(Rest, State, ChunkId, [encode_chunk_basic_header(3, ChunkId), ChunkPayload | Acc]);
        _ ->
            {State, lists:reverse([Payload | Acc])}
    end.
