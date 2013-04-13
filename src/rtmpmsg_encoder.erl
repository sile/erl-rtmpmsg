%%% @doc RTMP message encoder 
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
-module(rtmpmsg_encoder).
-include("../include/rtmpmsg.hrl").

%% Exported API
-export([new/0, encode/3]).

%% Exported Types
-export_type([encoder/0]).

%% Macro
-define(STATE, ?MODULE).

%% Record
-record(?STATE,
        {
          chunk_enc = rtmpmsg_chunk_encode:init() :: rtmpmsg_chunk_encode:state()
        }).

%%================================================================================
%% Types
%%================================================================================
-type encoder() :: #?STATE{}.

%%================================================================================
%% Exported API
%%================================================================================
%% @doc Return new encoder instance
-spec new() -> encoder().
new() -> #?STATE{}.

%% @doc Encode RTMP Message
-spec encode(encoder(), rtmpmsg:chunk_stream_id(), rtmpmsg:message()) -> {encoder(), EncodedData::iolist()}.
encode(Encoder, ChunkStreamId, Msg) ->
    ChunkEnc0  = Encoder#?STATE.chunk_enc,
    Chunk = rtmpmsg_message_encode:encode_to_chunk(ChunkStreamId, Msg),
    {ChunkEnc1, EncodedData} = rtmpmsg_chunk_encode:encode(ChunkEnc0, Chunk),
    ChunkEnc2 = case Msg#rtmpmsg.body of
               #rtmpmsg_set_chunk_size{size=Size} -> rtmpmsg_chunk_encode:set_chunk_size(ChunkEnc1, Size);
               _                                  -> ChunkEnc1
           end,
    {Encoder#?STATE{chunk_enc=ChunkEnc2}, EncodedData}.
