%%% @doc RTMP message decoder 
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
-module(rtmpmsg_decoder).
-include("../include/rtmpmsg.hrl").

%% Exported API
-export([new/0, decode/2]).

%% Exported Types
-export_type([decoder/0]).

%% Macro
-define(STATE, ?MODULE).

%% Record
-record(?STATE,
        {
          chunk_dec = rtmpmsg_chunk_decode:init() :: rtmpmsg_chunk_decode:state()
        }).

%%================================================================================
%% Types
%%================================================================================
-type decoder() :: #?STATE{}.

%%================================================================================
%% Exported API
%%================================================================================

%% @doc Return new decoder
-spec new() -> decoder().
new() -> #?STATE{}.

%% @doc Decode RTMP Message
-spec decode(decoder(), binary()) -> {ok, decoder(), rtmpmsg:message(), UnconsumedBin} | {partial, decoder(), UnconsumedBin} when
      UnconsumedBin :: binary().
decode(Decoder, Bin) ->
    case rtmpmsg_chunk_decode:decode(Decoder#?STATE.chunk_dec, Bin) of
        {partial, ChunkDec0, Bin1} -> 
            {partial, Decoder#?STATE{chunk_dec=ChunkDec0}, Bin1};
        {Chunk, ChunkDec0, Bin1} ->
            Msg = rtmpmsg_message_decode:decode_chunk(Chunk),
            ChunkDec1 = case Msg#rtmpmsg.body of
                            #rtmpmsg_set_chunk_size{size=Size} ->
                                rtmpmsg_chunk_decode:set_chunk_size(ChunkDec0, Size);
                            _ ->
                                ChunkDec0
                        end,
            {ok, Decoder#?STATE{chunk_dec=ChunkDec1}, Msg, Bin1}
    end.
