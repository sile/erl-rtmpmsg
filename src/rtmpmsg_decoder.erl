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
-compile(inline).
-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

%% Exported API
-export([new/0, decode/2, decode_all/2]).

%% Exported Types
-export_type([decoder/0]).

%% Macro
-define(STATE, ?MODULE).

%% Record
-record(?STATE,
        {
          chunk_dec  = rtmpmsg_chunk_decode:init() :: rtmpmsg_chunk_decode:state(),
          unconsumed = <<"">>                      :: binary()
        }).

%%================================================================================
%% Types
%%================================================================================
-opaque decoder() :: #?STATE{}.

%%================================================================================
%% Exported API
%%================================================================================

%% @doc Return new decoder instance
-spec new() -> decoder().
new() -> #?STATE{}.

%% @doc Decode RTMP Message
%%
%% If decoded message is #rtmpmsg_set_chunk_size{} or #rtmpmsg_abort{}, it will be automatically handled in this function.
-spec decode(decoder(), binary()) -> {ok, decoder(), rtmpmsg:message(), RestBin::binary()} | {partial, decoder()}.
decode(Decoder, <<Bin/binary>>) ->
    #?STATE{chunk_dec = ChunkDec, unconsumed = UnconsumedBin} = Decoder,
    case decode_impl(<<UnconsumedBin/binary, Bin/binary>>, ChunkDec) of
        {partial, ChunkDec1, Bin1} -> {partial, #?STATE{chunk_dec = ChunkDec1, unconsumed = Bin1}};
        {Msg, ChunkDec1, Bin1}     -> {ok, #?STATE{chunk_dec = ChunkDec1, unconsumed = <<"">>}, Msg, Bin1}
    end.

%% @doc Decode RTMP Messages
-spec decode_all(decoder(), binary()) -> {decoder(), [rtmpmsg:message()]}.
decode_all(Decoder, <<Bin/binary>>) ->
    #?STATE{chunk_dec = ChunkDec, unconsumed = UnconsumedBin} = Decoder,
    {Msgs, ChunkDec1, Bin1} = decode_all_impl(<<UnconsumedBin/binary, Bin/binary>>, ChunkDec, []),
    {#?STATE{chunk_dec = ChunkDec1, unconsumed = Bin1}, Msgs}.
                        
%%================================================================================
%% Internal Fuctions
%%================================================================================
-spec decode_impl(binary(), rtmpmsg_chunk_decode:state()) -> {rtmpmsg:message() | partial, rtmpmsg_chunk_decode:state(), binary()}.
decode_impl(<<Bin/binary>>, ChunkDec) ->
    case rtmpmsg_chunk_decode:decode(ChunkDec, Bin) of
        {partial, ChunkDec1, Bin1} -> {partial, ChunkDec1, Bin1};
        {Chunk, ChunkDec1, Bin1}   ->
            Msg = rtmpmsg_message_decode:decode_chunk(Chunk),
            ChunkDec2 = case Msg#rtmpmsg.body of
                            #rtmpmsg_set_chunk_size{size=Size} -> rtmpmsg_chunk_decode:set_chunk_size(ChunkDec1, Size);
                            #rtmpmsg_abort{chunk_stream_id=Id} -> rtmpmsg_chunk_decode:reset(ChunkDec1, Id);
                            _                                  -> ChunkDec1
                        end,
            {Msg, ChunkDec2, Bin1}
    end.

-spec decode_all_impl(binary(), rtmpmsg_chunk_decode:state(), [rtmpmsg:message()]) -> {[rtmpmsg:message()], rtmpmsg_chunk_decode:state(), binary()}.
decode_all_impl(<<Bin/binary>>, ChunkDec, Acc) ->
    case decode_impl(Bin, ChunkDec) of
        {partial, ChunkDec1, Bin1} -> {lists:reverse(Acc), ChunkDec1, Bin1};
        {Message, ChunkDec1, Bin1} -> decode_all_impl(Bin1, ChunkDec1, [Message | Acc])
    end.
