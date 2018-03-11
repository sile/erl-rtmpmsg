%%% @doc Benchmark Module
%%% @end
%%%
%%%
%%% Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>
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
-module(rtmpmsg_bench).

-include("internal/rtmpmsg_internal.hrl").

%% Exported API
-export([bench/2]).

%%================================================================================
%% Exported API
%%================================================================================

%% @doc Do Benchmark
%%
%% ```
%% > {ok, Input} = file:read_file("data/flat.chunks").
%% > rtmpmsg_bench:bench(100, Input).
%% '''
-spec bench(non_neg_integer(), binary()) -> [{chunk_decode_time, non_neg_integer()} |
                                             {chunk_encode_time, non_neg_integer()} |
                                             {message_decode_time, non_neg_integer()} |
                                             {message_encode_time, non_neg_integer()} |
                                             {input_byte_size, non_neg_integer()} |
                                             {chunk_count, non_neg_integer()}].
bench(Times, Input) ->
    Chunks = rtmpmsg_util:decode_chunks(Input),
    {ChunkDecodeTime, _} = timer:tc(fun () -> chunk_decode_loop(Times, Input, Input, rtmpmsg_chunk_decode:init()) end),
    {ChunkEncodeTime, _} = timer:tc(fun () -> chunk_encode_loop(Times, Chunks, Chunks, rtmpmsg_chunk_encode:init()) end),

    Messages = rtmpmsg_util:decode_messages(Chunks),
    {MessageDecodeTime, _} = timer:tc(fun () -> message_decode_loop(Times, Chunks, Chunks) end),
    {MessageEncodeTime, _} = timer:tc(fun () -> message_encode_loop(Times, Messages, Messages) end),

    [{chunk_decode_time, ChunkDecodeTime},
     {chunk_encode_time, ChunkEncodeTime},
     {message_decode_time, MessageDecodeTime},
     {message_encode_time, MessageEncodeTime},
     {input_byte_size, byte_size(Input)},
     {chunk_count, length(Chunks)}].

%%================================================================================
%% Internal Functions
%%================================================================================
-spec chunk_decode_loop(non_neg_integer(), binary(), binary(), rtmpmsg_chunk_decode:state()) -> ok.
chunk_decode_loop(0, _, _, _) ->
    ok;
chunk_decode_loop(Times, FullInput, Input, Decoder0) ->
    case rtmpmsg_chunk_decode:decode(Decoder0, Input) of
        {partial, _, _}         -> chunk_decode_loop(Times - 1, FullInput, FullInput, rtmpmsg_chunk_decode:init());
        {Chunk, Decoder1, Next} ->
            Decoder2 =
                case Chunk of
                    #chunk{msg_type_id = ?TYPE_SET_CHUNK_SIZE, payload = <<Size:32>>} ->
                        rtmpmsg_chunk_decode:set_chunk_size(Decoder1, Size);
                    _ -> Decoder1
                end,
            chunk_decode_loop(Times, FullInput, Next, Decoder2)
    end.

-spec chunk_encode_loop(non_neg_integer(), [#chunk{}], [#chunk{}], rtmpmsg_chunk_encode:state()) -> ok.
chunk_encode_loop(0, _, _, _) ->
    ok;
chunk_encode_loop(Times, FullInput, [], Encoder) ->
    chunk_encode_loop(Times - 1, FullInput, FullInput, Encoder);
chunk_encode_loop(Times, FullInput, [Chunk | Chunks], Encoder0) ->
    {Encoder1, _} = rtmpmsg_chunk_encode:encode(Encoder0, Chunk),
    Encoder2 =
        case Chunk of
            #chunk{msg_type_id = ?TYPE_SET_CHUNK_SIZE, payload = <<Size:32>>} ->
                rtmpmsg_chunk_encode:set_chunk_size(Encoder1, Size);
            _ -> Encoder1
        end,
    chunk_encode_loop(Times, FullInput, Chunks, Encoder2).

-spec message_decode_loop(non_neg_integer(), [#chunk{}], [#chunk{}]) -> ok.
message_decode_loop(0, _, _) ->
    ok;
message_decode_loop(Times, FullInput, []) ->
    message_decode_loop(Times - 1, FullInput, FullInput);
message_decode_loop(Times, FullChunks, [Chunk | Chunks]) ->
    _ = rtmpmsg_message_decode:decode_chunk(Chunk),
    message_decode_loop(Times, FullChunks, Chunks).

-spec message_encode_loop(non_neg_integer(), [rtmpmsg:message()], [rtmpmsg:message()]) -> ok.
message_encode_loop(0, _, _) ->
    ok;
message_encode_loop(Times, FullInput, []) ->
    message_encode_loop(Times - 1, FullInput, FullInput);
message_encode_loop(Times, FullInput, [Message | Messages]) ->
    ChunkStreamId = 2, % dummy value
    _ = rtmpmsg_message_encode:encode_to_chunk(ChunkStreamId, Message),
    message_encode_loop(Times, FullInput, Messages).
