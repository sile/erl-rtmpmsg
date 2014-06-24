%%% @doc Utility Functions (for debug purpose)
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
-module(rtmpmsg_util).

-include("rtmpmsg.hrl").
-include("internal/rtmpmsg_internal.hrl").

%% Exported API
-export([decode_chunks/1, decode_chunks/2,
         encode_chunks/1, encode_chunks/2,
         decode_messages/1,
         encode_messages/1]).

%%================================================================================
%% Exported API
%%================================================================================
%% @equiv decode_chunks(Input, rtmpmsg_chunk_decode:init())
-spec decode_chunks(binary()) -> [#chunk{}].
decode_chunks(Input) ->
    decode_chunks(Input, rtmpmsg_chunk_decode:init()).

%% @doc Decode RTMP Chunks from Binary
-spec decode_chunks(binary(), rtmpmsg_chunk_decode:state()) -> [#chunk{}].
decode_chunks(Input, Decoder0) ->
    case rtmpmsg_chunk_decode:decode(Decoder0, Input) of
        {partial, _, _}         -> [];
        {Chunk, Decoder1, Next} ->
            Decoder2 =
                case Chunk of
                    #chunk{msg_type_id = ?TYPE_SET_CHUNK_SIZE, payload = <<Size:32>>} ->
                        rtmpmsg_chunk_decode:set_chunk_size(Decoder1, Size);
                    _ -> Decoder1
                end,
            [Chunk | decode_chunks(Next, Decoder2)]
    end.

%% @equiv encode_chunks(Input, rtmpmsg_chunk_encode:init())
-spec encode_chunks([#chunk{}]) -> binary().
encode_chunks(Chunks) ->
    encode_chunks(Chunks, rtmpmsg_chunk_encode:init()).

%% @doc Encode RTMP Chunks to Binary
-spec encode_chunks([#chunk{}], rtmpmsg_chunk_encode:state()) -> binary().
encode_chunks(Chunks, Encoder0) ->
    Result =
        lists:foldl(
          fun (Chunk, {AccEncoder0, AccBin}) ->
                  {AccEncoder1, Bin} = rtmpmsg_chunk_encode:encode(AccEncoder0, Chunk),
                  AccEncoder2 =
                      case Chunk of
                          #chunk{msg_type_id = ?TYPE_SET_CHUNK_SIZE, payload = <<Size:32>>} ->
                              rtmpmsg_chunk_encode:set_chunk_size(AccEncoder1, Size);
                          _ -> AccEncoder1
                      end,
                  {AccEncoder2, <<AccBin/binary, (list_to_binary(Bin))/binary>>}
          end,
          {Encoder0, <<"">>},
          Chunks),
    element(2, Result).

%% @doc Decode RTMP Messages from Chunks
-spec decode_messages([#chunk{}]) -> [rtmpmsg:message()].
decode_messages([])           -> [];
decode_messages([C | Chunks]) ->
    Message = rtmpmsg_message_decode:decode_chunk(C),
    [Message | decode_messages(Chunks)].

%% @doc Encode RTMP Messages to Chunks
-spec encode_messages([rtmpmsg:message()]) -> [#chunk{}].
encode_messages(Messages) ->
    [begin
         ChunkStreamId =
             case Messages of
                 #rtmpmsg{type_id = Type} when Type =< 6 -> 2; % protocol control message
                 _                                       -> 3
             end,
         rtmpmsg_message_encode:encode_to_chunk(ChunkStreamId, Message)
     end || Message <- Messages].
