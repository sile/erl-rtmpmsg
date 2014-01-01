%%% @doc RTMP message encoding module 
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
-module(rtmpmsg_message_encode).
-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

%% Exported API
-export([encode_to_chunk/2]).

%%================================================================================
%% Exported API
%%================================================================================
%% @doc Encode RTMP message to RTMP chunk
-spec encode_to_chunk(rtmpmsg:chunk_stream_id(), rtmpmsg:message()) -> rtmpmsg:chunk().
encode_to_chunk(ChunkStreamId, Msg) ->
    #chunk
    {
      id            = ChunkStreamId,
      timestamp     = Msg#rtmpmsg.timestamp,
      msg_type_id   = Msg#rtmpmsg.type_id,
      msg_stream_id = Msg#rtmpmsg.stream_id,
      payload       = encode_body(Msg#rtmpmsg.body)
    }.

%%================================================================================
%% Internal Functions
%%================================================================================
-spec encode_body(rtmpmsg:message_body()) -> binary().
encode_body(#rtmpmsg_set_chunk_size{size=Size}) -> <<Size:32>>;
encode_body(#rtmpmsg_abort{chunk_stream_id=Id}) -> <<Id:32>>;
encode_body(#rtmpmsg_ack{sequence_number=Num})  -> <<Num:32>>;
encode_body(#rtmpmsg_win_ack_size{size=Size})   -> <<Size:32>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=hard})    -> <<Size:32, 0>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=soft})    -> <<Size:32, 1>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=dynamic}) -> <<Size:32, 2>>;
encode_body(#rtmpmsg_user_control{event=Event}) -> encode_event(Event);
encode_body(#rtmpmsg_audio{data=Audio})    -> Audio;
encode_body(#rtmpmsg_video{data=Video})    -> Video;
encode_body(#rtmpmsg_command{}=Body)       -> encode_command(Body);
encode_body(#rtmpmsg_data{}=Body)          -> encode_data(Body);
encode_body(#rtmpmsg_aggregate{}=Body)     -> encode_aggregate(Body);
encode_body(#rtmpmsg_shared_object{}=Body) -> encode_shared_object(Body);
encode_body(#rtmpmsg_unknown{payload=Bin}) -> Bin.

-spec encode_event(rtmpmsg:user_control_event()) -> binary().
encode_event(#rtmpmsg_event_stream_begin{stream_id=Id})                  -> <<?EVENT_STREAM_BEGIN:16, Id:32>>;
encode_event(#rtmpmsg_event_stream_eof{stream_id=Id})                    -> <<?EVENT_STREAM_EOF:16, Id:32>>;
encode_event(#rtmpmsg_event_stream_dry{stream_id=Id})                    -> <<?EVENT_STREAM_DRY:16, Id:32>>;
encode_event(#rtmpmsg_event_set_buffer_length{stream_id=Id, length=Len}) -> <<?EVENT_SET_BUFFER_LENGTH:16, Id:32, Len:32>>;
encode_event(#rtmpmsg_event_stream_is_recorded{stream_id=Id})            -> <<?EVENT_STREAM_IS_RECORDED:16, Id:32>>;
encode_event(#rtmpmsg_event_ping_request{timestamp=Timestamp})           -> <<?EVENT_PING_REQUEST:16, Timestamp:32>>;
encode_event(#rtmpmsg_event_ping_response{timestamp=Timestamp})          -> <<?EVENT_PING_RESPONSE:16, Timestamp:32>>;
encode_event(#rtmpmsg_event_buffer_empty{stream_id=Id})                  -> <<?EVENT_BUFFER_EMPTY:16, Id:32>>;
encode_event(#rtmpmsg_event_buffer_ready{stream_id=Id})                  -> <<?EVENT_BUFFER_READY:16, Id:32>>;
encode_event(#rtmpmsg_event_unknown{type_id=Type, payload=Payload})      -> <<Type:16, Payload/binary>>.

-spec encode_command(rtmpmsg:message_body_command()) -> binary().
encode_command(Cmd) ->
    #rtmpmsg_command{amf_version = AmfVer,
                     name = Name,
                     transaction_id = TransactionId,
                     object = Object,
                     args = Args} = Cmd,
    list_to_binary(
      [amf_encode(AmfVer, Name),
       amf_encode(AmfVer, TransactionId),
       amf_encode(AmfVer, Object),
       [amf_encode(AmfVer, Value) || Value <- Args]]).

-spec encode_data(rtmpmsg:message_body_data()) -> binary().
encode_data(#rtmpmsg_data{amf_version=AmfVer, values=Values}) ->
    list_to_binary([amf_encode(AmfVer, Value) || Value <- Values]).

-spec encode_aggregate(rtmpmsg:message_body_aggregate()) -> binary().
encode_aggregate(#rtmpmsg_aggregate{messages=Messages}) -> encode_aggregate_messages(Messages, []).

-spec encode_aggregate_messages([rtmpmsg:message()], iolist()) -> binary().
encode_aggregate_messages([], Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_aggregate_messages([Msg|Messages], Acc) ->
    #rtmpmsg{type_id=Type, stream_id=StreamId, timestamp=Timestamp, body=Body} = Msg,
    Payload = encode_body(Body),
    Size = byte_size(Payload),
    BackPointer = 1 + 3 + 4 + 3 + Size,

    MsgData = [<<Type:8, Size:24, Timestamp:32, StreamId:24>>, Payload, <<BackPointer:32>>],
    encode_aggregate_messages(Messages, [MsgData | Acc]).

-spec encode_shared_object(rtmpmsg:message_body_shared_object()) -> binary().
encode_shared_object(#rtmpmsg_shared_object{payload=Payload}) -> Payload.

-spec amf_encode(amf:amf_version(), amf:amf_value()) -> iolist().
amf_encode(AmfVersion, Value) ->
    {ok, EncodedData} = amf:encode(AmfVersion, Value),
    EncodedData.
