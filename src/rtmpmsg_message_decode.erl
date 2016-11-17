%%% @doc RTMP message decoding module
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
-module(rtmpmsg_message_decode).
-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

%% Exported API
-export([decode_chunk/1]).

%%================================================================================
%% Exported API
%%================================================================================
%% @doc Decode RTMP Chunk
-spec decode_chunk(rtmpmsg:chunk()) -> rtmpmsg:message().
decode_chunk(Chunk) ->
    #chunk{msg_stream_id=StreamId, msg_type_id=TypeId, timestamp=Timestamp, payload=Payload} = Chunk,
    decode(StreamId, TypeId, Timestamp, Payload).

%%================================================================================
%% Internal Functions
%%================================================================================
-spec decode(rtmpmsg:message_stream_id(), rtmpmsg:message_type_id(), rtmpmsg:message_timestamp(), binary()) -> rtmpmsg:message().
decode(MessageStreamId, MessageTypeId, Timestamp, Payload) ->
    #rtmpmsg
    {
      stream_id = MessageStreamId,
      type_id   = MessageTypeId,
      timestamp = Timestamp,
      body      = decode_body(MessageTypeId, Timestamp, Payload)
    }.

-spec decode_body(rtmpmsg:message_type_id(), rtmpmsg:message_timestamp(), binary()) -> rtmpmsg:message_body().
decode_body(TypeId, Timestamp, Payload) ->
    Ret =
        case TypeId of
            ?TYPE_SET_CHUNK_SIZE     -> decode_set_chunk_size(Payload);
            ?TYPE_ABORT              -> decode_abort(Payload);
            ?TYPE_ACK                -> decode_ack(Payload);
            ?TYPE_USER_CONTROL       -> decode_user_control(Payload);
            ?TYPE_WIN_ACK_SIZE       -> decode_win_ack_size(Payload);
            ?TYPE_SET_PEER_BANDWIDTH -> decode_set_peer_bandwidth(Payload);
            ?TYPE_AUDIO              -> decode_audio(Payload);
            ?TYPE_VIDEO              -> decode_video(Payload);
            ?TYPE_COMMAND_AMF0       -> decode_command(amf0, Payload);
            ?TYPE_COMMAND_AMF3       -> decode_command(amf3, Payload);
            ?TYPE_DATA_AMF0          -> decode_data(amf0, Payload);
            ?TYPE_DATA_AMF3          -> decode_data(amf3, Payload);
            ?TYPE_SHARED_OBJECT_AMF0 -> decode_shared_object(amf0, Payload);
            ?TYPE_SHARED_OBJECT_AMF3 -> decode_shared_object(amf3, Payload);
            ?TYPE_AGGREGATE          -> decode_aggregate(Payload, Timestamp);
            _                        -> #rtmpmsg_unknown{type_id=TypeId, payload=Payload}
        end,
    case Ret of
        undefined ->
            #rtmpmsg_unknown{type_id=TypeId, payload=Payload};
        _ ->
            Ret
    end.

-spec decode_set_chunk_size(binary()) -> rtmpmsg:message_body_set_chunk_size().
decode_set_chunk_size(<<Size:32>>) -> #rtmpmsg_set_chunk_size{size=Size}.

-spec decode_abort(binary()) -> rtmpmsg:message_body_abort().
decode_abort(<<ChunkStreamId:32>>) -> #rtmpmsg_abort{chunk_stream_id=ChunkStreamId}.

-spec decode_ack(binary()) -> rtmpmsg:message_body_ack().
decode_ack(<<SequenceNumber:32>>) -> #rtmpmsg_ack{sequence_number=SequenceNumber}.

-spec decode_win_ack_size(binary()) -> rtmpmsg:message_body_win_ack_size().
decode_win_ack_size(<<Size:32>>) -> #rtmpmsg_win_ack_size{size=Size}.

-spec decode_set_peer_bandwidth(binary()) -> rtmpmsg:message_body_set_peer_bandwidth().
decode_set_peer_bandwidth(<<Size:32, 0:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=hard};
decode_set_peer_bandwidth(<<Size:32, 1:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=soft};
decode_set_peer_bandwidth(<<Size:32, 2:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=dynamic}.

-spec decode_user_control(binary()) -> rtmpmsg:message_body_user_control().
decode_user_control(<<EventType:16, Payload/binary>>) ->
    Event =
        case EventType of
            ?EVENT_STREAM_BEGIN        -> decode_event_stream_begin(Payload);
            ?EVENT_STREAM_EOF          -> decode_event_stream_eof(Payload);
            ?EVENT_STREAM_DRY          -> decode_event_stream_dry(Payload);
            ?EVENT_SET_BUFFER_LENGTH   -> decode_event_set_buffer_length(Payload);
            ?EVENT_STREAM_IS_RECORDED  -> decode_event_stream_is_recorded(Payload);
            ?EVENT_PING_REQUEST        -> decode_event_ping_request(Payload);
            ?EVENT_PING_RESPONSE       -> decode_event_ping_response(Payload);
            ?EVENT_BUFFER_EMPTY        -> decode_event_buffer_empty(Payload);
            ?EVENT_BUFFER_READY        -> decode_event_buffer_ready(Payload);
            _                          -> #rtmpmsg_event_unknown{type_id=EventType, payload=Payload}
        end,
    #rtmpmsg_user_control{event=Event}.

-spec decode_audio(binary()) -> rtmpmsg:message_body_audio().
decode_audio(Payload) -> #rtmpmsg_audio{data = Payload}.

-spec decode_video(binary()) -> rtmpmsg:message_body_video().
decode_video(Payload) -> #rtmpmsg_video{data = Payload}.

-spec decode_command(amf:amf_version(), binary()) -> rtmpmsg:message_body_command() | undefined.
decode_command(AmfVersion0, Payload0) ->
    {AmfVersion, Payload} =
        case {AmfVersion0, Payload0} of
            {amf3, <<0, Rest/binary>>} -> {amf0, Rest};  % NOTE: 理由は分からないがFlashPlayerはこのような変則的なデータを送ってくる
            _                          -> {AmfVersion0, Payload0}
        end,
    try
        {ok, CommandName, Bin1}   = amf:decode(AmfVersion, Payload),
        {ok, TransactionId, Bin2} = amf:decode(AmfVersion, Bin1),
        {ok, CommandObject, Bin3} = amf:decode(AmfVersion, Bin2),
        OptionalArgs              = decode_amf_values(AmfVersion, Bin3, []),
        #rtmpmsg_command{
           amf_version    = AmfVersion,
           name           = CommandName,
           transaction_id = TransactionId,
           object         = CommandObject,
           args           = OptionalArgs
          }
    catch
        error:{badmatch, {error, _}} ->
            undefined
    end.

-spec decode_data(amf:amf_version(), binary()) -> rtmpmsg:message_body_data().
decode_data(AmfVersion0, Payload0) ->
    {AmfVersion, Payload} =
        case {AmfVersion0, Payload0} of
            {amf3, <<0, Rest/binary>>} -> {amf0, Rest};  % NOTE: 理由は分からないがFlashPlayerはこのような変則的なデータを送ってくる
            _                          -> {AmfVersion0, Payload0}
        end,
    #rtmpmsg_data{amf_version = AmfVersion,
                  values      = decode_amf_values(AmfVersion, Payload, [])}.

-spec decode_shared_object(amf:amf_version(), binary()) -> rtmpmsg:message_body_shared_object().
decode_shared_object(AmfVersion, Payload) ->
    #rtmpmsg_shared_object{amf_version = AmfVersion,
                           payload     = Payload}.

-spec decode_aggregate(binary(), rtmpmsg:message_timestamp()) -> rtmpmsg:message_body_aggregate().
decode_aggregate(Payload, Timestamp) ->
    #rtmpmsg_aggregate{messages = decode_aggregate_messages_first(Payload, Timestamp)}.

-spec decode_aggregate_messages_first(binary(), rtmpmsg:message_timestamp()) -> [rtmpmsg:message()].
decode_aggregate_messages_first(<<>>, _) ->
    [];
decode_aggregate_messages_first(<<Type:8, Size:24, TimestampBase:24, TimestampExtended:8, StreamId:24, Payload:Size/binary, _BackPointer:32, Bin/binary>>, TimestampOffset) ->
    <<Timestamp:32/signed>> = <<TimestampExtended:8, TimestampBase:24>>,
    TimestampOffset2 = TimestampOffset - Timestamp,
    Msg = decode(StreamId, Type, TimestampOffset2 + Timestamp, Payload),
    decode_aggregate_messages(Bin, TimestampOffset2, [Msg]).

-spec decode_aggregate_messages(binary(), rtmpmsg:message_timestamp(), [rtmpmsg:message()]) -> [rtmpmsg:message()].
decode_aggregate_messages(<<>>, _, Acc) ->
    lists:reverse(Acc);
decode_aggregate_messages(<<Type:8, Size:24, TimestampBase:24, TimestampExtended:8, StreamId:24, Payload:Size/binary, _BackPointer:32, Bin/binary>>, TimestampOffset, Acc) ->
    <<Timestamp:32/signed>> = <<TimestampExtended:8, TimestampBase:24>>,
    Msg = decode(StreamId, Type, TimestampOffset + Timestamp, Payload),
    decode_aggregate_messages(Bin, TimestampOffset, [Msg|Acc]).

-spec decode_event_stream_begin(binary())  -> rtmpmsg:event_stream_begin().
decode_event_stream_begin(<<StreamId:32>>) -> #rtmpmsg_event_stream_begin{stream_id=StreamId}.

-spec decode_event_stream_eof(binary())  -> rtmpmsg:event_stream_eof().
decode_event_stream_eof(<<StreamId:32>>) -> #rtmpmsg_event_stream_eof{stream_id=StreamId}.

-spec decode_event_stream_dry(binary())  -> rtmpmsg:event_stream_dry().
decode_event_stream_dry(<<StreamId:32>>) -> #rtmpmsg_event_stream_dry{stream_id=StreamId}.

-spec decode_event_stream_is_recorded(binary())  -> rtmpmsg:event_stream_is_recorded().
decode_event_stream_is_recorded(<<StreamId:32>>) -> #rtmpmsg_event_stream_is_recorded{stream_id=StreamId}.

-spec decode_event_buffer_empty(binary())  -> rtmpmsg:event_buffer_empty().
decode_event_buffer_empty(<<StreamId:32>>) -> #rtmpmsg_event_buffer_empty{stream_id=StreamId}.

-spec decode_event_buffer_ready(binary())  -> rtmpmsg:event_buffer_ready().
decode_event_buffer_ready(<<StreamId:32>>) -> #rtmpmsg_event_buffer_ready{stream_id=StreamId}.

-spec decode_event_set_buffer_length(binary())          -> rtmpmsg:event_set_buffer_length().
decode_event_set_buffer_length(<<StreamId:32, Len:32>>) -> #rtmpmsg_event_set_buffer_length{stream_id=StreamId, length=Len}.

-spec decode_event_ping_request(binary())   -> rtmpmsg:event_ping_request().
decode_event_ping_request(<<Timestamp:32>>) -> #rtmpmsg_event_ping_request{timestamp=Timestamp}.

-spec decode_event_ping_response(binary())   -> rtmpmsg:event_ping_response().
decode_event_ping_response(<<Timestamp:32>>) -> #rtmpmsg_event_ping_response{timestamp=Timestamp}.

-spec decode_amf_values(amf:amf_version(), binary(), [amf:amf_value()]) -> [amf:amf_value()].
decode_amf_values(_AmfVersion, <<"">>, Acc) ->
    lists:reverse(Acc);
decode_amf_values(AmfVersion, Bin, Acc) ->
    {ok, Value, Bin1} = amf:decode(AmfVersion, Bin),
    decode_amf_values(AmfVersion, Bin1, [Value|Acc]).
