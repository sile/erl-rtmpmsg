-module(rtmpmsg_message_decode).
-export([decode/4]).

-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

decode(MessageStreamId, MessageTypeId, Timestamp, Payload) ->
    #rtmpmsg
    {
      stream_id = MessageStreamId,
      type_id = MessageTypeId,
      timestamp = Timestamp,
      body = decode_payload(MessageTypeId, Payload)
    }.

decode_payload(TypeId, Payload) ->
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
        ?TYPE_AGGREGATE          -> decode_aggregate(Payload);
        _                        -> #rtmpmsg_unknown{type_id=TypeId, payload=Payload}
    end.

decode_set_chunk_size(<<Size:32>>) -> #rtmpmsg_set_chunk_size{size=Size}.

decode_abort(<<ChunkStreamId:32>>) -> #rtmpmsg_abort{chunk_stream_id=ChunkStreamId}.

decode_ack(<<SequenceNumber:32>>) -> #rtmpmsg_ack{sequence_number=SequenceNumber}.

decode_win_ack_size(<<Size:32>>) -> #rtmpmsg_win_ack_size{size=Size}.

decode_set_peer_bandwidth(<<Size:32, 0:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=hard};
decode_set_peer_bandwidth(<<Size:32, 1:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=soft};
decode_set_peer_bandwidth(<<Size:32, 2:8>>) -> #rtmpmsg_set_peer_bandwidth{size=Size, limit_type=dynamic}.

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

decode_audio(Payload) ->
    #rtmpmsg_audio{data = Payload}. % TODO: flv

decode_video(Payload) ->
    #rtmpmsg_video{data = Payload}. % TODO: flv

decode_command(AmfVersion, Payload) ->
    {ok, CommandName, Bin1}   = amf:decode(AmfVersion, Payload),
    {ok, TransactionId, Bin2} = amf:decode(AmfVersion, Bin1),
    {ok, CommandObject, Bin3} = amf:decode(AmfVersion, Bin2),
    OptionalArgs              = decode_amf_values(AmfVersion, Bin3, []),
    #rtmpmsg_command
    {
      amf_version   =AmfVersion,
      name          =CommandName,
      transaction_id=TransactionId,
      object        =CommandObject,
      args          =OptionalArgs
    }.

decode_data(AmfVersion, Payload) ->
    #rtmpmsg_data{amf_version = AmfVersion,
                  values      = decode_amf_values(AmfVersion, Payload, [])}.

decode_shared_object(AmfVersion, Payload) ->
    #rtmpmsg_shared_obejct{amf_version = AmfVersion,
                           payload = Payload}.

decode_aggregate(Payload) ->
    decode_aggregate_impl(Payload, []).

decode_aggregate_impl(<<>>, Acc) ->
    #rtmpmsg_aggregate{messages=lists:reverse(Acc)};
decode_aggregate_impl(<<Type:8, Size:24, Timestamp:32, StreamId:24, Payload:Size/binary, _BackPointer:32, Bin/binary>>, Acc) ->
    Msg = decode(StreamId, Type, Timestamp, Payload),
    decode_aggregate_impl(Bin, [Msg|Acc]).

decode_event_stream_begin(<<StreamId:32>>)       -> #rtmpmsg_event_stream_begin{stream_id=StreamId}.
decode_event_stream_eof(<<StreamId:32>>)         -> #rtmpmsg_event_stream_eof{stream_id=StreamId}.
decode_event_stream_dry(<<StreamId:32>>)         -> #rtmpmsg_event_stream_dry{stream_id=StreamId}.
decode_event_stream_is_recorded(<<StreamId:32>>) -> #rtmpmsg_event_stream_is_recorded{stream_id=StreamId}.
decode_event_buffer_empty(<<StreamId:32>>)       -> #rtmpmsg_event_buffer_empty{stream_id=StreamId}.
decode_event_buffer_ready(<<StreamId:32>>)       -> #rtmpmsg_event_buffer_ready{stream_id=StreamId}.
decode_event_set_buffer_length(<<StreamId:32, Len:32>>) -> #rtmpmsg_event_set_buffer_length{stream_id=StreamId, length=Len}.

decode_event_ping_request(<<Timestamp:32>>)  -> #rtmpmsg_event_ping_request{timestamp=Timestamp}.
decode_event_ping_response(<<Timestamp:32>>) -> #rtmpmsg_event_ping_response{timestamp=Timestamp}.

decode_amf_values(_AmfVersion, <<"">>, Acc) ->
    lists:reverse(Acc);
decode_amf_values(AmfVersion, Bin, Acc) ->
    {ok, Value, Bin1} = amf:decode(AmfVersion, Bin),
    decode_amf_values(AmfVersion, Bin1, [Value|Acc]).
