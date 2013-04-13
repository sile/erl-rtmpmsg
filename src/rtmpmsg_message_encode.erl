-module(rtmpmsg_message_encode).
-export([encode_to_chunk/2]).

-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

encode_to_chunk(ChunkStreamId, Msg) ->
    Payload = encode_body(Msg#rtmpmsg.body),
    #chunk
    {
      id            = ChunkStreamId,
      timestamp     = Msg#rtmpmsg.timestamp,
      msg_type_id   = Msg#rtmpmsg.type_id,
      msg_stream_id = Msg#rtmpmsg.stream_id,
      payload       = Payload
    }.

encode_body(#rtmpmsg_set_chunk_size{size=Size}) -> <<Size:32>>;
encode_body(#rtmpmsg_abort{chunk_stream_id=Id}) -> <<Id:32>>;
encode_body(#rtmpmsg_ack{sequence_number=Num})  -> <<Num:32>>;
encode_body(#rtmpmsg_win_ack_size{size=Size})   -> <<Size:32>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=hard})    -> <<Size:32, 0>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=soft})    -> <<Size:32, 1>>;
encode_body(#rtmpmsg_set_peer_bandwidth{size=Size, limit_type=dynamic}) -> <<Size:32, 2>>;
encode_body(#rtmpmsg_user_control{event=Event}) -> encode_event(Event);
encode_body(#rtmpmsg_audio{data=Audio})    -> encode_audio(Audio);
encode_body(#rtmpmsg_video{data=Video})    -> encode_video(Video);
encode_body(#rtmpmsg_command{}=Body)       -> encode_command(Body);
encode_body(#rtmpmsg_data{}=Body)          -> encode_data(Body);
encode_body(#rtmpmsg_aggregate{}=Body)     -> encode_aggregate(Body);
encode_body(#rtmpmsg_shared_object{}=Body) -> encode_shared_object(Body);
encode_body(#rtmpmsg_unknown{payload=Bin}) -> Bin.

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

encode_audio(Audio) ->
    list_to_binary(flv_tag:encode_audio(Audio)). %% TODO: iolistで渡せるようにしたい

encode_video(Video) ->
    list_to_binary(flv_tag:encode_video(Video)). %% TODO: iolistで渡せるようにしたい

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

encode_data(#rtmpmsg_data{amf_version=AmfVer, values=Values}) ->
    list_to_binary([amf_encode(AmfVer, Value) || Value <- Values]).

encode_aggregate(#rtmpmsg_aggregate{messages=Messages}) ->
    encode_aggregate_messages(Messages, []).

encode_aggregate_messages([], Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_aggregate_messages([Msg|Messages], Acc) ->
    #rtmpmsg{type_id=Type, stream_id=StreamId, timestamp=Timestamp, body=Body} = Msg,
    Payload = encode_body(Body),
    Size = byte_size(Payload),
    BackPointer = 1 + 3 + 4 + 3 + Size,

    ReversedBin = [<<BackPointer:32>>, Payload, <<Type:8, Size:24, Timestamp:32, StreamId:24>>],
    encode_aggregate_messages(Messages, ReversedBin ++ Acc).

encode_shared_object(#rtmpmsg_shared_object{payload=Payload}) -> Payload.

amf_encode(AmfVersion, Value) ->
    {ok, EncodedData} = amf:encode(AmfVersion, Value),
    EncodedData.
