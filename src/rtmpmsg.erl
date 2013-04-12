-module(rtmpmsg).

-export([
         message/3,
         program_control_message/1,
         user_control_message/1,
         
         set_chunk_size/1,
         abort/1,
         ack/1,
         win_ack_size/1,
         set_peer_bandwidth/2,
         
         event_stream_begin/1,
         event_stream_eof/1,
         event_stream_dry/1,
         event_set_buffer_length/2,
         event_stream_is_recorded/1,
         event_ping_request/1,
         event_ping_response/1,
         event_buffer_empty/1,
         event_buffer_ready/1,
         event/2,

         audio/3,
         video/3,

         command/6,
         response_command/4, 
         notify_command/4, notify_command/5,

         data/3,
         aggregate/2, aggregate/3
        ]).

-export_type([
              chunk_size/0
             ]).

-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

-type chunk_size() :: 1..?CHUNK_SIZE_MAX.

message(StreamId, Timestamp, Body) ->
    #rtmpmsg
    {
      type_id   = get_type_id(Body),
      stream_id = StreamId,
      timestamp = Timestamp,
      body      = Body
    }.

program_control_message(Body) ->
    message(?PCM_MESSAGE_STREAM_ID, 0, Body).

user_control_message(Event) ->
    program_control_message(#rtmpmsg_user_control{event=Event}).

set_chunk_size(ChunkSize) -> program_control_message(#rtmpmsg_set_chunk_size{size=ChunkSize}).
abort(ChunkStreamId) -> program_control_message(#rtmpmsg_abort{chunk_stream_id=ChunkStreamId}).
ack(SequenceNumber) -> program_control_message(#rtmpmsg_ack{sequence_number=SequenceNumber}).
win_ack_size(WindowSize) -> program_control_message(#rtmpmsg_win_ack_size{size=WindowSize}).
set_peer_bandwidth(WindowSize, LimitType) -> program_control_message(#rtmpmsg_set_peer_bandwidth{size=WindowSize, limit_type=LimitType}).

audio(StreamId, Timestamp, AudioData) ->
    message(StreamId, Timestamp, #rtmpmsg_audio{data=AudioData}).

video(StreamId, Timestamp, VideoData) ->
    message(StreamId, Timestamp, #rtmpmsg_video{data=VideoData}).

command(StreamId, AmfVersion, Name, TransactionId, Object, Args) ->
    message(StreamId, 0, #rtmpmsg_command{amf_version=AmfVersion, 
                                          name=Name,
                                          transaction_id=TransactionId,
                                          object=Object,
                                          args=Args}).

response_command(RequestMessage, Name, Object, Args) ->
    #rtmpmsg{stream_id=StreamId, body=Cmd} = RequestMessage,
    #rtmpmsg_command{amf_version=AmfVer, transaction_id=TransactionId} = Cmd,
    command(StreamId, AmfVer, Name, TransactionId, Object, Args).

notify_command(RequestMessage, Name, Object, Args) ->
    #rtmpmsg{stream_id=StreamId, body=Cmd} = RequestMessage,
    #rtmpmsg_command{amf_version=AmfVer} = Cmd,
    notify_command(StreamId, AmfVer, Name, Object, Args).

notify_command(StreamId, AmfVersion, Name, Object, Args) ->
    command(StreamId, AmfVersion, Name, 0, Object, Args).


data(StreamId, AmfVersion, Values) ->
    message(StreamId, 0, #rtmpmsg_data{amf_version=AmfVersion, values=Values}).

aggregate(StreamId, Messages) ->
    aggregate(StreamId, 0, Messages).

aggregate(StreamId, Timestamp, Messages) ->
    message(StreamId, Timestamp, #rtmpmsg_aggregate{messages=Messages}).

event_stream_begin(StreamId) -> user_control_message(#rtmpmsg_event_stream_begin{stream_id=StreamId}).
event_stream_eof(StreamId) -> user_control_message(#rtmpmsg_event_stream_eof{stream_id=StreamId}).
event_stream_dry(StreamId) -> user_control_message(#rtmpmsg_event_stream_dry{stream_id=StreamId}).
event_set_buffer_length(StreamId, Length) -> user_control_message(#rtmpmsg_event_set_buffer_length{stream_id=StreamId, length=Length}).
event_stream_is_recorded(StreamId) -> user_control_message(#rtmpmsg_event_stream_is_recorded{stream_id=StreamId}).
event_ping_request(Timestamp) -> user_control_message(#rtmpmsg_event_ping_request{timestamp=Timestamp}).
event_ping_response(Timestamp) -> user_control_message(#rtmpmsg_event_ping_response{timestamp=Timestamp}).
event_buffer_empty(StreamId) -> user_control_message(#rtmpmsg_event_buffer_empty{stream_id=StreamId}).
event_buffer_ready(StreamId) -> user_control_message(#rtmpmsg_event_buffer_ready{stream_id=StreamId}).
event(EventTypeId, Payload) -> user_control_message(#rtmpmsg_event_unknown{type_id=EventTypeId, payload=Payload}).

get_type_id(#rtmpmsg_set_chunk_size{})                -> ?TYPE_SET_CHUNK_SIZE;
get_type_id(#rtmpmsg_abort{})                         -> ?TYPE_ABORT;
get_type_id(#rtmpmsg_ack{})                           -> ?TYPE_ACK;
get_type_id(#rtmpmsg_win_ack_size{})                  -> ?TYPE_WIN_ACK_SIZE;
get_type_id(#rtmpmsg_set_peer_bandwidth{})            -> ?TYPE_SET_PEER_BANDWIDTH;
get_type_id(#rtmpmsg_user_control{})                  -> ?TYPE_USER_CONTROL;
get_type_id(#rtmpmsg_audio{})                         -> ?TYPE_AUDIO;
get_type_id(#rtmpmsg_video{})                         -> ?TYPE_VIDEO;
get_type_id(#rtmpmsg_command{amf_version=amf0})       -> ?TYPE_COMMAND_AMF0;
get_type_id(#rtmpmsg_command{amf_version=amf3})       -> ?TYPE_COMMAND_AMF3;
get_type_id(#rtmpmsg_data{amf_version=amf0})          -> ?TYPE_DATA_AMF0;
get_type_id(#rtmpmsg_data{amf_version=amf3})          -> ?TYPE_DATA_AMF3;
get_type_id(#rtmpmsg_shared_obejct{amf_version=amf0}) -> ?TYPE_SHARED_OBJECT_AMF0;
get_type_id(#rtmpmsg_shared_obejct{amf_version=amf3}) -> ?TYPE_SHARED_OBJECT_AMF3;
get_type_id(#rtmpmsg_aggregate{})                     -> ?TYPE_AGGREGATE;
get_type_id(#rtmpmsg_unknown{type_id=TypeId})         -> TypeId.
