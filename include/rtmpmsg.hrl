
-record(rtmpmsg_unknown,
        {
          type_id = 0      :: rtmpmsg:message_type_id(),
          payload = <<"">> :: binary()
        }).

-record(rtmpmsg,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id(),
          type_id = 0   :: rtmpmsg:message_type_id(),
          timestamp = 0 :: rtmpmsg:message_timestamp(),
          body          :: rtmpmsg:message_body()
        }).


-record(rtmpmsg_set_chunk_size,
        {
          size = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_set_peer_bandwidth,
        {
          size = 0          :: non_neg_integer(),
          limit_type = hard :: rtmpmsg:set_peer_bandwidth_limit_type()
        }).

-record(rtmpmsg_abort,
        {
          chunk_stream_id = 0 :: rtmpmsg:chunk_stream_id()
        }).

-record(rtmpmsg_ack,
        {
          sequence_number = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_win_ack_size,
        {
          size = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_user_control,
        {
          event :: rtmpmsg:user_control_event()
        }).

-record(rtmpmsg_shared_object,
        {
          amf_version = amf0 :: amf:amf_version(),
          payload = <<"">> :: binary()
        }).

-record(rtmpmsg_data,
        {
          amf_version = amf0 :: amf:amf_version(),
          values = [] :: [amf:amf_value()]
        }).

-record(rtmpmsg_command,
        {
          amf_version    = amf0 :: amf:amf_version(),
          name           = <<"">> :: amf:amf_string(),
          transaction_id = 0 :: amf:amf_number(),
          object         = null :: amf:amf_value(),
          args           = [] :: [amf:amf_value()]
        }).

-record(rtmpmsg_audio,
        {
          data :: binary()
        }).

-record(rtmpmsg_video,
        {
          data :: binary()
        }).

-record(rtmpmsg_aggregate,
        {
          messages = [] :: [rtmpmsg:message()]
        }).

-record(rtmpmsg_event_unknown,
        {
          type_id = 0 :: non_neg_integer(),
          payload = <<"">> :: binary()
        }).

-record(rtmpmsg_event_stream_begin,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_stream_eof,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_stream_dry,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_stream_is_recorded,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_buffer_empty,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_buffer_ready,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id()
        }).

-record(rtmpmsg_event_set_buffer_length,
        {
          stream_id = 0 :: rtmpmsg:message_stream_id(),
          length = 0    :: rtmpmsg:milliseconds()
        }).

-record(rtmpmsg_event_ping_request,
        {
          timestamp = 0 :: rtmpmsg:message_timestamp()
        }).

-record(rtmpmsg_event_ping_response,
        {
          timestamp = 0 :: rtmpmsg:message_timestamp()
        }).

%% Protocol Control Message
-define(RTMPMSGP_TYPE_SET_CHUNK_SIZE,     1).
-define(RTMPMSGP_TYPE_ABORT,              2).
-define(RTMPMSGP_TYPE_ACK,                3).
-define(RTMPMSGP_TYPE_USER_CONTROL,       4).
-define(RTMPMSGP_TYPE_WIN_ACK_SIZE,       5).
-define(RTMPMSGP_TYPE_SET_PEER_BANDWIDTH, 6).

-define(RTMPMSGP_TYPE_AUDIO,              8).
-define(RTMPMSGP_TYPE_VIDEO,              9).
-define(RTMPMSGP_TYPE_DATA_AMF3,          15).
-define(RTMPMSGP_TYPE_SHARED_OBJECT_AMF3, 16). % aka. kMsgContainerEx
-define(RTMPMSGP_TYPE_COMMAND_AMF3,       17).
-define(RTMPMSGP_TYPE_DATA_AMF0,          18).
-define(RTMPMSGP_TYPE_SHARED_OBJECT_AMF0, 19). % aka. kMsgContainer
-define(RTMPMSGP_TYPE_COMMAND_AMF0,       20).
-define(RTMPMSGP_TYPE_AGGREGATE,          22).

-define(RTMPMSG_EVENT_STREAM_BEGIN, 0).
-define(RTMPMSG_EVENT_STREAM_EOF, 1).
-define(RTMPMSG_EVENT_STREAM_DRY, 2).
-define(RTMPMSG_EVENT_SET_BUFFER_LENGTH, 3).
-define(RTMPMSG_EVENT_STREAM_IS_RECORDED, 4).
-define(RTMPMSG_EVENT_PING_REQUEST, 6).
-define(RTMPMSG_EVENT_PING_RESPONSE, 7).
-define(RTMPMSG_EVENT_BUFFER_EMPTY, 31).
-define(RTMPMSG_EVENT_BUFFER_READY, 32).
