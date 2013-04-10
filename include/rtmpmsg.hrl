
-record(rtmpmsg_unknown,
        {
          type_id = 0      :: non_neg_integer(),
          payload = <<"">> :: binary()
        }).

-record(rtmpmsg,
        {
          stream_id = 0 :: non_neg_integer(),
          type_id = 0 :: non_neg_integer(),
          timestamp = 0 :: non_neg_integer(),
          body :: term() %% TODO
        }).


-record(rtmpmsg_set_chunk_size,
        {
          size = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_set_peer_bandwidth,
        {
          size = 0 :: non_neg_integer(),
          limit_type = hard :: hard|soft|dynamic
        }).

-record(rtmpmsg_abort,
        {
          chunk_stream_id = 0 :: non_neg_integer() % TODO
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
          event :: term() % TODO
        }).

%% TODO
-record(rtmpmsg_shared_obejct,
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
          data = <<"">> :: binary()
        }).

-record(rtmpmsg_video,
        {
          data = <<"">> :: binary()
        }).

-record(rtmpmsg_aggregate,
        {
          messages = [] :: list() % TODO
        }).

-record(rtmpmsg_event_unknown,
        {
          type_id = 0 :: non_neg_integer(),
          payload = <<"">> :: binary()
        }).

-record(rtmpmsg_event_stream_begin,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_stream_eof,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_stream_dry,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_stream_is_recorded,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_buffer_empty,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_buffer_ready,
        {
          stream_id = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_set_buffer_length,
        {
          stream_id = 0 :: non_neg_integer(),
          length = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_ping_request,
        {
          timestamp = 0 :: non_neg_integer()
        }).

-record(rtmpmsg_event_ping_response,
        {
          timestamp = 0 :: non_neg_integer()
        }).
