
-define(CHUNK_SIZE_DEFAULT, 128).
-define(CHUNK_SIZE_MAX, 65536).

-define(CHUNK_FMT_0, 0).
-define(CHUNK_FMT_1, 1).
-define(CHUNK_FMT_2, 2).
-define(CHUNK_FMT_3, 3).

%% Protocol Control Message
-define(TYPE_SET_CHUNK_SIZE,     1).
-define(TYPE_ABORT,              2).
-define(TYPE_ACK,                3).
-define(TYPE_USER_CONTROL,       4).
-define(TYPE_WIN_ACK_SIZE,       5).
-define(TYPE_SET_PEER_BANDWIDTH, 6).

-define(TYPE_AUDIO,              8).
-define(TYPE_VIDEO,              9).
-define(TYPE_DATA_AMF3,          15).
-define(TYPE_SHARED_OBJECT_AMF3, 16). % aka. kMsgContainerEx
-define(TYPE_COMMAND_AMF3,       17).
-define(TYPE_DATA_AMF0,          18).
-define(TYPE_SHARED_OBJECT_AMF0, 19). % aka. kMsgContainer
-define(TYPE_COMMAND_AMF0,       20).
-define(TYPE_AGGREGATE,          22).

-define(EVENT_STREAM_BEGIN, 0).
-define(EVENT_STREAM_EOF, 1).
-define(EVENT_STREAM_DRY, 2).
-define(EVENT_SET_BUFFER_LENGTH, 3).
-define(EVENT_STREAM_IS_RECORDED, 4).
-define(EVENT_PING_REQUEST, 6).
-define(EVENT_PING_RESPONSE, 7).
-define(EVENT_BUFFER_EMPTY, 31).
-define(EVENT_BUFFER_READY, 32).

-record(chunk,
        {
          id = 0 :: non_neg_integer(),
          msg_stream_id = 0 :: non_neg_integer(),
          msg_type_id = 0 :: non_neg_integer(),
          timestamp = 0 :: non_neg_integer(),
          payload = <<"">> :: binary()
        }).
