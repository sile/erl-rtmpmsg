-define(PCM_MESSAGE_STREAM_ID, 0).
-define(PCM_CHUNK_STREAM_ID, 2).

-define(CHUNK_SIZE_DEFAULT, 128).
-define(CHUNK_SIZE_MAX, 65536).

-record(chunk,
        {
          id            = 0    :: rtmpmsg:chunk_stream_id(),
          msg_stream_id = 0    :: rtmpmsg:message_stream_id(),
          msg_type_id   = 0    :: rtmpmsg:message_type_id(),
          timestamp     = 0    :: rtmpmsg:message_timestamp(),
          payload       = <<>> :: binary()
        }).
