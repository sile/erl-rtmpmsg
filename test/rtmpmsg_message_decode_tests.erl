-module(rtmpmsg_message_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

%% 現状、encodeのテストも兼ねてしまっている

decode_test_() ->
    [
     {"set_chunk_sizeメッセージのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:set_chunk_size(1234))
      end},
     {"abortメッセージのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:abort(10))
      end},
     {"ackメッセージのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:ack(56789))
      end},
     {"win_ack_sizeメッセージのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:win_ack_size(45678))
      end},
     {"set_peer_bandwidthメッセージのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:set_peer_bandwidth(4567, soft))
      end},
     {"user_controlメッセージのstream_beginイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_stream_begin(10))
      end},
     {"user_controlメッセージのstream_eofイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_stream_eof(10))
      end},
     {"user_controlメッセージのstream_dryイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_stream_dry(10))
      end},
     {"user_controlメッセージのset_buffer_lengthイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_set_buffer_length(10, 1234))
      end},
     {"user_controlメッセージのstream_is_recordedイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_stream_is_recorded(10))
      end},
     {"user_controlメッセージのping_requestイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_ping_request(3456))
      end},
     {"user_controlメッセージのping_responseイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_ping_response(3456))
      end},
     {"user_controlメッセージのbuffer_emptyイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_buffer_empty(10))
      end},
     {"user_controlメッセージのbuffer_readyイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event_buffer_ready(10))
      end},
     {"user_controlメッセージの未知のイベントのデコード",
      fun () ->
              assert_message_decode(rtmpmsg:event(10, <<"xyzzy">>))
      end},
     {"audioメッセージのデコード",
      fun () ->
              todo_mock
      end},
     {"videoメッセージのデコード",
      fun () ->
              todo_mock
      end}
    ].

assert_message_decode(InputMsg) ->
    ChunkStreamId = 3,
    InputChunk = rtmpmsg_message_encode:encode_to_chunk(ChunkStreamId, InputMsg),
    Msg = rtmpmsg_message_decode:decode_chunk(InputChunk),
    ?assertEqual(InputMsg, Msg).
