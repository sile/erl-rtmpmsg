# ErlRTMPMsg

[![hex.pm version](https://img.shields.io/hexpm/v/rtmpmsg.svg)](https://hex.pm/packages/rtmpmsg)
[![Build Status](https://travis-ci.org/sile/erl-rtmpmsg.svg?branch=master)](https://travis-ci.org/sile/erl-rtmpmsg)
[![Code Coverage](https://codecov.io/gh/sile/erl-rtmpmsg/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/erl-rtmpmsg/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

ErlRTMPMsg is an RTMP message(ver1.0) encoding/deconding library written in Erlang.

## API

[EDoc Documentation](https://hexdocs.pm/rtmpmsg/)

## Examples

```erlang
$ rebar3 shell

%% Load Message Record
1> rr(rtmpmsg).
[chunk,rtmpmsg,rtmpmsg_abort,rtmpmsg_ack,rtmpmsg_aggregate,
 rtmpmsg_audio,rtmpmsg_command,rtmpmsg_data,
 rtmpmsg_event_buffer_empty,rtmpmsg_event_buffer_ready,
 rtmpmsg_event_ping_request,rtmpmsg_event_ping_response,
 rtmpmsg_event_set_buffer_length,rtmpmsg_event_stream_begin,
 rtmpmsg_event_stream_dry,rtmpmsg_event_stream_eof,
 rtmpmsg_event_stream_is_recorded,rtmpmsg_event_unknown,
 rtmpmsg_set_chunk_size,rtmpmsg_set_peer_bandwidth,
 rtmpmsg_shared_object,rtmpmsg_unknown,rtmpmsg_user_control,
 rtmpmsg_video,rtmpmsg_win_ack_size]

%% Encode Message
2> Enc0 = rtmpmsg_encoder:new().
3> Msg  = rtmpmsg:command(10, amf0, <<"connect">>, 1.0, amf:object([{<<"key">>, <<"value">>}]), []).
#rtmpmsg{stream_id = 10,type_id = 20,timestamp = 0,
         body = #rtmpmsg_command{amf_version = amf0,
                                 name = <<"connect">>,transaction_id = 1.0,
                                 object = {amf_object,undefined,true,[],
                                                      [{<<"key">>,<<"value">>}]},
                                 args = []}}

4> {Enc1, EncodedData} = rtmpmsg_encoder:encode(Enc0, 3, Msg).
5> EncodedData.
[3,
 <<0,0,0,0,0,36,20,10,0,0,0>>,
 <<2,0,7,99,111,110,110,101,99,116,0,63,240,0,0,0,0,0,0,3,
   0,3,107,101,121,2,...>>]

%% Decode Message
6> Dec0 = rtmpmsg_decoder:new().
7> {ok, Dec1, DecodedMsg, UnconsumedBin} = rtmpmsg_decoder:decode(Dec0, list_to_binary(EncodedData)).
8> DecodedMsg.
#rtmpmsg{stream_id = 10,type_id = 20,timestamp = 0,
         body = #rtmpmsg_command{amf_version = amf0,
                                 name = <<"connect">>,transaction_id = 1.0,
                                 object = {amf_object,undefined,true,[],
                                                      [{<<"key">>,<<"value">>}]},
                                 args = []}}
9> UnconsumedBin.
<<>>.
10> Msg =:= DecodedMsg.
true.
```

## Reference

* [RTMP Specification (ver1.0)](http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/rtmp/pdf/rtmp_specification_1.0.pdf)
