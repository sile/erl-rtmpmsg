-module(rtmpmsg_chunk_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").


get_chunk_size_test_() ->
    [
     {"チャンクサイズの初期値が適切か",
      fun () ->
              Dec = rtmpmsg_chunk_decode:init(),
              ?assertMatch(?CHUNK_SIZE_DEFAULT, rtmpmsg_chunk_decode:get_chunk_size(Dec))
      end}
    ].

decode_test_() ->
    {setup,
     fun () -> 
             meck:new(rtmpmsg_message_decode, [no_passthrough_cover]),
             meck:expect(rtmpmsg_message_decode, decode, 
                         fun (ChunkStreamId, MsgStreamId, MsgTypeId, Timestamp, Payload) ->
                                 {ChunkStreamId, MsgStreamId, MsgTypeId, Timestamp, Payload}
                         end)
     end,
     fun (_) -> 
             meck:unload(rtmpmsg_message_decode) 
     end,
     [
      {"チャンクフォーマットが 0 のデータをデコードできる",
       fun () ->
               InputMsg = input_message_data1(),
               Input = input_chunk_fmt_0(InputMsg),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode_messages(Dec, Input),
               ?assertMatch({[_], _, <<"">>}, Result),
               {[Msg], _, _} = Result,

               ?assertEqual(InputMsg, Msg)
       end},
      {"チャンクフォーマットが 0 => 1 => 2 => 3 のデータをデコードできる",
       fun () ->
               InputMsg = input_message_data1(),
               Input = <<(input_chunk_fmt_0(InputMsg))/binary,
                         (input_chunk_fmt_1(InputMsg))/binary,
                         (input_chunk_fmt_2(InputMsg))/binary,
                         (input_chunk_fmt_3(InputMsg))/binary>>,
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode_messages(Dec, Input),
               ?assertMatch({[_, _, _, _], _, <<"">>}, Result),
               {[Msg1, Msg2, Msg3, Msg4], _, _} = Result,
               
               ?assertEqual(InputMsg, Msg1),
               ?assertEqual(InputMsg, Msg2),
               ?assertEqual(InputMsg, Msg3),
               ?assertEqual(InputMsg, Msg4)
       end},
      {"最初のチャンクフォーマットが 0 以外の場合は、エラーになる",
       fun () ->
               InputMsg = input_message_data1(),
               Input = input_chunk_fmt_1(InputMsg),
               Dec = rtmpmsg_chunk_decode:init(),
               ?assertError({first_chunk_format_id_must_be_0, _, _}, rtmpmsg_chunk_decode:decode_messages(Dec, Input))
       end},
      {"BasicHeaderが 2バイト のチャンク",
       fun () ->
               InputMsg = input_message_data2(),
               Input = input_chunk_fmt_0(InputMsg),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode_messages(Dec, Input),
               ?assertMatch({[_], _, <<"">>}, Result),
               {[Msg], _, _} = Result,

               ?assertEqual(InputMsg, Msg)
       end},
      {"BasicHeaderが 3バイト のチャンク",
       fun () ->
               InputMsg = input_message_data3(),
               Input = input_chunk_fmt_0(InputMsg),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode_messages(Dec, Input),
               ?assertMatch({[_], _, <<"">>}, Result),
               {[Msg], _, _} = Result,

               ?assertEqual(InputMsg, Msg)
       end},
      {"ペイロードが複数のチャンクにまたがる場合",
       fun () ->
               InputMsg = input_message_data4(),
               Input = input_chunk_fmt_0(InputMsg),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode_messages(Dec, Input),
               ?assertMatch({[_], _, <<"">>}, Result),
               {[Msg], _, _} = Result,
               
               ?assertEqual(InputMsg, Msg)
       end},
      {"チャンクが細切れになっている場合",
       fun () ->
               InputMsg = input_message_data3(),
               Input = <<(input_chunk_fmt_0(InputMsg))/binary,
                         (input_chunk_fmt_1(InputMsg))/binary,
                         (input_chunk_fmt_2(InputMsg))/binary,
                         (input_chunk_fmt_3(InputMsg))/binary>>,
               Result = 
                   lists:foldl(fun (Byte, {AccDec, AccBin, MsgCount}) ->
                                       {Msgs, AccDec1, AccBin1} = 
                                           rtmpmsg_chunk_decode:decode_messages(AccDec, <<AccBin/binary, Byte>>),
                                       lists:foreach(fun (Msg) ->
                                                             ?assertEqual(InputMsg, Msg)
                                                     end,
                                                     Msgs),
                                       {AccDec1, AccBin1, MsgCount+length(Msgs)}
                               end,
                               {rtmpmsg_chunk_decode:init(), <<>>, 0},
                               binary_to_list(Input)),
               ?assertMatch({_, <<"">>, 4}, Result)
       end}
      %% TODO: set_chunk_size
      %% TODO: extended-timestamp
      %% TODO: chunk-stream-idが途中で変わる
     ]}.

input_message_data1() ->
    ChunkId = 4,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    {ChunkId, MsgStreamId, MsgTypeId, Timestamp, Payload}.

input_message_data2() ->
    ChunkId = 70,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    {ChunkId, MsgStreamId, MsgTypeId, Timestamp, Payload}.    

input_message_data3() ->
    ChunkId = 700,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    {ChunkId, MsgStreamId, MsgTypeId, Timestamp, Payload}.    

input_message_data4() ->
    ChunkId = 800,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = crypto:rand_bytes(round(?CHUNK_SIZE_DEFAULT * 2.5)),
    {ChunkId, MsgStreamId, MsgTypeId, Timestamp, Payload}. 

encode_chunk_basic_header(Fmt, ChunkStreamId) when ChunkStreamId < 64->
    <<Fmt:2, ChunkStreamId:6>>;
encode_chunk_basic_header(Fmt, ChunkStreamId) when ChunkStreamId < 320 ->
    <<Fmt:2, 0:6, (ChunkStreamId-64):8>>;
encode_chunk_basic_header(Fmt, ChunkStreamId) ->
    <<Fmt:2, 1:6, (ChunkStreamId-64):16>>.

encode_payload(ChunkId, <<Payload:?CHUNK_SIZE_DEFAULT/binary, Bin/binary>>) ->
    ChunkBasicHeader = encode_chunk_basic_header(3, ChunkId),
    MessageHeader = <<>>,
    <<Payload/binary, ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Bin))/binary>>;
encode_payload(_, Payload) ->
    Payload.

input_chunk_fmt_0(Msg) ->
    {ChunkId, MsgStreamId, MsgTypeId, Timestamp, Payload} = Msg,
    ChunkBasicHeader = encode_chunk_basic_header(0, ChunkId),
    MessageHeader = <<Timestamp:24, (byte_size(Payload)):24, MsgTypeId:8, MsgStreamId:32/little>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.

input_chunk_fmt_1(Msg) ->
    {ChunkId, _MsgStreamId, MsgTypeId, _Timestamp, Payload} = Msg,
    TimestampDelta = 0,
    
    ChunkBasicHeader = encode_chunk_basic_header(1, ChunkId),
    MessageHeader = <<TimestampDelta:24, (byte_size(Payload)):24, MsgTypeId:8>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.

input_chunk_fmt_2(Msg) ->
    {ChunkId, _MsgStreamId, _MsgTypeId, _Timestamp, Payload} = Msg,
    TimestampDelta = 0,

    ChunkBasicHeader = encode_chunk_basic_header(2, ChunkId),
    MessageHeader = <<TimestampDelta:24>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.
    
input_chunk_fmt_3(Msg) ->
    {ChunkId, _MsgStreamId, _MsgTypeId, _Timestamp, Payload} = Msg,
    
    ChunkBasicHeader = encode_chunk_basic_header(3, ChunkId),
    MessageHeader = <<>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.
