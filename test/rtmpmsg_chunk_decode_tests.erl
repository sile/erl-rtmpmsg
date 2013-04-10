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
    [
      {"チャンクフォーマットが 0 のデータをデコードできる",
       fun () ->
               InputChunk = input_data1(),
               Input = input_chunk_fmt_0(InputChunk),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode(Dec, Input),
               ?assertMatch({#chunk{}, _, <<"">>}, Result),
               {Chunk, _, _} = Result,

               ?assertEqual(InputChunk, Chunk)
       end},
      {"チャンクフォーマットが 0 => 1 => 2 => 3 のデータをデコードできる",
       fun () ->
               InputChunk = input_data1(),
               Input = <<(input_chunk_fmt_0(InputChunk))/binary,
                         (input_chunk_fmt_1(InputChunk))/binary,
                         (input_chunk_fmt_2(InputChunk))/binary,
                         (input_chunk_fmt_3(InputChunk))/binary>>,
               Dec = rtmpmsg_chunk_decode:init(),
               
               {Chunk1, Dec1, Bin1} = rtmpmsg_chunk_decode:decode(Dec, Input),
               ?assertEqual(InputChunk, Chunk1),

               {Chunk2, Dec2, Bin2} = rtmpmsg_chunk_decode:decode(Dec1, Bin1),
               ?assertEqual(InputChunk, Chunk2),

               {Chunk3, Dec3, Bin3} = rtmpmsg_chunk_decode:decode(Dec2, Bin2),
               ?assertEqual(InputChunk, Chunk3),

               {Chunk4, _Dec4, Bin4} = rtmpmsg_chunk_decode:decode(Dec3, Bin3),
               ?assertEqual(InputChunk, Chunk4),
               ?assertMatch(<<"">>, Bin4)
       end},
      {"最初のチャンクフォーマットが 0 以外の場合は、エラーになる",
       fun () ->
               InputChunk = input_data1(),
               Input = input_chunk_fmt_1(InputChunk),
               Dec = rtmpmsg_chunk_decode:init(),
               ?assertError({first_chunk_format_id_must_be_0, _, _}, rtmpmsg_chunk_decode:decode(Dec, Input))
       end},
      {"BasicHeaderが 2バイト のチャンク",
       fun () ->
               InputChunk = input_data2(),
               Input = input_chunk_fmt_0(InputChunk),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode(Dec, Input),
               ?assertMatch({#chunk{}, _, <<"">>}, Result),
               {Chunk, _, _} = Result,
               
               ?assertEqual(InputChunk, Chunk)
       end},
      {"BasicHeaderが 3バイト のチャンク",
       fun () ->
               InputChunk = input_data3(),
               Input = input_chunk_fmt_0(InputChunk),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode(Dec, Input),
               ?assertMatch({#chunk{}, _, <<"">>}, Result),
               {Chunk, _, _} = Result,
               
               ?assertEqual(InputChunk, Chunk)
       end},
      {"ペイロードが複数のチャンクにまたがる場合",
       fun () ->
               InputChunk = input_data4(),
               Input = input_chunk_fmt_0(InputChunk),
               Dec = rtmpmsg_chunk_decode:init(),
               
               Result = rtmpmsg_chunk_decode:decode(Dec, Input),
               ?assertMatch({#chunk{}, _, <<"">>}, Result),
               {Chunk, _, _} = Result,
               
               ?assertEqual(InputChunk, Chunk)
       end},
      {"チャンクが細切れになっている場合",
       fun () ->
               InputChunk = input_data3(),
               Input = <<(input_chunk_fmt_0(InputChunk))/binary,
                         (input_chunk_fmt_1(InputChunk))/binary,
                         (input_chunk_fmt_2(InputChunk))/binary,
                         (input_chunk_fmt_3(InputChunk))/binary>>,
               Result = 
                   lists:foldl(fun (Byte, {AccDec, AccBin, Count}) ->
                                       case rtmpmsg_chunk_decode:decode(AccDec, <<AccBin/binary, Byte>>) of
                                           {partial, AccDec1, AccBin1} ->
                                               {AccDec1, AccBin1, Count};
                                           {Chunk, AccDec1, AccBin1} ->
                                               ?assertEqual(InputChunk, Chunk),
                                               {AccDec1, AccBin1, Count+1}
                                       end
                               end,
                               {rtmpmsg_chunk_decode:init(), <<>>, 0},
                               binary_to_list(Input)),
               ?assertMatch({_, <<"">>, 4}, Result)
       end}
    ].
    %%  [
    %%   %% TODO: set_chunk_size
    %%   %% TODO: extended-timestamp
    %%   %% TODO: chunk-stream-idが途中で変わる
    %%  ]}.

input_data1() ->
    ChunkId = 4,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    #chunk{id=ChunkId, msg_stream_id=MsgStreamId, msg_type_id=MsgTypeId, timestamp=Timestamp, payload=Payload}.

input_data2() ->
    ChunkId = 70,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    #chunk{id=ChunkId, msg_stream_id=MsgStreamId, msg_type_id=MsgTypeId, timestamp=Timestamp, payload=Payload}.

input_data3() ->
    ChunkId = 700,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = <<"abcde">>,
    #chunk{id=ChunkId, msg_stream_id=MsgStreamId, msg_type_id=MsgTypeId, timestamp=Timestamp, payload=Payload}.

input_data4() ->
    ChunkId = 800,
    MsgStreamId = 2,
    MsgTypeId = 3,
    Timestamp = 4567,
    Payload = crypto:rand_bytes(round(?CHUNK_SIZE_DEFAULT * 2.5)),
    #chunk{id=ChunkId, msg_stream_id=MsgStreamId, msg_type_id=MsgTypeId, timestamp=Timestamp, payload=Payload}.

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

input_chunk_fmt_0(Chunk) ->
    #chunk{id=ChunkId, msg_stream_id=MsgStreamId, msg_type_id=MsgTypeId, timestamp=Timestamp, payload=Payload} = Chunk,
    ChunkBasicHeader = encode_chunk_basic_header(0, ChunkId),
    MessageHeader = <<Timestamp:24, (byte_size(Payload)):24, MsgTypeId:8, MsgStreamId:32/little>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.

input_chunk_fmt_1(Chunk) ->
    #chunk{id=ChunkId, msg_type_id=MsgTypeId, payload=Payload} = Chunk,
    TimestampDelta = 0,
    
    ChunkBasicHeader = encode_chunk_basic_header(1, ChunkId),
    MessageHeader = <<TimestampDelta:24, (byte_size(Payload)):24, MsgTypeId:8>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.

input_chunk_fmt_2(Chunk) ->
    #chunk{id=ChunkId, payload=Payload} = Chunk,
    TimestampDelta = 0,

    ChunkBasicHeader = encode_chunk_basic_header(2, ChunkId),
    MessageHeader = <<TimestampDelta:24>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.
    
input_chunk_fmt_3(Chunk) ->
    #chunk{id=ChunkId, payload=Payload} = Chunk,
    
    ChunkBasicHeader = encode_chunk_basic_header(3, ChunkId),
    MessageHeader = <<>>,
    
    <<ChunkBasicHeader/binary, MessageHeader/binary, (encode_payload(ChunkId, Payload))/binary>>.
