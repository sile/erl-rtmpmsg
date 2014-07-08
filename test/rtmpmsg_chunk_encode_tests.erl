%% coding: latin-1
-module(rtmpmsg_chunk_encode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

-define(FMT_0(ChunkIdLessThan64, TimestampLessThan4bytes, Type, StreamId, Data),
        (<<0:2, ChunkIdLessThan64:6, TimestampLessThan4bytes:24,
           (byte_size(Data)):24, Type:8, StreamId:32/little, Data/binary>>)/binary).

-define(FMT_1(ChunkIdLessThan64, TimestampDeltaLessThan4bytes, Type, Data),
        (<<1:2, ChunkIdLessThan64:6, TimestampDeltaLessThan4bytes:24,
           (byte_size(Data)):24, Type:8, Data/binary>>)/binary).

-define(FMT_2(ChunkIdLessThan64, TimestampDeltaLessThan4bytes, Data),
        (<<2:2, ChunkIdLessThan64:6, TimestampDeltaLessThan4bytes:24, Data/binary>>)/binary).

-define(FMT_3(ChunkIdLessThan64, Data),
        (<<3:2, ChunkIdLessThan64:6, Data/binary>>)/binary).

-define(FMT_EXT_0(ChunkIdLessThan64, ExtendedTimestamp, Type, StreamId, Data),
        (<<0:2, ChunkIdLessThan64:6, 16#FFFFFF:24, (byte_size(Data)):24,
           Type:8, StreamId:32/little, ExtendedTimestamp:32, Data/binary>>)/binary).

-define(FMT_EXT_1(ChunkIdLessThan64, ExtendedTimestampDelta, Type, Data),
        (<<1:2, ChunkIdLessThan64:6, 16#FFFFFF:24, (byte_size(Data)):24,
           Type:8, ExtendedTimestampDelta:32, Data/binary>>)/binary).

-define(FMT_EXT_2(ChunkIdLessThan64, ExtendedTimestampDelta, Data),
        (<<2:2, ChunkIdLessThan64:6, 16#FFFFFF:24, ExtendedTimestampDelta:32, Data/binary>>)/binary).

-define(FMT_EXT_3(ChunkIdLessThan64, ExtendedTimestampDelta, Data),
        (<<3:2, ChunkIdLessThan64:6, ExtendedTimestampDelta:32, Data/binary>>)/binary).

encode_test_() ->
    [
     {"基本的なチャンクデータのエンコードができる",
      fun () ->
              InputChunk = input_chunk(),
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk]))
      end},
     {"連続したチャンクデータのエンコードができる: fmt0 => fmt1",
      fun () ->
              InputChunk0 = input_chunk(),
              InputChunk1 = InputChunk0#chunk{msg_type_id = 200},
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_1(4, 0, 200, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1]))
      end},
     {"連続したチャンクデータのエンコードができる: fmt0 => fmt2",
      fun () ->
              InputChunk0 = input_chunk(),
              InputChunk1 = InputChunk0#chunk{timestamp = 700},
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_2(4, 400, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1]))
      end},
     {"連続したチャンクデータのエンコードができる: fmt0 => fmt2 => fmt3 => fmt3",
      fun () ->
              InputChunk = input_chunk(),
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_2(4, 0, <<"abc">>),
                           ?FMT_3(4, <<"abc">>),
                           ?FMT_3(4, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk, InputChunk, InputChunk, InputChunk]))
      end},
     {"連続したチャンクデータのエンコードができる: fmt0 => fmt3 => fmt3",
      fun () ->
              InputChunk0 = input_chunk(),
              InputChunk1 = InputChunk0#chunk{timestamp = InputChunk0#chunk.timestamp + InputChunk0#chunk.timestamp},
              InputChunk2 = InputChunk1#chunk{timestamp = InputChunk1#chunk.timestamp + InputChunk0#chunk.timestamp},
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_3(4, <<"abc">>),
                           ?FMT_3(4, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1, InputChunk2]))
      end},
     {"連続したチャンクデータのエンコードができる: fmt0 => fmt1 => fmt2 => fmt3",
      fun () ->
              InputChunk0 = input_chunk(),
              InputChunk1 = InputChunk0#chunk{msg_type_id = 200},
              InputChunk2 = InputChunk1#chunk{timestamp = InputChunk1#chunk.timestamp + 400},
              InputChunk3 = InputChunk2#chunk{timestamp = InputChunk2#chunk.timestamp + 400},
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_1(4, 0, 200, <<"abc">>),
                           ?FMT_2(4, 400, <<"abc">>),
                           ?FMT_3(4, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1, InputChunk2, InputChunk3]))
      end},
     {"連続したチャンクデータのデコードができる: fmt0 => fmt2 => fmt0 => fmt1",
      fun () ->
              InputChunk0 = input_chunk(),
              InputChunk1 = InputChunk0#chunk{timestamp = 700},
              InputChunk2 = InputChunk1#chunk{timestamp = 100},
              InputChunk3 = InputChunk2#chunk{payload = <<"a">>},
              Expected = <<?FMT_0(4, 300, 3, 2, <<"abc">>),
                           ?FMT_2(4, 400, <<"abc">>),
                           ?FMT_0(4, 100, 3, 2, <<"abc">>),
                           ?FMT_1(4, 0, 3, <<"a">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1, InputChunk2, InputChunk3]))
      end},
     {"ExtendedTimestampの取り扱い: fmt0 => fmt3",
      fun () ->
              Timestamp = 16#12345678, % 16#FFFFFF 以上の値
              InputChunk0 = (input_chunk())#chunk{timestamp = Timestamp},
              InputChunk1 = InputChunk0,
              Expected = <<?FMT_EXT_0(4, Timestamp, 3, 2, <<"abc">>),
                           ?FMT_EXT_3(4, Timestamp, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1]))
      end},
     {"ExtendedTimestampの取り扱い(境界値): fmt0 => fmt3",
      fun () ->
              %% 0xFFFFFFちょうどのケース
              Timestamp0 = 16#FFFFFF,
              InputChunk0 = (input_chunk())#chunk{timestamp = Timestamp0},
              Expected0 = <<?FMT_EXT_0(4, Timestamp0, 3, 2, <<"abc">>),
                            ?FMT_EXT_3(4, Timestamp0, <<"abc">>)>>,
              ?assertEqual(Expected0, encode_chunks([InputChunk0, InputChunk0])),

              %% 1小さい
              Timestamp1 = Timestamp0 - 1,
              InputChunk1 = (input_chunk())#chunk{timestamp = Timestamp1},
              Expected1 = <<?FMT_0(4, Timestamp1, 3, 2, <<"abc">>),
                            ?FMT_3(4, <<"abc">>)>>,
              ?assertEqual(Expected1, encode_chunks([InputChunk1, InputChunk1])),

              %% 1大きい
              Timestamp2 = Timestamp0 + 1,
              InputChunk2 = (input_chunk())#chunk{timestamp = Timestamp2},
              Expected2 = <<?FMT_EXT_0(4, Timestamp2, 3, 2, <<"abc">>),
                            ?FMT_EXT_3(4, Timestamp2, <<"abc">>)>>,
              ?assertEqual(Expected2, encode_chunks([InputChunk2, InputChunk2]))
      end},
     {"ExtendedTimestampの取り扱い: fmt0 => fmt1 => fmt3",
      fun () ->
              TimestampBase  = 300,
              TimestampDelta = 16#123456, % 16#FFFFFF 以上の差
              InputChunk0 = (input_chunk())#chunk{timestamp = TimestampBase},
              InputChunk1 = InputChunk0#chunk{msg_type_id = 9, timestamp = TimestampBase + TimestampDelta},
              InputChunk2 = InputChunk1#chunk{timestamp = TimestampBase + TimestampDelta + TimestampDelta},
              Expected = <<?FMT_0    (4, TimestampBase, 3, 2, <<"abc">>),
                           ?FMT_EXT_1(4, TimestampDelta, 9, <<"abc">>),
                           ?FMT_EXT_3(4, TimestampDelta, <<"abc">>)>>,
              ?assertEqual(Expected, encode_chunks([InputChunk0, InputChunk1, InputChunk2]))
      end},
     {"ExtendedTimestampの取り扱い(境界値): fmt0 => fmt1 => fmt3",
      fun () ->
              TimestampBase  = 300,

              %% 16#FFFFFF ちょうどのケース
              TimestampDelta0 = 16#FFFFFF,
              InputChunk0_0 = (input_chunk())#chunk{timestamp = TimestampBase},
              InputChunk0_1 = InputChunk0_0#chunk{msg_type_id = 9, timestamp = TimestampBase + TimestampDelta0},
              InputChunk0_2 = InputChunk0_1#chunk{timestamp = TimestampBase + TimestampDelta0 + TimestampDelta0},
              Expected0 = <<?FMT_0    (4, TimestampBase, 3, 2, <<"abc">>),
                            ?FMT_EXT_1(4, TimestampDelta0, 9, <<"abc">>),
                            ?FMT_EXT_3(4, TimestampDelta0, <<"abc">>)>>,
              ?assertEqual(Expected0, encode_chunks([InputChunk0_0, InputChunk0_1, InputChunk0_2])),

              %% 1小さい
              TimestampDelta1 = TimestampDelta0 - 1,
              InputChunk1_0 = (input_chunk())#chunk{timestamp = TimestampBase},
              InputChunk1_1 = InputChunk1_0#chunk{msg_type_id = 9, timestamp = TimestampBase + TimestampDelta1},
              InputChunk1_2 = InputChunk1_1#chunk{timestamp = TimestampBase + TimestampDelta1 + TimestampDelta1},
              Expected1 = <<?FMT_0    (4, TimestampBase, 3, 2, <<"abc">>),
                            ?FMT_1(4, TimestampDelta1, 9, <<"abc">>),
                            ?FMT_3(4, <<"abc">>)>>,
              ?assertEqual(Expected1, encode_chunks([InputChunk1_0, InputChunk1_1, InputChunk1_2])),

              %% 1大きい
              TimestampDelta2 = TimestampDelta0 + 1,
              InputChunk2_0 = (input_chunk())#chunk{timestamp = TimestampBase},
              InputChunk2_1 = InputChunk2_0#chunk{msg_type_id = 9, timestamp = TimestampBase + TimestampDelta2},
              InputChunk2_2 = InputChunk2_1#chunk{timestamp = TimestampBase + TimestampDelta2 + TimestampDelta2},
              Expected2 = <<?FMT_0    (4, TimestampBase, 3, 2, <<"abc">>),
                            ?FMT_EXT_1(4, TimestampDelta2, 9, <<"abc">>),
                            ?FMT_EXT_3(4, TimestampDelta2, <<"abc">>)>>,
              ?assertEqual(Expected2, encode_chunks([InputChunk2_0, InputChunk2_1, InputChunk2_2]))
      end}
    ].

input_chunk() ->
    #chunk{id            = 4,
           msg_stream_id = 2,
           msg_type_id   = 3,
           timestamp     = 300,
           payload       = <<"abc">>}.

encode_chunks(Chunks) ->
    element(2, encode_chunks(rtmpmsg_chunk_encode:init(), Chunks)).

encode_chunks(InitEnc, Chunks) ->
    {LastEnc, EncodedData} =
        lists:foldl(fun (Chunk, {AccEnc, AccBin}) ->
                            {Enc, Bin} = rtmpmsg_chunk_encode:encode(AccEnc, Chunk),
                            {Enc, AccBin ++ Bin}
                    end,
                    {InitEnc, []},
                    Chunks),
    {LastEnc, list_to_binary(EncodedData)}.
