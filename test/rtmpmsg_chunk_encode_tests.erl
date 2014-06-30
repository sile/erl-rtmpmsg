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
