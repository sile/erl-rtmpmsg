%% coding: latin-1
-module(rtmpmsg_decoder_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").

%% NOTE: encoderのテストも兼ねてしまっている

decode_test_() ->
    [
     {"基本的なデコード",
      fun () ->
              ChunkStreamId = 4,
              Enc0 = rtmpmsg_encoder:new(),
              Dec0 = rtmpmsg_decoder:new(),
              Msg0 = rtmpmsg:abort(10),

              assertDecode(Enc0, Dec0, ChunkStreamId, Msg0)
      end},
     {"チャンクサイズが途中で変わる場合",
      fun () ->
              ChunkStreamId = 4,
              Enc0 = rtmpmsg_encoder:new(),
              Dec0 = rtmpmsg_decoder:new(),

              %% チャンクサイズ変更前
              Msg0 = rtmpmsg:abort(10),
              {Enc1, Dec1} = assertDecode(Enc0, Dec0, ChunkStreamId, Msg0),

              %% チャンクサイズ変更
              Msg1 = rtmpmsg:set_chunk_size(4),
              {Enc2, Dec2} = assertDecode(Enc1, Dec1, ChunkStreamId, Msg1),
              
              %% チャンクサイズ変更後
              Msg2 = rtmpmsg:data(10, amf0, [<<"string">>, amf:object([{<<"key">>, <<"value">>}])]),
              {_Enc, _Dec} = assertDecode(Enc2, Dec2, ChunkStreamId, Msg2)
      end},
     {"入力が分割されて渡された場合",
      fun () ->
              ChunkStreamId = 4,
              Enc0 = rtmpmsg_encoder:new(),
              Dec0 = rtmpmsg_decoder:new(),
              Msg0 = rtmpmsg:data(10, amf0, [<<"string">>, amf:object([{<<"key">>, <<"value">>}])]),

              {_Enc, Data} = rtmpmsg_encoder:encode(Enc0, ChunkStreamId, Msg0),
              <<Bin1:10/binary, Bin2/binary>> = list_to_binary(Data),
              
              {partial, Dec1} = rtmpmsg_decoder:decode(Dec0, Bin1),
              {ok, _Dec, DecodedMsg, <<"">>} = rtmpmsg_decoder:decode(Dec1, Bin2),
      
              ?assertEqual(Msg0, DecodedMsg)
      end}
    ].

assertDecode(Encoder0, Decoder0, ChunkStreamId, Message) ->
    {Encoder1, Data} = rtmpmsg_encoder:encode(Encoder0, ChunkStreamId, Message),
    {ok, Decoder1, DecodedMessage, <<"">>} = rtmpmsg_decoder:decode(Decoder0, list_to_binary(Data)),
    
    ?assertEqual(Message, DecodedMessage),
    {Encoder1, Decoder1}.

    
