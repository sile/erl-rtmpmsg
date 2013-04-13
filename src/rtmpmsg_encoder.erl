-module(rtmpmsg_encoder).
-export([new/0, encode/3]).

-include("../include/rtmpmsg.hrl").

-define(STATE, ?MODULE).

-record(?STATE,
        {
          chunk_enc = rtmpmsg_chunk_encode:init() :: term() % XXX
        }).

new() ->
    #?STATE{}.

encode(State, ChunkStreamId, Msg) ->
    Enc0 = State#?STATE.chunk_enc,
    Chunk = rtmpmsg_message_encode:encode_to_chunk(ChunkStreamId, Msg),
    {Enc1, EncodedData} = rtmpmsg_chunk_encode:encode(Enc0, Chunk),
    Enc2 = case Msg#rtmpmsg.body of
               #rtmpmsg_set_chunk_size{size=Size} -> rtmpmsg_chunk_encode:set_chunk_size(Enc1, Size);
               _                                  -> Enc1
           end,
    {State#?STATE{chunk_enc=Enc2}, EncodedData}.
