-module(rtmpmsg_decoder).
-export([new/0, decode/2]).

-include("../include/rtmpmsg.hrl").

-define(STATE, ?MODULE).

-record(?STATE,
        {
          chunk_dec = rtmpmsg_chunk_decode:init() :: term() % TODO
        }).

new() ->
    #?STATE{}.

decode(State, Bin) ->
    case rtmpmsg_chunk_decode:decode(State#?STATE.chunk_dec, Bin) of
        {partial, Dec, Bin1} -> 
            {partial, State#?STATE{chunk_dec=Dec}, Bin1};
        {Chunk, Dec, Bin1} ->
            Msg = rtmpmsg_message_decode:decode_chunk(Chunk),
            Dec1 = case Msg of
                       #rtmpmsg_set_chunk_size{size=Size} ->
                           rtmpmsg_chunk_decode:set_chunk_size(Dec, Size);
                       _ ->
                           Dec
                   end,
            {ok, Msg, State#?STATE{chunk_dec=Dec1}, Bin1}
    end.
