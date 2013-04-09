-module(rtmpmsg_message_decode).
-export([decode/5]).

decode(ChunkStreamId, MessageStreamId, MessageTypeId, Timestamp, Payload) ->
    {todo, ChunkStreamId, MessageStreamId, MessageTypeId, Timestamp, Payload}.

