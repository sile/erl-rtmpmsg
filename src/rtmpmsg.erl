-module(rtmpmsg).

-export_type([
              chunk_size/0
             ]).

-include("../include/internal/rtmpmsg_internal.hrl").

-type chunk_size() :: 1..?CHUNK_SIZE_MAX.


