%%% @doc RTMP messages and exported types
%%% @end
%%%
%%%
%%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmpmsg).
-include("../include/rtmpmsg.hrl").
-include("../include/internal/rtmpmsg_internal.hrl").


%% RTMP Message Construct API
-export([
         message/3,
         protocol_control_message/1,
         user_control_message/1,
         
         set_chunk_size/1,
         abort/1,
         ack/1,
         win_ack_size/1,
         set_peer_bandwidth/2,
         
         event_stream_begin/1,
         event_stream_eof/1,
         event_stream_dry/1,
         event_set_buffer_length/2,
         event_stream_is_recorded/1,
         event_ping_request/1,
         event_ping_response/1,
         event_buffer_empty/1,
         event_buffer_ready/1,

         audio/3,
         video/3,

         command/6,
         response_command/4, 
         notify_command/4, notify_command/5,

         data/3,
         shared_object/3,
         aggregate/2
        ]).

%% Exported Types
-export_type([
              byte_size/0,
              milliseconds/0,
              
              chunk_size/0,
              chunk_stream_id/0,
              message_stream_id/0,
              rtmp_timestamp/0,
              
              message/0,
              message_body/0,
              message_body_protocol_control/0,
              user_control_event/0,
              set_peer_bandwidth_limit_type/0
             ]).

%%================================================================================
%% Types
%%================================================================================
-type byte_size()    :: non_neg_integer().
-type milliseconds() :: non_neg_integer().

-type chunk_size()        :: 1..?CHUNK_SIZE_MAX.
-type chunk_stream_id()   :: 2..65599.
-type message_stream_id() :: non_neg_integer().
-type message_type_id()   :: non_neg_integer().
-type rtmp_timestamp()    :: milliseconds().

-type message() :: #rtmpmsg{}.
-type message_body() :: message_body_protocol_control() |
                        #rtmpmsg_audio{} |
                        #rtmpmsg_video{} |
                        #rtmpmsg_command{} |
                        #rtmpmsg_data{} |
                        #rtmpmsg_shared_obejct{} |
                        #rtmpmsg_aggregate{} |
                        #rtmpmsg_unknown{}.
-type message_body_protocol_control() :: #rtmpmsg_set_chunk_size{} |
                                         #rtmpmsg_abort{} |
                                         #rtmpmsg_ack{} |
                                         #rtmpmsg_set_peer_bandwidth{} |
                                         #rtmpmsg_user_control{}.
-type user_control_event() :: #rtmpmsg_event_stream_begin{} |
                              #rtmpmsg_event_stream_eof{} |
                              #rtmpmsg_event_stream_dry{} |
                              #rtmpmsg_event_set_buffer_length{} |
                              #rtmpmsg_event_stream_is_recorded{} |
                              #rtmpmsg_event_ping_request{} |
                              #rtmpmsg_event_ping_response{} |
                              #rtmpmsg_event_buffer_empty{} |
                              #rtmpmsg_event_buffer_ready{} |
                              #rtmpmsg_event_unknown{}.

-type set_peer_bandwidth_limit_type() :: hard | soft | dynamic.

%%================================================================================
%% RTMP Message Construct API
%%================================================================================

%% @doc Make RTMP message
-spec message(message_stream_id(), rtmp_timestamp(), message_body()) -> message().
message(StreamId, Timestamp, Body) ->
    #rtmpmsg
    {
      type_id   = get_type_id(Body),
      stream_id = StreamId,
      timestamp = Timestamp,
      body      = Body
    }.

%% @doc Make RTMP Protocol Control Message
-spec protocol_control_message(message_body_protocol_control()) -> message().
protocol_control_message(Body) -> message(?PCM_MESSAGE_STREAM_ID, 0, Body).

%% @doc Make RTMP User Control Message
-spec user_control_message(user_control_event()) -> message().
user_control_message(Event) -> protocol_control_message(#rtmpmsg_user_control{event=Event}).

%% @doc Make SetChunkSize Message
-spec set_chunk_size(chunk_size()) -> message().
set_chunk_size(ChunkSize) -> protocol_control_message(#rtmpmsg_set_chunk_size{size=ChunkSize}).

%% @doc Make Abort Message
-spec abort(chunk_stream_id()) -> message().
abort(ChunkStreamId) -> protocol_control_message(#rtmpmsg_abort{chunk_stream_id=ChunkStreamId}).

%% @doc Make Acknowledgement Message
-spec ack(byte_size()) -> message().
ack(SequenceNumber) -> protocol_control_message(#rtmpmsg_ack{sequence_number=SequenceNumber}).

%% @doc Make WindowAcknowledgementSize Message
-spec win_ack_size(byte_size()) -> message().
win_ack_size(WindowSize) -> protocol_control_message(#rtmpmsg_win_ack_size{size=WindowSize}).

%% @doc Make SetPeerBandwidth Message
-spec set_peer_bandwidth(byte_size(), set_peer_bandwidth_limit_type()) -> message().
set_peer_bandwidth(WindowSize, LimitType) -> protocol_control_message(#rtmpmsg_set_peer_bandwidth{size=WindowSize, limit_type=LimitType}).

%% @doc Make StreamBeginEvent Message
-spec event_stream_begin(message_stream_id()) -> message().
event_stream_begin(StreamId) -> user_control_message(#rtmpmsg_event_stream_begin{stream_id=StreamId}).

%% @doc Make StreamEofEvent Message
-spec event_stream_eof(message_stream_id()) -> message().
event_stream_eof(StreamId) -> user_control_message(#rtmpmsg_event_stream_eof{stream_id=StreamId}).

%% @doc Make StreamDryEvent Message
-spec event_stream_dry(message_stream_id()) -> message().
event_stream_dry(StreamId) -> user_control_message(#rtmpmsg_event_stream_dry{stream_id=StreamId}).

%% @doc Make SetBufferLengthEvent Message
-spec event_set_buffer_length(message_stream_id(), milliseconds()) -> message().
event_set_buffer_length(StreamId, Length) -> user_control_message(#rtmpmsg_event_set_buffer_length{stream_id=StreamId, length=Length}).

%% @doc Make StreamIsRecordedEvent Message
-spec event_stream_is_recorded(message_stream_id()) -> message().
event_stream_is_recorded(StreamId) -> user_control_message(#rtmpmsg_event_stream_is_recorded{stream_id=StreamId}).

%% @doc Make PingRequestEvent Message
-spec event_ping_request(milliseconds()) -> message().
event_ping_request(Timestamp) -> user_control_message(#rtmpmsg_event_ping_request{timestamp=Timestamp}).

%% @doc Make PingResponseEvent Message
-spec event_ping_response(milliseconds()) -> message().
event_ping_response(Timestamp) -> user_control_message(#rtmpmsg_event_ping_response{timestamp=Timestamp}).

%% @doc Make BufferEmptyEvent Message
-spec event_buffer_empty(message_stream_id()) -> message().
event_buffer_empty(StreamId) -> user_control_message(#rtmpmsg_event_buffer_empty{stream_id=StreamId}).

%% @doc Make BufferReadyEvent Message
-spec event_buffer_ready(message_stream_id()) -> message().
event_buffer_ready(StreamId) -> user_control_message(#rtmpmsg_event_buffer_ready{stream_id=StreamId}).

%% @doc Make Audio Message
-spec audio(message_stream_id(), milliseconds(), flv:tag_audio()) -> message().
audio(StreamId, Timestamp, AudioData) -> message(StreamId, Timestamp, #rtmpmsg_audio{data=AudioData}).

%% @doc Make Video Message
-spec video(message_stream_id(), milliseconds(), flv:tag_video()) -> message().
video(StreamId, Timestamp, VideoData) -> message(StreamId, Timestamp, #rtmpmsg_video{data=VideoData}).

%% @doc Make Command Message
-spec command(message_stream_id(), amf:amf_version(), amf:amf_string(), amf:amf_number(), amf:amf_value(), [amf:amf_value()]) -> message().
command(StreamId, AmfVersion, Name, TransactionId, Object, Args) ->
    message(StreamId, 0, #rtmpmsg_command{amf_version=AmfVersion, 
                                          name=Name,
                                          transaction_id=TransactionId,
                                          object=Object,
                                          args=Args}).

%% @doc Make Command Message (for response)
-spec response_command(message(), amf:amf_string(), amf:amf_value(), [amf:amf_value()]) -> message().
response_command(RequestMessage, Name, Object, Args) ->
    #rtmpmsg{stream_id=StreamId, body=Cmd} = RequestMessage,
    #rtmpmsg_command{amf_version=AmfVer, transaction_id=TransactionId} = Cmd,
    command(StreamId, AmfVer, Name, TransactionId, Object, Args).

%% @doc Make Command Message (for notify)
-spec notify_command(message(), amf:amf_string(), amf:amf_value(), [amf:amf_value()]) -> message().
notify_command(RequestMessage, Name, Object, Args) ->
    #rtmpmsg{stream_id=StreamId, body=Cmd} = RequestMessage,
    #rtmpmsg_command{amf_version=AmfVer} = Cmd,
    notify_command(StreamId, AmfVer, Name, Object, Args).

%% @doc Make Command Message (for notify)
-spec notify_command(message_stream_id(), amf:amf_version(), amf:amf_string(), amf:amf_value(), [amf:amf_value()]) -> message().
notify_command(StreamId, AmfVersion, Name, Object, Args) -> command(StreamId, AmfVersion, Name, 0, Object, Args).

%% @doc Make Data Message
-spec data(message_stream_id(), amf:amf_version(), [amf:amf_value()]) -> message().
data(StreamId, AmfVersion, Values) -> message(StreamId, 0, #rtmpmsg_data{amf_version=AmfVersion, values=Values}).

%% @doc Make SharedObject Message (experimental)
-spec shared_object(message_stream_id(), amf:amf_version(), binary()) -> message().
shared_object(StreamId, AmfVersion, Payload) -> message(StreamId, 0, #rtmpmsg_shared_obejct{amf_version=AmfVersion, payload=Payload}).

%% @doc Make Aggregate Message
-spec aggregate(message_stream_id(), [message()]) -> message().
aggregate(StreamId, Messages) -> message(StreamId, 0, #rtmpmsg_aggregate{messages = Messages}).

%%================================================================================
%% Internal Functions
%%================================================================================
-spec get_type_id(message_body()) -> message_type_id().
get_type_id(#rtmpmsg_set_chunk_size{})                -> ?TYPE_SET_CHUNK_SIZE;
get_type_id(#rtmpmsg_abort{})                         -> ?TYPE_ABORT;
get_type_id(#rtmpmsg_ack{})                           -> ?TYPE_ACK;
get_type_id(#rtmpmsg_win_ack_size{})                  -> ?TYPE_WIN_ACK_SIZE;
get_type_id(#rtmpmsg_set_peer_bandwidth{})            -> ?TYPE_SET_PEER_BANDWIDTH;
get_type_id(#rtmpmsg_user_control{})                  -> ?TYPE_USER_CONTROL;
get_type_id(#rtmpmsg_audio{})                         -> ?TYPE_AUDIO;
get_type_id(#rtmpmsg_video{})                         -> ?TYPE_VIDEO;
get_type_id(#rtmpmsg_command{amf_version=amf0})       -> ?TYPE_COMMAND_AMF0;
get_type_id(#rtmpmsg_command{amf_version=amf3})       -> ?TYPE_COMMAND_AMF3;
get_type_id(#rtmpmsg_data{amf_version=amf0})          -> ?TYPE_DATA_AMF0;
get_type_id(#rtmpmsg_data{amf_version=amf3})          -> ?TYPE_DATA_AMF3;
get_type_id(#rtmpmsg_shared_obejct{amf_version=amf0}) -> ?TYPE_SHARED_OBJECT_AMF0;
get_type_id(#rtmpmsg_shared_obejct{amf_version=amf3}) -> ?TYPE_SHARED_OBJECT_AMF3;
get_type_id(#rtmpmsg_aggregate{})                     -> ?TYPE_AGGREGATE;
get_type_id(#rtmpmsg_unknown{type_id=TypeId})         -> TypeId.
