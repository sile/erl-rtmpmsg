

# Module rtmpmsg #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

RTMP messages and exported types.

<a name="types"></a>

## Data Types ##




### <a name="type-byte_size">byte_size()</a> ###


<pre><code>
byte_size() = non_neg_integer()
</code></pre>




### <a name="type-chunk">chunk()</a> ###


<pre><code>
chunk() = #chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}
</code></pre>




### <a name="type-chunk_size">chunk_size()</a> ###


<pre><code>
chunk_size() = 1..65536
</code></pre>




### <a name="type-chunk_stream_id">chunk_stream_id()</a> ###


<pre><code>
chunk_stream_id() = 2..65599
</code></pre>




### <a name="type-event_buffer_empty">event_buffer_empty()</a> ###


<pre><code>
event_buffer_empty() = #rtmpmsg_event_buffer_empty{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_buffer_ready">event_buffer_ready()</a> ###


<pre><code>
event_buffer_ready() = #rtmpmsg_event_buffer_ready{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_ping_request">event_ping_request()</a> ###


<pre><code>
event_ping_request() = #rtmpmsg_event_ping_request{timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>}
</code></pre>




### <a name="type-event_ping_response">event_ping_response()</a> ###


<pre><code>
event_ping_response() = #rtmpmsg_event_ping_response{timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>}
</code></pre>




### <a name="type-event_set_buffer_length">event_set_buffer_length()</a> ###


<pre><code>
event_set_buffer_length() = #rtmpmsg_event_set_buffer_length{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, length = <a href="rtmpmsg.md#type-milliseconds">rtmpmsg:milliseconds()</a>}
</code></pre>




### <a name="type-event_stream_begin">event_stream_begin()</a> ###


<pre><code>
event_stream_begin() = #rtmpmsg_event_stream_begin{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_stream_dry">event_stream_dry()</a> ###


<pre><code>
event_stream_dry() = #rtmpmsg_event_stream_dry{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_stream_eof">event_stream_eof()</a> ###


<pre><code>
event_stream_eof() = #rtmpmsg_event_stream_eof{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_stream_is_recorded">event_stream_is_recorded()</a> ###


<pre><code>
event_stream_is_recorded() = #rtmpmsg_event_stream_is_recorded{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>}
</code></pre>




### <a name="type-event_unknown">event_unknown()</a> ###


<pre><code>
event_unknown() = #rtmpmsg_event_unknown{type_id = non_neg_integer(), payload = binary()}
</code></pre>




### <a name="type-message">message()</a> ###


<pre><code>
message() = #rtmpmsg{stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, body = undefined | <a href="rtmpmsg.md#type-message_body">rtmpmsg:message_body()</a>}
</code></pre>




### <a name="type-message_body">message_body()</a> ###


<pre><code>
message_body() = <a href="#type-message_body_protocol_control">message_body_protocol_control()</a> | <a href="#type-message_body_audio">message_body_audio()</a> | <a href="#type-message_body_video">message_body_video()</a> | <a href="#type-message_body_command">message_body_command()</a> | <a href="#type-message_body_data">message_body_data()</a> | <a href="#type-message_body_shared_object">message_body_shared_object()</a> | <a href="#type-message_body_aggregate">message_body_aggregate()</a> | <a href="#type-message_body_unknown">message_body_unknown()</a>
</code></pre>




### <a name="type-message_body_abort">message_body_abort()</a> ###


<pre><code>
message_body_abort() = #rtmpmsg_abort{chunk_stream_id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>}
</code></pre>




### <a name="type-message_body_ack">message_body_ack()</a> ###


<pre><code>
message_body_ack() = #rtmpmsg_ack{sequence_number = non_neg_integer()}
</code></pre>




### <a name="type-message_body_aggregate">message_body_aggregate()</a> ###


<pre><code>
message_body_aggregate() = #rtmpmsg_aggregate{messages = [<a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>]}
</code></pre>




### <a name="type-message_body_audio">message_body_audio()</a> ###


<pre><code>
message_body_audio() = #rtmpmsg_audio{data = undefined | binary()}
</code></pre>




### <a name="type-message_body_command">message_body_command()</a> ###


<pre><code>
message_body_command() = #rtmpmsg_command{amf_version = <a href="amf.md#type-amf_version">amf:amf_version()</a>, name = <a href="amf.md#type-amf_string">amf:amf_string()</a>, transaction_id = <a href="amf.md#type-amf_number">amf:amf_number()</a>, object = <a href="amf.md#type-amf_value">amf:amf_value()</a>, args = [<a href="amf.md#type-amf_value">amf:amf_value()</a>]}
</code></pre>




### <a name="type-message_body_data">message_body_data()</a> ###


<pre><code>
message_body_data() = #rtmpmsg_data{amf_version = <a href="amf.md#type-amf_version">amf:amf_version()</a>, values = [<a href="amf.md#type-amf_value">amf:amf_value()</a>]}
</code></pre>




### <a name="type-message_body_protocol_control">message_body_protocol_control()</a> ###


<pre><code>
message_body_protocol_control() = <a href="#type-message_body_set_chunk_size">message_body_set_chunk_size()</a> | <a href="#type-message_body_abort">message_body_abort()</a> | <a href="#type-message_body_ack">message_body_ack()</a> | <a href="#type-message_body_win_ack_size">message_body_win_ack_size()</a> | <a href="#type-message_body_set_peer_bandwidth">message_body_set_peer_bandwidth()</a> | <a href="#type-message_body_user_control">message_body_user_control()</a>
</code></pre>




### <a name="type-message_body_set_chunk_size">message_body_set_chunk_size()</a> ###


<pre><code>
message_body_set_chunk_size() = #rtmpmsg_set_chunk_size{size = non_neg_integer()}
</code></pre>




### <a name="type-message_body_set_peer_bandwidth">message_body_set_peer_bandwidth()</a> ###


<pre><code>
message_body_set_peer_bandwidth() = #rtmpmsg_set_peer_bandwidth{size = non_neg_integer(), limit_type = <a href="rtmpmsg.md#type-set_peer_bandwidth_limit_type">rtmpmsg:set_peer_bandwidth_limit_type()</a>}
</code></pre>




### <a name="type-message_body_shared_object">message_body_shared_object()</a> ###


<pre><code>
message_body_shared_object() = #rtmpmsg_shared_object{amf_version = <a href="amf.md#type-amf_version">amf:amf_version()</a>, payload = binary()}
</code></pre>




### <a name="type-message_body_unknown">message_body_unknown()</a> ###


<pre><code>
message_body_unknown() = #rtmpmsg_unknown{type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, payload = binary()}
</code></pre>




### <a name="type-message_body_user_control">message_body_user_control()</a> ###


<pre><code>
message_body_user_control() = #rtmpmsg_user_control{event = undefined | <a href="rtmpmsg.md#type-user_control_event">rtmpmsg:user_control_event()</a>}
</code></pre>




### <a name="type-message_body_video">message_body_video()</a> ###


<pre><code>
message_body_video() = #rtmpmsg_video{data = undefined | binary()}
</code></pre>




### <a name="type-message_body_win_ack_size">message_body_win_ack_size()</a> ###


<pre><code>
message_body_win_ack_size() = #rtmpmsg_win_ack_size{size = non_neg_integer()}
</code></pre>




### <a name="type-message_stream_id">message_stream_id()</a> ###


<pre><code>
message_stream_id() = non_neg_integer()
</code></pre>




### <a name="type-message_timestamp">message_timestamp()</a> ###


<pre><code>
message_timestamp() = <a href="#type-milliseconds">milliseconds()</a>
</code></pre>




### <a name="type-message_type_id">message_type_id()</a> ###


<pre><code>
message_type_id() = byte()
</code></pre>




### <a name="type-milliseconds">milliseconds()</a> ###


<pre><code>
milliseconds() = non_neg_integer()
</code></pre>




### <a name="type-set_peer_bandwidth_limit_type">set_peer_bandwidth_limit_type()</a> ###


<pre><code>
set_peer_bandwidth_limit_type() = hard | soft | dynamic
</code></pre>




### <a name="type-user_control_event">user_control_event()</a> ###


<pre><code>
user_control_event() = <a href="#type-event_stream_begin">event_stream_begin()</a> | <a href="#type-event_stream_eof">event_stream_eof()</a> | <a href="#type-event_stream_dry">event_stream_dry()</a> | <a href="#type-event_set_buffer_length">event_set_buffer_length()</a> | <a href="#type-event_stream_is_recorded">event_stream_is_recorded()</a> | <a href="#type-event_ping_request">event_ping_request()</a> | <a href="#type-event_ping_response">event_ping_response()</a> | <a href="#type-event_buffer_empty">event_buffer_empty()</a> | <a href="#type-event_buffer_ready">event_buffer_ready()</a> | <a href="#type-event_unknown">event_unknown()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abort-1">abort/1</a></td><td>Make Abort Message.</td></tr><tr><td valign="top"><a href="#ack-1">ack/1</a></td><td>Make Acknowledgement Message.</td></tr><tr><td valign="top"><a href="#aggregate-2">aggregate/2</a></td><td>Make Aggregate Message.</td></tr><tr><td valign="top"><a href="#audio-3">audio/3</a></td><td>Make Audio Message.</td></tr><tr><td valign="top"><a href="#command-6">command/6</a></td><td>Make Command Message.</td></tr><tr><td valign="top"><a href="#data-3">data/3</a></td><td>Make Data Message.</td></tr><tr><td valign="top"><a href="#data-4">data/4</a></td><td>Make Data Message.</td></tr><tr><td valign="top"><a href="#event_buffer_empty-1">event_buffer_empty/1</a></td><td>Make BufferEmptyEvent Message.</td></tr><tr><td valign="top"><a href="#event_buffer_ready-1">event_buffer_ready/1</a></td><td>Make BufferReadyEvent Message.</td></tr><tr><td valign="top"><a href="#event_ping_request-1">event_ping_request/1</a></td><td>Make PingRequestEvent Message.</td></tr><tr><td valign="top"><a href="#event_ping_response-1">event_ping_response/1</a></td><td>Make PingResponseEvent Message.</td></tr><tr><td valign="top"><a href="#event_set_buffer_length-2">event_set_buffer_length/2</a></td><td>Make SetBufferLengthEvent Message.</td></tr><tr><td valign="top"><a href="#event_stream_begin-1">event_stream_begin/1</a></td><td>Make StreamBeginEvent Message.</td></tr><tr><td valign="top"><a href="#event_stream_dry-1">event_stream_dry/1</a></td><td>Make StreamDryEvent Message.</td></tr><tr><td valign="top"><a href="#event_stream_eof-1">event_stream_eof/1</a></td><td>Make StreamEofEvent Message.</td></tr><tr><td valign="top"><a href="#event_stream_is_recorded-1">event_stream_is_recorded/1</a></td><td>Make StreamIsRecordedEvent Message.</td></tr><tr><td valign="top"><a href="#message-3">message/3</a></td><td>Make RTMP message.</td></tr><tr><td valign="top"><a href="#notify_command-4">notify_command/4</a></td><td>Make Command Message (for notify).</td></tr><tr><td valign="top"><a href="#notify_command-5">notify_command/5</a></td><td>Make Command Message (for notify).</td></tr><tr><td valign="top"><a href="#protocol_control_message-1">protocol_control_message/1</a></td><td>Make RTMP Protocol Control Message.</td></tr><tr><td valign="top"><a href="#response_command-4">response_command/4</a></td><td>Make Command Message (for response).</td></tr><tr><td valign="top"><a href="#set_chunk_size-1">set_chunk_size/1</a></td><td>Make SetChunkSize Message.</td></tr><tr><td valign="top"><a href="#set_peer_bandwidth-2">set_peer_bandwidth/2</a></td><td>Make SetPeerBandwidth Message.</td></tr><tr><td valign="top"><a href="#shared_object-3">shared_object/3</a></td><td>Make SharedObject Message (experimental).</td></tr><tr><td valign="top"><a href="#user_control_message-1">user_control_message/1</a></td><td>Make RTMP User Control Message.</td></tr><tr><td valign="top"><a href="#video-3">video/3</a></td><td>Make Video Message.</td></tr><tr><td valign="top"><a href="#win_ack_size-1">win_ack_size/1</a></td><td>Make WindowAcknowledgementSize Message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="abort-1"></a>

### abort/1 ###

<pre><code>
abort(ChunkStreamId::<a href="#type-chunk_stream_id">chunk_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Abort Message

<a name="ack-1"></a>

### ack/1 ###

<pre><code>
ack(SequenceNumber::<a href="#type-byte_size">byte_size()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Acknowledgement Message

<a name="aggregate-2"></a>

### aggregate/2 ###

<pre><code>
aggregate(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Messages::[<a href="#type-message">message()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Aggregate Message

<a name="audio-3"></a>

### audio/3 ###

<pre><code>
audio(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Timestamp::<a href="#type-milliseconds">milliseconds()</a>, X3::binary()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Audio Message

<a name="command-6"></a>

### command/6 ###

<pre><code>
command(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, AmfVersion::<a href="amf.md#type-amf_version">amf:amf_version()</a>, Name::<a href="amf.md#type-amf_string">amf:amf_string()</a>, TransactionId::<a href="amf.md#type-amf_number">amf:amf_number()</a>, Object::<a href="amf.md#type-amf_value">amf:amf_value()</a>, Args::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Command Message

<a name="data-3"></a>

### data/3 ###

<pre><code>
data(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, AmfVersion::<a href="amf.md#type-amf_version">amf:amf_version()</a>, Values::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Data Message

XXX: deprecated

<a name="data-4"></a>

### data/4 ###

<pre><code>
data(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Timestamp::<a href="#type-milliseconds">milliseconds()</a>, AmfVersion::<a href="amf.md#type-amf_version">amf:amf_version()</a>, Values::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Data Message

<a name="event_buffer_empty-1"></a>

### event_buffer_empty/1 ###

<pre><code>
event_buffer_empty(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make BufferEmptyEvent Message

<a name="event_buffer_ready-1"></a>

### event_buffer_ready/1 ###

<pre><code>
event_buffer_ready(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make BufferReadyEvent Message

<a name="event_ping_request-1"></a>

### event_ping_request/1 ###

<pre><code>
event_ping_request(Timestamp::<a href="#type-milliseconds">milliseconds()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make PingRequestEvent Message

<a name="event_ping_response-1"></a>

### event_ping_response/1 ###

<pre><code>
event_ping_response(Timestamp::<a href="#type-milliseconds">milliseconds()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make PingResponseEvent Message

<a name="event_set_buffer_length-2"></a>

### event_set_buffer_length/2 ###

<pre><code>
event_set_buffer_length(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Length::<a href="#type-milliseconds">milliseconds()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make SetBufferLengthEvent Message

<a name="event_stream_begin-1"></a>

### event_stream_begin/1 ###

<pre><code>
event_stream_begin(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make StreamBeginEvent Message

<a name="event_stream_dry-1"></a>

### event_stream_dry/1 ###

<pre><code>
event_stream_dry(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make StreamDryEvent Message

<a name="event_stream_eof-1"></a>

### event_stream_eof/1 ###

<pre><code>
event_stream_eof(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make StreamEofEvent Message

<a name="event_stream_is_recorded-1"></a>

### event_stream_is_recorded/1 ###

<pre><code>
event_stream_is_recorded(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make StreamIsRecordedEvent Message

<a name="message-3"></a>

### message/3 ###

<pre><code>
message(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Timestamp::<a href="#type-message_timestamp">message_timestamp()</a>, Body::<a href="#type-message_body">message_body()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make RTMP message

<a name="notify_command-4"></a>

### notify_command/4 ###

<pre><code>
notify_command(RequestMessage::<a href="#type-message">message()</a>, Name::<a href="amf.md#type-amf_string">amf:amf_string()</a>, Object::<a href="amf.md#type-amf_value">amf:amf_value()</a>, Args::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Command Message (for notify)

<a name="notify_command-5"></a>

### notify_command/5 ###

<pre><code>
notify_command(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, AmfVersion::<a href="amf.md#type-amf_version">amf:amf_version()</a>, Name::<a href="amf.md#type-amf_string">amf:amf_string()</a>, Object::<a href="amf.md#type-amf_value">amf:amf_value()</a>, Args::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Command Message (for notify)

<a name="protocol_control_message-1"></a>

### protocol_control_message/1 ###

<pre><code>
protocol_control_message(Body::<a href="#type-message_body_protocol_control">message_body_protocol_control()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make RTMP Protocol Control Message

<a name="response_command-4"></a>

### response_command/4 ###

<pre><code>
response_command(RequestMessage::<a href="#type-message">message()</a>, Name::<a href="amf.md#type-amf_string">amf:amf_string()</a>, Object::<a href="amf.md#type-amf_value">amf:amf_value()</a>, Args::[<a href="amf.md#type-amf_value">amf:amf_value()</a>]) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Command Message (for response)

<a name="set_chunk_size-1"></a>

### set_chunk_size/1 ###

<pre><code>
set_chunk_size(ChunkSize::<a href="#type-chunk_size">chunk_size()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make SetChunkSize Message

<a name="set_peer_bandwidth-2"></a>

### set_peer_bandwidth/2 ###

<pre><code>
set_peer_bandwidth(WindowSize::<a href="#type-byte_size">byte_size()</a>, LimitType::<a href="#type-set_peer_bandwidth_limit_type">set_peer_bandwidth_limit_type()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make SetPeerBandwidth Message

<a name="shared_object-3"></a>

### shared_object/3 ###

<pre><code>
shared_object(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, AmfVersion::<a href="amf.md#type-amf_version">amf:amf_version()</a>, Payload::binary()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make SharedObject Message (experimental)

<a name="user_control_message-1"></a>

### user_control_message/1 ###

<pre><code>
user_control_message(Event::<a href="#type-user_control_event">user_control_event()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make RTMP User Control Message

<a name="video-3"></a>

### video/3 ###

<pre><code>
video(StreamId::<a href="#type-message_stream_id">message_stream_id()</a>, Timestamp::<a href="#type-milliseconds">milliseconds()</a>, X3::binary()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make Video Message

<a name="win_ack_size-1"></a>

### win_ack_size/1 ###

<pre><code>
win_ack_size(WindowSize::<a href="#type-byte_size">byte_size()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Make WindowAcknowledgementSize Message

