

# Module rtmpmsg_util #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Utility Functions (for debug purpose).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_chunks-1">decode_chunks/1</a></td><td>Equivalent to <a href="#decode_chunks-2"><tt>decode_chunks(Input, rtmpmsg_chunk_decode:init())</tt></a>.</td></tr><tr><td valign="top"><a href="#decode_chunks-2">decode_chunks/2</a></td><td>Decode RTMP Chunks from Binary.</td></tr><tr><td valign="top"><a href="#decode_messages-1">decode_messages/1</a></td><td>Decode RTMP Messages from Chunks.</td></tr><tr><td valign="top"><a href="#encode_chunks-1">encode_chunks/1</a></td><td>Equivalent to <a href="#encode_chunks-2"><tt>encode_chunks(Input, rtmpmsg_chunk_encode:init())</tt></a>.</td></tr><tr><td valign="top"><a href="#encode_chunks-2">encode_chunks/2</a></td><td>Encode RTMP Chunks to Binary.</td></tr><tr><td valign="top"><a href="#encode_messages-1">encode_messages/1</a></td><td>Encode RTMP Messages to Chunks.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode_chunks-1"></a>

### decode_chunks/1 ###

<pre><code>
decode_chunks(Input::binary()) -&gt; [#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}]
</code></pre>
<br />

Equivalent to [`decode_chunks(Input, rtmpmsg_chunk_decode:init())`](#decode_chunks-2).

<a name="decode_chunks-2"></a>

### decode_chunks/2 ###

<pre><code>
decode_chunks(Input::binary(), Decoder0::<a href="rtmpmsg_chunk_decode.md#type-state">rtmpmsg_chunk_decode:state()</a>) -&gt; [#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}]
</code></pre>
<br />

Decode RTMP Chunks from Binary

<a name="decode_messages-1"></a>

### decode_messages/1 ###

<pre><code>
decode_messages(Chunk::[#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}]) -&gt; [<a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>]
</code></pre>
<br />

Decode RTMP Messages from Chunks

<a name="encode_chunks-1"></a>

### encode_chunks/1 ###

<pre><code>
encode_chunks(Chunk::[#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}]) -&gt; binary()
</code></pre>
<br />

Equivalent to [`encode_chunks(Input, rtmpmsg_chunk_encode:init())`](#encode_chunks-2).

<a name="encode_chunks-2"></a>

### encode_chunks/2 ###

<pre><code>
encode_chunks(Chunk::[#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}], Encoder0::<a href="rtmpmsg_chunk_encode.md#type-state">rtmpmsg_chunk_encode:state()</a>) -&gt; binary()
</code></pre>
<br />

Encode RTMP Chunks to Binary

<a name="encode_messages-1"></a>

### encode_messages/1 ###

<pre><code>
encode_messages(Messages::[<a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>]) -&gt; [#chunk{id = <a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, msg_stream_id = <a href="rtmpmsg.md#type-message_stream_id">rtmpmsg:message_stream_id()</a>, msg_type_id = <a href="rtmpmsg.md#type-message_type_id">rtmpmsg:message_type_id()</a>, timestamp = <a href="rtmpmsg.md#type-message_timestamp">rtmpmsg:message_timestamp()</a>, payload = binary()}]
</code></pre>
<br />

Encode RTMP Messages to Chunks

