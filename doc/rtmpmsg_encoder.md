

# Module rtmpmsg_encoder #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

RTMP message encoder.

<a name="types"></a>

## Data Types ##




### <a name="type-encoder">encoder()</a> ###


__abstract datatype__: `encoder()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#encode-3">encode/3</a></td><td>Encode RTMP Message.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Return new encoder instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="encode-3"></a>

### encode/3 ###

<pre><code>
encode(Encoder::<a href="#type-encoder">encoder()</a>, ChunkStreamId::<a href="rtmpmsg.md#type-chunk_stream_id">rtmpmsg:chunk_stream_id()</a>, Msg::<a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>) -&gt; {<a href="#type-encoder">encoder()</a>, EncodedData::iolist()}
</code></pre>
<br />

Encode RTMP Message

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-encoder">encoder()</a>
</code></pre>
<br />

Return new encoder instance

