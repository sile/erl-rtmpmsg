

# Module rtmpmsg_decoder #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

RTMP message decoder.

<a name="types"></a>

## Data Types ##




### <a name="type-decoder">decoder()</a> ###


__abstract datatype__: `decoder()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td>Decode RTMP Message.</td></tr><tr><td valign="top"><a href="#decode_all-2">decode_all/2</a></td><td>Decode RTMP Messages.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Return new decoder instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(Decoder::<a href="#type-decoder">decoder()</a>, X2::binary()) -&gt; {ok, <a href="#type-decoder">decoder()</a>, <a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>, RestBin::binary()} | {partial, <a href="#type-decoder">decoder()</a>}
</code></pre>
<br />

Decode RTMP Message

If decoded message is #rtmpmsg_set_chunk_size{} or #rtmpmsg_abort{}, it will be automatically handled in this function.

<a name="decode_all-2"></a>

### decode_all/2 ###

<pre><code>
decode_all(Decoder::<a href="#type-decoder">decoder()</a>, X2::binary()) -&gt; {<a href="#type-decoder">decoder()</a>, [<a href="rtmpmsg.md#type-message">rtmpmsg:message()</a>]}
</code></pre>
<br />

Decode RTMP Messages

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-decoder">decoder()</a>
</code></pre>
<br />

Return new decoder instance

