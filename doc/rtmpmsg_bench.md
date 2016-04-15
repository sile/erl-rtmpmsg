

# Module rtmpmsg_bench #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Benchmark Module.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bench-2">bench/2</a></td><td>Do Benchmark.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bench-2"></a>

### bench/2 ###

<pre><code>
bench(Times::non_neg_integer(), Input::binary()) -&gt; [{chunk_decode_time, non_neg_integer()} | {chunk_encode_time, non_neg_integer()} | {message_decode_time, non_neg_integer()} | {message_encode_time, non_neg_integer()} | {input_byte_size, non_neg_integer()} | {chunk_count, non_neg_integer()}]
</code></pre>
<br />

Do Benchmark

```
  > {ok, Input} = file:read_file("data/flat.chunks").
  > rtmpmsg_bench:bench(100, Input).
```

