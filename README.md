# ledis

*An LFE Redis Client Library*

<img src="resources/logos/ButterCrunchLettuce-2-small.png" />

## Table of Contents

* [Dependences](#dependences-)
* [Installtion and Setup](#installtion-and-setup-)
* [Usage](#usage-)
* [Difference from Redis CLI](#difference-from-redis-cli-)


## Dependences [&#x219F;](#table-of-contents)

You will need the following installed on your system:

* Erlang
* Redis
* rebar

Additionally, the ledis ``Makefile`` sets up the following dependencies for you
automatically when you run the ``compile`` target:

* LFE
* eredis
* ltest


## Installtion and Setup [&#x219F;](#table-of-contents)

Here's what you need to do:

```bash
$ git clone https://github.com/lfex/ledis.git
$ cd ledis
$ make compile
```

At this point, you will be able to run an LFE REPL (shell):

```bash
$ make repl
```


## Usage [&#x219F;](#table-of-contents)

To use ledis from the shell, just do this:

```bash
$ make repl-no-deps
```
```cl
Erlang R15B03 (erts-5.9.3) [source] [smp:8:8] ...

LFE Shell V5.9.3 (abort with ^G)
>
```
```cl
> (ledis:start-link)
true
> (ledis:get 'foo)
#(ok undefined)
> (ledis:set 'foo 'bar)
#(ok #B(79 75))
> (ledis:get 'foo)
#(ok #B(118 97 108 117 101))
```

You may also provide an option to convert all results to string values:

```cl
> (ledis:set 'foo 'bar '(#(return-type string)))
#(ok "OK")
> (ledis:get 'foo '(#(return-type string)))
#(ok "bar")
```

If you would like to receive string values by default, simply update either
your project's ``lfe.config`` or your ``~/.lfe/lfe.config`` file with the
following:

```cl
#(ledis
  (#(client-process-name ledis-client)
   #(return-type string)))
```


### Difference from Redis CLI [&#x219F;](#table-of-contents)

Since LFE is a fixed-arity language, a few differences exist between ledis and
the Redis CLI. These usually are for the following scenarios:

1. Optional command arguments which have been converted to LFE as options (using
   proplists).
1. Some Redis commands conflict with LFE functions/macros, and these have been
   renamed.
1. Some variable arity commands are converted to single-arity in LFE where the
   single argument is a list.

Functions which are different in these ways have been listed below with sample
usage.

#### ``bitcount`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:bitcount 'foo)
#(ok "10")
> (ledis:bitcount 'foo '(#(start 2) #(end 4)))
#(ok "4")
```

#### ``bitop`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:set 'key1 "foobar")
#(ok "OK")
> (ledis:set 'key2 "absdef")
#(ok "OK")
> (ledis:bitop 'AND 'dest '(key1 key2))
#(ok "6")
> (ledis:get 'dest)
#(ok "`bc`ab")
```

#### ``bitpos`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:set 'mykey #b(#xff #xf0 #x00))
#(ok "OK")
> (ledis:bitpos 'mykey 0)
#(ok "12")
> (ledis:set 'mykey #b(#x00 #xff #xf0))
#(ok "OK")
> (ledis:bitpos 'mykey 1 '(#(start 1)))
#(ok "8")
> (ledis:bitpos 'mykey 1 '(#(start 2)))
#(ok "16")
> (ledis:set 'mykey #b(#x00 #x00 #x00))
#(ok "OK")
> (ledis:bitpos 'mykey 1)
#(ok "-1")
```

#### ``del`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:set 'mykey "Hello")
#(ok "OK")
> (ledis:del 'mykey)
#(ok "1")
> (ledis:multi-set '(key1 "val1" key2 "val2" key3 "val3"))
#(ok "OK")
> (ledis:del '(key1 key2 key3 key4))
#(ok "3")
```

#### ``M*`` renamed to ``multi-*`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:multi-set '(a 10 b 20 c 30))
#(ok "OK")
> (ledis:multi-get '(a b c))
#(ok ("10" "20" "30"))
```

#### ``set`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:set 'mykey "athirdvalue" '(#(xx) #(px 10000)))
#(ok "OK")
> (ledis:get 'mykey)
#(ok "athirdvalue")
> (timer:sleep 10000)
ok
> (ledis:get 'mykey)
#(ok undefined)
```
