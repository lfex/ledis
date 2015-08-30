# ledis

*An LFE Redis Client Library*

<img src="resources/logos/ButterCrunchLettuce-2-small.png" />

## Table of Contents

* [Dependences](#dependences-)
* [Installtion and Setup](#installtion-and-setup-)
* [Usage](#usage-)
  * [Difference from Redis CLI](#difference-from-redis-cli-)
* [A Note for Developers](#a-note-for-developers-)


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

#### ``blpop`` [&#x219F;](#table-of-contents)

```lisp
> (ledis:blpop 'list1 0)
#(ok ("list1" "banana"))
> (ledis:blpop '(list2 list3 list1) 0)
#(ok ("list1" "tranbar"))
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

#### ``hmset`` [&#x219F;](#table-of-contents)

Normally, the ``HMSET`` Redis command is of the form
``HMSET key field value [field value ...]``, but for simplicity's sake with
regard to LFE's arity, the ``hmset`` function is called a bit differently: It
is of the form ``(hmset <key> '(<kv pair> ...))``. For instance:

```lfe
> (ledis:hmset 'foo2 '(field1 bar2))
#(ok "OK")
> (ledis:hmset 'foo2 '(field2 bizaz field3 boz field4 bleez))
#(ok "OK")
```

These can then be accessed using ``hmget``:

```lfe
> (ledis:hmget 'foo2 'field4)
#(ok ("bleez"))
> (ledis:hmget 'foo2 '(field3 field2 field1))
#(ok ("boz" "bizaz" "bar2"))
```

#### ``lrange`` [&#x219F;](#table-of-contents)

In addition to ``lrange/3`` and ``lrange/4`` (which offers the same signature as
associated Redis commands, with the 4-arity function also allowing options to be
passed), ledis offers two additional arities for this function. Arity-1 and
arity-2 will return the entire list at the given key:

```lisp
> (ledis:lrange 'fruits)
#(ok
  ("banana"
   "tranbar"
   "kiwi"))
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

## A Note for Developers [&#x219F;](#table-of-contents)

If you have an interest in making contributions to this library, you'll need to
make note of how the wrappring works.

* For every *n*-arity function you wish to add (wrap), you'll also need to add
  an *n+1*-arity function of the same name. This is because ledis supports
  (requires) a version of every function that takes LFE-specific options (e.g.,
  setting return types).
* Under most circumstances, this just means making an entry in the functions
  ``get-api-funcs-no-opts`` and ``get-api-funcs-with-opts`` in the file
  ``include/ledis.lfe``.
* Non-normal circumstances include supporting Redis functions of mixed arity,
  special options, and non-standard or irregular function arguments. In these
  cases, you will often be able to make an entry in ``get-api-funcs-no-opts``,
  but have to then create a custom *n+1*-arity function in ``src/ledis.lfe``.
  Sometimes you won't be able to use either of the API-generating functions and
  macros, and will instead need to implement both the *n*-arity and *n+1*-arity
  functions in ``src/ledis.lfe``.
