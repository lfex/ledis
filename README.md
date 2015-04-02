# ledis

*An LFE Redis Client Library*

<img src="resources/logos/ButterCrunchLettuce-2-small.png" />


## Dependences

You will need the following installed on your system:

* Erlang
* Redis
* rebar

Additionally, the ledis ``Makefile`` sets up the following dependencies for you
automatically when you run the ``compile`` target:

* LFE
* eredis
* ltest


## Installtion and Setup

Here's what you need to do:

```bash
$ git clone https://github.com/oubiwann/ledis.git
$ cd ledis
$ make compile
```

At this point, you will be able to run an LFE REPL (shell):

```bash
$ make repl
```

and use the library dependencies:

```cl
Erlang R15B03 (erts-5.9.3) [source] [smp:8:8] ...

LFE Shell V5.9.3 (abort with ^G)
> (set `#(ok ,client) (eredis:start_link))
#(ok <0.31.0>)
> (eredis:q client '("SET" "fooz42" "barz42"))
#(ok #B(79 75))
> (eredis:q client '("GET" "fooz42"))
#(ok #B(98 97 114 122 52 50))
> (eredis:q client '("GET" "fooz43"))
#(ok undefined)
> (set `#(ok ,result) (eredis:q client '("GET" "fooz42")))
#(ok #B(98 97 114 122 52 50))
> result
#B(98 97 114 122 52 50)
> (erlang:binary_to_list result)
"barz42"
```
That's just an example on how to use eredis from LFE. In the next section,
we'll give some examples of using ledis directly.


## Usage

To use ledis from the shell, just do this:

```bash
$ make repl-no-deps
```
```cl
Erlang R15B03 (erts-5.9.3) [source] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

LFE Shell V5.9.3 (abort with ^G)
> (set client (ledis-client:make-client))
#Fun<ledis-client.0.24556364>
> (ledis-api:set client "my-key" "some cool data")
"OK"
> (ledis-api:get client "my-key")
"some cool data"
```

To use from an LFE project that imports ledis:

```cl
(defmodule my-mod
  (export all)
  (import (from ledis-client (make-client 0)))
  (import (from ledis-api (get 2) (set 3))))

(defun set-and-get ()
  (let ((client (make-client)))
    (set client "key-1" "data 1")
    (set client "key-2" "data 2")
    (get client "key-1")))
```
