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


## Usage

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
