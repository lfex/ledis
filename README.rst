ledis
=====

.. image:: resources/logos/ButterCrunchLettuce-2-small.png
   :target: resources/logos/ButterCrunchLettuce-2-medium.png

An `LFE`_ wrapper for the eredis library.


Dependences
-----------

You will need the following installed on your system:

* `Erlang`_

* `Redis`_

* `rebar`_


Additionally, the ledis ``Makefile`` sets up the following dependencies for you
automatically when you run the ``compile`` target:

* `LFE`_

* `eredis`_

Installtion and Setup
---------------------

Here's what you need to do::

  $ git clone https://github.com/oubiwann/ledis.git
  $ cd ledis
  $ make compile

At this point, you will be able to run an LFE REPL (shell)::

  $ make shell

and use the library

.. code:: cl

    Erlang R15B03 (erts-5.9.3) [source] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

    LFE Shell V5.9.3 (abort with ^G)
    > (set (tuple 'ok client) (: eredis start_link))
    #(ok <0.31.0>)
    > (: eredis q client '("SET" "fooz42" "barz42"))
    #(ok #B(79 75))
    > (: eredis q client '("GET" "fooz42"))
    #(ok #B(98 97 114 122 52 50))
    > (: eredis q client '("GET" "fooz43"))
    #(ok undefined)
    > (set (tuple 'ok result) (: eredis q client '("GET" "fooz42")))
    #(ok #B(98 97 114 122 52 50))
    > result
    #B(98 97 114 122 52 50)
    > (: erlang binary_to_list result)
    "barz42"
    >

Usage
-----

TBD

.. code:: cl

  (this is a (test))

.. Links
.. -----
.. _LFE: http://lfe.github.io/
.. _Erlang: http://www.erlang.org/
.. _Redis: http://redis.io/
.. _rebar: https://github.com/rebar/rebar
.. _eredis: https://github.com/wooga/eredis