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


Additionally, the ledis Makefile sets up the following dependencies for you
automatically when you run the `compile` target:

* `LFE`_

* `eredis`_

Installtion and Setup
---------------------

Here's what you need to do::

  $ git clone https://github.com/oubiwann/ledis.git
  $ cd ledis
  $ make compile

At this point, you will be able to run an LFE REPL (shell) and use the library::

  $ make shell

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