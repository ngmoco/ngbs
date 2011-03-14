NGBS - The ngmoco:) BertRPC Server
==================================

ngbs is a BertRPC_ server..

Features
--------

* OTP application and configuration
* Builds with rebar
* Module and function level ACLs
* ``emfile`` overload protection
* Runtime configurable Command dispatch instrumentation
* Persistent and transient BertRPC_ connections
* Safe bert decoding (binary_to_term(BertBin, [safe]))


Installation
------------

Building ngbs
_____________

Prerequisites:
* Erlang R14B+
* rebar

#. rebar compile
#. enjoy a well earned margarita

Configuring ngbs
________________

In these examples we'll configure the ngbs server to listen on tcp
port 8000 and allow clients to call any function in the ``my_api``
module as well as ``erlang:memory()``.


If you want to start the ngbs server via configuration only, add a
section for ngbs to your ``erl -config <file>``::

    {ngbs, [{port, 8000},
            {allowed_calls, [{module, my_api},
                             {function, {erlang, memory, 0}}]},
            {listen_on_startup, true}]}

You can also start the ngbs server manually via the API.

In your application startup code, add code to allow calls to the
API you want to expose to BertRPC_ clients::

    -module(my_app).
    
    start(_,_) ->
        %% applicaton:start(ngbs) has already been called by the
        %% time we get here.
        ngbs:allow_call({module, my_api}),
        ngbs:allow_call({function, {erlang, memory, 0}),
        ngbs:listen(8000).

Notes
-----

Each BertRPC_ connection is run as a separate Erlang process. ``call``
requests are processed synchronously in these processes. Asynchronous
``cast`` requests are sent to a central cast dispatch process for
background evaluation.

Restrictions
------------

* ngbs at present can only listen on one port at a time.
* The dispatch process that runs 'cast' calls 
* ``info`` packets are ignored.
* Doesn't support BertRPC_ binary streaming

.. _BertRPC: http://bert-rpc.org/
