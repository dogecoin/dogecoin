****************************
  Examples
****************************

A basic program that uses ``python-dogecoin`` looks like this:

First, import the library and exceptions.

::

    import dogecoinrpc
    from dogecoinrpc.exceptions import InsufficientFunds

Then, we connect to the currently running ``dogecoin`` instance of the current user on the local machine
with one call to
:func:`~dogecoinrpc.connect_to_local`. This returns a :class:`~dogecoinrpc.connection.DogecoinConnection` objects:

::

    conn = dogecoinrpc.connect_to_local()

Try to move one dogecoin from account ``testaccount`` to account ``testaccount2`` using 
:func:`~dogecoinrpc.connection.DogecoinConnection.move`. Catch the :class:`~dogecoinrpc.exceptions.InsufficientFunds`
exception in the case the originating account is broke:

::  

    try: 
        conn.move("testaccount", "testaccount2", 1.0)
    except InsufficientFunds,e:
        print "Account does not have enough funds available!"


Retrieve general server information with :func:`~dogecoinrpc.connection.DogecoinConnection.getinfo` and print some statistics:

::

    info = conn.getinfo()
    print "Blocks: %i" % info.blocks
    print "Connections: %i" % info.connections
    print "Difficulty: %f" % info.difficulty
  

