erlias
=====

Compile an Alias Module for Erlang


About this Library
==================

(This was originally a part of [sql_bridge](https://github.com/choptastic/sql_bridge),
but has been split off into its own library for others to use, if they want.)

There are a number of other Erlang module alias libraries out there, but they
all appear to be parse transforms, where you must compile your modules with
the parse transform. This gives an interesting level of flexibility, where you
can have different module aliases for different calling modules.

This is not that.

This is not a parse transform. This creates a new globally accessible module.
And you do not have to modify your source code to use this in any way, or
recompile any modules to make it work. This new module simply maps from the
new module to the original module.


This was originally built for `sql_bridge` so that instead of calling

`sql_bridge:q(Query)`

you could simply call 

`db:q(Query)`

Usage
=====

Sometime (probably during application startup), call the following

```erlang
erlias:build(my_super_long_module_name, short).
```

This will compile a new module called `short` that takes the exports from
`my_super_long_module_name` and forwards them to the long module.


Instead of calling:

```
my_super_long_module_name:some_function().
```

You can now call

```
short:some_function().
```

Because `short` is a new globally accessible module, it can be called from
any module or from the Erlang shell.

How it works
------------
It literally just looks at the exports of `my_super_long_module_name` and
creates a mapper module:

For example, if `some_function()` is the only exported function in
`my_super_long_module_name`, the true implementation of `short` would be this:

```
-module(short).
-export([some_function/0]).

some_function() ->
	my_super_long_module_name:some_function().
```


Author
======

[Jesse Gumm](http://jessegumm.com)

MIT Licensed
