erlias
=====

Compile an Alias Module for Erlang


Make and alias to another a module
==================================

```erlang
erlias:build(short, my_super_long_module_name).
```

This will compile a new module called `short` that takes the exports from `my_super_long_module_name` and forwards them to the long module.


So for example, if you call:

```
my_super_long_module_name:some_function().
```

Erlias allows you to call

```
short:some_function().
```

NOTE
====

This is not a parse transform. You do not have to include anything with this, or
recompile any modules to make it work. This creates a new module on the fly.

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

[Jesse Gumm](https://jessegumm.com)

MIT Licensed
