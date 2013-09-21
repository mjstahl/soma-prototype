Memory Model
============

Heap should only be accessible via channel.  HeapInsert and HeapLookup should be functions that insert or retrieve to/from the heap and block until the value is inserted or retrieved.

Scopes should be passed in by value and not by reference. After this then can then be chained as opposed to copied as they are now. Self needs to be added to scope to fix the current problems caused by passing it along in a message as the 0th argument.

AST / Runtime
=============

The Expr interface methods (Eval, and Visit) should be changed to return uint64.  Currently, by returning Value, they datastructures loose information as was discovered by the implementation of Integer.


soma eval and :load
===================
```soma eval``` and ```:load``` need to be completed.  ```soma eval``` currently has a ```soma console``` option and just requires a completion of the implemention.  ```:load``` needs to be added to the console. ```:load``` will take one argument, the path to a file, and load it into the REPL.

