# The Discourse Programming Language
## Overview
### Goals
The Discourse programming language is an exercise (read 'experiment') in language design and the evaluation of assumptions.  The three assumptions that affected the design are as follows:

1. Every object is an isolated, concurrent unit, easing the burden on the programmer by removing the need to choose whether to thread code or not.
2. All modern computer languages are designed in the context that a language is designed for writing a single application running on a single machine.  The network is an afterthought and therefore relegated to APIs. This is invalid due to the ubiquity of the internet.
3. The privacy of all communications, computational in nature or not, is paramount. All messages between local and remote runtimes should be encrypted.

### Semantics
The core of Discourse is Carl Hewitt's [Actor Model](https://en.wikipedia.org/wiki/Actor_model). All Discourse objects exhibit three core behaviors:

1. Create Objects
2. Send Messages
3. Receive Messages

All message passing in the Actor Model was done asynchronously. To make a program slightly easier to reason about, Discourse adds [Promises](https://en.wikipedia.org/wiki/Futures_and_promises). All Discourse Promises are first class.  Promises allow the source to appear sequential at the potential expense of dead locks/live locks.

### Syntax
The syntax is greatly inspired by Smalltalk.  An example of the ```True``` object is listed below. The ```+``` indicates the defining of an External Behavior.
```smalltalk
    + True ifFalse: fBlock { Nil }

    + True ifTrue: tBlock { tBlock value }

    + True ifTrue: tBlock ifFalse: fBlock { tBlock value }

    + True not { False }

    + True & aBool {
      aBool ifTrue: { True } ifFalse: { False }
    }

    + True | aBool { True }

    + True ^ aBool {
      aBool ifTrue: { False } ifFalse: { True }
    }
```
## Getting Started
```bash
    $ git clone https://github.com/mjstahl/discourse.git
    $ mkdir ~/discourse/bin ~/discourse/pkg
    $ export GOROOT=$GOROOT:$HOME/discourse
    $ export PATH=$PATH:$GOROOT/bin
```

#### Compilation & Execution
```bash
    $ cd ~/discourse/src/disco
    $ go install
    $ disco
```
#### Testing
```bash
    $ cd ~/discourse/src/test
    $ go test
```

## License
Discourse source code is released under the GNU AGPL version 3 with parts under Go's BSD-style license.

Refer to the AGPL-LICENSE and BSD-LICENSE files for more information. 

