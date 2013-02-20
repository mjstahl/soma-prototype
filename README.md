# The Discourse Programming Language
 * [Goals](#goals)
 * [Semantics](#semantics)
 * [Syntax](#syntax)
 * [Getting Started](#getting-started)
 * [Roadmap](#roadmap)
 * [License](#license)

## Goals
The Discourse programming language is an exercise (read 'experiment') in language design and the evaluation of assumptions.  The three assumptions that affected the design are as follows:

1. Every object is an isolated, concurrent unit, easing the burden on the programmer by removing the need to choose whether to thread code or not.
2. All modern computer languages are designed in the context that a language is designed for writing a single application running on a single machine.  The network is an afterthought and therefore relegated to APIs. This is invalid due to the ubiquity of the internet.
3. The privacy of all communications, computational in nature or not, is paramount. All messages between local and remote runtimes should be encrypted.

## Semantics
The core of Discourse is Carl Hewitt's [Actor Model](https://en.wikipedia.org/wiki/Actor_model). All Discourse objects exhibit three core behaviors:

1. Create Objects
2. Send Messages
3. Receive Messages

All message passing in the Actor Model was done asynchronously. To make a program slightly easier to reason about, Discourse adds [Promises](https://en.wikipedia.org/wiki/Futures_and_promises). All Discourse Promises are first class.  Promises allow the source to behave sequentially at the potential expense of dead locks/live locks.

## Syntax
The syntax is greatly inspired by Smalltalk.  An example of the ```True``` object is listed below. The ```+``` indicates the defining of an External Behavior.
```smalltalk
    + True ifFalse: fBlock => { Nil }

    + True ifTrue: tBlock => { tBlock value }

    + True ifTrue: tBlock ifFalse: fBlock => { tBlock value }

    + True not => { False }

    + True & aBool => {
      aBool ifTrue: { True } ifFalse: { False }
    }

    + True | aBool => { True }

    + True ^ aBool => {
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

## Roadmap
The below road map of the features to be complete before 1 Jan. 2014 and their estimated time of completion.

### Q1 2013
&#x2714; **v0.1** Scanner, Parser

&#x2718; **v0.2** Runtime, Booleans, Nil

&#x2718; **v0.3** Distribution, [Elliptic Curve Cryptography](http://en.wikipedia.org/wiki/Elliptic_curve_cryptography)

### Q2 2013
&#x2718; **v0.4**  Numbers (integers), block arguments

&#x2718; **v0.5**  Arrays, [Message Patterns](http://www.fscript.org/documentation/OOPAL.pdf)

### Q3 2013
&#x2718; **v0.6**  Strings, Characters, Symbols

&#x2718; **v0.7**  Hash Tables

&#x2718; **v0.8**  Floats

### Q4 2013
&#x2718; **v0.9**  Attributes (state)

&#x2718; **v0.10** [Byzantine Paxos](http://en.wikipedia.org/wiki/Paxos_\(computer_science\)#Byzantine_Paxos)

## License
Discourse source code is released under the [GNU AGPL version 3](http://www.gnu.org/licenses/agpl.html) with parts under [Go's BSD-style](https://github.com/mjstahl/discourse/blob/master/legal/BSD-LICENSE) license.

Refer to the AGPL-LICENSE and BSD-LICENSE files for more information. 
