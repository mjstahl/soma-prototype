# Social Machines
Social Machines is a server-side programming language with a syntax greatly inspired by Smalltalk. Every object in Social Machines is a concurrent unit of computation, libraries can be shared like peers in a BitTorrent network, and communication between peers is encrypted.

 * [Goals](#goals)
 * [Semantics](#semantics)
 * [Syntax](#syntax)
 * [Getting Started](#getting-started)
 * [License](#license)

## Goals
Social Machines is an exercise (read 'experiment') in language design and the evaluation of assumptions. The three assumptions that affected the design are as follows:

1. Every object is an isolated, concurrent unit, easing the burden on the programmer by removing the need to choose whether to thread code or not.
2. All modern computer languages are designed in the context that a language is designed for writing a single application running on a single machine. The network is an afterthought and therefore relegated to APIs. This is invalid due to the ubiquity of the internet.

## Semantics
The core of Social Machines is Carl Hewitt's [Actor Model](https://en.wikipedia.org/wiki/Actor_model). All Social Machines objects exhibit three core behaviors:

1. Create Objects
2. Send Messages
3. Receive Messages

All message passing in the Actor Model was done asynchronously. To make a program slightly easier to reason about, Social Machines adds [Promises](https://en.wikipedia.org/wiki/Futures_and_promises). All Social Machines Promises are first class.  Promises allow the source to behave sequentially at the potential expense of dead locks/live locks.

## Syntax
The syntax is greatly inspired by Smalltalk.  An example of the ```True``` object is listed below. The ```+``` indicates the defining of an External Behavior.
```smalltalk
    True ifFalse: fBlock => nil.

    True ifTrue: tBlock => tBlock value.

    True ifTrue: tBlock ifFalse: fBlock => tBlock value.

    True not => false.

    (t True) & aBool =>
       aBool ifTrue: { t } ifFalse: { false }.

    (t True) | aBool => t.

    (t True) ^ aBool =>
       aBool ifTrue: { false } ifFalse: { t }.
```
## Getting Started
```bash
    $ git clone https://github.com/mjstahl/socialmachines.git
    $ mkdir ~/socialmachines/bin ~/socialmachines/pkg
    $ export GOPATH=$GOROOT:$HOME/socialmachines
    $ export PATH=$PATH:$GOROOT/bin
```

#### Compilation & Execution
```bash
    $ cd ~/socialmachines/src/soma
    $ go install
    $ soma
```
#### Testing
```bash
    $ cd ~/socialmachines/src/test
    $ go test
```

## License
Social Machines source code is released under the [AGPLv3 License](https://www.gnu.org/licenses/agpl-3.0.html) with parts under [Go's BSD-style](https://github.com/mjstahl/socialmachines/blob/master/legal/BSD-LICENSE) license.

Refer to the legal/AGPL and legal/BSD files for more information.
