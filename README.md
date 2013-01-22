# The Discourse Programming Language
## Overview
### Goals
The Discourse programming language is an exercise (read 'experiment') in language design and the evaluation of assumptions.  The three assumptions that affected the design are as follows:

1. Threading is hard to get right and should be avoided. Therefore we will make every object an isolated, concurrent unit to ease the burden on the programmer by removing the need to choose whether to thread code or not.
2. All modern computer languages exhibit the assumption that a language is designed for writing a single application running on a single machine.  The network is an afterthought and therefore relegated to APIs. This is invalid due to the ubiquity of the internet.
3. The privacy of all communications, computational in nature or not, is paramount. All messages between local and remote objects should be encrypted.

### Semantics
The core of Discourse is Carl Hewitt's [Actor Model](https://en.wikipedia.org/wiki/Actor_model). All Discourse objects exhibit three core behaviors:

1. Create Objects
2. Sending Messages
3. Receiving Messages

All message passing in the Actor Model was done asynchronously. To make a program slightly easier to reason about, Discourse adds [Promises](https://en.wikipedia.org/wiki/Futures_and_promises) (properly named a [Future](https://en.wikipedia.org/wiki/Futures_and_promises). All Discourse Promises are first class.  Promises allow the source to appear sequential at the potential expense of dead locks/live locks.

### Terminology
In many cases, familiar terminology will be used throughout the Discourse examples and documentation. In other cases, new terminology will be introduced to describe constructs you are already familiar with.  Most likely the words will not mean the same thing as you are used to.  The intention was not to confuse, I merely chose what I thought to be the best terminology to describe a given construct without regard to its past or current usage.

For example, Discourse does have **Objects**, but there are no classes, no prototypes, and no inheritance.  Objects have internal 'private' variables to manage state, they are called **Attributes**, not members. Computation is preformed by an object's **Behaviors** (not methods) and an object can have both **Internal** (private) and **External** (public) behaviors.

### Syntax
The syntax is greatly inspired by Smalltalk.  An example of the ```True``` object is listed below. The ```+``` indicates the defining of an External Behavior.

    + True ifFalse: fBlock => Nil.

    + True ifTrue: tBlock => tBlock value.

    + True ifTrue: tBlock ifFalse: fBlock => tBlock value.

    + True not => False.

    + True & aBool =>
      aBool ifTrue: { True } ifFalse: { False }.

    + True | aBool => True.

    + True ^ aBool =>
      aBool ifTrue: { False } ifFalse: { True }.

## Getting Started
We will assume that all of of below commands are executed from the ```$HOME``` directory.

    $ git clone https://github.com/mjstahl/discourse.git
    $ mkdir discourse/bin discourse/pkg
    $ export GOROOT=$GOROOT:$HOME/discourse
    $ export PATH=$PATH:$GOROOT/bin


#### Compilation & Execution
    $ cd discourse/src/disco
    $ go install
    $ disco

#### Testing
    $ cd discourse/src/test
    $ go test


