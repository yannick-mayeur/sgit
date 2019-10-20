# SGIT

[![Build Status](https://travis-ci.org/yannick-mayeur/sgit.svg?branch=master)](https://travis-ci.org/yannick-mayeur/sgit)

## Requirements

* Scala version 2.13.0
* sbt version 1.3.2
* Java 8 JDK

## Installation

```
$ git clone https://github.com/yannick-mayeur/sgit.git sgit-yannickm
$ cd sgit-yannickm
$ sbt assembly
```

The binary is generated into `target/scala-2.13`. You can add the
path to this folder to your `PATH` variable to be able to launch sgit
commands from anywhere on your system.

To get an overview of available commands execute the following command:

```
$ sgit --help
```

## Usage

- [x] help
- [x] init
- [x] status
- [x] add \<file\>
- [x] commit \[-m \<message\>\]
- [x] diff
- [x] log
- [x] log -p
- [x] log --stat
- [x] branch \[\<name\>\]
- [x] branch -av
- [x] tag \[\<name\>\]
- [x] checkout \[\<branch tag or commit\>\]
- [ ] merge (in progress)
- [ ] rebase
- [ ] rebase -i

## Test

The test suite can be launched with:

```
$ sbt test
```
