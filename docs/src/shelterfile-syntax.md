---
title: Shelterfile Syntax
date: 2025-08-15
---

Shelterfiles, files with a `.shl` extension, are a funny mix of syntax borrowing 
from Dockerfiles and shell scripts.

## Basic Syntax {#basix-syntax}

A basic shelterfile will have a series of commands (some which might be [meta-commands](shelter-0002)).
Take the following Dockerfile for example:

```dockerfile
FROM alpine
COPY hello.txt hello.txt
RUN cat hello.txt > hello2.txt
```

In [Shelter](), this can be expressed very similarly.

```shelter
@ session example1 --image=alpine
@ import file://./hello.txt hello.txt
cat hello.txt > hello2.txt
```

We have had to use two [meta-commands](shelter-0002) to achieve the same functionality. The first line
creates a fresh session using the `alpine` base image.

We then [import](shelter-0006) a file from the local filesystem into our image.

## Conditionals {#conditionals}

Unlike Dockerfiles, we have the possibility to introduce conditionals.
Conditionals allow you to check a certain command exits with `0` and if so
execute the first branch, otherwise execute the second branch.

### Syntax

The synax is similar to many programming languages' implementation of `if-then-else`
constructs.

```shelter
@ session example2 --image=alpine
apk add node
if (printf "13.0.0\n%s" $(node --version) | sort --version-sort --check=quiet) {
  echo "Node version greater than 13!"
} else {
  echo "Node version less than 13!"
}
```

## Parallel Loops {#parallel-for}

Given Shelter's underlying mergeable structure, it can offer a rather unique
model for parallel computation. Tools exist within the shell scripting world
for taking scripts and arguments and running them in parallel. For example,
[GNU parallel](https://www.gnu.org/software/parallel/) or
[littlejohn](https://github.com/quantifyearth/littlejohn). These tools, like
Shelter, use processes for parallelism. Shelter, however, runs the processes in
isolation using namespaces which gives us two advantages:

 1. It ensures that the parallel parts cannot interfere with one another. Each
    instantiation of the body of the loop starts from the same starting point
    (whatever came just before).
 2. Since each loop-body is independent, successful executions are recorded
    and do not need to be re-run in the event of a partial failure.

After all parts of the loop have completed, the resulting data is
[merged](shelter-0009) and execution continues from there. By providing this as
an extra feature, if users do not like these semantics, they can always opt to
use a more generic tool for their parallelism too!

### Syntax

For now, parallel for-loops are rather simplistic. 

```shelter
for file in [ a.txt, b.txt, c.txt ] {
  echo #file > #file
}
```

You may introduce a variable, here `file`, which will take on the value in your
list. In the commands of the loop-body (between the curly braces) variables are
referenced using `#` and will be replaced accordingly.
