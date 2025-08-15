---
title: Shelterfile Syntax
date: 2025-08-15
---

Shelterfiles, files with a `.shl` extension, are a funny mix of syntax borrowing 
from Dockerfiles and shell scripts.

## Basic Syntax

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

## Conditionals

Unlike Dockerfiles, we have the possibility to introduce conditionals.

```shelter
@ session example2 --image=alpine
apk add node
if (node --version )

## Parallel Loops


