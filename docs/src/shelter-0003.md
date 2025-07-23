---
title: Getting Started
date: 2025-07-21
---

After getting [shelter installed](shelter-0005), you can start by running the `shelter`
command.

```
$ shelter
```

If this is your first time running `shelter` then you will be guided to setting
up some user information. To avoid having to run commands as root, shelter
comes with a [`shelterd`](shelter-0005) command for running a daemon
environment. Users then communicate with this daemon using
[capnproto](https://capnproto.org/).


