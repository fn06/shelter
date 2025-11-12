---
title: Shelter Meta-commands
date: 2025-07-21
---

When developing inside Shelter, most commands are sent straight to a shell
of your choice (see [how to configure shelter](shelter-0003)).

To interact with the meta aspects of Shelter you can prefix your command
with `@`. If you type just `@` then you will get a list of possible commands. 

## @ session {#at-session}

Sessions are your primary way of controlling different environments. You can
think of them as git branches pointing at specific instances of data and code.
By default, you will start shelter in the `main` session. Sessions _must_ have
an associate "base image" (by default this is `alpine`).

```shelter
@ session experiment-1 --image=debian
```

This command will create a fresh session with no history called `experiment-1`
using the Debian base image.

```shelter
@ session --image=debian
```

This is a similar command, but the name will be randomly generated and is
ensured to be unique. It is a bit like having a Dockerfile with a `FROM
debian` at the top.

```shelter
@ session
```

Will list all the currently available sessions.

```shelter
@ session exp
```

When no `--image` is specified, this will try to switch you into an existing
session if one exists, otherwise it will create a new session based on the
current session.

## @ undo {#at-undo}

The `@ undo` command will move your session back one commit into the past. You
may specify a number of commits to move, for example `@ undo n` to move back
`n` commits. If there are less than `n` commits, we will stop going backwards.
