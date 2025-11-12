---
title: Shelter
author: The Shelter Developers
date: 2025-07-21
---

Welcome to Shelter's documentation.

Shelter is a _metashell_. A tool for managing shell sessions in a more reproducible way.

To get started with Shelter, you can read the [getting started](shelter-0003) tutorial.
For a more thorough understanding of what Shelter offers and how to use it, then please 
follow the [documentation](shelter-0004).

```forester
\put\transclude/numbered{false}
\put\transclude/expanded{false}

\transclude{shelter-0001}
```

For reference documentation, we have:

 - The [Shelterfile syntax](shelterfile-syntax) -- the syntax for shelterfiles, a mishmash of
   Dockerfiles and shell scripts. These are useful for setting up branches for developement
   or running (modest) pipelines.
 - The available [meta-commands](shelter-0002) -- these extend past shell scripts and allow users
   to access the more _interesting_ parts of Shelter like `@ undo` or `@ merge <branch>` for example.
