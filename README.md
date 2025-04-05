shelter
-------

A shell session shim that makes exploring from the terminal a little bit easier.

## Up and running

To test shelter locally you feel need a ZFS pool, for now you must name it `shelter`.

```
$ truncate --size=10G /var/shelter.img
$ sudo zpool create shelter /var/shelter.img 
$ sudo -E dune exec -- shelter
```

Sometimes you want to just restart the world.

```
$ sudo zpool destroy shelter && sudo zpool create shelter /var/shelter.img && sudo rm -rf ~/.cache/shelter
```

## Shl files

You can run both the main shelter program and the passthrough mode via a series of actions in a `.shl` file.
