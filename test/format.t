This file tests the formatter of shl files. In reality, it is also a test for
actually parsing the files too!

  $ cat > simple.shl << EOF
  > echo hello > hello.txt
  > cat hello.txt
  > EOF

  $ cat simple.shl | shelter format  
  echo hello > hello.txt
  cat hello.txt

For loops are possible in the language. These are parallel for loops that are
merged after the successful execution of each branch of the loop.

  $ cat > for.shl << EOF
  > for file in [ a.txt, b.txt, c.txt ] {
  >   echo hello > file
  > }
  > EOF

  $ cat for.shl | shelter format  
  for file in [ a.txt, b.txt, c.txt ] {
    echo hello > file
  }

There are also conditionals too.

  $ cat > if.shl << EOF
  > if exit 0 then {
  >   echo "exited 0"
  > } else {
  >   echo "exited nonzero"
  > }
  > EOF

  $ cat if.shl | shelter format  
  if exit 0 then {
    echo "exited 0"
  } else {
    echo "exited nonzero"
  }

For commands that might need to broken across multiple lines, you can escape
the newline similar to how you might do it in bash or in a Dockerfile.

  $ cat > break.shl << EOF
  > python3 ./prepare_layers/make_current_map.py --jung "\${DATADIR}"/habitat/jung_l2_raw.tif \
  >               --update_masks "\${DATADIR}"/habitat/lvl2_changemasks_ver004 \
  >               --crosswalk "\${DATADIR}"/crosswalk.csv \
  >               --output "\${DATADIR}"/habitat/current_raw.tif \
  >               -j 16
  > EOF

  $ cat break.shl | shelter format
  python3 ./prepare_layers/make_current_map.py --jung "${DATADIR}"/habitat/jung_l2_raw.tif --update_masks "${DATADIR}"/habitat/lvl2_changemasks_ver004 --crosswalk "${DATADIR}"/crosswalk.csv --output "${DATADIR}"/habitat/current_raw.tif -j 16

For proper formatting we either need to implement our own line breaking or
preserve the initial line breaking by the user... we do neither right now.
