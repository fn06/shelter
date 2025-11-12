#!/bin/bash

files=("merge" "failed-merge" "fresh-session" "for" "if" "undo")

if [ $# -ge 2 ]; then
	files=("${@:2}")
else
	files=("${files[@]}")
fi

function run_test {
  echo ">>>>>>>>>>>>>>>>> Running test for $1"
  ./test/run.sh "./test/fixtures/$1.shl" 2>&1 | tee "./test/$1.expected"
}

function run_diff {
  echo ">>>>>>>>>>>>>>>>> Running diff for $1"
  ./test/run.sh "./test/fixtures/$1.shl" 2>&1 | tee "./test/$1.out"
  diff -Naru "./test/$1.expected" "./test/$1.out"
  rm "./test/$1.out"
}

if [[ $1 == "diff" ]]; then
  for file in ${files[@]}
  do
	  run_diff $file
  done
elif [[ $1 == "test" ]]; then
  for file in ${files[@]}
  do
	  run_test $file
  done
else
  echo "Use either diff or test"
  exit 1
fi

