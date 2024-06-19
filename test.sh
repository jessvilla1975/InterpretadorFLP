#!/bin/bash

par=$1
tipo=$2
result=$(racket "tests/test"$par"_"$tipo".rkt" 2>&1 > /dev/null)
if [ -z "$result" ]
then
  echo "Test exitoso"
else
  echo "Test fallido :C  with error: "$result
  exit 125
fi

