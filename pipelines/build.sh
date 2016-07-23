#!/bin/bash
files=( "hello" "3d" )
for i in "${files[@]}"
do
  echo "building $i.lc"
  lc $i.lc -o ../data/pipelines/$i.json
done
