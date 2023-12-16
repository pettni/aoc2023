#!/bin/bash

for i in {16..20}; do
  aocd "$i" > "data/$i.txt"
done
