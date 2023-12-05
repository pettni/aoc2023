#!/bin/bash

for i in {1..3}; do
  aocd "$i" > "data/$i.txt"
done
