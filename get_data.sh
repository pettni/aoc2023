#!/bin/bash

for i in {1..4}; do
  aocd "$i" > "data/$i.txt"
done
