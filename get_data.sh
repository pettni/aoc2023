#!/bin/bash

for i in {10..15}; do
  aocd "$i" > "data/$i.txt"
done
