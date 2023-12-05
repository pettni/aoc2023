#!/bin/bash

for i in {1..10}; do
  aocd "$i" > "data/$i.txt"
done
