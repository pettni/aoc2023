#!/bin/bash

for i in {19..25}; do
  aocd "$i" > "data/$i.txt"
done
