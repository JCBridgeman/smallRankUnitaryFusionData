#!/bin/bash


cd ./uFC
for f in *; do
  cd $f
  tar -cf $f.tar *
  cd ..
  mv $f/$f.tar .
done
