#!/usr/bin/env bash

mkdir -p output

for f in scenes/*
do
   ./cabal-dev/bin/haray $f output/$(basename $f).png $*
done
