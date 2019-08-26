#!/usr/bin/env bash

mkdir -p output

for f in scenes/*
do
   cabal new-run haray -- $f output/$(basename $f).png $*
done
