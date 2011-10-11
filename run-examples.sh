#!/usr/bin/env bash

mkdir -p output

for f in scenes/*
do
   ./cabal-dev/bin/raytracer $f output/$(basename $f).png
done
