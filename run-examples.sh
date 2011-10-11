#!/usr/bin/env bash

for f in scenes/*
do
   ./cabal-dev/bin/raytracer $f $(basename $f).png
done
