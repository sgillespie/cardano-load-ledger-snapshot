#!/usr/bin/env bash

nix build

./result/bin/lstate-loader --trim data/129340741-8ecba84963-496.lstate
./result/bin/lstate-loader data/129340741-8ecba84963-496.lstate.trimmed +RTS -l -i1 -hc

eventlog2html lstate-loader.eventlog -o lstate-loader.eventlog.html
xdg-open lstate-loader.eventlog.html
