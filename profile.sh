#!/bin/bash
set -e
stack clean
stack install --profile
brainfuck-exe $1 +RTS -p
