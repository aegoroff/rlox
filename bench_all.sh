#!/bin/bash

find ~/code/craftinginterpreters/test/benchmark -type f -print0 | grep -zEv 'zoo_batch|string_equality' | sort -z | xargs -0 ./bench.sh
