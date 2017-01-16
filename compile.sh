#!/bin/bash -ex

buildapp --output ibm-model-1 --entry tau:main --asdf-tree ~/quicklisp/dists/quicklisp/software/ --load-system tau --dynamic-space-size 4096
