#!/bin/sh

set -e
set -x

for file in *.el
do
    ln -sf ${PWD}/${file} ~/.emacs.d/elisps/
done
