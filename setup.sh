#!/bin/sh

set -e

for file in emacsclient.sh emacs_serverstart.pl magit-status
do
    ln -sf ${PWD}/${file} ~/bin
    chmod +x ~/bin/${file}
done

for file in *.el
do
    ln -sf ${PWD}/${file} ~/.emacs.d/elisps/
done
