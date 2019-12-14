#!/bin/bash

set -e

export LANG=en_US.UTF-8

arg="${1}"
here=$(realpath $(dirname ${0}))
host=$(hostname | awk -F. '{ print $1 }')

if [ "${arg}" = "--install" ]; then
    if ! [ "$(readlink ${here}/pre-commit)" = "../../../../../${host}/.emacs.d/pre-commit.sh" ]; then
        cd "${here}"/../../.git/modules/${host}/.emacs.d/hooks
        ln -fsv ../../../../../${host}/.emacs.d/pre-commit.sh ./pre-commit
    fi
    exit
fi

cd "${here}"/../../../../../${host}/.emacs.d

./test-startup.py

if ! [ "$(pwd)" = "$HOME/.emacs.d" ]; then
    rm -fr elpa snippets my-custom.el*
fi
