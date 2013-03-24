#!/bin/sh

sbcl \
    --eval "(ql:quickload 'object-system)" \
    --eval "(object-system-tests:run)" \
    --non-interactive
