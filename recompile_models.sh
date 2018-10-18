#!/bin/bash

for i in /srv/deepclip/models/*.pkl; do
    echo "Recompiling $i into /srv/deepclip/results/predict_fn_$(basename $i)"
    python2 /srv/deepclip/code/recompile.py "$i" "/srv/deepclip/results/predict_fn_$(basename $i)"
done
