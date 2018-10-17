#!/bin/bash

rm models/predict_fn_*.pkl

for i in models/*.pkl; do
    echo "Recompiling $i"
    python2 deepclip/recompile.py "$i" "models/predict_fn_$(basename $i)"
done
