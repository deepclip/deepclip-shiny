#!/bin/bash

pip2 install -r https://raw.githubusercontent.com/Lasagne/Lasagne/master/requirements.txt
pip2 install https://github.com/Lasagne/Lasagne/archive/master.zip
pip2 install matplotlib
pip2 install scikit-learn
pip2 install biopython
pip2 install HTSeq

mkdir -p /srv/deepclip/code /srv/deepclip/results

chown -R shiny:shiny /srv/deepclip 
usermod -a -G shiny root
usermod -a -G shiny shiny
chmod -R g+rw /srv/deepclip
