#!/bin/bash

mkdir -p /srv/deepclip/code /srv/deepclip/data /srv/deepclip/networks /srv/deepclip/results

sqlite3 /srv/deepclip/deepclip.db "CREATE TABLE jobs (id INTEGER PRIMARY KEY, token TEXT, status INTEGER);"

chown -R shiny:shiny /srv/deepclip 
usermod -a -G shiny root
usermod -a -G shiny shiny
chmod -R g+rw /srv/deepclip

pip2 install -r https://raw.githubusercontent.com/Lasagne/Lasagne/master/requirements.txt
pip2 install https://github.com/Lasagne/Lasagne/archive/master.zip
pip2 install matplotlib
pip2 install scikit-learn
pip2 install biopython
pip2 install HTSeq
