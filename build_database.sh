#!/bin/bash
sqlite3 -batch deepclip.db << "EOF"
CREATE TABLE jobs (id INTEGER PRIMARY KEY, token TEXT, status INTEGER);
.mode csv
.import database_data.csv jobs
EOF
