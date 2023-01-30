#!/bin/bash

RAWBASE=raw66
RAWN=1

RAWFILE=${RAWBASE}_${RAWN}

# Crappy downloader
grep msl-raw-images ${RAWFILE}.html | tr ' ' '\n' | grep msl-raw-images | tr '"' '\n' | grep msl-raw-images | grep -v 'DXXX-thm' | sort -u > ${RAWFILE}.txt

mkdir -p data/${RAWFILE}


for raw in `cat ${RAWFILE}.txt`; do
  echo "For $raw"
  wget -nc -P data/${RAWFILE}/ $raw
  sleep 10
done
