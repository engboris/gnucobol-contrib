#!/bin/bash
#set -x #echo on

RES_FOLDER=resources
FILE=$RES_FOLDER/CA.txt

RECORD_SIZE_PADDING=600
DEBUG_MODE=true

if [ ! -f $FILE ]; then
    echo "Downloading resource CA.txt..."
    wget http://download.geonames.org/export/dump/CA.zip
    unzip -x CA.zip
    mkdir -p $RES_FOLDER
    mv CA.txt $FILE
    rm CA.zip
fi

./scripts/gen_city_record_from.sh $FILE

if $DEBUG_MODE; then
    cobc worldcities0.cbl -x -W -fdebugging-line
else
    cobc worldcities0.cbl -x -W 
fi
./worldcities0 $FILE
rm worldcities0
