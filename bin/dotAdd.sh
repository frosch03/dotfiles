#!/usr/bin/env bash

if [ -z $1 ];
then
    echo "$(basename $0) moves a given file to a configured folder and"
    echo "instead of the file inserts a symbolic link to the moved"
    echo "file.  If the file is located within a folder, that same"
    echo "folder is created at the configured target location."
    echo ""
    echo "usage: $0 <filename>"
    echo "  filename - the file you like to move and link"
    exit -1;
fi

HOME=/home/frosch03
DOTDIR=$HOME/localStorage/dotfiles

PATHLEN=${#1}
PARPATH=""

BASE=$(basename $1)
BASELEN=${#BASE}

if [ $PATHLEN -eq $BASELEN ];
then
    mv ./$1 $DOTDIR;
    echo "MOV $1 -> $DOTDIR";
else
    TMP=$((-1 * $BASELEN));
    PARPATH=${1::$TMP};

    if [ ! -d $DOTDIR/$PARPATH ];
    then
        mkdir -p $DOTDIR/$PARPATH;
        echo "MKD $DOTDIR/$PARPATH";
    fi
    mv ./$1 $DOTDIR/$PARPATH;
    echo "MOV $1 -> $DOTDIR/$PARPATH";
fi

ln -s $DOTDIR/$1 $1;
echo "LNK $DOTDIR/$1 $1";

exit 0;

