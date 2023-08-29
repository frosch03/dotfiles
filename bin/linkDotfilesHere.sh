#!/usr/bin/env bash

help () {
    echo "Usage: $(basename $0) <path-to-dotfiles>"
    echo ""
    echo "Mandatory arguments:"
    echo "  <path-to-dotfiles>    path (relative or absolute) to the"
    echo "                        location where dotfiles are stored"
}

if [ $# -ne 1 ];
then
    help;
    exit 0;
fi

FROMFOLDER=$(realpath $1)
TOFOLDER=$(pwd)

echo "From Folder: $FROMFOLDER"
echo "To Folder: $TOFOLDER"

create_folderstructure () {
    local src=$1
    local dst=$2

    for i in $(find $src ! -path "$src" ! -path "*.git*" -type d | xargs);
    do
        echo "CREATE DIR: $dst/${i:$((${#src}+1))}"
        mkdir -p $dst/${i:$((${#src}+1))};
    done
}

create_folderstructure $FROMFOLDER $TOFOLDER

pushd $TOFOLDER

for x in $(find $FROMFOLDER/ \
                ! -path "*.git*" \
                ! -path "*default.nix*" \
                ! -path "*~" \
                ! -path "*.hi" ! -path "*.o" ! -path "*.errors" ! -path "*prompt-history" ! -path "*xmonad*-linux" \
                ! -path "*ReadMe.*" \
                -type f | xargs);
do
    NEW_FILE=$TOFOLDER/${x:$((${#FROMFOLDER}+1))}

    if [ -f $NEW_FILE -a ! -L $NEW_FILE ];
    then
        echo "SKIP: $NEW_FILE exists and is no symlink"
    else
    # if [ ! -d $NEW_FILE ];
    # then
        if [ -f $NEW_FILE ];
        then
            echo "DROP: $NEW_FILE symlink"
            rm $NEW_FILE
        fi
        echo "LINK: $TOFOLDER/${x:$((${#FROMFOLDER}+1))} -> $x"
        ln -s $x $TOFOLDER/${x:$((${#FROMFOLDER}+1))};
    # fi
    fi
done

popd
