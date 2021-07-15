#!/usr/bin/env bash

DOTFILEFOLDER=~/localStorage/dotfiles
# test
pushd ~

if [ ! -d ./.emacs.d ];
then
    echo "SEED: copying emacs seed directory"
    cp -r $DOTFILEFOLDER/.emacs.d ./
fi

for x in $(find $DOTFILEFOLDER/ -maxdepth 1 -not -iname ".git" -iname ".*");
do
    if [ -f $(basename $x) -a ! -L $(basename $x) ];
    then
        echo "SKIP: $(basename $x) exists and is no symlink"
    else
        if [ ! -d $(basename $x) ];
        then
            if [ -f $(basename $x) ];
            then
                echo "DROP: $(basename $x) symlink"
                rm $(basename $x)
            fi
            echo "LINK: $(basename $x) -> $x"
            ln -s $x $(basename $x);
        fi
    fi
done

popd

mkdir ~/bin 2>/dev/null
pushd ~/bin

for x in $(find $DOTFILEFOLDER/bin -maxdepth 1 -executable -not -type d);
do
    if [ -f $(basename $x) -a ! -L $(basename $x) ];
    then
        echo "SKIP: $(basename $x) exists and is no symlink"
    else
        if [ ! -d $(basename $x) ];
        then
            if [ -f $(basename $x) ];
            then
                echo "DROP: $(basename $x) symlink"
                rm $(basename $x)
            fi
            echo "LINK: $(basename $x) -> $x"
            ln -s $x $(basename $x);
        fi
    fi
done

popd
