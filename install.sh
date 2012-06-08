#! /bin/sh

INIT=~/.emacs.d/init.el
SRC_REPOS=tarao/emacs-pervasives
SRC=https://github.com/$SRC_REPOS
RAW_SRC=https://raw.github.com/$SRC_REPOS/master
EMACSD=~/.emacs.d
DST=$EMACSD/site-lisp
GITHUB=https://github.com
SIGNATURE="emacs-pervasives"

mkdir -p $EMACSD

for f in ~/.emacs ~/.emacs.el ~/.emacs.d/init.el; do
    [ -w "$f" ] && {
        INIT=$f
        break
    }
done

echo "Emacs init file: $INIT"
grep $SIGNATURE $INIT >/dev/null 2>&1 || {
    wget -O - $RAW_SRC/init.el >> $INIT
}

mkdir -p $DST
cd $DST

LOAD="-L ."
for repos in m2ym/popup-el m2ym/auto-complete emacsmirror/undo-tree; do
    [ -d "$repos" ] || svn checkout "$GITHUB/$repos/trunk" "$repos"
    last=`pwd`
    cd "$repos"
    for f in `ls *.el`; do
        emacs $LOAD --batch -f batch-byte-compile "$f"
    done
    cd "$last"
    LOAD="$LOAD -L $last/$repos"
done
