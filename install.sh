#! /bin/bash

INIT=~/.emacs.d/init.el
SRC=https://github.com/tarao/emacs-pervasives
DST=~/.emacs.d/site-lisp
GITHUB=https://github.com
SIGNATURE="emacs-pervasives"

for f in ~/.emacs ~/.emacs.el ~/.emacs.d/init.el; do
    [ -w "$f" ] && {
        INIT=$f
        break
    }
done

echo "Emacs init file: $INIT"
grep $SIGNATURE $INIT >/dev/null 2>&1 || {
    wget -O - $SRC/init.el >> $INIT
}

mkdir -p $DST
pushd $DST

for repos in m2ym/popup-el m2ym/auto-complete emacsmirror/undo-tree; do
    [ -d "$repos" ] || svn checkout "$GITHUB/$repos/trunk" "$repos"
    for f in `ls $repos/*.el`; do
        [ -e `basename "$f"` ] || ln -s  "$f" `basename "$f"`
        emacs -L . --batch -f batch-byte-compile `basename "$f"`
    done
done

popd
