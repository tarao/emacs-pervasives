#! /bin/sh

INIT=~/.emacs.d/init.el

SRC_REPOS=tarao/emacs-pervasives
GITHUB=http://github.com
RAW_GITHUB=http://raw.github.com
SRC=$GITHUB/$SRC_REPOS
RAW_SRC=$RAW_GITHUB/$SRC_REPOS/master
EMACSD=~/.emacs.d
DST=$EMACSD/site-lisp
SIGNATURE="emacs-pervasives"

VCS=""
REPOS_DIR=""

(which svn >/dev/null && [ -x `which svn` ]) && {
    VCS="svn checkout"
    REPOS_DIR="/trunk"
}
(which git >/dev/null && [ -x `which git` ]) && {
    VCS="git clone"
    REPOS_DIR=""
}

[ "x$VCS" = 'x' ] && {
    echo "You need to have git or svn installed."
    exit
}

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
    [ -d "$repos" ] || $VCS "$GITHUB/$repos$REPOS_DIR" "$repos"
    last=`pwd`
    cd "$repos"
    for f in `ls *.el`; do
        emacs $LOAD --batch -f batch-byte-compile "$f"
    done
    cd "$last"
    LOAD="$LOAD -L $last/$repos"
done
