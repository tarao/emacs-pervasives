#! /bin/sh

SIGNATURE="emacs-pervasives"

# remote info
SRC_REPOS=tarao/emacs-pervasives
GITHUB=http://github.com
EMACSWIKI=http://www.emacswiki.org/emacs/download
RAW_GITHUB=https://raw.github.com
RAW_SRC=$RAW_GITHUB/$SRC_REPOS/master

# local info
EMACSD="$HOME/.emacs.d"
INIT_EL="$HOME/.emacs $HOME/.emacs.el $HOME/.emacs.d/init.el"
GITHUB_PACKAGES="m2ym/popup-el m2ym/auto-complete emacsmirror/undo-tree"
EMACSWIKI_PACKAGES="anything.el anything-match-plugin.el anything-config.el \
    anything-obsolete.el anything-complete.el descbinds-anything.el"
PACKAGE_DST=$EMACSD/site-lisp

testing() {
    [ -n "$TEST" ] && return 0
    return 1
}

silent() {
    cmd="$1"; shift
    if [ -n "$VERBOSE" ]; then
        $cmd "$@" && return
    else
        $cmd "$@" >/dev/null 2>&1 && return
    fi
    return 1
}

executable() {
    type "$1" >/dev/null && return 0
    return 1
}

if executable wget; then
    wcat() {
        wget -O - "$1" 2>/dev/null && return
        return 1
    }
elif executable curl; then
    wcat() {
        curl -L "$1" && return
        return 1
    }
fi

is_github() {
    pattern='^\(git\|https\?\)://github\.com/[a-zA-Z0-9_]\+/[^/]\+\(\.git\)\?$'
    echo "$1" | grep "$pattern" >/dev/null && return
    return 1
}

github_repos() {
    pat_github='\(git\|https\?\):\/\/github\.com'
    pat_repos='\([a-zA-Z0-9_]\+\/[^/]\+\?\)'
    strip_ext='s/^\(.*\)\.git$/\1/'
    echo "$1" | sed -e "s/^$pat_github\/$pat_repos$/\\2/" | sed -e "$strip_ext"
}

pgit_clone() {
    url="$1"; shift

    executable git && git clone "$url" "$@" && return
    is_github "$url" || return 1
    executable sed || {
        echo 'You need sed to retrieve repository without git command' >&2
        return 1
    }

    http_repos=http://github.com/`github_repos "$url"`
    https_repos=https://github.com/`github_repos "$url"`
    svn_repos="$https_repos"

    executable svn && svn checkout "$svn_repos/trunk" "$@" && return

    executable tar && {
        tar_url="$http_repos/tarball/master"
        dir="$1"
        mkdir -p "$dir"
        wcat "$tar_url" | tar xfz - --strip-components 1 -C "$dir" && return
    }

    echo 'You need svn or tar to retrieve repository without git command' >&2
    return 1
}

pgit() {
    cmd="$1"; shift
    case "$cmd" in
    '' | '-h' | '--help')
        echo 'pgit COMMAND'
        ;;
    *)
       cmd="pgit_$cmd"
       executable "$cmd" && $cmd "$@" && return
       return 1
       ;;
    esac
}

clone() {
    echo "Retrieving $GITHUB/$1"
    testing && return

    silent pgit clone "$GITHUB/$1" "$1" || {
        echo 'You need either git, svn or tar to retrieve remote packages.'
        return 1
    }
}

emacswiki_get() {
    echo "Retrieving $EMACSWIKI/$1"
    testing && return

    sleep 2
    wcat "$EMACSWIKI/$1" > `basename "$1"`
}

select_init_el() {
    for f in $@; do
        [ -w "$f" ] && {
            echo $f
            return
        }
    done
    echo $f
}

install_init_el() {
    init=`select_init_el $INIT_EL`
    echo "Emacs init file: $init"

    if grep "$SIGNATURE" "$init" >/dev/null 2>&1; then
        echo "$SIGNATURE is already installed"
        return
    fi

    echo "Adding $RAW_SRC/init.el to $init"
    testing || wcat "$RAW_SRC/init.el" >> "$init"
}

emacs_compile() {
    echo -n "Compiling '$2'..."
    testing || {
        if silent emacs $1 --batch -f batch-byte-compile "$2"; then
            echo 'done'
        else
            echo 'fail'
        fi
    }
}

compile_els() {
    [ -d "$2" ] || return

    last=`pwd`
    cd "$2"

    for f in `ls *.el`; do
        emacs_compile "$1" "$f"
    done

    cd "$last"
}

install_github_packages() {
    load_path='-L .'
    for repos in "$@"; do
        [ -d "$repos" ] || clone "$repos"
        dir=`pwd`
        compile_els "$load_path" "$repos"
        load_path="$load_path -L $dir/$repos"
    done
}

install_emacswiki_packages() {
    load_path='-L .'
    for file in "$@"; do
        [ -e "$" ] || emacswiki_get "$file"
        emacs_compile "$load_path" `basename "$file"`
    done
}

( executable grep && executable wcat ) || {
    echo 'You need the following commands:'
    echo '  - grep'
    echo '  - wget or curl'
    exit 1
}

mkdir -p "$EMACSD"
install_init_el
mkdir -p "$PACKAGE_DST"
cd "$PACKAGE_DST"
install_github_packages $GITHUB_PACKAGES
install_emacswiki_packages $EMACSWIKI_PACKAGES
