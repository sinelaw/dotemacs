#!/bin/bash -ue

cmd=echo

while [[ $[$# >= 1] == 1 ]] ; do
    case $1 in
	clone-non-existing) cmd=clone-non-existing ; shift 1 ;;
	*) echo $1; break;;
    esac
done

clone-non-existing() {
    path=$1
    url=$2
    branch=$3

    if [[ ! -d ${path} ]] ; then
	git subtree add --squash -P ${path} ${url} ${branch}
    fi
}

${cmd} modes/dash https://github.com/magnars/dash.el.git master
${cmd} modes/diff-hl https://github.com/dgutov/diff-hl.git master
${cmd} modes/flycheck https://github.com/flycheck/flycheck.git 0.21
${cmd} modes/gitmodes https://github.com/magit/git-modes.git master
${cmd} modes/magit https://github.com/magit/magit.git master
${cmd} modes/haskell-mode https://github.com/haskell/haskell-mode.git master
${cmd} modes/smooth-scrolling https://github.com/aspiers/smooth-scrolling.git master
${cmd} modes/rust add https://github.com/rust-lang/rust-mode master
${cmd} modes/flycheck-rust https://github.com/flycheck/flycheck-rust master
${cmd} modes/dmode https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode.git master
${cmd} modes/grep-a-lot https://github.com/ZungBang/emacs-grep-a-lot.git master
${cmd} modes/company-mode https://github.com/company-mode/company-mode 0.8.11
