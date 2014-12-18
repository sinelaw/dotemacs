#!/bin/bash -ue

cmd="$@"

git subtree ${cmd} -P modes/dash https://github.com/magnars/dash.el.git master
git subtree ${cmd} -P modes/diff-hl https://github.com/dgutov/diff-hl.git master
git subtree ${cmd} -P modes/flycheck https://github.com/flycheck/flycheck.git 0.21
git subtree ${cmd} -P modes/gitmodes https://github.com/magit/git-modes.git master
git subtree ${cmd} -P modes/magit https://github.com/magit/magit.git master
git subtree ${cmd} -P modes/haskell-mode https://github.com/haskell/haskell-mode.git master
