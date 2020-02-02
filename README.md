# dotfiles

- Install [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh)
- Install [Emacs](https://github.com/railwaycat/homebrew-emacsmacport)
  - emacs-mac to enable composition-mode for [FiraCode](https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-mode-in-emacs-mac-port)
- Install [doom emacs](https://github.com/hlissner/doom-emacs)

## Init

- `cd ~`
- `git init --bare $HOME/.dotfiles`
- `echo '.dotfiles' >> .gitignore`
- `git remote add origin git@github.com:yosevu/dotfiles.git`
- `alias .git='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
- `git. config --local status.showUntrackedFiles no`

## Clone

- `cd ~`
- `git clone --bare <git-repo-url> $HOME/.dotfiles`
- `alias git.='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
- `git. checkout`
- `git. config --local status.showUntrackedFiles no`

## Reference

[dotfiles](https://www.atlassian.com/git/tutorials/dotfiles)
