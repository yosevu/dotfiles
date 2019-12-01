# dotfiles

- Install [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh)
- Install [Emacs](https://github.com/d12frosted/homebrew-emacs-plus)
- Install [doom emacs](https://github.com/hlissner/doom-emacs)


## Init

- `cd ~`
- `git init --bare $HOME/.dotfiles`
- `echo '.dotfiles' >> .gitignore`
- `git remote add origin git@github.com:yosevu/dotfiles.git`
- `alias dot='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
- `dot config --local status.showUntrackedFiles no`

## Clone

- `cd ~`
- `git clone --bare <git-repo-url> $HOME/.dotfiles`
- `alias dot='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
- `dot checkout`
- `dot config --local status.showUntrackedFiles no`

## Reference

[dotfiles](https://www.atlassian.com/git/tutorials/dotfiles)
