wget -N http://www.bookshelf.jp/elc/color-grep.el
wget -N http://www.bookshelf.jp/elc/color-moccur.el
wget -N http://www.bookshelf.jp/elc/grep-edit.el
wget -N http://www.bookshelf.jp/elc/moccur-edit.el
wget -N http://www.emacswiki.org/emacs/download/java-mode-indent-annotations.el

sed -i -e 's/toggle-read-only/read-only-mode/g' *.el
