wget -N http://www.emacswiki.org/emacs/download/color-grep.el
wget -N http://www.emacswiki.org/emacs/download/color-moccur.el
wget -N http://www.emacswiki.org/emacs/download/grep-edit.el
wget -N http://www.emacswiki.org/emacs/download/moccur-edit.el
wget -N http://www.emacswiki.org/emacs/download/java-mode-indent-annotations.el

sed -i -e 's/toggle-read-only/read-only-mode/g' *.el
