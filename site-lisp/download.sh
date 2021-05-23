# cannot download anymore
# wget -N http://www.emacswiki.org/emacs/download/color-grep.el
wget -N http://www.emacswiki.org/emacs/download/color-moccur.el
# can only download older version
# wget -N http://www.emacswiki.org/emacs/download/grep-edit.el
wget -N http://www.emacswiki.org/emacs/download/moccur-edit.el
wget -N http://www.emacswiki.org/emacs/download/java-mode-indent-annotations.el

sed -i -e 's/toggle-read-only/read-only-mode/g' *.el
