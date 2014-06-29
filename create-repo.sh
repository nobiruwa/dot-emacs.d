CUR_DIR=`pwd`
REPO_DIR=~/repo

NOBIRUWA_DIR=~/repo/nobiruwa.github
mkdir -p $NOBIRUWA_DIR

cd $NOBIRUWA_DIR
git clone https://github.com/nobiruwa/EastAsianAmbiguousWidth.git EastAsianAmbiguousWidth.git
git clone https://github.com/nobiruwa/dot-emacs.d.git dot-emacs.d.git
git clone https://github.com/nobiruwa/yasnippet-snippets.git yasnippet-snippets.git

cd $REPO_DIR
git clone https://github.com/senny/emacs-eclim.git emacs-eclim.git
git clone https://github.com/daleharvey/jshint-mode.git jshint-mode.git
git clone https://github.com/fxbois/web-mode.git web-mode.git
git clone https://github.com/emacsmirror/cedet.git cedet.git
cd cedet.git
touch `find . -name Makefile`
make
cd ..
cd $CUR_DIR
