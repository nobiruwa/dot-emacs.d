CUR_DIR=`pwd`
REPO_DIR=~/repo

NOBIRUWA_DIR=~/repo/nobiruwa.github
mkdir -p $NOBIRUWA_DIR

cd $NOBIRUWA_DIR
git clone https://github.com/nobiruwa/east-asian-ambiguous-width.git east-asian-ambiguous-width.git
git clone https://github.com/nobiruwa/dot-emacs.d.git dot-emacs.d.git
git clone https://github.com/nobiruwa/yasnippet-snippets.git yasnippet-snippets.git

cd $CUR_DIR
