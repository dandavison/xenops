echo "install-auctex.sh"
set -eu
dir="$1"
cd "$dir"

if [ $(uname) != Darwin ]; then
    echo "apt-get update..."
    sudo apt-get update > /dev/null

    echo "apt-get install..."
    sudo apt-get -y install \
         texinfo \
         texlive-binaries \
         texlive-base \
         texlive-latex-base > /dev/null
fi

if [ ! -d auctex ]; then
    echo "git clone auctex..."
    git clone git://git.savannah.gnu.org/auctex.git
fi


echo "build auctex..."
cd auctex
git clean -fxd
./autogen.sh > /dev/null
./configure --prefix=$PWD --with-lispdir=$PWD > /dev/null
make lisp > /dev/null
