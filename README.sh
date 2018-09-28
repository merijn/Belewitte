#!/bin/sh
# The Prerequisites
# =================

set -euo pipefail

#export MODULEPATH="/cm/local/modulefiles:/cm/shared/modulefiles"
#
#module() { eval `/cm/local/apps/environment-modules/3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash $*`; }
#export -f module
#
#PKG_CONFIG_PATH="${PKG_CONFIG_PATH%:}"
#PKG_CONFIG_PATH="${PKG_CONFIG_PATH#:}"
#
#PATH="$HOME/.cabal/bin:$HOME/opt/bin:$PATH"
#
#module load opencl-nvidia/9.0
#module load cuda90/blas/9.0.176
#module load cuda90/fft/9.0.176
#module load cuda90/nsight/9.0.176
#module load cuda90/profiler/9.0.176
#module load cuda90/toolkit/9.0.176
#module load prun
#module load slurm
#module load prun-slurm

# * CUDA 9 (searches includes and libraries relative to CUDA\_PATH environment
#   variable)
#
# * OpenCL (looks for libraries in path specified by OPENCL\_LIB environment
#   variable)
#
# * A C++17 supporting compiler (e.g., g++ 7.x or clang++ 5.x)

PARALLEL=$(awk '/^processor/{n+=1}END{print n}' /proc/cpuinfo)
INSTALL_PATH=$HOME/opt/
PATH="$HOME/.cabal/bin:$INSTALL_PATH/bin/:$PATH"

wget https://cmake.org/files/v3.12/cmake-3.12.1.tar.gz
wget https://releases.llvm.org/6.0.1/llvm-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/cfe-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/compiler-rt-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/libcxx-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/libcxxabi-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/libunwind-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/openmp-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/polly-6.0.1.src.tar.xz
wget https://releases.llvm.org/6.0.1/clang-tools-extra-6.0.1.src.tar.xz

tar xf cmake-3.12.1.tar.gz
rm cmake-3.12.1.tar.gz
cd cmake-3.12.1/
./bootstrap --prefix=$INSTALL_PATH --parallel=$PARALLEL
make
make install
hash -r
cd ..

tar xf llvm-6.0.1.src.tar.xz
rm llvm-6.0.1.src.tar.xz
tar xf cfe-6.0.1.src.tar.xz
mv cfe-6.0.1.src llvm-6.0.1.src/tools/clang
rm cfe-6.0.1.src.tar.xz
tar xf clang-tools-extra-6.0.1.src.tar.xz
mv clang-tools-extra-6.0.1.src llvm-6.0.1.src/tools/clang/tools/extra
rm clang-tools-extra-6.0.1.src.tar.xz
tar xf compiler-rt-6.0.1.src.tar.xz
mv compiler-rt-6.0.1.src llvm-6.0.1.src/projects/compiler-rt
rm compiler-rt-6.0.1.src.tar.xz
tar xf libcxx-6.0.1.src.tar.xz
mv libcxx-6.0.1.src llvm-6.0.1.src/projects/libcxx
rm libcxx-6.0.1.src.tar.xz
tar xf polly-6.0.1.src.tar.xz
mv polly-6.0.1.src llvm-6.0.1.src/tools/polly
rm polly-6.0.1.src.tar.xz
tar xf openmp-6.0.1.src.tar.xz
mv openmp-6.0.1.src llvm-6.0.1.src/projects/openmp
rm openmp-6.0.1.src.tar.xz
tar xf libcxxabi-6.0.1.src.tar.xz
mv libcxxabi-6.0.1.src llvm-6.0.1.src/projects/libcxxabi
rm libcxxabi-6.0.1.src.tar.xz
tar xf libunwind-6.0.1.src.tar.xz
mv libunwind-6.0.1.src llvm-6.0.1.src/runtimes/libunwind
rm libunwind-6.0.1.src.tar.xz
mkdir build
cd build/
CC=gcc CXX=g++ cmake ../llvm-6.0.1.src/ -DCMAKE_INSTALL_PREFIX=$INSTALL_PATH -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86
make -j$PARALLEL
make install
hash -r
cd ..

# * Boost (looks for includes and libraries relative to BOOST\_PATH, defaults
#   to $HOME/opt/)

wget https://dl.bintray.com/boostorg/release/1.68.0/source/boost_1_68_0.tar.gz
tar xvf boost_1_68_0.tar.gz
rm boost_1_68_0.tar.gz
cd boost_1_68_0
./bootstrap.sh --with-toolset=clang --prefix=$INSTALL_PATH
./b2 toolset=clang cxxflags="-stdlib=libc++" linkflags="-stdlib=libc++" -j$PARALLEL
./b2 install
cd ..
rm -r boost_1_68_0

# * GHC 8.4

wget https://downloads.haskell.org/~ghc/8.4.3/ghc-8.4.3-x86_64-deb8-linux-dwarf.tar.xz
tar xvf ghc-8.4.3-x86_64-deb8-linux-dwarf.tar.xz
rm ghc-8.4.3-x86_64-deb8-linux-dwarf.tar.xz
cd ghc-8.4.3
./configure --prefix=$INSTALL_PATH
make install
hash -r
cd ..
rm -r ghc-8.4.3

# * cabal-install 2.4

git clone https://github.com/haskell/cabal.git
cd cabal/cabal-install
./bootstrap.sh -j $PARALLEL
cabal new-update
cd ../..
rm -fr cabal

# * ICC (Only required for evolutionary graph generation)
#
# * setuptools

virtualenv $INSTALL_PATH/
pip install setuptools --upgrade

# * NumPy, SciPy, scikit-learn, matplotlib

pip install numpy scipy scikit-learn matplotlib
