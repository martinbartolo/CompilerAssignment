#!/bin/bash
set -e
export LLVM_INSTALL_PATH=/modules/cs325/llvm-8/
#export LLVM_INSTALL_PATH=/home/gihan/LLVM/install

export PATH=$LLVM_INSTALL_PATH/bin:$PATH
export LD_LIBRARY_PATH=$LLVM_INSTALL_PATH/lib:$LD_LIBRARY_PATH
CLANG=$LLVM_INSTALL_PATH/bin/clang++

DIR="$(pwd)"

### Build mccomp compiler
echo "Cleanup *****"
rm -rf ./mccomp

echo "Compile *****"

#$CLANG -g -O3 ./mccomp.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` \
#-Wno-unused-function -Wno-unknown-warning-option -o mccomp
make clean
make mccomp

COMP=$DIR/mccomp
echo $COMP

function validate {
  $1 > perf_out
  echo
  echo $1
  grep "Result" perf_out;grep "PASSED" perf_out
  rc=$?; if [[ $rc != 0 ]]; then echo "TEST FAILED *****";exit $rc; fi;rm perf_out
}

echo "Test *****"

cd tests/addition/
pwd
rm -rf output.ll add
"$COMP" ./addition.c
$CLANG driver.cpp output.ll  -o add
validate "./add"


cd ../factorial 
pwd
rm -rf output.ll fact
"$COMP" ./factorial.c
$CLANG driver.cpp output.ll -o fact
validate "./fact"

cd ../fibonacci
pwd
rm -rf output.ll fib
"$COMP" ./fibonacci.c
$CLANG driver.cpp output.ll -o fib
validate "./fib"

cd ../pi
pwd
rm -rf output.ll pi
"$COMP" ./pi.c
$CLANG driver.cpp output.ll -o pi
validate "./pi"

cd ../while
pwd
rm -rf output.ll while
"$COMP" ./while.c
$CLANG driver.cpp output.ll -o while
validate "./while"

cd ../void
pwd
rm -rf output.ll void
"$COMP" ./void.c 
$CLANG driver.cpp output.ll -o void
validate "./void"

cd ../cosine
pwd
rm -rf output.ll cosine
"$COMP" ./cosine.c
$CLANG driver.cpp output.ll -o cosine
validate "./cosine"

cd ../unary
pwd
rm -rf output.ll unary
"$COMP" ./unary.c
$CLANG driver.cpp output.ll -o unary
validate "./unary"

cd ../recurse
pwd
rm -rf output.ll recurse
"$COMP" ./recurse.c
$CLANG driver.cpp output.ll -o recurse
validate "./recurse"

cd ../rfact
pwd
rm -rf output.ll rfact
"$COMP" ./rfact.c
$CLANG driver.cpp output.ll -o rfact
validate "./rfact"

cd ../palindrome
pwd
rm -rf output.ll palindrome
"$COMP" ./palindrome.c
$CLANG driver.cpp output.ll -o palindrome
validate "./palindrome"

echo "***** ALL TESTS PASSED *****"
