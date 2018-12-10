#!/bin/bash

function test_it () {
 $1 -l $2 $3 "run_tests."
}
function test_sicstus() {
  sicstus -f --noinfo --nologo -l $1 --goal "run_tests."
}

function test_swipl() {
  swipl -O -l $1 -g run_tests
}

for i in test_*.pl; do
    if [ -f $i ];then
        #echo "I do something with the file $i"
        #test_it $1 $i $2 "$3."
        test_$1 $i
    fi
done

for i in test?.pl; do
    if [ -f $i ];then
       #test_it $1 $i $2 "$3."
       test_$1 $i
    fi
done

