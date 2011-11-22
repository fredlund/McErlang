#!/bin/sh

TempFile1=/tmp/convertLTL$$.ltl
TempFile2=/tmp/convertLTL$$.xml
echo $1 > $TempFile1
java -classpath ../test/ref_impl/LTL2Buchi/ltl2buchi.jar gov/nasa/ltl/trans/LTL2Buchi -o xml -f $TempFile1 > $TempFile2
sed -n '/version/,/graph>/p' < $TempFile2
rm $TempFile1
rm $TempFile2
