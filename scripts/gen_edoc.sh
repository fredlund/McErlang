#!/bin/bash

# A stupid shell script for starting erlang with the right libraries;
# there is a probably a better way, but...

CMD=`basename $0`
PRG=$0

while [ -L "${PRG}" ]; do
    ls=`ls -ld "${PRG}"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "${link}" : '\/' > /dev/null; then
        PRG="${link}"
    else
        PRG="`dirname ${PRG}`/${link}"
    fi
done

EVHOME=`dirname "${PRG}"`/..

cd $EVHOME
erl -pa utility/ebin/ -noshell -s run_edoc run_edoc
