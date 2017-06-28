#!/bin/bash

set -e

ARGCOUNT=$#

export COMMDIR=`pwd -P`
export SCHEDULER=$1
export SCHPARAM=$2
export TEST=$3
export BOUND=500
export TESTPATH="$PWD/comm_tests/"
export SUTPATH="${PWD%/*}/comm_benchs/$TEST"

#comm_benchs/$TEST"

while getopts ":n" opt; do
  case $opt in
    n)
      NEWTEST=$2
      sed "1s/.*/-module(${NEWTEST}_comm)./" comm_test_template.erl > comm_tests/${NEWTEST}_comm.erl
      exit 0
      ;;
  esac
done

if [ "$ARGCOUNT" -gt 3 ] || [ "$ARGCOUNT" -lt 3 ]; then
	echo "Wrong number of arguments: $0 scheduler_type scheduler_param test_name"
	exit 1
fi

if [ ! "$SCHEDULER" = "delay" ] && [ ! "$SCHEDULER" = "random" ]; then
  echo "The scheduler $SCHEDULER is not defined: $0 scheduler_type scheduler_param app_under_test"
  exit 1
fi

SCH_LONG="comm_$SCHEDULER_scheduler"

echo $SCH_LONG

APPSCHDIR=schedules/$TEST/$SCHEDULER

if [ ! -d "$APPSCHDIR" ]; then
  mkdir -p $APPSCHDIR
fi

# Compile commander
make clean
make all

# Run the test for SUT
make run-test

