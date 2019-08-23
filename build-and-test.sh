#!/usr/bin/env bash

scriptroot="$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )"

CONFIGURATION=Debug

while [ $# -gt 0 ]; do
  case "$1" in
    --configuration|-c)
      CONFIGURATION=$2
      shift
      ;;
    *)
      echo "Invalid argument: $1"
      exit 1
      ;;
  esac
  shift
done

SOLUTION=$scriptroot/IxMilia.Lisp.sln
dotnet restore $SOLUTION
dotnet build $SOLUTION -c $CONFIGURATION
dotnet test $SOLUTION -c $CONFIGURATION
