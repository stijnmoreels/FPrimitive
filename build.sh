#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")"

PAKET_BOOTSTRAPPER_EXE=.paket/paket.bootstrapper.exe
PAKET_EXE=.paket/paket.exe
FAKE_EXE=packages/build/FAKE/tools/FAKE.exe

FSIARGS=""
FSIARGS2=""
OS=${OS:-"unknown"}
if [ "$OS" != "Windows_NT" ]
then
  # Can't use FSIARGS="--fsiargs -d:MONO" in zsh, so split it up
  # (Can't use arrays since dash can't handle them)
  FSIARGS="--fsiargs"
  FSIARGS2="-d:MONO"
fi

run() {
  if [ "$OS" != "Windows_NT" ]
  then
    mono "$@"
  else
    "$@"
  fi
}

yesno() {
  # NOTE: Defaults to NO
  read -p "$1 [y/N] " ynresult
  case "$ynresult" in
    [yY]*) true ;;
    *) false ;;
  esac
}

set +e
run $PAKET_BOOTSTRAPPER_EXE
bootstrapper_exitcode=$?
set -e

if [ "$OS" != "Windows_NT" ] &&
       [ $bootstrapper_exitcode -ne 0 ] &&
       [ $(certmgr -list -c Trust | grep X.509 | wc -l) -le 1 ] &&
       [ $(certmgr -list -c -m Trust | grep X.509 | wc -l) -le 1 ]
then
  echo "Your Mono installation has no trusted SSL root certificates set up."
  echo "This may result in the Paket bootstrapper failing to download Paket"
  echo "because Github's SSL certificate can't be verified. One way to fix"
  echo "this issue would be to download the list of SSL root certificates"
  echo "from the Mozilla project by running the following command:"
  echo ""
  echo "    mozroots --import --sync"
  echo ""
  echo "This will import over 100 SSL root certificates into your Mono"
  echo "certificate repository."
  echo ""
  if yesno "Run 'mozroots --import --sync' now?"
  then
    mozroots --import --sync
  else
    echo "Attempting to continue without running mozroots. This might fail."
  fi
  # Re-run bootstrapper whether or not the user ran mozroots, because maybe
  # they fixed the problem in a separate terminal window.
  run $PAKET_BOOTSTRAPPER_EXE
fi

run $PAKET_EXE restore

# liberated from https://stackoverflow.com/a/18443300/433393
realpath() {
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
}

TOOL_PATH=$(realpath .fake)
FAKE="$TOOL_PATH"/fake

if ! [ -e "$FAKE" ]
then
  dotnet tool install fake-cli --tool-path "$TOOL_PATH"
fi

FAKE_DETAILED_ERRORS=true "$FAKE" build -t "$@"