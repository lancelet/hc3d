#!/bin/bash
# 
# Fetches test C3D files from c3d.org, and dumps parts of them for testing.
#
# The copyright status of these files is somewhat uncertain, so they are not
# included intact in the source repository. Instead, this script can be
# used to fetch them when required. Portions of the C3D files are used in the
# test suite, but not whole files.

# locations
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repodir=$scriptdir/..
datadir=$repodir/data

# check that the hexutil command exists
pushd $repodir > /dev/null
stack exec -- hexutil --help &>/dev/null
if [ $? -ne 0 ]
then
  echo "Could not execute the hexutil command!"
  echo "Have you compiled it yet? (stack build)"
  exit 1
else
  echo "Verified that the hexutil command exists"
fi
popd > /dev/null

# safe bash settings
set -euf -o pipefail

# location to put C3D files
mkdir -p $datadir

# fetches a file from C3D.org to the data directory
# arguments:
#   filename - name of the file to fetch (eg. sample01.zip)
function fetchc3d {
    filename=$1
    if [ ! -e $datadir/$filename ]
    then
        pushd $datadir > /dev/null
        echo "Fetching $filename from C3D.org"
        curl -O -# https://www.c3d.org/data/$filename
        unzip -o $filename
        popd > /dev/null
    else
        echo "File $filename already fetched"
    fi
}

# dumps part of a file into Haskell format (for testing)
# arguments:
#   srcfile - source file (in data dir, but without path)
#   dstfile - destination file (in data dir, but without path)
#   skip    - skip this many bytes before dumping
#   length  - number of bytes to dump
function bindump {
    srcfile=$1
    dstfile=$2
    skip=$3
    length=$4
    echo "Dumping as Hex:"
    echo "  Source file      $datadir/$srcfile"
    echo "  Destination file $datadir/$dstfile"
    echo "  Origin           $skip"
    echo "  Length           $length"
    pushd $repodir > /dev/null
    stack exec hexutil -- \
      $datadir/$srcfile \
      --skip $skip \
      --take $length \
      > $datadir/$dstfile
    popd > /dev/null
}

# fetch the example files we need
echo "Fetching files if necessary"
fetchc3d sample01.zip

# extract parts from the files that we need
echo "Extracting parts from files"
bindump Eb015pr.c3d Eb015pr_header.hs 0 512

