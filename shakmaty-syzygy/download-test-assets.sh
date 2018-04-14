#!/bin/sh -e

cd tables/regular
wget -nc -i TEST-SOURCE.txt
cd ../..

cd tables/atomic
wget -nc -i TEST-SOURCE.txt
cd ../..
