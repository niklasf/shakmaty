#!/bin/sh -e

cd tables/chess
wget -w 1 -nc -i TEST-SOURCE.txt
cd ../..

cd tables/atomic
wget -w 1 -nc -i TEST-SOURCE.txt
cd ../..

cd tables/antichess
wget -w 1 -nc -i TEST-SOURCE.txt
cd ../..
