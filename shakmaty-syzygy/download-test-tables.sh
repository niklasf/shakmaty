#!/bin/sh -e

cd tables/chess
wget -nc -i TEST-SOURCE.txt
cd ../..

cd tables/atomic
wget -nc -i TEST-SOURCE.txt
cd ../..

cd tables/antichess
wget -nc -i TEST-SOURCE.txt
cd ../..
