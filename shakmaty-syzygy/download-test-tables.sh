#!/bin/sh -e

(cd tables/chess && wget -w 1 -nc -i TEST-SOURCE.txt)
(cd tables/atomic && wget -w 1 -nc -i TEST-SOURCE.txt)
(cd tables/antichess && wget -w 1 -nc -i TEST-SOURCE.txt)
