#!/usr/bin/env bash

for f in */output/*.pdf ; do gs -q -dNOCACHE -dNOPAUSE -dBATCH -dSAFER -sDEVICE=eps2write -sOutputFile="${f%.pdf}.eps" "$f" ; done
