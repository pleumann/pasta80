#!/bin/bash
fpc pasta80
git clean -df .
find . -name .DS_Store -delete
ZIP=pasta80-`./pasta80 --version`-`git rev-parse --short HEAD`-`uname -s`-`uname -m`.zip
zip -r $ZIP LICENSE.txt README.md pasta80 docs examples misc rtl tests
echo
echo ------------------------------[ Release ready ]---------------------------------
ls -lho $ZIP
echo --------------------------------------------------------------------------------
echo
