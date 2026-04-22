#!/bin/bash
fpc pasta
git clean -df .
find . -name .DS_Store -delete
ZIP=pasta80-`./pasta --version`-`git rev-parse --short HEAD`-`uname -s`-`uname -m`.zip
zip -r $ZIP LICENSE.txt README.md pasta docs examples misc rtl tests
echo
echo ------------------------------[ Release ready ]---------------------------------
ls -lho $ZIP
echo --------------------------------------------------------------------------------
echo
