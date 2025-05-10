#!/bin/bash
fpc pasta
git clean -df .
ZIP=pasta80-`./pasta --version`-`uname -s`-`uname -m`.zip
zip -r $ZIP LICENSE.txt README.md pasta examples rtl tests
echo
echo ------------------------------[ Release ready ]---------------------------------
ls -l $ZIP
echo --------------------------------------------------------------------------------
echo
