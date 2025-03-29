#/bin/bash
cp $1 bytes.bin
zxnftp "rm bytes.bin" "put bytes.bin" "quit"
rm bytes.bin
