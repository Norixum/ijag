#!/usr/bin/bash
nasm -f elf64 output.nasm -g
ld -o output output.o
./output
