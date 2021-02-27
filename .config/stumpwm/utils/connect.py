#!/bin/python

import sys

def parseLine(line: str):
    sp=line.split('|')
    if len(sp) == 1:
        left = sp[0]
        right = left
    else:
        (left, right) = sp

    left = left.split(' ')
    right = right.split(' ')

    ind = 0

    left1 = left[0]
    right1 = right[0]
    left2 = left[1]
    right2 = right[1]

    if left2 == right2 and left1 == right1:
        pass
    elif left2 == 'NoSymbol' or right2 == 'NoSymbol' or left2 == right2:
        print(f'("{left1}" . "{right1}")')
    elif left1 == 'NoSymbol' or right1 == 'NoSymbol' or left1 == right1:
        print(f'("{left2}" . "{right2}")')
    else:
        print(f'("{left1}" . "{right1}")\n("{left2}" . "{right2}")')

print("(make-key-translation \"bg phonetic\" '(")
for line in sys.stdin:
    parseLine(line)
print("))")
