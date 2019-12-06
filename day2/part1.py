#!/usr/bin/python3

import fileinput


def run(code, p=0):
    while True:
        inscode = code[p]
        if inscode == 1:
            code[code[p + 3]] = code[code[p + 1]] + code[code[p + 2]]
        elif inscode == 2:
            code[code[p + 3]] = code[code[p + 1]] * code[code[p + 2]]
        elif inscode == 99:
            print(",".join(map(lambda x: str(x), code)))
            return
        p += 4


if __name__ == '__main__':
    input = "".join(fileinput.input())
    code = list(map(lambda x: int(x), input.split(",")))
    run(code)
