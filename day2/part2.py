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
            return code[0]
        p += 4


if __name__ == '__main__':
    input = "".join(fileinput.input())
    code = list(map(lambda x: int(x), input.split(",")))
    for noun in range(0, 100):
        for verb in range(0, 100):
            code_copy = code.copy()
            code_copy[1] = noun
            code_copy[2] = verb
            if run(code_copy) == 19690720:
                print(f"noun = {noun}, verb = {verb}")
