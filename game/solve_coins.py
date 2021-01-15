#!/usr/bin/env python3

"""
A script to brute force the coin game.

My brain isn't big enough to do the maths.
"""

from collections import namedtuple
import itertools

Coin = namedtuple("Coin", ["color", "value"])

coins = [
    Coin("red", 2),
    Coin("corroded", 3),
    Coin("shiny", 5),
    Coin("concave", 7),
    Coin("blue", 9),
]


def check_perm(p):
    s = p[0].value + p[1].value * (p[2].value ** 2) + (p[3].value ** 3) - p[4].value
    return s == 399


def take_coins(p):
    return "".join(f"take {c.color} coin\n" for c in p)


def use_coins(p):
    return "".join(f"use {c.color} coin\n" for c in p)


for perm in itertools.permutations(coins, r=5):
    if check_perm(perm):
        print(take_coins(perm))
        print(use_coins(perm))
        break
else:
    print("No configuration found... you suck at programming.")
