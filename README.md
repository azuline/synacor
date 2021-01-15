# Synacor Challenge

My ~~solution to~~ attempt at the Synacor challenge.

https://challenge.synacor.com/

## Notes

Running:

```sh
$ cabal run synacor challenge.bin
```

Using a map to represent memory really is inefficient, probably better to
switch to an array. It would also be nice to use an array for the registers.

Save point encoded as instructions in `game/save_point.in`.

Coin game brute forcer in `game/solve_coins.py`.

Later^tm I'll go write a little debugger and TUI for this... and RE the
teleporter.

## Progress

- [x] Code 1 - mxNXGuMXzRII (free)
- [x] Code 2 - qdnbyIYbtdIq (opcodes 0,19,21)
- [x] Code 3 - eqoTTVUAOdxX (passed self-test)
- [x] Code 4 - dYhOtyHmLAnE (start -> tablet -> use tablet)
- [x] Code 5 - nzOQMPRdYiCJ (ladder made, go west -> south -> north)
- [x] Code 6 - jPkGCXxNysAV (coin game -> teleporter)
- [ ] Code 7
- [ ] Code 8
