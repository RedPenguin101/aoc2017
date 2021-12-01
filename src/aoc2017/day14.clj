(ns aoc2017.day14)

(def input "ljoxqyyw")

"Day 14: Disk Defragmentation
 128x128 finite grid
 Binary state of coords = free or used
 State of a row is tracked by bits in a knot hash (ref day10)
 1=used, 0=free
 hash input=[string]-[row-number]
 eg row1 hash is ljoxqyyw-1
 output is 32 4-bit hex numbers (0x0 - 0xf)
 "

"Part1: how many squares are used?"