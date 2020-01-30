#!/usr/bin/env gnuplot

set terminal png
set output 'state-vs-ioref.png'
set encoding utf8

set xlabel "n"
set ylabel "time (/μs)"

set logscale x
set logscale y

plot \
  "state-vs-ioref.data" using 1:2 title "sumByState" with linespoints ps 0.5, \
  "state-vs-ioref.data" using 1:3 title "sumByIORef" with linespoints ps 0.5
