#!/bin/bash
make && 
./exec &&
gnuplot display_fdtd1.gp && 
gnuplot display_fdtd2.gp && 
gnuplot display_fdtd3.gp &&
gnuplot display_fdtd4.gp &&
gnuplot display_fdtd5.gp &&
gnuplot display_fdtd6.gp &&
gnuplot display_fdtd7.gp &&
gnuplot display_fdtd8.gp &&
gnuplot display_fdtd8.gp &&
gnuplot display_fdtd9.gp &&
gnuplot display_fdtd10.gp &&
  make clean
