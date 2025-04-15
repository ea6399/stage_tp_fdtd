#!/bin/bash
make && ./exec && gnuplot display_fdtd1.gp && gnuplot display_fdtd2.gp && gnuplot display_fdtd3.gp && gnuplot display_fdtd4.gp && gnuplot display_fdtd5.gp &&
gnuplot display_fdtd6.gp && make clean
