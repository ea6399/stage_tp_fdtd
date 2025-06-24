#!/usr/bin/env bash

exe="./bin/exec"    # remplacez par le chemin de votre binaire
outfile="times_fd.csv"  # résultat en CSV pour Excel/gnuplot…
nruns=100             # nombre d’itérations

echo "run,elapsed_s" > $outfile
for i in $(seq 1 $nruns); do
  echo "run $i"
  t0=$(date +%s.%N)
  $exe
  t1=$(date +%s.%N)
  elapsed=$(echo "$t1 - $t0" | bc)
  echo "$i,$elapsed" >> $outfile
done
echo "Terminé – résultats dans $outfile"