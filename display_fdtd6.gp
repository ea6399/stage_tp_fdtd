# Configuration du terminal
set terminal wxt enhanced persist size 800,600
# set terminal pngcairo enhanced font "arial,10" size 800,600
# set output "output.png"

# Titres et labels
set title "Gaussienne temporelle"
set xlabel "t"
set ylabel "e(t)"
set grid

# Légende
set key top right

#Format
set style line 1 \
    linecolor rgb "blue" \
    linetype 1 \
    linewidth 2

set style line 2 \
    linetype 2 \
    linewidth 1 \
    linecolor rgb "red"

# Format des données 
plot "E_t.txt" using 1:7 with lines ls 1 title "Champs électrique", \
     "H_t.txt" using 1:7 with lines ls 2 title "Champs magnétique"