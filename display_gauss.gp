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
    pointtype 7 \
    pointsize 0.5 \
    linecolor rgb "red"

# Format des données 
# Exemple: colonne 1 = temps, colonne 2 = signal
plot "gauss.txt" using 1:2 with lines ls 1 title "Signal temporelle analytique"

pause -1 "Appuyez sur Entrée pour quitter..." 
exit 