set terminal pngcairo enhanced size 800,600
set output "frames/frame_%05d.png"  
set xlabel "Position"
set ylabel "Amplitude"
set yrange[-0.01 : 0.01]  # Ajustez la plage y selon vos données
set grid

Nsnapshots = 500  # Nombre total de snapshots dans votre fichier

do for [n=1:Nsnapshots] {
outputfile = sprintf("frames/frame_%05d.png", n)
    set output outputfile
    plot "H_t.txt" using 1:(column(n+1)) with lines title "Electric Wave propagation"
}