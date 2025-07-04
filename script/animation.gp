set terminal pngcairo enhanced size 800,600
set output "frames/frame_%07d.png"  # %05d = numéro sur 5 chiffres (ex: 00001)
set xlabel "Position"
set ylabel "Amplitude"
set yrange[-2 : 2]  # Ajustez la plage y selon vos données
set grid

Nsnapshots = 300  # Nombre total de snapshots dans votre fichier

do for [n=1:Nsnapshots] {
    outputfile = sprintf("frames/frame_%05d.png", n)
    set output outputfile
    plot "E.txt" using 1:(column(n+1)) with lines title "Wave propagation"
}