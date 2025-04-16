set terminal pngcairo enhanced size 800,600
set output "frames/frame_%05d.png"  # %05d = numéro sur 5 chiffres (ex: 00001)
set xlabel "Position"
set ylabel "Amplitude"
set grid

Nt = 1000  # Nombre total de pas de temps
Δt = 1e-3  # Pas de temps en secondes
do for [n=0:Nt-1] {
    filename = sprintf("data/E_t_%05d.txt", n)
    plot filename using 1:2 with lines title sprintf("t = %.2f ns", n*Δt*1e9)
}