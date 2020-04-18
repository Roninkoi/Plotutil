set title "Säteily (HT80, 120 s)"
set xlabel "Lkm"
set ylabel "Ln N"
set grid
set key bottom left; set style data points
plot "-" using 1:2:3 title "data" with errorbars lt 5 lc rgb "#ffaa00", "-" using 1:2 title "sovitus" with lines lt 5 lc rgb "#0000ff", "-" using 1:2 title "sovitus max" with lines dt 2 lt 5 lc rgb "#ff00ff", "-" using 1:2 title "sovitus min" with lines dt 2 lt 5 lc rgb "#00ffff", "-" using 1:2 title "teoria" with lines lt 5 lc rgb "#00ff00"
