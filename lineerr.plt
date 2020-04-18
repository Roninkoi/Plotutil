set title "Koe"
set xlabel "x"
set ylabel "y"
set grid
set key bottom right; set style data points
plot "-" using 1:2:3 title "data" with errorbars lt 5 lc rgb "#ffaa00", "-" using 1:2 title "fit" with lines lt 5 lc rgb "#0000ff", "-" using 1:2 title "fit max" with lines dt 2 lt 5 lc rgb "#ff00ff", "-" using 1:2 title "fit min" with lines dt 2 lt 5 lc rgb "#00ffff"
