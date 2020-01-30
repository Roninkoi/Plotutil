set title "Säteily"
set xlabel "1/r2 [1/cm2]"
set ylabel "N [1/min]"
set grid
set key top left; set style data points
plot "-" using 1:2 title "data" with linespoints pt 7 lt 5 lc rgb "#ffaa00", "-" using 1:2 title "fit" with lines lt 5 lc rgb "#0000ff"

