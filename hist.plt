set title "Energy distribution"
set xrange [0:]
set yrange [0:]
set tics out nomirror
set grid
set key top right
set style fill solid 1.
set xlabel "E (eV)"
set ylabel "N"
set boxwidth 24.
plot "-" u 1:2 title "Neutrals" smooth freq w boxes lc rgb "green", "-" u 1:2 title "Cu 3000 K" w lines lt 5 lc rgb "red"
