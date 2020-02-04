set title "Säteily"
set xrange [0:]
set yrange [0:]
set tics out nomirror
set grid
set style fill solid 1.0
set xlabel "E"
set ylabel "Log N"
plot "-" u 1:2 title "Cs-137" smooth freq w boxes lc rgb "green", "-" u 1:2 title "Tausta" smooth freq w boxes lc rgb "red"
