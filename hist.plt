set title "Säteily (HT40, 120 s)"
set xrange [30:]
set yrange [0:]
set tics out nomirror
set grid
set key top right
set style fill solid 1.0
set xlabel "E"
set ylabel "Ln N"
plot "-" u 1:2 title "Cs-137 (41.0k)" smooth freq w boxes lc rgb "green", "-" u 1:2 title "Tausta (30.0k)" smooth freq w boxes lc rgb "red"
