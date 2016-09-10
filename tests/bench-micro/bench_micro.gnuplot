set terminal eps
set output 'bench_micro.eps'

set key left top
set key font ",15"

set xlabel "Arity"

set autoscale
set yrange [0:]

set ylabel "Time (ns)"

#set title "Mean FFI Call Latency by Arity"

set style data linespoints

set style line 1 lc rgb '#8b1a0e' pt 1 ps 1 lt 1 lw 2
set style line 2 lc rgb '#5e9c36' pt 6 ps 1 lt 1 lw 2
set style line 3 lc rgb '#5060D0' pt 2 ps 1 lt 1 lw 2

set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror

set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12

plot "staged_functor.txt" using 1:2 title "Cmeleon Staged" w lp ls 1, \
     "cowboy.txt"         using 1:2 title "OCaml Expert" w lp ls 2, \
     "traditional.txt"    using 1:2 title "OCaml Manual" w lp ls 3
