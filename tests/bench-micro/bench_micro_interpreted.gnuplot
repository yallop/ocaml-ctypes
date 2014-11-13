set terminal eps
set output 'bench_micro_interpreted.eps'

set xlabel "Arity"

set autoscale
set yrange [0:]

set ylabel "Time (ns)"

# set title "Mean FFI Call Latency by Arity"

set style data linespoints

plot "interpreted_shared.txt" using 1:2 title "Interpreted", \
     "traditional.txt"        using 1:2 title "Official"
