set datafile separator ','
set xdata time
set timefmt "%d/%m/%Y"
plot 'data.csv' using 1:2 with lines