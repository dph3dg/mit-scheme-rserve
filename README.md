# mit-scheme-rserve
Simple Rserve client using mit-scheme


This is a minimal, in that it works for me connection between RSERVE and MIT-Scheme
It works for my small amount of data sent (less than 100,000 data points)
The return format from RSERVE was too difficult to deconstruct for anything more complicated than float arrays
therefore if you need anymore you need to get R to write to a file directly or write the parser yourself.
The graphs write directly to a single PDF file that is overwritten each time and is displayed assuming
you use OSX. There must be an equivalent in BSD, Linux etc.

Examples given are:

1.fitting a nonlinear function

2.plotting a list, with points, or joined, optionally error bars, x label, y label, title using ggplot2

3.showing a fitted function, optionally with error bars. You need to construct the fitted

4.function yourself. ie raw data plus calculated y values from fn

5.1.if you have Scmutils you can also plot linear fits from scheme (you could use the R equivalent)

5.2 density array plot using m:generate from scmutils
