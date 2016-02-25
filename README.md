# Benchmark Metrics

This is an R package containing sepcifically design metrics to assess and benchmark Land Surface and Vegetation Models against observations, but can also be applied to any simulation vs observation comparison.

## Metrics
That are 6 metrics used for differenttypes of comparison
* Normalised Mean Error and Normalised Mean Squred Error (NME/NMSE) for most comparisons (inc gridded, temporal and site based comparisons)
* Manhattan Metric and Square Chord Distance (MM/SCD) for "item" comparisons (i.e, fractional cover of different land surfaces)
* Mean Phase Difference (MPD) for comparisons of osillaiting processes (i.e, seasonal or diurnal variations

## Interpretation
Scores of metrics are easy to interpret for model everulation and inter-model comparisons. Null models help aid score evalutation futher.

## How TO Use
To insatll the package, first make sure you have R/Rstudio running and you have installed and loaded devtools:

    install.packages('devtools')
    library(devtools)

Then run the following command to install and load Benchmark Metrics:

    install_github('douglask3/benchmarkmetrics/benchmarkMetrics')
    library(benchmarkMetrics)

To see help files and examples of how to use:

    ?benchmarkMetrics

Info on the main functions can be found using:

    ?NME
    ?MM
    ?MPD

## More Info
[click here](http://douglask3.github.io/docs/benchmarkMetrics-manual.pdf) for documentation.

