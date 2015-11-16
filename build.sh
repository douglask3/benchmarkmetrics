#!/usr/bin/env sh
R CMD build benchmarkMetrics
R CMD check benchmarkMetrics
