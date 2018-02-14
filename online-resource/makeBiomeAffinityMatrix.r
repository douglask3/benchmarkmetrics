biomeAffinityMatrix = read.csv('online-resource/biomeAffinityMatrix.csv', stringsAsFactors = FALSE)
 
save(biomeAffinityMatrix, file = 'benchmarkMetrics/data/biomeAffinityMatrix.RData')