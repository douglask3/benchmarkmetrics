library(gitBasedProjects)
sourceAllLibs('benchmarkMetrics/R/')

mat = 'benchmarkMetrics/data/affinityMat.csv'

obs = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4)
sim = c(1, 2, 3, 3, 4, 2, 1, 2, 3, 2, 1, 1, 2, 3, 3, 5, 2, 4, 5)

null = null.DMM(mat, obs)
plot(null, legend = TRUE)

out = DMM(mat, obs, sim)
print(summary(out))
boxplot(out)
plot(out)