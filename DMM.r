library(gitBasedProjects)
sourceAllLibs('benchmarkMetrics/R/')
graphics.off()

	###################
	## Set up inputs ##
	###################
	## biomeAffinityMatrix is an example dataset provided as part of the package
	print(biomeAffinityMatrix)

	## bit of a random example of items numbers
	obs = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4)
	sim = c(1, 2, 3, 3, 4, 2, 1, 2, 3, 2, 1, 1, 2, 3, 3, 5, 2, 4, 5)

	####################
	## run and output ##
	####################
	out = DMM(biomeAffinityMatrix, obs, sim)
	
	print(out)
	cat("\n=======\n")
	print(score(out))
	cat("\n=======\n")
	print(summary(out))
	cat("\n=======\n\n")	
	
	## can feed in biome names instead
	obs = biomeAffinityMatrix[obs,1]
	sim = biomeAffinityMatrix[sim,1]	
	print(rbind(obs = obs, sim = sim))
	
	## to get the same result
	out = DMM(biomeAffinityMatrix, obs, sim)
	cat("\n")
	print(out)
	
	##################
	## plot results ##
	##################
	par(mfrow = c(2,1), mar = c(2, 4, 0, 1))
	boxplot(out)
	plot(out)
	
	#########################
	## Run the null models ##
	#########################
	null = null.DMM(biomeAffinityMatrix, obs, n = 100)
	cat("\n\n Null Model: \n")
	print(summary(null))
	dev.new()
	plot(null, legend = TRUE)