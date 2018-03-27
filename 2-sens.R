# Sensitivity analysis. Run this script second.

library(rjags)
library(abind)

load("data1.RData")

v <- function(x) unlist(res[,x])

# Fit the model in a cumulative fashion
sensMC <- lapply(6:17, function(i) {
  dat <- list(
      K = 3,
      T = nrow(data1[1:i,]),
      N = c(300000, 9600000, 900000),
      I = with(data1[1:i,], cbind(roma, greek, foreign)),
      F = 52
  )
  cat(sprintf("Running for i=%s...\n", i))
  model <- jags.model(file = "model.jag", data = dat, n.chains = 4, n.adapt = 2000)
  res <- coda.samples(model, var=c("Re", "FES"), n.iter=30000, thin=1)
  res
})

# Extract the results (Re and final epidemic sizes, in a 3D array)
sensRes <- do.call(abind, c(lapply(sensMC, function(x) summary(x)[[2]][,c(1,3,5)]), along=3))

save(sensRes, file="sens1.RData")

