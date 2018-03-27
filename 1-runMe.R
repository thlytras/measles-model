# Run this script first, to fit the model

library(rjags)

# Load the data
load("data1.RData")

# Set up the data for JAGS
dat <- list(
    K = 3,
    T = nrow(data1),
    N = c(300000, 9600000, 900000),
    I = with(data1, cbind(roma, greek, foreign)),
    F = 52
)

model <- jags.model(file = "model.jag", data = dat, n.chains = 4, n.adapt = 2000)

res <- coda.samples(model, var=c("R0", "Re", "FES", "su", "If", "IES", "a", "rsd", "VC"), n.iter=30000, thin=1)

cat("Summarizing...\n")
a <- summary(res)[[2]]

v <- function(x) unlist(res[,x])

cat("Saving...\n")
save(data1, res, a, v, file="res.RData")


