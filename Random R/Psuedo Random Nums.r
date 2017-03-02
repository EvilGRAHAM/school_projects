x <- c()
u <- c()
m <- 2^31
a <- 969757
b <- 3260041
x[1] <- 272
u[1] <- x[1] / m
for (j in 1:1000) {
	x[j+1] <- (a*x[j] + b) %% m
	u[j+1] <- x[j+1] / m
	}
hist(u, freq=FALSE)