n <- 1000
p <- 0.5

rand.walk <- matrix(nrow=n, ncol=4, dimnames=list(c(),c("n", "X_i", "M_n", "W_n")))
rand.walk[,"n"] <- 1:n
rand.walk[,"X_i"] <- sample(c(1, -1), n, replace=TRUE, prob=c(p,1-p))
rand.walk[1,"M_n"] <- rand.walk[1,"X_i"]
for (i in 2:n) {
	rand.walk[i,"M_n"] <- rand.walk[i-1,"M_n"] + rand.walk[i,"X_i"]
	}
rand.walk[,"W_n"] <- rand.walk[,"M_n"] / sqrt(rand.walk[,"n"])

windows(title="Random Walk")
plot(rand.walk[,"n"], rand.walk[,"M_n"], type="l")

windows(title="Weiner Process")
plot(rand.walk[,"n"], rand.walk[,"W_n"], type="l")
hist(rand.walk[,"W_n"])