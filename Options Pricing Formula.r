S <- 94
K <- 96
t.curr <- 1
T <- 2
u <- 0.4
d <- -0.6
r <- 0.02

black.scholes <- function(S, K, sigma, r, T, delta){
	d_1 <- (log((S*exp(-delta*T))/(K*exp(-r*T)))+0.5*sigma^2*T)/(sigma*T^0.5)
	d_2 <- (log((S*exp(-delta*T))/(K*exp(-r*T)))-0.5*sigma^2*T)/(sigma*T^0.5)
	opt.prem <- c(
		call.prem <- S*exp(-delta*T)*pnorm(d_1)-K*exp(-r*T)*pnorm(d_2),
		put.prem <- K*exp(-r*T)*pnorm(-d_2)-S*exp(-delta*T)*pnorm(-d_1)
	)
	cat("The European Call Option Premium is $",call.prem, "\n")
	cat("The European Put Option Premium is $",put.prem, "\n")
	return(opt.prem)
	}

cox.ross <- function(S, K, r, u, d, T, t.curr, print.bool=1, return.bool=0){
	m_t <- ceiling(log(K/(S*(1+d)^(T-t.curr)))/log((1+u)/(1+d)))
	q.star <- ((r-d)/(u-d))
	p.star <- ((1+u)/(1+r))*q.star
	if (m_t > T-t.curr) {m_t <- T-t.curr+1}
	call.prem <- S*(1-pbinom(m_t-1, T-t.curr, p.star)) - K*(1+r)^(-(T-t.curr))*(1-pbinom(m_t-1, T-t.curr, q.star))
	if (print.bool == 1){
		cat("The European Call Option Premium is $", call.prem, "\n")
		}
	if (return.bool == 1) {
		return(call.prem)
		}
	}
	
repeat.port <- function(S, K, r, u, d, T, t.curr, print.bool=1, return.bool=0){
	C.down <- cox.ross(S*(1+d), K, r, u, d, T, t.curr+1, 0, 1)
	C.up <- cox.ross(S*(1+u), K, r, u, d, T, t.curr+1, 0, 1)
	y_t1 <- (C.up-C.down)/(S*(u-d))
	x_t1 <- (C.up - y_t1*S*(1+u))*(1+r)^(-(t.curr+1))
	if (print.bool == 1){
		cat("(x_", t.curr+1, ",y_", t.curr+1, ")=(", x_t1, ",", y_t1, ")\n", sep="")
		}
	if (return.bool == 1) {
		return(c(x_t1, y_t1))
		}
	}

cox.ross(S, K, r, u, d, T, t.curr)
repeat.port(S, K, r, u, d, T, t.curr)

price.matrix <- matrix(nrow=200*10, ncol=2, dimname=list(c(),c("K", "Price")))
for (k in 1:length(price.matrix[,1])){
	price.matrix[k,] <- cbind(k/10, cox.ross(S, k/10, r, u, d, T, t.curr, 0, 1))
	}

price.smooth <- loess(price.matrix[,2] ~ price.matrix[,1], span=0.5, degree=2)
windows()
plot(price.matrix[,1], price.matrix[,2], type="l", ylab="Price of Call", xlab="K")
lines(price.matrix[,1], predict(price.smooth), col="red")
