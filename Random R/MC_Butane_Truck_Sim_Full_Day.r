# This is a simulation that uses queuing theory to model the arrival and offload times
# of butane trucks into a terminal. A M/D/c type of model is being used, as the arrival of butane trucks
# is assumed to follow a poisson distribution with mean lambda, and the offload times are assumed to be constant
# which is defined as mu.

# Developed by Scott Graham
# June, 2016


set.seed(815609)

# Generates the riser matrix.
riserMatrixGen <- function(riserNum) {
	riserColName <- c("Riser #", "Occupied")
	riserTable <- matrix(nrow = riserNum, ncol = length(riserColName))
	colnames(riserTable) <- riserColName
	
	# Populates the riser #'s.
	riserTable[,"Riser #"] <- 1:riserNum
	# 0 is unoccupied, 1 is occupied.
	riserTable[,"Occupied"] <- 0
	return(riserTable)
}


# Builds the timeTable matrix housing the data for this run.
truckMatrixGen <- function(actlTruckNum, actlTruckNumOffPeak, vTruck) {
	ttColName <- c("Truck #", "Interarrival Time", "Cumulative Arrival Time" ,"Offload Start Time", "Exit Time", "Time in Queue", "Time in System", "Volume", "Status", "Riser #")
	
	# The times in timeTable are all in minutes. timeTable is a matrix w/ rows equal to the number of trucks,
	# and columns equal to the length of ttColName.
	timeTable <- matrix(nrow = actlTruckNum + actlTruckNumOffPeak, ncol = length(ttColName))
	colnames(timeTable) <- ttColName

	# Populates the "Truck #" column.
	timeTable[,"Truck #"] <- 1:(actlTruckNum + actlTruckNumOffPeak)
	timeTable[,"Volume"] <- vTruck
	# Initializing the timers.
	timeTable[,"Time in Queue"] <- 0
	timeTable[,"Time in System"] <- 0
	# For the status column, 0 means the truck hasn't yet arrived, 1 is in line, 2 is at the pump, 3 means it has left the system.
	timeTable[,"Status"] <- 0
	return(timeTable)
}


# Generates the interarrival time between trucks and saves it to timeTable.
arrivalTimeGen <- function(timeTable, lambda, lambdaOffPeak, actlTruckNum, actlTruckNumOffPeak, numMin) {
	# The time between succesive arrivals in minutes follows an exponential distribution w/ mean of 1/lambda.
	arrTime <- rexp(actlTruckNum, lambda)
	arrTime <- append(arrTime,rexp(actlTruckNumOffPeak, lambdaOffPeak))
	
	# Using the rounded value to the nearest integer simplifies some of the code.
	timeTable[,"Interarrival Time"] <- round(arrTime,0)
	# This loop populates the cumulative arrival time column by adding the previous cumulative arrival time and the interarrival time for that truck.
	timeTable[1, "Cumulative Arrival Time"] <- timeTable[1,"Interarrival Time"]
	for (i in 2:(actlTruckNum + actlTruckNumOffPeak)) {
		timeTable[i, "Cumulative Arrival Time"] <- timeTable[i-1, "Cumulative Arrival Time"] + timeTable[i,"Interarrival Time"]
	}
	return(timeTable)
}


# This function loops through each minute in the day, and checks to see what events occurs and updates the volumes as needed.
timeCounter <- function(timeTable, riserTable, mu1, mu2, vBullet.Init, vBullet.Max, tBullet.Empty, tBullet.Full, truckNum, riserNum, dayLength, rBullet) {
	# Current bullet is equal to the initial volume.
	vBullet.Current <- vBullet.Init

	# This loop iterates through the day minute by minute, and does the requisite inflows and outflows of butane.
	for (currentMin in 1:dayLength) {
		# Takes out the volume from the bullet that we're injecting.
		vBullet.Current <- vBullet.Current - rBullet
		# Checks to see if the bullet is empty, and prevents it from going into negative values.
		if (vBullet.Current <= 0) {
			vBullet.Current <- 0
			#cat("EMPTY", "\n")
		}
		timeTable <- truckArrCheck (timeTable, currentMin, truckNum)
		# Cycles through each truck and checks to see if it can be moved to a riser.
		for (i in 1:truckNum) {
			# Checks to see if we can move a truck from the queue line to the riser.
			if (timeTable[i,"Status"] == 1) {
				tempVar <- NULL
				tempVar <- riserOccCheck (timeTable, riserTable, truckNum, riserNum, i, currentMin)
				timeTable <- tempVar[[1]]
				riserTable <- tempVar[[2]]
			}
			# If a truck is at a riser, it offloads it.
			if (timeTable[i,"Status"] == 2) {
				tempVar <- NULL
				# If only 1 truck is offloading, or a truck just arrived, we use mu1 as our offload rate.
				if (sum(riserTable[,"Occupied"]) <= 1) {
					tempVar <- truckOffload (timeTable, mu1, vBullet.Current, vBullet.Max, truckNum, rBullet, i)
				}
				# When two trucks are offloading simulataneously, we use mu2 as our offload rate.
				if (sum(riserTable[,"Occupied"]) == 2) {
					tempVar <- truckOffload (timeTable, mu2, vBullet.Current, vBullet.Max, truckNum, rBullet, i)
				}
				timeTable <- tempVar[[1]]
				vBullet.Current <- tempVar[[2]]
				# Checks to see if a truck is empty. If so we find what riser that truck was at, and set the occupancy status on the riserTable back to 0.
				if (timeTable[i,"Volume"] ==0) {
					riserTable[timeTable[i,"Riser #"],"Occupied"] <- 0
					timeTable[i,"Status"] <- 3
					timeTable[i, "Exit Time"] <- currentMin
				}
			}
		}
		# Checks to see if the bullet is empty or full, and if so adds one to the respective counter.
		if (vBullet.Current == 0) {
			tBullet.Empty <- tBullet.Empty + 1
		}
		if (vBullet.Current == vBullet.Max) {
			tBullet.Full <- tBullet.Full + 1
		}
	}

	return(list(timeTable, riserTable, vBullet.Current, tBullet.Empty, tBullet.Full))
}


# This function checks to see if a truck has arrived.
truckArrCheck <- function(timeTable, currentMin, truckNum) {
	# Cycles through all trucks and checks if the cumulative arrival time matches the current time.
	for (i in 1:truckNum) {
		# When true, we increase the status of the truck from 0 to 1
		if (timeTable[i,"Cumulative Arrival Time"] == currentMin) {timeTable[i,"Status"] <- 1}
	}
	return(timeTable)
}


# Checks if the riser is occupied.
riserOccCheck <- function(timeTable, riserTable, truckNum, riserNum, i, currentMin) {
	# First we cycle through the risers performing some checks and updates.
	for(j in 1:riserNum) {
		# We look at whatever the current truck is as defined in timeCounter, and check to see if it is in line.
		if(timeTable[i,"Status"] == 1) {
			# Then we check if there is an unoccupied riser.
			if(riserTable[j,"Occupied"] == 0) {
				# If so we update the truck status and record when the offload started.
				timeTable[i,"Status"] <- 2
				timeTable[i, "Offload Start Time"] <- currentMin
				timeTable[i,"Riser #"] <- j
				riserTable[j,"Occupied"] <- 1
			}
		}
	}
	return(list(timeTable, riserTable))
}


# Offloads the volume from the truck into the tank.
truckOffload <- function(timeTable, mu, vBullet.Current, vBullet.Max, truckNum, rBullet, i) {
	# We take out butane from the truck at rate mu, and put it into the bullet.
	# 1. This if statement checks if the bullet can accept a minutes worth of butane, As well if the truck can pump a minutes worth of butane.
	if (vBullet.Current <= vBullet.Max - mu & timeTable[i,"Volume"] >= mu) {
		timeTable[i,"Volume"] <- timeTable[i,"Volume"] - mu
		vBullet.Current <- vBullet.Current + mu
		#cat("1")
	}
	# 2. Checks if the tank is nearly full (i.e. within a minutes pumping to it).
	if (vBullet.Max - mu < vBullet.Current & vBullet.Current < vBullet.Max & timeTable[i,"Volume"] >= mu) {
		timeTable[i,"Volume"] <- timeTable[i,"Volume"] - (vBullet.Max - vBullet.Current)
		vBullet.Current <- vBullet.Max
		#cat("2")
	}
	# 3. Checks if tank can accept butane, but truck is nearly empty.
	if (vBullet.Current <= vBullet.Max - mu & timeTable[i,"Volume"] < mu) {
		vBullet.Current <- vBullet.Current + timeTable[i,"Volume"]
		timeTable[i,"Volume"] <- 0
		#cat("3")
	}
	# 4. Bullet is near full and truck is near empty.
	if (vBullet.Max - mu < vBullet.Current & vBullet.Current < vBullet.Max & timeTable[i,"Volume"] < mu) {
		#We need to check if the bullet has more or less space than what's left in the truck.
		if (vBullet.Max - vBullet.Current > timeTable[i,"Volume"]) {
			vBullet.Current <- vBullet.Current + timeTable[i,"Volume"]
			timeTable[i,"Volume"] <- 0
		}
		else {
			timeTable[i,"Volume"] <- timeTable[i,"Volume"] - (vBullet.Max - vBullet.Current)
			vBullet.Current <- vBullet.Max
		}
		#cat("4")
	}
	# 5. Bullet and truck are both nearly empty.
	if (vBullet.Current < mu & timeTable[i,"Volume"] < mu) {
		vBullet.Current <- vBullet.Current + timeTable[i,"Volume"]
		timeTable[i,"Volume"] <- 0
		#cat("5")
	}
	
	return(list(timeTable, vBullet.Current))
}


#This functions contains the actual simulation, that is repeated 'n' times	
simulation <- function(numMin, numMinOffPeak, dayLength, avgTruckNum, avgTruckNumOffPeak, truckShort, vBullet.Max, vBullet.Init, vBullet.Current, vTruck, tBullet.Empty, tBullet.Full, lambda, lambdaOffPeak, mu1, mu2, rBullet, riserNum) {

	# The actual number of trucks that arrive over the period set by numHrs follows a poisson distribution w/ mean = avgTruckNum.
	# Maybe put a hard cap on this...
	actlTruckNum <- rpois(1,avgTruckNum) + truckShort
	actlTruckNumOffPeak <- rpois(1,avgTruckNumOffPeak)
	# Checks to see if the combined number of trucks exceeds 16 per day.
	if (actlTruckNum + actlTruckNumOffPeak > avgTruckNum + avgTruckNumOffPeak) {
		actlTruckNumOffPeak <- (avgTruckNum + avgTruckNumOffPeak) - actlTruckNum
	}
	# If the peak number of trucks exceeds what'd we'd expect to see normally, the off peak number will be set to a negative integer, obviously we can't have negative trucks, so it is set to 0.
	if (actlTruckNumOffPeak < 0) {
		actlTruckNum <- avgTruckNum + avgTruckNumOffPeak
		actlTruckNumOffPeak <- 0
	}
	
	riserTable <- riserMatrixGen (riserNum)
	timeTable <- truckMatrixGen (actlTruckNum, actlTruckNumOffPeak, vTruck)
	timeTable <- arrivalTimeGen (timeTable, lambda, lambdaOffPeak, actlTruckNum, actlTruckNumOffPeak, numMin)
	actlTruckNum <- actlTruckNum + actlTruckNumOffPeak
	
	# R handles returning multiple values strangely, so what I did was return a list into some temporary variable, and then splice it out as needed.
	tempVar <- NULL
	# This handles the actual stepping through each minute in the day, and then returns the timeTable, riserTable and day ending bullet volume.
	tempVar <- timeCounter (timeTable, riserTable, mu1, mu2, vBullet.Init, vBullet.Max, tBullet.Empty, tBullet.Full, actlTruckNum, riserNum, dayLength, rBullet)
	timeTable <- tempVar[[1]]
	riserTable <- tempVar[[2]]
	vBullet.Current <- tempVar[[3]]
	tBullet.Empty <- tempVar[[4]]
	tBullet.Full <- tempVar[[5]]
	
	# This populates the time in columns by taking either the offload or exit times and subtracting out when they arrived.
	timeTable[,"Time in Queue"] <- timeTable[,"Offload Start Time"] - timeTable[,"Cumulative Arrival Time"]
	timeTable[,"Time in System"] <- timeTable[,"Exit Time"] - timeTable[,"Cumulative Arrival Time"]
	return(list(timeTable, riserTable, vBullet.Init, vBullet.Current, actlTruckNum, tBullet.Empty, tBullet.Full))
}


# This function exists so I can test some functionality regarding the actual simulation.
simulationTest <- function() {
	set.seed(815609)

	# Number of trucks that arrive in the number of hours defined below.
	# the peak time is 9am-4:59pm, and off peak time is 5pm-8:59am
	avgTruckNumPeak <- 12
	avgTruckNumOffPeak <- 4
	numHrs <- 8
	numHrsOffPeak <- 16
	numMin <- numHrs * 60
	numMinOffPeak <- numHrsOffPeak * 60
	
	# The dayLength is seperate from numHrs, as this is the period of time the simulation is run through, where as numHrs is the expected period of time that the rate trucks come in is constant.
	# So our dayLength for the sim may be 18hrs, but we can expect to recieve 12 trucks over the first 8 hrs. Because we are dealing with random variables, the trucks can sometimes take much longer to unload,
	# so the model needs to account for that.
	dayLength <- 24 * 60
	
	# Volume in m3.
	vBullet.Max <- 350
	# vBullet.Init is the starting volume. I used 50%.
	vBullet.Init <- vBullet.Max * 0.50
	vBullet.Current <- vBullet.Init
	vTruck <- 35
	
	# Arrival rate in trucks/min. Calculated by taking our average number of trucks that arrive over a given period, divided by the length of said period.
	lambda <- avgTruckNumPeak / numMin
	lambdaOffPeak <- avgTruckNumOffPeak / numMinOffPeak
	
	# Truck offload rate in m3/min.
	# mu = (#trucks offloaded/time to offload)*(m3 per truck/number of risers used)
	# mu1 is the time it takes to offload when only one truck is being offloaded.
	mu1 <- (1/45)*(vTruck/1)
	# mu2 is the time it takes to offload two trucks consecutively.
	mu2 <- (2/75)*(vTruck/2)
	# How fast the outflow is on the bullet in m3/min.
	rBullet <- 22.177/60
	
	# Number of risers.
	riserNum <- 2
	
	#tBullet.* tracks how long per trial the bullet is empty or full.
	tBullet.Empty <- 0
	tBullet.Full <- 0
	
	# The actual number of trucks that arrive over the period set by numHrs follows a poisson distribution w/ mean = avgTruckNum.
	# Maybe put a hard cap on this...
	actlTruckNumPeak <- rpois(1,avgTruckNumPeak)
	actlTruckNumOffPeak <- rpois(1,avgTruckNumOffPeak)
	
	# Checks to see if the combined number of trucks exceeds 16 per day.
	if (actlTruckNumPeak + actlTruckNumOffPeak > 16) {
		actlTruckNumOffPeak <- 16 - actlTruckNumPeak
	}
	# If the peak number of trucks exceeds 16, the off peak number will be set to a negative integer, obviously we can't have negative trucks, so it is set to 0.
	if (actlTruckNumOffPeak < 0) {
		actlTruckNumPeak <- 16
		actlTruckNumOffPeak <- 0
	}
	
	riserTable <- riserMatrixGen (riserNum)
	timeTable <- truckMatrixGen (actlTruckNumPeak, actlTruckNumOffPeak, vTruck)
	timeTable <- arrivalTimeGen (timeTable, lambda, lambdaOffPeak, actlTruckNumPeak, actlTruckNumOffPeak, numMin)
	actlTruckNum <- actlTruckNumPeak + actlTruckNumOffPeak
	
	# R handles returning multiple values strangely, so what I did was return a list into some temporary variable, and then splice it out as needed.
	tempVar <- NULL
	tempVar <- timeCounter (timeTable, riserTable, mu1, mu2, vBullet.Init, vBullet.Max, tBullet.Empty, tBullet.Full, actlTruckNum, riserNum, dayLength, rBullet)
	timeTable <- tempVar[[1]]
	riserTable <- tempVar[[2]]
	vBullet.Current <- tempVar[[3]]
	tBullet.Empty <- tempVar[[4]]
	tBullet.Full <- tempVar[[5]]
	# This populates the time in columns by taking either the offload or exit times and subtracting out when they arrived.
	timeTable[,"Time in Queue"] <- timeTable[,"Offload Start Time"] - timeTable[,"Cumulative Arrival Time"]
	timeTable[,"Time in System"] <- timeTable[,"Exit Time"] - timeTable[,"Cumulative Arrival Time"]
	return(list(timeTable, riserTable, vBullet.Init, vBullet.Current, actlTruckNum, actlTruckNumPeak, actlTruckNumOffPeak, tBullet.Empty, tBullet.Full))
}


# n is the number of trials for the simulation.
main <- function(n, riserNum) {
	
	# Number of trucks that arrive in the number of hours defined below.
	# the peak time is 9am-4:59pm, and off peak time is 5pm-8:59am the following day.
	avgTruckNum <- 12
	avgTruckNumOffPeak <- 4
	truckShort <- 0
	numHrs <- 8
	numHrsOffPeak <- 16
	numMin <- numHrs * 60
	numMinOffPeak <- numHrsOffPeak * 60
	
	# The dayLength is separate from numHrs, as this is the period of time the simulation is run through, where as numHrs is the expected period of time that the rate trucks come in is constant.
	# So our dayLength for the sim may be 18hrs, but we can expect to recieve 12 trucks over the first 8 hrs. Because we are dealing with random variables, the trucks can sometimes take much longer to unload,
	# so the model needs to account for that.
	dayLength <- 24 * 60
	
	# Volume in m3.
	vBullet.Max <- 350
	# vBullet.Init is the starting volume. I used 50%.
	vBullet.Init <- vBullet.Max * 0.50
	vBullet.Current <- vBullet.Init
	vTruck <- 35
	
	# Arrival rate in trucks/min. Calculated by taking our average number of trucks that arrive over a given period, divided by the length of said period.
	lambda <- avgTruckNum / numMin
	lambdaOffPeak <- avgTruckNumOffPeak / numMinOffPeak
	
	# Truck offload rate in m3/min.
	# mu = (#trucks offloaded/time to offload)*(m3 per truck/number of risers used)
	# mu1 is the time it takes to offload when only one truck is being offloaded.
	mu1 <- (1/45)*(vTruck/1)
	# mu2 is the time it takes to offload two trucks consecutively.
	mu2 <- (2/75)*(vTruck/2)
	# How fast the outflow is on the bullet in m3/min.
	rBullet <- 22.177/60
	
	# Our table that summarizes the simulation. 
	summaryTableCol <- c("Day", "Number of Trucks Recieved", "Time in Queue", "Time in System", "Day Starting Tank Volume", "Day Ending Tank Volume", "Time Bullet Spent Empty", "Time Bullet Spent Full")
	summaryTable <- matrix(nrow = n, ncol = length(summaryTableCol))
	colnames(summaryTable) <- summaryTableCol
	
	# Runs the simulation 'n' times.
	for (i in 1:n) {
	
		# tBullet.* tracks how long per trial the bullet is empty or full.
		tBullet.Empty <- 0
		tBullet.Full <- 0	
		tempVar <- NULL
		tempVar <- simulation (numMin, numMinOffPeak, dayLength, avgTruckNum, avgTruckNumOffPeak, truckShort, vBullet.Max, vBullet.Init, vBullet.Current, vTruck, tBullet.Empty, tBullet.Full, lambda, lambdaOffPeak, mu1, mu2, rBullet, riserNum)
		timeTable <- tempVar[[1]]
		riserTable <- tempVar[[2]]
		vBullet.Init <- tempVar[[3]]
		vBullet.Current <- tempVar[[4]]
		actlTruckNum <- tempVar[[5]]
		tBullet.Empty <- tempVar[[6]]
		tBullet.Full <- tempVar[[7]]

		summaryTable[i,"Day"] <- i
		summaryTable[i,"Number of Trucks Recieved"] <- actlTruckNum
		# Trucks that don't finish offloading or haven't arrived yet output as NA in the timeTable, so when we average the time spent in the queue and system, the mean will result in NA if there's an NA.
		# By using na.rm = TRUE, we can ignore these when calculating the averages.
		summaryTable[i,"Time in Queue"] <- sum(timeTable[,"Time in Queue"], na.rm = TRUE)
		summaryTable[i,"Time in System"] <- sum(timeTable[,"Time in System"], na.rm = TRUE)
		summaryTable[i,"Day Starting Tank Volume"] <- vBullet.Init
		summaryTable[i,"Day Ending Tank Volume"] <- vBullet.Current
		summaryTable[i,"Time Bullet Spent Empty"] <- tBullet.Empty
		summaryTable[i,"Time Bullet Spent Full"] <- tBullet.Full

		# This sets the starting volume for the bullet to whatever it finished at the previous "day".
		vBullet.Init <- vBullet.Current
		
		# truckShort is the number of trucks we'd expect to see less what we actually recieved.
		if (actlTruckNum < avgTruckNum + avgTruckNumOffPeak) {
			truckShort <- (avgTruckNum + avgTruckNumOffPeak) - actlTruckNum
			avgTruckNum <- avgTruckNum + truckShort
		}
		else {
			truckShort <- 0
			avgTruckNum <- 12
		}
	}
	
	# This writes the results to a csv file that currently lives in my scheduling folder.
	try(write.csv(summaryTable, paste("C:/Users/scott_000/Desktop/TruckSimOutput", riserNum, ".csv")))
	return(summaryTable)
}


statAnalysis <- function(oneRiser, twoRiser, alpha, columnName) {
	# There is a bit of a statistical quirk, you should enter in the level you wish to reject H0 at, but the tests require 1 - alpha.
	# First we perform an F-test w/ a null hypothesis that the variances are equal. We reject H0 at some level alpha as defined in the function.
	fTest <- var.test(oneRiser[,columnName], twoRiser[,columnName], conf.level = 1 - alpha)
	# If the p-value in the F-test is <= alpha, we reject H0 at some level alpha and perform a Welch's t-test w/ unequal variances.
	if (fTest$"p.value" <= alpha) {
		varBool <- FALSE
	}
	# If our p-value > alpha, we fail to reject H0 at some level alpha and do a 2 sample Student's t-test on the means
	else {
		varBool <- TRUE
	}
	# varBool is a boolean variable that when true implies we have equal variance at some level alpha, otherwise are variances are not equal, and we should account for that in our model.
	tTest <- t.test(oneRiser[,columnName], twoRiser[,columnName], conf.level = 1-alpha, var.equal = varBool)
	
	# ni is the number of trials in each of the simulations we ran.
	n1 <- dim(oneRiser)[1]
	n2 <- dim(twoRiser)[1]
	# pntEst is the point estimator for the difference between the sample means.
	pntEst <- mean(oneRiser[,columnName]) - mean(twoRiser[,columnName])
	# MoE is our margin of error based on the samples.
	# If we deem the variances to be equal, we use the Student's version foor our CI.
	if (varBool) {
		sSquared <- ((n1 - 1) * var(oneRiser[,columnName]) + (n2 - 1) * var(twoRiser[,columnName])) / (n1 + n2 - 2)
		MoE <- sqrt(sSquared) * sqrt((1 / n1) + (1 / n2))
	}
	# If we deem are variances to be unequal, we use Welch's version for our CI.
	else {
		sSquared <- (var(oneRiser[,columnName]) / n1) + (var(twoRiser[,columnName]) / n2)
		MoE <- sqrt(sSquared)
	}
	tStat <- qt (alpha / 2, tTest$"parameter", lower.tail = FALSE)
	LB <- pntEst - tStat * MoE
	UB <- pntEst + tStat * MoE
	
	print(fTest)
	print(tTest)
	cat("The ", (1 - alpha) * 100, "% CI for the difference between the ", columnName, " sample means are:", "\n", "[", LB, ",", UB, "]", "\n", sep = "" )
}


graphAnalysis <- function(summaryTable) {
	windows()
	par(mfrow=c(3,2))
	hist(summaryTable[,"Number.of.Trucks.Recieved"])
	hist(summaryTable[,"Time.in.Queue"])
	hist(summaryTable[,"Time.in.System"])
	hist(summaryTable[,"Day.Ending.Tank.Volume"])
	hist(summaryTable$"Time.Bullet.Spent.Empty")
	hist(summaryTable$"Time.Bullet.Spent.Full")

	windows()
	par(mfrow=c(1,2))
	plot(summaryTable[,"Number.of.Trucks.Recieved"], summaryTable[,"Avg.Time.in.Queue"])
	plot(summaryTable[,"Number.of.Trucks.Recieved"], summaryTable[,"Avg.Time.in.System"])
	
	regr.Volume <- lm(summaryTable[,"Day.Ending.Tank.Volume"] ~ summaryTable[,"Number.of.Trucks.Recieved"])
	regr.Queue.Time <- lm(summaryTable[,"Time.in.Queue"] ~ summaryTable[,"Number.of.Trucks.Recieved"])
	regr.System.Time <- lm(summaryTable[,"Time.in.System"] ~ summaryTable[,"Number.of.Trucks.Recieved"])
	print(summary(regr.Volume))
	print(summary(regr.Queue.Time))
	print(summary(regr.System.Time))
	windows()
	qqnorm(regr.Volume$residuals)
	qqline(regr.Volume$residuals)
	windows()
	plot(regr.Queue.Time$fitted, regr.Queue.Time$residuals)
}


# Here we generate two data sets of size 100, the former using a single riser, and the latter using two risers.
oneRiser <- data.frame(main(100,1))
twoRiser <- data.frame(main(100,2))
