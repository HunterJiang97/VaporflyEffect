setwd("C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/")

set.seed(1)
indi <- sort(sample(c(1:595), 5))

par(mfrow = c(1,5))
for (ii in indi){
  f <- sample.jags[[1]][,ii]
  pl <- as.numeric(f)
  plot(pl, type = "l", xlab = attributes(sample.jags[[1]])[[2]][[2]][ii], ylab = "")
}

summ <- summary(sample.jags)
