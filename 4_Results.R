rm(list = ls())
sample1 <- readRDS("samples.RDS")
sample2 <- readRDS("samples1.RDS")
sample3 <- readRDS("samples2.RDS")
sample4 <- readRDS("samples3.RDS")

s1 <- summary(sample1)
beta <- as.numeric(sample1[[1]][,592])
s2.beta <- summary(sample2)$statistics[1,1]
s3.beta <- summary(sample3)$statistics[1,1]
s4.beta <- summary(sample4)$statistics[1,1]

d <- density(beta)
plot(d, main="")
polygon(d, col="gray", border="black")
abline(v = s2.beta, type = "l", lty = 2, lwd = 2, col = "red")
abline(v = s3.beta, type = "l", lty = 3, lwd = 2, col = "blue")
abline(v = s4.beta, type = "l", lty = 4, lwd = 2, col = "green")

legend("topright", 
       legend = c("Without Runner", "Without Course", "Without Gender"), 
       col = c("red", "blue", "green"), 
       lty = c(2, 3, 4),
       lwd = c(2, 2, 2),
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.01))

library(splines2)
x <- c(19:47)
spline.beta <- summary(sample1)$quantiles[593:595,c(2,3,4)]
spline.x <- as.data.frame(bSpline(c(19:47), df = NULL, knots = NULL, degree = 3, intercept = FALSE))
relation <- as.matrix(spline.x) %*% as.matrix(spline.beta)
plot(c(20:47), relation[-1,2], lty = 1, lwd = 2, type = "l",
     xlim = c(20, 45), ylim = c(-2.5,7), xlab = "Age", ylab = "Effects")
lines(c(20:47), relation[-1,1], lty = 2, lwd = 2, type = "l")
lines(c(20:47), relation[-1,3], lty = 3, lwd = 2, type = "l")
legend("topleft", 
       legend = c("75% Upper Bound", "50% Estiamte", "25% Lower Bound"), 
       col = c("Black", "Black", "Black"), 
       lty = c(3, 1, 2),
       lwd = c(2, 2, 2),
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.01))

library(dgof)
ks.test(as.numeric(sample1[[1]][,592]), as.numeric(sample2[[1]][,1]))
ks.test(as.numeric(sample1[[1]][,592]), as.numeric(sample3[[1]][,1]))
ks.test(as.numeric(sample1[[1]][,592]), as.numeric(sample4[[1]][,1]))

estimate <- summary(sample1)$statistics[592,c(1,2)]
estimate[1] + c(-1, 1) * 1.96 * estimate[2]
