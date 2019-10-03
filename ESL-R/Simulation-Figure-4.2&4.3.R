# Figure 4.2 Simulation
# 3 multivariate guassian distribution (X_1, X_2)
mu_s <- c(0.25, 0.5, 0.75)
sigma_s <- 0.005*matrix(c(1,0,0,1),2,2)

library(MASS)
set.seed(872)
N <- 100
C1 <- mvrnorm(n = N, c(mu_s[1],mu_s[1]),Sigma = sigma_s)
C2 <- mvrnorm(n = N, c(mu_s[2],mu_s[2]),Sigma = sigma_s)
C3 <- mvrnorm(n = N, c(mu_s[3],mu_s[3]),Sigma = sigma_s)
# assign classes
# C1 <- cbind(C1, rep(1,N))
# C2 <- cbind(C2, rep(2,N))
# C3 <- cbind(C3, rep(3,N))

png("fig-4-2.png")
plot(C1[,1],C1[,2], pch='1',col = 'orange',
     xlim = c(min(C1[,1]), max(C3[,1])), ylim=c(min(C1[,2]),max(C3[,2])))
points(C2[,1],C2[,2], pch='2', col = 'lightblue')
points(C3[,1],C3[,2], pch='3', col = 'darkgreen')
dev.off()

# Figure 4.3
## project X onto the line joining the three centroids
X <- rbind(C1,C2,C3)
# project the data onto the line joining the three centroids
# then the length = sqrt(2)/2 * (X_1 + X_2)
X.proj <- sqrt(2)*rowMeans(X) # if necessary, multiply sqrt 2
y1 <- c(rep(1,N),rep(0,2*N))
y2 <- c(rep(0,N),rep(1,N),rep(0,N))
y3 <- c(rep(0,2*N),rep(1,N))

ls1 <- lm(y1~X.proj)
ls2 <- lm(y2~X.proj)
ls3 <- lm(y3~X.proj)
# order the corresponding prediction values y_hat
pred1 <- as.numeric(fitted(ls1)[order(X.proj)])
pred2 <- as.numeric(fitted(ls2)[order(X.proj)])
pred3 <- as.numeric(fitted(ls3)[order(X.proj)])
# get the first estimated class : 1 ~ c1
c1 <- which(pred1 <= pred2)[1]
# get the second estimated class: c1+1 ~ c2
c2 <- min(which(pred3 > pred2)) 
# get the third estimated class : c2 ~ c3
# but, you will find that c1 = c2 !!!
error1 <- (abs(c2 - 2*N) + abs(c1 - N))/(3*N)

## reproduce figure 4.3 left
# png()
# jpeg()
# bmp()
png("fig-4-3-left.png")
plot(0, 0, type = "n", 
     xlim = c(0, 1), ylim = c(0,1), xlab = "X.proj", ylab = "Y",
     main = paste0("Degree = 1; Error = ", round(error1, digits = 4)))
abline(coef(ls1), col = "orange")
abline(coef(ls2), col = "lightblue")
abline(coef(ls3), col = "darkgreen")
points(X.proj, fitted(ls1), pch="1", col="orange")
points(X.proj, fitted(ls2), pch = "2", col = "lightblue")
points(X.proj, fitted(ls3), pch = "3", col = "darkgreen")
rug(X.proj[1:N], col = "orange")
rug(X.proj[(N+1):(2*N)], col = "lightblue")
rug(X.proj[(2*N+1):(3*N)], col = "darkgreen")
abline(h=c(0.0, 0.5, 1.0), lty=5, lwd = 0.4)
abline(v=c(sort(X.proj)[N], sort(X.proj)[N*2]), lwd = 0.4)
dev.off()

## reproduce figure 4.3 right
lm1 <- lm(y1 ~ X.proj + I(X.proj^2))
lm2 <- lm(y2 ~ X.proj + I(X.proj^2))
lm3 <- lm(y3 ~ X.proj + I(X.proj^2))
# order the corresponding prediction values y_hat
pred21 <- as.numeric(fitted(lm1)[order(X.proj)])
pred22 <- as.numeric(fitted(lm2)[order(X.proj)])
pred23 <- as.numeric(fitted(lm3)[order(X.proj)])
# get the first estimated class : 1 ~ c21
c21 <- which(pred21 <= pred22)[1]
# get the second estimated class: c21+1 ~ c22
c22 <- max(which(pred23 <= pred22)) 
# get the third estimated class : c22 ~ end
error2 <- (abs(c22 - 2*N) + abs(c21 - N))/(3*N)

# plot 
png("fig-4-3-right.png")
plot(0, 0, type = "n", 
     xlim = c(0, 2), ylim = c(-1,1),  xlab = "X.proj", ylab = "Y",
     main = paste0("Degree = 2; Error = ", round(error2, digits = 4)))
lines(sort(X.proj), pred21, type = 'o', pch = '1', col = "orange")
lines(sort(X.proj), pred22, type = 'o', pch = '2', col = "lightblue")
lines(sort(X.proj), pred23, type = 'o', pch = '3',  col = "darkgreen")

rug(X.proj[1:N], col = "orange")
rug(X.proj[(N+1):(2*N)], col = "lightblue")
rug(X.proj[(2*N+1):(3*N)], col = "darkgreen")
abline(h=c(0.0, 0.5, 1.0), lty=5, lwd = 0.4)
abline(v=c(sort(X.proj)[N], sort(X.proj)[N*2]), lwd = 0.4)
dev.off()



# y_hat <- X %*% solve(t(X) %*% X) %*% t(X) %*% y
