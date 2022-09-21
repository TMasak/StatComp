y <- rnorm(100)
X <- matrix(rnorm(100*50),ncol=50)
mydat <- as.data.frame(X)
mydat$y <- y

m1 <- lm(y~.-1, data=mydat)
summary(m1)

str(summary(m1))
summary(m1)$coefficients
keepers <- which(summary(m1)$coefficients[,4] < 0.1)
newdat <- mydat[,c(51,keepers)]
str(newdat)

m2 <- lm(y~.-1, data=newdat)
summary(m2)
