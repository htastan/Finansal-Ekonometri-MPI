# BIST100 günlük veri xts 

library(xts)
load("R/Data/bist100xts.rda")

plot(BIST100)

# simple returns 
basit_getiri <- diff(BIST100)
plot(basit_getiri)

# göreli getiri
lag1 <- lag(BIST100, 1)
head(lag1)
goreli_getiri <- (BIST100 - lag1)/lag1
head(goreli_getiri)
plot(goreli_getiri)

# logaritmik getiri 
log_bist <- log(BIST100)
log_getiri <- log_bist - lag(log_bist, 1)
head(log_getiri)

# yüzde getiri 
log_getiri100 <- 100*(log_bist - lag(log_bist, 1))
head(log_getiri100)

# haftalık getiriler 
log_getiri100_h <- apply.weekly(log_getiri100, sum)
head(log_getiri100_h)
plot(log_getiri100_h)

# aylık log getiriler 
log_getiri100_a <- apply.monthly(log_getiri100, sum)
head(log_getiri100_a)
plot(log_getiri100_a)

# 3 aylık log getiriler 
log_getiri100_q <- apply.quarterly(log_getiri100, sum)
head(log_getiri100_q)
plot(log_getiri100_q)

# yıllık log getiriler 
log_getiri100_y <- apply.yearly(log_getiri100, sum)
head(log_getiri100_y)
plot(log_getiri100_y)

# normal dağılım 
z <- rnorm(10000, mean=0, sd=1)
hist(z, nclass = 20, probability = TRUE, main=" ")
zsorted <- sort(z, decreasing = FALSE)
lines(zsorted, dnorm(zsorted, mean=0, sd=1), col="red")

qqnorm(z)
qqline(z)


# 2003 sonrası 
BIST100x <- log_getiri100["2003/"]
hist(BIST100x, nclass = 20, probability = TRUE)

xbar <- mean(BIST100x)
sigma <- sd(BIST100x)

hist(BIST100x, breaks = 20, probability = TRUE)
lines(BIST100x, dnorm(BIST100x, mean = xbar, sd = sigma), col = "red")

z <- as.numeric(BIST100x)
hist(z, breaks = 20, probability = TRUE, main=" ")
lines(sort(z), dnorm(sort(z), mean = xbar, sd = sigma), col = "red")

qqnorm(z)
qqline(z)
