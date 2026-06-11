# Kovaryans ve korelasyon modelleri sunumu icin grafikler
# (docs/zaman_serileri6.Rmd icinde kullaniliyor)
load("Data/bist_usd.rda")

tarih <- as.Date(paste0(bist_usd$date, "-01"))
r_bist <- diff(log(bist_usd$bist100)) * 100
r_usd  <- diff(log(bist_usd$usd)) * 100
tarih_r <- tarih[-1]

# 1. Getiri serileri
png("../docs/img/mgarch1.png", width = 1800, height = 1100, res = 170)
par(mfrow = c(2, 1), mar = c(3, 4, 2.5, 1))
plot(tarih_r, r_bist, type = "l", col = "steelblue", lwd = 1.2,
     main = "BIST-100 aylık getiriler (%)", xlab = "", ylab = "%")
abline(h = 0, col = "gray")
plot(tarih_r, r_usd, type = "l", col = "firebrick", lwd = 1.2,
     main = "USD/TRY aylık getiriler (%)", xlab = "", ylab = "%")
abline(h = 0, col = "gray")
dev.off()

# 2. EWMA varyans-kovaryans ve korelasyon (RiskMetrics, lambda = 0.97 aylik)
lambda <- 0.97
n <- length(r_bist)
x <- r_bist - mean(r_bist)
y <- r_usd - mean(r_usd)
h11 <- h22 <- h12 <- numeric(n)
h11[1] <- var(x); h22[1] <- var(y); h12[1] <- cov(x, y)
for (t in 2:n) {
  h11[t] <- lambda * h11[t - 1] + (1 - lambda) * x[t - 1]^2
  h22[t] <- lambda * h22[t - 1] + (1 - lambda) * y[t - 1]^2
  h12[t] <- lambda * h12[t - 1] + (1 - lambda) * x[t - 1] * y[t - 1]
}
rho <- h12 / sqrt(h11 * h22)

png("../docs/img/mgarch2.png", width = 1800, height = 1000, res = 170)
par(mar = c(3, 4, 2.5, 1))
plot(tarih_r, rho, type = "l", col = "steelblue", lwd = 2, ylim = c(-1, 0.2),
     main = expression(paste("BIST-100 ve USD/TRY getirileri: EWMA korelasyonu (", lambda, " = 0.97)")),
     xlab = "", ylab = "Korelasyon")
abline(h = cor(r_bist, r_usd), col = "firebrick", lty = 2, lwd = 2)
abline(h = 0, col = "gray")
legend("topright", legend = c("EWMA (zamanla değişen) korelasyon", "Örneklem (sabit) korelasyonu"),
       col = c("steelblue", "firebrick"), lty = c(1, 2), lwd = 2, bty = "n")
dev.off()

cat("Örneklem korelasyonu:", round(cor(r_bist, r_usd), 3), "\n")
