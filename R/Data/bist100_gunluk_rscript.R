# BIST100 günlük veri okuma ve analiz

library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)

bist100_gunluk <- read_excel("R/Data/bist100_gunluk.xlsx")

bist100_gunluk <- bist100_gunluk %>% drop_na() %>% 
  mutate(date = lubridate::dmy(date))

bist100_2003 <- bist100_gunluk %>% 
  filter(lubridate::year(date)>2002) 

bist100_2003 %>% ggplot(aes(date, BIST100)) +
  geom_line() +
  xlab("") +
  ylab("BIST100 Bileşik Endeks") + 
  ggtitle("Borsa Istanbul 100 Endeksi: 02.01.2003-29.09.2021")

# Günlük getiri oranları 
bist100_2003 <- bist100_2003 %>% 
  mutate(getiri = 100*(log(BIST100) - log(lag(BIST100,1,order_by = date))))

# getiri grafiği 
bist100_2003 %>% ggplot(aes(date, getiri)) + 
  geom_line() +
  xlab("") +
  ylab("Getiri, %") + 
  ggtitle("Borsa Istanbul 100 Endeks Getirileri: 03.01.2003-29.09.2021")

# getirilerin karesi 
bist100_2003 %>% ggplot(aes(date, getiri^2)) + 
  geom_line() +
  xlab("") +
  ylab("Getiri Kare") + 
  ggtitle("Borsa Istanbul 100 Endeks Getirilerin Karesi: 03.01.2003-29.09.2021")

# getirileri için zaman serisi objesi oluştur
# ilk gözlem NA olduğu için dışla 
bist100_2003_getiri <- ts(bist100_2003$getiri[-1])

# getirilerin korelogramını çiz 
ggAcf(bist100_2003_getiri)
ggAcf(bist100_2003_getiri^2 , 60)

# ARCH testi
library(FinTS)
FinTS::ArchTest(bist100_2003_getiri, lags = 1, demean = TRUE)

# ARCH modeli 
library(rugarch) 

arch_spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(1,0)), 
  distribution.model="norm")

archfit1 <- ugarchfit(spec = arch_spec1, 
                      data = bist100_2003_getiri)
coef(archfit1)
plot(archfit1, which="all")

# ARMA(1,1) + ARCH(5)
arch_spec2 <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(5,0)),
  mean.model = list(armaOrder = c(1,1)), 
  distribution.model="norm")

archfit2 <- ugarchfit(spec = arch_spec2, 
                      data = bist100_2003_getiri)
coef(archfit2)
plot(archfit2, which="all")

# ARMA(1,1) + GARCH(1,1) + skewed t 
garch_spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)), 
  distribution.model="sstd")

garchfit1 <- ugarchfit(spec = garch_spec1, 
                      data = bist100_2003_getiri)
coef(garchfit1)
print(garchfit1)
plot(garchfit1, which="all")

# Standardize edilmiş kalıntıların karesinin ACF'sini inceleyiniz
plot(garchfit1, which=11)


# ARMA(1,1) + EGARCH(1,1) + skewed t 
egarch_spec1 <- ugarchspec(
  variance.model = list(model = "eGARCH",
                        garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)), 
  distribution.model="sstd")

egarchfit1 <- ugarchfit(spec = egarch_spec1, 
                       data = bist100_2003_getiri)
coef(egarchfit1)
print(egarchfit1)
plot(egarchfit1, which="all")

# Standardize edilmiş kalıntıların karesinin ACF'sini inceleyiniz
plot(egarchfit1, which=11)

# news impact curve
plot(egarchfit1, which=12)



