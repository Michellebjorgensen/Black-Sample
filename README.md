# Black-Sample

library(quantmod)
library(dynlm) 
library(AER) 
library(vars) 
library(forecast) 
library(stargazer) 
library(strucchange) 
library(readr) 
library(fGarch) 
library(urca)
library(xts)
library (astsa)
library (car) 
library(haven)

Black.samlet <- read_csv("Black_Samlet.csv") 
Black.samlet.xts <- xts(Black.samlet[, -1],  order.by = Black.samlet$DATE)

#Check for stationarity 
"Fundamentals"

Black.samlet.RETURNS <- ur.df(Black.samlet.xts$RETURNS, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.RETURNS)
plot(Black.samlet.xts$RETURNS)


Black.samlet.EBIT <- ur.df(Black.samlet.xts$EBIT, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.EBIT)
plot(Black.samlet.xts$EBIT)

Black.samlet.EBITD <- ur.df(Black.samlet.xts$EBITD, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.EBITD)
plot(Black.samlet.xts$EBITD)


Black.samlet.EBITDN <- ur.df(Black.samlet.xts$EBITDN, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.EBITDN)
plot(Black.samlet.xts$EBITDN)


Black.samlet.EBITDNC <- ur.df(Black.samlet.xts$EBITDNC, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.EBITDNC)
plot(Black.samlet.xts$EBITDNC)

Black.samlet.FCF <- ur.df(Black.samlet.xts$FCF, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.FCF)
plot(Black.samlet.xts$FCF)

Black.samlet.DTE <- ur.df(Black.samlet.xts$DTE, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.DTE)
plot(Black.samlet.xts$DTE)

Black.samlet.EBITREV <- ur.df(Black.samlet.xts$EBITREV, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.EBITREV)
plot(Black.samlet.xts$EBITREV)

"Contagion"

Black.samlet.VOLUME <- ur.df(Black.samlet.xts$VOLUME, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.VOLUME)
plot(Black.samlet.xts$VOLUME)

Black.samlet.VOLATILITY <- ur.df(Black.samlet.xts$VOLATILITY, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.VOLATILITY)
plot(Black.samlet.xts$VOLATILITY)

Black.samlet.MF <- ur.df(Black.samlet.xts$MF, type = "drift", lags = 6, selectlags = "AIC")
summary(Black.samlet.MF)
plot(Black.samlet.xts$MF)

#Estimating models 

ic.mat <- matrix(NA, nrow = 6, ncol = 2) 
print(ic.mat)

#EBIT

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(EBIT, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.EBIT <- dynlm(RETURNS ~  L(EBIT, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.EBIT, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.EBIT)
coeftest(DL.1.B.EBIT, vcov. = vcovHAC)

#EBITD

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(EBITD, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.EBITD <- dynlm(RETURNS ~  L(EBITD, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.EBITD, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.EBITD)
coeftest(DL.1.B.EBITD, vcov. = vcovHAC)


#EBITDN

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(EBITDN, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.EBITDN <- dynlm(RETURNS ~  L(EBITDN, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.EBITDN, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.EBITDN)
coeftest(DL.1.B.EBITDN, vcov. = vcovHAC)

#EBITDNC

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(EBITDNC, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.EBITDNC <- dynlm(RETURNS ~  L(EBITDNC, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.EBITDNC, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.EBITDNC)
coeftest(DL.1.B.EBITDNC, vcov. = vcovHAC)

#FCF

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(FCF, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.FCF <- dynlm(RETURNS ~  L(FCF, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.FCF, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.FCF)
coeftest(DL.1.B.FCF, vcov. = vcovHAC)

#EBITREV

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(EBITREV, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.EBITREV <- dynlm(RETURNS ~  L(EBITREV, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.EBITREV, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.EBITREV)
coeftest(DL.1.B.EBITREV, vcov. = vcovHAC)

#DTE

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(DTE, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.DTE <- dynlm(RETURNS ~  L(DTE, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.DTE, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.DTE)
coeftest(DL.1.B.DTE, vcov. = vcovHAC)


"CONTAGION"

#VOLUME

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(VOLUME, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.VOLUME <- dynlm(RETURNS ~  L(VOLUME, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.VOLUME, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.VOLUME)
coeftest(DL.1.B.VOLUME, vcov. = vcovHAC)

#VOLATILITY

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(VOLATILITY, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.VOLATILITY <- dynlm(RETURNS ~  L(VOLATILITY, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.VOLATILITY, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.VOLATILITY)
coeftest(DL.1.B.VOLATILITY, vcov. = vcovHAC)

#MF

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:6) {
  mod.temp <- dynlm(RETURNS ~  L(MF, 1:i), data = as.zoo(Black.samlet.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
} 
print(ic.mat)


which.min(ic.mat[, "AIC"])
which.min(ic.mat[, "BIC"])

DL.1.B.MF <- dynlm(RETURNS ~  L(MF, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.MF, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.MF)
coeftest(DL.1.B.MF, vcov. = vcovHAC)


#COMBINING MODELS 

#FCF and DTE 

DL.2.1.FCF.DTE <- dynlm(RETURNS ~  L(FCF, 1:2) + L(DTE, 1), as.zoo(Black.samlet.xts))
stargazer(DL.2.1.FCF.DTE, type = "text", keep.stat = c("n", "rsq"))
summary(DL.2.1.FCF.DTE)
coeftest(DL.2.1.FCF.DTE, vcov. = vcovHAC)

#VOLATILITY + FCF

DL.1.2.FCF.VOLATILITY <- dynlm(RETURNS ~  L(FCF, 1) + L(VOLATILITY, 1:2), as.zoo(Black.samlet.xts))
stargazer(DL.1.2.FCF.VOLATILITY, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.2.FCF.VOLATILITY)
coeftest(DL.1.2.FCF.VOLATILITY, vcov. = vcovHAC)


#GRANGER CAUSALITY 

"FCF"

DL.1.B.FCF <- dynlm(RETURNS ~  L(FCF, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.FCF, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.FCF)
coeftest(DL.1.B.FCF, vcov. = vcovHAC)

linearHypothesis(DL.1.B.FCF, c("L(FCF, 1) = 0"), vcov = vcovHAC)

"VOLATILITY"

DL.1.B.VOLATILITY <- dynlm(RETURNS ~  L(VOLATILITY, 1), as.zoo(Black.samlet.xts))
stargazer(DL.1.B.VOLATILITY, type = "text", keep.stat = c("n", "rsq"))
summary(DL.1.B.VOLATILITY)
coeftest(DL.1.B.VOLATILITY, vcov. = vcovHAC)

linearHypothesis(DL.1.B.VOLATILITY, c("L(VOLATILITY, 1) = 0"), vcov = vcovHAC)


#Testing for structural breaks in FCF

Black.samlet.xts$FCF.1 <- lag(Black.samlet.xts$FCF) 

head(Black.samlet.xts)

Black.samlet.xts<- na.omit(Black.samlet.xts)

Black.samlet.ts <- ts(as.matrix(Black.samlet.xts), start = c(2009, 2), frequency = 4)

Black.FCF.model.1 <- RETURNS ~ FCF.1 

qlr.FCF.model.1 <- Fstats(Black.FCF.model.1, from = 0.15, to = 0.85, data = Black.samlet.ts)

max(qlr.FCF.model.1$Fstats)

qlr.FCF.model.1$breakpoint

time(Black.samlet.ts)[qlr.FCF.model.1$breakpoint]

plot(qlr.FCF.model.1, alpha = 0.05)

#Testing for structural break in VOLATILITY 

Black.samlet.xts$VOLATILITY.1 <- lag(Black.samlet.xts$VOLATILITY) 
head(Black.samlet.xts)

Black.samlet.xts<- na.omit(Black.samlet.xts)

Black.samlet.ts <- ts(as.matrix(Black.samlet.xts), start = c(2009, 2), frequency = 4)

Black.VOLATILITY.model.1 <- RETURNS ~ VOLATILITY.1 

qlr.VOLATILITY.model.1 <- Fstats(Black.VOLATILITY.model.1, from = 0.15, to = 0.85, data = Black.samlet.ts)

max(qlr.VOLATILITY.model.1$Fstats)

qlr.VOLATILITY.model.1$breakpoint

time(Black.samlet.ts)[qlr.VOLATILITY.model.1$breakpoint]

plot(qlr.VOLATILITY.model.1, alpha = 0.05)
