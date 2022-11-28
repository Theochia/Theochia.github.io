# simpleVAR.R example
#
# 20220117 : Initial version
#
# Patrick T. Brandt
# UT Dallas, EPPS
#

# Load libraries
library(fredr)
library(xts)
library(vars)

key <- "f00943b4cc68f3867f1762517ed4d467"

fredr_set_key(key)

install.packages("jsonlite")

# Simple data for a 3 equation VAR for US Economy, quarterly data

# CPI
CPI <- fredr(series_id = "CPIAUCSL", 
             observation_start = as.Date("1954-07-01"),
             frequency = "q", units = "pc1")

CPI <- xts(CPI$value, CPI$date)

dCPI <- (diff(CPI))

plot(diff(CPI))
             
# U
U <- fredr(series_id = "UNRATE", 
           observation_start=as.Date("1948-01-01"),
           frequency = "q", units="lin")
U <- xts(U$value, U$date)

plot(U)

plot(diff(U))

dU <- diff(U)

# GDP
Y <- fredr(series_id = "GDPC1",
           observation_start = as.Date("1948-01-01"),
           frequency = "q", units="pc1")
Y <- xts(Y$value, Y$date)

plot(Y)

acf(Y)

pacf(Y)

dY <- diff(Y)

plot(dY)

# FFR

R <- fredr(series_id = "FEDFUNDS", observation_start = as.Date("1954-07-01"),
           frequency = "q", units = "lin" )
R <- xts(R$value, R$date)

plot(R)

acf(R)
pacf(R)

dR <- diff(R)

plot(dR)

dR <- diff(log(R))

plot(dR)
# Plot
plot(cbind(dCPI,dU,dY,dR))

# Set up the sample
Z <- cbind(dR,dY,dU,dCPI)["1954-10-01/2022-04-01",]

# Select a lag length
VARselect(Z, lag.max=8)

# Fit a model
V4x <- VAR(X, 4)

V8x <- VAR(X, 8)

# IRFs
V4.irf <- vars::irf(V4, n.ahead=12)

plot(V4.irf)

V8.irf <- vars::irf(V8, n.ahead=12)

plot(V8.irf)

fevd(V4x, n.ahead = 12)

fevd(V8x, n.ahead = 12)

# Set up the sample
X <- cbind(dCPI,dU,dY,dR)["1954-10-01/2022-04-01",]

# Select a lag length
VARselect(X, lag.max=8)

# Fit a model
V4 <- VAR(X, 4)

V8 <- VAR(X, 8)

# IRFs
V4.irf <- vars::irf(V4, n.ahead=12)

plot(V4.irf)

V8.irf <- vars::irf(V8, n.ahead=12)

plot(V8.irf)

