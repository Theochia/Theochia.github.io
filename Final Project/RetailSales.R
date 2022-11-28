# RetailSales.R
#
# Analysis of monthly US retail sales, NSA
#

rs <- read.csv("RSAFSNA.csv")
rs <- ts(rs[,2], start=c(1992,1), freq=12)

pdf(file="../slides/retail.pdf", width=4, height=4)
plot(rs, ylab="Monthly Retail Sales, Millions $",
     cex=0.6, cex.lab=0.8)
dev.off()

# Now plot the transformed data, ACF, and PACF

dlrs <- diff(log(rs))

pdf(file="../slides/retail1.pdf", width=6, height=4)
par(mfrow=c(1,3))
plot(dlrs)
abline(h=0)
acf(dlrs, main="")
pacf(dlrs, main="")
dev.off()

# Fit some ARMA models to these data
M0 <- arima(diff(dlrs, 12), order=c(1,0,1), seasonal=list(order=c(0,0,2)))
Ma <- arima(dlrs, order=c(1,0,1),
            seasonal=list(order=c(0,1,2)))

M1 <- arima(dlrs, order=c(2,0,1),
            seasonal=list(order=c(0,1,4)))

tsdiag(M1, gof.lag=24)

