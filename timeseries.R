library(PerformanceAnalytics)
library(FinTS)
library(rugarch)
library(forecast)
library(tseries)

EFGI = read.csv(file = "Egyptian Financial Group Historical Data.csv")
EFGI1 = EFGI[1:which(EFGI$Date == "1997-01-30"),]
EFGI1 = ts(EFGI1$Price)
EFGI1 = CalculateReturns(EFGI1, method="log")[-1]
EFGI2 = EFGI[which(EFGI$Date == "1997-02-02"):dim(EFGI)[1],]
EFGI2 = ts(EFGI2$Price)
EFGI2 = CalculateReturns(EFGI2, method="log")[-1]

HFI = read.csv(file = "Hermes Financial Historical Data.csv")
HFI1 = HFI[1:which(HFI$Date == "1997-01-30"),]
HFI1 = ts(HFI1$Price)
HFI1 = CalculateReturns(HFI1, method="log")[-1]
HFI2 = EFGI[which(HFI$Date == "1997-02-02"):dim(HFI)[1],]
HFI2 = ts(HFI2$Price)
HFI2 = CalculateReturns(HFI2, method="log")[-1]
HFI2 = na.omit(HFI2)

#plots
plot(EFGI1, type="l")
plot(EFGI2, type="l")
plot(HFI1, type="l")
plot(HFI2, type="l")

#statistics
table.Stats(EFGI1)
table.Stats(EFGI2)
table.Stats(HFI1)
table.Stats(HFI2)

#jarque bera test
jarque.bera.test(EFGI1)
jarque.bera.test(EFGI2)
jarque.bera.test(HFI1)
jarque.bera.test(HFI2)

#acf
acf(EFGI1)
acf(EFGI2)
acf(HFI1)
acf(HFI2)

#testing for autocorrelations
Box.test(EFGI1, type="Ljung-Box", lag = 5)
Box.test(EFGI2, type="Ljung-Box", lag = 5)
Box.test(HFI1, type="Ljung-Box", lag = 5)
Box.test(HFI2, type="Ljung-Box", lag = 5)

#ARCH LM test
ArchTest(EFGI1)
ArchTest(EFGI2)
ArchTest(HFI1)
ArchTest(HFI2)

#GARCH
garch11.11 = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                        mean.model = list(armaOrder=c(1,1)))
garch11.22 = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                        mean.model = list(armaOrder=c(2,2)))
EFGI1.fit = ugarchfit(spec=garch11.11, data=EFGI1)
EFGI2.fit = ugarchfit(spec=garch11.22, data=EFGI2)
HFI1.fit = ugarchfit(spec=garch11.11, data=HFI1)
HFI2.fit = ugarchfit(spec=garch11.11, data=HFI2)


#100-ahead forecasts
EFGI1.forecast = ugarchforecast(EFGI1.fit, n.ahead=100, out.sample=100)
EFGI2.forecast = ugarchforecast(EFGI2.fit, n.ahead=100, out.sample=100)
HFI1.forecast = ugarchforecast(HFI1.fit, n.ahead=100, out.sample=100)
HFI2.forecast = ugarchforecast(HFI2.fit, n.ahead=100, out.sample=100)

#100-ahead forecasts - time series
plot(EFGI1.forecast, which=1)
plot(EFGI2.forecast, which=1)
plot(HFI1.forecast, which=1)
plot(HFI2.forecast, which=1)

#100-ahead forecasts - sigma
plot(EFGI1.forecast, which=3); abline(a=sqrt(uncvariance(EFGI1.fit)),b=0)
plot(EFGI2.forecast, which=3); abline(a=sqrt(uncvariance(EFGI2.fit)),b=0)
plot(HFI1.forecast, which=3); abline(a=sqrt(uncvariance(HFI1.fit)),b=0, lty=5)
plot(HFI2.forecast, which=3); abline(a=sqrt(uncvariance(HFI2.fit)),b=0, lty=5)


#Rolling forecasts
#hold back last 100 observations - how well does it predict?
EFGI1.roll = ugarchroll(garch11.11, data = EFGI1, n.start = length(EFGI1)-100,
                        refit.window = "moving", refit.every = 10)
EFGI2.roll = ugarchroll(garch11.22, data = EFGI2, n.start = length(EFGI2)-100,
                        refit.window = "moving", refit.every = 10)
HFI1.roll = ugarchroll(garch11.11, data = HFI1, n.start = length(HFI1)-100,
                       refit.window = "moving", refit.every = 10)
HFI2.roll = ugarchroll(garch11.11, data = HFI2, n.start = length(HFI2)-100,
                       refit.window = "moving", refit.every = 10)

#Rolling forecasts - time series
plot(EFGI1.roll, which=3)
plot(EFGI2.roll, which=3)
plot(HFI1.roll, which=3)
plot(HFI2.roll, which=3)

#Rolling forecasts - sigma
plot(EFGI1.roll, which=2)
plot(EFGI2.roll, which=2)
plot(HFI1.roll, which=2)
plot(HFI2.roll, which=2)