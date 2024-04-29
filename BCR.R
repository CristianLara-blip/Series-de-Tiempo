install.packages("forecast")
install.packages("tseries")
install.packages("ts")
library(forecast)
library(tseries)
library(ts)

library(readr)
Data <- read_csv("G:/Mensuales.csv")
View(Data)
dim(Data)
print(Data)
str(Data)
names(Data)

#Cargamos los datos co2
co2<-read.csv("G:/Mensuales.csv", header = TRUE, sep = ";")
# Transformar los datos en una serie temporal
co2ts <- ts(as.numeric(Data$PN01234PM[-1]), start = c(1992, 1), frequency = 12)

# Imprimir la serie temporal
print(co2ts)



library(ggfortify)
#Trazamos la serie de tiempo co2ts
autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")

autoplot(acf(co2ts, plot = FALSE))

autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")

#MODELO ARIMA
ndiffs(co2ts)

nsdiffs(co2ts)

diff.co2ts<-autoplot(diff(co2ts), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.co2ts


autoplot(acf(diff(co2ts), plot = FALSE))


monthplot(diff(co2ts), col = "midnightblue")

#Diagrama de cajas
diffco2<-diff(co2ts)
boxplot(diffco2~cycle(diffco2))

diff.co2ts.12<-diff(co2ts, lag = 12)
autoplot(diff.co2ts.12, ts.colour = "darkorange4", ts.linetype = "dashed")

#Prueba de Hipótesis
library(tseries)
adf<-adf.test(diff.co2ts.12)
adf$p.value

kpss<-kpss.test(diff.co2ts.12)

kpss$p.value


autoplot(acf(diff.co2ts.12, plot = FALSE))

autoplot(pacf(diff.co2ts.12, plot = FALSE))

#Modelos ARIMA
library(forecast)
arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))

AIC(arima1,arima2,arima3,arima5,arima6,arima7)

BIC(arima1,arima2,arima3,arima5,arima6,arima7)


autoplot(acf(arima6$residuals, plot = FALSE))


autoplot(pacf(arima6$residuals, plot = FALSE))


ggtsdiag(arima6)


bp <- Box.test(arima6$residuals) # Test de Box-Pierce
bp$p.value

lb <- Box.test(arima6$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value

jb <- jarque.bera.test(arima6$residuals) # Test de Jarque-Bera
jb$p.value


sht<-shapiro.test(arima6$residuals) $ # Test de Shapiro-Wilk
  sht$p.value


auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)


#Predicción de la Series
forecast1<-forecast(arima6, level = c(95), h = 50)
autoplot(forecast1)

