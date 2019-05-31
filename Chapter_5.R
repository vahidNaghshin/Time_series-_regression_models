library(fpp2)

#Problem 5-1
daily20 <- head(elecdaily,20)
qplot(Temperature, Demand, data=as.data.frame(elecdaily)) + ylab("Demand (GW)") + xlab("Temperature (Celsius)")

autoplot(daily20[,c("Demand", "Temperature")]) + ylab("GW") + xlab("Week")
daily20 %>% as.data.frame() %>% GGally::ggpairs()
# scatter plot
ggplot(as.data.frame(daily20),aes(x=Temperature, y=Demand))+geom_point()

daily20 %>% as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) + ylab("Demand (GW)") + xlab("Temperature (Celsius)") + geom_point() +
  geom_smooth(method="lm", se=FALSE)
# The linear regression line
fit.Dem <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit.Dem)

autoplot(daily20[,'Demand'], series="Data") + autolayer(fitted(fit.Dem), series="Fitted") + xlab("Week") + ylab("") +
  ggtitle("Elec Demand") + guides(colour=guide_legend(title=" "))
newdata <- data.frame(
  Temperature = c(15,35))
fcast.up <- forecast(fit.Dem, newdata=data.frame(Temperature=c(15,35)))
autoplot(daily20[,'Demand'], series="Data") + autolayer(fitted(fit.Dem), series="Fitted")+autolayer(fcast.up, series="Forecasted") + xlab("Week") + ylab("") +
  ggtitle("Elec Demand") + guides(colour=guide_legend(title=" "))
#Problem 5-2
autoplot(mens400)
fit.mens <- tslm(mens400 ~ trend)
# the rate of decrease per year
fit.mens[1]$coefficients[2]
autoplot(mens400, series="Data") + autolayer(fitted(fit.mens), series="Fitted") + xlab("Year") + ylab("second") +
  ggtitle("the winning times (in seconds) for the men’s
400 meters") + guides(colour=guide_legend(title=" "))
checkresiduals(fit.mens)
fcast <- forecast(fit.mens, h=2)
fcast
#Problem 5-3
# determine the dummy variables for Qtr data 
easter(ausbeer)
# Problem 5-5
autoplot(fancy)
fancy
autoplot(log(fancy))
BoxCox.lambda(fancy)
# defining the surfing festival dummy variable
dummy_fest = rep(0, length(fancy))
dummy_fest[seq_along(dummy_fest)%%12 == 3] <- 1
dummy_fest[3] <- 0 #festival started one year later
dummy_fest = ts(dummy_fest, freq = 12, start=c(1987,1))
my_data <- data.frame(
  log(fancy),
  dummy_fest
)
fit.fancy <- tslm(log(fancy) ~ trend + season + dummy_fest, data=my_data)
autoplot(log(fancy), series="Data") + autolayer(fitted(fit.fancy), series="Fitted") + xlab("Year") + ylab("log(sale)") +
  ggtitle("log of Monthly sales") + guides(colour=guide_legend(title=" "))
summary(fit.fancy)
my_data_res <- data.frame(
  log(fancy),
  residuals(fit.fancy)
)
autoplot(residuals(fit.fancy))
qplot(log.fancy., residuals.fit.fancy., data=my_data_res) + ylab("residual error") + xlab("fitted value")
boxplot(residuals(fit.fancy))
checkresiduals(fit.fancy)

future_data <- data.frame(
  dummy_fest = rep(0, 12)
)
fcast <- forecast(fit.fancy, newdata=future_data)
autoplot(log(fancy)) +
  ylab("log of Monthly sales") + autolayer(fcast, PI = TRUE, series = "forecasted sales")+ guides(colour = guide_legend(title = "Scenario"))
#un-logging the forecast result
fcast$mean<-exp(fcast$mean)
fcast$upper<-exp(fcast$upper)
fcast$lower<-exp(fcast$lower)
fcast$x<-exp(fcast$x)
autoplot(fancy) +
  ylab("Monthly sales") + autolayer(fcast, PI = TRUE, series = "forecasted sales")+ guides(colour = guide_legend(title = "Scenario"))
#Problem 5-6
gasweekly <- window(gasoline, end=2005)
autoplot(gasweekly)
fourier.gas <- tslm(gasweekly ~ trend + fourier(gasweekly, K=4)) 
summary(fourier.gas)
autoplot(gasweekly, series="Data") + autolayer(fitted(fourier.gas), series="Fitted") + xlab("Year") + ylab("million barrels per day") +
  ggtitle("Suuplies of US finished motor gasoline product") + guides(colour=guide_legend(title=" "))
# model selection: number of Fourier series by looking at AIC
for (i in seq(2,10)) {
  g=i
  print(g)
  fourier.gas <- tslm(gasweekly ~ trend + fourier(gasweekly, K=g)) 
  print(CV(fourier.gas))
  
}
fourier.gas <- tslm(gasweekly ~ trend + fourier(gasweekly, K=7)) 
summary(fourier.gas)
autoplot(gasweekly, series="Data") + autolayer(fitted(fourier.gas), series="Fitted") + xlab("Year") + ylab("million barrels per day") +
  ggtitle("Suuplies of US finished motor gasoline product") + guides(colour=guide_legend(title=" "))
checkresiduals(fourier.gas)
fc <- forecast(fourier.gas, newdata=data.frame(fourier(gasweekly,7,52)))
autoplot(gasweekly, series="Data") + autolayer(fc, series="Forecasted") + xlab("Year") + ylab("million barrels per day") +
  ggtitle("Suuplies of US finished motor gasoline product") + guides(colour=guide_legend(title=" "))

#Proiblem 5-7
autoplot(huron)
fit.lin <- tslm(huron ~ trend)
autoplot(huron, series="Data") + autolayer(fitted(fit.lin), series="Fitted") + xlab("Year") + ylab("Lake level") +
  ggtitle("Horun Lake level") + guides(colour=guide_legend(title=" "))
h <- 8
fcast.huron <- forecast(fit.lin, h=h)
t <- time(huron)
t.break1 <- 1915
tb1 <- ts(pmax(0, t-t.break1), start = 1875 )
fit.pw <- tslm(huron ~ t + tb1)
autoplot(huron, series="Data") + autolayer(fitted(fit.lin), series="Fitted") + autolayer(fitted(fit.pw), series="piece-wise Fitted")+ xlab("Year") + ylab("Lake level") +
  ggtitle("Horun Lake level") + guides(colour=guide_legend(title=" "))
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h) 
newdata <- cbind(t=t.new, tb1=tb1.new) %>% as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)
autoplot(huron, series="Data") + autolayer(fitted(fit.lin), series="Fitted") + autolayer(fitted(fit.pw), series="piece-wise Fitted")+
  autolayer(fcast.huron, series="trend forecasted")+
  autolayer(fcasts.pw, series="piece-wise forecasted")+
  xlab("Year") + ylab("Lake level") +
  ggtitle("Horun Lake level") + guides(colour=guide_legend(title=" "))


