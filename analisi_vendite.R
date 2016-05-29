library(gdata)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forecast)
library(xtable)

df = read.xls ("fatture_20160525.xls", sheet = 1, header = TRUE)
df2 = separate(data = df, col = Cliente_forn, into = c("cliente", "codice"), sep = "\\-")
df2$data = paste("2016", df2$Mese, df2$Giorno, sep = "/")
df2$data <- as.Date(df2$data, "%Y/%m/%d")
df_librerie = filter(df2, df2$Contributo == "")
df_contributi = filter(df2, df2$Contributo == "x")
df_librerie$Settimana <- as.Date(cut(df_librerie$data, breaks = "week", start.on.monday = TRUE))

ggplot(data = df_librerie,
aes(Mese, Valore_fatturato)) +
stat_summary(fun.y = sum, geom = "bar")

ggplot(data = df2,
aes(Mese, Valore_fatturato)) +
stat_summary(fun.y = sum, geom = "bar")

ggplot(data = df_librerie,
       aes(Settimana, Valore_fatturato)) +
  stat_summary(fun.y = sum, geom = "bar")

df_librerie_by_week = group_by(df_librerie, Settimana)
fatturato_settimanale_librerie = summarise(df_librerie_by_week, fatturato = sum(Valore_fatturato), count = n())

ggplot(fatturato_settimanale_librerie, aes(Settimana, fatturato)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

myts <- ts(fatturato_settimanale_librerie$fatturato, frequency=4)
plot.ts(myts)
components = decompose(myts)
plot(components)
myts_forecast = HoltWinters(myts, beta=FALSE, gamma=FALSE)
plot(myts_forecast)
# previsioni per il mese successivo
myts_forecast4 = forecast.HoltWinters(myts_forecast, h=4)
plot(myts_forecast4)

# fatturato_settimanale_librerie$ID<-seq.int(nrow(fatturato_settimanale_librerie))
# trend<-lm(ID ~ fatturato, data=fatturato_settimanale_librerie)
# summary(trend)
# mts <- ts(df_librerie$monthly_revenue, 
#           start=c(2012,1),end=c(2016,6),frequency=12) 
# fit <- stl(mts, s.window="period")
# plot(fit)
