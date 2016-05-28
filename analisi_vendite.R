library(gdata)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)

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

