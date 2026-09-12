#2
profil<-data.frame(
  gender = c("f","f","f","m","m","f","m","m","f","m"),
  v1 = c(8,15,8,14,8,10,9,9,10,12),
  v2 = c(9,8,13,9,2,6,9,10,13,10),
  v3 = c(5,9,12,8,9,10,7,8,6,9),
  v4 = c(9,5,6,NA,16,9,13,10,7,8),
  v5 = c(9,10,7,11,8,10,9,5,10,17),
  v6 = c(9,8,9,10,10,10,12,10,12,7),
  v7 = c(5,10,14,6,8,9,10,6,13,9),
  v8 = c(9,12,12,11,9,7,9,12,9,10),
  v9 = c(11,9,12,11,8,7,11,6,6,7),
  v10 = c(8,15,9,8,9,11,7,8,9,7)
)
filter <- (profil$gender=="m" & profil$v1>9) | (profil$gender=="f" & profil$v3<=10)
subs<-profil[filter,]
urut<-order(subs$gender, subs$v5, decreasing = c(T,F),method = "radix")
subs[urut,]
setwd(dir = "D:\\00. S2 SSD IPB\\Bahan Ajar\\SEM 1\\Pemrograman Statistika\\Praktikum\\4")
write.csv(subs[urut,],"profil_filter.csv")

#3
df_sales <- data.frame(
  id_toko = c("T1", "T2", "T3"),
  Minggu1 = c(120, 150, 95),
  Minggu2 = c(135, 140, 110)
)

df_info <- data.frame(
  id = c("T1", "T2", "T4"),
  Kota = c("Jakarta", "Bandung", "Surabaya")
)

df_merge<-merge(x = df_sales, y = df_info, by = 1, all = T)
row.names(df_merge)<-NULL
df_merge
df_long<-reshape(data = df_merge,varying = c("Minggu1", "Minggu2"), v.names = "Penjualan", timevar = "Waktu", times = c("Minggu1", "Minggu2"), direction = "long")
row.names(df_long) <- NULL
df_long
write.csv(df_long,"penjualan_long.csv")

#4
#a
df<-CO2
str(df)
head(df)
summary(df$uptake)
summary(df$conc)
plot(x = df$conc, y = df$uptake, type = "p", xlab = "Ambient CO2 Concentration", ylab = "CO2 Uptake Rate", 
     main = "Pengaruh konsentrasi CO2 terhadap CO2 uptake", pch=8, cex=1.2, col="seagreen",
     xlim = c(0,1000), ylim = c(0,50), axes = F, frame.plot = T)
meanuptake<-tapply(df$uptake, df$conc, mean)
unique(df$conc)
axis(1,unique(df$conc))
axis(2,seq(0,50, by=5))
points(unique(df$conc), meanuptake, pch=14, cex=2,col="red")
lines(unique(df$conc), meanuptake, lty=2, col="red")

#b
par(mfrow=c(2,2))
hist(x = df$uptake, freq = F, main = "Sebaran Data CO2 Uptake", xlab = "CO2 Uptake")
curve(dnorm(x, mean = mean(df$uptake), sd = sd(df$uptake)),
      add = TRUE, col = "red", lwd = 2)

plot(x = df$conc, y = df$uptake, type = "p", xlab = "Ambient CO2 Concentration", ylab = "CO2 Uptake Rate", 
     main = "Pengaruh konsentrasi CO2\nterhadap CO2 uptake", pch=8, cex=1.2, col="seagreen",
     xlim = c(0,1000), ylim = c(0,50), axes = F, frame.plot = T)
meanuptake<-tapply(df$uptake, df$conc, mean)
unique(df$conc)
axis(1,unique(df$conc))
axis(2,seq(0,50, by=5))
points(unique(df$conc), meanuptake, pch=14, cex=2,col="red")
lines(unique(df$conc), meanuptake, lty=2, col="red")

aggr<-aggregate(uptake ~ Plant, data=df, FUN = mean)
barp<-setNames(aggr[,2],aggr[,1])
barplot(height = barp, main = "Rata-rata CO2 uptake\nMenurut Jenis Tanaman", xlab = "Jenis Tanaman", ylab="CO2 uptake", ylim=c(0,40))

aggr2<-aggregate(uptake ~ Treatment, data=df, FUN = mean)
piec<-setNames(aggr2[,2],paste(aggr2[,1], "\n", round(aggr2[,2], 2)))
pie(x = piec, main = "Rata-rata CO2 uptake\nMenurut Treatment")

#c
library(ggplot2)
qplot(x = conc, y = uptake, data = df, color = Treatment)
gg<-ggplot(data = df, aes(x = conc, y = uptake, colour = Treatment, shape = Type))
gg1<-gg+geom_point()
gg1

#d
gg2<-gg1+stat_smooth(aes(group = Treatment),method="loess", se = FALSE)+
  scale_color_manual(values = c("chilled" = "skyblue",
                                "nonchilled" = "red"))+
  coord_cartesian(xlim = c(0, 1000), ylim = c(0, 50))
gg2<- gg2 + labs(title = "Pengaruh konsentrasi CO2 terhadap CO2 uptake",
                 subtitle = "Menurut Type dan Treatment Tanaman",
                 x = "Konsentrasi CO2",
                 y = "Uptake CO2") +
  theme_minimal()
gg2
ggsave(plot = gg2, filename = "co2_uptake_visualisasi.png")
