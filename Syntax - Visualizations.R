install.packages("ggdark")
data1 <- read_excel("C:/Users/Abraham/Documents/abraham/aeddd sem 4/aeddd sem 4/data aed.xlsx", sheet = 4)
data1
sum(is.na(data1))

data2 <- read_excel("C:/Users/Abraham/Documents/abraham/aeddd sem 4/aeddd sem 4/data aed.xlsx", sheet = 5)
data2
data2$`Produksi Daging`
View(data2)

sum(is.na(data2))
data2ayamkampung = data2[data2$`Jenis Ayam`=="Ayam Kampung",]
data2ayamkampung
sum(is.na(data2ayamkampung$`Produksi Daging`))

data3 <- read_excel("C:/Users/Abraham/Documents/abraham/aeddd sem 4/aeddd sem 4/data aed.xlsx", sheet = 6)
data3$Tahun
data3[data3$`Jenis Ternak`=="Domba",]$Produksi
meandomba <- mean(data3[data3$`Jenis Ternak`=="Domba",]$Produksi, na.rm = T)
data3[data3$`Jenis Ternak`=="Domba",]$Produksi <- replace(data3[data3$`Jenis Ternak`=="Domba",]$Produksi,
                                                          is.na(data3[data3$`Jenis Ternak`=="Domba",]$Produksi), meandomba)
View(data3)
max(data3$Produksi)

#BOX PLOT
par(mfrow = c(1,1))
ggplot(data3, aes(x=`Jenis Ternak`, y=Produksi, fill=Tahun)) + 
  geom_boxplot() +
  facet_wrap(~Tahun, scales = "free") +
  labs(title = "Box Plot Produksi Daging Ternak",
       subtitle = "By Species and years",
       caption = "Source : BPS Jawa Timur")

#populasi 2021
data2020pop = data2[data2$Tahun==2020,]
View(data2020pop)
data2020pop$Tahun
pet2021 = data2021pop[data2021pop$`Jenis Ayam`=="Ayam Petelur",]
max(pet2021$Populasi)
meanpet21pop <- mean(pet2021$Populasi)
meanpet21pop

#SCATTER + BUBBLE
ggplot(data2 , 
       aes(x = Populasi, y = `Produksi Daging`, size = `Produksi Telur`, col = `Jenis Ayam`)) + 
  geom_point(alpha = 0.7) + 
  xlim(0,7500000) +
  facet_grid(Tahun~`Jenis Ayam`) +
  scale_size(range = c(.1, 10), name="Produksi Telur") + 
  labs(title = "Scatter Plot Produksi Daging vs Populasi",
       subtitle = "By species and years",
       x = "Populasi",
       y = "Produksi Daging",
       caption = "Data source : BPS Jawa Timur") + 
  dark_theme_gray() + 
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 15), 
        plot.caption = element_text(face = "bold.italic", size = 9)) +
  scale_color_brewer(palette = "Pastel2")

#changing missing value for ayam kampung
fixedayamk <- mean(data2[data2$`Jenis Ayam`=="Ayam Kampung",]$`Produksi Daging`, na.rm = T)
data2[data2$`Jenis Ayam`=="Ayam Kampung",]$`Produksi Daging` <- replace(data2[data2$`Jenis Ayam`=="Ayam Kampung",]$`Produksi Daging`,
                            is.na(data2[data2$`Jenis Ayam`=="Ayam Kampung",]$`Produksi Daging`), fixedayamk)

#changing missing value for ayam petelur
sum(is.na(data2[data2$`Jenis Ayam`=="Ayam Petelur",]$`Produksi Daging`))
fixedayampet <- mean(data2[data2$`Jenis Ayam`=="Ayam Petelur",]$`Produksi Daging`, na.rm = T)
data2[data2$`Jenis Ayam`=="Ayam Petelur",]$`Produksi Daging`<- replace(data2[data2$`Jenis Ayam`=="Ayam Petelur",]$`Produksi Daging`,
                                                                       is.na(data2[data2$`Jenis Ayam`=="Ayam Petelur",]$`Produksi Daging`), fixedayampet)

#changing missing value for itik (daging)
sum(is.na(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Daging`))
fixeddagingitik <- mean(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Daging`, na.rm = T)
data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Daging` <- replace(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Daging`,
                                                                                is.na(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Daging`), fixeddagingitik)

#changing missing value for itik (telur)
sum(is.na(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Telur`))
fixedteluritik <- mean(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Telur`, na.rm = T)
data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Telur` <- replace(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Telur`,
                                                                                is.na(data2[data2$`Jenis Ayam`=="Itik dan Itik Manila",]$`Produksi Telur`), fixedteluritik)

sum(is.na(data2))

#DENSITY PLOT
data20202 = data2[data2$Tahun==2020,]
ggplot(data20202, aes(x = `Produksi Daging`, color = `Jenis Ayam`, fill = `Jenis Ayam`)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Produksi Daging Unggas",
       subtitle = "Based on species") +
  xlim(0,4000) +
  scale_fill_brewer(palette = "Set2") + 
  scale_color_brewer(palette = "Set2") + 
  facet_grid(rows = vars(`Jenis Ayam`))

sum(is.na(data1$`Produksi Daging Sapi`))
sum(is.na(data1$`Produksi Daging Kambing`))
sum(is.na(data1$`Produksi Daging Domba`))
sum(is.na(data1$`Produksi Daging Ayam Kampung`))
sum(is.na(data1$`Produksi Daging Ayam Petelur`))
sum(is.na(data1$`Produksi Daging Ayam Pedaging`))
sum(is.na(data1$`Produksi Daging Itik dan Itik Manila`))
sum(is.na(data1$`Produksi Telur Ayam Kampung`))
sum(is.na(data1$`Produksi Telur Ayam Petelur`))
sum(is.na(data1$`Produksi Telur Itik dan Itik Manila`))
sum(is.na(data1$`Produksi Susu Sapi Perah`))

#na : daging domba, daging ayam kampung, daging ayam petelur, daging ayam pedaging
#na : daging itik dan itik manila, telur itik dan itik manila, susu sapi perah

#changing missing value for daging domba
dombamean <- mean(data1$`Produksi Daging Domba`, na.rm = T)
data1$`Produksi Daging Domba` <- replace(data1$`Produksi Daging Domba`, 
                                  is.na(data1$`Produksi Daging Domba`), dombamean)
data1$`Produksi Daging Domba`
View(data1)
sum(is.na(data1))

#changing missing value for daging ayam kampung
akmean <- mean(data1$`Produksi Daging Ayam Kampung`, na.rm = T)
data1$`Produksi Daging Ayam Kampung` <- replace(data1$`Produksi Daging Ayam Kampung`,
                                                is.na(data1$`Produksi Daging Ayam Kampung`), akmean)
data1$`Produksi Daging Ayam Kampung`

#changing missing value for daging ayam petelur
apmean <- mean(data1$`Produksi Daging Ayam Petelur`, na.rm = T)
data1$`Produksi Daging Ayam Petelur` <- replace(data1$`Produksi Daging Ayam Petelur`,
                                                is.na(data1$`Produksi Daging Ayam Petelur`), apmean)

#changing missing value for daging ayam pedaging
aydag <- mean(data1$`Produksi Daging Ayam Pedaging`, na.rm = T)
data1$`Produksi Daging Ayam Pedaging` <- replace(data1$`Produksi Daging Ayam Pedaging`,
                                                 is.na(data1$`Produksi Daging Ayam Pedaging`), aydag)

#changing missing value for daging itik dan itik manila
itikmean <- mean(data1$`Produksi Daging Itik dan Itik Manila`, na.rm = T)
data1$`Produksi Daging Itik dan Itik Manila` <- replace(data1$`Produksi Daging Itik dan Itik Manila`,
                                                        is.na(data1$`Produksi Daging Itik dan Itik Manila`), itikmean)

#changing missing value for telur itik dan itik manila
teluritikmean <- mean(data1$`Produksi Telur Itik dan Itik Manila`, na.rm = T)
data1$`Produksi Telur Itik dan Itik Manila` <- replace(data1$`Produksi Telur Itik dan Itik Manila`,
                                                       is.na(data1$`Produksi Telur Itik dan Itik Manila`), teluritikmean)

#changing missing value for susu sapi perah
meansapi <- mean(data1$`Produksi Susu Sapi Perah`, na.rm = T)
data1$`Produksi Susu Sapi Perah` <- replace(data1$`Produksi Susu Sapi Perah`,
                                            is.na(data1$`Produksi Susu Sapi Perah`), meansapi)

#HISTOGRAM
data2020 = data1[data1$Tahun==2020,]
data2020

par(mfrow = c(3,1))
options(scipen = 999)
hist(data2020$`Produksi Daging Domba`,
     xlab = "Produksi Daging Domba",
     col = "#c8b6ff",
     main = "Produksi Daging Domba")
hist(data2020$`Produksi Daging Kambing`,
     xlab = "Produksi Daging Kambing",
     col = "#b8c0ff",
     main = "Produksi Daging Kambing")
hist(data2020$`Produksi Daging Sapi`,
     xlab = "Produksi Daging Sapi",
     col = "#bbd0ff",
     main = "Produksi Daging Sapi")

#taking some of the highest value
data21forsusu = data1[data1$Tahun==2021,]
ordered <- data21forsusu[order(-data21forsusu$`Produksi Susu Sapi Perah`, na.last=TRUE),]
top10 <- ordered[1:10,]
View(top10)

#BAR CHART
ggplot(data=top10, aes(x=`Kota/Kabupaten`, y=`Produksi Susu Sapi Perah`)) +
  geom_bar(stat="identity", fill = c("#f94144","#f3722c","#f8961e","#f9844a","#f9c74f",
                                     "#90be6d","#43aa8b","#4d908e","#577590","#277da1"))+
  labs(title = "Top 10 Cities In East Java In Producing Milk (In Thousand)",
       caption = "Source : BPS Jawa Timur") +
  geom_text(aes(label=round(`Produksi Susu Sapi Perah`/1000, digits = 2)), vjust=-0.3, size=3.5, color = "black")+
  scale_x_discrete(limits=c("Pasuruan", "Malang", "Tulungagung", "Blitar",
                            "Batu", "Kediri", "Sampang", "Sumenep",
                            "Mojokerto Kota", "Lumajang"))

data2
#REGRESSION
data2trial3 = data2[,4:6]
head(data2trial3)

fixed231 <- mean(data2trial3$`Produksi Daging`, na.rm = T)
data2trial3$`Produksi Daging`<- replace(data2trial3$`Produksi Daging`,
                                        is.na(data2trial3$`Produksi Daging`), fixed231)
fixed232 <- mean(data2trial3$`Produksi Telur`, na.rm = T)
data2trial3$`Produksi Telur`<- replace(data2trial3$`Produksi Telur`,
                                        is.na(data2trial3$`Produksi Telur`), fixed232)
fixed233 <- mean(data2trial3$Populasi, na.rm = T)
data2trial3$Populasi<- replace(data2trial3$Populasi,
                                        is.na(data2trial3$Populasi), fixed233)

install.packages("corrplot")
par(mfrow=c(1,1))
korelasi = cor(data2trial3)
korelasi
corrplot(korelasi, method = "number")
sum(is.na(data2trial3))


par(mfrow=c(1,2))
plot(data2$Populasi, data2$`Produksi Telur`,
     xlab = "Populasi",
     ylab = "Produksi Telur",
     main = "Scatter Plot Regresi Produksi Telur vs Populasi")
linearpopegg <- lm(`Produksi Telur` ~ Populasi, data = data2)
summary(linearpopegg)
names(linearpopegg)
abline(linearpopegg, lty = 2, lwd = 2)
linearpopegg$coefficients


pred <- predict(linearpopegg)
res <- resid(linearpopegg)
sres <-rstandard(linearpopegg)

plot(pred,sres, 
     xlab = "predicted values",
     ylab = "standarized residuals", 
     ylim = c(-3,3),
     main = "Scatter Plot Predicted Values vs Standardized Residuals")
abline(h = 0, lty = 2)
abline(h = 2, lty = 2, col = "blue")
abline(h = -2, lty = 2, col = "blue")