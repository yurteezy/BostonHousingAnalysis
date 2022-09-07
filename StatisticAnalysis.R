#install.packages("mlbench")
library(mlbench)
#setwd("")
source("Boston_regions.R")
data(BostonHousing2)
options(warn=-1)
mujSeed <- 200023

set.seed(mujSeed)
data <- base::merge(BostonHousing2,regs)[sample(1:nrow(BostonHousing2),200),] 
# ukazka postupu
popRozptyl <- function(x) mean((x-mean(x))^2)
popSmOdch  <- function(x) sqrt(popRozptyl(x))
sikmost_moment <- function(x) mean((x - mean(x))**3)/popSmOdch(x)**3
spicatost_moment <- function(x) mean((x - mean(x))**4)/popRozptyl(x)**2

{
prumer<-na.exclude(sapply(data,mean))
dec <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.10),NA)))
dk <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.25),NA)))
mn <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.5),NA)))
vk <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.75),NA)))
devet <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.99),NA)))
so <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),sd(x),NA)))
vr <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),max(x) - min(x),NA)))
kr <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.75) - quantile(x, 0.25),NA)))
vark <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),(sd(x)/mean(x))*100,NA)))
dpom <- na.exclude(sapply(data,function(x) ifelse(is.numeric(x),quantile(x,0.90)/quantile(x, 0.10),NA)))
sikm <- na.exclude(sapply(data,sikmost_moment))
spim <- na.exclude(sapply(data,spicatost_moment))
}
popis<-data.frame(prumer,dec,dk,mn,vk,devet,so,vr,kr,vark,dpom,sikm,spim)
colnames(popis) <- c("prumer","1. decil","dolni kvartil","median","3 kvartil", "99% kvantil",
                     "sm. odchylka","variacni rozpeti","kvartilove rozpeti",
                     "variacni koeficient","decilovy pomer","momentovy koeficient sikmosti",
                     "momentovy koeficient spicatosti")
popis
cat("Pro nalezeni dvou nejvice variabilni promenne budeme se koukat na nejvetsi hodnoty variacnich koeficientu. 
Dve nejvetsi jsou: crim a zm..
Variacni koeficient je vhodny pro srovanani souboru s podstatne odlisnou urovni hodnot.Proto jsem ho vybrala")

#Druha uloha

tabulka <- data.frame(row.names = names(table(cut(data$age,10))))
tabulka[,1] <- c(table(cut(data$age,10)))
tabulka[,2] <- prop.table(tabulka[,1])
tabulka[,3] <- cumsum(tabulka[,1])
tabulka[,4] <- cumsum(tabulka[,2])
colnames(tabulka) <- c("absolutni","relativni","kumulativni","rel.kumulativni")
tabulka
cat("Rozdelila jsem sloupec age na 10 intervalu pro lepsi rozdeleni hodnot.
Nejvyssi pocet lidi je ve vekovem intervalu vice nez 90 let. Jejich absolutni pocet je 75, celkovy podil 37.5%.
Celkovy pocet lidi ve zkoumanem souboru je 200. Podil spolu se predchozimi hodnotami je 100%")

#Treti uloha

set.seed(mujSeed)
nahodni_vyber <-matrix(sample(data$medv,20*30,T),nrow = 20)
u.9   <- qnorm(.9)                     
spoleh <- data.frame()
sh <- apply(nahodni_vyber,1, function(x) mean(x)-u.9*sd(x)/sqrt(length(x)))
stred <- apply(nahodni_vyber,1, mean)
hh <- apply(nahodni_vyber,1, function(x) mean(x)+u.9*sd(x)/sqrt(length(x)))
pp <- ifelse((mean(data$medv) >= sh & mean(data$medv) <= hh), pp <- 1,pp <- 0)
spoleh<-(cbind(sh,stred,hh,pp))
colnames(spoleh) <- c("SH","PRUMER","HH","obsahuje_pp")
spoleh
cat("Pocet intervalu obsahujicich populacni prumer je 18, coz uplne splnuje moje ocekavani,
ve dvou pripadech bych ocekavala nuly, protoze mame pravdepodobnost 90%. 20 * 0.9 = 18")
#Ctvrta uloha

#install.packages("corrplot")
library(corrplot)
nums <- unlist(lapply(data, is.numeric)) 
k <- cor(dplyr::select_if(data, is.numeric))
corrplot(k, method = "square")
cat("Hodnota jedna znamena, ze tato korelace je mezi stejnym znakem. 
Kladna hodnota korelace znamena, ze se zvysovanim hodnot jednoho znaku se zvysuji i hodnoty druheho znaku.
S zapornimy cisly je naopak")

#Pata uloha
library(ggplot2)
ggplot(data,aes(x=age))+
  geom_histogram(breaks=seq(0, 100, by=10), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age", x="Age", y="Count") +
  ylim(c(0,90))

ggplot(data,aes(x=tract))+
  geom_density(color = "green", fill = "pink") + 
  geom_vline(aes(xintercept = mean(tract)), color = "blue", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(tract)), color = "red", linetype = 4, size = 1)+
  ggtitle("Density plot for $tract")



ggplot(data,aes(x=region))+
  ggtitle("Pocet nemovitosti podle regionu")+
  geom_bar(stat = ,fill="lightblue")+
  ylim(c(0,85))+
  geom_text(aes(label =..count..), stat = "count", vjust = -1, colour = "black",size = 6)+
  theme_minimal()
              

library(dplyr)
library(hrbrthemes)
library(viridis)    

data %>%
  ggplot(aes(x=region, y=age,fill=region)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.6) +
  scale_fill_viridis(discrete = TRUE) +
  ylim(c(0,110))+
  theme_modern_rc()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )+
  ggtitle("Variabilita age podle regionu")


data %>%
  ggplot(aes(x=region, y=tax,fill=region)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="white", alpha=0.6) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ft_rc()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )+
  ggtitle("Variabilita tax podle regionu")


data %>%
  ggplot(aes(x=region, y=rm,fill=region)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="red", alpha=0.6) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum_rc()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )+
  ggtitle("Variabilita rm podle regionu")

  
cat("Za nejvhodnejsi zpusob tady pocitam violin plot, ktery ukazuje hustotu a vsechny ukazatele boxplotu(median,mean,kvartily atd.) ")

