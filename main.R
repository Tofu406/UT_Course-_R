# Gold Price analysis 

##load reference
library(dplyr)
library(ggplot2)
library(TTR)
library(quantmod)

## Variables
DataPath <- 'gold.csv'

## Check environment
result <- c(CurrentDirectory=getwd(),IsDataExist=file.exists(DataPath))
result

## load data form csv file.
goldPrice <- read.csv("gold.csv", sep = ",", header = T)
goldPrice$Date <- as.Date(goldPrice$Date)
class(goldPrice$Date)

goldPrice$ma20=ma(goldPrice$Close,order = 20)
goldPrice$ma60=ma(goldPrice$Close,order = 60)
goldPrice$ma120=ma(goldPrice$Close,order = 120)



##ggplot2

###收盤價
p1 <- ggplot(goldPrice, aes(x=as.Date(Date), y=Close)) +
  geom_line() + 
  labs(x = 'Time',y='Close Price') + 
  scale_x_date(date_breaks = "2 years",date_labels=c("%b %y"))

###交易量
p1 <- p1 + geom_bar(stat = 'identity',aes(y=Volume/1000,fill='Volume'))

### 20 60 120 日均線
p1 <- p1 + 
  geom_line(aes(y=ma20,colour='ma20'),size=0.8) + 
  geom_line(aes(y=ma60,colour='ma60'),size=0.8) + 
  geom_line(aes(y=ma120,colour='ma120'),size=0.8) 

### 加上 標題 & 顏色
p1 <- p1 + 
  labs(title='Gold Price Moving Average') +
  scale_colour_manual(values = c('blue','purple','red','black'))

p1

### is rise or fall (0:fall,1:rise)
vec <- c()
for (i in c(1:length(goldPrice$Close))) {
  if(goldPrice$Open[i] >= goldPrice$Close[i]){
    group = 0
  }else{
    group = 1
  }
  vec <- c(vec,group)
}
goldPrice$Group <- vec

#TODO:

##quantmod
GoldPrice <- as.xts(read.zoo(goldPrice,header=T))

periodicity(GoldPrice) 
nquarters(GoldPrice)
nmonths(GoldPrice)
ndays(GoldPrice)

ma_20 <- runMean(GoldPrice[,5],n=30)
ma_60 <- runMean(GoldPrice[,5],n=60)
ma_120 <- runMean(GoldPrice[,5],n=120)

### 畫圖 (資料範圍 : 2020-01-01 - 2022-01-01)
chartSeries(GoldPrice["2022-01-01::2022-09-01"]
            ,theme = 'white'
            )
GoldPrice["2022-01-01::2022-09-01"]
### 加上 20 60 120 日均線
chartSeries(GoldPrice["2000-01-01::2022-09-01"]
            ,theme = 'white'
            ,TA=c(
              addEMA(n=20,col='red'),     #20日均線
              addEMA(n=60,col='blue'),    #60日均線
              addEMA(n=120,col='purple'), #120日均線
              addVo()                     #交易量
              )
            )


## 
m <- auto.arima(ts(goldPrice$Close),seasonal = TRUE)
library(forecast)
plot(forecast(m,h=600))

#TODO: 預測 ...
