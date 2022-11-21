## load reference
library(dplyr)
library(lubridate)
library(ggplot2)
library(graphics)

## load data form csv file.
goldPrice <- read.csv("gold.csv", sep = ",", header = T)
goldPrice$Date <- as.Date(goldPrice$Date)

# 平均交易量直方圖
Draw_VolumeBar <- function(data){
  p_month_mean_volume <- ggplot(data,aes(x=month,y=avg_vol)) +
    geom_bar(stat = 'identity',aes(y=avg_vol,fill='avg_vol')) +
    labs(x = 'Date',y='Volume') 
  p_month_mean_volume
}

# 平均資料數直方圖
Draw_DataCountBar <- function(data){
  p_month_mean_DataCount <- ggplot(data,aes(x=month,y=data_cnt)) +
    geom_bar(stat = 'identity',aes(y=data_cnt,fill='data_cnt')) +
    labs(x = 'Date',y='Count') 
  p_month_mean_DataCount
}

# 平均 開盤 & 收盤價格
Draw_OpenCloseHighLow <- function(data){
  p <- ggplot(data,aes(x=month,y=avg_close)) +
    geom_line(color = "#ff6b6b",size = 5) +
    geom_line(aes(x=month,y=avg_open),size=3, color = "#292F36") +  
    geom_line(aes(x=month,y=avg_High),size=1, color = "#b185a7") + 
    geom_line(aes(x=month,y=avg_Low),size=0.5, color = "#E8DBC5") 
  p  
}

# 資料簡易描述

## 計算資料跨越幾年
DataYears <- unique(format(as.Date(goldPrice$Date, format="%d/%m/%Y"),"%Y"))
## 計算資料跨越幾個月
DataYearsMonths <- unique(format(as.Date(goldPrice$Date, format="%d/%m/%Y"),"%Y%m"))

print(paste('資料共跨',length(DataYears),'年'))
print(paste('資料共跨',length(DataYearsMonths),'月'))
print(paste('在',difftime(max(goldPrice$Date),min(goldPrice$Date)),'天中,有',nrow(goldPrice),'天有交易'))
print(paste('資料共',nrow(goldPrice),'筆'))
print(paste('資料共',ncol(goldPrice),'欄位'))
colnames(goldPrice)

#最高價日期
max(goldPrice$High)
subset(goldPrice,High == max(goldPrice$High))$Date

#最低價日期
min(goldPrice$Low)
subset(goldPrice,Low == min(goldPrice$Low))$Date

#加入星期幾的資料
goldPrice$wd <- wday(goldPrice$Date)

wd_count <- data.frame(
  group = c('星期日', '星期一', '星期二', '星期三', '星期四', '星期五', '星期六'),
  value = c(
    nrow(subset(goldPrice, wd == 1)),
    nrow(subset(goldPrice, wd == 2)),
    nrow(subset(goldPrice, wd == 3)),
    nrow(subset(goldPrice, wd == 4)),
    nrow(subset(goldPrice, wd == 5)),
    nrow(subset(goldPrice, wd == 6)),
    nrow(subset(goldPrice, wd == 7))
  )
)
wd_count

wd_vol_count <- data.frame(
  group = c('星期日', '星期一', '星期二', '星期三', '星期四', '星期五', '星期六'),
  value = c(
    sum(subset(goldPrice, wd == 1)$Volume)/1000,
    sum(subset(goldPrice, wd == 2)$Volume)/1000,
    sum(subset(goldPrice, wd == 3)$Volume)/1000,
    sum(subset(goldPrice, wd == 4)$Volume)/1000,
    sum(subset(goldPrice, wd == 5)$Volume)/1000,
    sum(subset(goldPrice, wd == 6)$Volume)/1000,
    sum(subset(goldPrice, wd == 7)$Volume)/1000
  )
)
wd_vol_count

## 交易日分布
p <- ggplot(wd_count,aes(x=group,y=value)) +
  geom_bar(stat = 'identity',aes(y=value,fill='value')) +
  geom_text(aes(label = value), vjust = -0.3, size = 5.5) +
  labs(x = 'week day',y='times') 
p

## 交易量分布按星期
p <- ggplot(wd_vol_count,aes(x=group,y=value)) +
  geom_bar(stat = 'identity',aes(y=value,fill='value')) +
  geom_text(aes(label = value), vjust = -0.3, size = 5.5) +
  labs(x = 'week day',y='weight(kg)') 
p

vec <- c()
upCOunt <- 0
downCOunt <- 0
for (i in c(1:length(goldPrice$Close))) {
  if(goldPrice$Open[i] >= goldPrice$Close[i]){
    group = 0
    upCOunt <- upCOunt+1
  }else{
    group = 1
    downCOunt <- downCOunt+1
  }
  vec <- c(vec,group)
}
goldPrice$Group <- vec

updown <- data.frame(
  group = c('up','down'),
  value = c(upCOunt,downCOunt)
  ) 

## 漲跌次數比較
print(paste('上漲',upCOunt,'次',',佔',round(upCOunt/nrow(goldPrice)*100, 4),'%'))
print(paste('下跌',downCOunt,'次',',佔',round(downCOunt/nrow(goldPrice)*100, 4),'%'))

## 漲跌次數直方圖
p <- ggplot(updown,aes(x=group,y=value)) +
  geom_bar(stat = 'identity',aes(y=value,fill='value')) +
  geom_text(aes(label = value), vjust = -0.3, size = 5.5) +
  labs(x = 'up or down',y='times') 
p

wd_updown_count <- data.frame(
  group = c('星期日', '星期一', '星期二', '星期三', '星期四', '星期五', '星期六'),
  value = c(
    round(nrow(subset(
      goldPrice, wd == 1 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 1)), 4),
    round(nrow(subset(
      goldPrice, wd == 2 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 2)), 4),
    round(nrow(subset(
      goldPrice, wd == 3 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 3)), 4),
    round(nrow(subset(
      goldPrice, wd == 4 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 4)), 4),
    round(nrow(subset(
      goldPrice, wd == 5 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 5)), 4),
    round(nrow(subset(
      goldPrice, wd == 6 &
        Group == 0
    )) / nrow(subset(goldPrice, wd == 6)), 4),
    round(nrow(subset(
      goldPrice, wd == 7 & Group == 0
    )) / nrow(subset(goldPrice, wd == 7)), 4)
  )
)

wd_updown_count

## 勝率分布按星期
p <- ggplot(wd_updown_count,aes(x=group,y=value)) +
  geom_bar(stat = 'identity',aes(y=value,fill='value')) +
  geom_text(aes(label = value), vjust = -0.3, size = 5.5) +
  labs(x = 'week day',y='times') 
p


goldPrice$year <- strftime(goldPrice$Date,"%Y")
goldPrice$month <- strftime(goldPrice$Date,"%m")

# 月
gp_m <- goldPrice %>%
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarise(avg_open = mean(Open),
            avg_close = mean(Close),
            avg_High = mean(High),
            avg_Low = mean(Low),
            avg_vol = mean(Volume),
            data_cnt = n()
            )
gp_m

## 月平均交易量直方圖
Draw_VolumeBar(gp_m)

## 月平均資料數直方圖
Draw_DataCountBar(gp_m)

## 月平均 開盤 & 收盤 & 最高 & 最低價格
Draw_OpenCloseHighLow(gp_m)

gp_m$dm <- format(as.Date(gp_m$month, format="%d/%m/%Y"),"%m")
gp_m$dy <- format(as.Date(gp_m$month, format="%d/%m/%Y"),"%Y")

#季
gp_q <- goldPrice %>%
  group_by(month = lubridate::floor_date(Date, 'quantor')) %>%
  summarise(avg_open = mean(Open),
            avg_close = mean(Close),
            avg_High = mean(High),
            avg_Low = mean(Low),
            avg_vol = mean(Volume),
            data_cnt = n()
  )
gp_q

## 季平均交易量直方圖
Draw_VolumeBar(gp_q)

## 季平均資料數直方圖
Draw_DataCountBar(gp_q)

## 季平均 開盤 & 收盤 & 最高 & 最低價格
Draw_OpenCloseHighLow(gp_q)

#年
gp_y <- goldPrice %>%
  group_by(month = lubridate::floor_date(Date, 'year')) %>%
  summarise(avg_open = mean(Open),
            avg_close = mean(Close),
            avg_High = mean(High),
            avg_Low = mean(Low),
            avg_vol = mean(Volume),
            data_cnt = n()
  )
gp_y

## 年平均交易量直方圖
Draw_VolumeBar(gp_y)

## 年平均資料數直方圖
Draw_DataCountBar(gp_y)

## 年平均 開盤 & 收盤 & 最高 & 最低價格
Draw_OpenCloseHighLow(gp_y)


##同期整理
for (y in DataYears) {
    assign(paste0('b',y),subset(gp_m,y==dy)) 
}

gp_m$quantor <- substr(quarters(as.Date(gp_m$month)), 2, 2)

#同期平均交易量
quantor_vol_count <- data.frame(
  type=c('Q1','Q2','Q3','Q4'),
  value=c(mean(subset(gp_m,quantor==2)$avg_vol),
          mean(subset(gp_m,quantor==3)$avg_vol),
          mean(subset(gp_m,quantor==4)$avg_vol),
          mean(subset(gp_m,quantor==1)$avg_vol))
  )

p <- ggplot(data = quantor_vol_count, aes(x = type, y = value)) +
  geom_bar(stat = "identity", position = position_dodge())
p

#同期平均收盤價
quantor_close <- data.frame(
  type=c('Q1','Q2','Q3','Q4'),
  value=c(mean(subset(gp_m,quantor==2)$avg_close),
          mean(subset(gp_m,quantor==3)$avg_close),
          mean(subset(gp_m,quantor==4)$avg_close),
          mean(subset(gp_m,quantor==1)$avg_close))
)

p <- ggplot(data = quantor_close, aes(x = type, y = value)) +
  geom_bar(stat = "identity", position = position_dodge())
p


