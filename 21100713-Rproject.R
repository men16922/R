
install.packages("pastecs")
install.packages("psych")
install.packages("gmodels")
install.packages("nortest")
install.packages("choroplethr")
install.packages("plotly") 
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")
install.packages("plyr")
install.packages("vcd")
install.packages("grid")
install.packages("MASS")
install.packages("vioplot")
install.packages("rJava")
install.packages("memoise")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("stringr")
install.packages("glue")
install.packages("tm")
install.packages("tidyverse")
install.packages("textdata")
install.packages("maps")
install.packages("ggiraphExtra")
install.packages("mapproj")
install.packages("choroplethr")
install.packages("mapdata")
install.packages("fmsb")
install.packages("doBy")
install.packages("Hmisc")
install.packages("SnowballC")
# 사용한 패키지


library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(reshape)
library(vcd)
library(grid)
library(MASS)
library(vioplot)
library(gmodels)
library(nortest)
library(rJava)
library(memoise)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(twiiter)
library(stringr)
library(glue)
library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(maps)
library(ggiraphExtra)
library(mapproj)
library(choroplethr)
library(mapdata)
library(plotly)
library(fmsb)
library(doBy)
library(SnowballC)


#사용한 라이브러리

#파일 읽기
mydata<- read.csv("10000-Sales-Records/10000 Sales Records.csv",stringsAsFactor = TRUE)
#출처 http://eforexcel.com/wp/downloads-18-sample-csv-files-data-sets-for-testing-sales/
# 분석에 사용한 data file: EforExcel에서 지원하는 sales record 파일(10000)
# 수집 방법: 인터넷 검색을 통해 R 분석에 적합한 엑셀 파일을 다운로드 받음
# 분석의 목적: 지역, 나라, 날짜 등의 여러 변수와 Item Sold 간의 상관 관계 파악
# Item 판매에 영향을 주는 변수들은 무엇이 있는지 분석
# Item 판매에 직접적인 영향을 주는 것으로 보이는 요소가 있다면, test를 통해서 가설을 검증
# 이를 통해서 매출을 올리는 방법을 찾을 수 있을 것으로 기대함


mydata$Ship.Date <-mdy(mydata$Ship.Date) #interval scale
mydata$Order.Date <-mdy(mydata$Order.Date) #interval scale
mydata$day <-as.factor(day(mydata$Ship.Date)) #nominal scale
mydata$month <-as.factor(month(mydata$Ship.Date)) #nominal scale
mydata$year <-as.factor(year(mydata$Ship.Date)) #nominal scale
mydata$dayofweek <-as.factor(wday(mydata$Ship.Date)) #nominal scale
save(mydata, file="mydata.RData")
load("mydata.RData")
View(mydata)
#R Data 저장


#mydata 결측값 찾기
table(is.na(mydata)) #결측값 없음

#boxplot을 통해 mydata 이상값 확인
boxplot(mydata$Total.Revenue)
boxplot(mydata$Units.Sold)
boxplot(mydata$Unit.Price)
boxplot(mydata$Total.Cost)
boxplot(mydata$Total.Profit)

#이상값 찾기. Revenue와 Profit의 경우는 data상 값 편차가 큰 것으로 해석되어, outlier example 에서만 극단치를 결측값으로 대체
boxplot(mydata$Total.Revenue)$stats
outlier <-mydata
outlier$Total.Revenue <- ifelse(mydata$Total.Revenue<167.94 | mydata$Total.Revenue>4113870.12,NA, outlier$Total.Revenue)
table(is.na(outlier)) #NA값이 들어간 것 확인

#결측값 대표값(평균)으로 대체
outlier$Total.Revenue <- ifelse(is.na(outlier$Total.Revenue), mean(outlier$Total.Revenue,na.rm=T), outlier$Total.Revenue)
table(is.na(outlier))

#mydata 앞에서부터 10행까지 출력
head(mydata, 10)
#mydata 뒤에서 10행까지 출력
tail(mydata,10)
#mydata 행,열 출력
dim(mydata)
#mydata 속성 확인
str(mydata)
# Region, Country, Type, Channel, Priority, day, month, year, dayofweek, ID는 nominal scale
# Order.Date, Ship.Date는 interval scale
# Unit.Price, Units.Sold, Unit.Cost, Total.Revenue, Total.Cost, Total.Profit 은 ratio sclae

#mydata 요약 통계량 출력
summary(mydata)
names(mydata)
#변수명 수정
mydata <- reshape::rename(mydata, c(Item.Type = "Type", Sales.Channel="Channel", Order.Priority="Priority", Order.ID="ID"))
View(mydata)

#Item Type별 빈도표 출력, 어떤 Item Type이 많이 거래되었는지 확인
table(mydata$Type)
#막대 그래프
qplot(mydata$Type)

#Item Type 별 profit 표
Unit_profit <-mydata %>%  group_by(Type) %>% dplyr::summarize(profit=mean(Unit.Price-Unit.Cost)) #ratio scale
Unit_profit

#Item Type 별 profit 산점도,선,막대그래프,
ggplot(data=Unit_profit, aes(x=reorder(Type,-profit), y=profit))+geom_point() +ggtitle("Type and profit")
ggplot(data=Unit_profit, aes(x=reorder(Type,-profit), y=profit,group=1))+geom_line()
bb<-ggplot(data=Unit_profit, aes(x=reorder(Type,-profit), y=profit))+geom_col(fill="lightblue", color="black") +ggtitle("Type and profit")
ggplotly(bb)
#Type별로 Profit의 편차가 큰 것을 확인


#Type별 평균 Order부터 Ship까지 걸리는 시간, interval scale
E_date <- mydata %>% group_by(Type) %>% dplyr::summarize(Elapsed_Date=mean(as.numeric(as.Date(Ship.Date, format='%Y-%m-%d')-as.Date(Order.Date, format='%Y-%m-%d'))))
ss<-ggplot(data=E_date, aes(x=reorder(Type,-Elapsed_Date), y=Elapsed_Date))+geom_col(fill="orange", color="black") +ggtitle("Type and Elapsed Date")
ggplotly(ss)
#Vegetable이 가장 적게, Snack이 가장 오래 시간이 걸린다

#Region 별 평균 Order부터 Ship 까지 걸리는 시간, interval scale
E_date2 <- mydata %>% group_by(Region) %>% dplyr::summarize(Elapsed_Date=mean(as.numeric(as.Date(Ship.Date, format='%Y-%m-%d')-as.Date(Order.Date, format='%Y-%m-%d'))))
ggplot(data=E_date2, aes(x=reorder(Region,-Elapsed_Date), y=Elapsed_Date))+geom_col() +ggtitle("Region and Elapsed Date")
#큰 차이는 없으나, 중동,아프리카 지역이 가장 빠르며 중앙아메리카가 가장 느리다

#지역별로 Total revenue 순위 내기
#어느 지역에서 가장 많은 총매출을 올리는지 확인
#합계로 막대그래프 그리기
total_revenue <- mydata %>%  filter( !is.na(Total.Revenue)) %>% group_by(Region) %>% dplyr::summarize(total=sum(Total.Revenue,na.rm=T)) %>% arrange(desc(total))
total_revenue #Region은 nominal, total은 ratio scale
re<-ggplot(data=total_revenue,aes(reorder(Region,total,sum),total)) +geom_bar(stat="identity", fill= "skyblue", color="dark blue") + ggtitle("Total Revenue bar graph by Region")+
  scale_y_continuous(labels=comma)
ggplotly(re)
#유럽, 아프리카에서 가장 높은 총매출, 북아메리카에서 총매출이 가장 낮음

#합계로 지역별 순위표와 grade, 조건문 활용
rank <-1:nrow(total_revenue)
df <-data.frame(rank)
df
total_revenue <-cbind(total_revenue,df)
y <-quantile(total_revenue$total, c(0.8,0.6,0.4,0.2))
y
# grade가 높을수록 총매출에서 중요도가 높은 지역이라고 볼 수 있음
total_revenue$grade<-ifelse(total_revenue$total>=y[1],'A',ifelse(total_revenue$total>=y[2],'B',ifelse(total_revenue$total>=y[3],'C',ifelse(total_revenue$total>=y[4],'D','E'))))
total_revenue  #grade는 ordinal scale

#평균매출으로 막대그래프 그리기
total_revenue_average <- mydata %>% filter( !is.na(Total.Revenue)) %>% group_by(Region) %>% dplyr::summarize(mean=mean(Total.Revenue,na.rm=T)) %>% arrange(desc(mean))
total_revenue_average #mean은 ratio scale
wq<-ggplot(data=total_revenue_average,aes(reorder(Region,mean,sum),mean)) +geom_bar(stat="identity", fill= "red", color="dark red") + ggtitle("Mean Revenue bar graph by Region") +
  scale_y_continuous(labels=comma)
ggplotly(wq)
#반대로, 평균매출은 북아메리카가 가장 높고 아프리카가 가장 낮다

#평균 매출으로 지역별 순위표와 grade, 조건문 활용
total_revenue_average <-cbind(total_revenue_average,df)
y <-quantile(total_revenue_average$mean, c(0.8,0.6,0.4,0.2))
y
#
total_revenue_average$grade<-ifelse(total_revenue_average$mean>=y[1],'A',ifelse(total_revenue_average$mean>=y[2],'B',ifelse(total_revenue_average$mean>=y[3],'C',ifelse(total_revenue_average$mean>=y[4],'D','E'))))
total_revenue_average #grade는 ordinal scale
# grade가 높을수록 평균매출에서 중요한 지역이라고 볼 수 있음

#합계와 평균 그래프 합치기, 이중 그래프

final_revenue <- merge(total_revenue,total_revenue_average,by="Region")
final_revenue

#선 그래프가 total/1000, 막대 그래프가 mean, 왼쪽 축이 총매출/1000, 오른쪽 축이 평균
vc<-ggplot(data=final_revenue,aes(x=Region,group=1)) +geom_line(aes(y=total/1000), colour="red", size=2) +geom_bar(aes(y=mean),fill="blue", stat="identity") + 
  scale_y_continuous(sec.axis=sec_axis(~.*(0.5),name="mean",labels=comma),labels=comma)
ggplotly(vc)
#총매출과 평균매출 간에 그래프상으로 유의미한 상관관계가 발견되지 않음

#지역별 평균 profit을 type별로 누적막대그래프
region_profit <- mydata %>% group_by(Region,Type) %>% dplyr::summarize(mean_profit=mean(Total.Profit,na.rm=T))
region_profit #mean_profit은 ratio scale, Region, Type은 nominal scale
c<-ggplot(region_profit, aes(x=Region, y=mean_profit, fill=Type))+geom_bar(stat="identity") +
  scale_y_continuous(labels=comma) +ggtitle("Type and mean_profit by Region")
ggplotly(c) #interactive graph

#지역별 평균 profit을 Channel별로 누적막대그래프
channel_profit <- mydata %>% group_by(Region,Channel) %>% dplyr::summarize(mean_profit=mean(Total.Profit,na.rm=T))
channel_profit #Region, Channel은 nominal, mean_profit은 ratio scale
d <-ggplot(channel_profit, aes(x=Region, y=mean_profit, fill=Channel))+geom_bar(stat="identity") +
  scale_y_continuous(labels=comma) +ggtitle("Channel and mean_profit by Region")
ggplotly(d) #interactive graph
#North America 지역에서 Online 매출의 수치가 높은 것을 확인

# 지역별 Type and mean_profit Pie Charts 
ggplot(region_profit,aes(x="",y=mean_profit, fill=Type)) +facet_grid(facets=.~Region) +
  geom_bar(stat="identity", width=1) +coord_polar(theta="y") + scale_y_continuous(labels=comma) +ggtitle("Mean_profit and Type by Region")

#Price Histogram by Region
diff(range(mydata$Unit.Price))/30
qq<-ggplot(mydata,aes(x=Unit.Price)) + geom_histogram(binwidth=diff(range(mydata$Unit.Price))/30, fill="orange", colour="red") +
  ggtitle("Bindwidth=21.96467; Default, range/30") +facet_grid(Region~.)
ggplotly(qq) #interactive graph
#가격 빈도가 100~200사이에 주로 분포

#Kernel density curve of Unit.Price
f <- ggplot(mydata, aes(x=Unit.Price)) + geom_density(fill = "yellow", colour=NA, alpha=.5) +
  geom_line(stat="density") + expand_limits(y=0) + ggtitle("Kernel Density Curve of Unit.Price")
ggplotly(f) #interactive graph
#가격이 대체로 140, 430, 650 근처에 분포

#Kernel density curve of Unit.Price by Type
a<- ggplot(mydata, aes(x=Unit.Price, colour = Type)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  ggtitle("Kernel Density Curve by Item Type")
ggplotly(a) #interactive graph
# 과일, 음료 종류가 낮은 가격대에 집중적으로 분포

# Histogram + Kernel Density Curve
ggplot(mydata, aes(x=Unit.Price, y=..density..)) + 
  geom_histogram(binwidth=5, fill = "blue", colour="white", alpha=0.5) + 
  geom_density(fill = NA, colour=NA, alpha=0.8) + 
  geom_line(stat="density") + 
  expand_limits(y=0) + 
  ggtitle("Histogram + Kernel Density Curve")


# Boxplot of Unit.Price
h<- ggplot(mydata, aes(x = 1, y = Unit.Price)) +
  geom_boxplot(width=0.8, outlier.size=3, outlier.shape=16, outlier.colour="red") +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Box Plot of Unit.Price")
ggplotly(h) #interactive graph
#가격은 대체적으로 100~400 사이에 분포함

# Boxplot of Unit.Price by Region and mean
ggplot(mydata, aes(x = Region, y = Unit.Price)) +
  geom_boxplot(width=0.8, outlier.size=3, outlier.shape=16, outlier.colour="red") +
  stat_summary(fun.y="mean", geom="point", shape=21, size=3, fill="blue") +
  ggtitle("Box Plot by Region, adding mean")
# 아프리카가 다른 지역에 비해 가격이낮은 데에 분포


# Boxplot of Unit.Price by Region and Priority
i<-ggplot(mydata, aes(x = Region, y = Unit.Price, fill = Priority)) +
  geom_boxplot(width=0.8, outlier.size=3, outlier.shape=16, outlier.colour="red") +
  ggtitle("Box Plot by Region and Prioirty")
ggplotly(i) #interactive graph
#우선순위는 대부분이 M이며, 배송 우선 순위에 따른 가격의 차이는 특별히 발견되지 않음

# Violin Plot of Unit.Pricewith Box Plot by Region, 데이터 분포 확인
p <-ggplot(mydata, aes(x = Region, y = Unit.Price)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, fill="white", outlier.colour=NA) +
  stat_summary(fun.y="median", geom="point", shape=21, size=2, fill="skyblue") + 
  ggtitle("Violin Plot by Region with Box Plot")
ggplotly(p)
#아시아, 유럽, 중동,북아프리카, 북아메리카는 가격 분포가 높고
#아프리카, 중앙아메리카, 오스트레일리아 지역은 가격 분포가 낮다

# Dot plot of Total Profit of Housedhold item in Asia 
Dotdata <-mydata %>% filter(Type=="Household") #Type이 Household인 데이터만
ggplot(Dotdata, aes(x=Region, y=Total.Profit)) + 
  geom_violin(trim = FALSE)+
  geom_dotplot(binaxis='y', stackdir='center', binwidth=diff(range(mydata$Total.Profit))/30) +scale_y_continuous(labels=comma)

# summary of variable in mydata
vars <-c("Units.Sold", "Unit.Price", "Unit.Cost", "Total.Revenue",
         "Total.Cost", "Total.Profit")
#ratio scale인 변수만 가져옴
summary(mydata[vars])
sapply(mydata[vars],mean)

#function

mystats <-function(x, na.omit=FALSE) {
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <-sum((x-m)^3/s^3)/n
  kurt <-sum((x-m)^4/s^4)/n-3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
#
sapply(mydata[vars],mystats)
#여러 가지 분석 방법
#Hmisc
library(Hmisc)
describe(mydata[vars])
#pastecs
library(pastecs)
stat.desc(mydata[vars])
#psych
library(psych)
describe(mydata[vars])

#descriptive statistics by group
aggregate(mydata[vars], by=list(price=mydata$Unit.Price), mean)
aggregate(mydata[vars], by=list(price=mydata$Unit.Price), sd)
describeBy(mydata[vars], mydata$Unit.Price)

#one-way tables of Region Frquency
mytable <-with(mydata, table(Region))
mytable
prop.table(mytable)*100
#유럽, 아프리카가 빈도가 가장 높음


#two-way tables of Region by Priority
mytable2 <-table(mydata$Region,mydata$Priority)
addmargins(prop.table(mytable2))

#CrossTable of Region and  Channel, 채널과 지역의 연관성 알아보기
CrossTable(mydata$Region, mydata$Channel,
           prop.chisq=TRUE,
           chisq=TRUE)

#Correlation 분석
cov(mydata[vars])
cor(mydata[vars])
cor(mydata[vars], method="spearman")

#Hypothesis Test
#Channel(Online,Offline)에 따른 Profit의 차이가 있는가
# frequency distribution table
table(mydata$Channel)
names(mydata)
# summary
with(mydata, tapply(Total.Profit, Channel, summary))
#평균에서 Offline이 약간 높은 것을 확인


#정규성이 만족되는가?
a <- mydata %>% filter(Channel=="Online")
b <- mydata %>% filter(Channel=="Offline")

qqnorm(a$Total.Profit)
qqline(a$Total.Profit)
boxplot(a$Total.Profit)
hist(a$Total.Profit)
mean(a$Total.Profit)
#그래프에서 정규분포 확인되지 않음
#Kolomogorove-Smirnov test, 표본 개수가 5천 이상 1만 이하
lillie.test(a$Total.Profit)
#귀무가설: Total.Profit은 정규분포를 따른다
#검정통계량 D = 0.14842, p-value < 2.2e-16 이므로 귀무가설 기각, 대립가설 정규분포를 만족하지 않음 채택
par=(mar=c(2,2))
qqnorm(b$Total.Profit)
qqline(b$Total.Profit)
boxplot(b$Total.Profit)
hist(b$Total.Profit)
#마찬가지로 그래프에서 정규분포 확인되지 않음
#Kolomogorove-Smirnov test
lillie.test(b$Total.Profit)
#귀무가설: Total.Profit은 정규분포를 따른다
#검정통계량 D = 0.14693, p-value < 2.2e-16 이므로 귀무가설 기각, 대립가설 정규분포를 만족하지 않음 채택
#따라서 t.test를 실시하지 않음
#wilcox test
wilcox.test(Total.Profit ~ Channel, 
            data=mydata, 
            alternative = c("two.sided"), 
            mu = 0, 
            conf.int = FALSE, 
            conf.level = 0.95)
#귀무가설: Channel Online과 Offline간의 Profit 차이는 없다
#검정 결과, p-value는 0.1895로 유의수준 10%에서 귀무가설 채택


# 등분산검정
# 귀무가설 : 두 집단의 분산은 유의미하게 다르다
var.test(a$Total.Profit,b$Total.Profit)
#p-value is 0.0931, 0.0931>0.05 이므로 귀무가설 기각에 실패, 두 집단의 분산은 같다

#결론적으로 Channel 과 Profit간의 유의미한 상관관계 발견하지 못함


#Channel에 따라 Units.Sold에 차이가 있는가?
#정규성 검정
#Kolomogorove-Smirnov test
lillie.test(a$Units.Sold)
#귀무가설: Units.Sold은 정규분포를 따른다
#검정통계량 D = 0.14693, p-value < 2.2e-16 이므로 귀무가설 기각, 대립가설 정규분포를 만족하지 않음 채택
lillie.test(b$Units.Sold)
#귀무가설: Units.Sold은 정규분포를 따른다
#검정통계량 0.062694, p-value < 2.2e-16 이므로 귀무가설 기각, 대립가설 정규분포를 만족하지 않음 채택
#wilcox test
wilcox.test(Units.Sold ~ Channel, 
            data=mydata, 
            alternative = c("two.sided"), 
            mu = 0, 
            conf.int = FALSE, 
            conf.level = 0.95)
#귀무가설: Channel Online과 Offline간의 Units.Sold 차이는 없다
#검정 결과, p-value는 0.5462로 유의수준 10%에서 귀무가설 채택

#결론적으로 Channel Online과 Offline이 Sales에 큰 영향을 주지 않는다는 것을 알 수 있다.

# Regression analysis(회귀분석)
# Units.Sold가 하나씩 늘어남에 따라 Total.Profit은 평균적으로 얼마나 증가하는가?

cor(mydata$Total.Profit,mydata$Units.Sold)

# 회귀식 추정
flt <- lm(mydata$Total.Profit~mydata$Units.Sold)
flt
#Total.Profit= 2303.40 + 78.51 
#Units.Sold가 1 증가할 때마다 Total.Profit이 78.51만큼 증가한다고 볼 수 있음
par(mar=c(1,1,1,1))
plot(Total.Profit~Units.Sold,data=mydata)
abline(flt, col='red')

# 회귀모형의 검정 및 적합도 파악
summary(flt)

#잔차에 대한 기술통계량
#최소값 -762718,  최대값 950987 , 사분위수 -148366, -26115, 98185
#F-statistic의 p-value값 2.2e-16으로 0.05보다 작기 때문에 
#이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있다
#Coefficients의 Units.Sold 변수 값이 2e-16으로 0.05보다 작기 때문에
#Total.Profit를 설명하는데 유의하다고 판단할 수 있다.
#Adjusted R-squared 값은 0.3569로 35퍼센트만큼의 설명력을 가진다.
#귀무가설: 독립변수가 종속변수에 영향을 주지 않는다.
#p-value <0.05이므로 귀무가설 기각 , 추정된 회귀식은 의미가 있다.
#잔차검정
par(mfrow=c(2,2))
plot(flt)
#잔차는 정규분포를 따르고, 등분산이며 독립이다.

#Multiple Linear regression
flt2 <-lm(mydata$Total.Profit ~mydata$Units.Sold+mydata$Total.Revenue)
summary(flt2)

#회귀식: Total.Profit= 1.999e+02 + 2.518e+01 + 2.017e-01
#잔차에 대한 기술통계량
#최소값 -530907, 최대값 604731, 사분위수 -95625, 1786, 71258
#F-statistic의 p-value값 2.2e-16으로 0.05보다 작기 때문에 
#이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있다
#Coefficeints에 나온 변수들의 p-value 값이 모두 0.05보다 작으므로, 
#Total.Profit를 설명하는데 유의하다고 판단할 수 있다.
#Adjusted R-squared 값은 0.8047로 80퍼센트만큼의 설명력을 가진다
#앞선 결과보다 더 높은 설명력을 가짐
#p-value <0.05이므로 귀무가설 기각 , 추정된 회귀식은 의미가 있다.
#잔차검정
plot(flt2)
#잔차는 정규분포를 따르고, 등분산이며 독립이다.
par(mar=c(1,1,1,1))


#MAP으로 mydata의 Total.Profit 코로플레스 지도 그리기
#mydata에 몇몇 국가가 없어서 일부 생략됨, 미국은 mydata에 포함되지 않았음
world_map <-map_data("world2")


#나라별 Unit.Price
ggChoropleth(data=mydata, aes(fill=Unit.Price,map_id=Country),
             map=world_map)

#나라별 Total.Revenue와 Total.Profit
ggChoropleth(data=mydata, aes(fill=c(Total.Revenue,Total.Profit),map_id=Country),
             map=world_map)

#2011,2015,2016,2017 년의 Units.Sold 판매 비교
tbc1<- mydata %>% filter(mydata$year %in% c(2011,2015,2016,2017))
ggChoropleth(data=tbc1,
             aes(fill=Units.Sold,
                 map_id=Country,
                 tooltip=Country,
                 facet=year),
             map=world_map,
)

#sort by number of items(Total)
#show top 30 dates arranged by desc(Total), 가장 많이 팔린 날짜
date <- mydata %>% group_by(Ship.Date) %>% 
  dplyr::summarize(Total=n()) %>% arrange(desc(Total)) %>% 
  head(30)
#Ship.Date는 interval scale, Total은 ratio scale
datatable(date)

#날짜별 아이템 수
hh<-ggplot(date, aes(reorder(Ship.Date,Total,sum),Total)) +
  geom_bar( stat="identity", fill= "skyblue", color="blue") +
  ggtitle("Number of items for Date") +
  theme(legend.position="none") +
  scale_y_continuous(labels=comma)
ggplotly(hh)

#2015년의 월별 아이템 sales 수
year_date <- mydata %>% 
  filter(year==2015) %>% 
  group_by(month) %>% 
  dplyr::summarize(Total=n()) %>% arrange(month) %>% 
  head(30)
#month는 nominal, Total은 ratio scale
datatable(year_date)

aa<- ggplot(year_date, aes(month,Total, fill=month)) +
  geom_bar(stat="identity") +
  ggtitle("Number of Items per Month of 2015") +
  scale_y_continuous(labels=comma)
ggplotly(aa)
#월별 Items Sold 수를 요일별로 보기
colors=c("#CC1101", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0", "purple",
         "black", "orange", "brown", "pink")
month_weekday <-mydata %>% 
  group_by(month, dayofweek) %>% 
  dplyr::summarize(Total=n())
datatable(month_weekday)
#month,dayofweek는 nominal, Total은 ratio scale
hh<-ggplot(month_weekday, aes(month, Total, fill= dayofweek)) +
  geom_bar(stat= "identity", position="dodge") +
  ggtitle("Item Sold by Day and Month") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=colors)
ggplotly(hh)
#대체로 3월~8월에 Item Sold 수치가 높음

#2015년의 7월 일(day)별 이용자 수
day_7 <- mydata %>% group_by(day) %>% 
  filter(year==2015 & month==7) %>% 
  dplyr::summarize(Total=n())
datatable(day_7)

cc<- ggplot(day_7, aes(day,Total)) +
  geom_bar(stat= "identity", fill= "steelblue") +
  ggtitle("2015/7 items sales per Day") +
  theme(legend.position="none") +
  scale_y_continuous(labels=comma)
ggplotly(cc)

#2015년의 일(day) 별 item sales 수를 월별로 구분


day_month_item <-mydata %>% 
  filter(year==2015) %>% 
  group_by(month,day) %>% 
  dplyr::summarize(Total=n())

ww<-ggplot(day_month_item,aes(day,Total, fill=month)) +
  geom_bar(stat="identity") +
  ggtitle("Item sales by Day and Month") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=colors)
ggplotly(ww)

#각 지역별 item sold 수

ggplot(mydata, aes(Region)) +
  geom_bar(fill="darkblue") +
  scale_y_continuous(labels=comma) +
  ggtitle("Items Sold by Region")

#각 지역 별 Item sold 수를  월별로 보기
zz<-ggplot(mydata,aes(Region,fill=month)) +
  geom_bar(position="dodge") +
  scale_y_continuous(labels=comma) +
  ggtitle("Items Sold by Region and Month") +
  scale_fill_manual(values=colors)
ggplotly(zz)
#각 지역 별 Item sold 수를 요일별로 보기
ee<-ggplot(mydata, aes(Region, fill=dayofweek)) +
  geom_bar(position="dodge") +
  scale_y_continuous(labels=comma) +
  ggtitle("Items Sold by Region and DayofWeek") +
  scale_fill_manual(values=colors)
ggplotly(ee)


# a Heatmap visualization of month and year
month_and_year <-mydata %>% 
  group_by(month, year) %>% 
  dplyr :: summarize(Total=n())
datatable(month_and_year)

ggplot(month_and_year, aes(month, year, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Year")

#a Heatmap of day and month
ggplot(day_month_item, aes(day,month, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Day")

#a Heatmap of dayofweek and month
ggplot(month_weekday, aes(dayofweek, month, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by month and Day of week")


#a Heatmap of Region and month
month_region <-mydata %>% 
  group_by(Region,month) %>% 
  dplyr::summarize(Total=n())

ggplot(month_region, aes(Region,month, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Region")

#방사형 차트 그리기
mean_by_Type <- summaryBy(Units.Sold + Unit.Price + Unit.Cost + Total.Revenue + Total.Cost + Total.Profit ~ Type, 
                          data=mydata, 
                          FUN = c(mean))
mean_by_Type

df_radarchart <- function(df) 
{ df <- data.frame(df) 
dfmax <- apply(df, 2, max) 
dfmin <- apply(df, 2, min) 
as.data.frame(rbind(dfmax, dfmin, df))
}
mean_by_Type_2 <- df_radarchart(scale(mean_by_Type[,c(2:7)]))
mean_by_Type_2

radarchart(df = mean_by_Type_2,
           seg = 6,
           pty = 16,
           pcol = 1:6, 
           plty = 1:6, 
           plwd = 2, 
           title = c("radar chart by Types")
)






#국가별 빈도표 만들기(top20)
df_country <-as.data.frame(table(mydata$Country)) 
df_country <-reshape::rename(df_country, c(Var1="country",Freq="freq"))
top20 <-df_country %>% arrange(desc(freq)) %>% head(20)
top20
#빈도순 막대정렬
order <- arrange(top20,freq)$country
ggplot(data=top20,aes(x=country,y=freq)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit=order) +
  geom_text(aes(label=freq), hjust=-0.3)

# Word cloud of country

set.seed(1234)
wordcount <-sort(table(mydata$Country),decreasing=T)
head(wordcount,100)
View(mydata)
wordcloud(names(wordcount), 
          
          freq = wordcount,   
          scale=c(0.5,0.1),
          min.freq = 30,          
          max.words= 100,
          random.order = F,      
          
          rot.per = 0.3,         
          colors = brewer.pal(8,"Dark2")) 

#wordcloud2
wordcloud2(df_country,minRotation=-pi/6,maxRotation=-pi/6, minSize=0.9,
           color="random-light",size=0.1,shape="star")


#read charles_1.txt and wordcloud
# 출처 : http://www.gutenberg.org/browse/scores/top
# 영어 소설을 txt파일로 받을 수 있는 사이트
# Charles Dickins와 Conan Doyle을 감성분석
# 쌓인 PLOT이 많아서 Error가 날 수 있습니다. 빗자루 버튼을 눌러 청소하면 에러가 안 납니다
#Charles Dickins의 Christmas Carol
carol <-readLines("./novels/charles_1.txt")
pal <- brewer.pal(8,"Dark2")
c_carol <-VCorpus(VectorSource(carol))
c_carol <-tm_map(c_carol, stripWhitespace)
c_carol <-tm_map(c_carol, content_transformer(tolower))
c_carol <-tm_map(c_carol, removePunctuation)
c_carol <-tm_map(c_carol, removeNumbers)
c_carol <-tm_map(c_carol, removeWords, stopwords("english"))

tdm <-TermDocumentMatrix(c_carol)
carol_table <-as.matrix(tdm)
carol_v <- sort(rowSums(carol_table),decreasing =T)
carol_txt <- data.frame(word=names(carol_v),freq=carol_v)
#carol wordcloud
wordcloud(words = carol_txt$word,  
          
          freq = carol_txt$freq,   
          
          min.freq = 10,          
          
          max.words = 500,       
          
          random.order = F,      
          
          rot.per = 0.1,         
          scale=c(4,0.1),
          
          colors = pal)

#carol wordcloud2

wordcloud2(carol_txt,minRotation=-pi/6,maxRotation=-pi/6, minSize=1,
           color="random-light",size=0.3,shape="star")

#Sentiment analysis using novels
# 출처 : http://www.gutenberg.org/browse/scores/top
getwd()
wd<-paste(getwd(),"./novels", sep="") #input directory
files <-list.files(wd)
files

#Sentiment Function

GetSentiment <- function(file) 
  { fileName <- glue("./novels/", file, sep="")
  fileName <- trimws(fileName) 
  fileText <- readLines(fileName)
  tokens <- data_frame(text=fileText) %>% unnest_tokens(word, text)
  sentiment <- tokens %>% inner_join(get_sentiments("bing")) %>% count(sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment=positive-negative) %>%
    mutate(file=file) %>%
    mutate(order=as.numeric(str_match(file, "\\d{1}"))) %>% 
    mutate(author=str_match(file, "(.*)_")[2]) 
  return(sentiment)
  }

sentiments <-data.frame()
for (i in files){
  sentiments <-rbind(sentiments, GetSentiment(i))
}
sentiments #Charles와 doyle의 감성분석표

#graph by sentiment
summary(sentiments)

ggplot(sentiments, aes(x=order, y=sentiment)) +
  geom_point(aes(color=author))+
  geom_smooth(method="lm")

ggplot(sentiments,aes(x=author, y=sentiment)) +
  geom_boxplot(aes(color=author))

#compare sentiment of Charles and Doyle
sentiments <-sentiments %>% 
  mutate(Writer=ifelse(author=="charles", "Charles", "Doyle"))

ggplot(sentiments, aes(x=Writer,y=sentiment)) +
  geom_boxplot(aes(color=Writer)) +
  geom_point(aes(color=Writer))
t.test(sentiments$sentiment ~ sentiments$Writer)
#귀무가설: 두 작가의 감성점수는 동일하지 않다
#p-value=0.8592>0.05이므로 귀무가설을 기각한다
#Doyle이 Charles보다 감성점수가 높다

#nrc를 이용한 doyle_1의 감성분석
doyle <-readLines("novels/doyle_1.txt")
tokens_bush <-tibble(text=doyle) %>% unnest_tokens(word,text)
so1 <-tokens_bush %>% inner_join(get_sentiments("nrc"))
table(so1$sentiment)

#afinn을 이용한 doyle_1의 평균 감성점수
so2<-tokens_bush %>% inner_join(get_sentiments("afinn"))
mean(so2$value)




