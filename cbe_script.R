library(dplyr)
library(ggplot2)
library(tidyverse)
library(DataExplorer)
df <- read.csv("C:/Users/sigma_phi_kappa/Downloads/Capital Bikeshare Data.csv")
View(df)



#a: Encoding factors/numerics
df$season <- factor(df$season)
df$yr <- factor(df$yr)
df$mnth <- factor(df$mnth)
df$holiday <- factor(df$holiday)
df$weekday <- factor(df$weekday)
df$workingday <- factor(df$workingday)
df$weathersit <- factor(df$weathersit)
df$temp <- as.numeric(df$temp)
df$atemp <- as.numeric(df$atemp)
df$hum <- as.numeric(df$hum)
df$windspeed <- as.numeric(df$windspeed)
df$casual <- as.numeric(df$casual)
df$registered <- as.numeric(df$registered)
df$cnt <- as.numeric(df$cnt)

ggplot() + geom_smooth(aes(x=df$cnt, y=df$windspeed))
ggplot() + geom_smooth(aes(x=df$cnt, y=df$temp))
df %>% group_by(weathersit) %>% summarize(registered = mean(registered), casual = mean(casual))
                                          #b
summary(df)
DataExplorer::plot_box(df)
DataExplorer::plot_histogram(df)
#Clear outlier of 20 in registered
#Clear outlier of 0 in humidity...doubt it's validity
#Clear outlier of .0223 and .507 in windspeed
#Months pass sanity check
#Years pass sanity check (2012 is a leap year)



#c
cor.test(df$cnt, df$temp) #cor = .627
cor.test(df$cnt, df$atemp) #cor = .631
cor.test(df$cnt, df$hum) #cor = -.101
cor.test(df$cnt, df$windspeed) #cor = -.234
cor.test(df$cnt, df$casual) #cor = .673
cor.test(df$cnt, df$registered) #cor = .946
#Registered has far and away the heaviest correlation
#Casual comes in at 2nd most correlated



#d
df %>% group_by(df$yr) %>% summarize(mean(cnt))
df %>% group_by(df$mnth) %>% summarize(mean(cnt))
df %>% group_by(df$holiday) %>% summarize(mean(cnt))
df %>% group_by(df$weekday) %>% summarize(mean(cnt))
df %>% group_by(df$workingday) %>% summarize(mean(cnt))
df %>% group_by(df$weathersit) %>% summarize(mean(cnt))




#e
yes <- df %>% filter(df$workingday==1)
no <- df %>% filter(df$workingday==0)
t.test(yes$cnt,no$cnt)
#Fail to reject the null hypothesis due to a p-value of
#.11 - cannot assume a statistical difference between
#workdays vs non-workdays.

tw11 <- df %>% filter(df$yr==0)
tw12 <- df %>% filter(df$yr==1)
t.test(tw11$cnt,tw12$cnt)
#Reject the null hypothesis due to a p-value of
#2.2e-16 - likely a significant difference in usage between
#between 2011 and 2012.



#f
obj<-aov(df$cnt~df$weekday,data=df)
summary(obj)
#Fail to reject null hypothesis due to p-value of 0.583

obj<-aov(df$cnt~df$season)
summary(obj)
#Reject null hypothesis due to p-value of <2e-16

#Weekday does not appear to have a statistically significant
#impact on bike rentals; however, season does.



#g
holiday<-df[df$holiday==1,]
noholiday<-df[df$holiday==0,]
spring<-df[df$season==1,]
fall<-df[df$season==3,]

t.test(holiday$casual,holiday$registered)
#Reject null hypothesis due to p-val of .00016

t.test(noholiday$casual,noholiday$registered)
#Reject null hypothesis due to p-val of 2.2e-16

t.test(spring$casual, spring$registered)
#Reject null hypothesis due to p-val of 2.2e-16

t.test(fall$casual, fall$registered)
#Reject null hypothesis due to p-val of 2.2e-16

#We see a statistical significance with each of the four
#tests, but it appears the correlation on holidays is less
#than on non-holidays




#h
cor.test(df$casual, df$temp) #cor = .543
cor.test(df$casual, df$atemp) #cor = .544
cor.test(df$casual, df$hum) #cor = -.077
cor.test(df$casual, df$windspeed) #cor = -.168
cor.test(df$casual, df$registered) #cor = .395
cor.test(df$casual, df$cnt) #cor = .673

cor.test(df$registered, df$temp) #cor = .540
cor.test(df$registered, df$atemp) #cor = .544
cor.test(df$registered, df$hum) #cor = -.091
cor.test(df$registered, df$windspeed) #cor = -.217
cor.test(df$registered, df$casual) #cor = .395
cor.test(df$registered, df$cnt) #cor = .946

#There are marginal differences in correlation with windspeed and humidity,
#but the biggest difference is that registered correlates to total
#count far more than casual does.





#i
training<-sample_n(df, 512)
validation<-sample_n(df, 219)





#j
mdl_obj <- lm(as.formula(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed), training)
summary(mdl_obj)




#k
#Seasons 1, 3, and 4, yr1, holiday, weatersit 2 & 3, hum and windspeed all correlate at a *** level
#Season 2, mnth 3, 5, & 9, weekday 4, 5, & 6 all correlate at a ** level




#l
mdl_obj2 <- lm(as.formula(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed), validation)
summary(mdl_obj2)
#validation only supports correlation for season4, yr1, mnth3, weekday6, weathersit3 and windspeed.
#validation implies that supposed correlation btwn Season 1 & 3, holiday, weathersit 2, and hum was possibly inaccurate.





#m
highhum <- validation %>% filter(hum > .9)
lowhum <- validation %>% filter (hum < .3)

mean(predict(mdl_obj, highhum)) #predicted 2462 average users on days w high humidity
mean(predict(mdl_obj, lowhum)) #predicted 3373 average users on days w low humidity

#Predicted difference btwn low & high humidity days is 911 users.




#n
summer <- validation %>% filter(season==2)
fall <- validation %>% filter(season==3)

mean(predict(mdl_obj, summer)) #predicted val of 5128
mean(predict(mdl_obj, fall)) #predicted val of 5655

#Predicted difference btwn fall and summer is 527 users





#o
mdl_casual <- lm(as.formula(casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed), training)
summary(mdl_casual)

mdl_registered <- lm(as.formula(registered~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed), training)
summary(mdl_registered)

#casual has a *** correlation with Spring only, while registered has a *** for all 4 seasons
#registered has a *** correlation with weathersit 2&3, and atemp, while casual has no significant correlation
#registered also has a *** correlation with months 3, 9, & 10, while casual has no significant correlation



w1<-df[df$weathersit==1,]
w2<-df[df$weathersit==2,]
w3<-df[df$weathersit==3,]

