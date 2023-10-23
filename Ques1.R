getwd()
mydata <- read.csv("C:/Users/HP/Downloads/adult.csv")


## Ques1.a)
mean(mydata$age)
median(mydata$age)
sd(mydata$age)
IQR(mydata$age)

hist(mydata$age)

mean(mydata$hours.per.week)
median(mydata$hours.per.week)
sd(mydata$hours.per.week)
IQR(mydata$hours.per.week)

## Ques1.b)

library(tidyverse)
plt <- ggplot(mydata,aes(x=age, y=hours.per.week)) + geom_col()
plt



## Ques1.c)

plt <- ggplot(mydata, aes (x=age, y=hours.per.week)) + geom_point(alpha = 0.5) + geom_smooth(method = lm)
plt

## Ques1.d)

mydata %>% group_by(occupation) %>% summarise("count " = n())
mydata %>% group_by(education) %>% summarise("count " = n())

plt <- ggplot(mydata, aes (x = occupation)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plt

## Ques1.e)
xtab <- table (mydata$education, mydata$sex)
xtab

ggplot(data = as.data.frame(xtab), aes (x=Var1, y=Freq, fill= Var2)) +
  geom_bar(stat = "identity", width = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ xlab ("Education Level") + ylab("sex")


