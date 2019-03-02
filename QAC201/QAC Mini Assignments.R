

library(ggplot2)
library(Hmisc)
library(descr)

Mini4 <- read.csv("/Users/gibsonfarabow/Desktop/QAC201/Mini_Assignments/Utilities.csv")

barplot(Mini4$totalbill)


str(Mini4$month)
str(Mini4$gasbill)

Mini4$month <- as.factor(Mini4$month)

ggplot(data = as.data.frame(Mini4)) +
  geom_col(aes(x= month, y = gasbill))

ggplot(Mini4, aes(x=as.factor(month), y=gasbill)) + 
         geom_col()

plot(Mini4$month, Mini4$gasbill)


ggplot(data=Mini4) +
  stat_summary(aes(x=month, y=gasbill),
               fun.y=mean, geom="bar")

ggplot(data=Mini4) +
  stat_summary(aes(x=month, y=elecbill),
               fun.y=mean, geom="bar")

scatter.smooth(Mini4$kwh, y=Mini4$gasbill)

ggplot(data=Mini4) +
  stat_summary(aes(x=kwh, y=elecbill),
              geom="bar")

Mini4$Season <- Mini4$month
Mini4$Season <- as.numeric(Mini4$Season)

Mini4$Season[Mini4$month == 12 | Mini4$month == 1 |
               Mini4$month == 2] <- "winter" 
Mini4$Season[Mini4$month == 3 | Mini4$month == 4 |
               Mini4$month == 5] <- "spring" 
Mini4$Season[Mini4$month == 6 | Mini4$month == 7 |
               Mini4$month == 8] <- "summer" 
Mini4$Season[Mini4$month == 9 | Mini4$month == 10 |
               Mini4$month == 11] <- "fall" 

Mini4$donate <- Mini4$donate

Mini4$DonorStatus[Mini4$donate == "no"] <- 0
Mini4$DonorStatus[Mini4$donate == "yes"] <- 1

Mini4$DonorStatus <- as.numeric(Mini4$DonorStatus)
class((Mini4$DonorStatus))

ggplot(data=Mini4) + geom_col(aes(x=Season, y=DonorStatus))
                              

# Mini Assignment 5
############################################################################

ggplot(data=Mini4) + stat_summary(aes(x=Season, y=totalbill), fun.y=mean, geom="bar")

aov.season.bill <- aov(Mini4$totalbill ~ Mini4$Season, data=Mini4)
# Q1 ANOV test     # Q2 Yes 
summary(aov.season.bill)
TukeyHSD(aov.season.bill)

# Mini Assignment 6
############################################################################
Mini6 <- read.csv("/Users/gibsonfarabow/Desktop/QAC201/Mini_Assignments/CPS.csv")

q1 <- Mini6$wage[Mini6$sex == "M"]
q2 <- Mini6$wage[Mini6$sex == "F"]
mean(q1)
mean(q2)
# men mean is 9.99
# women mean is 7.87  
q3 <- as.data.frame(cbind(q1, q2))
names(q3) <- c("Male", "Female")

aov.q3 <- aov(Mini6$wage ~ Mini6$sex, data=Mini6)
summary(aov.q3)
TukeyHSD(aov.q3)

asdz<-cor.test(Mini6$age,Mini6$wage)

ggplot(Mini6) + geom_point(aes(x=age, y=wage))
# weak positive correlation

# Q4

library(fifer)
myChi <- chisq.test(Mini6$sex, Mini6$sector)

myChi$observed # for actual, observed cell counts
prop.table(myChi$observed, 2) # for column percentages
prop.table(myChi$observed, 1) # for row percentages
round(myChi$residuals, 3)

# What are residuals - think about residuals of sector and gender 
# - female repulsion to construction
# - difference between observed and expected - big negative # means repulsion & vice versa

