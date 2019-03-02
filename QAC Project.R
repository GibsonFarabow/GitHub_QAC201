# Gibson Farabow
# QAC 201

library(ggplot2)
library(descr)
library(fifer)

setwd("/Users/gibsonfarabow/Desktop/QAC201/") 

#load("/Users/gibsonfarabow/Desktop/QAC201/OutlookOnLifeData/OOL_PDSwoLabels.RData")
#OOL_Labels <- OOL_PDSwoLabels
load("/Users/gibsonfarabow/Desktop/QAC201/OutlookOnLifeData/OOL_PDS.RData")
Data <- OOL_PDS

# Subsetting to create a smaller dataset which contains all variables of interest
####################################################################################################
myData <- Data[c("ppagect4","ppeducat","ppethm","ppgender","ppincimp","ppmsacat","pphhsize","ppreg4",
                 "ppreg9","ppstaten","ppwork",
                 "w1_a1", "w1_a10", "w1_a11", "w1_a12a", "w1_b1", "w1_b2", "w1_b4", "w1_c1",
                 "w1_c2", "w1_e1", "w1_f3", "w1_g2", "w1_g3a", "w1_i1", "w1_i2", "w1_j3a_a",
                 "w1_j3a_b","w1_j3a_c", "w1_k1_a", "w1_k1_b", "w1_k1_c", "w1_k1_d", "w1_l2_1",
                 "w1_l2_2", "w1_l2_3", "w1_m1", "w1_m3","w1_m7", "w1_o1","w1_p2", 
                 "w1_p4", "w1_p5", "w1_p13a", "w1_p14", "w1_p15", "w1_p20",
                 
                 "w1_d11", "w1_d12", "w1_d13", "w1_d14", "w1_d15", "w1_d16", "w1_d17", "w1_d18",
                 "w1_d19", "w1_d20",
                 "w2_qb1c", "w2_qb1d", "w2_qb2a", "w2_qf10d", "w2_qi3", "w2_qi4_c", "w2_qk3",
                 "w2_qm12", "w2_qn3", "w2_qn4")] 


Wave2 <- Data[c("w2_qb1c", "w2_qb1d", "w2_qb2a", "w2_qf10d", "w2_qi3", "w2_qi4_c", "w2_qk3",
              "w2_qm12", "w2_qn3", "w2_qn4")]
Wave1 <- Data[c("w1_a1", "w1_a10", "w1_a11", "w1_a12a", "w1_b1", "w1_b2", "w1_b4", "w1_c1",
                "w1_c2", "w1_e1", "w1_f3", "w1_g2", "w1_g3a", "w1_i1", "w1_i2", "w1_j3a_a",
                "w1_j3a_b","w1_j3a_c", "w1_k1_a", "w1_k1_b", "w1_k1_c", "w1_k1_d", "w1_l2_1",
                "w1_l2_2", "w1_l2_3", "w1_m1", "w1_m3","w1_m7", "w1_o1","w1_p2", 
                "w1_p4", "w1_p5", "w1_p13a", "w1_p14", "w1_p15", "w1_p20",
                "w1_d11", "w1_d12", "w1_d13", "w1_d14", "w1_d15", "w1_d16", "w1_d17", "w1_d18",
                "w1_d19", "w1_d20")]

demographics <- c("ppagect4","ppeducat","ppethm","ppgender","ppincimp","ppmsacat","pphhsize","ppreg4","ppreg9",
                  "ppstaten","ppwork")

Thermometer_D <- c("w1_d11", "w1_d12", "w1_d13", "w1_d14", "w1_d15", "w1_d16", "w1_d17", "w1_d18",
                   "w1_d19", "w1_d20")
#(D variables are questions asking subjects to rate how they feel obout certain groups on a scale
# of 1-100)

# Project Component E - Data Management
##################################################################################################

myData$Cong_Rep_U <- myData$w1_i2
myData$Cong_Rep_U[myData$w1_i2 == -1] <- NA
myData$Cong_Rep_Amer <- myData$w1_i1
myData$Cong_Rep_Amer[myData$w1_i1 == -1] <- NA

#Recode - 5 means Congress works "extremely well"
myData$Cong_Rep_Amer[myData$w1_i1 == 5] <- 1
myData$Cong_Rep_Amer[myData$w1_i1 == 4] <- 2
myData$Cong_Rep_Amer[myData$w1_i1 == 3] <- 3
myData$Cong_Rep_Amer[myData$w1_i1 == 2] <- 4
myData$Cong_Rep_Amer[myData$w1_i1 == 1] <- 5

myData$Cong_Rep_U[myData$w1_i2 == 5] <- 1
myData$Cong_Rep_U[myData$w1_i2 == 4] <- 2
myData$Cong_Rep_U[myData$w1_i2 == 3] <- 3
myData$Cong_Rep_U[myData$w1_i2 == 2] <- 4
myData$Cong_Rep_U[myData$w1_i2 == 1] <- 5

# Below show there is no significant difference in score - so it should be ok to combine them
# or in other words, personal scores and American scores have a high correlation of similarity
ggplot(myData, aes(x=Cong_Rep_U, y=Cong_Rep_Amer)) + geom_jitter() + stat_smooth(method='lm')
x <- cor.test(myData$Cong_Rep_Amer, myData$Cong_Rep_U)

myData$Cong_Rep_Score <- myData$Cong_Rep_U + myData$Cong_Rep_Amer
myData$Cong_Rep_Score[myData$Cong_Rep_Score== -2] <- NA


myData$Polit_Int <- myData$w1_a1
myData$Polit_Int[myData$w1_a1 == -1] <- NA 

# Recode - 5 means "extremely interested" in politics
myData$Polit_Int[myData$w1_a1 == 5] <- 1
myData$Polit_Int[myData$w1_a1 == 4] <- 2
myData$Polit_Int[myData$w1_a1 == 3] <- 3
myData$Polit_Int[myData$w1_a1 == 2] <- 4
myData$Polit_Int[myData$w1_a1 == 1] <- 5
myData$Polit_Int <- as.factor(myData$Polit_Int)

myData$sub_Rel <- myData$w1_m3
myData$sub_Rel[myData$w1_m3 == -1] <- NA
myData$sub_Rel[myData$w1_m3 == 4] <- 1
myData$sub_Rel[myData$w1_m3 == 3] <- 2
myData$sub_Rel[myData$w1_m3 == 2] <- 3
myData$sub_Rel[myData$w1_m3 == 1] <- 4

myData$religion <- myData$w1_m1
myData$religion <- as.factor(myData$religion)
myData$religion[myData$w1_m1 == -1] <- NA
levels(myData$religion) <- c("Baptist", "Protestant", "Catholic", "Mormon", "Jewish", "Muslim", "Hindu",
                             "Buddhist", "Pentecostal", "Eastern Orthodox", "Other Christian",
                             "Other Christian", "Other", "None")

myData$religion.a <- myData$w1_m1
myData$religion.a[myData$w1_m1 == 1] <- "Baptist (any)"
myData$religion.a[myData$w1_m1 == 2] <- "Protestant"
myData$religion.a[myData$w1_m1 == 3] <- "Catholic"
myData$religion.a[myData$w1_m1 == 13] <- "None"
myData$religion.a[myData$w1_m1 == 4 | 
  myData$w1_m1 == 9 | myData$w1_m1 ==10 | myData$w1_m1 == 11] <- "Other Christian"
myData$religion.a[myData$w1_m1 !=1 & myData$w1_m1 !=2 & myData$w1_m1 !=3 & myData$w1_m1 !=4 & 
  myData$w1_m1 !=9 & myData$w1_m1 !=10 & myData$w1_m1 !=11 & myData$w1_m1 !=13 ] <- "Other Religion"

myData$religion.a <- as.factor(myData$religion.a)

myData$pol_party <- myData$w1_c1
myData$pol_party[myData$w1_c1 == 1] <- "Republican"
myData$pol_party[myData$w1_c1 == 2] <- "Democrat"
myData$pol_party[myData$w1_c1 == 3] <- "Independent"
myData$pol_party[myData$w1_c1 == 4 | myData$w1_c1 == -1] <- NA
myData$pol_party <- as.factor(myData$pol_party)

## Extra stuff
table(myData$pol_party)
ggplot(data=subset(myData, !is.na(myData$pol_party))) + stat_summary(aes(x=pol_party,
                                     y=Cong_Rep_Score), fun.y=mean, geom="bar")

# Politically minded Christian score #(and other religions as of now)

myData$Polit_Int <- as.numeric(myData$Polit_Int)
myData$Polit_Christ <- myData$sub_Rel + myData$Polit_Int  
myData$Polit_Int <- as.factor(myData$Polit_Int) # keep for later ggplot continuity
myData$Polit_Christ <- as.factor(myData$Polit_Christ)

#Project Component F & G
#################################################################################################

barplot(table(myData$Polit_Int), main="Political Interest",
        xlab="Value of Political Interest", ylab="Frequency")
barplot(table(myData$Cong_Rep_Score), main="Congressional Representation Score",
        xlab="Value", ylab="Frequency")
barplot(table(myData$sub_Rel), main="Politics and Church", ylab="Frequency",
        xlab="Value - 1: strong feeling of seperation, 4:strong feeling of compatability")
barplot(table(myData$religion.a), main="Religion", ylab="Frequency")
# Political interest is explanatory variable; Representation is response variable
ggplot(data=subset(myData, !is.na(myData$Polit_Int))) + stat_summary(aes(x=Polit_Int, y=Cong_Rep_Score),
                                   fun.y=mean, geom="bar")

ggplot(data=myData) + geom_bar(aes(x=sub_Rel, y=Cong_Rep_Score), stat="summary", fun.y=mean)
# people who strongly believe politics and church should be seperate have the least amount of faith in Congress
# figure out legend/labels etc.

ggplot(data=subset(myData, !is.na(myData$Polit_Int))) + stat_summary(aes(x=Polit_Int, y=sub_Rel), fun.y=mean, geom="bar")
# Shows clear positive correlation between political interest and belief that
# churches should be involved in politics  

ggplot(data=subset(myData, !is.na(myData$Polit_Int))) +
  geom_bar(aes(x=Polit_Int, y=sub_Rel, fill = religion.a), stat = "summary", fun.y = "mean")
# use position "dodge" to make bars beside each other  

# same data different graph below                                   
ggplot(data=subset(myData, !is.na(myData$Polit_Int))) +
  geom_boxplot(aes(x=Polit_Int, y=sub_Rel)) +
  facet_grid(~religion.a)
# Propotions of interest to pro-religion and church look roughly the same across religions

ggplot(data=subset(myData, !is.na(Polit_Christ))) + geom_bar(aes(x=Polit_Christ, y=sub_Rel), fun.y="mean", stat="summary")
# shows strong linear relationship between political mindedness and positive feelings towards congress


