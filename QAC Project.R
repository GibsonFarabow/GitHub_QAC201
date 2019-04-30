# Gibson Farabow
# QAC 201

library(ggplot2)
library(descr)
library(fifer)

setwd("/Users/gibsonfarabow/Desktop/GitHub_QAC201")

#load("/Users/gibsonfarabow/Desktop/GitHub_QAC201/QAC201/OutlookOnLifeData/OOL_PDSwoLabels.RData")
#OOL_Labels <- OOL_PDSwoLabels
load("/Users/gibsonfarabow/Desktop/GitHub_QAC201/QAC201/OOL_Data_Info/OOL_PDS.RData")
Data <- OOL_PDS

# Subsetting to create a smaller dataset which contains all variables of interest
####################################################################################################
myData <- Data[c("ppagect4","ppeducat","ppethm","ppgender","ppincimp","ppmsacat","pphhsize","ppreg4",
                 "ppreg9","ppstaten","ppwork",
                 "w1_a1", "w1_a10", "w1_a11", "w1_a12a", "w1_b1", "w1_b2", "w1_b4", "w1_c1",
                 "w1_c2", "w1_f3", "w1_g2", "w1_g3a", "w1_i1", "w1_i2", "w1_j3a_a",
                 "w1_j3a_b","w1_j3a_c", "w1_k1_a", "w1_k1_b", "w1_k1_c", "w1_k1_d", "w1_l2_1",
                 "w1_l2_2", "w1_l2_3", "w1_m1", "w1_m3","w1_m7", "w1_o1","w1_p2",
                 "w1_p4", "w1_p5", "w1_p13a", "w1_p14", "w1_p15", "w1_p20",

                 "w1_d11", "w1_d12", "w1_d13", "w1_d14", "w1_d15", "w1_d16", "w1_d17", "w1_d18",
                 "w1_d19", "w1_d20",
                 "w2_qb1c", "w2_qb1d", "w2_qb2a", "w2_qe1", "w2_qf10d", "w2_qi3", "w2_qi4_c", "w2_qk3",
                 "w2_qm12", "w2_qn3", "w2_qn4")]

myData$ID <- 1:nrow(myData)
myData$obsv <- 1

myData <- myData[, c("ID", "obsv", "ppagect4","ppeducat","ppethm","ppgender","ppincimp","ppmsacat","pphhsize","ppreg4",
                    "ppreg9","ppstaten","ppwork",
                    "w1_a1", "w1_a10", "w1_a11", "w1_a12a", "w1_b1", "w1_b2", "w1_b4", "w1_c1",
                    "w1_c2", "w1_f3", "w1_g2", "w1_g3a", "w1_i1", "w1_i2", "w1_j3a_a",
                    "w1_j3a_b","w1_j3a_c", "w1_k1_a", "w1_k1_b", "w1_k1_c", "w1_k1_d", "w1_l2_1",
                    "w1_l2_2", "w1_l2_3", "w1_m1", "w1_m3","w1_m7", "w1_o1","w1_p2",
                    "w1_p4", "w1_p5", "w1_p13a", "w1_p14", "w1_p15", "w1_p20",
                    
                    "w1_d11", "w1_d12", "w1_d13", "w1_d14", "w1_d15", "w1_d16", "w1_d17", "w1_d18",
                    "w1_d19", "w1_d20",
                    "w2_qb1c", "w2_qb1d", "w2_qb2a", "w2_qe1", "w2_qf10d", "w2_qi3", "w2_qi4_c", "w2_qk3",
                    "w2_qm12", "w2_qn3", "w2_qn4") ]

# copy for managing NAs
myData2 <- Data[c("ppagect4","ppeducat","ppethm","ppgender","ppincimp","ppmsacat","pphhsize","ppreg4",
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
myData$w1_i2[myData$w1_i2 == -1] <- NA
myData$w1_i1[myData$w1_i1 == -1] <- NA

myData$Cong_Rep_U <- myData$w1_i2

myData$Cong_Rep_Amer <- myData$w1_i1

#Recode - 5 means Congress works "extremely well"
myData$Cong_Rep_Amer <- 6 - myData$w1_i1

myData$Cong_Rep_U <- 6 - myData$w1_i2

# Below shows there is no significant difference in score,
# so it should be ok to combine them to make a holistic score. In other words,
# personal representation American collective representation scores are not typically different
ggplot(myData, aes(x=Cong_Rep_U, y=Cong_Rep_Amer)) + geom_jitter() + stat_smooth(method='lm')
Cong_Test <- cor.test(myData$Cong_Rep_Amer, myData$Cong_Rep_U)
# p value < .001 and cor coefficient = .84



myData$Cong_Rep_Score <- myData$Cong_Rep_U + myData$Cong_Rep_Amer
myData$Cong_Rep_Score[myData$Cong_Rep_Score== -2] <- NA

myData$w1_a1[myData$w1_a1 == -1] <- NA
myData$Polit_Int <- myData$w1_a1



# Recode - 5 means "extremely interested" in politics
myData$Polit_Int_Num <- 6 - myData$Polit_Int
myData$Polit_Int <- myData$Polit_Int_Num

myData$Polit_Int[myData$w1_a1 == 1] <- "Extreme"
myData$Polit_Int[myData$w1_a1 == 2] <- "Very"
myData$Polit_Int[myData$w1_a1 == 3] <- "Moderate"
myData$Polit_Int[myData$w1_a1 == 4] <- "Slight"
myData$Polit_Int[myData$w1_a1 == 5] <- "None"
myData$Polit_Int <- as.factor(myData$Polit_Int)

# ????
###
#reorder(!is.na(myData$Polit_Int), levels(myData$Polit_Int)[c("None", "Slight", "Moderate", "Very", "Extreme")])



myData$w1_m3[myData$w1_m3 == -1] <- NA
myData$sub_Rel <- 5 - myData$w1_m3 # where 4 is strongly agree church should be involved in politics 

myData$religion <- myData$w1_m1
myData$religion <- as.factor(myData$religion)
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

# Moderator variable
myData$w1_p4[myData$w1_p4== 1] <-"heterosexual"
myData$w1_p4[myData$w1_p4== 2] <-"gay"
myData$w1_p4[myData$w1_p4== 3] <-"lesbian"
myData$w1_p4[myData$w1_p4== 4] <- "bisexual"
myData$w1_p4[myData$w1_p4== 5] <- NA
myData$w1_p4[myData$w1_p4== -1] <- NA

#Project Component F & G
#################################################################################################

barplot(table(myData$pol_party), main="Political Party", ylab="Frequency")
barplot(table(myData$Polit_Int), main="Political Interest",
        xlab="Value of Political Interest", ylab="Frequency")
barplot(table(myData$Cong_Rep_Score), main="Congressional Representation Score",
        xlab="Value", ylab="Frequency")
barplot(table(myData$sub_Rel), main="Politics and Church", ylab="Frequency",
        xlab="Value - 1: strong feeling of seperation, 4:strong feeling of compatability")
barplot(table(myData$religion.a), main="Religion", ylab="Frequency")

ggplot(myData) +
  geom_bar(aes(x=pol_party)) +
  geom_label(stat="count", aes(x=pol_party, label=..count..))


# Political interest is explanatory variable; Representation is response variable
ggplot(data=subset(myData, !is.na(myData$Polit_Int))) + stat_summary(aes(x=Polit_Int, y=Cong_Rep_Score),
                                   fun.y=mean, geom="bar")

ggplot(data=myData) + geom_bar(aes(x=sub_Rel, y=Cong_Rep_Score), stat="summary", fun.y=mean)
# people who strongly believe politics and church should be seperate have the least amount of faith in Congress
# figure out legend/labels etc.

ggplot(data=subset(myData, !is.na(myData$Polit_Int))) + stat_summary(aes(x=Polit_Int, y=sub_Rel), fun.y=mean, geom="bar")
# Shows clear positive correlation between political interest and belief that
# churches should be involved in politics

ggplot(data=subset(myData, !is.na(myData$Polit_Int)),
       aes(x=Polit_Int, y=sub_Rel, fill = religion.a)) +
  geom_bar(stat = "summary", fun.y = "mean") #+ geom_text(label=sub_Rel)
# use position "dodge" to make bars beside each other

# same data different graph below
ggplot(data=subset(myData, !is.na(myData$Polit_Int))) +
  geom_boxplot(aes(x=Polit_Int, y=sub_Rel)) +
  facet_grid(. ~religion.a)
# Propotions of interest to pro-religion and church look roughly the same across religions


# (Original) Main Varaibles Statistical Testing
########################################################################################

Cong_Pol.aov <- aov(myData$Cong_Rep_Score ~ myData$Polit_Int, data=myData)
summary(Cong_Pol.aov)
TukeyHSD(Cong_Pol.aov)
# Conducting an ANOVA test indicates that there is a significant association
# between political interest and feelings. Running a post-hoc test indicates
# there is only a significant correlation between political interest and feelings toward Congress
# when 1: comparing no political interest ("None") and "Moderate,"
# 2: comparing "Slight" to "None," and
# 3: comparing "Very" to "None."
# These results show that there is only a significant association between Political Interest
# and faith in Congress when measuring political interest in comparison to no political interest,
# however even here the relationship is not linear. (I will conduct a regular correlation later, because
# I think it makes more sense to make political interest a numerical value instead of a factor.)

#Visualize the relationship for correlation
ggplot(myData, aes(x=Polit_Int_Num, y=Cong_Rep_Amer)) + geom_jitter() + stat_smooth(method="lm")
cor.test(myData$Polit_Int_Num, myData$Cong_Rep_Amer)
# Shows there is not a significant correlation between Political Interest and Congressional score
# r = -.100, p < .001


cor.test(myData$Polit_Int_Num, myData$sub_Rel)
# Shows there is a weak positive correlation between political interest and
# the belief that church should be involved in politics; r = .124, p < .001
Pol_Sub.aov <- aov(myData$sub_Rel ~ myData$Polit_Int, data=myData)
summary(Pol_Sub.aov)
TukeyHSD(Pol_Sub.aov)
# Again mostly shows a significant relationship between people with 'None'
# political interest vs others. Also shows significant relationship between people
# 'Very' interested in politics vs 'Extremely.'

##############################################################################################
ggplot(myData, aes(x=sub_Rel, y=Cong_Rep_Score)) + geom_jitter() + stat_smooth(method="lm")
cor.test(myData$sub_Rel, myData$Cong_Rep_Score)
# weak correlation (r = .111), which shows a statistically significant positive relation with the belief
# that church and polilitcs can go together and Congress score

Party_Cong.aov <- aov(myData$Cong_Rep_Score ~ myData$pol_party)
summary(Party_Cong.aov)
TukeyHSD(Party_Cong.aov)
# There is only a significant difference between Democrats and Indendents for Congress Score

Party_subRel.aov <- aov(myData$sub_Rel ~ myData$pol_party)
summary(Party_subRel.aov)
TukeyHSD(Party_subRel.aov)
# Significant difference between everyone but Republican vs Independent; p < .005
# Considering last test, suggests Independents and Republicans are more aligned than Democrats on this issue


# Multivariate testing for moderation
#####################################################################################
# (large differences between Cong_Rep_Amer and Cong_Rep_U for correlations.) 

# 1 by religion for religious association
by(myData,
   myData$religion.a,
   function(x) cor.test(x$sub_Rel, x$Polit_Int_Num))

# Shows there is a significant relationship between political interest and sub_rel for Baptists
# and 'Other Christians' at p <.005 and for 'Other Religions' and 'Protestants' at p <.05. "Other Christians"
# had the strongest negative correlation at r= -.257 (still weak), and the others had weaker
# correlations between r= -.1 and -.2.
# The results show p >.8 for "Catholics," strongly suggesting there is no relationship between political
# interest and belief in the seperation of church and state. There was also no significant relationship
# found for peole with "None" for religion

Denom <- myData[c("Polit_Int_Num", "sub_Rel", "religion.a")]
ggplot(Denom, aes(x=religion.a, y=sub_Rel, fill=religion.a)) + stat_summary(fun.y=mean, geom="bar")

#2 by class for pol_int to cong_rep
by(myData,
   myData$w1_p2,
   function(x) cor.test(x$Cong_Rep_Amer, x$Polit_Int_Num)) # by class

# Middle class has r= -.186 and p < .0001, strongly suggesting that as political interest goes up,
# Cong_Score goes down though the relationship is weak. No other groups had statistically significant results

# Sexual orientation makes for stronger correlations:
by(myData,
   myData$w1_p4,
   function(x) cor.test(x$Cong_Rep_Amer, x$Polit_Int_Num)) 

# for people who consider themselves homosexual, the is a significant negative correlation between
# political interest and congress score. 'Lesbian'- p<.01 and r= -.77; 'Gay' - p<.01 and r= -.48
# for heterosexuals there seems to be no significant relationship: r= -.09 and p<.001.


# try some feeling thermometers for this (but remember explanatory categorical) for anova moderation
by(myData,
   myData$religion.a,
   function(x) list(aov(w1_d20 ~ Polit_Int, data = x), summary(aov(w1_d20 ~ Polit_Int, data = x))))


# Moderator graphs
#####################################################################################
ggplot(data=subset(myData,!is.na(w1_p4)), aes(x=Polit_Int_Num, y=Cong_Rep_Amer, fill=w1_p4)) + 
  geom_bar(stat="summary", fun.y=mean, position="dodge") 

Sex_Pol <- myData[c("Cong_Rep_Amer", "Polit_Int_Num", "w1_p4")]
Sex_Pol$w1_p4[Sex_Pol$w1_p4 == "bisexual"] <-NA
Sex_Pol$w1_p4[Sex_Pol$w1_p4 == "gay" | Sex_Pol$w1_p4 == "lesbian"] <- "homosexual"

ggplot(data=subset(Sex_Pol,!is.na(w1_p4)), aes(x=Polit_Int_Num, y=Cong_Rep_Amer, fill=w1_p4)) + 
  geom_bar(stat="summary", fun.y=mean, position="dodge") 

colors <- c("red", "blue", "green", "yellow", "purple", "black")
Cath_Rel <- myData[c("Polit_Int_Num", "sub_Rel", "religion.a")]

ggplot(data=subset(myData,!is.na(religion.a)), aes(x=Polit_Int_Num, y=sub_Rel, fill=religion.a)) + 
  geom_jitter(stat="summary", fun.y=mean) + stat_smooth(method="auto", size=0)

ggplot(data=subset(myData,!is.na(religion.a)), aes(x=Polit_Int_Num, y=sub_Rel, color=religion.a, fill=religion.a)) +
         geom_jitter(stat="summary", fun.y=mean, shape=NA) + stat_smooth( method="auto", se=FALSE)
  
this <- subset(Cath_Rel, Cath_Rel$religion.a=="Catholic") # Can switch these around

ggplot(data=this, aes(x=Polit_Int_Num, y=sub_Rel, color=religion.a)) + 
  geom_jitter(stat="summary", fun.y=mean, shape=NA) + 
  stat_smooth(method="auto", fill="light blue", color= "green")

this <- subset(Cath_Rel, Cath_Rel$religion.a=="Catholic" | religion.a=="Baptist (any)")

ggplot(data=this, aes(x=Polit_Int_Num, y=sub_Rel, color=religion.a)) + 
  geom_jitter(stat="summary", fun.y=mean, shape=NA) + 
  stat_smooth(method="auto")

# Project Component J - Multiple Regression 
##########################################################################################
levels(myData$religion.a)
myData$religion.a <- relevel(myData$religion.a, ref = 2) # recode level to have "None" as
# default for multiple regression test (comparing None religion to the others)
levels(myData$religion.a)

my.lm <- lm(myData$sub_Rel ~ myData$Polit_Int_Num + myData$religion.a + 
              myData$Polit_Int_Num*myData$religion.a, data = myData)
summary(my.lm)

levels(myData$religion.a) <- c("Catholic", "Protestant", "Baptist (any)", "Other Christian", "Other Religion", "None")


myData$household_inc <- myData$ppincimp
freq(myData$household_inc)   # think of as quantitative - check codebook for levels

ggplot(myData) + stat_summary(fun.y= mean, aes(x=religion.a, y=household_inc), geom="bar")
# See how wealth may confound (it doesn't seem to considering None is much different
# And protestant is the same) - do test later 

this <- subset(Cath_Rel, Cath_Rel$religion.a=="Catholic" | religion.a=="Protestant")

ggplot(data=this, aes(x=Polit_Int_Num, y=sub_Rel, color=religion.a)) + 
  geom_jitter(stat="summary", fun.y=mean, shape=NA) + 
  stat_smooth(method="auto") # + facet_grid(. ~ religion.a)


# Poster Graphs
######################################################################################

myData$sub_Rel <- as.factor(myData$sub_Rel) # for fill coloring
ggplot(data=subset(myData, !is.na(sub_Rel)), aes(x=sub_Rel, y=obsv, fill=sub_Rel)) + geom_col() +
  labs(x="1: Strong Yes;  4: Strong No", y="Frequency", 
       title="Figure 1: Should Religion and Politics Be Seperate?")
myData$sub_Rel <- as.numeric(myData$sub_Rel) # revert back

levels(myData$Polit_Int)

myData$Polit_Int <- relevel(myData$Polit_Int, ref = 5)
myData$Polit_Int <- relevel(myData$Polit_Int, ref = 3)
myData$Polit_Int <- relevel(myData$Polit_Int, ref = 5)
myData$Polit_Int <- relevel(myData$Polit_Int, ref = 5)

ggplot(data=subset(myData, !is.na(Polit_Int)), aes(x=Polit_Int, y=sub_Rel, fill=Polit_Int)) +
  stat_summary(geom="bar", fun.y=mean) +
  labs(x="Political Interest", y="Church & Politics (Figure 1)", fill="Pol_Int",
       title="Figure 2: The association between political interest and feelings of whether 'Church & Politics' should be seperate")

ggplot(data=subset(myData,!is.na(religion.a)),
       aes(x=Polit_Int_Num, y=sub_Rel, color=religion.a)) +
  geom_jitter(stat="summary", fun.y=mean, shape=NA) +
  stat_smooth( method="auto", se=FALSE) + facet_grid(. ~ religion.a) +
  labs(x="Political Interest", y="Church & Politics (Figure 1)", color="Religion",
       title="Figure 3: Adding Religious Identity")
