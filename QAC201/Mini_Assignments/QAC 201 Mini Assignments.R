#Gibson Farabow
# QAC 201
# Mini Assignments: 3 onwards
library(descr)
library(Hmisc)
load("/Volumes/qac201/PracticeData/HELP.RData")

mydata <- HELP
  
mydata$ExtremeMCS <- mydata$mcs

mydata$ExtremeMCS[mydata$mcs < 20] <- 1
mydata$ExtremeMCS[mydata$mcs >= 20] <- 0


mydata$g1b <- as.character(mydata$g1b)
mydata$SuicidalThought <- mydata$g1b

mydata$SuicidalThought[mydata$g1b == 'yes'] <- 1
mydata$SuicidalThought[mydata$g1b == 'no'] <- 0


mydata$HomelessStatus <- as.character(mydata$homeless)
mydata$HomelessStatus[mydata$homeless == 'housed'] <- 0
mydata$HomelessStatus[mydata$homeless == 'homeless'] <- 1

mydata$HomelessStatus <- as.numeric(mydata$HomelessStatus)
mydata$SuicidalThought <- as.numeric(mydata$SuicidalThought)
mydata$ExtremeMCS <- as.numeric(mydata$ExtremeMCS)

mydata$RiskTotal <- 1
mydata$RiskTotal <- mydata$HomelessStatus + mydata$ExtremeMCS + mydata$SuicidalThought


freq(mydata$ExtremeMCS)
freq(mydata$SuicidalThought)
freq(mydata$RiskTotal)
