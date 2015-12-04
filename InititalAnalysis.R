library(ggplot2)
library(reshape2)
library(plyr)
library(foreign)
library(data.table)

setwd("~/Box Sync/work/SHOW_SprawlIndex/analysis/")

rawSHOW <- read.csv("./dr180_20151111_csv/dr180_20151111.csv", header=T)

rawSPRAWL <- read.csv("./tractSPRAWL_WI.csv", header=T)

join <- merge(rawSHOW, rawSPRAWL, by.x="CENSUS_TRACT_2010", by.y="tract", all.x=T)

join$hasSPRAWL <- ifelse(is.na(join$state), 0,1)

joinDT <- data.table(join)
setkey(joinDT,"hasSPRAWL","COHORT")

try1 <- joinDT[,list(BMI=mean(ANT_BMI, na.rm=T), BPsys=mean(BP_SYSTOLIC_23, na.rm=T)), by= list(hasSPRAWL, COHORT) ]

ggplot(try1, aes(x=factor(hasSPRAWL), y=BMI)) + geom_bar(stat="identity") + facet_grid(.~COHORT)

dots <- subset(join, hasSPRAWL ==1)

ggplot(dots, aes(x=compositeindex2010, y=AGE_CONSENT, color=factor(COHORT))) + geom_point(stat="identity")

ggplot(join, aes(x=AGE_CONSENT, fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=LAB_GH, fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=LAB_HDLCHOL, fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=BP_SYSTOLIC_23, fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=log(MET_MOD), fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=log(MET_TRANS), fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=log(MET_MIN_WEEK), fill=factor(hasSPRAWL))) + geom_histogram() 

ggplot(join, aes(x=ANT_BMI, fill=factor(hasSPRAWL))) + geom_histogram() 




stats <- ddply(join, .(hasSPRAWL), summarise, 
      n = length(MET_MOD),
      mean = mean(MET_MOD, na.rm=T),
      min = min(MET_MOD, na.rm=T),
      max = max(MET_MOD, na.rm=T)           
)






########## modeling sprawl only #################
sprawlOnly <- subset(join, hasSPRAWL ==1)

sprawlOnly$highBP <- ifelse(sprawlOnly$BP_SYSTOLIC_23 > 120, 1, 0)

sprawlModel <- na.omit(sprawlOnly[,c("highBP", "compositeindex2010", "INCOME_HH_MID")])
sprawlModel <- subset(sprawlModel, INCOME_HH_MID != "R" & INCOME_HH_MID != "D" )
sprawlModel$Income <- as.numeric(as.character(sprawlModel$INCOME_HH_MID))

library(aod)

mylogit <- glm(highBP ~ compositeindex2010  + Income , data = sprawlModel, family = "binomial")

summary(mylogit)

confint.default(mylogit)

exp(cbind(OR = coef(mylogit), confint.default(mylogit)))

