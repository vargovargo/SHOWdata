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









sprawlOnly <- subset(join, hasSPRAWL ==1)

#sprawlOnly$BMI <- sprawlOnly$ANT070/((sprawlOnly$ANT_MEAS_HEIGHT_CM/100)**2)

sprawlOnly$obese <- ifelse(sprawlOnly$ANT_BMI > 30, 1, 0)
sprawlOnly$anyExercise <- ifelse(sprawlOnly$MET_MIN_WEEK > 0 | , 1, 0)
sprawlOnly$highBPdiagnosis <- ifelse(sprawlOnly$BP_SYSTOLIC_23 > 120 | sprawlOnly$BP_DIASTOLIC_23 > 80, 1, 0)
sprawlOnly$highBP <- ifelse(sprawlOnly$HHQ200 == "1", 1, 0)
sprawlOnly$heartDIS <- ifelse(sprawlOnly$HHQ110 == "1" | sprawlOnly$HHQ100 == "1", 1, 0)
sprawlOnly$diabetes <- ifelse(sprawlOnly$HHQ190 == "1", 1, 0)


#gender
sprawlOnly$MALE <- ifelse(sprawlOnly$GENDER == "1", 1, 0)

#age
sprawlOnly$age30_44 <- ifelse(sprawlOnly$AGE_CONSENT > 29 & sprawlOnly$AGE_CONSENT < 45, 1, 0)
sprawlOnly$age45_64 <- ifelse(sprawlOnly$AGE_CONSENT > 44 & sprawlOnly$AGE_CONSENT < 65, 1, 0)
sprawlOnly$age65_74 <- ifelse(sprawlOnly$AGE_CONSENT > 64 & sprawlOnly$AGE_CONSENT < 75, 1, 0)
sprawlOnly$age75plus <- ifelse(sprawlOnly$AGE_CONSENT > 74, 1, 0)

#race and ethnicity
sprawlOnly$blackNH <- ifelse(sprawlOnly$RACE_ETHNICITY_4CAT == 2, 1, 0)
sprawlOnly$otherNH <- ifelse(sprawlOnly$RACE_ETHNICITY_4CAT == 1, 1, 0)
sprawlOnly$Hisp <- ifelse(sprawlOnly$RACE_ETHNICITY_4CAT == 3, 1, 0)

#missing educational attainment


#income
sprawlOnly$INCOME <- as.numeric(as.character(sprawlOnly$INCOME_HH_MID))
sprawlOnly$incomeLT25k <- ifelse(sprawlOnly$INCOME < 25000, 1, 0)
sprawlOnly$income25k_50k <- ifelse(sprawlOnly$INCOME > 24999 & sprawlOnly$INCOME >= 50000, 1, 0)
sprawlOnly$income50k_75k <- ifelse(sprawlOnly$INCOME > 49999 & sprawlOnly$INCOME >= 75000, 1, 0)


#smoking
sprawlOnly$currentSmoking <- ifelse(sprawlOnly$SMQ_DER_FORMER_NEVER_CURRENT ==1, 1, 0)

#Fruits Vegetables
#Daily Total fruit and vegetable intake in cup equivalents
#recommended servings? say 4
sprawlOnly$FruitVeg <- ifelse(sprawlOnly$QI_DT_FRUITSVEGGIES > 4, 1, 0)


Table1 <- sprawlOnly[, c("SPID","ANT_BMI","obese","anyExercise","MET_MOD","highBPdiagnosis","highBP","heartDIS","diabetes","MALE","age30_44","age45_64","age65_74","age75plus","blackNH","otherNH","Hisp","incomeLT25k","income25k_50k","income50k_75k","currentSmoking","FruitVeg")]

Table1 <- melt(Table1, id.vars = "SPID")

############### calculate stats for Table 1 ##################


stats <- ddply(Table1, .(variable), summarise, 
               n = length(sort(value)),
               mean = mean(value, na.rm=T),
               min = min(value, na.rm=T),
               max = max(value, na.rm=T)           
)

########## modeling sprawl only #################

sprawlOnly$indexSD <- sprawlOnly$compositeindex2010/25

library(aod)

mylogit <- glm(diabetes ~ indexSD + MALE + age30_44 + age45_64 + age65_74 + blackNH + otherNH + Hisp + incomeLT25k + income25k_50k + income50k_75k + currentSmoking + FruitVeg, data = sprawlOnly, family = "binomial")
mylogit <- glm(obese ~ indexSD + MALE + age30_44 + age45_64 + age65_74 + blackNH + otherNH + Hisp + incomeLT25k + income25k_50k + income50k_75k + currentSmoking + FruitVeg, data = sprawlOnly, family = "binomial")
mylogit <- glm(heartDIS ~ indexSD + MALE + age30_44 + age45_64 + age65_74 + blackNH + otherNH + Hisp + incomeLT25k + income25k_50k + income50k_75k + currentSmoking + FruitVeg, data = sprawlOnly, family = "binomial")
mylogit <- glm(higBP ~ indexSD + MALE + age30_44 + age45_64 + age65_74 + blackNH + otherNH + Hisp + incomeLT25k + income25k_50k + income50k_75k + currentSmoking + FruitVeg, data = sprawlOnly, family = "binomial")




summary(mylogit)

confint.default(mylogit)

exp(cbind(OR = coef(mylogit), confint.default(mylogit)))





