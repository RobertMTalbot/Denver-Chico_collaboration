#imputed data analysis

library(dplyr)
library(nlme)

#JMN Read in
lvl1 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (laptop & home)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1$row <- 1:nrow(lvl1)

#Bring lvl2_imp into lvl1_imp
lvl1 <- left_join(lvl1, lvl2[,c("Assessment_Sequence_ID", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "Assessment_Sequence_ID")
#Calculate absolute gain for filtering students later.
lvl1$absgain <- lvl1$POST.score-lvl1$PRE.score

#turn dummy variable in a single factor

lvl1$gender <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*3 + lvl1$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))
lvl1$instrument <- factor(lvl1$PCA*1 + lvl1$IMCA*2 + lvl1$GCA*3 + lvl1$CINS*4 + lvl1$CCI*5 + lvl1$FMCE*6 + lvl1$BEMA*7 + lvl1$FCI*8 + lvl1$CSEM*9 + lvl1$LSCI*10, labels = c(NA, "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))
lvl1$race <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*3 + lvl1$american_indian*4 + lvl1$hawaiian_or_other_pacific_islander*5 + lvl1$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))


#Convert the student time data to numeric strings. The warning is not important.
lvl1$PRE.Duration..Seconds.<-as.numeric(levels(lvl1$PRE.Duration..Seconds.)[lvl1$PRE.Duration..Seconds.])
lvl1$POST.Duration..Seconds.<-as.numeric(levels(lvl1$POST.Duration..Seconds.)[lvl1$POST.Duration..Seconds.])

#Filter out LA's 
lvl1_filt <- lvl1 %>%
  filter(Student.or.LA == 0) #This also removes the students that provided no other information

# remove the students that did not provide either a pre or a post test  **THis is here to watch what happens it can be deleted after this works.
lvl1_filt1 <- subset(lvl1_filt, is.na(PRE.score)==FALSE | is.na(POST.score)== FALSE)

#create and save an unfiltered data set of matched test data
matched_unfiltered <- subset(lvl1_filt1, is.na(PRE.score)==FALSE & is.na(POST.score)==FALSE)
save(matched_unfiltered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_unfiltered")

#For students that took less than 300 seconds on the pre or post test 
lvl1_filt1$PRE.score[lvl1_filt1$PRE.Duration..Seconds.<300]<- NA
lvl1_filt1$POST.score[lvl1_filt1$POST.Duration..Seconds.<300]<- NA
#Calculate raw gains for filtered data
lvl1_filt1$absgain <- lvl1_filt1$POST.score - lvl1_filt1$PRE.score

#Calculate the Z scores for the absolute gains by instrument to filter out decreases greater than 2 SD
lvl1_filt1$Zabsgain <- ave(lvl1_filt1$absgain, lvl1_filt1$instrument, FUN=scale)
lvl1_filt1$POST.score[lvl1_filt1$Zabsgain< -2 ]<- NA

#Create and save a matched filtered data set
matched_filtered <- subset(lvl1_filt1, is.na(PRE.score)==FALSE & is.na(POST.score)== FALSE)
save(matched_filtered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_filtered")
# remove the students that did not provide either a pre or a post test  
lvl1_filt1 <- subset(lvl1_filt1, is.na(PRE.score)==FALSE | is.na(POST.score) == FALSE)


#######These numbers should add up to the number of observations in lvl1_filt1
#thing <- subset(lvl1_filt1, is.na(PRE.score)==FALSE  & is.na(POST.score)==FALSE)
#thing1 <- subset(lvl1_filt1, is.na(PRE.score)==TRUE  & is.na(POST.score)==FALSE)
#thing2 <- subset(lvl1_filt1, is.na(PRE.score)==FALSE  & is.na(POST.score)==TRUE)

####################GIves the N, SD, and Mean by instrument for absolute gains
#absgain.n<-aggregate(absgain2~instrument, lvl1_filt1,NROW)
#absgain.mean<-aggregate(absgain2~instrument, lvl1_filt1,mean)
#absgain.sd<-aggregate(absgain2~instrument, lvl1_filt1,sd)
#renames the columns so that they can be merged
#colnames(absgain.n) <- c("Assessment_Sequence_ID","N")
#colnames(absgain.mean) <- c("Assessment_Sequence_ID","Mean")
#colnames(absgain.sd) <- c("Assessment_Sequence_ID","SD")
#Combine Data sets
#absgain<-merge(absgain.mean,absgain.n)
#absgain<-merge(absgain,absgain.sd)

####################create outputs for filters
#lvl1$absgain <- lvl1$POST.score-lvl1$PRE.score
#lvl1$badgain<-with(lvl1,{ifelse(absgain<= "-20",1,ifelse(absgain> "-20",0,"na"))})
#lowpre <-subset(lvl1, PRE.Duration..Seconds. <300)
#plot(lowpre$PRE.Duration..Seconds.,lowpre$PRE.score)
#lowpost <-subset(lvl1_filt1, POST.Duration..Seconds. <2000 &lowpost$POST.score !="NA")
#plot(lowpost$POST.Duration..Seconds.,lowpost$POST.score)
#hist(lowpost$POST.Duration..Seconds.)
#badgain<-subset(lvl1, absgain< "-20" & POST.Duration..Seconds.<400)
#table(badgain$Assessment_Sequence_ID)

############################



#select variables we'll be using

#variable.names(lvl1_filt1) #it was easier to open the data.frame and compare to the list

imp_pre <- lvl1_filt1 %>%
  select(Assessment_Sequence_ID,PRE.Duration..Seconds.,POST.Duration..Seconds., PRE.score, POST.score, First_time, row,
         Year_in_school, PCA, IMCA, GCA, CINS, CCI, FMCE, BEMA, FCI, CSEM, LSCI, 
         male, female, transgender, other, Hispanic, black, white, asian, american_indian,hawaiian_or_other_pacific_islander, other.1, gender, instrument, race)

#New imputation code
#Right now we have 35% of the filtered data being complete cases. 84% of pre data and 43% of post data. It seems like 57 iterations is a good start, 65 might be more conservative
library(Amelia)

bds <- matrix(c(2, 3, 0, 0, 100, 100), nrow = 2, ncol = 3)

a.out <- amelia(imp_pre, m = 5, idvars = c("Assessment_Sequence_ID", "row", "gender", "instrument", "race"),
                ords = "Year_in_school", bounds = bds) 

lvl1_imp1 <- data.frame(a.out$imputations[[1]])
lvl1_imp2 <- data.frame(a.out$imputations[[2]])
lvl1_imp3 <- data.frame(a.out$imputations[[3]])
lvl1_imp4 <- data.frame(a.out$imputations[[4]])
lvl1_imp5 <- data.frame(a.out$imputations[[5]])

with(lvl1_imp1, cor(PRE.score,POST.score))
with(lvl1_imp2, cor(PRE.score,POST.score))
with(lvl1_imp3, cor(PRE.score,POST.score))
with(lvl1_imp4, cor(PRE.score,POST.score))
with(lvl1_imp5, cor(PRE.score,POST.score))


#Calculate Cohen's d, LGcourse, and LGind

lvl1_imp <- lvl1_imp1 %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(Assessment_Sequence_ID, POST.score, PRE.score, row, PCA, IMCA, GCA, CINS, CCI, FMCE, BEMA, FCI, CSEM, LSCI, male, female, Hispanic, black, white, asian, american_indian,hawaiian_or_other_pacific_islander) %>%
  na.omit() %>%
  mutate(n1=length(PRE.score[!is.na(PRE.score)]),
         n2=length(POST.score[!is.na(POST.score)]),
         sd1=sd(PRE.score, na.rm=TRUE),
         sd2=sd(POST.score, na.rm=TRUE),
         CohensD=(POST.score-PRE.score)/sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2)),
         Preave=mean(PRE.score, na.rm = TRUE),
         Postave=mean(POST.score, na.rm = TRUE),
         LGcourse=(POST.score-PRE.score)/(100-Preave),
         LGind=(POST.score-PRE.score)/(100-PRE.score))

# filter = things to keep

lvl1_imp <- lvl1_imp %>%
  filter(CohensD < 4 , CohensD > -1)%>%
  filter(PRE.score < 100)
#filter("PRE.Duration..Seconds." > 300) 
#filter("POST.Duration..Seconds." > 300)
# "|" is an or, "," is an and
#filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%


#delete courses and students with <10 students (uses 2 datasets)

x <- lvl1_imp %>%
  select(Assessment_Sequence_ID, CohensD) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)

lvl1_imp <- lvl1_imp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

lvl2_imp <- lvl2_imp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)



#multidimension outlier analysis

library(mvoutlier)

lvl1_3d <- lvl1_imp %>%
  ungroup() %>%
  select(CohensD,LGind,LGcourse)

thingiwant <- aq.plot(lvl1_3d, alpha=0.1, quan=0.9)
lvl1_imp$outliers <- thingiwant$outliers

#Plots

with(lvl1_imp, plot(CohensD, LGcourse))
with(lvl1_imp, plot(LGind, LGcourse))
with(lvl1_imp, plot(CohensD, LGind))

library(rgl)

with(filter(lvl1_imp, LGind>-2), plot3d(LGind, LGcourse, CohensD, col=factor(outliers, labels=c(1,2))))
