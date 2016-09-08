#matched data analysis

library(dplyr)

#BVD read in (laptop)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (home)
lvl1 <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1$row <- 1:nrow(lvl1)

#turn dummy variable in a single factor

lvl1$gender <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*3 + lvl1$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))

lvl2$instrument <- factor(lvl2$PCA*1 + lvl2$IMCA*2 + lvl2$GCA*3 + lvl2$CINS*4 + lvl2$GCIICS*5 + lvl2$CCI*6 + lvl2$FMCE*7 + lvl2$BEMA*8 + lvl2$FCI*9 + lvl2$CSEM*10 + lvl2$LSCI*11, labels = c("PCA", "IMCA", "GCA", "CINS", "GCIICS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))

lvl1$race <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*3 + lvl1$american_indian*4 + lvl1$hawaiian_or_other_pacific_islander*5 + lvl1$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))

#white is Dom
lvl1$race_URM <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*2 + lvl1$american_indian*2 + lvl1$hawaiian_or_other_pacific_islander*2 + lvl1$other.1*2, labels = c("NA", "Dom", "NonDom"))

#male is Dom
lvl1$gender_URM <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*2 + lvl1$other*2, labels = c("NA", "Dom", "NonDom"))

#Bring lvl2 into lvl1

lvl1 <- left_join(lvl1, lvl2[,c("Assessment_Sequence_ID", "instrument")], by = "Assessment_Sequence_ID")

#Calculate Cohen's d, LGcourse, and LGind

lvl1 <- lvl1 %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(POST.score, PRE.score, row, instrument, gender_URM, race_URM, race, gender, PRE.Duration..Seconds., POST.Duration..Seconds.) %>%
  na.omit() %>%
  mutate(n1=length(PRE.score[!is.na(PRE.score)]),
         n2=length(POST.score[!is.na(POST.score)]),
         sd1=sd(PRE.score, na.rm=TRUE),
         sd2=sd(POST.score, na.rm=TRUE),
         CohensD=(POST.score-PRE.score)/sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2)),
         Preave=mean(PRE.score, na.rm = TRUE),
         LGcourse=(POST.score-PRE.score)/(100-Preave),
         LGind=(POST.score-PRE.score)/(100-PRE.score))

# filter = things to keep

lvl1 <- lvl1 %>%
  filter(CohensD < 4 , CohensD > -1) %>%
  filter(PRE.score < 100)
#filter("PRE.Duration..Seconds." > 300) %>%
#filter("POST.Duration..Seconds." > 300)
# "|" is an or, "," is an and
#filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%


#delete courses and students with <10 students (uses 2 datasets)

x <- lvl1 %>%
  select(Assessment_Sequence_ID, CohensD) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)

lvl1 <- lvl1 %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

lvl2 <- lvl2 %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)


#Multiple Linear Regression by pre-score

dfit <- lm(CohensD ~ PRE.score, data=lvl1) 

summary(dfit)

LGindfit <- lm(LGind ~ PRE.score, data=lvl1) 

summary(LGindfit)

LGcoursefit <- lm(LGcourse ~ PRE.score, data=lvl1) 

summary(LGcoursefit)

#Multiple Linear Regression by gender and race

dfit <- lm(CohensD ~ gender_URM + race_URM, data=lvl1) 

summary(dfit)

LGindfit <- lm(LGind ~ gender_URM + race_URM, data=lvl1) 

summary(LGindfit)

LGcoursefit <- lm(LGcourse ~ gender_URM + race_URM, data=lvl1) 

summary(LGcoursefit)
