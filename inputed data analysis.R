#inputed data analysis

library(dplyr)
library(nlme)

#BVD read in (laptop)
lvl1_inp <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2_inp <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (home)
lvl1_inp <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2_inp <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1_inp <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2_inp <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1_inp$row <- 1:nrow(lvl1_inp)

#turn dummy variable in a single factor

lvl1_inp$gender <- factor(lvl1_inp$male*1 + lvl1_inp$female*2 + lvl1_inp$transgender*3 + lvl1_inp$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))

lvl2_inp$instrument <- factor(lvl2_inp$PCA*1 + lvl2_inp$IMCA*2 + lvl2_inp$GCA*3 + lvl2_inp$CINS*4 + lvl2_inp$GCIICS*5 + lvl2_inp$CCI*6 + lvl2_inp$FMCE*7 + lvl2_inp$BEMA*8 + lvl2_inp$FCI*9 + lvl2_inp$CSEM*10 + lvl2_inp$LSCI*11, labels = c("PCA", "IMCA", "GCA", "CINS", "GCIICS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))

lvl1_inp$race <- factor(lvl1_inp$white*1 + lvl1_inp$black*2 + lvl1_inp$asian*3 + lvl1_inp$american_indian*4 + lvl1_inp$hawaiian_or_other_pacific_islander*5 + lvl1_inp$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))

#white is Dom
lvl1_inp$race_URM <- factor(lvl1_inp$white*1 + lvl1_inp$black*2 + lvl1_inp$asian*2 + lvl1_inp$american_indian*2 + lvl1_inp$hawaiian_or_other_pacific_islander*2 + lvl1_inp$other.1*2, labels = c("NA", "Dom", "NonDom"))

#male is Dom
lvl1_inp$gender_URM <- factor(lvl1_inp$male*1 + lvl1_inp$female*2 + lvl1_inp$transgender*2 + lvl1_inp$other*2, labels = c("NA", "Dom", "NonDom"))

#Bring lvl2_inp into lvl1_inp

lvl1_inp <- left_join(lvl1_inp, lvl2_inp[,c("Assessment_Sequence_ID", "instrument")], by = "Assessment_Sequence_ID")

#filter for students
lvl1_inp <- lvl1_inp %>%
  filter(Student.or.LA == 0)
  #filter(PRE.score < 100 | PRE.score == NA)

#replace NA with calculated value (course average or MLR prediction)

boxplot(lvl1_inp$POST.score ~ lvl1_inp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)

lvl1_inp_po <- lvl1_inp

postpred <- gls(POST.score ~ PRE.score + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument))

summary(postpred)

NApost <- filter(lvl1_inp_po, is.na(POST.score) & !is.na(PRE.score)) #== is question, = is setting the value

matched <- match(NApost$row, lvl1_inp_po$row)

lvl1_inp_po$POST.score[matched] <- predict(postpred, newdata=NApost)



lvl1_inp_popre <- lvl1_inp_po

prepred <- gls(PRE.score ~ POST.score + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) #predicting the pre

summary(prepred)

NApre <- filter(lvl1_inp_popre, is.na(PRE.score) & !is.na(POST.score)) #== is question, = is setting the value

matched <- match(NApre$row, lvl1_inp_popre$row)

lvl1_inp_popre$PRE.score[matched] <- predict(prepred, newdata=NApre)


#Calculate Cohen's d, LGcourse, and LGind

lvl1_inp <- lvl1_inp_popre %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(Assessment_Sequence_ID, POST.score, PRE.score, row, instrument, gender_URM, race_URM, race, gender, PRE.Duration..Seconds., POST.Duration..Seconds.) %>%
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

lvl1_inp <- lvl1_inp %>%
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

x <- lvl1_inp %>%
  select(Assessment_Sequence_ID, CohensD) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)

lvl1_inp <- lvl1_inp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

lvl2_inp <- lvl2_inp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)


#Multiple Linear Regression by pre-score

dfit_inp <- lm(CohensD ~ PRE.score, data=lvl1_inp) # no controlling for instrument differences
dfit_inp_inst <- gls(CohensD ~ PRE.score * instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(dfit_inp)
summary(dfit_inp_inst)

LGindfit_inp <- lm(LGind ~ PRE.score, data=lvl1_inp) # no controlling for instrument differences
LGindfit_inp_inst <- gls(LGind ~ PRE.score * instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(LGindfit_inp)
summary(LGindfit_inp_inst)

LGcoursefit_inp <- lm(LGcourse ~ PRE.score, data=lvl1_inp) # no controlling for instrument differences
LGcoursefit_inp_inst <- gls(LGcourse ~ PRE.score + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(LGcoursefit_inp)
summary(LGcoursefit_inp_inst)

#Multiple Linear Regression by gender and race

dfit_inp<- lm(CohensD ~ gender_URM + race_URM, data=lvl1_inp) # no controlling for instrument differences
dfit_inp_inst <- gls(CohensD ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(dfit_inp)
summary(dfit_inp_inst)

LGindfit_inp <- lm(LGind ~ gender_URM + race_URM, data=lvl1_inp) # no controlling for instrument differences
LGindfit_inp_inst <- gls(LGind ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(LGindfit_inp)
summary(LGindfit_inp_inst)

LGcoursefit_inp <- lm(LGcourse ~ gender_URM + race_URM, data=lvl1_inp) # no controlling for instrument differences
LGcoursefit_inp_inst <- gls(LGcourse ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_inp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences

summary(LGcoursefit_inp)
summary(LGcoursefit_inp_inst)
