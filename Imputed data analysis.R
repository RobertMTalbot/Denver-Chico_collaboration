#imputed data analysis

library(dplyr)
library(nlme)

#BVD read in (laptop & home)
lvl1_imp <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2_imp <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1_imp <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2_imp <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1_imp$row <- 1:nrow(lvl1_imp)

#turn dummy variable in a single factor

lvl1_imp$gender <- factor(lvl1_imp$male*1 + lvl1_imp$female*2 + lvl1_imp$transgender*3 + lvl1_imp$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))

lvl2_imp$instrument <- factor(lvl2_imp$PCA*1 + lvl2_imp$IMCA*2 + lvl2_imp$GCA*3 + lvl2_imp$CINS*4 + lvl2_imp$GCIICS*5 + lvl2_imp$CCI*6 + lvl2_imp$FMCE*7 + lvl2_imp$BEMA*8 + lvl2_imp$FCI*9 + lvl2_imp$CSEM*10 + lvl2_imp$LSCI*11, labels = c("PCA", "IMCA", "GCA", "CINS", "GCIICS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))

lvl1_imp$race <- factor(lvl1_imp$white*1 + lvl1_imp$black*2 + lvl1_imp$asian*3 + lvl1_imp$american_indian*4 + lvl1_imp$hawaiian_or_other_pacific_islander*5 + lvl1_imp$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))

#white is Dom
lvl1_imp$race_URM <- factor(lvl1_imp$white*1 + lvl1_imp$black*2 + lvl1_imp$asian*2 + lvl1_imp$american_indian*2 + lvl1_imp$hawaiian_or_other_pacific_islander*2 + lvl1_imp$other.1*2, labels = c("NA", "Dom", "NonDom"))

#male is Dom
lvl1_imp$gender_URM <- factor(lvl1_imp$male*1 + lvl1_imp$female*2 + lvl1_imp$transgender*2 + lvl1_imp$other*2, labels = c("NA", "Dom", "NonDom"))


#Bring lvl2_imp into lvl1_imp

lvl1_imp <- left_join(lvl1_imp, lvl2_imp[,c("Assessment_Sequence_ID", "instrument")], by = "Assessment_Sequence_ID")

#filter for students
lvl1_imp <- lvl1_imp %>%
  filter(Student.or.LA == 0)
#filter(PRE.Duration..Seconds.>300 | PRE.Duration..Seconds.==NA)
#filter(PRE.score < 100 | PRE.score == NA)

#select variables we'll be using

variable.names(lvl1_imp)

lvl1_imptest <- lvl1_imp %>%
  select(Assessment_Sequence_ID, PRE.score, POST.score, First_time, Year_in_school, gender, race, instrument)

#New imputation code

library(Amelia)
?amelia

bds <- matrix(c(2, 3, 0, 0, 100, 100), nrow = 2, ncol = 3)
bds

a.out <- amelia(lvl1_imptest, m = 5, idvars = c("Assessment_Sequence_ID", "gender", "race", "instrument"),
                noms = NULL, ords = "Year_in_school", bounds = bds) 

save(a.out, file ="lvl1_imps.RData")

lvl1_impnew <- data.frame(a.out$imputations[[5]])

#cutdata <- lvl1_imp  %>%  
#select(POST.score, PRE.score, First_time)

#new_imp <- amelia(x=cutdata, m = 5) #This seems to work but I need to get it to write the data in the lvl1_imp file (plus pick which iteration)


#Old imputation code

boxplot(lvl1_imp$POST.score ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)
boxplot(lvl1_imp$PRE.score ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)


lvl1_imp_po <- lvl1_imp

postpred <- gls(POST.score ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument))
NApost <- filter(lvl1_imp_po, is.na(POST.score) & !is.na(PRE.score)) #== is question, = is setting the value
matched <- match(NApost$row, lvl1_imp_po$row)
lvl1_imp_po$POST.score[matched] <- predict(postpred, newdata=NApost)



lvl1_imp_popre <- lvl1_imp_po
prepred <- gls(PRE.score ~ POST.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) #predicting the pre
NApre <- filter(lvl1_imp_popre, is.na(PRE.score) & !is.na(POST.score)) #== is question, = is setting the value
matched <- match(NApre$row, lvl1_imp_popre$row)
lvl1_imp_popre$PRE.score[matched] <- predict(prepred, newdata=NApre)


#Calculate Cohen's d, LGcourse, and LGind

lvl1_imp <- lvl1_imp_popre %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(Assessment_Sequence_ID, POST.score, PRE.score, row, instrument, gender_URM, race_URM, race, gender, PRE.Duration..Seconds., POST.Duration..Seconds.) %>%
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


#Multiple Linear Regression by pre-score

dfit_imp_inst <- gls(CohensD ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(dfit_imp_inst)

LGindfit_imp_inst <- gls(LGind ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGindfit_imp_inst)

LGcoursefit_imp_inst <- gls(LGcourse ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGcoursefit_imp_inst)

#Multiple Linear Regression by gender and race

dfit_imp_inst <- gls(CohensD ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(dfit_imp_inst)

LGindfit_imp_inst <- gls(LGind ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGindfit_imp_inst)

LGcoursefit_imp_inst <- gls(LGcourse ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGcoursefit_imp_inst)


boxplot(lvl1_imp$CohensD ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)

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
