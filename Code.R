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

write.csv(newdf, "~/drive/consulting/ben_van_dusen/amended_exampledata.csv")

#Multiple Linear Regression

fit.equation <- lm(Effect.size ~ male, data=lvl1_clean2) #"*" is interaction effect and the terms indepedantly

#count students in specific conditions (i.e. by instrument with and without LAs) - Need to add a course level count

lvl1_clean2 %>%
  group_by(Instrument, URM, LApresence) %>%
  summarise(n=n())

#replace NA with calculated value (course average or MLR prediction)

dump$Race <- as.factor(dump$Race)

lvl1_po <- lvl1

postpred <- lm(POST.score ~ PRE.score + male, data=lvl1_po)

summary(postpred)

NApost <- filter(lvl1_po, is.na(POST.score)) #== is question, = is setting the value

matched <- match(NApost$Student_ID, lvl1_po$Student_ID)

lvl1_po$POST.score[matched] <- predict(postpred, lvl1_po=NApost)

lvl1_popre <- lvl1_po

prepred <- lm(PRE.score ~ POST.score + male, data=lvl1_popre) #predicting the pre

summary(prepred)

NApre <- filter(lvl1_popre, is.na(PRE.score)) #== is question, = is setting the value

matched <- match(NApre$Student_ID, lvl1_popre$Student_ID)

lvl1_popre$PRE.score[matched] <- predict(prepred, lvl1_po=NApost)


#how to show values in variable

unique(lvl1$male)

#Calculate Cohen's d, LGcourse, and LGind

lvl1_Calc <- lvl1_popre %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(POST.score, PRE.score, Effect.size) %>%
  na.omit() %>%
  mutate(n1=length(PRE.score[!is.na(PRE.score)]),
         n2=length(POST.score[!is.na(POST.score)]),
         sd1=sd(PRE.score, na.rm=TRUE),
         sd2=sd(POST.score, na.rm=TRUE),
         CohensD=(POST.score-PRE.score)/sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2)),
         Preave=mean(PRE.score, na.rm = TRUE),
         LGcourse=(POST.score-PRE.score)/(100-Preave),
         LGind=(POST.score-PRE.score)/(100-PRE.score))

# Filtering
?filter # filter = things to keep

lvl1_clean1 <- lvl1_Calc %>%
  filter(Effect.size < 4 , Effect.size > -2)  # "|" is an or, "," is an and
#filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%
## filter(`PRE duration` > 300) %>%
## filter(`POST duration` > 300) %>%

#delete courses and students with <10 students (uses 2 datasets)

x <- lvl1_clean1 %>%
  select(Assessment_Sequence_ID, Effect.size) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)


lvl1_clean2 <- lvl1_clean1 %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)


plot(lvl1_clean2$CohensD, lvl1_clean2$LGcourse)
plot(lvl1_clean2$CohensD, lvl1_clean2$LGind)
plot(lvl1_clean2$LGcourse, lvl1_clean2$LGind)
