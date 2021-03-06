library(dplyr)
library(nlme)

#BVD read in (laptop)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

write.csv(newdf, "~/drive/consulting/ben_van_dusen/amended_exampledata.csv")

#Add row #s
lvl1$u <- 1:nrow(lvl1)

#Multiple Linear Regression

fit.equation <- gls(POST.score ~ PRE.score + gender + instrument, data=lvl1, weights=varIdent(form= ~1|instrument)) #"*" is interaction effect and the terms indepedantly

#count students in specific conditions (i.e. by instrument with and without LAs) - Need to add a course level count

combo %>%
  group_by(instrument, gender) %>%
  summarise(n=n())

#replace NA with calculated value (course average or MLR prediction)

boxplot(lvl1$POST.score ~ lvl1$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)

lvl1_po <- lvl1

postpred <- gls(POST.score ~ PRE.score + gender + instrument, data=lvl1_po, weights=varIdent(form= ~1|instrument))

summary(postpred)

NApost <- filter(lvl1_po, is.na(POST.score) & !is.na(PRE.score)) #== is question, = is setting the value

matched <- match(NApost$u, lvl1_po$u)

lvl1_po$POST.score[matched] <- predict(postpred, newdata=NApost)



lvl1_popre <- lvl1_po

prepred <- gls(PRE.score ~ POST.score + gender + instrument, data=lvl1_popre, weights=varIdent(form= ~1|instrument)) #predicting the pre

summary(prepred)

NApre <- filter(lvl1_popre, is.na(PRE.score) & !is.na(POST.score)) #== is question, = is setting the value

matched <- match(NApre$u, lvl1_popre$u)

lvl1_popre$PRE.score[matched] <- predict(prepred, newdata=NApre)


#how to show values in variable

unique(lvl1$male)

#Calculate Cohen's d, LGcourse, and LGind

lvl1_Calc <- lvl1_popre %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(POST.score, PRE.score, Effect.size, Assessment_Sequence_ID) %>%
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

# Filtering
?filter # filter = things to keep

lvl1_clean1 <- lvl1 %>%
  filter(Effect.size < 4 , Effect.size > -2)  # "|" is an or, "," is an and
#filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%
## filter(`PRE duration` > 300) %>%
## filter(`POST duration` > 300) %>%

#delete courses and students with <10 students (uses 2 datasets)

x <- lvl1 %>%
  select(Assessment_Sequence_ID, Effect.size) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)

lvl1_clean2 <- lvl1 %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

lvl2_clean2 <- lvl2 %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

#turn dummy variable in a single variable

lvl1$gender <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*3 + lvl1$other*4, labels = c("NA", "male", "female", "transgender", "other"))

lvl2$instrument <- factor(lvl2$PCA*1 + lvl2$IMCA*2 + lvl2$GCA*3 + lvl2$CINS*4 + lvl2$GCIICS*5 + lvl2$CCI*6 + lvl2$FMCE*7 + lvl2$BEMA*8 + lvl2$FCI*9 + lvl2$CSEM*10 + lvl2$LSCI*11, labels = c("PCA", "IMCA", "GCA", "CINS", "GCIICS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))

#Bring lvl2 into lvl1

combo <- left_join(lvl1, lvl2[,c("Assessment_Sequence_ID", "instrument")], by = "Assessment_Sequence_ID")

#plots

plot(lvl1_clean2$CohensD, lvl1_clean2$LGcourse)
plot(lvl1_clean2$CohensD, lvl1_clean2$LGind)
plot(lvl1_clean2$LGcourse, lvl1_clean2$LGind)

#multidimension outlier analysis

library(mvoutlier)

lvl1_3d <- lvl1_inp %>%
  ungroup() %>%
  select(CohensD,LGind,LGcourse)

lvl1_inp$outliers <- aq.plot(lvl1_3d, delta=qchisq(0.975, df=ncol(x)), quan=1/2, alpha=0.05)
thingiwant <- aq.plot(lvl1_3d, alpha=0.1, quan=0.9)
lvl1_inp$outliers <- thingiwant$outliers

plot(aq.plot)

#multidimension plots

library(plot3D)

scatter3D(x = lvl1_inp$LGind, y = lvl1_inp$LGcourse, z = lvl1_inp$CohensD, colvar = lvl1_inp$CohensD,
                    pch = 16, cex = 1.5, xlab = "LG individual", ylab = "LG Course",
                    zlab = "Cohen's d", 
                    ticktype = "detailed",
                    panel.first = NULL, theta = 10, d = 2,
                    colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))



library(rgl)

with(filter(lvl1_inp, LGind>-2), plot3d(LGind, LGcourse, CohensD, col=factor(outliers, labels=c(1,2))))
?rgl
