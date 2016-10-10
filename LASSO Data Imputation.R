#select variables we'll be using

#variable.names(lvl1_filt1) #it was easier to open the data.frame and compare to the list

imp_pre <- lvl1_filt1 %>%
  select(assessment_sequence_id, pre_duration, post_duration, pre_score, post_score, first_time, row,
         year_in_school, instrument, race_URM, gender_URM)

#New imputation code
#Right now we have 35% of the filtered data being complete cases. 84% of pre data and 43% of post data. It seems like 57 iterations is a good start, 65 might be more conservative
#This is set up to be run with the data set in the working memory. If need be a line of code can be added to read in the data.

library(Amelia)

bds <- matrix(c(4, 5, 0, 0, 100, 100), nrow = 2, ncol = 3)

a.out <- amelia(imp_pre, m = 5, idvars = c("assessment_sequence_id", "row" ),
                ords = "year_in_school", noms= c("instrument"), bounds = bds) 

unique(lvl1_filt1$race_URM)

lvl1_imp1 <- data.frame(a.out$imputations[[1]])
lvl1_imp2 <- data.frame(a.out$imputations[[2]])
lvl1_imp3 <- data.frame(a.out$imputations[[3]])
lvl1_imp4 <- data.frame(a.out$imputations[[4]])
lvl1_imp5 <- data.frame(a.out$imputations[[5]])

with(lvl1_imp1, cor(pre_score,post_score))
with(lvl1_imp2, cor(pre_score,post_score))
with(lvl1_imp3, cor(pre_score,post_score))
with(lvl1_imp4, cor(pre_score,post_score))
with(lvl1_imp5, cor(pre_score,post_score))

instrument.mean <-aggregate(pre_score~instrument, lvl1_imp1,mean)
instrument.mean$imp2a <-aggregate(pre_score~instrument, lvl1_imp2,mean)
instrument.mean$imp3a <-aggregate(pre_score~instrument, lvl1_imp3,mean)
instrument.mean$imp4a <-aggregate(pre_score~instrument, lvl1_imp4,mean)
instrument.mean$imp5a <-aggregate(pre_score~instrument, lvl1_imp5,mean)


instrument.postmean <-aggregate(post_score~instrument, lvl1_imp1,mean)
instrument.postmean$imp2a <-aggregate(post_score~instrument, lvl1_imp2,mean)
instrument.postmean$imp3a <-aggregate(post_score~instrument, lvl1_imp3,mean)
instrument.postmean$imp4a <-aggregate(post_score~instrument, lvl1_imp4,mean)
instrument.postmean$imp5a <-aggregate(post_score~instrument, lvl1_imp5,mean)
