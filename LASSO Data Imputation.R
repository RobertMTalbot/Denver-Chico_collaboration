#select variables we'll be using

#variable.names(lvl1_filt1) #it was easier to open the data.frame and compare to the list
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl1_filt1")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

library(dplyr)
imp_pre <- lvl1_filt1 %>%
  select(assessment_sequence_id, pre_duration, post_duration, pre_score, post_score, first_time, row, year_in_school, instrument, race_URM, gender_URM, PCA,IMCA,GCA,CINS,CCI,FMCE,BEMA,FCI,CSEM,LSCI,pre_missing,post_missing,missing)

#New imputation code
#Right now we have 35% of the filtered data being complete cases. 84% of pre data and 43% of post data. It seems like 57 iterations is a good start, 65 might be more conservative
#This is set up to be run with the data set in the working memory. If need be a line of code can be added to read in the data.

library(Amelia)
library(effsize)
bds <- matrix(c(4, 5, 0, 0, 100, 100), nrow = 2, ncol = 3)

m=52

a.out <- amelia(imp_pre, m = m, idvars = c("assessment_sequence_id", "row", "instrument", "PCA", "pre_missing","post_missing","missing", "CINS" ),
                ords = "year_in_school", bounds = bds) 
# add the individual gain to the imputed data
save(a.out,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/aout")




load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/aout")
m=52
library(Amelia)
library(effsize)
a.out<-transform(a.out, g_i=ifelse(pre_score==100, NA, (post_score-pre_score)/(100-pre_score)))
a.out<-transform(a.out, c_i=ifelse(pre_score==100, NA,ifelse(pre_score>post_score, (post_score-pre_score)/pre_score,ifelse(pre_score<post_score,(post_score-pre_score)/(100-pre_score), NA))))

temp <- data.frame(a.out$imputations[[1]])
class.mean.pre<-aggregate(pre_score~assessment_sequence_id, temp,mean) 
class.mean.post<-aggregate(post_score~assessment_sequence_id, temp,mean) 
class.sd.pre<-aggregate(pre_score~assessment_sequence_id, temp,sd)
class.sd.post<-aggregate(post_score~assessment_sequence_id, temp,sd)
class.g_i<-aggregate(g_i~assessment_sequence_id, temp,mean) 
class.n<-aggregate(pre_score~assessment_sequence_id, temp,NROW) 
pre.missing<-aggregate(pre_missing~assessment_sequence_id, temp,mean)
post.missing<-aggregate(post_missing~assessment_sequence_id, temp,mean)
missing<-aggregate(missing~assessment_sequence_id, temp,mean)
class.c_i<-aggregate(c_i~assessment_sequence_id, temp,mean) 
##Getting the URM's into the data set
URMgender<-aggregate(gender_URM~assessment_sequence_id, temp,mean)
URMrace<-aggregate(race_URM~assessment_sequence_id, temp,mean)
URM <- merge(URMrace,URMgender, by = "assessment_sequence_id")


#These establish the base data frames that the other ones feed into. The N is the same for all so I will write it in later. THe next thing to do is write th for loops.

#This is the loops to generate the data frames for means, sds and g_i.
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.mean.pre1<-aggregate(pre_score~assessment_sequence_id, temp,mean) 
  class.mean.pre[i+1]<-class.mean.pre1$pre_score
  }
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.mean.post1<-aggregate(post_score~assessment_sequence_id, temp,mean) 
  class.mean.post[i+1]<-class.mean.post1$post_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.sd.pre1<-aggregate(pre_score~assessment_sequence_id, temp,sd) 
  class.sd.pre[i+1]<-class.sd.pre1$pre_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.sd.post1<-aggregate(post_score~assessment_sequence_id, temp,sd) 
  class.sd.post[i+1]<-class.sd.post1$post_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.g_i1<-aggregate(g_i~assessment_sequence_id, temp,mean) 
  class.g_i[i+1]<-class.g_i1$g_i
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  class.c_i1<-aggregate(c_i~assessment_sequence_id, temp,mean) 
  class.c_i[i+1]<-class.c_i1$c_i
}

#This averages across all of the variables and brings them together
lvl2_imp_ave <-data.frame(class.mean.pre$assessment_sequence_id)
class.mean.pre <-within(class.mean.pre, rm("assessment_sequence_id"))
lvl2_imp_ave$pre_score <- rowMeans(class.mean.pre)

class.mean.post <-within(class.mean.post, rm("assessment_sequence_id"))
lvl2_imp_ave$post_score <- rowMeans(class.mean.post)

class.sd.pre <-within(class.sd.pre, rm("assessment_sequence_id"))
lvl2_imp_ave$pre_sd <- rowMeans(class.sd.pre)

class.sd.post <-within(class.sd.post, rm("assessment_sequence_id"))
lvl2_imp_ave$post_sd <- rowMeans(class.sd.post)

class.g_i <-within(class.g_i, rm("assessment_sequence_id"))
lvl2_imp_ave$g_i <- rowMeans(class.g_i)

class.c_i <-within(class.c_i, rm("assessment_sequence_id"))
lvl2_imp_ave$c_i <- rowMeans(class.c_i)

lvl2_imp_ave$class_n <- class.n$pre_score
lvl2_imp_ave$pre_missing <- pre.missing$pre_missing
lvl2_imp_ave$post_missing <- post.missing$post_missing
lvl2_imp_ave$missing <- missing$missing

#might want to add an RM function to clear up the workspace
rm(class.g_i1, class.mean.post1, class.mean.pre1, class.sd.pre1, class.sd.post1, temp)

#colnames(lvl2_imp_ave)[1] <-"assessment_sequence_id"

#calculating effect sizes by class
lvl2_imp_ave$d<-with(lvl2_imp_ave,{(post_score-pre_score)/sqrt(((class_n-1)*pre_sd^2 + (class_n-1)*post_sd^2)/(class_n+class_n-2))})
lvl2_imp_ave$g_c<-with(lvl2_imp_ave,{(post_score-pre_score)/(100-pre_score)})

#######
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
library(dplyr)
colnames(lvl2_imp_ave)[1]<-"assessment_sequence_id"
lvl2_imp <-filter(lvl2_imp_ave, class_n>9)
colnames(lvl2_imp)[1] <- "assessment_sequence_id"
lvl2_imp<-merge(lvl2_imp,lvl2, by ="assessment_sequence_id")
lvl2_imp <-filter(lvl2_imp, CINS ==0)

esdata <- lvl2_imp

esdata <- left_join(esdata, URM, by="assessment_sequence_id")
save(esdata,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")

save(lvl2_imp_ave,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl2_imp_ave_miss_fil")


############## Data analysis on the MI data
#Correlations between the three g's

#Correlations between d, g, prescore, post score, absgain, pre sd, post sd

# MLR between g, d, pre score, post score
