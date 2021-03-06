load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/aout")

m=52

temp <- data.frame(a.out$imputations[[1]])
temp <- temp[temp$gender_URM ==1,]
class.mean.pre<-  aggregate(pre_score~assessment_sequence_id, temp ,mean)
class.mean.pre<-with(temp[temp$gender_URM==1,],  aggregate(pre_score~assessment_sequence_id, temp ,mean)) 
class.mean.post<-with(temp[temp$gender_URM==1,],aggregate(post_score~assessment_sequence_id, temp,mean) )
class.sd.pre<-with(temp[temp$gender_URM==1,],aggregate(pre_score~assessment_sequence_id, temp,sd))
class.sd.post<-with(temp[temp$gender_URM==1,],aggregate(post_score~assessment_sequence_id, temp,sd))
class.n<-with(temp[temp$gender_URM==1,],aggregate(pre_score~assessment_sequence_id, temp,NROW)) 
pre.missing<-with(temp[temp$gender_URM==1,],aggregate(pre_missing~assessment_sequence_id, temp,mean))
post.missing<-with(temp[temp$gender_URM==1,],aggregate(post_missing~assessment_sequence_id, temp,mean))
missing<-with(temp[temp$gender_URM==1,],aggregate(missing~assessment_sequence_id, temp,mean))

##Getting the URM's into the data set
URMgender<-aggregate(gender_URM~assessment_sequence_id, temp,mean)
URMrace<-aggregate(race_URM~assessment_sequence_id, temp,mean)
URM <- merge(URMrace,URMgender, by = "assessment_sequence_id")


#These establish the base data frames that the other ones feed into. The N is the same for all so I will write it in later. THe next thing to do is write th for loops.

#This is the loops to generate the data frames for means, sds and g_i.
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==1,]
  class.mean.pre1<-aggregate(pre_score~assessment_sequence_id, temp,mean)
  class.mean.pre[i+1]<-class.mean.pre1$pre_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==1,]
  class.mean.post1<-aggregate(post_score~assessment_sequence_id, temp,mean) 
  class.mean.post[i+1]<-class.mean.post1$post_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==1,]
  class.sd.pre1<-with(temp[temp$gender_URM==1,],aggregate(pre_score~assessment_sequence_id, temp,sd) )
  class.sd.pre[i+1]<-class.sd.pre1$pre_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==1,]
  class.sd.post1<-aggregate(post_score~assessment_sequence_id, temp,sd) 
  class.sd.post[i+1]<-class.sd.post1$post_score
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


lvl2_imp_ave$class_n <- class.n$pre_score
lvl2_imp_ave$pre_missing <- pre.missing$pre_missing
lvl2_imp_ave$post_missing <- post.missing$post_missing
lvl2_imp_ave$missing <- missing$missing

#might want to add an RM function to clear up the workspace
rm( class.mean.post1, class.mean.pre1, class.sd.pre1, class.sd.post1, temp)

#colnames(lvl2_imp_ave)[1] <-"assessment_sequence_id"

#calculating effect sizes by class
lvl2_imp_ave$d<-with(lvl2_imp_ave,{(post_score-pre_score)/sqrt(((class_n-1)*pre_sd^2 + (class_n-1)*post_sd^2)/(class_n+class_n-2))})
lvl2_imp_ave$g_c<-with(lvl2_imp_ave,{(post_score-pre_score)/(100-pre_score)})

#######
library(dplyr)
lvl2_imp <-left_join
lvl2_imp <-filter(lvl2_imp_ave, class_n>9)
colnames(lvl2_imp_ave)[1]<-"assessment_sequence_id"
colnames(lvl2_imp)[1] <- "assessment_sequence_id"
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
lvl2_imp <- left_join(lvl2_imp, lvl2[,c("assessment_sequence_id", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "assessment_sequence_id")
lvl2_imp <-filter(lvl2_imp, CINS ==0) ## Removed this test because it produced high pre test outliers.

lvl2_imp<-merge(lvl2_imp,lvl2, by ="assessment_sequence_id")
lvl2_imp <-filter(lvl2_imp, CINS ==0)

esdata <- lvl2_imp

esdata <- left_join(esdata, URM, by="assessment_sequence_id")

esdata_f <- esdata
save(esdata_f,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata_m")

#########################
########################
########################

temp <- data.frame(a.out$imputations[[1]])
temp <- temp[temp$gender_URM ==0,]
class.mean.pre<-  aggregate(pre_score~assessment_sequence_id, temp ,mean)
class.mean.pre<-with(temp[temp$gender_URM ==0,],  aggregate(pre_score~assessment_sequence_id, temp ,mean)) 
class.mean.post<-with(temp[temp$gender_URM ==0,],aggregate(post_score~assessment_sequence_id, temp,mean) )
class.sd.pre<-with(temp[temp$gender_URM ==0,],aggregate(pre_score~assessment_sequence_id, temp,sd))
class.sd.post<-with(temp[temp$gender_URM ==0,],aggregate(post_score~assessment_sequence_id, temp,sd))
class.n<-with(temp[temp$gender_URM ==0,],aggregate(pre_score~assessment_sequence_id, temp,NROW)) 
pre.missing<-with(temp[temp$gender_URM ==0,],aggregate(pre_missing~assessment_sequence_id, temp,mean))
post.missing<-with(temp[temp$gender_URM ==0,],aggregate(post_missing~assessment_sequence_id, temp,mean))
missing<-with(temp[temp$gender_URM ==0,],aggregate(missing~assessment_sequence_id, temp,mean))

##Getting the URM's into the data set
URMgender<-aggregate(gender_URM~assessment_sequence_id, temp,mean)
URMrace<-aggregate(race_URM~assessment_sequence_id, temp,mean)
URM <- merge(URMrace,URMgender, by = "assessment_sequence_id")


#These establish the base data frames that the other ones feed into. The N is the same for all so I will write it in later. THe next thing to do is write th for loops.

#This is the loops to generate the data frames for means, sds and g_i.
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==0,]
  class.mean.pre1<-aggregate(pre_score~assessment_sequence_id, temp,mean)
  class.mean.pre[i+1]<-class.mean.pre1$pre_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==0,]
  class.mean.post1<-aggregate(post_score~assessment_sequence_id, temp,mean) 
  class.mean.post[i+1]<-class.mean.post1$post_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==0,]
  class.sd.pre1<-with(temp[temp$gender_URM ==0,],aggregate(pre_score~assessment_sequence_id, temp,sd) )
  class.sd.pre[i+1]<-class.sd.pre1$pre_score
}
for(i in 1:m) {
  temp <- data.frame(a.out$imputations[[i]])
  temp <- temp[temp$gender_URM ==0,]
  class.sd.post1<-aggregate(post_score~assessment_sequence_id, temp,sd) 
  class.sd.post[i+1]<-class.sd.post1$post_score
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


lvl2_imp_ave$class_n <- class.n$pre_score
lvl2_imp_ave$pre_missing <- pre.missing$pre_missing
lvl2_imp_ave$post_missing <- post.missing$post_missing
lvl2_imp_ave$missing <- missing$missing

#might want to add an RM function to clear up the workspace
rm( class.mean.post1, class.mean.pre1, class.sd.pre1, class.sd.post1, temp)

#colnames(lvl2_imp_ave)[1] <-"assessment_sequence_id"

#calculating effect sizes by class
lvl2_imp_ave$d<-with(lvl2_imp_ave,{(post_score-pre_score)/sqrt(((class_n-1)*pre_sd^2 + (class_n-1)*post_sd^2)/(class_n+class_n-2))})
lvl2_imp_ave$g_c<-with(lvl2_imp_ave,{(post_score-pre_score)/(100-pre_score)})

#######
library(dplyr)
lvl2_imp <-left_join
lvl2_imp <-filter(lvl2_imp_ave, class_n>9)
colnames(lvl2_imp_ave)[1]<-"assessment_sequence_id"
colnames(lvl2_imp)[1] <- "assessment_sequence_id"
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
lvl2_imp <- left_join(lvl2_imp, lvl2[,c("assessment_sequence_id", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "assessment_sequence_id")
lvl2_imp <-filter(lvl2_imp, CINS ==0) ## Removed this test because it produced high pre test outliers.

lvl2_imp<-merge(lvl2_imp,lvl2, by ="assessment_sequence_id")
lvl2_imp <-filter(lvl2_imp, CINS ==0)

esdata <- lvl2_imp

esdata <- left_join(esdata, URM, by="assessment_sequence_id")

esdata_m <- esdata
save(esdata_m,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata_m")


equity_data <- data.frame(esdata_f[1:11])
colnames(equity_data)<- c("assessment_sequence_id", "pre_score_f","post_score_f","pre_sd_f","post_sd_f","n_f","pre_missing_f", "post_missing_f" , "missing_f","d_f","g_c_f")
temp <- data.frame(esdata_m[1:11])
colnames(temp)<- c("assessment_sequence_id", "pre_score_m","post_score_m","pre_sd_m","post_sd_m","n_m","pre_missing_m", "post_missing_m" , "missing_m","d_m","g_c_m")
equity_data <- merge(equity_data,temp, by="assessment_sequence_id")

#######generating new variables

equity_data$delta_d <- equity_data$d_m - equity_data$d_f
equity_data$delta_g <- equity_data$g_c_m-equity_data$g_c_f
equity_data$coloring <- with( equity_data,ifelse(pre_score_f< pre_score_m, 1 ,0))
equity_data$quad <- with(equity_data, {ifelse(delta_g >0 & delta_d>0,1, ifelse(delta_g >0 & delta_d<0, 2, ifelse(delta_g <0 & delta_d<0,3,4)))})
equity_data$delta_pre <- equity_data$pre_score_m-equity_data$pre_score_f
equity_data$delta_pre_sd <- equity_data$pre_sd_m-equity_data$pre_sd_f
equity_data$delta_absgain <- with(equity_data, {post_score_m-pre_score_m -(post_score_f-pre_score_f)})
equity_data$delta_es <- with(equity_data,{abs(delta_d-(2.41*delta_g))}) # 2.41 is the conversion from earlier analysis

#Analysis
with(equity_data, plot(delta_d,delta_g, col=coloring+1, pch=coloring+24, main=expression("Comparison of Gender Differences for"~italic(d)~"and"~bold(g)), xlab=expression(italic("d")["Male"]*" - "*italic("d")["Female"]), ylab=expression(bold("g")["Male"]*" - "*bold("g")["Female"])   )) # I want to color code these by male greater than female on pre
abline(h=0)
abline(v=0)
legend("bottomright", c("F > M Pretest Mean","M > F Pretest Mean"), col=c(1,2), pch=c(24,25))

fil <- filter(equity_data, quad==2)

library(xlsx)
write.xlsx(fil, "/Users/kerstin/Desktop/quadII.xlsx") 

#Analysis to parse out the relationship between the disagreement in d and g with delat_pre_score and delta_pre_sd
fit <- lm(delta_es~ delta_pre + delta_pre_sd + delta_pre*delta_pre_sd, data=equity_data)
summary.lm(fit)

# Calculate Relative Importance for Each Predictor
library(relaimpo)

calc.relimp(delta_es~ delta_pre*delta_pre_sd + delta_pre + delta_pre_sd , data=equity_data)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(delta_es~ delta_pre*delta_pre_sd + delta_pre + delta_pre_sd , data=equity_data)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

#Correlations
library(Hmisc)
rcorr(with(equity_data,cbind(pre_score_f, pre_sd_f, d_f, g_c_f,pre_score_m, pre_sd_m, d_m, g_c_m )))

#More analysis for gender differences in FCI data
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
stuff<-merge(equity_data,lvl2, by="assessment_sequence_id")

with(stuff[stuff$FCI==1,], plot(post_score_f,post_score_m))
abline(0,1)
with(stuff[stuff$FCI==1,], plot(pre_score_f,pre_score_m))
abline(0,1)
