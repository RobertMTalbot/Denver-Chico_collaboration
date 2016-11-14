#what is here
#similar analysis to Vankorff, lm for pooling data, correlations between es and pre, looking at individual instructors,
#linear regressions between es and pre and URM with nice table it porduces.
load(file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")
#lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
#esdata<-merge(esdata,lvl2, by ="assessment_sequence_id")
#esdata<-esdata[esdata$CINS==0,]

# generate IE code
esdata$acteng <- ifelse(esdata$lec_clickers==0 & esdata$lec_small_groups==0 & esdata$collab_learning_during_class==0, 0, 1)

esdata$absgain<-  esdata$post_score-esdata$pre_score


##analysis for VanKorff Paper
cor(esdata$pre_score,esdata$absgain)
cor(esdata$pre_score,esdata$g_c)
cor(esdata$pre_score,esdata$d)
fit<-lm(post_score~ pre_score, data=esdata)
summary.lm(fit)
plot(fit)

with(esdata[esdata$FCI==1 & esdata$acteng==1,], cor(pre_score, g_c))
  with(esdata[esdata$FCI==1 & esdata$acteng==1,], length(pre_score))
with(esdata[esdata$FCI==1 & esdata$acteng==0,], cor(pre_score, g_c))
  with(esdata[esdata$FCI==1 & esdata$acteng==0,], length(pre_score))
with(esdata[esdata$FMCE==1 & esdata$acteng==1,], cor(pre_score, g_c))
  with(esdata[esdata$FMCE==1 & esdata$acteng==1,], length(pre_score))
with(esdata[esdata$FMCE==1 & esdata$acteng==0,], cor(pre_score, g_c))
  with(esdata[esdata$FMCE==1 & esdata$acteng==0,], length(pre_score))

fit <-with(esdata[esdata$FCI==1 | esdata$FMCE==1,], aov(g_c~pre_score + FCI + acteng))
summary.aov(fit)
fit <-with(esdata[esdata$FCI==1 | esdata$FMCE==1,], aov(g_c~  FCI + acteng+pre_score))
summary.aov(fit)

fit <-with(esdata[esdata$FCI==1 | esdata$FMCE==1,], aov(d~pre_score + FCI + acteng))
summary.aov(fit)
fit <-with(esdata[esdata$FCI==1 | esdata$FMCE==1,], aov(d~ acteng + FCI + pre_score ))
summary.aov(fit)

with(esdata[esdata$FCI==1,],cor(pre_score,g_c))
with(esdata[esdata$FMCE==1,],cor(pre_score,g_c))

fit<- lm(g_c~  PCA + IMCA + CCI + FMCE + BEMA + FCI + CSEM + LSCI + pre_score , data=esdata)
summary.lm(fit)
fit<- lm(d~ pre_score + PCA + IMCA + CCI + FMCE + BEMA + FCI + CSEM + LSCI, data=esdata)
summary.lm(fit)

fit<- lm(g_c~ pre_score , data=esdata)
summary.lm(fit)
fit<- lm(d~ pre_score , data=esdata)
summary.lm(fit)

### Compare g_i and g_c
with(esdata, plot(g_c,g_i))
abline(0,1)
with(esdata, plot(pre_score, deltag))
with(esdata[esdata$class_n<60,], plot(class_n, deltag))
with(esdata, plot(post_missing, deltag))
abline(0,0)

### Compare g_c and d
with(esdata, lm(d~g_c))
with(esdata, cor(g_c,d))
with(esdata, plot(g_c,d))

with(esdata, plot(pre_score,d))
with(esdata, cor(pre_score,d))
with(esdata, lm(d~pre_score))
abline(with(esdata, lm(d~pre_score)))

with(esdata, plot(pre_score,g_c))
with(esdata, cor(pre_score,g_c))
with(esdata, lm(g_c~pre_score))
abline(with(esdata, lm(g_c~pre_score)))

###This is to create a data set for looking at the random versus teaching effects.
library(dplyr)
courses<-esdata %>%
  group_by(instructor) %>%
  summarise(no_rows = length(pre_score))

### quantiles of effect sizes
quantile(esdata$d)
quantile(esdata$g_c)
#### generate outlier variables
esdata$hidlog <-ifelse(esdata$d>1.294 & esdata$g_c<0.355, 1,0)
esdata$lodhig <-ifelse(esdata$d<1.294 & esdata$g_c>0.355, 1,0)
esdata$esdisagree <- esdata$hidlog-esdata$lodhig
aggregate(pre_score~esdisagree,data=esdata, mean)

with(esdata,plot(pre_score,post_score, col=esdisagree+2))
with(esdata[esdisagree=-1,],plot)

aggregate(pre_score~esdisagree,data=esdata, mean)
aggregate(pre_sd~esdisagree,data=esdata, mean)
aggregate(class_n~esdisagree,data=esdata, mean)

library(lattice)
histogram(~pre_score | esdisagree, data=esdata)


##Looking at individual instructors
with(esdata[esdata$instructor== "La Porta, Philip" & esdata$CSEM == 1,], plot(g_c,d, xlim = c(0,0.65), ylim = c(0,2.6),col=2))
with(esdata[esdata$instructor== "Smith, Anthony" & esdata$course_name != "General Physics I" & esdata$FCI == 1,], points(g_c,d,col=3))
with(esdata[esdata$institution_name == "Benedictine University",], points(g_c,d,col=4))
with(esdata[esdata$instructor== "Brasoveanu, Theodore",], points(g_c,d,col=5))
with(esdata[esdata$instructor== "Duffy, Andrew",], points(g_c,d,col=6))
#not sure how to normalize these two perhaps z scores.
drange= max(esdata$d)-min(esdata$d)
grange= max(esdata$g_c)-min(esdata$g_c)


### Linear regressions
lm(d~pre_score + race_URM + gender_URM, data=esdata)
lm(d~ race_URM + gender_URM, data=esdata)
lm(d~ gender_URM, data=esdata)
lm(d~ race_URM, data=esdata)
lm(d~pre_score , data=esdata)

lm(g_c~pre_score + race_URM + gender_URM, data=esdata)
lm(g_c~ race_URM + gender_URM, data=esdata)
lm(g_c~ gender_URM, data=esdata)
lm(g_c~ race_URM, data=esdata)
lm(g_c~pre_score , data=esdata)

#Relative importance of the variables
# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(g_c~pre_score + race_URM + gender_URM, data=esdata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(g_c~pre_score + race_URM + gender_URM, data=esdata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

calc.relimp(d~pre_score + race_URM + gender_URM, data=esdata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(d~pre_score + race_URM + gender_URM, data=esdata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

###Doing the correlations
library(dplyr)
physdataforcor <- filter(esdata, Physics==1)
phys <- data.frame("g_c" = physdataforcor$g_c, "d" = physdataforcor$d, "pre_score" = physdataforcor$pre_score, "gender_URM"=physdataforcor$gender_URM,"race_URM"=physdataforcor$race_URM) 
cor(notphys)

dataforcor <- data.frame("g_c" = esdata$g_c, "d" = esdata$d, "pre_score" = esdata$pre_score, "gender_URM"=esdata$gender_URM,"race_URM"=esdata$race_URM) 
cor(dataforcor)


with(esdata[esdata$Physics=="1",], plot(gender_URM, d))

#########gets at the useful information in the Linear regressions

fit<-lm(g_c~ pre_score + race_URM + gender_URM, data=esdata)
summary.lm(fit)
plot(fit)

with(esdata, plot(post_score, post_sd, xlim = c(0,100)))
with(esdata, points(pre_score, post_sd, col=4))

###########################################################
# Basic Scatterplot Matrix
pairs(~d+g_c+pre_score+gender_URM+race_URM,data=fcidata,main="Scatterplot Matrix for All Data")

cor(fcidata)

fit<-lm(d~ pre_score + race_URM + gender_URM, data=fcidata)
summary.lm(fit)
plot(fit)

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(d~pre_score + race_URM + gender_URM, data=fcidata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(d~pre_score + race_URM + gender_URM, data=fcidata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

calc.relimp(g_c~pre_score + race_URM + gender_URM, data=fcidata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(g_c~pre_score + race_URM + gender_URM, data=fcidata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 


###Analysis to look at disagrements between d and g in the esdata set
esdata$delta_es <- with(esdata,{abs(d-(2.41*g_c))}) # 2.41 is the conversion from earlier analysis


#Analysis to parse out the relationship between the disagreement in d and g with delat_pre_score and delta_pre_sd
fit <- lm(delta_es~ pre_score + pre_sd+ pre_score*pre_sd, data=esdata)
summary.lm(fit)

# Calculate Relative Importance for Each Predictor
library(relaimpo)

calc.relimp(delta_es~ pre_score + pre_sd+ pre_score*pre_sd, data=esdata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(delta_es~ pre_score + pre_sd+ pre_score*pre_sd, data=esdata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

 cor(esdata$pre_sd,esdata$pre_score)
 cor(esdata$pre_sd,esdata$d)
 cor(esdata$pre_score,esdata$d)
 
 library(Hmisc)
 rcorr(cbind(esdata$pre_score,esdata$pre_sd, esdata$d,esdata$g_c))
 plot(esdata$pre_score,esdata$pre_sd)
 