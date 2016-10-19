load(file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")

esdata$absgain<-  esdata$post_score-esdata$pre_score

cor(esdata$pre_score,esdata$absgain)

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

with(esdata, cor(d,pre_score))
with(esdata, cor(g_c,pre_score))

with(esdata, cor(d,race_URM))
with(esdata, cor(g_c,race_URM))

with(esdata, cor(d,gender_URM))
with(esdata, cor(g_c,gender_URM))

with(esdata[esdata$Physics=="1",], plot(gender_URM, d))
