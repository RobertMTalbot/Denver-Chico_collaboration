load(file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")
##Libraries
library(Hmisc)
library(relaimpo)
##Generate useful variables
esdata$delta_es <- with(esdata,{abs(d-(2.41*g_c))}) # 2.41 is the conversion from earlier analysis
esdata$gain <- esdata$post_score-esdata$pre_score

# Analysis
# relationships between d and g
rcorr(esdata$d,esdata$g_c)

fit<- lm(d~g_c, data = esdata)
summary.lm(fit)

fit<- lm(d~g_c+ pre_sd, data = esdata)
summary.lm(fit)

fit<- lm(g_c~ pre_score + pre_sd , data = esdata)
summary.lm(fit)




#relationships between es measures and class characteristics
rcorr(cbind(esdata$d,esdata$g_c,esdata$pre_score,esdata$post_score, esdata$gain , esdata$pre_sd,  esdata$post_sd))

fit<-lm(d~pre_score +pre_sd, data=esdata)
summary.lm(fit)

calc.relimp(d~pre_score +pre_sd, data=esdata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(d~ pre_score + pre_sd, data=esdata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

fit<-lm(g_c~pre_score, data=esdata)
summary.lm(fit)

calc.relimp(g_c~pre_score , data=esdata)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(g_c~pre_score , data=esdata)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
