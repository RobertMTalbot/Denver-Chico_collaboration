#I need to filter the courses down to those with >9 students 



load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/aout")
m=52
library(Amelia)
library(effsize)
library(miceadds)
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

#This is the loops to generate the data frames for the class level variables.
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

#make the course averages g
class.g  <- data.frame (class.mean.post$assessment_sequence_id)
for(i in 2:(m+1)) {
  class.g[i] <- (class.mean.post[i]-class.mean.pre[i])/(100-class.mean.pre[i])
}
#make the course averages d
class.d  <- data.frame (class.mean.post$assessment_sequence_id)
for(i in 2:(m+1)) {
  class.d[i] <- (class.mean.post[i]-class.mean.pre[i])/sqrt(((class.n[2]-1)*class.sd.pre[i]^2 + (class.n[2]-1)*class.sd.post[i]^2)/(2*class.n[2]))
  }
#make the course averages absolute gain
class.gain  <- data.frame (class.mean.post$assessment_sequence_id)
for(i in 2:(m+1)) {
  class.gain[i] <- (class.mean.post[i]-class.mean.pre[i])
}
###############
#Filters low N courses
library(dplyr)

colnames(class.n)<- c("assessment_sequence_id", "N")
colnames(class.d)[1]<-"assessment_sequence_id"
colnames(class.g)[1]<-"assessment_sequence_id"
colnames(class.gain)[1]<-"assessment_sequence_id"

class.c_i <- left_join(class.c_i,class.n, by="assessment_sequence_id")
class.d <- left_join(class.d,class.n, by="assessment_sequence_id")
class.g <- left_join(class.g,class.n, by="assessment_sequence_id")
class.g_i <- left_join(class.g_i,class.n, by="assessment_sequence_id")
class.mean.post <- left_join(class.mean.post,class.n, by="assessment_sequence_id")
class.mean.pre <- left_join(class.mean.pre,class.n, by="assessment_sequence_id")
class.sd.post <- left_join(class.sd.post,class.n, by="assessment_sequence_id")
class.sd.pre <- left_join(class.sd.pre,class.n, by="assessment_sequence_id")
class.gain <- left_join(class.gain,class.n, by="assessment_sequence_id")



#Removes low N courses
class.c_i <-class.c_i[class.c_i$N>9,]
class.d <-class.d[class.d$N>9,]
class.g <-class.g[class.g$N>9,]
class.g_i <-class.g_i[class.g_i$N>9,]
class.mean.post <-class.mean.post[class.mean.post$N>9,]
class.mean.pre <-class.mean.pre[class.mean.pre$N>9,]
class.sd.post <-class.sd.post[class.sd.post$N>9,]
class.sd.pre <-class.sd.pre[class.sd.pre$N>9,]
class.gain <-class.gain[class.gain$N>9,]

#generate teh correlations between g, gi, and ci
sink(file="/Users/kerstin/Documents/LA Postdoc stuff/dandg_results/cor_n_MLR", append=FALSE)


MIcor<-function(x,y,m){
  C <-data.frame(c(1:m))
  Uprime<-data.frame(c(1:m))
  for(i in 2:(m+1)) {
    fit<- cor.test(x[[i]],y[[i]])
    C[i-1,2]<-fit$estimate
    C[(i-1),3]<- (fit$estimate/fit$statistic)^2
    }
    U<-mean(C[[3]])
    B<-(1/(m-1))*var(C[[2]])
    Tp <-(1+1/m)*B+U
    Q<- mean(C[[2]])
    tt<-Q/sqrt(Tp)
    vv<- (m-1)*(1+U/((1+1/m)*B))^2
    pp<-dt(tt,vv)
    print("r=")
    print(Q)
    print("p=")
    print(pp)
  }
 

CV<-list(class.d,class.g,class.mean.pre,class.mean.post,class.gain,class.sd.pre,class.sd.post)
x=length(CV) # of variables for correlations to be calculated between
ZZZ<-data.frame(NA)
for (i in 1:x) {
  for (j in 1:x) {
    ZZZ[i,j]<- (MIcor(CV[[i]],CV[[j]]))
  }
}
colnames(ZZZ)<-c("class.d","class.g","class.mean.pre","class.mean.post","class.gain","class.sd.pre","class.sd.post")
print(ZZZ)
for (i in 1:x) {
  for (j in 1:x) {
    ZZZ[i,j]<- (MIcor.p(CV[[i]],CV[[j]],42))
  }
}
print(ZZZ)

CV<-list(class.c_i,class.g,class.g_i)
x=length(CV) # of variables for correlations to be calculated between
ZZZ<-data.frame(NA)
for (i in 1:x) {
  for (j in 1:x) {
    ZZZ[i,j]<- (MIcor(CV[[i]],CV[[j]]))
  }
}
colnames(ZZZ)<-c("class.c_i","class.g","class.g_i")
print(ZZZ)
for (i in 1:x) {
  for (j in 1:x) {
    ZZZ[i,j]<- (MIcor.p(CV[[i]],CV[[j]]))
  }
}
print(ZZZ)
sink()

#################
# MLR between g, d, pre score, post score
sink(file="/Users/kerstin/Documents/LA Postdoc stuff/dandg_results/MLR", append=FALSE)

###g and d
m=42 #number of imputations
l=2 #number of dependent variables  
Estimate<-c(1:2)
Std.Error <- c(1:2)
t.value <- c(1:2)
p. <- c(1:2)
rsq <- c(1:2)
adjrsq <- c(1:2)

Xestimate<-data.frame(Estimate,Std.Error,t.value,p.)
Xd<-data.frame(Estimate,Std.Error,t.value,p.)
Xpremean<-data.frame(Estimate,Std.Error,t.value,p.)
Xpresd<-data.frame(Estimate,Std.Error,t.value,p.)
Bnot<-data.frame(Estimate,Std.Error,t.value,p.)
Unot<-data.frame(Estimate,Std.Error,t.value,p.)
xdf<-data.frame(Estimate,Std.Error,t.value,p.)
Xvar<-data.frame(Estimate,Std.Error,t.value,p.)
Xest<-data.frame(Estimate,Std.Error,t.value,p.)
Xse<-data.frame(Estimate,Std.Error,t.value,p.)
modvar <-data.frame(Estimate,Std.Error,t.value,rsq,adjrsq)

for (i in 2:(m+1)) {
  fit<-summary(lm(class.g[[i]]~class.d[[i]])) 
  for (j in 1:2) {
    Xest[(i-1),j] <-  fit$coefficients[j,1]
    Xse[(i-1),j] <-  fit$coefficients[j,2]
    Xcov[(i-1),j]<- vcov(fit)[j,j]
  }
  modvar[(i-1),1] <- fit$fstatistic[[1]]
  modvar[(i-1),2] <- fit$fstatistic[[2]]
  modvar[(i-1),3] <- fit$fstatistic[[3]]
  modvar[(i-1),4] <- fit$r.squared
  modvar[(i-1),5] <- fit$adj.r.squared
}
Bnot<-colMeans(Xcov)
Unot[1,1] <-var(Xest[1])
Unot[1,2] <-var(Xest[2])
xdf1 <-(m-1)*(1+Unot[1,1]/((1+1/m)*Bnot[1]))^2
xdf2 <-(m-1)*(1+Unot[1,2]/((1+1/m)*Bnot[2]))^2

micombine.F(modvar$Estimate,mean(modvar$t.value))
print("Model Variables")
print(colMeans(modvar))
vvv <-mi.meld(Xest,Xse)
print(vvv)
2-2*pt((vvv$q.mi[1]/vvv$se.mi[1]),xdf1)
2-2*pt((vvv$q.mi[2]/vvv$se.mi[2]),xdf2)

#####
###g and d and prescore
for (i in 2:(m+1)) {
  fit<-summary(lm(class.g[[i]]~class.d[[i]]+class.sd.pre[[i]])) 
  for (j in 1:4) {
    Xintercept[(i-1),j]<-fit$coefficients[1,j] 
    Xd[(i-1),j]<-fit$coefficients[2,j] 
    Xpremean[(i-1),j]<-fit$coefficients[3,j] 
    #add more variables here
  }
  modvar[(i-1),1] <- fit$fstatistic[[1]]
  modvar[(i-1),2] <- fit$fstatistic[[2]]
  modvar[(i-1),3] <- fit$fstatistic[[3]]
  modvar[(i-1),4] <- fit$r.squared
  modvar[(i-1),5] <- fit$adj.r.squared
}
micombine.F(modvar$Estimate,mean(modvar$t.value))
print("Model Variables")
print(colMeans(modvar))
print("Intercept")
print(colMeans(Xintercept))
print("d")
print(colMeans(Xd))
print("premean")
print(colMeans(Xpremean))

#

#####
###g and d and presd

for (i in 2:(m+1)) {
  fit<-summary(lm(class.g[[i]]~class.d[[i]]+class.sd.pre[[i]])) 
  for (j in 1:4) {
    Xintercept[(i-1),j]<-fit$coefficients[1,j] 
    Xd[(i-1),j]<-fit$coefficients[2,j] 
    Xpresd[(i-1),j]<-fit$coefficients[3,j] 
    #add more variables here
  }
  modvar[(i-1),1] <- fit$fstatistic[[1]]
  modvar[(i-1),2] <- fit$fstatistic[[2]]
  modvar[(i-1),3] <- fit$fstatistic[[3]]
  modvar[(i-1),4] <- fit$r.squared
  modvar[(i-1),5] <- fit$adj.r.squared
}
micombine.F(modvar$Estimate,mean(modvar$t.value))
print("Model Variables")
print(colMeans(modvar))
print("Intercept")
print(colMeans(Xintercept))
print("d")
print(colMeans(Xd))
print("presd")
print(colMeans(Xpresd))

#####
###g and d and prescore
for (i in 2:(m+1)) {
  fit<-summary(lm(class.g[[i]]~class.d[[i]]+class.mean.pre[[i]]+class.sd.pre[[i]])) 
  #Xcov[[i]]<-vcov(fit)
  for (j in 1:4) {
    Xintercept[(i-1),j]<-fit$coefficients[1,j] 
    Xd[(i-1),j]<-fit$coefficients[2,j] 
    Xpremean[(i-1),j]<-fit$coefficients[3,j] 
    Xpresd[(i-1),j]<-fit$coefficients[4,j]
    #add more variables here
  }
  modvar[(i-1),1] <- fit$fstatistic[[1]]
  modvar[(i-1),2] <- fit$fstatistic[[2]]
  modvar[(i-1),3] <- fit$fstatistic[[3]]
  modvar[(i-1),4] <- fit$r.squared
  modvar[(i-1),5] <- fit$adj.r.squared
}
micombine.F(modvar$Estimate,mean(modvar$t.value))
print("Model Variables")
print(colMeans(modvar))
print("Intercept")
print(colMeans(Xintercept))
print("d")
print(colMeans(Xd))
print("premeans")
print(colMeans(Xpremean))
print("presd")
print(colMeans(Xpresd))



sink(modvar$rsq,Xcov)