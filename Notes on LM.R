MIlinreg<-function(x){ #Make a list of the Variables with IV as first
  fit<-summary(lm(x[[1]]~x[[2]]+x[[3]]+x[[4]]+x[[5]]))
}

m=42 #number of imputations
l=2 #number of dependent variables  
Estimate<-c(1:2)
Std.Error <- c(1:2)
t.value <- c(1:2)
p. <- c(1:2)
Xintercept<-data.frame(Estimate,Std.Error,t.value,p.)
Xd<-data.frame(Estimate,Std.Error,t.value,p.)
#

for (i in 2:(m+1)) {
  fit<-summary(lm(class.g[[i]]~class.d[[i]])) 
  for (j in 1:4) {
  Xintercept[(i-1),j]<-fit$coefficients[1,j] 
  Xd[(i-1),j]<-fit$coefficients[2,j] 
  #add more variables here
  }
  }
print("Intercept")
print(colMeans(Xintercept))
print("d")
print(colMeans(Xd))

rm(Xintercept)
rm(Xd)
rm(i)
rm(j)

i=2
fit<-summary(lm(class.g[[i]]~class.d[[i]])) #add more variables here
for (j in 1:4) {
  Xintercept[i-1,j]<-fit$coefficients[1,j] 
  Xd[(i-1),j]<-fit$coefficients[2,j] 
  #add more variables here
}


modvar<-list(class.g[[2]],class.d[[2]])
rm(fit)
MIlinreg(modvar)

fit<- summary(lm(class.g[[2]]~class.d[[2]]))
fit

str(fit)
fit$sigma
fit$df
fit$fstatistic
