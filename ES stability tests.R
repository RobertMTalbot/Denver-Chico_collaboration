load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/aout")
m=52
temp <- data.frame(a.out$imputations[[1]])
data<-data.frame(temp[c("assessment_sequence_id", "pre_score", "post_score")])
temp1<-aggregate(pre_score~assessment_sequence_id, data,mean)
temp1[3]<-c(100)
colnames(temp1)<-c("assessment_sequence_id", "pre_score", "post_score")
data1 <- rbind(data, temp1)
data2 <- rbind(data1, temp1)
data3 <- rbind(data2, temp1)
data4 <- rbind(data3, temp1)
data5 <- rbind(data4, temp1)



esd<- aggregate(pre_score~assessment_sequence_id, data, mean)
dat<- aggregate(post_score~assessment_sequence_id, data, mean)
esd<- merge(esd,dat, by="assessment_sequence_id")
dat<- aggregate(pre_score~assessment_sequence_id, data, mean)
esd<- merge(esd,dat, by="assessment_sequence_id")
dat<- aggregate(post_score~assessment_sequence_id, data, mean)
esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(pre_score~assessment_sequence_id, data1, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data1, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(pre_score~assessment_sequence_id, data1, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data1, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")  

  dat<- aggregate(pre_score~assessment_sequence_id, data2, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data2, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(pre_score~assessment_sequence_id, data2, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data2, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")  
  
  dat<- aggregate(pre_score~assessment_sequence_id, data3, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data3, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(pre_score~assessment_sequence_id, data3, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data3, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id") 
  
  dat<- aggregate(pre_score~assessment_sequence_id, data4, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data4, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(pre_score~assessment_sequence_id, data4, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")
  dat<- aggregate(post_score~assessment_sequence_id, data4, mean)
  esd<- merge(esd,dat, by="assessment_sequence_id")  
 
dat<- aggregate(post_score~assessment_sequence_id, data, NROW)
esd<- merge(esd,dat, by="assessment_sequence_id")   

colnames(esd$post_score)<-"N"  
################## effect sizes need to be fixed for the pooled variance

dstab<-data.frame(esd$assessment_sequence_id)
dstab$d0 <-(esd[,3]-esd[,2])/(esd[,4]/2+esd[,5]/2)
dstab$d1 <-(esd[,7]-esd[,6])/(esd[,8]/2+esd[,9]/2)
dstab$d2 <-(esd[,11]-esd[,10])/(esd[,12]/2+esd[,13]/2)
dstab$d3 <-(esd[,15]-esd[,14])/(esd[,16]/2+esd[,17]/2)
dstab$d4 <-(esd[,19]-esd[,18])/(esd[,20]/2+esd[,21]/2)

############
gstab<-data.frame(esd$assessment_sequence_id)
gstab$g0 <-(esd[,3]-esd[,2])/(100-esd[,2])
gstab$g1 <-(esd[,7]-esd[,6])/(100-esd[,6])
gstab$g2 <-(esd[,11]-esd[,10])/(100-esd[,10])
gstab$g3 <-(esd[,15]-esd[,14])/(100-esd[,14])
gstab$g4 <-(esd[,19]-esd[,18])/(100-esd[,18])

###########
gstabper <-data.frame(esd$assessment_sequence_id)
gstabper$g1 <- (gstab$g1-gstab$g0)/gstab$g0
gstabper$g2 <- (gstab$g2-gstab$g0)/gstab$g0
gstabper$g3 <- (gstab$g3-gstab$g0)/gstab$g0
gstabper$g4 <- (gstab$g4-gstab$g0)/gstab$g0


###########
dstabper <-data.frame(esd$assessment_sequence_id)
dstabper$d1 <- (dstab$d1-dstab$d0)/dstab$d0
dstabper$d2 <- (dstab$d2-dstab$d0)/dstab$d0
dstabper$d3 <- (dstab$d3-dstab$d0)/dstab$d0
dstabper$d4 <- (dstab$d4-dstab$d0)/dstab$d0

estabper<-merge(dstabper,gstabper, by= "esd.assessment_sequence_id")
estabper$N<-esd$post_score
library(xlsx)
write.xlsx(estabper, "/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/estabper.xlsx")
