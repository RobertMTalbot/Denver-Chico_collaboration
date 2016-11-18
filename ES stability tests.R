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


estabper<-subset(estabper,estabper$N>9)
estabper<-subset(estabper,estabper$esd.assessment_sequence_id!=137 ) # Removes the CINS data to give the 89 courses used in the other analyses
estabper<-subset(estabper,estabper$esd.assessment_sequence_id!= 322)

#Making a box plot using the estabper data set

df <- subset(estabper, select = c(2,6,3,7,4,8,5,9))
df <- df*100
boxplot(df, at =c(1,2, 4,5, 7,8, 10,11), 
        col=c("red","royalblue2","red","royalblue2","red","royalblue2","red","royalblue2"), 
        ylim=c(0,120), outline=FALSE, xaxt="n", 
        xlab="Number of Outliers Added", ylab="Percentage Increase",
        main="Stability of Effect Size Measures")
        legend(1,114, c(expression(italic("d")),expression(bold("g"))), fill = c("red","royalblue2"))
axis(1,at=c(1.5,4.5,7.5,10.5), labels=c(1,2,3,4))


 # Running a repeated measures ANOVA on the data
 es_aov_dat <- estabper[1:2]
 es_aov_dat$outliers<-1
 es_aov_dat$es<-"d"
 colnames(es_aov_dat) <- c("id", "change", "outliers","es")
 
 tempx <- data.frame(estabper[1],estabper[3])
 tempx$outliers<-2
 tempx$es<-"d"
 colnames(tempx) <- c("id", "change", "outliers","es")
 
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
 tempx <- data.frame(estabper[1],estabper[4])
 tempx$outliers<-3
 tempx$es<-"d"
 colnames(tempx) <- c("id", "change", "outliers","es")
 
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
 tempx <- data.frame(estabper[1],estabper[5])
 tempx$outliers<-4
 tempx$es<-"d"
 colnames(tempx) <- c("id", "change", "outliers","es")
 
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
 ##
   tempx <- data.frame(estabper[1],estabper[6])
 tempx$outliers<-1
 tempx$es<-"g"
 colnames(tempx) <- c("id", "change", "outliers","es")
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
 tempx <- data.frame(estabper[1],estabper[7])
 tempx$outliers<-2
 tempx$es<-"g"
 colnames(tempx) <- c("id", "change", "outliers","es")
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
   tempx <- data.frame(estabper[1],estabper[8])
 tempx$outliers<-3
 tempx$es<-"g"
 colnames(tempx) <- c("id", "change", "outliers","es")
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)
 
 tempx <- data.frame(estabper[1],estabper[9])
 tempx$outliers<-4
 tempx$es<-"g"
 colnames(tempx) <- c("id", "change", "outliers","es")
 es_aov_dat<- rbind.data.frame(es_aov_dat,tempx)

###estab aov
  esstab.aov <- with(es_aov_dat,{aov(change~outliers*es)})
 summary(esstab.aov)

 median(estabper$d1)
 median(estabper$g1)
 
 median(estabper$g1)