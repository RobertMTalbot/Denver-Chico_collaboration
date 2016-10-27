# I need to calculate d and g_c for matched, unmatched and MI data
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_filtered")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl1_filt1")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")

#Need N, g_c, D for each course for each data set so nine variables
#threees<-data.frame(esdata$assessment_sequence_id)

############Unmatched Data Generator
absgain.n<-aggregate(pre_score~assessment_sequence_id, lvl1_filt1,NROW)
absgain.mean<-aggregate(pre_score~assessment_sequence_id, lvl1_filt1,mean)
absgain.sd<-aggregate(pre_score~assessment_sequence_id, lvl1_filt1,sd)
#renames the columns so that they can be merged
colnames(absgain.n) <- c("assessment_sequence_id","pre_n")
colnames(absgain.mean) <- c("assessment_sequence_id","pre_score")
colnames(absgain.sd) <- c("assessment_sequence_id","pre_sd")
#Combine Data sets
UM<-merge(absgain.mean,absgain.n)
UM<-merge(UM,absgain.sd)

absgain.n<-aggregate(post_score~assessment_sequence_id, lvl1_filt1,NROW)
absgain.mean<-aggregate(post_score~assessment_sequence_id, lvl1_filt1,mean)
absgain.sd<-aggregate(post_score~assessment_sequence_id, lvl1_filt1,sd)
#renames the columns so that they can be merged
colnames(absgain.n) <- c("assessment_sequence_id" ,"post_n")
colnames(absgain.mean) <- c("assessment_sequence_id" ,"post_score")
colnames(absgain.sd) <- c("assessment_sequence_id" ,"post_sd")
#Combine Data sets
UM<-merge(UM, absgain.mean)
UM<-merge(UM,absgain.n)
UM<-merge(UM,absgain.sd)

threeesum<-data.frame(UM$assessment_sequence_id)
threeesum$d_um <- with(UM, {(post_score-pre_score)/(sqrt(((pre_n-1)*pre_sd^2 + (post_n-1)*post_sd^2)/(pre_n+post_n-2)))})
threeesum$g_um <- with(UM, {(post_score-pre_score)/(100-pre_score)})
threeesum$pre_n_um <- UM$pre_n
threeesum$post_n_um <- UM$post_n

############ matched Data Generator
absgain.n<-aggregate(pre_score~assessment_sequence_id, matched_filtered,NROW)
absgain.mean<-aggregate(pre_score~assessment_sequence_id, matched_filtered,mean)
absgain.sd<-aggregate(pre_score~assessment_sequence_id,matched_filtered,sd)
#renames the columns so that they can be merged
colnames(absgain.n) <- c("assessment_sequence_id","pre_n")
colnames(absgain.mean) <- c("assessment_sequence_id","pre_score")
colnames(absgain.sd) <- c("assessment_sequence_id","pre_sd")
#Combine Data sets
MA<-merge(absgain.mean,absgain.n)
MA<-merge(MA,absgain.sd)

absgain.n<-aggregate(post_score~assessment_sequence_id, matched_filtered,NROW)
absgain.mean<-aggregate(post_score~assessment_sequence_id, matched_filtered,mean)
absgain.sd<-aggregate(post_score~assessment_sequence_id, matched_filtered,sd)
#renames the columns so that they can be merged
colnames(absgain.n) <- c("assessment_sequence_id" ,"post_n")
colnames(absgain.mean) <- c("assessment_sequence_id" ,"post_score")
colnames(absgain.sd) <- c("assessment_sequence_id" ,"post_sd")
#Combine Data sets
MA<-merge(MA, absgain.mean)
MA<-merge(MA,absgain.n)
MA<-merge(MA,absgain.sd)

threeesma<-data.frame(MA$assessment_sequence_id)
threeesma$d_ma <- with(MA, {(post_score-pre_score)/(sqrt(((pre_n-1)*pre_sd^2 + (post_n-1)*post_sd^2)/(pre_n+post_n-2)))})
threeesma$g_ma <- with(MA, {(post_score-pre_score)/(100-pre_score)})
threeesma$n_ma <- MA$pre_n
####this is what I am working on now
########
############ MI Data Generator 
threees<- data.frame(threeesum)
colnames(threees)[1] <- "assessment_sequence_id"
colnames(threeesma)[1] <- "assessment_sequence_id"
threees<- merge(threees,threeesma, by="assessment_sequence_id")
temp <- data.frame("assessment_sequence_id" = esdata$assessment_sequence_id, "d_mi" = esdata$d, "g_mi" = esdata$g_c, "n_mi"=esdata$class_n)
threees <- merge(threees, temp, by="assessment_sequence_id")
#Notes
#I think that I need to add the N's for each of these

#Calculating distances
threees$d_dis_mi_um <- with(threees,{(d_um-d_mi)})
threees$d_dis_mi_ma <- with(threees,{(d_ma-d_mi)})
threees$d_dis_ma_um <- with(threees,{abs(d_ma-d_um)})
with(threees, plot(d_dis_ma_um,d_dis_mi_um))
#greater than one is better for unmatched
threees$d_disratio <- threees$d_dis_ma_um/threees$d_dis_mi_um
library(dplyr)
threefilter <- filter(threees,n_ma>9)
mean(threefilter$d_disratio)
median(threefilter$d_disratio)
hist(threees$d_disratio[threees$d_disratio<10])
length(threefilter$d_disratio[threefilter$d_disratio>1])
length(threefilter$d_disratio[threefilter$d_disratio<1])
length(threefilter$d_disratio)

library(dplyr)
threees<-threees %>% mutate(rank = dense_rank(d_mi-d_ma))
with(threees, plot(rank,d_mi))
with(threees, points(rank,d_ma, col=4))
with(threees, points(rank,d_um, col=3))

with(threefilter,{cor(d_ma,d_um)})
with(threefilter,{cor(d_mi,d_um)})
with(threefilter,{cor(d_mi,d_ma)})

with(threefilter,{cor(g_ma,g_um)})
with(threefilter,{cor(g_mi,g_um)})
with(threefilter,{cor(g_mi,g_ma)})
##############makes plot for d
threefilter$d_dis_mi_um <- with(threefilter,{(d_um-d_mi)}) # sets g_mi as zero
threefilter$d_dis_mi_ma <- with(threefilter,{(d_ma-d_mi)}) # sets g_mi as zero

threefilter$temp <- abs(threefilter$d_dis_mi_um)-abs(threefilter$d_dis_mi_ma)
threefilter$ranka <-rank(threefilter$temp)
plot(threefilter$ranka,threefilter$temp, pch=19)
abline(h=0)
mean(threefilter$temp)
median(threefilter$temp)
#################### Plot for g_c
threefilter$g_dis_mi_um <- with(threefilter,{(g_um-g_mi)}) # sets g_mi as zero
threefilter$g_dis_mi_ma <- with(threefilter,{(g_ma-g_mi)}) # sets g_mi as zero

threefilter$tempg <- abs(threefilter$g_dis_mi_um)-abs(threefilter$g_dis_mi_ma) #calculates the difference in the distances just calculated with negative indicating that un matched is closer
threefilter$rankg <-rank(threefilter$tempg) #organizes the data nicely
plot(threefilter$rankg,threefilter$tempg, pch=19)
abline(h=0)
mean(threefilter$tempg)
median(threefilter$tempg)

threefilter$rankgg <-rank(abs(threefilter$g_dis_mi_ma)) #organizes the data nicely
with(threefilter, plot(rankgg,abs(g_dis_mi_ma)))
with(threefilter, points(rankgg,abs(g_dis_mi_um), col=3, pch=18))
