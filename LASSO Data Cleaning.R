#imputed data analysis

library(dplyr)
library(nlme)

#JMN Read in
lvl1 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (laptop & home)
lvl1 <- read.csv("/Users/bvd/Dropbox/work/Research/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Dropbox/work/Research/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1$row <- 1:nrow(lvl1)

#Bring lvl2_imp into lvl1_imp
lvl1 <- left_join(lvl1, lvl2[,c("assessment_sequence_id", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "assessment_sequence_id")
#Calculate absolute gain for filtering students later.
lvl1$absgain <- lvl1$post_score-lvl1$pre_score

#turn dummy variable in a single factor

#lvl1$gender <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*3 + lvl1$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))
lvl1$instrument <- factor(lvl1$PCA*1 + lvl1$IMCA*2 + lvl1$GCA*3 + lvl1$CINS*4 + lvl1$CCI*5 + lvl1$FMCE*6 + lvl1$BEMA*7 + lvl1$FCI*8 + lvl1$CSEM*9 + lvl1$LSCI*10, labels = c(NA, "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))
#lvl1$race <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*3 + lvl1$american_indian*4 + lvl1$hawaiian_or_other_pacific_islander*5 + lvl1$other.1*6 + lvl1$hispanic*7 , labels = c(NA, "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other","hispanic", "mixed", "mixed", "mixed", "mixed", "mixed", "mixed"))
#I commented this out because while it works it gives a bunch of mixed labels for different combinationso f variables.
#white is Dom
lvl1$race_URM <- ifelse(lvl1$hispanic + lvl1$black + lvl1$american_indian + lvl1$hawaiian_or_other_pacific_islander + lvl1$race_other >=1,1,0)
#male is Dom
lvl1$gender_URM <- ifelse(lvl1$male==1,0,1)

#Filter out LA's 
lvl1_filt <- lvl1 %>%
  filter(student_or_la == 0) #This also removes the students that provided no other information

###################################
#Remove students that did the CINS
lvl1_filt<-lvl1_filt[lvl1_filt$CINS==0,]

# remove the students that did not provide either a pre or a post test  **THis is here to watch what happens it can be deleted after this works.
lvl1_filt1 <- subset(lvl1_filt, is.na(pre_score)==FALSE | is.na(post_score)== FALSE)
actual_data <- subset(lvl1_filt, is.na(pre_score)==FALSE | is.na(post_score)== FALSE)
#create and save an unfiltered data set of matched test data
matched_unfiltered <- subset(lvl1_filt1, is.na(pre_score)==FALSE & is.na(post_score)==FALSE)
save(matched_unfiltered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_unfiltered")

############
#### filtering out the courses with too much missing data.
lvl1_filt1$pre_missing <- ifelse(is.na(lvl1_filt1$pre_score) == TRUE, 1,0)
lvl1_filt1$post_missing <- ifelse(is.na(lvl1_filt1$post_score) == TRUE, 1,0)
lvl1_filt1$missing <- ifelse(is.na(lvl1_filt1$pre_score) == TRUE | is.na(lvl1_filt1$post_score) == TRUE, 1,0)

pre.missing<-aggregate(pre_missing~assessment_sequence_id, lvl1_filt1,mean)
post.missing<-aggregate(post_missing~assessment_sequence_id, lvl1_filt1,mean)
missing <- merge(pre.missing,post.missing, by = "assessment_sequence_id")
colnames(missing)[2]<-"pre_missing_course"
colnames(missing)[3]<-"post_missing_course"
lvl1_filt1<-merge(lvl1_filt1,missing, by="assessment_sequence_id")

lvl1_filt_missing <- subset(lvl1_filt1,post_missing_course <0.99) # These were set to 0.99 just to remove any courses that did not do the post test.
lvl1_filt_missing_both <- subset(lvl1_filt_missing, pre_missing_course <0.99)
lvl1_filt1<-lvl1_filt_missing_both
students_start<-lvl1_filt1
pre_start <- subset(students_start,is.na(pre_score)==FALSE)
post_start <- subset(students_start,is.na(post_score)==FALSE)
match_start <- subset(students_start,is.na(post_score)==FALSE &is.na(pre_score)==FALSE)

courses_start <-table(lvl1_filt1$assessment_sequence_id)
courses_start<- as.data.frame(courses_start)
courses_start <- courses_time[courses_start$Freq>9,]
########
#For students that took less than 300 seconds on the pre or post test 
lvl1_filt1$pre_score[lvl1_filt1$pre_duration <300]<- NA
lvl1_filt1$post_score[lvl1_filt1$post_duration<300]<- NA
lvl1_filt1$pre_duration[lvl1_filt1$pre_duration <300]<- NA
lvl1_filt1$post_duration[lvl1_filt1$post_duration<300]<- NA
time_filtered <- subset(lvl1_filt1, is.na(pre_score)==FALSE | is.na(post_score)== FALSE)
courses_time <-table(time_filtered$assessment_sequence_id)
courses_time<- as.data.frame(courses_time)
courses_time <- courses_time[courses_time$Freq>9,]

time_pre <- subset(time_filtered,is.na(pre_score)==FALSE)
time_post <- subset(time_filtered,is.na(post_score)==FALSE)
time_match <- subset(time_filtered,is.na(post_score)==FALSE &is.na(pre_score)==FALSE)


lvl1_filt1$pre_score[lvl1_filt1$pre_answered <80]<- NA
lvl1_filt1$post_score[lvl1_filt1$post_answered<80]<- NA
answered_filtered <- subset(lvl1_filt1, is.na(pre_score)==FALSE | is.na(post_score)== FALSE)
courses_comp <-table(answered_filtered$assessment_sequence_id)
courses_comp<- as.data.frame(courses_comp)
courses_comp <- courses_comp[courses_comp$Freq>9,]

comp_pre <- subset(answered_filtered,is.na(pre_score)==FALSE)
comp_post <- subset(answered_filtered,is.na(post_score)==FALSE)
comp_match <- subset(answered_filtered,is.na(post_score)==FALSE &is.na(pre_score)==FALSE)


#Calculate raw gains for filtered data
lvl1_filt1$absgain <- lvl1_filt1$post_score - lvl1_filt1$pre_score


#Calculate the Z scores for the absolute gains by instrument to filter out decreases greater than 2 SD
lvl1_filt1$Zabsgain <- ave(lvl1_filt1$absgain, lvl1_filt1$instrument, FUN=scale)
lvl1_filt1$post_score[lvl1_filt1$Zabsgain< -2 ]<- NA
gain_filtered <- subset(lvl1_filt1, is.na(pre_score)==FALSE | is.na(post_score)== FALSE)
courses_gain <-table(gain_filtered$assessment_sequence_id)
courses_gain<- as.data.frame(courses_gain)
courses_gain <- courses_gain[courses_gain$Freq>9,]

gain_pre <- subset(gain_filtered,is.na(pre_score)==FALSE)
gain_post <- subset(gain_filtered,is.na(post_score)==FALSE)
gain_match <- subset(gain_filtered,is.na(post_score)==FALSE &is.na(pre_score)==FALSE)



#Create and save a matched filtered data set
matched_filtered <- subset(lvl1_filt1, is.na(pre_score)==FALSE & is.na(post_score)== FALSE)
save(matched_filtered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_filtered")
# remove the students that did not provide either a pre or a post test  
lvl1_filt1 <- subset(lvl1_filt1, is.na(pre_score)==FALSE | is.na(post_score) == FALSE)

#### filtering out the courses with too much missing data.
lvl1_filt1$pre_missing <- ifelse(is.na(lvl1_filt1$pre_score) == TRUE, 1,0)
lvl1_filt1$post_missing <- ifelse(is.na(lvl1_filt1$post_score) == TRUE, 1,0)
lvl1_filt1$missing <- ifelse(is.na(lvl1_filt1$pre_score) == TRUE | is.na(lvl1_filt1$post_score) == TRUE, 1,0)

pre.missing<-aggregate(pre_missing~assessment_sequence_id, lvl1_filt1,mean)
post.missing<-aggregate(post_missing~assessment_sequence_id, lvl1_filt1,mean)
missing <- merge(pre.missing,post.missing, by = "assessment_sequence_id")
colnames(missing)[2]<-"pre_missing_course1"
colnames(missing)[3]<-"post_missing_course1"
lvl1_filt1<-merge(lvl1_filt1,missing, by="assessment_sequence_id")

lvl1_filt_missing <- subset(lvl1_filt1,post_missing_course1 <0.60)
lvl1_filt_missing_both <- subset(lvl1_filt_missing, pre_missing_course1 <0.60)

lvl1_filt1<-lvl1_filt_missing_both
save(lvl1_filt1,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl1_filt1")

courses_final <-table(lvl1_filt1$assessment_sequence_id)
courses_final<- as.data.frame(courses_final)
courses_final <- courses_final[courses_final$Freq>9,]

final_pre <- subset(lvl1_filt1,is.na(pre_score)==FALSE)
final_post <- subset(lvl1_filt1,is.na(post_score)==FALSE)
final_match <- subset(lvl1_filt1,is.na(post_score)==FALSE &is.na(pre_score)==FALSE)


#############This cleans up the workspace
#rm(answered_filtered, gain_filtered, actual_data,time_filtered,lvl1_filt)


