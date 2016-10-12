load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_filtered")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_unfiltered")
fil<-matched_filtered
fil$g_i <- with(fil, {ifelse(pre_score==100, NA, (post_score-pre_score)/(100-pre_score))})

fil_lvl2<-aggregate(pre_score~assessment_sequence_id, fil,mean) 
class.mean.post<-aggregate(post_score~assessment_sequence_id, fil,mean) 
class.sd.pre<-aggregate(pre_score~assessment_sequence_id, fil,sd)
class.sd.post<-aggregate(post_score~assessment_sequence_id, fil,sd)
class.g_i<-aggregate(g_i~assessment_sequence_id, fil,mean) 
class.n<-aggregate(pre_score~assessment_sequence_id, fil,NROW) 

colnames(class.sd.pre)[2]<-"pre_sd"
colnames(class.sd.post)[2]<-"post_sd"
colnames(class.n)[2]<-"class_n"

fil_lvl2 <-merge(fil_lvl2, class.mean.post)
fil_lvl2 <-merge(fil_lvl2, class.sd.pre)
fil_lvl2 <-merge(fil_lvl2, class.sd.post)
fil_lvl2 <-merge(fil_lvl2, class.n)
fil_lvl2 <-merge(fil_lvl2, class.g_i)



#calculating effect sizes by class
fil_lvl2$d<-with(fil_lvl2,{(post_score-pre_score)/sqrt(((class_n-1)*pre_sd^2 + (class_n-1)*post_sd^2)/(class_n+class_n-2))})
fil_lvl2$g_c<-with(fil_lvl2,{(post_score-pre_score)/(100-pre_score)})


unfil<-matched_unfiltered
unfil$g_i <- with(unfil, {ifelse(pre_score==100, NA, (post_score-pre_score)/(100-pre_score))})

unfil_lvl2<-aggregate(pre_score~assessment_sequence_id, unfil,mean) 
class.mean.post<-aggregate(post_score~assessment_sequence_id, unfil,mean) 
class.sd.pre<-aggregate(pre_score~assessment_sequence_id, unfil,sd)
class.sd.post<-aggregate(post_score~assessment_sequence_id, unfil,sd)
class.g_i<-aggregate(g_i~assessment_sequence_id, unfil,mean) 
class.n<-aggregate(pre_score~assessment_sequence_id, unfil,NROW) 

colnames(class.sd.pre)[2]<-"pre_sd"
colnames(class.sd.post)[2]<-"post_sd"
colnames(class.n)[2]<-"class_n"

unfil_lvl2 <-merge(unfil_lvl2, class.mean.post)
unfil_lvl2 <-merge(unfil_lvl2, class.sd.pre)
unfil_lvl2 <-merge(unfil_lvl2, class.sd.post)
unfil_lvl2 <-merge(unfil_lvl2, class.n)
unfil_lvl2 <-merge(unfil_lvl2, class.g_i)



#calculating effect sizes by class
unfil_lvl2$d<-with(unfil_lvl2,{(post_score-pre_score)/sqrt(((class_n-1)*pre_sd^2 + (class_n-1)*post_sd^2)/(class_n+class_n-2))})
unfil_lvl2$g_c<-with(unfil_lvl2,{(post_score-pre_score)/(100-pre_score)})


save(unfil_lvl2,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/unfil_lvl2")
save(fil_lvl2,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/fil_lvl2")

