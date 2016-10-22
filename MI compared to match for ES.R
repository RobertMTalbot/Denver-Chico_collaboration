

load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/esdata")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/fil_lvl2")
colnames(fil_lvl2)<- c("assessment_sequence_id","pre_score_ma","post_score_ma","pre_sd_ma","post_sd_ma","class_n_ma","g_i_ma","d_ma","g_c_ma")
library(dplyr)
joint<-left_join(esdata,fil_lvl2, by="assessment_sequence_id")

#########calculate deltas
joint$delta_pre_score <- (joint$pre_score - joint$pre_score_ma)
joint$delta_post_score <- (joint$post_score - joint$post_score_ma)
joint$delta_pre_sd <- (joint$pre_sd - joint$pre_sd_ma)
joint$delta_post_sd <- (joint$post_sd - joint$post_sd_ma)
joint$delta_class_n <- (joint$class_n - joint$class_n_ma)
joint$delta_d <- (joint$d - joint$d_ma)
joint$delta_g_c <- (joint$g_c - joint$g_c_ma)


with(joint,cor(d,d_ma))
with(joint[joint$class_n_ma>9,],cor(d,d_ma))

with(joint,cor(g_c,g_c_ma))
with(joint[joint$class_n_ma>9,],cor(g_c,g_c_ma))

with(joint[joint$class_n_ma>9,],plot(d,d_ma))
with(joint[joint$class_n_ma<10,],points(d,d_ma, col=3))
abline(0,1)
abline(lm(joint$d_ma~joint$d),col=5)
