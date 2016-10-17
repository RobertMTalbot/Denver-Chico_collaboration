load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl2_imp_ave")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/fil_lvl2")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/unfil_lvl2")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")
library(dplyr)
#x = MI, y= matched
miandmatch<- left_join(lvl2_imp_ave,fil_lvl2,by = "assessment_sequence_id")
miandmatch<- left_join(lvl2_imp_ave,lvl2,by = "assessment_sequence_id")

library(xlsx)
write.xlsx(miandmatch, "/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/miandmatch.xlsx") 

# plots of d for MI and matched data
with(miandmatch[miandmatch$class_n.y > 19,], plot(d.x, d.y, xlab= "d mi", ylab="d matched", xlim=c(0,3), ylim=c(0,3) ,main="n>19"))
with(miandmatch[miandmatch$class_n.y > 19,], abline(lm(d.y~d.x)))
abline(0,1, col=2)

with(miandmatch[miandmatch$class_n.y > 9,], lm(d.y~d.x))

# plots of g_c for MI and matched data
u=19
with(miandmatch[miandmatch$class_n.y > u,], plot(g_c.x, g_c.y, xlab= "g_c mi", ylab="g_c matched", xlim=c(0,1), ylim=c(0,1) ,main="n>19"))
with(miandmatch[miandmatch$class_n.y > u,], abline(lm(g_c.y~g_c.x)))
abline(0,1, col=2)

with(miandmatch[miandmatch$class_n.y > u,], lm(g_c.y~g_c.x))

# plots of pre and post for MI and matched data
u=9
with(miandmatch[miandmatch$class_n.y > u,], plot(post_score.x, post_score.y, xlab= "post mi", ylab="post matched", xlim=c(0,100), ylim=c(0,100) ,main="n>9"))
with(miandmatch[miandmatch$class_n.y > u,], abline(lm(post_score.y~post_score.x)))
abline(0,1, col=2)

with(miandmatch[miandmatch$class_n.y > u,], lm(post_score.y~post_score.x))


#make a data set of matched and MI data for comparing effect sizes between the two.
lvl2_imp_ave <- left_join(lvl2_imp_ave, lvl2, by = "assessment_sequence_id")


with(lvl2_imp_ave,plot(class_n,pre_score, main = "n=any for imputed data"))
with(fil_lvl2,plot(g_c,g_i, main = "n=any for filtered data"))
with(unfil_lvl2,plot(g_c,g_i, main = "n=any for unfiltered data"))


library(dplyr)
fil_lvl2 <-filter(fil_lvl2, class_n>9)
unfil_lvl2 <-filter(unfil_lvl2, class_n>9)
lvl2_imp_ave <-filter(lvl2_imp_ave, class_n>9)

#Bring lvl2_imp into lvl1_imp
lvl2_imp_ave <- left_join(lvl2_imp_ave, lvl2[,c("assessment_sequence_id", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI", "Physics" )], by = "assessment_sequence_id")


with(lvl2_imp_ave,plot(g_c,g_i, main = "n>9 for imputed data"))
with(fil_lvl2,plot(g_c,g_i, main = "n>9 for filtered data"))
with(unfil_lvl2,plot(g_c,g_i, main = "n>9 for unfiltered data"))

fil_lvl2 <-filter(fil_lvl2, class_n>24)
unfil_lvl2 <-filter(unfil_lvl2, class_n>24)
lvl2_imp_ave <-filter(lvl2_imp_ave, class_n>24)

with(lvl2_imp_ave,plot(g_c,g_i, main = "n>24 for imputed data"))
with(fil_lvl2,plot(g_c,g_i, main = "n>24 for filtered data"))
with(unfil_lvl2,plot(g_c,g_i, main = "n>24 for unfiltered data"))

with(lvl2_imp_ave,plot(g_c,d, main = "n>24 for imputed data"))
with(fil_lvl2,plot(g_c,d, main = "n>24 for filtered data"))
with(unfil_lvl2,plot(g_c,d, main = "n>24 for unfiltered data"))

with(fil_lvl2,plot(pre_score,d, main = "n>24 for filtered data"))
with(fil_lvl2,plot(pre_score,g_c, main = "n>24 for filtered data"))

with(lvl2_imp_ave,plot(pre_score,d, main = "n>24 for imputed data"))
with(lvl2_imp_ave,plot(pre_score, g_c, main = "n>24 for imputed data"))

with(lvl2_imp_ave[lvl2_imp_ave$FMCE="1"],cor(pre_score, g_c))
with(fil_lvl2,cor(pre_score, g_c))
with(lvl2_imp_ave[lvl2_imp_ave$class_n<40,], hist(class_n))


###################looking at disagreement between d and g_c
lvl2_imp_ave$hid_log <- ifelse(lvl2_imp_ave$g_c<0.4 & lvl2_imp_ave$d>1.5,1,0)
lvl2_imp_ave$hig_lod <- ifelse(lvl2_imp_ave$g_c>0.4 & lvl2_imp_ave$d<1.5,1,0)

### -1 is low d +1 is high d
lvl2_imp_ave$disagree <-lvl2_imp_ave$hid_log - lvl2_imp_ave$hig_lod
aggregate(post_score~disagree, lvl2_imp_ave, mean)

library(lattice)
histogram(~lvl2_imp_ave$pre_score | lvl2_imp_ave$disagree)

institution227 <- miandmatch[miandmatch$institution_id==227,]
institution227 <-filter(institution227, assessment_sequence_id!="134")
with(institution227, plot(d.x,g_c.x))

institution227lvl1raw<- filter(lvl1, assessment_sequence_id =="368")


### Produces a list of how many data sets an instructor provides
library(dplyr)
courses<-miandmatch %>%
group_by(instructor) %>%
  summarise(no_rows = length(pre_score.x))

