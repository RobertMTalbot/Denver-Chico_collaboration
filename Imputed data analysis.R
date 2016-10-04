#imputed data analysis

library(dplyr)
library(nlme)

#JMN Read in
lvl1 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (laptop & home)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1$row <- 1:nrow(lvl1)

#Bring lvl2_imp into lvl1_imp

lvl1 <- left_join(lvl1, lvl2[,c("Assessment_Sequence_ID", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "Assessment_Sequence_ID")

#filter for students
lvl1$PRE.Duration..Seconds.<-as.numeric(lvl1$PRE.Duration..Seconds.)
lvl1$POST.Duration..Seconds.<-as.numeric(lvl1$POST.Duration..Seconds.)

?as.numeric

lvl1_filt <- lvl1 %>%
  filter(Student.or.LA == 0) %>%
  filter(PRE.Duration..Seconds.>300 | PRE.Duration..Seconds.==1)%>%
  filter(POST.Duration..Seconds.>300 | POST.Duration..Seconds.==1)

 # filter(!is.na(POST.score) | !is.na(PRE.score))
#filter(PRE.score < 100 | PRE.score == NA)

#select variables we'll be using

variable.names(lvl1)

lvl1_cut <- lvl1_filt %>%
  select(Assessment_Sequence_ID, PRE.score, POST.score, First_time, row,
         Year_in_school, PCA, IMCA, GCA, CINS, CCI, FMCE, BEMA, FCI, CSEM, LSCI, 
         male, female, Hispanic, black, white, asian, american_indian,hawaiian_or_other_pacific_islander)

#New imputation code

library(Amelia)

bds <- matrix(c(2, 3, 0, 0, 100, 100), nrow = 2, ncol = 3)

a.out <- amelia(lvl1_cut, m = 5, idvars = c("Assessment_Sequence_ID", "row"),
                ords = "Year_in_school", bounds = bds) 

lvl1_imp1 <- data.frame(a.out$imputations[[1]])
lvl1_imp2 <- data.frame(a.out$imputations[[2]])
lvl1_imp3 <- data.frame(a.out$imputations[[3]])
lvl1_imp4 <- data.frame(a.out$imputations[[4]])
lvl1_imp5 <- data.frame(a.out$imputations[[5]])

with(lvl1_imp1, cor(PRE.score,POST.score))
with(lvl1_imp2, cor(PRE.score,POST.score))
with(lvl1_imp3, cor(PRE.score,POST.score))
with(lvl1_imp4, cor(PRE.score,POST.score))
with(lvl1_imp5, cor(PRE.score,POST.score))


#Calculate Cohen's d, LGcourse, and LGind

lvl1_imp <- lvl1_imp1 %>% 
  group_by(Assessment_Sequence_ID) %>%
  select(Assessment_Sequence_ID, POST.score, PRE.score, row, PCA, IMCA, GCA, CINS, CCI, FMCE, BEMA, FCI, CSEM, LSCI, male, female, Hispanic, black, white, asian, american_indian,hawaiian_or_other_pacific_islander) %>%
  na.omit() %>%
  mutate(n1=length(PRE.score[!is.na(PRE.score)]),
         n2=length(POST.score[!is.na(POST.score)]),
         sd1=sd(PRE.score, na.rm=TRUE),
         sd2=sd(POST.score, na.rm=TRUE),
         CohensD=(POST.score-PRE.score)/sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2)),
         Preave=mean(PRE.score, na.rm = TRUE),
         Postave=mean(POST.score, na.rm = TRUE),
         LGcourse=(POST.score-PRE.score)/(100-Preave),
         LGind=(POST.score-PRE.score)/(100-PRE.score))

# filter = things to keep

lvl1_imp <- lvl1_imp %>%
  filter(CohensD < 4 , CohensD > -1)%>%
  filter(PRE.score < 100)
#filter("PRE.Duration..Seconds." > 300) 
#filter("POST.Duration..Seconds." > 300)
# "|" is an or, "," is an and
#filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%


#delete courses and students with <10 students (uses 2 datasets)

x <- lvl1_imp %>%
  select(Assessment_Sequence_ID, CohensD) %>%
  na.omit() %>%
  group_by(Assessment_Sequence_ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)

lvl1_imp <- lvl1_imp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)

lvl2_imp <- lvl2_imp %>%
  filter(Assessment_Sequence_ID %in% x$Assessment_Sequence_ID)



#multidimension outlier analysis

library(mvoutlier)

lvl1_3d <- lvl1_imp %>%
  ungroup() %>%
  select(CohensD,LGind,LGcourse)

thingiwant <- aq.plot(lvl1_3d, alpha=0.1, quan=0.9)
lvl1_imp$outliers <- thingiwant$outliers

#Plots

with(lvl1_imp, plot(CohensD, LGcourse))
with(lvl1_imp, plot(LGind, LGcourse))
with(lvl1_imp, plot(CohensD, LGind))

library(rgl)

with(filter(lvl1_imp, LGind>-2), plot3d(LGind, LGcourse, CohensD, col=factor(outliers, labels=c(1,2))))
