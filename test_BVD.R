#This is just a test script

library(dplyr)

#BVD read in (laptop)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (home)
lvl1 <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/benvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (home)
lvl1 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")


# Filtering
?filter # filter = things to keep

newdf <- df %>%
  filter(Effect.size < 4 , Effect.size > -2)%>% # "|" is an or, "," is an and
  filter(Instrument == 7 | Instrument == 8)  
##filter(`Effect.size` > -2) # %>% # remove the previous # to add steps
## filter(FCI == 1, FMCE == 1) %>%
## filter(FMCE == 1)
## filter(numstudent > 10) %>%
## filter(`PRE duration` > 300) %>%
## filter(`POST duration` > 300) %>%



write.csv(newdf, "~/drive/consulting/ben_van_dusen/amended_exampledata.csv")

#Multiple Linear Regression

fit.equation <- lm(Effect.size ~ instrument + URM*LApresence, data=fci) #"*" is interaction effect and the terms indepedantly

#count courses and students in specific conditions (i.e. by instrument with and without LAs)

lvl2 %>%
  group_by(Instrument, URM, LApresence) %>%
  summarise(n=n())

#How to bring variables over from lvl 2 into lvl 1 file

#How to turn dummy variables into multiple answer variables

#replace NA with calculated value (course average or MLR prediction)

dump$Race <- as.factor(dump$Race)

postpred <- lm(Post.Score ~ Pre.Score + Race, data=dump)

summary(postpred)

NApost <- filter(dump, is.na(Post.Score)) #== is question, = is setting the value

matched <- match(NApost$Student.ID, dump$Student.ID)

dump$Post.Score[matched] <- predict(postpred, newdata=NApost)

#delete courses and students with <10 students (uses 2 datasets)


x <- dump %>%
  select(Course.ID, Effect.Size) %>%
  na.omit() %>%
  group_by(Course.ID) %>%
  summarise(n=n()) %>%
  filter(n > 10)


newdump <- dump %>%
  filter(Course.ID %in% x$Course.ID)


#calculate LG(class)  This is a work in progress
#(post-pre)/(1-pre)

dump$LGclass <- as.numeric()

#how to show values in variable

unique(dump$Race)
