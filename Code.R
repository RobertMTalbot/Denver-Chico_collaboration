library(dplyr)

df <- read.csv("/Users/bvandusen/Dropbox/work/Publications/2016/PERC/diversity/R/PairedPHYS_Cleaned_LASSO_2016June21.csv")
dump <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/datadump_5.24.16.csv")


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

#count students in specific conditions (i.e. by instrument with and without LAs) - Need to add a course level count

newdf %>%
  group_by(Instrument, URM, LApresence) %>%
  summarise(n=n())

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

#how to show values in variable

unique(dump$Race)

#calculate LG(class)  This is a work in progress

dump$LGclass <- as.numeric()

#Calculate Cohen's d

dump2 <- dump %>% 
  group_by(Course.ID) %>%
  select(Post.Score, Pre.Score, Effect.Size) %>%
  na.omit() %>%
  mutate(n1=length(Pre.Score[!is.na(Pre.Score)]),
         n2=length(Post.Score[!is.na(Post.Score)]),
         sd1=sd(Pre.Score, na.rm=TRUE),
         sd2=sd(Post.Score, na.rm=TRUE),
         CohensD=(Post.Score-Pre.Score)/sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2)),
         Preave=mean(Pre.Score, na.rm = TRUE),
         LGcourse=(Post.Score-Pre.Score)/(100-Preave),
         LGind=(Post.Score-Pre.Score)/(100-Pre.Score))

plot(dump2$CohensD, dump2$LGcourse)
plot(dump2$CohensD, dump2$LGind)
plot(dump2$LGcourse, dump2$LGind)
