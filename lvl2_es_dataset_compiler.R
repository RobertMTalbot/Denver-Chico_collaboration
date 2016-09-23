#This program generates a level 2 data set for either imputed or or non-imputed data. I haven't tested it with non-matched data.
#simply load your data sets, change their names(lines 9 and 10), change the save name (line 146) and run it.

#Read in Data
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl1inp")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl2inp")

### Change the name of the loaded data files for the script to run
#lvl1inp<-
#lvl2inp<-

### PRE for class level data
#generate the columns
class.n<-aggregate(PRE.score~Assessment_Sequence_ID, lvl1_inp,NROW) 
class.mean<-aggregate(PRE.score~Assessment_Sequence_ID, lvl1_inp,mean)
class.sd<-aggregate(PRE.score~Assessment_Sequence_ID, lvl1_inp,sd)


#renames the columns so that they can be merged
colnames(class.n) <- c("Assessment_Sequence_ID","Nclasspre")
colnames(class.mean) <- c("Assessment_Sequence_ID","Meanclasspre")
colnames(class.sd) <- c("Assessment_Sequence_ID","SDclasspre")

#Combine Data sets
esdat<-merge(class.mean,class.n)
esdat<-merge(esdat,class.sd)
esdat<-merge(esdat,class.mean)
esdat<-merge(esdat,class.n)
esdat<-merge(esdat,class.sd)

### POST for class level data
#generate the columns
class.n<-aggregate(POST.score~Assessment_Sequence_ID, lvl1_inp,NROW) 
class.mean<-aggregate(POST.score~Assessment_Sequence_ID, lvl1_inp,mean)
class.sd<-aggregate(POST.score~Assessment_Sequence_ID, lvl1_inp,sd)

#renames the columns so that they can be merged
colnames(class.n) <- c("Assessment_Sequence_ID","Nclasspost")
colnames(class.mean) <- c("Assessment_Sequence_ID","Meanclasspost")
colnames(class.sd) <- c("Assessment_Sequence_ID","SDclasspost")

#Combine Data sets
esdat<-merge(esdat,class.mean)
esdat<-merge(esdat,class.n)
esdat<-merge(esdat,class.sd)
esdat<-merge(esdat,class.mean)
esdat<-merge(esdat,class.n)
esdat<-merge(esdat,class.sd)

###for pre data
#selects female students and produces summary statistics in separate data sets
gender_URM_yes<-subset(lvl1_inp, gender_URM =="NonDom") 
fem.n<-aggregate(PRE.score~Assessment_Sequence_ID, gender_URM_yes,NROW) 
fem.mean<-aggregate(PRE.score~Assessment_Sequence_ID, gender_URM_yes,mean)
fem.sd<-aggregate(PRE.score~Assessment_Sequence_ID, gender_URM_yes,sd)

#renames the columns so that they can be merged
colnames(fem.n) <- c("Assessment_Sequence_ID","Nfempre")
colnames(fem.mean) <- c("Assessment_Sequence_ID","Meanfempre")
colnames(fem.sd) <- c("Assessment_Sequence_ID","SDfempre")


#selects male data and produces summary statistics in separate data sets
gender_URM_no<-subset(lvl1_inp, gender_URM =="Dom")
mal.n<-aggregate(PRE.score~Assessment_Sequence_ID,gender_URM_no,NROW)
mal.mean<-aggregate(PRE.score~Assessment_Sequence_ID,gender_URM_no,mean)
mal.sd<-aggregate(PRE.score~Assessment_Sequence_ID,gender_URM_no,sd)

#renames the columns so that they can be merged
colnames(mal.n) <- c("Assessment_Sequence_ID","Nmalpre")
colnames(mal.mean) <- c("Assessment_Sequence_ID","Meanmalpre")
colnames(mal.sd) <- c("Assessment_Sequence_ID","SDmalpre")

#Combine Data sets
esdat<-merge(esdat,fem.mean)
esdat<-merge(esdat,fem.n)
esdat<-merge(esdat,fem.sd)
esdat<-merge(esdat,mal.mean)
esdat<-merge(esdat,mal.n)
esdat<-merge(esdat,mal.sd)

###now to to the same things for the post test scores
#selects female students and produces summary statistics in separate data sets

fem.n<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_yes,NROW)
fem.mean<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_yes,mean)
fem.sd<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_yes,sd)

#renames the columns so that they can be merged
colnames(fem.n) <- c("Assessment_Sequence_ID","Nfempost")
colnames(fem.mean) <- c("Assessment_Sequence_ID","Meanfempost")
colnames(fem.sd) <- c("Assessment_Sequence_ID","SDfempost")

#selects male data and produces summary statistics in separate data sets
mal.n<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_no,NROW)
mal.mean<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_no,mean)
mal.sd<-aggregate(POST.score~Assessment_Sequence_ID, gender_URM_no,sd)

#renames the columns so that they can be merged
colnames(mal.n) <- c("Assessment_Sequence_ID","Nmalpost")
colnames(mal.mean) <- c("Assessment_Sequence_ID","Meanmalpost")
colnames(mal.sd) <- c("Assessment_Sequence_ID","SDmalpost")

###Calclating the LGind 
LGind.class<-aggregate(LGind~Assessment_Sequence_ID, lvl1_inp,mean)
LGind.class.fem<-aggregate(LGind~Assessment_Sequence_ID,gender_URM_yes,mean)
LGind.class.mal<-aggregate(LGind~Assessment_Sequence_ID,gender_URM_no,mean)

colnames(LGind.class) <- c("Assessment_Sequence_ID","LGindclass") 
colnames(LGind.class.fem) <- c("Assessment_Sequence_ID","LGindclassfem") 
colnames(LGind.class.mal) <- c("Assessment_Sequence_ID","LGindclassmal") 

#Combine Data sets
esdat<-merge(esdat,fem.mean)
esdat<-merge(esdat,fem.n)
esdat<-merge(esdat,fem.sd)
esdat<-merge(esdat,mal.mean)
esdat<-merge(esdat,mal.n)
esdat<-merge(esdat,mal.sd)
esdat<-merge(esdat,LGind.class)
esdat<-merge(esdat,LGind.class.fem)
esdatafinal<-merge(esdat,LGind.class.mal)


 
###Calculating effect sizes

#by class
esdatafinal$d.class<-with(esdatafinal,{(Meanclasspost-Meanclasspre)/sqrt(((Nclasspre-1)*SDclasspre^2 + (Nclasspost-1)*SDclasspost^2)/(Nclasspre+Nclasspost-2))})
esdatafinal$LGC.class<-with(esdatafinal,{(Meanclasspost-Meanclasspre)/(100-Meanclasspre)})

#cohen's d between genders
esdatafinal$d.mvf.pre<-((esdatafinal$Meanmalpre-esdatafinal$Meanfempre)/sqrt(((esdatafinal$Nfempre-1)*esdatafinal$SDfempre^2 + (esdatafinal$Nmalpre-1)*esdatafinal$SDmalpre^2)/(esdatafinal$Nmalpre+esdatafinal$Nfempre-2)))
esdatafinal$d.mvf.post<-((esdatafinal$Meanmalpost-esdatafinal$Meanfempost)/sqrt(((esdatafinal$Nfempost-1)*esdatafinal$SDfempost^2 + (esdatafinal$Nmalpost-1)*esdatafinal$SDmalpost^2)/(esdatafinal$Nmalpost+esdatafinal$Nfempost-2)))

#Cohen's d on genders
esdatafinal$d.mal<-((esdatafinal$Meanmalpost-esdatafinal$Meanmalpre)/sqrt(((esdatafinal$Nmalpre-1)*esdatafinal$SDmalpre^2 + (esdatafinal$Nmalpost-1)*esdatafinal$SDmalpost^2)/(esdatafinal$Nmalpre+esdatafinal$Nmalpost-2)))
esdatafinal$d.fem<-((esdatafinal$Meanfempost-esdatafinal$Meanfempre)/sqrt(((esdatafinal$Nfempre-1)*esdatafinal$SDfempre^2 + (esdatafinal$Nfempost-1)*esdatafinal$SDfempost^2)/(esdatafinal$Nfempre+esdatafinal$Nfempost-2)))

#LGcourse on genders
esdatafinal$LGcourse.mal<-(esdatafinal$Meanmalpost-esdatafinal$Meanmalpre)/(100-esdatafinal$Meanmalpre)
esdatafinal$LGcourse.fem<-(esdatafinal$Meanfempost-esdatafinal$Meanfempre)/(100-esdatafinal$Meanfempre)

####Merge the two data files and save the data file
eslvl2imp<-merge(esdatafinal,lvl2_inp,by="Assessment_Sequence_ID")
save(eslvl2imp,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/eslvl2imp")
