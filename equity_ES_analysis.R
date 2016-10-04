load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/eslvl2imp")

library (dplyr) 
library (lattice)
eqdat<-eslvl2imp

#Inequity=1 for cohensd for gender differences
eqdat$dmvf.delta<-eslvl2imp$d.mvf.post-eslvl2imp$d.mvf.pre
eqdat$dmvf.delta<-with(eqdat,{ifelse(dmvf.delta<=0,0,ifelse(dmvf.delta>0,1,"na"))})

#Inequity = 1 for larger cohen's d on male students than on female students
eqdat$dgen.deltav<-eslvl2imp$d.mal-eslvl2imp$d.fem
eqdat$dgen.delta<-with(eqdat,{ifelse(dgen.deltav<=0,0,ifelse(dgen.deltav>0,1,"na"))})

#Inequity = 1 for Course larger learning gains on male students than on female students
eqdat$LGC.deltav<-eslvl2imp$LGcourse.mal-eslvl2imp$LGcourse.fem
eqdat$LGC.delta<-eslvl2imp$LGcourse.mal-eslvl2imp$LGcourse.fem
eqdat$LGC.delta<-with(eqdat,{ifelse(LGC.delta<=0,0,ifelse(LGC.delta>0,1,"na"))})

#Inequity = 1 for Individual larger learning gains on male students than on female students
eqdat$LGI.deltav<-eslvl2imp$LGindclassmal-eslvl2imp$LGindclassfem
eqdat$LGI.delta<-eslvl2imp$LGindclassmal-eslvl2imp$LGindclassfem
eqdat$LGI.delta<-with(eqdat,{ifelse(LGI.delta<=0,0,ifelse(LGI.delta>0,1,"na"))})

#Inequity =1 for pvalues between means
eqdat$t.value<-with(eslvl2imp,(Meanmalpost-Meanfempost)/(SDclasspost/sqrt(Nclasspost)))
eqdat$p.value <- pt(-2*abs(eqdat$t.value),df=eslvl2imp$Nclasspost-2)
eqdat$p.value <- with(eqdat,{ifelse(p.value>0.05,0,ifelse(p.value<=0.05,1,"na"))})

#Inequity =1 for men having higher post test scores than women.
eqdat$mean.delta<-eslvl2imp$Meanmalpost-eslvl2imp$Meanfempost
eqdat$mean.delta<-with(eqdat,{ifelse(mean.delta<=0,0,ifelse(mean.delta>0,1,"na"))})

#Recode the data into an equity column
eqdat$equity<- 0
eqdat$equity[eqdat$mean.delta==1 & eqdat$LGI.delta==1 & eqdat$LGC.delta==1 & eqdat$dmvf.delta ==1 & eqdat$dgen.delta ==1] <- "Inequity"
eqdat$equity[eqdat$mean.delta==0 & eqdat$LGI.delta==0 & eqdat$LGC.delta==0 & eqdat$dmvf.delta ==0 & eqdat$dgen.delta ==0] <- "Equity"
eqdat$equity[eqdat$equity==0]<-"Mixed"
table(eqdat$equity,eqdat$mean.delta)

#Code the data into an agreement column for LGC and LGI,
eqdat$LGagree<- "disagree"
eqdat$LGagree[eqdat$LGI.delta==1 & eqdat$LGC.delta==1 ] <- "Inequity"
eqdat$LGagree[eqdat$LGI.delta==0 & eqdat$LGC.delta==0 ] <- "Equity"

#create a new code for LG equity that dumps the disagree data into the equity group since these are all very small differences
eqdat$LGequity<-eqdat$LGagree
eqdat$LGequity[eqdat$LGagree=="disagree"]<-"Equity"

