#Old imputation code

boxplot(lvl1_imp$POST.score ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)
boxplot(lvl1_imp$PRE.score ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)


lvl1_imp_po <- lvl1_imp

postpred <- gls(POST.score ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument))
NApost <- filter(lvl1_imp_po, is.na(POST.score) & !is.na(PRE.score)) #== is question, = is setting the value
matched <- match(NApost$row, lvl1_imp_po$row)
lvl1_imp_po$POST.score[matched] <- predict(postpred, newdata=NApost)



lvl1_imp_popre <- lvl1_imp_po
prepred <- gls(PRE.score ~ POST.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) #predicting the pre
NApre <- filter(lvl1_imp_popre, is.na(PRE.score) & !is.na(POST.score)) #== is question, = is setting the value
matched <- match(NApre$row, lvl1_imp_popre$row)
lvl1_imp_popre$PRE.score[matched] <- predict(prepred, newdata=NApre)

#Multiple Linear Regression by pre-score

dfit_imp_inst <- gls(CohensD ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(dfit_imp_inst)

LGindfit_imp_inst <- gls(LGind ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGindfit_imp_inst)

LGcoursefit_imp_inst <- gls(LGcourse ~ PRE.score + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGcoursefit_imp_inst)

#Multiple Linear Regression by gender and race

dfit_imp_inst <- gls(CohensD ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(dfit_imp_inst)

LGindfit_imp_inst <- gls(LGind ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGindfit_imp_inst)

LGcoursefit_imp_inst <- gls(LGcourse ~ gender_URM + race_URM + instrument, data=na.omit(lvl1_imp), weights=varIdent(form= ~1|instrument)) # controlling for instrument differences
summary(LGcoursefit_imp_inst)


boxplot(lvl1_imp$CohensD ~ lvl1_imp$instrument) #check to see if boxes are taller than other boxes. If so, then create seperate postpreds. If not, include instrument in postpred.)
