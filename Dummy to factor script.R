#turn dummy variable in a single factor

lvl1_imp$gender <- factor(lvl1_imp$male*1 + lvl1_imp$female*2 + lvl1_imp$transgender*3 + lvl1_imp$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))

lvl2_imp$instrument <- factor(lvl2_imp$PCA*1 + lvl2_imp$IMCA*2 + lvl2_imp$GCA*3 + lvl2_imp$CINS*4 + 
                                lvl2_imp$GCIICS*5 + lvl2_imp$CCI*6 + lvl2_imp$FMCE*7 + lvl2_imp$BEMA*8 + 
                                lvl2_imp$FCI*9 + lvl2_imp$CSEM*10 + lvl2_imp$LSCI*11, 
                              labels = c("PCA", "IMCA", "GCA", "CINS", "GCIICS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))

lvl1_imp$race <- factor(lvl1_imp$white*1 + lvl1_imp$black*2 + lvl1_imp$asian*3 + lvl1_imp$american_indian*4 + lvl1_imp$hawaiian_or_other_pacific_islander*5 + lvl1_imp$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))

#white is Dom
lvl1_imp$race_URM <- factor(lvl1_imp$white*1 + lvl1_imp$black*2 + lvl1_imp$asian*2 + lvl1_imp$american_indian*2 + lvl1_imp$hawaiian_or_other_pacific_islander*2 + lvl1_imp$other.1*2, labels = c("NA", "Dom", "NonDom"))

#male is Dom
lvl1_imp$gender_URM <- factor(lvl1_imp$male*1 + lvl1_imp$female*2 + lvl1_imp$transgender*2 + lvl1_imp$other*2, labels = c("NA", "Dom", "NonDom"))
