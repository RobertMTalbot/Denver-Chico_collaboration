load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/lvl2_imp_ave")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/fil_lvl2")
load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/unfil_lvl2")

with(lvl2_imp_ave,plot(g_c,g_i, main = "n=any for imputed data"))
with(fil_lvl2,plot(g_c,g_i, main = "n=any for filtered data"))
with(unfil_lvl2,plot(g_c,g_i, main = "n=any for unfiltered data"))


library(dplyr)
fil_lvl2 <-filter(fil_lvl2, class_n>9)
unfil_lvl2 <-filter(unfil_lvl2, class_n>9)
lvl2_imp_ave <-filter(lvl2_imp_ave, class_n>9)

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

with(lvl2_imp_ave,cor(pre_score, g_c))
with(fil_lvl2,cor(pre_score, g_c))
