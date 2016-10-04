load("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/eslvl2imp")

#multidimension outlier analysis
library(mvoutlier)
library(dplyr)
library(ggplot2)
library(rgl)
library(plot3D)
library(lattice)


lvl2_3d <- eslvl2imp %>%
  select(d.class,LGC.class,LGindclass)

thingiwant <- aq.plot(lvl2_3d, alpha=0.1, quan=0.9)
eslvl2imp$outliers <- thingiwant$outliers
eslvl2imp$outliersnum <- 1*eslvl2imp$outliers

#outliers for just the two LG measures
lvl2_LG <- eslvl2imp %>%
  select(LGC.class,LGindclass)

thingiwant <- aq.plot(lvl2_LG, alpha=0.1, quan=0.9)
eslvl2imp$outliersLG <- thingiwant$outliers
eslvl2imp$outliersLGnum <- 1*eslvl2imp$outliersLG

sink("/Users/kerstin/Desktop/MVOutlierlvl2")
#correlations
"correlations: d-LGC,d-LGi,LGC-LGi"
with(eslvl2imp, cor(d.class,LGC.class))
with(eslvl2imp, cor(d.class,LGindclass))
with(eslvl2imp, cor(LGC.class,LGindclass))
sink()
#Plots

pdf("/Users/kerstin/Desktop/MVOutlierlvl2_plots")
with(eslvl2imp, plot(x=d.class,y=LGC.class, col=outliersnum+1))
with(eslvl2imp, plot(d.class,LGindclass, col=outliersnum+1))
with(eslvl2imp, plot(LGC.class,LGindclass, col=outliersnum+1))

#Histograms

histogram(~Meanclasspre | outliersnum, data=eslvl2imp)
histogram(~Meanclasspost | outliersnum, data=eslvl2imp)
histogram(~SDclasspre | outliersnum, data=eslvl2imp)
histogram(~SDclasspost | outliersnum, data=eslvl2imp)
histogram(~Nclasspre | outliersnum, data=eslvl2imp)
histogram(~Nclasspost | outliersnum, data=eslvl2imp)

dev.off()

# 3d plot broken, also working on splitting data set between outliers to compare the two.
#with(eslvl2imp, plot3d(d.class,LGC.class,LGindclass, col=factor(outliers, labels=c(1,2))))

#Ttests
sink("/Users/kerstin/Desktop/MVOutlierlvl2", append=TRUE)
t.test(Meanclasspre~outliersnum, data=eslvl2imp)
t.test(Meanclasspost~outliersnum, data=eslvl2imp)
t.test(SDclasspre~outliersnum, data=eslvl2imp)
t.test(SDclasspost~outliersnum, data=eslvl2imp)
t.test(Nclasspre~outliersnum, data=eslvl2imp)
t.test(Nclasspost~outliersnum, data=eslvl2imp)
dev.off()


