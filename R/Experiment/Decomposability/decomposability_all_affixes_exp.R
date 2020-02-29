library(languageR)
library(Hmisc)
library(lattice)
library(dplyr)



setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Decomposability Analysis")

########I need to get the right df (need to combine the right ones, i.e. for each affix I need the one with everything
# coded and with unvalid tokens excluded - I need to get that from the individual csvs... (the affix_complex ones))

un<- read.csv( "unComplex.csv")
In<- read.csv( "inComplex.csv")
im<- read.csv( "imComplex.csv")
ly<- read.csv( "lyComplex.csv")
dis<- read.csv( "disComplex.csv")


# We need to cobine them (use the right columns)

un%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase)%>% 
  rbind (In%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase))


ExperimentComplex<-un%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase)%>% 
  rbind (In%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase))%>%
  rbind (im%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase))%>%
  rbind (dis%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase))%>%
  rbind (ly%>%select(Item,SemanticTransparency,Affix, Rating,logRelFreq,TypeOfBase))

# we need to exclude those, where Rating is na

ExperimentComplex2<-ExperimentComplex[!is.na(ExperimentComplex$Rating),]

# YEAS, this is exactly the numer which is found in the dissertation, i.e. the one which results
# from the other data sets

# yayyy


##########correlations


pairscor.fnc(ExperimentComplex2[ , c("Rating","SemanticTransparency","logRelFreq","TypeOfBase")])

ExperimentComplex2$numSemTrans<-as.numeric(ExperimentComplex2$SemanticTransparency)
cor.test(ExperimentComplex2$Rating,(ExperimentComplex2$numSemTrans), method = "spearman")

# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$Rating and (ExperimentComplex2$numSemTrans)
# S = 5.8118e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3516367 


cor.test(ExperimentComplex2$Rating,(ExperimentComplex2$logRelFreq), method = "spearman")
# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$Rating and (ExperimentComplex2$logRelFreq)
# S = 3.3186e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.2281916 

cor.test(ExperimentComplex2$Rating,as.numeric(ExperimentComplex2$TypeOfBase), method = "spearman")
# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$Rating and as.numeric(ExperimentComplex2$TypeOfBase)
# S = 2.8003e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.3487389 


# RelFreq and SemTrans
cor.test(ExperimentComplex2$logRelFreq,(ExperimentComplex2$numSemTrans), method = "spearman")
# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$logRelFreq and (ExperimentComplex2$numSemTrans)
# S = 5.5114e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.2817773 


# RelFreq and TypeOfBAse
cor.test(ExperimentComplex2$logRelFreq,as.numeric(ExperimentComplex2$TypeOfBase), method = "spearman")
# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$logRelFreq and as.numeric(ExperimentComplex2$TypeOfBase)
# S = 2.9136e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.3223842 


# Type of base and SemTrans
cor.test(ExperimentComplex2$numSemTrans,as.numeric(ExperimentComplex2$TypeOfBase), method = "spearman")
# Spearman's rank correlation rho
# 
# data:  ExperimentComplex2$numSemTrans and as.numeric(ExperimentComplex2$TypeOfBase)
# S = 7.1518e+10, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.6632869 


ExperimentComplex2$numRating<-as.numeric(ExperimentComplex2$Rating)
ExperimentComplex2$numTypeOfBase<-as.numeric(ExperimentComplex2$TypeOfBase)


############
# Let's see what a cluster does



plot(varclus(as.matrix(select(ExperimentComplex2,numSemTrans,logRelFreq, numTypeOfBase,numRating))))





# let's have a look at the clusters for each affix

#un

# type of base does not show any variation
plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="un",],numSemTrans,logRelFreq, numRating))))


plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="Loc",],numSemTrans,logRelFreq, numTypeOfBase,numRating))))


plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="Neg",],numSemTrans,logRelFreq, numTypeOfBase,numRating))))

ExperimentComplex2$numAffix<-as.numeric(ExperimentComplex2$Affix)

plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="Loc"|ExperimentComplex2$Affix=="Neg",],numSemTrans,logRelFreq, numTypeOfBase,numRating))))

plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="dis",],numSemTrans,logRelFreq, numTypeOfBase,numRating))))

# for ly, type of base and sem trans does not work, so a cluster analysis is not possible


# let's save the plots 

# need to change the names of the variables...

ExperimentComplex2$numSemanticTransparency<- ExperimentComplex2$numSemTrans

ExperimentComplex2$logRelativeFrequency<-ExperimentComplex2$logRelFreq

ExperimentComplex2$SemanticTransparencyRating<- ExperimentComplex2$numRating

ExperimentComplex2$numTypeOfBase<- ExperimentComplex2$numTypeOfBase



png("cluster analysis decomposability experiment all tokens.png", units="cm", height=15, width=15, res=300, pointsize=12)

par(mai=c(0.3,1.2,0.5,0.4))

plot(varclus(as.matrix(select(ExperimentComplex2,numSemanticTransparency,logRelativeFrequency, SemanticTransparencyRating,numTypeOfBase))))

dev.off()





png("cluster analysis decomposability exp dis and in.png", units="cm", height=15, width=25, res=300, pointsize=12)
par(mfrow=c(1,2))

plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="Loc"|ExperimentComplex2$Affix=="Neg",],numSemanticTransparency,logRelativeFrequency, as.numeric(SemanticTransparencyRating),numTypeOfBase))),ylab="Spearman rho²")
title("in-")
plot(varclus(as.matrix(select(ExperimentComplex2[ExperimentComplex2$Affix=="dis",],numSemanticTransparency,logRelativeFrequency, as.numeric(SemanticTransparencyRating),numTypeOfBase))),ylab="",main="dis-")
title("dis-")
dev.off()


#### and let's get the correlation matrix for all affixes

(varclus(as.matrix(select(ExperimentComplex2,numSemanticTransparency,logRelativeFrequency, SemanticTransparencyRating,numTypeOfBase))))


varclus(x = as.matrix(select(ExperimentComplex2, numSemanticTransparency, 
                             logRelativeFrequency, SemanticTransparencyRating, numTypeOfBase)))


# Similarity matrix (Spearman rho^2)
# 
# numSemanticTransparency logRelativeFrequency SemanticTransparencyRating
# numSemanticTransparency                       1.00                 0.08                       0.12
# logRelativeFrequency                          0.08                 1.00                       0.05
# SemanticTransparencyRating                    0.12                 0.05                       1.00
# numTypeOfBase                                 0.44                 0.10                       0.12
# numTypeOfBase
# numSemanticTransparency             0.44
# logRelativeFrequency                0.10
# SemanticTransparencyRating          0.12
# numTypeOfBase                       1.00
# 
# No. of observations used for each pair:
#   
#   numSemanticTransparency logRelativeFrequency SemanticTransparencyRating
# numSemanticTransparency                       6366                 6366                       6366
# logRelativeFrequency                          6366                 6366                       6366
# SemanticTransparencyRating                    6366                 6366                       6366
# numTypeOfBase                                 6366                 6366                       6366
# numTypeOfBase
# numSemanticTransparency             6366
# logRelativeFrequency                6366
# SemanticTransparencyRating          6366
# numTypeOfBase                       6366
# 
# hclust results (method=complete)
# 
# 
# Call:
#   hclust(d = as.dist(1 - x), method = method)
# 
# Cluster method   : complete 
# Number of objects: 4 


#######Rating per affix

table(ExperimentComplex2$Affix,ExperimentComplex2$Rating)
#       1    2    3    4
# un  1868  129   37    5
# Loc  201   81  100  194
# Neg 1225  244  148  100
# dis  590  119   69   51
# ly   747  213  182   63

prop.table(table(ExperimentComplex2$Affix,ExperimentComplex2$Rating),margin=1)
# 1           2           3           4
# un  0.916135360 0.063266307 0.018146150 0.002452182
# Loc 0.348958333 0.140625000 0.173611111 0.336805556
# Neg 0.713453698 0.142108328 0.086196855 0.058241118
# dis 0.711700844 0.143546441 0.083232811 0.061519903
# ly  0.619917012 0.176763485 0.151037344 0.052282158

library(xtable)

xtable(t(prop.table(table(ExperimentComplex2$Affix,ExperimentComplex2$Rating),margin=1)))

kruskal.test(Rating~Affix,ExperimentComplex2)
# Kruskal-Wallis rank sum test
# 
# data:  Rating by Affix
# Kruskal-Wallis chi-squared = 987.84, df = 4, p-value < 2.2e-16

#significant

# now we need to do the same test for all contrats

library(pgirmess)

kruskalmc(Rating~Affix,ExperimentComplex2)

# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# un-Loc  2018.619572     243.4298       TRUE
# un-Neg   657.293134     168.9767       TRUE
# un-dis   663.804567     212.5012       TRUE
# un-ly    946.730412     187.4546       TRUE
# Loc-Neg 1361.326438     248.4068       TRUE
# Loc-dis 1354.815006     279.8385       TRUE
# Loc-ly  1071.889160     261.3274       TRUE
# Neg-dis    6.511432     218.1848      FALSE
# Neg-ly   289.437277     193.8738       TRUE
# dis-ly   282.925845     232.7890       TRUE