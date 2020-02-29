#Loading libraries

#library(ascii)
library(xtable)
library(languageR)
library(lme4)
library (MASS)
#library (betareg)
#library(plotrix) 
library(LMERConvenienceFunctions)
library(nlme)
library(rms)
library(visreg)
#library(ggplot2)
#library(stargazer)
#library(texreg)
library(mlmRev)
library(lmerTest)
#library(influence.ME)
library(multcomp)
library(dplyr)

# set the directory, so R knows where to find a file


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses im/")


###########################################################################
# I will start with the complex dataset, and thus will need the complex dataset-
# In the following I will first take a look at the pertinent varoables
# and then fit a model
############################################################################



imComplete <- read.csv("imComplete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(imComplete)

# 'data.frame':	1635 obs. of  84 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Item                       : Factor w/ 64 levels "immaculate","immaterial",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 29 levels "Experiment_1_participant_10",..: 3 14 21 17 5 29 11 9 6 26 ...
# $ ID                         : int  442 2149 3423 2655 918 5437 3740 1524 949 4679 ...
# $ Filename                   : Factor w/ 1635 levels "Participant_10_108.TextGrid",..: 143 723 1139 887 280 1582 635 474 287 1405 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 2 levels "Accented","Unaccented": 2 2 2 1 2 2 1 2 1 2 ...
# $ Annotator                  : Factor w/ 5 levels "Lara","Mandy",..: 2 5 2 5 2 5 1 3 2 2 ...
# $ Order                      : int  117 58 72 3 293 18 299 64 32 8 ...
# $ WordDur                    : num  0.75 0.52 0.655 0.774 0.546 ...
# $ SyllNum                    : int  4 4 4 4 3 4 4 4 4 4 ...
# $ SegNum                     : int  9 8 9 9 8 9 9 9 9 8 ...
# $ ConsonantDur               : num  0.087 0.0958 0.076 0.1299 0.0839 ...
# $ PrecSeg                    : Factor w/ 7 levels "?","@","{","e",..: 6 6 6 6 6 6 6 6 5 6 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0507 0.0752 0.0459 0.0507 0.0642 ...
# $ FollSeg                    : Factor w/ 42 levels "?","@","@e","@U",..: 5 5 5 5 9 5 5 5 9 5 ...
# $ FollSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur                 : num  0.1544 0.0783 0.1242 0.1268 0.0643 ...
# $ PrePauseDur                : num  0 0 0 0 0.367 ...
# $ PostPauseDur               : num  0.155 0 0 0.101 0 ...
# $ SentenceDur                : num  2.03 2.37 3.46 1.85 4.79 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 1 2 2 2 2 2 2 2 2 2 ...
# $ GlottalStopDur             : num  0.0947 0 0 0 0 ...
# $ LocSpeech                  : num  12 15.4 13.7 11.6 14.7 ...
# $ AffixDur                   : num  0.232 0.171 0.122 0.181 0.148 ...
# $ BaseDuration               : num  0.518 0.349 0.533 0.594 0.397 ...
# $ FirstSyllDur               : num  0.232 0.171 0.122 0.181 0.148 ...
# $ WordDurWithoutGlottalStop  : num  0.655 0.52 0.655 0.774 0.546 ...
# $ AffixDurWithoutGlottalStop : num  0.138 0.171 0.122 0.181 0.148 ...
# $ Environment                : Factor w/ 3 levels "#mV","m#C","m#mV": 3 3 3 3 3 3 3 3 3 3 ...
# $ Affix                      : Factor w/ 2 levels "Loc","Neg": 2 2 2 2 2 2 2 2 2 2 ...
# $ WordFormFrequencyBNC       : int  467 467 467 467 467 467 467 467 467 467 ...
# $ WordFormFrequencyAllCOCA   : int  1026 1026 1026 1026 1026 1026 1026 1026 1026 1026 ...
# $ WordFormFrequencySpokenCOCA: int  49 49 49 49 49 49 49 49 49 49 ...
# $ Base                       : Factor w/ 42 levels "maculate","material",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  467 467 467 467 467 467 467 467 467 467 ...
# $ BaseLemmaFrequencyBNC      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ SyllPhon                   : int  4 4 4 4 4 4 4 4 4 4 ...
# $ AffixStress                : Factor w/ 4 levels "primary","Problem",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": 2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  4 3 1 1 2 1 4 3 4 2 ...
# $ TimeRating                 : num  968 1236 1221 828 1222 ...
# $ TotalTime                  : num  763 1056 1006 685 991 ...
# $ Age                        : int  19 30 61 39 19 19 28 21 57 20 ...
# $ Sex                        : Factor w/ 6 levels "female","Female",..: 1 2 2 5 2 1 5 1 1 5 ...
# $ L1                         : Factor w/ 10 levels "british","British",..: 4 8 2 5 8 5 8 1 5 5 ...
# $ Bilingual                  : Factor w/ 6 levels "I only know British English",..: 3 4 3 4 4 3 4 3 4 4 ...
# $ Grow_Up_Region             : Factor w/ 26 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 14 6 24 2 16 10 12 15 21 17 ...
# $ Languages                  : Factor w/ 19 levels "Basic French",..: 14 16 8 16 16 9 16 10 6 16 ...
# $ Latin                      : Factor w/ 14 levels "2 years secondary school",..: 9 6 5 8 6 5 6 10 6 2 ...
# $ Profession_Studies         : Factor w/ 28 levels "2nd year Natural Sciences (Chemistry and materials)",..: 11 13 24 5 12 3 25 19 18 6 ...
# $ University                 : Factor w/ 14 levels "anglia ruskin",..: 5 4 11 12 6 5 2 1 8 2 ...
# $ Knowledge_English_Ling     : Factor w/ 13 levels "Currently in my 2nd year of the course at university",..: 1 5 9 3 10 11 12 4 5 5 ...
# $ Phonetics                  : Factor w/ 12 levels "A couple of lectures",..: 12 4 11 5 12 12 9 6 7 7 ...
# $ Phonology                  : Factor w/ 11 levels "A couple of lectures",..: 11 3 10 5 11 11 7 6 7 7 ...
# $ Morphology                 : Factor w/ 10 levels "currently studying",..: 10 2 9 3 10 10 5 4 5 5 ...
# $ Semantics                  : Factor w/ 10 levels "currently studying",..: 10 2 9 3 10 10 10 4 5 5 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 1 2 2 1 2 2 1 2 1 2 ...
# $ Experiment                 : Factor w/ 1 level "Experiment_1": 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  6.15 6.15 6.15 6.15 6.15 ...
# $ logBaseLemmaFreq           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ logWordLemmaFreq           : num  6.15 6.15 6.15 6.15 6.15 ...
# $ RelFreq                    : num  467 467 467 467 467 467 467 467 467 467 ...
# $ logRelFreq                 : num  6.15 6.15 6.15 6.15 6.15 ...
# $ Root                       : logi  NA NA NA NA NA NA ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : logi  NA NA NA NA NA NA ...
# $ InCorpus                   : logi  NA NA NA NA NA NA ...
# $ Consonant                  : logi  NA NA NA NA NA NA ...
# $ Orthography                : Factor w/ 2 levels "m","mm": 2 2 2 2 2 2 2 2 2 2 ...
# $ median                     : int  3 3 3 3 3 3 3 3 3 3 ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ ConsonantDurMS             : num  87 95.8 76 129.9 83.9 ...
# $ PrePause                   : Factor w/ 2 levels "no pause","pause": 1 1 1 1 2 2 2 2 2 1 ...
# $ PostPause                  : Factor w/ 2 levels "no pause","pause": 2 1 1 2 1 2 2 2 2 1 ...
# $ GlobalSpeechRate           : num  1.97 3.8 2.6 2.16 1.88 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ StressPattern              : Factor w/ 7 levels "NA-primary","NA-unstressed",..: 6 6 6 6 6 6 6 6 6 6 ...

imComplete$X.1<-NULL
imComplete$X<-NULL

###############################################################
#   Summary: variables to include                            ##
###############################################################

## We are going to include the following predictors:

# - Item (rand. effect)
# - Participant (rand. effect)

# - Order
# - Environment
# - logWordFormFreq
# - FirstSyllableBaseStress
# - Accentuation

# - Loc Speech  and/or Global Speech
# - PrePause
# - PostPause


######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = imComplete)
cor(imComplete$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.2056888


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = imComplete)
cor(imComplete$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.1649706

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = imComplete)
cor(imComplete$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.3686474

# so around 37 percent of the variability can be explained by this! 

##########################################################
##              Do an initial model:
imComplete$OrderRescale<-imComplete$Order*0.1

imComplete.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + 
                          (1|Item) + (1|Participant), data = imComplete)


summary(imComplete.lmer1)    

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)
# (Intercept)                        0.17927222    0.00703148  325.30000000  25.496 < 0.0000000000000002
# Environmentm#C                    -0.00562000    0.00381274   59.00000000  -1.474              0.14580
# Environmentm#mV                   -0.00188244    0.00383092   59.10000000  -0.491              0.62498
# AccentuationConditionunaccented    0.00322315    0.00189044 1260.10000000   1.705              0.08845
# OrderRescale                      -0.00003809    0.00006302 1577.50000000  -0.604              0.54567
# logWordFormFreq                   -0.00001535    0.00058372   58.70000000  -0.026              0.97911
# BaseInitialStressunstressed       -0.01087198    0.00323207   58.20000000  -3.364              0.00136
# LocSpeech                         -0.00464551    0.00035352 1361.70000000 -13.141 < 0.0000000000000002
# GlobalSpeechRate                  -0.00966872    0.00220019 1011.30000000  -4.394            0.0000123
# PrePausepause                     -0.00467159    0.00149987 1594.40000000  -3.115              0.00187
# PostPausepause                     0.00042722    0.00166657 1598.20000000   0.256              0.79771
# 
# (Intercept)                     ***
#   Environmentm#C                     
# Environmentm#mV                    
# AccentuationConditionunaccented .  
# OrderRescale                       
# logWordFormFreq                    
# BaseInitialStressunstressed     ** 
#   LocSpeech                       ***
#   GlobalSpeechRate                ***
#   PrePausepause                   ** 
#   PostPausepause                     

cor(imComplete$ConsonantDur, fitted(imComplete.lmer1))^2
#[1]0.4902043

# not bad but not good either



#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own




# 1.  Loc Speech  and/or Global Speech


cor.test(imComplete$LocSpeech,imComplete$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  imComplete$LocSpeech and imComplete$GlobalSpeechRate
# t = 25.872, df = 1633, p-value < 0.00000000000000022
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5038815 0.5726975
# sample estimates:
#       cor 
#0.5391889 


imComplete.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = imComplete)

summary(imComplete.lmerSpeechRates)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
# (Intercept)         0.1621593    0.0041633  395.2000000  38.950 < 0.0000000000000002 ***
#   LocSpeech          -0.0047667    0.0003501 1419.5000000 -13.616 < 0.0000000000000002 ***
#   GlobalSpeechRate   -0.0049749    0.0014992 1544.7000000  -3.318             0.000927 ***
#   ---

cor(imComplete$ConsonantDur, fitted(imComplete.lmerSpeechRates))^2
#[1]  0.4867047



imComplete.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = imComplete)

summary(imComplete.lmerLocSpeech)

# Fixed effects:
#   Estimate   Std. Error           df t value            Pr(>|t|)    
# (Intercept)    0.1585288    0.0040417  358.0000000   39.22 <0.0000000000000002 ***
#   LocSpeech     -0.0054658    0.0002808 1602.6000000  -19.46 <0.0000000000000002 ***
#   ---

cor(imComplete$ConsonantDur, fitted(imComplete.lmerLocSpeech))^2
#[1] 0.4839755


options(scipen=999)
imComplete.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = imComplete)

print(summary(imComplete.lmerGlobalSpeech),digits=3)

# Fixed effects:
#   Estimate Std. Error         df t value            Pr(>|t|)    
# (Intercept)         0.13556    0.00403  225.00000    33.6 <0.0000000000000002 ***
#   GlobalSpeechRate   -0.01718    0.00127 1543.00000   -13.5 <0.0000000000000002 ***
#   ---

cor(imComplete$ConsonantDur, fitted(imComplete.lmerGlobalSpeech))^2
#[1] 0.4328353


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, bothe have a sign effect
# - The effect direction never changes (no supression)
# - I'll keep both for now
#################################################



###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (imComplete.lmer1))
qqline (residuals (imComplete.lmer1))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Maybe a box-cox transformation will lead to a better
# distribuition of res. Let's try


imComplete.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause, data = imComplete)

summary(imComplete.lm)

# Call:
#   lm(formula = ConsonantDur ~ Environment + AccentuationCondition + 
#        OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
#        GlobalSpeechRate + PrePause + PostPause, data = imComplete)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.090523 -0.016119 -0.002729  0.012968  0.122338 
# 
# Coefficients:
#   Estimate  Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.18119227  0.00488810  37.068 < 0.0000000000000002 ***
#   Environmentm#C                  -0.00549526  0.00182146  -3.017              0.00259 ** 
# Environmentm#mV                 -0.00136025  0.00183028  -0.743              0.45747    
# AccentuationConditionunaccented  0.00308135  0.00179212   1.719              0.08573 .  
# OrderRescale                    -0.00006757  0.00007039  -0.960              0.33729    
# logWordFormFreq                  0.00002563  0.00027868   0.092              0.92674    
# BaseInitialStressunstressed     -0.01158410  0.00153429  -7.550   0.0000000000000721 ***
#   LocSpeech                       -0.00440311  0.00030924 -14.238 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.01145335  0.00173481  -6.602   0.0000000000547550 ***
#   PrePausepause                   -0.00401170  0.00157268  -2.551              0.01084 *  
#   PostPausepause                  -0.00119091  0.00177118  -0.672              0.50144    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02638 on 1624 degrees of freedom
# Multiple R-squared:  0.2815,	Adjusted R-squared:  0.277 
# F-statistic: 63.61 on 10 and 1624 DF,  p-value: < 0.00000000000000022

bc<-boxcox(imComplete.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.3434343

imComplete$bc <- imComplete$ConsonantDur^lambda

imComplete.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause +(1|Item) + (1|Participant), data = imComplete)

summary(imComplete.lmerBC)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)
# (Intercept)                        0.57069450    0.01122890  305.70000000  50.824 < 0.0000000000000002
# Environmentm#C                    -0.00427746    0.00626642   58.90000000  -0.683             0.497534
# Environmentm#mV                    0.00105678    0.00629614   59.00000000   0.168             0.867278
# AccentuationConditionunaccented    0.00492108    0.00295438 1292.90000000   1.666             0.096017
# OrderRescale                      -0.00004675    0.00009822 1574.70000000  -0.476             0.634152
# logWordFormFreq                   -0.00011981    0.00095949   58.70000000  -0.125             0.901054
# BaseInitialStressunstressed       -0.01901959    0.00531372   58.30000000  -3.579             0.000702
# LocSpeech                         -0.00762130    0.00055431 1418.40000000 -13.749 < 0.0000000000000002
# GlobalSpeechRate                  -0.01342821    0.00344854 1063.40000000  -3.894             0.000105
# PrePausepause                     -0.00827862    0.00233878 1591.70000000  -3.540             0.000412
# PostPausepause                     0.00005544    0.00259882 1595.20000000   0.021             0.982983
# 
# (Intercept)                     ***
#   Environmentm#C                     
# Environmentm#mV                    
# AccentuationConditionunaccented .  
# OrderRescale                       
# logWordFormFreq                    
# BaseInitialStressunstressed     ***
#   LocSpeech                       ***
#   GlobalSpeechRate                ***
#   PrePausepause                   ***
#   PostPausepause                 

#let's check the assumptions

qqnorm (residuals (imComplete.lmerBC))
qqline (residuals (imComplete.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(imComplete.lmerBC, imComplete, trim = 2.5)
# n.removed = 37 
# percent.removed = 2.262997

imComplete2<-outliers$data

dim(imComplete2)
#[1] 1598   85

dim(imComplete)
#[1]1635   84


# okay it seemes to have worked

imComplete.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value
# (Intercept)                        0.56559372    0.01034043  290.00000000  54.697
# Environmentm#C                    -0.00801880    0.00592001   60.00000000  -1.355
# Environmentm#mV                   -0.00206044    0.00594355   59.90000000  -0.347
# AccentuationConditionunaccented    0.00371762    0.00264577 1341.80000000   1.405
# OrderRescale                      -0.00007724    0.00008722 1534.40000000  -0.886
# logWordFormFreq                    0.00008725    0.00090476   59.40000000   0.096
# BaseInitialStressunstressed       -0.01972322    0.00501172   59.00000000  -3.935
# LocSpeech                         -0.00678399    0.00050930 1418.00000000 -13.320
# GlobalSpeechRate                  -0.01441951    0.00312054 1174.80000000  -4.621
# PrePausepause                     -0.00568884    0.00208142 1551.00000000  -2.733
# PostPausepause                     0.00032372    0.00230441 1552.60000000   0.140
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                              0.180645    
# Environmentm#mV                             0.730054    
# AccentuationConditionunaccented             0.160218    
# OrderRescale                                0.375983    
# logWordFormFreq                             0.923502    
# BaseInitialStressunstressed                 0.000222 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   GlobalSpeechRate                          0.00000424 ***
#   PrePausepause                               0.006344 ** 
#   PostPausepause                              0.888301    

qqnorm (residuals (imComplete.lmerBC2))
qqline (residuals (imComplete.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> imComplete.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(imComplete.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value
# (Intercept)                        0.56559372    0.01034043  290.00000000  54.697
# Environmentm#C                    -0.00801880    0.00592001   60.00000000  -1.355
# Environmentm#mV                   -0.00206044    0.00594355   59.90000000  -0.347
# AccentuationConditionunaccented    0.00371762    0.00264577 1341.80000000   1.405
# OrderRescale                      -0.00007724    0.00008722 1534.40000000  -0.886
# logWordFormFreq                    0.00008725    0.00090476   59.40000000   0.096
# BaseInitialStressunstressed       -0.01972322    0.00501172   59.00000000  -3.935
# LocSpeech                         -0.00678399    0.00050930 1418.00000000 -13.320
# GlobalSpeechRate                  -0.01441951    0.00312054 1174.80000000  -4.621
# PrePausepause                     -0.00568884    0.00208142 1551.00000000  -2.733
# PostPausepause                     0.00032372    0.00230441 1552.60000000   0.140
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                              0.180645    
# Environmentm#mV                             0.730054    
# AccentuationConditionunaccented             0.160218    
# OrderRescale                                0.375983    
# logWordFormFreq                             0.923502    
# BaseInitialStressunstressed                 0.000222 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   GlobalSpeechRate                          0.00000424 ***
#   PrePausepause                               0.006344 ** 
#   PostPausepause                              0.888301  

# let's throw out PostPause

imComplete.lmerBC3 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC3)

# Fixed effects:
#   Estimate    Std. Error            df t value
# (Intercept)                        0.56608011    0.00977228  242.90000000  57.927
# Environmentm#C                    -0.00801031    0.00592272   60.10000000  -1.352
# Environmentm#mV                   -0.00204830    0.00594598   60.00000000  -0.344
# AccentuationConditionunaccented    0.00362837    0.00256208 1264.60000000   1.416
# OrderRescale                      -0.00007699    0.00008718 1535.30000000  -0.883
# logWordFormFreq                    0.00009011    0.00090499   59.40000000   0.100
# BaseInitialStressunstressed       -0.01972574    0.00501417   59.10000000  -3.934
# LocSpeech                         -0.00679084    0.00050745 1425.40000000 -13.382
# GlobalSpeechRate                  -0.01452830    0.00302449 1187.60000000  -4.804
# PrePausepause                     -0.00565961    0.00206925 1553.20000000  -2.735
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                              0.181288    
# Environmentm#mV                             0.731685    
# AccentuationConditionunaccented             0.156968    
# OrderRescale                                0.377295    
# logWordFormFreq                             0.921016    
# BaseInitialStressunstressed                 0.000222 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   GlobalSpeechRate                          0.00000176 ***
#   PrePausepause                               0.006307 ** 

anova(imComplete.lmerBC2,imComplete.lmerBC3)

# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    13 -6353.2 -6283.3 3189.6  -6379.2                        
# object 14 -6351.2 -6276.0 3189.6  -6379.2 0.023      1     0.8794

# model did not become worse


# let's throw out WordFormFreq

imComplete.lmerBC4 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause +  
                             (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC4)

# Fixed effects:
#   Estimate    Std. Error            df t value
# (Intercept)                        0.56647138    0.00861909  430.70000000  65.723
# Environmentm#C                    -0.00825560    0.00538339   61.70000000  -1.534
# Environmentm#mV                   -0.00217947    0.00576483   61.10000000  -0.378
# AccentuationConditionunaccented    0.00360303    0.00255877 1272.20000000   1.408
# OrderRescale                      -0.00007706    0.00008717 1535.80000000  -0.884
# BaseInitialStressunstressed       -0.01961268    0.00483554   60.10000000  -4.056
# LocSpeech                         -0.00678502    0.00050701 1424.30000000 -13.382
# GlobalSpeechRate                  -0.01450778    0.00301773 1190.90000000  -4.808
# PrePausepause                     -0.00565312    0.00206886 1554.20000000  -2.732
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                              0.130260    
# Environmentm#mV                             0.706695    
# AccentuationConditionunaccented             0.159343    
# OrderRescale                                0.376815    
# BaseInitialStressunstressed                 0.000146 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   GlobalSpeechRate                          0.00000172 ***
#   PrePausepause                               0.006357 ** 

anova(imComplete.lmerBC3,imComplete.lmerBC4)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -6355.2 -6290.7 3189.6  -6379.2                         
# object 13 -6353.2 -6283.3 3189.6  -6379.2 0.0107      1     0.9175

# nothing has changed



# let's throw out Order

imComplete.lmerBC5 <- lmer(bc ~ Environment+ AccentuationCondition+GlobalSpeechRate +
                             BaseInitialStress + LocSpeech + 
                             PrePause + 
                             (1|Item) + (1|Participant), data = imComplete2)


summary(imComplete.lmerBC5)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                        0.5646154    0.0083543  395.3000000  67.584
# Environmentm#C                    -0.0082336    0.0053915   61.7000000  -1.527
# Environmentm#mV                   -0.0021656    0.0057737   61.1000000  -0.375
# AccentuationConditionunaccented    0.0035112    0.0025559 1275.2000000   1.374
# GlobalSpeechRate                  -0.0143205    0.0030096 1194.6000000  -4.758
# BaseInitialStressunstressed       -0.0196342    0.0048430   60.1000000  -4.054
# LocSpeech                         -0.0067714    0.0005067 1425.1000000 -13.363
# PrePausepause                     -0.0056118    0.0020680 1555.0000000  -2.714
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                              0.131841    
# Environmentm#mV                             0.708902    
# AccentuationConditionunaccented             0.169767    
# GlobalSpeechRate                          0.00000219 ***
#   BaseInitialStressunstressed                 0.000147 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   PrePausepause                               0.006730 ** 

anova(imComplete.lmerBC4,imComplete.lmerBC5)

# 
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -6356.4 -6297.3 3189.2  -6378.4                         
# object 12 -6355.2 -6290.7 3189.6  -6379.2 0.7921      1     0.3735

# nothing has changed



# let's throw out Accentuation

imComplete.lmerBC6 <- lmer(bc ~ Environment+ +GlobalSpeechRate +
                             BaseInitialStress + LocSpeech + 
                             PrePause + 
                             (1|Item) + (1|Participant), data = imComplete2)


summary(imComplete.lmerBC6)


# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                    0.5587585    0.0072305  306.1000000  77.277
# Environmentm#C                -0.0084253    0.0053536   61.8000000  -1.574
# Environmentm#mV               -0.0025973    0.0057263   60.9000000  -0.454
# GlobalSpeechRate              -0.0116527    0.0022991 1563.8000000  -5.068
# BaseInitialStressunstressed   -0.0195750    0.0048096   60.3000000  -4.070
# LocSpeech                     -0.0066506    0.0005001 1404.0000000 -13.300
# PrePausepause                 -0.0055111    0.0020680 1555.7000000  -2.665
# Pr(>|t|)    
# (Intercept)                 < 0.0000000000000002 ***
#   Environmentm#C                          0.120648    
# Environmentm#mV                         0.651750    
# GlobalSpeechRate                     0.000000449 ***
#   BaseInitialStressunstressed             0.000139 ***
#   LocSpeech                   < 0.0000000000000002 ***
#   PrePausepause                           0.007779 ** 



anova(imComplete.lmerBC5, imComplete.lmerBC6)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    10 -6356.6 -6302.8 3188.3  -6376.6                         
# object 11 -6356.4 -6297.3 3189.2  -6378.4 1.8493      1     0.1739

# not worse
# so that would be the final model without interactions

visreg(imComplete.lmerBC6)

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1. Environment and accentuation and stress and pause



# Let's see



# 1. Environment and accentuation and stress and pause

# Environment and stress


imComplete.lmerBC7EnvStress <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition+ GlobalSpeechRate+
                            LocSpeech + 
                            PrePause +  
                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStress)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                    0.5677308    0.0080819  423.8000000  70.247
# Environmentm#C                                -0.0221253    0.0048664   56.5000000  -4.547
# Environmentm#mV                               -0.0034380    0.0051202   55.0000000  -0.671
# BaseInitialStressunstressed                   -0.0516818    0.0086148   54.9000000  -5.999
# AccentuationConditionunaccented                0.0030659    0.0025507 1301.5000000   1.202
# GlobalSpeechRate                              -0.0149529    0.0029751 1187.7000000  -5.026
# LocSpeech                                     -0.0063843    0.0004935 1146.5000000 -12.936
# PrePausepause                                 -0.0056586    0.0020662 1559.6000000  -2.739
# Environmentm#C:BaseInitialStressunstressed     0.0540322    0.0101462   55.1000000   5.325
# Environmentm#mV:BaseInitialStressunstressed    0.0148585    0.0111358   54.9000000   1.334
# Pr(>|t|)    
# (Intercept)                                 < 0.0000000000000002 ***
#   Environmentm#C                                       0.000029297 ***
# Environmentm#mV                                          0.50474    
# BaseInitialStressunstressed                          0.000000162 ***
#   AccentuationConditionunaccented                          0.22958    
# GlobalSpeechRate                                     0.000000578 ***
#   LocSpeech                                   < 0.0000000000000002 ***
#   PrePausepause                                            0.00624 ** 
#   Environmentm#C:BaseInitialStressunstressed           0.000001916 ***
# Environmentm#mV:BaseInitialStressunstressed              0.18761    

# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC6,imComplete.lmerBC7EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# object 10 -6356.6 -6302.8 3188.3  -6376.6                               
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 32.373      3 0.0000004368 ***


#YES

visreg(imComplete.lmerBC7EnvStress,"Environment", by="BaseInitialStress")

# Environment and Acc
imComplete.lmerBC7EnvAcc <- lmer(bc ~ Environment*AccentuationCondition+ GlobalSpeechRate+
                            LocSpeech + 
                            PrePause + BaseInitialStress+ 
                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvAcc)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                        0.5683196    0.0084617  413.3000000  67.164
# Environmentm#C                                    -0.0132150    0.0057117   77.5000000  -2.314
# Environmentm#mV                                   -0.0057350    0.0061236   77.1000000  -0.937
# AccentuationConditionunaccented                   -0.0029216    0.0036041 1502.5000000  -0.811
# GlobalSpeechRate                                  -0.0144957    0.0030053 1191.9000000  -4.823
# LocSpeech                                         -0.0067696    0.0005061 1425.1000000 -13.376
# PrePausepause                                     -0.0058007    0.0020664 1552.9000000  -2.807
# BaseInitialStressunstressed                       -0.0196250    0.0048443   60.1000000  -4.051
# Environmentm#C:AccentuationConditionunaccented     0.0101118    0.0038233 1505.3000000   2.645
# Environmentm#mV:AccentuationConditionunaccented    0.0072230    0.0041151 1504.2000000   1.755
# Pr(>|t|)    
# (Intercept)                                     < 0.0000000000000002 ***
#   Environmentm#C                                              0.023336 *  
# Environmentm#mV                                             0.351914    
# AccentuationConditionunaccented                             0.417694    
# GlobalSpeechRate                                          0.00000159 ***
#   LocSpeech                                       < 0.0000000000000002 ***
#   PrePausepause                                               0.005061 ** 
#   BaseInitialStressunstressed                                 0.000148 ***
#   Environmentm#C:AccentuationConditionunaccented              0.008259 ** 
# Environmentm#mV:AccentuationConditionunaccented             0.079423 .  
# ---


visreg(imComplete.lmerBC7EnvAcc,"Environment", by="AccentuationCondition")


# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7EnvAcc,imComplete.lmerBC7EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 13 -6359.5 -6289.6 3192.8  -6385.5                                        
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 23.423      0 < 0.00000000000000022 ***
#   ---

# stress one on better! And it makes more sense (diff. here are too small
# to be interpreted)

# Environment and PrePause
imComplete.lmerBC7EnvPause <- lmer(bc ~ Environment*PrePause +AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                   BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvPause)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                        0.5691589    0.0083379  401.3000000  68.262
# Environmentm#C                    -0.0162948    0.0056192   74.1000000  -2.900
# Environmentm#mV                   -0.0097073    0.0060675   75.9000000  -1.600
# PrePausepause                     -0.0215807    0.0035550 1534.2000000  -6.071
# AccentuationConditionunaccented    0.0030143    0.0025360 1278.8000000   1.189
# GlobalSpeechRate                  -0.0146299    0.0029856 1202.7000000  -4.900
# LocSpeech                         -0.0066300    0.0005031 1424.0000000 -13.177
# BaseInitialStressunstressed       -0.0205630    0.0048182   60.1000000  -4.268
# Environmentm#C:PrePausepause       0.0214040    0.0041336 1525.0000000   5.178
# Environmentm#mV:PrePausepause      0.0203949    0.0044336 1522.8000000   4.600
# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#C                               0.00491 ** 
# Environmentm#mV                              0.11378    
# PrePausepause                           0.0000000016 ***
#   AccentuationConditionunaccented              0.23483    
# GlobalSpeechRate                        0.0000010879 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   BaseInitialStressunstressed             0.0000710678 ***
#   Environmentm#C:PrePausepause            0.0000002540 ***
# Environmentm#mV:PrePausepause           0.0000045721 ***



visreg(imComplete.lmerBC7EnvPause,"Environment", by="PrePause")


# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7EnvPause,imComplete.lmerBC7EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 13 -6382.6 -6312.7 3204.3  -6408.6                                        
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 0.3641      0 < 0.00000000000000022 ***

# the stress one is better

# Accentuation and PrePause
imComplete.lmerBC7AccPause <- lmer(bc ~ Environment+PrePause*AccentuationCondition+ GlobalSpeechRate+
                                    LocSpeech + 
                                    BaseInitialStress+ 
                                    (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7AccPause)

# Fixed effects:
#   Estimate   Std. Error           df
# (Intercept)                                      0.5653505    0.0083631  396.6000000
# Environmentm#C                                  -0.0085008    0.0053879   61.7000000
# Environmentm#mV                                 -0.0025333    0.0057704   61.2000000
# PrePausepause                                   -0.0096819    0.0025886 1541.7000000
# AccentuationConditionunaccented                 -0.0014847    0.0031878 1373.9000000
# GlobalSpeechRate                                -0.0137517    0.0030144 1197.3000000
# LocSpeech                                       -0.0066731    0.0005071 1425.4000000
# BaseInitialStressunstressed                     -0.0198829    0.0048398   60.1000000
# PrePausepause:AccentuationConditionunaccented    0.0087782    0.0033656 1529.2000000
# t value             Pr(>|t|)    
# (Intercept)                                    67.601 < 0.0000000000000002 ***
#   Environmentm#C                                 -1.578             0.119731    
# Environmentm#mV                                -0.439             0.662200    
# PrePausepause                                  -3.740             0.000191 ***
#   AccentuationConditionunaccented                -0.466             0.641477    
# GlobalSpeechRate                               -4.562           0.00000559 ***
#   LocSpeech                                     -13.158 < 0.0000000000000002 ***
#   BaseInitialStressunstressed                    -4.108             0.000122 ***
#   PrePausepause:AccentuationConditionunaccented   2.608             0.009190 ** 


visreg(imComplete.lmerBC7AccPause,"AccentuationCondition", by="PrePause")


# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7AccPause,imComplete.lmerBC7EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df  Pr(>Chisq)    
# object 12 -6361.2 -6296.7 3192.6  -6385.2                              
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 23.732      1 0.000001108 ***


# the stress one is better

# Stress and PrePause
imComplete.lmerBC7StressPause <- lmer(bc ~ Environment+PrePause*BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                    LocSpeech + 
                                    (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7StressPause)

# no interaction

# Stress and Acc
imComplete.lmerBC7StressAcc <- lmer(bc ~ Environment+PrePause+BaseInitialStress*AccentuationCondition+ GlobalSpeechRate+
                                       LocSpeech + 
                                       (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7StressAcc)

# Fixed effects:
#   Estimate   Std. Error
# (Intercept)                                                    0.5652225    0.0083361
# Environmentm#C                                                -0.0082788    0.0053931
# Environmentm#mV                                               -0.0022064    0.0057754
# PrePausepause                                                 -0.0057415    0.0020595
# BaseInitialStressunstressed                                   -0.0261590    0.0051445
# AccentuationConditionunaccented                               -0.0005360    0.0027628
# GlobalSpeechRate                                              -0.0140286    0.0029992
# LocSpeech                                                     -0.0067030    0.0005051
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.0132428    0.0035120
# df t value
# (Intercept)                                                  393.9000000  67.805
# Environmentm#C                                                61.7000000  -1.535
# Environmentm#mV                                               61.1000000  -0.382
# PrePausepause                                               1553.7000000  -2.788
# BaseInitialStressunstressed                                   76.4000000  -5.085
# AccentuationConditionunaccented                             1359.0000000  -0.194
# GlobalSpeechRate                                            1198.9000000  -4.677
# LocSpeech                                                   1428.4000000 -13.270
# BaseInitialStressunstressed:AccentuationConditionunaccented 1510.8000000   3.771
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#C                                                          0.129882    
# Environmentm#mV                                                         0.703760    
# PrePausepause                                                           0.005370 ** 
#   BaseInitialStressunstressed                                           0.00000255 ***
#   AccentuationConditionunaccented                                         0.846213    
# GlobalSpeechRate                                                      0.00000324 ***
#   LocSpeech                                                   < 0.0000000000000002 ***
#   BaseInitialStressunstressed:AccentuationConditionunaccented             0.000169 ***

# Let's see whether this is better than the one with the other interaction

visreg(imComplete.lmerBC7StressAcc,"AccentuationCondition", by="BaseInitialStress")


# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7StressAcc,imComplete.lmerBC7EnvStress)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 12 -6368.6 -6304.1 3196.3  -6392.6                            
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 16.33      1  0.0000532 ***

# Stress one is better!

# Pause and Acc
imComplete.lmerBC7PauseAcc <- lmer(bc ~ Environment+BaseInitialStress+PrePause*AccentuationCondition+ GlobalSpeechRate+
                                      LocSpeech + 
                                      (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7PauseAcc)


# Fixed effects:
#   Estimate   Std. Error           df
# (Intercept)                                      0.5653505    0.0083631  396.6000000
# Environmentm#C                                  -0.0085008    0.0053879   61.7000000
# Environmentm#mV                                 -0.0025333    0.0057704   61.2000000
# BaseInitialStressunstressed                     -0.0198829    0.0048398   60.1000000
# PrePausepause                                   -0.0096819    0.0025886 1541.7000000
# AccentuationConditionunaccented                 -0.0014847    0.0031878 1373.9000000
# GlobalSpeechRate                                -0.0137517    0.0030144 1197.3000000
# LocSpeech                                       -0.0066731    0.0005071 1425.4000000
# PrePausepause:AccentuationConditionunaccented    0.0087782    0.0033656 1529.2000000
# t value             Pr(>|t|)    
# (Intercept)                                    67.601 < 0.0000000000000002 ***
#   Environmentm#C                                 -1.578             0.119731    
# Environmentm#mV                                -0.439             0.662200    
# BaseInitialStressunstressed                    -4.108             0.000122 ***
#   PrePausepause                                  -3.740             0.000191 ***
#   AccentuationConditionunaccented                -0.466             0.641477    
# GlobalSpeechRate                               -4.562           0.00000559 ***
#   LocSpeech                                     -13.158 < 0.0000000000000002 ***
#   PrePausepause:AccentuationConditionunaccented   2.608             0.009190 ** 
#   ---

visreg(imComplete.lmerBC7PauseAcc,"AccentuationCondition", by="PrePause")


# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7PauseAcc,imComplete.lmerBC7EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df  Pr(>Chisq)    
# object 12 -6361.2 -6296.7 3192.6  -6385.2                              
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9 23.732      1 0.000001108 ***


###### okay, Env*Stress is best interction but the others are also significanr
# so, let's test some 3-way interactions, as well as some combinations


######
# First three way:

#EnvStressAcc

imComplete.lmerBC7EnvStressAcc <- lmer(bc ~ Environment*BaseInitialStress*AccentuationCondition+ GlobalSpeechRate+ PrePause+
                                     LocSpeech + 
                                     (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressAcc)
#yes

visreg(imComplete.lmerBC7EnvStressAcc,"Environment", by="BaseInitialStress", 
       cond=list(AccentuationCondition="accented"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



visreg(imComplete.lmerBC7EnvStressAcc,"Environment", by="BaseInitialStress", 
       cond=list(AccentuationCondition="unaccented"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))

# is this better thaqb the one whtout interaction

anova(imComplete.lmerBC7EnvStressAcc,imComplete.lmerBC7EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    13 -6382.9 -6313.0 3204.5  -6408.9                             
# object 18 -6397.7 -6300.9 3216.9  -6433.7 24.774      5  0.0001541 ***


# yes


#EnvStressPause

imComplete.lmerBC7EnvStressPause <- lmer(bc ~ Environment*BaseInitialStress*PrePause+AccentuationCondition+ GlobalSpeechRate+
                                         LocSpeech + 
                                         (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressPause)

# yes, but let's see which one is better

anova(imComplete.lmerBC7EnvStressAcc,imComplete.lmerBC7EnvStressPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 18 -6397.7 -6300.9 3216.9  -6433.7                                        
# ..1    18 -6422.2 -6325.4 3229.1  -6458.2 24.481      0 < 0.00000000000000022 ***


# this one!


# let's have a look at the effect

visreg(imComplete.lmerBC7EnvStressPause,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



visreg(imComplete.lmerBC7EnvStressPause,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))


#StressAccPause

imComplete.lmerBC7PauseStressAcc <- lmer(bc ~ Environment+BaseInitialStress*AccentuationCondition*PrePause+ GlobalSpeechRate+
                                         LocSpeech + 
                                         (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7PauseStressAcc)

# no


# Combinations of interactions

#Env*Stress and EnvAcc

imComplete.lmerBC7EnvStressEnvAcc <- lmer(bc ~ Environment*BaseInitialStress+Environment*AccentuationCondition+
                                            PrePause+ GlobalSpeechRate+
                                           LocSpeech + 
                                           (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressEnvAcc)

# YES

#let's see whether this model is better or worse than the 3 way intercation

anova(imComplete.lmerBC7EnvStressEnvAcc,imComplete.lmerBC7EnvStressPause)
# WORSE

#Env*Stress and EnvPause

imComplete.lmerBC7EnvStressEnvPause <- lmer(bc ~ Environment*BaseInitialStress+Environment*PrePause+
                                              AccentuationCondition+
                                             GlobalSpeechRate+
                                            LocSpeech + 
                                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressEnvPause)

#let's see whether this model is better or worse than the 3 way intercation

anova(imComplete.lmerBC7EnvStressEnvPause,imComplete.lmerBC7EnvStressPause)

# WORSE

#Env*Stress and StressAcc

imComplete.lmerBC7EnvStressStressAcc <- lmer(bc ~ Environment*BaseInitialStress+BaseInitialStress*AccentuationCondition+
                                            PrePause+ GlobalSpeechRate+
                                            LocSpeech + 
                                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressStressAcc)

#let's see whether this model is better or worse than the 3 way intercation

anova(imComplete.lmerBC7EnvStressStressAcc,imComplete.lmerBC7EnvStressPause)
# WORSE

#Env*Stress and PauseAcc

imComplete.lmerBC7EnvStressPauseAcc <- lmer(bc ~ Environment*BaseInitialStress+AccentuationCondition* PrePause+
                                              GlobalSpeechRate+
                                            LocSpeech + 
                                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvStressPauseAcc)

anova(imComplete.lmerBC7EnvStressPauseAcc,imComplete.lmerBC7EnvStressPause)

# also worse

#Env*Pause and EnvAcc

imComplete.lmerBC7EnvPauseEnvAcc <- lmer(bc ~ Environment*PrePause+ BaseInitialStress+Environment*AccentuationCondition+
                                            GlobalSpeechRate+
                                            LocSpeech + 
                                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvPauseEnvAcc)

anova(imComplete.lmerBC7EnvPauseEnvAcc,imComplete.lmerBC7EnvStressPause)

# also worse

#Env*Pause and StressAcc

imComplete.lmerBC7EnvPauseStressAcc <- lmer(bc ~ Environment*PrePause+ BaseInitialStress*AccentuationCondition+
                                           GlobalSpeechRate+
                                           LocSpeech + 
                                           (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvPauseStressAcc)

anova(imComplete.lmerBC7EnvPauseStressAcc,imComplete.lmerBC7EnvStressPause)

#also worse

#Env*Pause and PauseAcc

imComplete.lmerBC7EnvPausePauseAcc <- lmer(bc ~ Environment*PrePause+ BaseInitialStress+
                                              AccentuationCondition*PrePause+ GlobalSpeechRate+
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvPausePauseAcc)

anova(imComplete.lmerBC7EnvPausePauseAcc,imComplete.lmerBC7EnvStressPause)
# worse...

#Env*Acc and StressAcc

imComplete.lmerBC7EnvAccStressAcc <- lmer(bc ~ Environment*AccentuationCondition+ BaseInitialStress*AccentuationCondition+
                                             PrePause+ GlobalSpeechRate+
                                             LocSpeech + 
                                             (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvAccStressAcc)

anova(imComplete.lmerBC7EnvAccStressAcc,imComplete.lmerBC7EnvStressPause)

# worse

#Env*Acc and AccPause

imComplete.lmerBC7EnvAccPauseAcc <- lmer(bc ~ Environment*AccentuationCondition+ BaseInitialStress+
                                            AccentuationCondition*PrePause+ GlobalSpeechRate+
                                            LocSpeech + 
                                            (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7EnvAccPauseAcc)

anova(imComplete.lmerBC7EnvAccPauseAcc,imComplete.lmerBC7EnvStressPause)

# worse


#Stress*Acc and AccPause

imComplete.lmerBC7StressAccPauseAcc <- lmer(bc ~ Environment+AccentuationCondition*BaseInitialStress+
                                           AccentuationCondition*PrePause+ GlobalSpeechRate+
                                           LocSpeech + 
                                           (1|Item) + (1|Participant), data = imComplete2)

summary(imComplete.lmerBC7StressAccPauseAcc)

anova(imComplete.lmerBC7StressAccPauseAcc,imComplete.lmerBC7EnvStressPause)
# worse


######################################################
# Summary: 
# There are a lot of interaction s significant, also the combinations...but
# the best model is the one witha 3way interaction between Environment, Stress and Pause

visreg(imComplete.lmerBC7EnvStressPause,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



visreg(imComplete.lmerBC7EnvStressPause,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



#############################################################
# The final model:

summary(imComplete.lmerBC7EnvStressPause)


# Fixed effects:
#   Estimate   Std. Error           df
# (Intercept)                                                  0.5719761    0.0080736  431.3000000
# Environmentm#C                                              -0.0276934    0.0052694   75.1000000
# Environmentm#mV                                             -0.0129374    0.0055543   74.1000000
# BaseInitialStressunstressed                                 -0.0466128    0.0090063   63.9000000
# PrePausepause                                               -0.0182865    0.0038285 1540.1000000
# AccentuationConditionunaccented                              0.0027535    0.0025190 1296.1000000
# GlobalSpeechRate                                            -0.0150982    0.0029504 1190.6000000
# LocSpeech                                                   -0.0063590    0.0004912 1167.2000000
# Environmentm#C:BaseInitialStressunstressed                   0.0439434    0.0110211   74.5000000
# Environmentm#mV:BaseInitialStressunstressed                  0.0254284    0.0125144   84.9000000
# Environmentm#C:PrePausepause                                 0.0155870    0.0046925 1534.5000000
# Environmentm#mV:PrePausepause                                0.0227714    0.0048882 1523.2000000
# BaseInitialStressunstressed:PrePausepause                   -0.0218344    0.0089701 1507.2000000
# Environmentm#C:BaseInitialStressunstressed:PrePausepause     0.0284774    0.0104225 1513.7000000
# Environmentm#mV:BaseInitialStressunstressed:PrePausepause   -0.0020373    0.0114795 1513.1000000
# t value             Pr(>|t|)    
# (Intercept)                                                70.845 < 0.0000000000000002 ***
#   Environmentm#C                                             -5.255          0.000001338 ***
# Environmentm#mV                                            -2.329             0.022574 *  
# BaseInitialStressunstressed                                -5.176          0.000002452 ***
#   PrePausepause                                              -4.776          0.000001954 ***
#   AccentuationConditionunaccented                             1.093             0.274552    
# GlobalSpeechRate                                           -5.117          0.000000361 ***
#   LocSpeech                                                 -12.946 < 0.0000000000000002 ***
#   Environmentm#C:BaseInitialStressunstressed                  3.987             0.000155 ***
# Environmentm#mV:BaseInitialStressunstressed                 2.032             0.045287 *  
# Environmentm#C:PrePausepause                                3.322             0.000916 ***
# Environmentm#mV:PrePausepause                               4.658          0.000003463 ***
# BaseInitialStressunstressed:PrePausepause                  -2.434             0.015043 *  
#   Environmentm#C:BaseInitialStressunstressed:PrePausepause    2.732             0.006362 ** 
# Environmentm#mV:BaseInitialStressunstressed:PrePausepause  -0.177             0.859158    


  lambda
#[1] 0.3434343


# I need to rename some variabels for the plot...


imComplete2<-rename(imComplete2,AccentuationAnnotator=Accentuation)

imComplete2<-rename(imComplete2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(imComplete2$BaseInitialStress)
#[1] "primary"    "unstressed"


levels(imComplete2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(imComplete2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# also need to change ref levels for environment

imComplete2$Environment <- relevel (imComplete2$Environment, ref= "m#mV")


final_im_complete_model.lmer<-lmer(bc ~  Environment*BaseInitialStress*PrePause+LocSpeech+ GlobalSpeechRate+                                              
                                    +(1|Participant)+ (1|Item) , data = imComplete2)                                 


summary(final_im_complete_model.lmer)

# Fixed effects:
#   Estimate   Std. Error           df
# (Intercept)                                                 0.5541018    0.0073404  415.1000000
# Environment#mV                                              0.0132557    0.0055027   74.6000000
# Environmentm#C                                             -0.0146107    0.0053827   85.5000000
# BaseInitialStressunstressed                                -0.0214622    0.0086331  122.7000000
# PrePausepause                                               0.0045598    0.0035107 1533.2000000
# LocSpeech                                                  -0.0062559    0.0004841 1129.3000000
# GlobalSpeechRate                                           -0.0130292    0.0022487 1484.6000000
# Environment#mV:BaseInitialStressunstressed                 -0.0249812    0.0124160   85.9000000
# Environmentm#C:BaseInitialStressunstressed                  0.0187714    0.0106929  116.7000000
# Environment#mV:PrePausepause                               -0.0228739    0.0048878 1524.3000000
# Environmentm#C:PrePausepause                               -0.0071919    0.0044848 1530.1000000
# BaseInitialStressunstressed:PrePausepause                  -0.0236049    0.0071498 1522.0000000
# Environment#mV:BaseInitialStressunstressed:PrePausepause    0.0017424    0.0114783 1513.5000000
# Environmentm#C:BaseInitialStressunstressed:PrePausepause    0.0303767    0.0088716 1525.9000000
# t value             Pr(>|t|)    
# (Intercept)                                               75.487 < 0.0000000000000002 ***
#   Environment#mV                                             2.409             0.018468 *  
# Environmentm#C                                            -2.714             0.008031 ** 
# BaseInitialStressunstressed                               -2.486             0.014263 *  
#   PrePausepause                                              1.299             0.194198    
# LocSpeech                                                -12.922 < 0.0000000000000002 ***
#   GlobalSpeechRate                                          -5.794        0.00000000837 ***
#   Environment#mV:BaseInitialStressunstressed                -2.012             0.047349 *  
# Environmentm#C:BaseInitialStressunstressed                 1.755             0.081797 .  
# Environment#mV:PrePausepause                              -4.680        0.00000312549 ***
# Environmentm#C:PrePausepause                              -1.604             0.109007    
# BaseInitialStressunstressed:PrePausepause                 -3.301             0.000984 ***
#   Environment#mV:BaseInitialStressunstressed:PrePausepause   0.152             0.879368    
# Environmentm#C:BaseInitialStressunstressed:PrePausepause   3.424             0.000633 ***
# ---

# Some plots to understand the interaction

visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120), overlay=TRUE, main="Pause" ,line.par = list(col = c('cornflowerblue','darkblue')))

visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120), overlay=TRUE, main="No pause", line.par = list(col = c('cornflowerblue','darkblue')))





visreg(final_im_complete_model.lmer,"BaseInitialStress", by="Environment", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))

visreg(final_im_complete_model.lmer,"BaseInitialStress", by="Environment", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,
       ylim=c(50,120))



#############
# Let's get the two models for the dissertation


table_final_models<-as.data.frame(coef(summary(final_im_complete_model.lmer)))

xtable(table_final_models,digits = 3)



#change directory for plots

# we need to change the ref

imComplete$Environment <- relevel (imComplete$Environment, ref= "m#C")
imComplete$Environment <- relevel (imComplete$Environment, ref= "m#mV")


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")

png("boxIm.png", units="cm", height=10, width=7, res=300, pointsize=8)
cols = list(col=c("dodgerblue3"),pch=c(1))
bwplot (ConsonantDurMS ~ Environment, imComplete, ylab="duration in milliseconds", 
        main="im-", ylim=c(0,320), cex.axis=0.5,
        par.settings = list(
          plot.symbol=cols,
          box.rectangle = cols,
          #box.dot = cols,
          box.umbrella=cols 
        ))

dev.off()

# what is lambda


lambda
#[1] 0.1010101




##############################
# We should  plot the main effect (not covariates)
###############################
# Plot main effect

png("imModelCompleteInterEnvStressPause.png", units="cm", height=12, width=20, res=300)

ylim=c(20,180)
par(mfrow=c(1,2))

par(oma = c(4, 1, 1, 1))

visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,band=F,
       ylim=ylim, overlay=TRUE,xlab="environment", ylab="duration in milliseconds",line.par = list(col = c('cornflowerblue','darkblue')),
       legend=FALSE, cex=0.8)
title(main = list("pause", cex = 1))

visreg(final_im_complete_model.lmer,"Environment", by="BaseInitialStress", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,band=F,
       ylim=ylim, overlay=TRUE,xlab="environment",ylab="duration in milliseconds",
       line.par = list(col = c('cornflowerblue','darkblue')),legend=FALSE, cex=0.8)
title(main = list("no pause", cex = 1))

# legend (2,8,-1,c("stressed","unstressed"), 
#        fill = c('cornflowerblue','darkblue'))


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 0, 0, 0), 
    new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("stressed","unstressed"), xpd = TRUE, horiz = TRUE, inset = c(0, 
                                                                                 0),
       bty = "n", lwd=3,col =c('cornflowerblue','darkblue'), cex =1)

dev.off()







library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 


imComplete.lm1<- lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause, 
   data = imComplete)

model_ranking <- dredge(imComplete.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Relative variable importance: 
#   LocSpeech BaseInitialStress GlobalSpeechRate Environment PrePause
# Importance:          1.00      1.00              1.00             0.99        0.90    
# N containing models:  256       256               256              256         256    
# PostPause Accentuation OrderRescale logWordFormFreq
# Importance:          0.55      0.38         0.36         0.27           
# N containing models:  256       256          256          256 


# now with interactions

options(na.action = "na.fail") 


imComplete.lm2<- lm(ConsonantDur ~ Environment*BaseInitialStress*Accentuation + 
                      OrderRescale +Environment*PrePause*BaseInitialStress+ logWordFormFreq +  + LocSpeech + 
                      GlobalSpeechRate + PrePause*BaseInitialStress*Accentuation + PostPause, 
                    data = imComplete)

model_ranking2 <- dredge(imComplete.lm2)

model_average2_<-model.avg(model_ranking2)


summary(model_average2_)

# Relative variable importance: 
#                     BaseInitialStress Environment LocSpeech BaseInitialStress:Environment PrePause GlobalSpeechRate
# Importance:          1.00              1.00        1.00      1.00                          1.00     1.00            
# N containing models: 4128              4096        2352      2240                          4096     2352            

              # Environment:PrePause Accentuation Accentuation:Environment BaseInitialStress:PrePause
# Importance:          1.00                 1.00         1.00                     0.99                      
# N containing models: 2048                 4096         2048                     2240                      

                      #BaseInitialStress:Environment:PrePause logWordFormFreq Accentuation:BaseInitialStress PostPause
# Importance:          0.96                                   0.80            0.70                           0.63     
# N containing models:  448                                   2352            2240                           2352     

                    # OrderRescale Accentuation:PrePause Accentuation:BaseInitialStress:Environment
# Importance:          0.40         0.32                  0.30                                      
# N containing models: 2352         2048                   448                                      

#                       Accentuation:BaseInitialStress:PrePause
# Importance:          0.08                                   
# N containing models:  448      
