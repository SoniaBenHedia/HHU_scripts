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


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses in/")



InComplete <- read.csv("inComplete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(InComplete)

# 'data.frame':	1232 obs. of  83 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 ...
# $ Item                       : Factor w/ 53 levels "inact","inappeasable",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 29 levels "Experiment_1_participant_10",..: 20 24 22 4 27 10 25 12 21 18 ...
# $ ID                         : int  3201 4144 4340 651 5011 1773 4506 2030 3556 2989 ...
# $ Filename                   : Factor w/ 1232 levels "Participant_10_10.TextGrid",..: 795 968 896 156 1136 405 1010 522 886 741 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 2 levels "Accented","Unaccented": 2 1 2 2 1 1 1 1 1 1 ...
# $ Annotator                  : Factor w/ 5 levels "Lara","Mandy",..: 1 2 5 5 4 1 4 3 3 2 ...
# $ Order                      : int  10 40 60 157 258 165 34 284 303 286 ...
# $ WordDur                    : num  0.418 0.763 0.329 0.418 0.617 ...
# $ SyllNum                    : int  2 2 2 2 2 2 2 2 2 2 ...
# $ SegNum                     : int  6 5 5 5 5 5 5 5 5 5 ...
# $ ConsonantDur               : num  0.0517 0.0656 0.0568 0.0439 0.0615 ...
# $ PrecSeg                    : Factor w/ 6 levels "?","@","{","i",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0561 0.0663 0.0449 0.0378 0.1358 ...
# $ FollSeg                    : Factor w/ 38 levels "@","@U","{","{kt",..: 3 3 3 3 5 3 3 3 3 3 ...
# $ FollSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur                 : num  0.108 0.18 0.107 0.13 0.201 ...
# $ PrePauseDur                : num  0 0.0867 0 0.6514 0 ...
# $ PostPauseDur               : num  0 0.25 0 0 0.169 ...
# $ SentenceDur                : num  2.91 2.08 2.21 4.33 1.76 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 1 2 2 2 2 2 2 2 2 1 ...
# $ GlottalStopDur             : num  0.0558 0 0 0 0 ...
# $ LocSpeech                  : num  14.34 6.55 15.19 11.97 8.11 ...
# $ AffixDur                   : num  0.1636 0.132 0.1017 0.0817 0.1973 ...
# $ BaseDuration               : num  0.255 0.631 0.227 0.336 0.419 ...
# $ FirstSyllDur               : num  0.1636 0.132 0.1017 0.0817 0.1973 ...
# $ WordDurWithoutGlottalStop  : num  0.363 0.763 0.329 0.418 0.617 ...
# $ AffixDurWithoutGlottalStop : num  0.1078 0.132 0.1017 0.0817 0.1973 ...
# $ Environment                : Factor w/ 4 levels "#nV","n#C","n#nV",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ Affix                      : Factor w/ 2 levels "Loc","Neg": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  2 2 2 2 2 2 2 2 2 2 ...
# $ WordFormFrequencyAllCOCA   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ WordFormFrequencySpokenCOCA: int  0 0 0 0 0 0 0 0 0 0 ...
# $ Base                       : Factor w/ 45 levels "act","appeasable",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  2 2 2 2 2 2 2 2 2 2 ...
# $ BaseLemmaFrequencyBNC      : int  38099 38099 38099 38099 38099 38099 38099 38099 38099 38099 ...
# $ SyllPhon                   : int  2 2 2 2 2 2 2 2 2 2 ...
# $ AffixStress                : Factor w/ 3 levels "primary","secondary",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": 2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  2 2 4 4 1 1 4 4 1 2 ...
# $ TimeRating                 : num  758 610 1315 1011 692 ...
# $ TotalTime                  : num  650 528 1129 826 605 ...
# $ Age                        : int  19 18 19 35 32 19 29 24 61 19 ...
# $ Sex                        : Factor w/ 6 levels "female","Female",..: 2 2 4 4 5 4 6 1 2 5 ...
# $ L1                         : Factor w/ 10 levels "british","British",..: 5 5 5 2 5 2 3 6 2 5 ...
# $ Bilingual                  : Factor w/ 6 levels "I only know British English",..: 4 4 3 3 4 6 2 3 3 4 ...
# $ Grow_Up_Region             : Factor w/ 26 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 13 19 18 24 11 3 22 26 24 5 ...
# $ Languages                  : Factor w/ 19 levels "Basic French",..: 4 1 12 15 19 5 3 13 8 16 ...
# $ Latin                      : Factor w/ 14 levels "2 years secondary school",..: 6 11 5 12 6 3 4 5 5 6 ...
# $ Profession_Studies         : Factor w/ 28 levels "2nd year Natural Sciences (Chemistry and materials)",..: 27 22 9 16 2 14 4 17 24 7 ...
# $ University                 : Factor w/ 14 levels "anglia ruskin",..: 14 9 5 7 2 2 3 2 11 5 ...
# $ Knowledge_English_Ling     : Factor w/ 13 levels "Currently in my 2nd year of the course at university",..: 5 5 13 4 5 6 6 5 9 5 ...
# $ Phonetics                  : Factor w/ 12 levels "A couple of lectures",..: 7 7 11 7 7 7 8 7 11 7 ...
# $ Phonology                  : Factor w/ 11 levels "A couple of lectures",..: 7 7 10 7 7 7 8 7 10 6 ...
# $ Morphology                 : Factor w/ 10 levels "currently studying",..: 5 5 9 5 5 5 6 5 9 5 ...
# $ Semantics                  : Factor w/ 10 levels "currently studying",..: 5 5 9 5 5 5 6 5 9 5 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 1 2 2 1 1 1 1 2 1 ...
# $ Experiment                 : Factor w/ 1 level "Experiment_1": 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  0.693 0.693 0.693 0.693 0.693 ...
# $ logBaseLemmaFreq           : num  10.5 10.5 10.5 10.5 10.5 ...
# $ logWordLemmaFreq           : num  0.693 0.693 0.693 0.693 0.693 ...
# $ RelFreq                    : num  5.25e-05 5.25e-05 5.25e-05 5.25e-05 5.25e-05 ...
# $ logRelFreq                 : num  -9.85 -9.85 -9.85 -9.85 -9.85 ...
# $ Root                       : logi  NA NA NA NA NA NA ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : logi  NA NA NA NA NA NA ...
# $ InCorpus                   : logi  NA NA NA NA NA NA ...
# $ Consonant                  : logi  NA NA NA NA NA NA ...
# $ Orthography                : Factor w/ 2 levels "n","nn": 1 1 1 1 1 1 1 1 1 1 ...
# $ median                     : int  2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrePause                   : Factor w/ 2 levels "No Pause","Pause": 1 2 1 2 1 2 1 2 1 1 ...
# $ PostPause                  : Factor w/ 2 levels "No Pause","Pause": 1 2 1 1 2 2 1 2 1 2 ...
# $ GlobalSpeechRate           : num  3.1 1.92 4.07 2.08 2.28 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ RatingCategorical          : Factor w/ 2 levels "difficultly_decomposed",..: 2 2 1 1 2 2 1 1 2 2 ...

InComplete$X.1<-NULL
InComplete$X<-NULL

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

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = InComplete)
cor(InComplete$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.07794004


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = InComplete)
cor(InComplete$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.5749699

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = InComplete)
cor(InComplete$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.6555855

# so around 66 percent of the variability can be explained by this! 

# before we do an initial model, let's set the reference level right

InComplete$Environment <- relevel (InComplete$Environment, ref= "n#nV")


##########################################################
##              Do an initial model:
InComplete$OrderRescale<-InComplete$Order*0.1

InComplete.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + 
                          (1|Item) + (1|Participant), data = InComplete)


summary(InComplete.lmer1)    

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.278e-01  7.043e-03  1.776e+02  18.140  < 2e-16 ***
#   Environment#nV                   2.276e-02  6.784e-03  4.650e+01   3.355  0.00159 ** 
#   Environmentn#C                   1.329e-02  4.898e-03  4.770e+01   2.714  0.00921 ** 
#   Environmentn#V                  -2.567e-02  5.055e-03  4.920e+01  -5.078 5.85e-06 ***
#   AccentuationConditionunaccented  8.423e-04  1.642e-03  9.314e+02   0.513  0.60804    
#   OrderRescale                    -3.540e-05  5.877e-05  1.184e+03  -0.602  0.54710    
#   logWordFormFreq                  5.785e-04  5.117e-04  4.870e+01   1.131  0.26380    
#   BaseInitialStressunstressed     -3.303e-03  2.831e-03  4.830e+01  -1.167  0.24899    
#   LocSpeech                       -3.427e-03  3.279e-04  1.079e+03 -10.450  < 2e-16 ***
#   GlobalSpeechRate                -3.394e-03  1.933e-03  7.380e+02  -1.756  0.07949 .  
#   PrePausePause                    1.247e-03  1.315e-03  1.193e+03   0.949  0.34304    
#   PostPausePause                  -2.127e-03  1.476e-03  1.194e+03  -1.441  0.14984  

cor(InComplete$ConsonantDur, fitted(InComplete.lmer1))^2
#[1]0.6997682

# not bad but not good either



#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slInming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own




# 1.  Loc Speech  and/or Global Speech


cor.test(InComplete$LocSpeech,InComplete$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  InComplete$LocSpeech and InComplete$GlobalSpeechRate
# t = 20.931, df = 1230, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.4700785 0.5525097
# sample estimates:
# cor 
# 0.5124738 


InComplete.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = InComplete)

summary(InComplete.lmerSpeechRates)

# Fixed effects:
# (Intercept)       1.176e-01  4.671e-03  2.309e+02  25.183   <2e-16 ***
#   LocSpeech        -3.583e-03  3.381e-04  1.221e+03 -10.597   <2e-16 ***
#   GlobalSpeechRate -1.377e-03  1.383e-03  1.148e+03  -0.995     0.32    

cor(InComplete$ConsonantDur, fitted(InComplete.lmerSpeechRates))^2
#[1]  0.7007864



InComplete.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = InComplete)

summary(InComplete.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.170e-01  4.615e-03  2.265e+02   25.36   <2e-16 ***
#   LocSpeech   -3.792e-03  2.665e-04  1.176e+03  -14.23   <2e-16 ***

cor(InComplete$ConsonantDur, fitted(InComplete.lmerLocSpeech))^2
#[1] 0.7003254


InComplete.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = InComplete)

print(summary(InComplete.lmerGlobalSpeech),digits=3)

# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)       9.48e-02   4.52e-03  1.44e+02   20.96   <2e-16 ***
#   GlobalSpeechRate -1.04e-02   1.14e-03  1.10e+03   -9.11   <2e-16 ***

cor(InComplete$ConsonantDur, fitted(InComplete.lmerGlobalSpeech))^2
#[1] 0.677208


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, only LocSpeech has a sign effect
# - The effect direction never changes (no supression)
# - I'll keep both for now
#################################################



###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (InComplete.lmer1))
qqline (residuals (InComplete.lmer1))

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


InComplete.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause, data = InComplete)

summary(InComplete.lm)

# Call:
#   lm(formula = ConsonantDur ~ Environment + AccentuationCondition + 
#        OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
#        GlobalSpeechRate + PrePause + PostPause, data = InComplete)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.064394 -0.013344 -0.001498  0.010912  0.119220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      1.347e-01  4.921e-03  27.371  < 2e-16 ***
#   Environment#nV                   2.135e-02  3.279e-03   6.511 1.09e-10 ***
# Environmentn#C                   1.219e-02  2.430e-03   5.015 6.08e-07 ***
# Environmentn#V                  -2.657e-02  2.561e-03 -10.376  < 2e-16 ***
# AccentuationConditionunaccented  4.848e-04  1.593e-03   0.304 0.760871    
# OrderRescale                    -5.419e-06  6.471e-05  -0.084 0.933280    
# logWordFormFreq                  5.798e-04  2.533e-04   2.289 0.022259 *  
#   BaseInitialStressunstressed     -2.998e-03  1.401e-03  -2.140 0.032592 *  
#   LocSpeech                       -3.509e-03  2.929e-04 -11.977  < 2e-16 ***
#   GlobalSpeechRate                -4.778e-03  1.586e-03  -3.013 0.002644 ** 
#   PrePausePause                   -1.563e-04  1.345e-03  -0.116 0.907525    
# PostPausePause                  -5.410e-03  1.523e-03  -3.552 0.000397 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02073 on 1220 degrees of freedom
# Multiple R-squared:  0.5888,	Adjusted R-squared:  0.5851 
# F-statistic: 158.8 on 11 and 1220 DF,  p-value: < 2.2e-16

bc<-boxcox(InComplete.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.02020202

InComplete$bc <- InComplete$ConsonantDur^lambda

InComplete.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause +(1|Item) + (1|Participant), data = InComplete)

summary(InComplete.lmerBC)
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      9.599e-01  1.753e-03  1.863e+02 547.632  < 2e-16 ***
#   Environment#nV                   4.584e-03  1.664e-03  4.650e+01   2.754  0.00838 ** 
# Environmentn#C                   3.681e-03  1.202e-03  4.770e+01   3.063  0.00359 ** 
# Environmentn#V                  -7.166e-03  1.240e-03  4.930e+01  -5.777 5.08e-07 ***
# AccentuationConditionunaccented -5.056e-05  4.101e-04  1.009e+03  -0.123  0.90190    
# OrderRescale                    -1.187e-05  1.454e-05  1.183e+03  -0.816  0.41454    
# logWordFormFreq                  8.955e-05  1.256e-04  4.870e+01   0.713  0.47920    
# BaseInitialStressunstressed     -1.532e-03  6.946e-04  4.830e+01  -2.206  0.03219 *  
#   LocSpeech                       -8.073e-04  8.120e-05  1.075e+03  -9.943  < 2e-16 ***
#   GlobalSpeechRate                -4.155e-04  4.846e-04  8.515e+02  -0.857  0.39151    
# PrePausePause                    3.283e-04  3.260e-04  1.199e+03   1.007  0.31415    
# PostPausePause                  -6.411e-04  3.659e-04  1.199e+03  -1.752  0.07998 .  

#let's check the assumptions

qqnorm (residuals (InComplete.lmerBC))
qqline (residuals (InComplete.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(InComplete.lmerBC, InComplete, trim = 2.5)
#n.removed = 22 
#percent.removed = 1.785714 

InComplete2<-outliers$data

dim(InComplete2)
#[1] 1210   84

dim(InComplete)
#1232   83


# okay it seemes to have worked

InComplete.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = InComplete2)

summary(InComplete.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      9.592e-01  1.654e-03  1.762e+02 580.014  < 2e-16 ***
#   Environment#nV                   5.393e-03  1.597e-03  4.640e+01   3.376  0.00150 ** 
# Environmentn#C                   3.981e-03  1.153e-03  4.760e+01   3.453  0.00118 ** 
# Environmentn#V                  -7.040e-03  1.191e-03  4.920e+01  -5.912 3.16e-07 ***
# AccentuationConditionunaccented  6.246e-06  3.793e-04  1.026e+03   0.016  0.98686    
# OrderRescale                    -1.408e-05  1.324e-05  1.158e+03  -1.064  0.28770    
# logWordFormFreq                  8.777e-05  1.201e-04  4.800e+01   0.731  0.46855    
# BaseInitialStressunstressed     -1.291e-03  6.653e-04  4.780e+01  -1.941  0.05819 .  
# LocSpeech                       -7.724e-04  7.511e-05  1.075e+03 -10.284  < 2e-16 ***
#   GlobalSpeechRate                -4.387e-04  4.497e-04  9.198e+02  -0.975  0.32958    
# PrePausePause                    1.060e-04  2.975e-04  1.174e+03   0.356  0.72158    
# PostPausePause                  -4.975e-04  3.383e-04  1.176e+03  -1.471  0.14161    
# ---

qqnorm (residuals (InComplete.lmerBC2))
qqline (residuals (InComplete.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> InComplete.lmerBC2



#########################################################################################
#                                                                                       #
#                      SInplification of the model                                      #
#########################################################################################

summary(InComplete.lmerBC2)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      9.592e-01  1.654e-03  1.762e+02 580.014  < 2e-16 ***
#   Environment#nV                   5.393e-03  1.597e-03  4.640e+01   3.376  0.00150 ** 
#   Environmentn#C                   3.981e-03  1.153e-03  4.760e+01   3.453  0.00118 ** 
#   Environmentn#V                  -7.040e-03  1.191e-03  4.920e+01  -5.912 3.16e-07 ***
#   AccentuationConditionunaccented  6.246e-06  3.793e-04  1.026e+03   0.016  0.98686    
#   OrderRescale                    -1.408e-05  1.324e-05  1.158e+03  -1.064  0.28770    
#   logWordFormFreq                  8.777e-05  1.201e-04  4.800e+01   0.731  0.46855    
#   BaseInitialStressunstressed     -1.291e-03  6.653e-04  4.780e+01  -1.941  0.05819 .  
#   LocSpeech                       -7.724e-04  7.511e-05  1.075e+03 -10.284  < 2e-16 ***
#   GlobalSpeechRate                -4.387e-04  4.497e-04  9.198e+02  -0.975  0.32958    
#   PrePausePause                    1.060e-04  2.975e-04  1.174e+03   0.356  0.72158    
#   PostPausePause                  -4.975e-04  3.383e-04  1.176e+03  -1.471  0.14161  


# let's throw out Accentuation

InComplete.lmerBC3 <- lmer(bc ~ Environment+  OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = InComplete2)
summary(InComplete.lmerBC3)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  9.592e-01  1.605e-03  1.608e+02 597.558  < 2e-16 ***
#   Environment#nV               5.393e-03  1.597e-03  4.640e+01   3.377  0.00149 ** 
# Environmentn#C               3.981e-03  1.153e-03  4.760e+01   3.453  0.00117 ** 
# Environmentn#V              -7.040e-03  1.191e-03  4.920e+01  -5.913 3.16e-07 ***
# OrderRescale                -1.408e-05  1.323e-05  1.160e+03  -1.064  0.28742    
# logWordFormFreq              8.763e-05  1.198e-04  4.750e+01   0.731  0.46812    
# BaseInitialStressunstressed -1.291e-03  6.648e-04  4.770e+01  -1.942  0.05810 .  
# LocSpeech                   -7.724e-04  7.501e-05  1.073e+03 -10.298  < 2e-16 ***
#   GlobalSpeechRate            -4.346e-04  3.686e-04  1.168e+03  -1.179  0.23866    
# PrePausePause                1.060e-04  2.973e-04  1.175e+03   0.356  0.72161    
# PostPausePause              -4.989e-04  3.305e-04  1.177e+03  -1.510  0.13142    
# ---
  
anova(InComplete.lmerBC2,InComplete.lmerBC3)

# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    14 -9722.7 -9651.4 4875.4  -9750.7                        
# object 15 -9720.7 -9644.3 4875.4  -9750.7 2e-04      1     0.9881

# model did not become worse


# let's throw out PrePause

InComplete.lmerBC4  <- lmer(bc ~ Environment+  OrderRescale +logWordFormFreq+
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              + PostPause + 
                              (1|Item) + (1|Participant), data = InComplete2)

summary(InComplete.lmerBC4)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  9.593e-01  1.567e-03  1.472e+02 612.080  < 2e-16 ***
#   Environment#nV               5.355e-03  1.593e-03  4.600e+01   3.361  0.00157 ** 
# Environmentn#C               3.979e-03  1.153e-03  4.760e+01   3.452  0.00118 ** 
# Environmentn#V              -7.039e-03  1.190e-03  4.920e+01  -5.913 3.15e-07 ***
# OrderRescale                -1.447e-05  1.318e-05  1.161e+03  -1.098  0.27251    
# logWordFormFreq              8.906e-05  1.197e-04  4.740e+01   0.744  0.46064    
# BaseInitialStressunstressed -1.289e-03  6.648e-04  4.770e+01  -1.940  0.05833 .  
# LocSpeech                   -7.709e-04  7.484e-05  1.079e+03 -10.301  < 2e-16 ***
#   GlobalSpeechRate            -4.675e-04  3.567e-04  1.158e+03  -1.311  0.19026    
# PostPausePause              -4.939e-04  3.300e-04  1.178e+03  -1.497  0.13471   

anova(InComplete.lmerBC3,InComplete.lmerBC4)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -9724.6 -9658.3 4875.3  -9750.6                         
# object 14 -9722.7 -9651.4 4875.4  -9750.7 0.1238      1     0.7249

# nothing has changed



# let's throw out Word Form Freq

InComplete.lmerBC5 <- lmer(bc ~ Environment+  OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             + PostPause + 
                             (1|Item) + (1|Participant), data = InComplete2)
summary(InComplete.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                  9.597e-01  1.503e-03  1.784e+02 638.446  < 2e-16 ***
#   Environment#nV               5.347e-03  1.587e-03  4.700e+01   3.370  0.00151 ** 
#   Environmentn#C               3.970e-03  1.148e-03  4.870e+01   3.459  0.00114 ** 
#   Environmentn#V              -7.159e-03  1.175e-03  5.060e+01  -6.093 1.51e-07 ***
#   OrderRescale                -1.421e-05  1.317e-05  1.162e+03  -1.079  0.28078    
#   BaseInitialStressunstressed -1.328e-03  6.600e-04  4.900e+01  -2.012  0.04974 *  
#   LocSpeech                   -7.745e-04  7.464e-05  1.068e+03 -10.376  < 2e-16 ***
#   GlobalSpeechRate            -4.418e-04  3.549e-04  1.150e+03  -1.245  0.21353    
#   PostPausePause              -4.879e-04  3.299e-04  1.180e+03  -1.479  0.13936  

anova(InComplete.lmerBC4,InComplete.lmerBC5)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -9726.0 -9664.8 4875.0  -9750.0                         
# object 13 -9724.6 -9658.3 4875.3  -9750.6 0.6194      1     0.4313

# nothing has changed



# let's throw out Order

InComplete.lmerBC6 <- lmer(bc ~ Environment+  
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             + PostPause + 
                             (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC6)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  9.594e-01  1.483e-03  1.714e+02 646.752  < 2e-16 ***
#   Environment#nV               5.332e-03  1.584e-03  4.710e+01   3.365  0.00153 ** 
# Environmentn#C               3.966e-03  1.146e-03  4.870e+01   3.460  0.00113 ** 
# Environmentn#V              -7.157e-03  1.173e-03  5.060e+01  -6.099 1.48e-07 ***
# BaseInitialStressunstressed -1.323e-03  6.591e-04  4.900e+01  -2.007  0.05027 .  
# LocSpeech                   -7.702e-04  7.453e-05  1.069e+03 -10.333  < 2e-16 ***
#   GlobalSpeechRate            -4.519e-04  3.548e-04  1.152e+03  -1.273  0.20314    
# PostPausePause              -5.014e-04  3.297e-04  1.181e+03  -1.521  0.12856    

anova(InComplete.lmerBC5, InComplete.lmerBC6)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -9726.8 -9670.7 4874.4  -9748.8                         
# object 12 -9726.0 -9664.8 4875.0  -9750.0 1.1578      1     0.2819

# not worse

# let's throw out Global

InComplete.lmerBC7 <- lmer(bc ~ Environment+  
                             BaseInitialStress + LocSpeech + 
                             + PostPause + 
                             (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC7)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  9.587e-01  1.383e-03  1.273e+02 693.384  < 2e-16 ***
#   Environment#nV               5.341e-03  1.584e-03  4.700e+01   3.371  0.00151 ** 
# Environmentn#C               4.000e-03  1.146e-03  4.860e+01   3.490  0.00104 ** 
# Environmentn#V              -7.057e-03  1.171e-03  5.020e+01  -6.028 1.96e-07 ***
# BaseInitialStressunstressed -1.321e-03  6.590e-04  4.890e+01  -2.004  0.05063 .  
# LocSpeech                   -8.118e-04  6.693e-05  1.169e+03 -12.129  < 2e-16 ***
#   PostPausePause              -2.890e-04  2.848e-04  1.195e+03  -1.015  0.31044    


anova(InComplete.lmerBC6, InComplete.lmerBC7)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    10 -9727.2 -9676.2 4873.6  -9747.2                        
# object 11 -9726.8 -9670.7 4874.4  -9748.8 1.635      1      0.201

# not worse

# let's throw out PostPause


InComplete.lmerBC8 <- lmer(bc ~ Environment+  
                             BaseInitialStress + LocSpeech + 
                             (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  9.582e-01  1.299e-03  1.036e+02 737.572  < 2e-16 ***
#   Environment#nV               5.343e-03  1.586e-03  4.700e+01   3.368  0.00152 ** 
# Environmentn#C               3.978e-03  1.147e-03  4.860e+01   3.468  0.00111 ** 
# Environmentn#V              -7.122e-03  1.170e-03  4.990e+01  -6.085 1.63e-07 ***
# BaseInitialStressunstressed -1.294e-03  6.593e-04  4.880e+01  -1.963  0.05539 .  
# LocSpeech                   -7.800e-04  5.938e-05  1.195e+03 -13.136  < 2e-16 ***


anova(InComplete.lmerBC7, InComplete.lmerBC8)
# ..1     9 -9728.1 -9682.3 4873.1  -9746.1                         
# object 10 -9727.2 -9676.2 4873.6  -9747.2 1.0519      1     0.3051


# let's throw out stress

InComplete.lmerBC9 <- lmer(bc ~ Environment+  
                              LocSpeech + 
                             (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC9)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     9.578e-01  1.307e-03  1.019e+02 733.019  < 2e-16 ***
#   Environment#nV  5.659e-03  1.621e-03  4.770e+01   3.491  0.00105 ** 
# Environmentn#C  3.889e-03  1.177e-03  4.910e+01   3.305  0.00178 ** 
# Environmentn#V -7.776e-03  1.150e-03  4.980e+01  -6.760 1.45e-08 ***
# LocSpeech      -7.734e-04  5.935e-05  1.196e+03 -13.032  < 2e-16 ***
#   ---

anova(InComplete.lmerBC8, InComplete.lmerBC9)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1     8 -9726.1 -9685.3 4871.0  -9742.1                           
# object  9 -9728.1 -9682.3 4873.1  -9746.1 4.0481      1    0.04422 *
#   ---


# the one with stress is sign. better

# so that would be the final model without interactions

visreg(InComplete.lmerBC8)

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view


# 1. Environment and accentuation and stress and pause



# Let's see



# 1. Environment and accentuation and stress and pause

# Environment and stress


InComplete.lmerBC8IntEnvStr <- lmer(bc ~ Environment*BaseInitialStress + LocSpeech + 
                             (1|Item) + (1|Participant), data = InComplete2)

summary(InComplete.lmerBC8IntEnvStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                 9.596e-01  1.207e-03  1.049e+02 794.853  < 2e-16 ***
#   Environment#nV                              3.743e-03  1.316e-03  4.190e+01   2.843 0.006871 ** 
#   Environmentn#C                              1.417e-03  1.040e-03  4.300e+01   1.363 0.180127    
#   Environmentn#V                             -7.689e-03  1.126e-03  4.390e+01  -6.827 2.07e-08 ***
#   BaseInitialStressunstressed                -7.919e-03  1.937e-03  4.910e+01  -4.088 0.000161 ***
#   LocSpeech                                  -7.591e-04  5.859e-05  1.139e+03 -12.956  < 2e-16 ***
#   Environmentn#C:BaseInitialStressunstressed  9.706e-03  2.097e-03  4.810e+01   4.629 2.81e-05 ***
#   Environmentn#V:BaseInitialStressunstressed  5.162e-03  2.066e-03  4.840e+01   2.499 0.015897 *  

# yes, but tehre is a warning, maybe one combination does not exist
# let's see

table(InComplete2$Environment,InComplete2$BaseInitialStress)
#       primary unstressed
# n#nV      67         17
# #nV       74          0
# n#C      304        132
# n#V      150        466

# YES! There are no bases without initial stress, let's have a look at
# the plot

visreg(InComplete.lmerBC8IntEnvStr, "Environment", by="BaseInitialStress",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the one without interactions


anova(InComplete.lmerBC8IntEnvStr,InComplete.lmerBC8)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1     9 -9728.1 -9682.3 4873.1  -9746.1                             
# object 11 -9750.2 -9694.1 4886.1  -9772.2 26.087      2  2.164e-06 ***

#YES

# Environment and Acc

InComplete.lmerBC8IntEnvAcc <- lmer(bc ~ Environment*AccentuationCondition+
                                      BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.588e-01  1.388e-03  1.327e+02 690.825  < 2e-16 ***
#   Environment#nV                                  5.908e-03  1.703e-03  6.360e+01   3.470  0.00094 ***
# Environmentn#C                                  3.233e-03  1.238e-03  6.720e+01   2.611  0.01113 *  
# Environmentn#V                                 -8.446e-03  1.259e-03  6.790e+01  -6.710 4.72e-09 ***
# AccentuationConditionunaccented                -1.830e-03  8.910e-04  1.136e+03  -2.054  0.04018 *  
#   BaseInitialStressunstressed                    -1.270e-03  6.570e-04  4.910e+01  -1.934  0.05892 .  
# LocSpeech                                      -7.480e-04  6.837e-05  1.142e+03 -10.940  < 2e-16 ***
#   Environment#nV:AccentuationConditionunaccented -1.186e-03  1.290e-03  1.130e+03  -0.919  0.35839    
# Environmentn#C:AccentuationConditionunaccented  1.449e-03  9.632e-04  1.130e+03   1.505  0.13269    
# Environmentn#V:AccentuationConditionunaccented  2.499e-03  9.403e-04  1.130e+03   2.658  0.00798 ** 



visreg(InComplete.lmerBC8IntEnvAcc, "Environment", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the one without interactions


anova(InComplete.lmerBC8IntEnvAcc,InComplete.lmerBC8IntEnvStr)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    11 -9750.2 -9694.1 4886.1  -9772.2                        
# object 13 -9739.5 -9673.2 4882.7  -9765.5     0      2          1

# they are the same




# Environment and PrePause
InComplete.lmerBC8IntEnvPause <- lmer(bc ~ Environment*PrePause+BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvPause)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   9.585e-01  1.418e-03  1.503e+02 676.132  < 2e-16 ***
#   Environment#nV                5.130e-03  1.692e-03  6.370e+01   3.031  0.00352 ** 
# Environmentn#C                2.654e-03  1.296e-03  8.200e+01   2.049  0.04370 *  
# Environmentn#V               -7.398e-03  1.313e-03  8.210e+01  -5.632 2.42e-07 ***
# PrePausePause                -8.006e-04  9.426e-04  1.158e+03  -0.849  0.39590    
# BaseInitialStressunstressed  -1.251e-03  6.509e-04  4.880e+01  -1.922  0.06046 .  
# LocSpeech                    -7.640e-04  6.009e-05  1.190e+03 -12.715  < 2e-16 ***
#   Environment#nV:PrePausePause -1.828e-04  1.430e-03  1.148e+03  -0.128  0.89832    
# Environmentn#C:PrePausePause  2.122e-03  1.019e-03  1.152e+03   2.083  0.03746 *  
# Environmentn#V:PrePausePause  3.931e-04  9.915e-04  1.150e+03   0.396  0.69185    

visreg(InComplete.lmerBC8IntEnvPause, "Environment", by="PrePause",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the one without interactions


anova(InComplete.lmerBC8IntEnvStr,InComplete.lmerBC8IntEnvPause)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 11 -9750.2 -9694.1 4886.1  -9772.2                        
# ..1    13 -9733.3 -9667.0 4879.6  -9759.3     0      2          1


# Accentuation and PrePause
InComplete.lmerBC8IntAccPause <- lmer(bc ~ Environment+BaseInitialStress + LocSpeech +
                                      AccentuationCondition*PrePause+(1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntAccPause)

# no

# Stress and PrePause
InComplete.lmerBC8IntPauseStr <- lmer(bc ~ Environment+ PrePause*BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntPauseStr)

# no

# Stress and Acc
InComplete.lmerBC8IntAccStr <- lmer(bc ~ Environment+ AccentuationCondition*BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntAccStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  9.584e-01  1.324e-03  1.098e+02 723.839  < 2e-16 ***
#   Environment#nV                                               5.325e-03  1.584e-03  4.710e+01   3.362  0.00154 ** 
# Environmentn#C                                               3.939e-03  1.146e-03  4.870e+01   3.438  0.00121 ** 
# Environmentn#V                                              -7.166e-03  1.171e-03  5.030e+01  -6.122 1.39e-07 ***
# AccentuationConditionunaccented                             -8.109e-04  3.638e-04  1.172e+03  -2.229  0.02600 *  
#   BaseInitialStressunstressed                                 -2.010e-03  6.996e-04  6.220e+01  -2.873  0.00555 ** 
#   LocSpeech                                                   -7.630e-04  6.834e-05  1.144e+03 -11.165  < 2e-16 ***
#   AccentuationConditionunaccented:BaseInitialStressunstressed  1.442e-03  4.689e-04  1.135e+03   3.074  0.00216 ** 


visreg(InComplete.lmerBC8IntAccStr, "BaseInitialStress", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the one without interactions


anova(InComplete.lmerBC8IntAccStr,InComplete.lmerBC8IntEnvPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 11 -9733.6 -9677.6 4877.8  -9755.6                         
# ..1    13 -9733.3 -9667.0 4879.6  -9759.3 3.6398      2      0.162

# so, we have 3 interactions which make the model equally good

# 1. Env*Stress
#2. Env*Acc
#3. Env*Pause
# 4. Stress*Acc

# Let's try out combinations

######
# First three way:

#EnvStressAcc

InComplete.lmerBC8IntEnvAccStr <- lmer(bc ~ Environment*AccentuationCondition*BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvAccStr)
#no


# EnvStress Pause
InComplete.lmerBC8IntEnvPauseStr <- lmer(bc ~ Environment*PrePause*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvPauseStr)
#no

# Env Acc Pause

InComplete.lmerBC8IntEnvPauseAcc <- lmer(bc ~ Environment*PrePause*AccentuationCondition+BaseInitialStress + LocSpeech + 
                                           (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvPauseAcc)
# no


# Acc Stress Pause
InComplete.lmerBC8IntAccPauseStr <- lmer(bc ~ Environment+AccentuationCondition*PrePause*BaseInitialStress + LocSpeech + 
                                           (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntAccPauseStr)

# no

###############
# So no 3-way interactions, now let's test the combinations of 2-way interactions



# Env*Stress and Env*Acc
InComplete.lmerBC8IntEnvAccEnvStr <- lmer(bc ~ Environment*AccentuationCondition+Environment*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvAccEnvStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.601e-01  1.295e-03  1.397e+02 741.435  < 2e-16 ***
#   Environment#nV                                  4.282e-03  1.445e-03  6.500e+01   2.963 0.004252 ** 
# Environmentn#C                                  6.123e-04  1.131e-03  6.440e+01   0.541 0.590254    
# Environmentn#V                                 -9.064e-03  1.208e-03  6.200e+01  -7.503 2.89e-10 ***
# AccentuationConditionunaccented                -1.953e-03  8.908e-04  1.136e+03  -2.192 0.028596 *  
#   BaseInitialStressunstressed                    -7.948e-03  1.906e-03  4.930e+01  -4.169 0.000123 ***
#   LocSpeech                                      -7.195e-04  6.701e-05  1.014e+03 -10.737  < 2e-16 ***
#   Environment#nV:AccentuationConditionunaccented -1.160e-03  1.291e-03  1.129e+03  -0.899 0.368998    
# Environmentn#C:AccentuationConditionunaccented  1.494e-03  9.635e-04  1.130e+03   1.551 0.121175    
# Environmentn#V:AccentuationConditionunaccented  2.585e-03  9.405e-04  1.130e+03   2.749 0.006077 ** 
# Environmentn#C:BaseInitialStressunstressed      9.818e-03  2.063e-03  4.820e+01   4.758 1.81e-05 ***
# Environmentn#V:BaseInitialStressunstressed      5.191e-03  2.033e-03  4.860e+01   2.554 0.013855 *  


# yes, is this better?


anova(InComplete.lmerBC8IntEnvAccEnvStr,InComplete.lmerBC8)
# ..1     9 -9728.1 -9682.3 4873.1  -9746.1                             
# object 15 -9762.8 -9686.3 4896.4  -9792.8 46.665      6  2.183e-08 ***

#yes

# Env*Stress and Env* Pause
InComplete.lmerBC8IntEnvStrEnvPause <- lmer(bc ~ Environment*PrePause+Environment*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvStrEnvPause)

#no

# Env*Stress and Acc*Stress

InComplete.lmerBC8IntEnvStressStrAcc <- lmer(bc ~ Environment*BaseInitialStress+AccentuationCondition*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvStressStrAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  9.598e-01  1.229e-03  1.108e+02 780.774  < 2e-16 ***
#   Environment#nV                                               3.734e-03  1.309e-03  4.190e+01   2.853  0.00671 ** 
# Environmentn#C                                               1.368e-03  1.034e-03  4.310e+01   1.323  0.19274    
# Environmentn#V                                              -7.722e-03  1.121e-03  4.400e+01  -6.890 1.65e-08 ***
# BaseInitialStressunstressed                                 -8.610e-03  1.940e-03  5.060e+01  -4.438 4.92e-05 ***
#   AccentuationConditionunaccented                             -8.791e-04  3.626e-04  1.178e+03  -2.424  0.01550 *  
#   LocSpeech                                                   -7.348e-04  6.705e-05  1.025e+03 -10.958  < 2e-16 ***
#   Environmentn#C:BaseInitialStressunstressed                   9.709e-03  2.085e-03  4.810e+01   4.656 2.56e-05 ***
# Environmentn#V:BaseInitialStressunstressed                   5.109e-03  2.054e-03  4.850e+01   2.487  0.01638 *  
# BaseInitialStressunstressed:AccentuationConditionunaccented  1.458e-03  4.689e-04  1.136e+03   3.109  0.00192 ** 

# yes, is this tha the other with2 interactions?


anova(InComplete.lmerBC8IntEnvAccEnvStr,InComplete.lmerBC8IntEnvStressStrAcc)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# ..1    13 -9756.1 -9689.9 4891.1  -9782.1                            
# object 15 -9762.8 -9686.3 4896.4  -9792.8 10.662      2   0.004839 **

# no!

# Env*Acc and Env*Pause

InComplete.lmerBC8IntEnvAccEnvPause <- lmer(bc ~ Environment*AccentuationCondition+ Environment*PrePause+BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvAccEnvPause)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.593e-01  1.510e-03  1.950e+02 635.159  < 2e-16 ***
#   Environment#nV                                  7.124e-03  1.904e-03  1.056e+02   3.741 0.000298 ***
# Environmentn#C                                  1.420e-03  1.414e-03  1.203e+02   1.004 0.317208    
# Environmentn#V                                 -9.162e-03  1.427e-03  1.184e+02  -6.418 2.97e-09 ***
# AccentuationConditionunaccented                -2.091e-03  8.993e-04  1.133e+03  -2.325 0.020269 *  
#   PrePausePause                                  -1.204e-03  9.488e-04  1.156e+03  -1.269 0.204847    
# BaseInitialStressunstressed                    -1.217e-03  6.440e-04  4.900e+01  -1.890 0.064719 .  
# LocSpeech                                      -7.224e-04  6.807e-05  1.136e+03 -10.612  < 2e-16 ***
#   Environment#nV:AccentuationConditionunaccented -2.927e-03  1.426e-03  1.131e+03  -2.053 0.040329 *  
# Environmentn#C:AccentuationConditionunaccented  1.918e-03  9.723e-04  1.127e+03   1.973 0.048728 *  
# Environmentn#V:AccentuationConditionunaccented  2.665e-03  9.488e-04  1.127e+03   2.809 0.005057 ** 
# Environment#nV:PrePausePause                   -2.839e-03  1.581e-03  1.146e+03  -1.795 0.072862 .  
# Environmentn#C:PrePausePause                    2.521e-03  1.025e-03  1.149e+03   2.459 0.014096 *  
# Environmentn#V:PrePausePause                    9.270e-04  9.975e-04  1.147e+03   0.929 0.352892  

anova(InComplete.lmerBC8IntEnvAccEnvStr,InComplete.lmerBC8IntEnvAccEnvPause)
# Data: InComplete2
# Models:
#   object: bc ~ Environment * AccentuationCondition + Environment * BaseInitialStress + 
#   object:     LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + Environment * PrePause + 
#   ..1:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 15 -9762.8 -9686.3 4896.4  -9792.8                        
# ..1    17 -9753.8 -9667.1 4893.9  -9787.8     0      2          1

# as good as model with interaction between Env and Stress and Env and Acc

# Env*Acc and Acc*Stress

InComplete.lmerBC8IntEnvAccStrAcc <- lmer(bc ~ Environment*AccentuationCondition+AccentuationCondition*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvAccStrAcc)

# no

#Env*Pause and Acc*Stress

InComplete.lmerBC8IntEnvPauseAccStr <- lmer(bc ~ Environment*PrePause+AccentuationCondition*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvPauseAccStr)



# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  9.589e-01  1.433e-03  1.558e+02 669.033  < 2e-16 ***
#   Environment#nV                                               5.125e-03  1.688e-03  6.370e+01   3.036  0.00347 ** 
# Environmentn#C                                               2.562e-03  1.293e-03  8.230e+01   1.981  0.05095 .  
# Environmentn#V                                              -7.618e-03  1.314e-03  8.290e+01  -5.799 1.17e-07 ***
# PrePausePause                                               -1.005e-03  9.447e-04  1.159e+03  -1.064  0.28754    
# AccentuationConditionunaccented                             -8.324e-04  3.738e-04  1.172e+03  -2.227  0.02613 *  
#   BaseInitialStressunstressed                                 -1.950e-03  6.924e-04  6.300e+01  -2.816  0.00648 ** 
#   LocSpeech                                                   -7.462e-04  6.829e-05  1.139e+03 -10.926  < 2e-16 ***
#   Environment#nV:PrePausePause                                -4.860e-04  1.431e-03  1.145e+03  -0.340  0.73417    
# Environmentn#C:PrePausePause                                 2.207e-03  1.016e-03  1.151e+03   2.171  0.03011 *  
# Environmentn#V:PrePausePause                                 6.697e-04  9.929e-04  1.149e+03   0.674  0.50016    
# AccentuationConditionunaccented:BaseInitialStressunstressed  1.415e-03  4.767e-04  1.133e+03   2.969  0.00305 ** 

anova(InComplete.lmerBC8IntEnvAccEnvStr,InComplete.lmerBC8IntEnvPauseAccStr)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 15 -9762.8 -9686.3 4896.4  -9792.8                        
# ..1    15 -9738.2 -9661.8 4884.1  -9768.2     0      0          1




anova(InComplete.lmerBC8IntEnvPauseAccStr,InComplete.lmerBC8IntEnvAccEnvPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -9738.2 -9661.8 4884.1  -9768.2                             
# ..1    17 -9753.8 -9667.1 4893.9  -9787.8 19.569      2  5.631e-05 ***


# okay this one is worse


# I want to try out one last thing - 3 2-way interacrions



InComplete.lmerBC8IntEnvStressEnvPauseAccStr <- lmer(bc ~ Environment*BaseInitialStress+
                                                       Environment*PrePause+AccentuationCondition*BaseInitialStress + LocSpeech + 
                                              (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvStressEnvPauseAccStr)
# no the interaction with pause vanishes



InComplete.lmerBC8IntEnvStressEnvAccAccStr <- lmer(bc ~ Environment*BaseInitialStress+
                                                       Environment*AccentuationCondition+AccentuationCondition*BaseInitialStress + LocSpeech + 
                                                       (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvStressEnvAccAccStr)

# no the interaction with Stress and Acc vanishes

InComplete.lmerBC8IntEnvStressEnvAccEnvPause <- lmer(bc ~ Environment*BaseInitialStress+
                                                     Environment*AccentuationCondition+Environment*PrePause + LocSpeech + 
                                                     (1|Item) + (1|Participant), data = InComplete2)


summary(InComplete.lmerBC8IntEnvStressEnvAccEnvPause)

#Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.605e-01  1.403e-03  2.065e+02 684.709  < 2e-16 ***
#   Environment#nV                                  5.766e-03  1.662e-03  1.225e+02   3.469 0.000722 ***
# Environmentn#C                                 -9.951e-04  1.293e-03  1.191e+02  -0.769 0.443163    
# Environmentn#V                                 -9.518e-03  1.349e-03  1.052e+02  -7.056 1.89e-10 ***
# BaseInitialStressunstressed                    -7.652e-03  1.877e-03  5.120e+01  -4.077 0.000159 ***
#   AccentuationConditionunaccented                -2.173e-03  8.990e-04  1.135e+03  -2.417 0.015797 *  
#   PrePausePause                                  -9.397e-04  9.539e-04  1.136e+03  -0.985 0.324759    
# LocSpeech                                      -6.923e-04  6.666e-05  1.002e+03 -10.385  < 2e-16 ***
#   Environmentn#C:BaseInitialStressunstressed      9.574e-03  2.028e-03  4.970e+01   4.721 1.95e-05 ***
# Environmentn#V:BaseInitialStressunstressed      4.905e-03  1.999e-03  5.020e+01   2.454 0.017645 *  
# Environment#nV:AccentuationConditionunaccented -2.968e-03  1.425e-03  1.133e+03  -2.083 0.037510 *  
# Environmentn#C:AccentuationConditionunaccented  1.930e-03  9.723e-04  1.127e+03   1.985 0.047403 *  
# Environmentn#V:AccentuationConditionunaccented  2.713e-03  9.489e-04  1.127e+03   2.859 0.004331 ** 
# Environment#nV:PrePausePause                   -3.153e-03  1.582e-03  1.146e+03  -1.994 0.046393 *  
# Environmentn#C:PrePausePause                    2.311e-03  1.030e-03  1.134e+03   2.245 0.024977 *  
# Environmentn#V:PrePausePause                    6.775e-04  1.002e-03  1.130e+03   0.676 0.499021    


anova(InComplete.lmerBC8IntEnvStressEnvAccEnvPause,InComplete.lmerBC8IntEnvAccEnvPause)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    17 -9753.8 -9667.1 4893.9  -9787.8                             
# object 19 -9777.7 -9680.8 4907.8  -9815.7 27.876      2  8.848e-07 ***

# so that is the final model - it has 3 2-way interactions!




######################################################
# Summary: 
# There are a lot of interaction s significant, also the combinations...but
# the best model is the one with 2 3 way interactions

visreg(InComplete.lmerBC8IntEnvStressEnvAccEnvPause,"Environment", by="BaseInitialStress", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,
       ylim=c(30,120))


visreg(InComplete.lmerBC8IntEnvStressEnvAccEnvPause,"Environment", by="PrePause", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,cond = list(BaseInitialStress="primary"),
       ylim=c(30,120))

visreg(InComplete.lmerBC8IntEnvStressEnvAccEnvPause,"Environment", by="AccentuationCondition", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,
       ylim=c(30,120))

#############################################################
# The final model:

summary(InComplete.lmerBC8IntEnvStressEnvAccEnvPause)


#Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.605e-01  1.403e-03  2.065e+02 684.709  < 2e-16 ***
#   Environment#nV                                  5.766e-03  1.662e-03  1.225e+02   3.469 0.000722 ***
# Environmentn#C                                 -9.951e-04  1.293e-03  1.191e+02  -0.769 0.443163    
# Environmentn#V                                 -9.518e-03  1.349e-03  1.052e+02  -7.056 1.89e-10 ***
# BaseInitialStressunstressed                    -7.652e-03  1.877e-03  5.120e+01  -4.077 0.000159 ***
#   AccentuationConditionunaccented                -2.173e-03  8.990e-04  1.135e+03  -2.417 0.015797 *  
#   PrePausePause                                  -9.397e-04  9.539e-04  1.136e+03  -0.985 0.324759    
# LocSpeech                                      -6.923e-04  6.666e-05  1.002e+03 -10.385  < 2e-16 ***
#   Environmentn#C:BaseInitialStressunstressed      9.574e-03  2.028e-03  4.970e+01   4.721 1.95e-05 ***
# Environmentn#V:BaseInitialStressunstressed      4.905e-03  1.999e-03  5.020e+01   2.454 0.017645 *  
# Environment#nV:AccentuationConditionunaccented -2.968e-03  1.425e-03  1.133e+03  -2.083 0.037510 *  
# Environmentn#C:AccentuationConditionunaccented  1.930e-03  9.723e-04  1.127e+03   1.985 0.047403 *  
# Environmentn#V:AccentuationConditionunaccented  2.713e-03  9.489e-04  1.127e+03   2.859 0.004331 ** 
# Environment#nV:PrePausePause                   -3.153e-03  1.582e-03  1.146e+03  -1.994 0.046393 *  
# Environmentn#C:PrePausePause                    2.311e-03  1.030e-03  1.134e+03   2.245 0.024977 *  
# Environmentn#V:PrePausePause                    6.775e-04  1.002e-03  1.130e+03   0.676 0.499021    

  lambda
#[1] 0.02020202


# I need to rename some variabels for the plot...


InComplete2<-rename(InComplete2,AccentuationAnnotator=Accentuation)

InComplete2<-rename(InComplete2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(InComplete2$BaseInitialStress)
#[1] "prInary"    "unstressed"


levels(InComplete2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(InComplete2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# need to rename the pause levels

levels(InComplete2$PrePause)
#[1] [1] "No Pause" "Pause"   



levels(InComplete2$PrePause)<-c("no pause"   , "pause")

levels(InComplete2$PrePause)
#[1] "no pause" "pause"   

# also need to change ref levels for environment



final_In_complete_model.lmer<-lmer(bc ~  Environment*BaseInitialStress+Environment*PrePause+
                                     Environment*Accentuation+ LocSpeech                                             
                                    +(1|Participant)+ (1|Item) , data = InComplete2)                                 


summary(final_In_complete_model.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                 9.605e-01  1.403e-03  2.065e+02 684.709  < 2e-16 ***
#   Environment#nV                              5.766e-03  1.662e-03  1.225e+02   3.469 0.000722 ***
# Environmentn#C                             -9.951e-04  1.293e-03  1.191e+02  -0.769 0.443163    
# Environmentn#V                             -9.518e-03  1.349e-03  1.052e+02  -7.056 1.89e-10 ***
# BaseInitialStressunstressed                -7.652e-03  1.877e-03  5.120e+01  -4.077 0.000159 ***
#   PrePausePause                              -9.397e-04  9.539e-04  1.136e+03  -0.985 0.324759    
# Accentuationunaccented                     -2.173e-03  8.990e-04  1.135e+03  -2.417 0.015797 *  
#   LocSpeech                                  -6.923e-04  6.666e-05  1.002e+03 -10.385  < 2e-16 ***
#   Environmentn#C:BaseInitialStressunstressed  9.574e-03  2.028e-03  4.970e+01   4.721 1.95e-05 ***
# Environmentn#V:BaseInitialStressunstressed  4.905e-03  1.999e-03  5.020e+01   2.454 0.017645 *  
# Environment#nV:PrePausePause               -3.153e-03  1.582e-03  1.146e+03  -1.994 0.046393 *  
# Environmentn#C:PrePausePause                2.311e-03  1.030e-03  1.134e+03   2.245 0.024977 *  
# Environmentn#V:PrePausePause                6.775e-04  1.002e-03  1.130e+03   0.676 0.499021    
# Environment#nV:Accentuationunaccented      -2.968e-03  1.425e-03  1.133e+03  -2.083 0.037510 *  
# Environmentn#C:Accentuationunaccented       1.930e-03  9.723e-04  1.127e+03   1.985 0.047403 *  
# Environmentn#V:Accentuationunaccented       2.713e-03  9.489e-04  1.127e+03   2.859 0.004331 ** 

#############
# Let's get the  model for the dissertation


table_final_models<-as.data.frame(coef(summary(final_In_complete_model.lmer)))

xtable(table_final_models,digits = 3)



#change directory for plots

# need to create the variable ConsonantDurMS

InComplete$ConsonantDurMS<-InComplete$ConsonantDur*1000

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/Images/Experiment")

# I need to change the ref level

InComplete$Environment <- relevel (InComplete$Environment, ref= "#nV")
InComplete$Environment <- relevel (InComplete$Environment, ref= "n#V")
InComplete$Environment <- relevel (InComplete$Environment, ref= "n#C")
InComplete$Environment <- relevel (InComplete$Environment, ref= "n#nV")



png("boxIn.png", units="cm", height=10, width=7, res=300, pointsize=8)
cols = list(col=c("dodgerblue3"),pch=c(1))
bwplot (ConsonantDurMS ~ Environment, InComplete, ylab="duration in milliseconds", 
        main="in-", ylim=c(0,320), cex.axis=0.5,
        par.settings = list(
          plot.symbol=cols,
          box.rectangle = cols,
          #box.dot = cols,
          box.umbrella=cols 
        ))

dev.off()


##############################
# We should  plot the main effect (not covariates)
###############################
# Plot main effect

png("InModelCompleteInterEnvAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das wei_ In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(final_In_complete_model.lmer, "Environment",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim, band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()



png("InModelCompleteInterEnvStress.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das wei_ In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(final_In_complete_model.lmer, "Environment",by="BaseInitialStress",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()

png("InModelCompleteInterEnvPause.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das wei_ In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(final_In_complete_model.lmer, "Environment",by="PrePause",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim, band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()



library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the Inportance of the
# different factors.

options(na.action = "na.fail") 


InComplete.lm1<- lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause, 
   data = InComplete)

model_ranking <- dredge(InComplete.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Relative variable importance: 
#   Environment LocSpeech PostPause GlobalSpeechRate logWordFormFreq
# Importance:          1.00        1.00      1.00      0.88             0.79           
# N containing models:  256         256       256       256              256           
# BaseInitialStress Accentuation PrePause OrderRescale
# Importance:          0.76              0.70         0.27     0.27        
# N containing models:  256               256          256      256 


# let's check a MuMin with interactions


options(na.action = "na.fail") 


InComplete.lm2<- lm(ConsonantDur ~ Environment*BaseInitialStress*Accentuation + 
                      OrderRescale +Environment*PrePause*BaseInitialStress+ logWordFormFreq +  + LocSpeech + 
                      GlobalSpeechRate + PrePause*BaseInitialStress*Accentuation + PostPause, 
                    data = InComplete)

model_ranking2 <- dredge(InComplete.lm2)

model_average_2<-model.avg(model_ranking2)


summary(model_average_2)


# Relative variable importance: 
#                  BaseInitialStress Environment LocSpeech BaseInitialStress:Environment Accentuation
# Importance:          1.00              1.00        1.00      1.00                          1.00        
# N containing models: 4128              4096        2352      2240                          4096        
#                     Accentuation:Environment PrePause Environment:PrePause PostPause Accentuation:BaseInitialStress
# Importance:          1.00                     1.00     1.00                 1.00      1.00                          
# N containing models: 2048                     4096     2048                 2352      2240                          
#                      GlobalSpeechRate Accentuation:BaseInitialStress:Environment BaseInitialStress:PrePause
# Importance:          0.99             0.97                                       0.96                      
# N containing models: 2352              448                                       2240                      
#                     Accentuation:PrePause Accentuation:BaseInitialStress:PrePause logWordFormFreq OrderRescale
# Importance:          0.95                  0.94                                    0.45            0.29        
# N containing models: 2048                   448                                    2352            2352        
#                     BaseInitialStress:Environment:PrePause
# Importance:          0.16                                  
# N containing models:  448     

