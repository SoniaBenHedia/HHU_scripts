#Loaddisg libraries

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
#library(disfluence.ME)
library(multcomp)
library(dplyr)

# set the directory, so R knows where to fdisd a file


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses dis/")



disComplete <- read.csv("disComplete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(disComplete)

# $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  5000 5001 5002 5003 5004 5005 5006 5007 5008 5009 ...
# $ Item                       : Factor w/ 59 levels "disabled","disabuse",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 22 levels "participant_10A_Experiment_2",..: 9 17 1 5 21 15 3 8 19 11 ...
# $ ID                         : int  2245 3955 152 1296 4898 3347 1045 1892 4531 2677 ...
# $ Filename                   : Factor w/ 1114 levels "participant_1_A_102.TextGrid",..: 446 856 129 312 1035 696 252 357 943 551 ...
# $ DeletionMorph              : Factor w/ 3 levels "L","N","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 1 2 2 1 1 1 1 2 1 2 ...
# $ Annotator                  : Factor w/ 4 levels "Mandy","Simon",..: 3 4 4 3 4 4 1 3 3 4 ...
# $ Order                      : int  323 72 316 155 224 103 99 116 301 34 ...
# $ WordDur                    : num  0.456 0.538 0.656 0.625 0.755 ...
# $ SyllNum                    : int  3 3 3 3 3 3 3 3 3 3 ...
# $ SegNum                     : int  7 7 7 8 7 7 7 8 7 7 ...
# $ ConsonantDur               : num  0.0887 0.099 0.1299 0.105 0.0928 ...
# $ PrecSeg                    : Factor w/ 8 levels "@","A","d","f",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0455 0.0326 0.0473 0.0711 0.0744 ...
# $ FollSeg                    : Factor w/ 36 levels "@","@O","@U",..: 18 18 18 18 12 18 18 18 18 18 ...
# $ FollSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur                 : num  0.135 0.149 0.158 0.169 0.176 ...
# $ PrePauseDur                : num  0.0548 0.0759 0.0972 0 0.0813 ...
# $ PostPauseDur               : num  0 0 0 0 0.337 ...
# $ SentenceDur                : num  3.59 7.23 4.67 4.13 4.4 ...
# $ GlottalStop                : Factor w/ 1 level "NoGlottalStop": 1 1 1 1 1 1 1 1 1 1 ...
# $ GlottalStopDur             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ LocSpeech                  : num  15.36 13.02 10.67 12.8 9.28 ...
# $ AffixDur                   : num  0.134 0.132 0.177 0.176 0.167 ...
# $ BaseDuration               : num  0.302 0.378 0.468 0.43 0.569 ...
# $ FirstSyllDur               : num  0.154 0.16 0.188 0.195 0.185 ...
# $ WordDurWithoutGlottalStop  : num  0.456 0.538 0.656 0.625 0.755 ...
# $ AffixDurWithoutGlottalStop : num  0.134 0.132 0.177 0.176 0.167 ...
# $ Environment                : Factor w/ 4 levels "#sV","s#sV","s#V",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Affix                      : Factor w/ 1 level "dis": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  3095 3095 3095 3095 3095 3095 3095 3095 3095 3095 ...
# $ WordFormFrequencyAllCOCA   : int  6368 6368 6368 6368 6368 6368 6368 6368 6368 6368 ...
# $ WordFormFrequencySpokenCOCA: int  812 812 812 812 812 812 812 812 812 812 ...
# $ Base                       : Factor w/ 50 levels "abled","abuse",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  3095 3095 3095 3095 3095 3095 3095 3095 3095 3095 ...
# $ BaseLemmaFrequencyBNC      : int  4 4 4 4 4 4 4 4 4 4 ...
# $ SyllPhon                   : int  3 3 3 3 3 3 3 3 3 3 ...
# $ AffixStress                : Factor w/ 2 levels "primary","unstressed": 2 2 2 2 2 2 2 2 2 2 ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 3 levels "opaque","simplex",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  2 1 2 1 1 1 2 2 3 1 ...
# $ TimeRating                 : num  652 681 691 752 678 ...
# $ TotalTime                  : num  769 793 786 873 777 ...
# $ Age                        : int  20 33 25 19 20 20 31 21 21 65 ...
# $ Sex                        : Factor w/ 2 levels "female","male": 1 2 1 2 1 2 2 2 2 2 ...
# $ L1                         : Factor w/ 2 levels "British English",..: 1 1 1 1 2 1 1 1 1 1 ...
# $ Bilingual                  : Factor w/ 1 level "no": 1 1 1 1 1 1 1 1 1 1 ...
# $ Grow_Up_Region             : Factor w/ 21 levels "Aberdeen. Scotland",..: 7 8 19 17 12 10 5 13 11 1 ...
# $ Languages                  : Factor w/ 14 levels "Basic French",..: 10 12 12 4 13 12 9 12 3 5 ...
# $ Latin                      : Factor w/ 6 levels "I know a few words and phrases. I have never studied it",..: 2 2 2 2 2 1 4 2 5 6 ...
# $ Profession_Studies         : Factor w/ 22 levels "2nd Year Meida Studies",..: 11 12 7 16 6 9 14 13 18 4 ...
# $ University                 : Factor w/ 9 levels "Aberdeen University",..: 2 2 4 9 3 4 8 4 9 1 ...
# $ Knowledge_English_Ling     : Factor w/ 10 levels "2 years","no",..: 1 2 5 9 7 8 10 6 2 2 ...
# $ Phonetics                  : Factor w/ 6 levels "I went to a few lectures on phonetics in my first year of university.",..: 4 2 4 4 5 4 2 2 1 2 ...
# $ Phonology                  : Factor w/ 5 levels "no","The above lecture also covered phonology.",..: 4 1 4 4 4 4 1 1 2 1 ...
# $ Morphology                 : Factor w/ 4 levels "no","Year 1 ARU",..: 3 1 3 1 3 3 1 1 1 1 ...
# $ Semantics                  : Factor w/ 4 levels "no","Year 1 ARU",..: 3 1 3 1 3 3 1 1 1 1 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 2 1 1 1 2 1 1 1 2 ...
# $ Experiment                 : Factor w/ 1 level "Experiment_2": 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  8.04 8.04 8.04 8.04 8.04 ...
# $ logBaseLemmaFreq           : num  1.39 1.39 1.39 1.39 1.39 ...
# $ logWordLemmaFreq           : num  8.04 8.04 8.04 8.04 8.04 ...
# $ RelFreq                    : num  774 774 774 774 774 ...
# $ logRelFreq                 : num  6.65 6.65 6.65 6.65 6.65 ...
# $ Root                       : Factor w/ 45 levels "able","abuse",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : num  0.167 0.229 0.309 0.261 0.393 ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ Consonant                  : Factor w/ 8 levels "I","I@","l","l?",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ Orthography                : Factor w/ 3 levels "s","sC","ss": 1 1 1 1 1 1 1 1 1 1 ...
# $ median                     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ ConsonantDurMS             : num  88.7 99 129.9 105 92.8 ...
# $ PrePause                   : Factor w/ 2 levels "no pause","pause": 2 2 2 1 2 2 2 2 2 2 ...
# $ PostPause                  : Factor w/ 2 levels "no pause","pause": 1 1 1 1 2 1 2 1 1 1 ...
# $ GlobalSpeechRate           : num  2.506 1.244 0.857 0.969 0.91 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ StressPattern              : Factor w/ 5 levels "NA-primary","NA-unstressed",..: 4 4 4 4 4 4 4 4 4 4 ...

disComplete$X.1<-NULL
disComplete$X<-NULL

###############################################################
#   Summary: variables to disclude                            ##
###############################################################

## We are godisg to disclude the followdisg predictors:

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
#                 fittdisg a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be expladised by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = disComplete)
cor(disComplete$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.13746


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = disComplete)
cor(disComplete$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.3518621

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = disComplete)
cor(disComplete$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.4897286

# so around 49 percent of the variability can be expladised by this! 

# before we do an disitial model, let's set the reference level right

disComplete$Environment <- relevel (disComplete$Environment, ref= "s#sV")

levels(disComplete$Environment)
#[1] "s#sV" "#sV"  "s#V"  "sV"  



##########################################################
##              Do an disitial model:
disComplete$OrderRescale<-disComplete$Order*0.1

disComplete.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + 
                          (1|Item) + (1|Participant), data = disComplete)


summary(disComplete.lmer1)    

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.815e-01  7.425e-03  4.144e+02  24.440  < 2e-16 ***
#   Environment#sV                   1.489e-02  5.287e-03  7.110e+01   2.816 0.006286 ** 
# Environments#V                  -2.443e-02  4.534e-03  4.840e+01  -5.389 2.08e-06 ***
# EnvironmentsV                   -1.333e-02  6.967e-03  4.790e+01  -1.913 0.061724 .  
# AccentuationConditionunaccented -2.979e-03  2.075e-03  1.001e+03  -1.436 0.151274    
# OrderRescale                    -1.347e-04  6.750e-05  1.053e+03  -1.995 0.046264 *  
#   logWordFormFreq                 -5.621e-04  5.571e-04  4.700e+01  -1.009 0.318142    
# BaseInitialStressunstressed     -3.288e-03  4.265e-03  4.650e+01  -0.771 0.444629    
# LocSpeech                       -4.779e-03  4.154e-04  8.578e+02 -11.504  < 2e-16 ***
#   GlobalSpeechRate                 2.890e-03  1.917e-03  9.181e+02   1.507 0.132144    
# PrePausepause                    1.060e-02  2.898e-03  1.051e+03   3.657 0.000268 ***
#   PostPausepause                   2.648e-03  1.803e-03  1.099e+03   1.468 0.142263    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envrnmnt#sV Envrnmnts#V EnvrnV AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsps
# Envrnmnt#sV -0.358                                                                                
# Envrnmnts#V -0.202  0.318                                                                         
# EnvirnmntsV -0.051  0.264       0.654                                                             
# AccnttnCndt  0.233 -0.043       0.031       0.011                                                 
# OrderRescal -0.094 -0.009       0.003       0.023  0.135                                          
# lgWrdFrmFrq -0.257 -0.363      -0.017      -0.199  0.028 -0.022                                   
# BsIntlStrss -0.027 -0.054      -0.680      -0.627 -0.014 -0.017  0.129                            
# LocSpeech   -0.689  0.139      -0.029      -0.063 -0.313 -0.033  0.023 -0.004                     
# GloblSpchRt -0.247  0.038      -0.020      -0.001 -0.665 -0.168 -0.038  0.021  0.043              
# PrePausepas -0.413  0.413       0.017      -0.004  0.040  0.001  0.011 -0.015  0.027  0.033       
# PostPauseps -0.372  0.023       0.005      -0.041  0.165  0.067  0.010  0.002  0.367  0.043 -0.053

cor(disComplete$ConsonantDur, fitted(disComplete.lmer1))^2
#[1]0.6041098

# not bad but not good either



#######################################################################################
# Dealing with colldisearity                                                           #
######################################################################################

# Before slimmdisg down the model we should deal with possible colldisearity problems


# I will do so, by lookdisg at what happens if both varables stay dis a model, what happens
# if one is thrown out and also which effect each one has on its own




# 1.  Loc Speech  and/or Global Speech


cor.test(disComplete$LocSpeech,disComplete$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  disComplete$LocSpeech and disComplete$GlobalSpeechRate
# t = 12.229, df = 1112, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.2914660 0.3950513
# sample estimates:
# cor 
# 0.344306 


disComplete.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = disComplete)

summary(disComplete.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.898e-01  4.985e-03  3.417e+02  38.081   <2e-16 ***
#   LocSpeech        -5.964e-03  3.639e-04  1.109e+03 -16.389   <2e-16 ***
#   GlobalSpeechRate  7.363e-04  1.403e-03  1.073e+03   0.525      0.6  

cor(disComplete$ConsonantDur, fitted(disComplete.lmerSpeechRates))^2
#[1]  0.600387



disComplete.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = disComplete)

summary(disComplete.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.898e-01  4.978e-03  3.450e+02   38.14   <2e-16 ***
#   LocSpeech   -5.888e-03  3.331e-04  1.108e+03  -17.67   <2e-16 ***

cor(disComplete$ConsonantDur, fitted(disComplete.lmerLocSpeech))^2
#[1] 0.6001395


disComplete.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = disComplete)

print(summary(disComplete.lmerGlobalSpeech),digits=3)

# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)       1.30e-01   3.76e-03  9.60e+01   34.53  < 2e-16 ***
#   GlobalSpeechRate -8.49e-03   1.43e-03  1.04e+03   -5.93  4.2e-09 ***

cor(disComplete$ConsonantDur, fitted(disComplete.lmerGlobalSpeech))^2
#[1] 0.5040824


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae dis the model, only LocSpeech has a sign effect
# - The effect direction changes gfor GlobSpeech when both variables are in the model
# - Since Locspeech is the better predictor, I'll just keeo that variable in the model
#################################################



###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (disComplete.lmer1))
qqline (residuals (disComplete.lmer1))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a ldisear relation between the dependent
# and the disdependent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Maybe a box-cox transformation will lead to a better
# distribuition of res. Let's try


disComplete.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + 
                    PrePause + PostPause, data = disComplete)

summary(disComplete.lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      1.790e-01  5.953e-03  30.064  < 2e-16 ***
#   Environment#sV                   1.342e-02  3.600e-03   3.729 0.000202 ***
# Environments#V                  -2.547e-02  2.498e-03 -10.198  < 2e-16 ***
# EnvironmentsV                   -1.481e-02  3.820e-03  -3.877 0.000112 ***
#   AccentuationConditionunaccented -2.156e-03  1.686e-03  -1.279 0.201167    
# OrderRescale                    -1.704e-04  7.585e-05  -2.247 0.024864 *  
#   logWordFormFreq                 -4.548e-04  3.003e-04  -1.515 0.130169    
# BaseInitialStressunstressed     -3.382e-03  2.289e-03  -1.478 0.139769    
# LocSpeech                       -4.028e-03  3.553e-04 -11.336  < 2e-16 ***
#   PrePausepause                    8.878e-03  3.259e-03   2.724 0.006549 ** 
#   PostPausepause                   2.612e-03  1.763e-03   1.482 0.138700    

bc<-boxcox(disComplete.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.3434343

disComplete$bc <- disComplete$ConsonantDur^lambda

disComplete.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + 
                            PrePause + PostPause +(1|Item) + (1|Participant), data = disComplete)

summary(disComplete.lmerBC)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      5.663e-01  9.690e-03  3.970e+02  58.438  < 2e-16 ***
#   Environment#sV                   1.706e-02  7.202e-03  7.090e+01   2.369  0.02057 *  
# Environments#V                  -3.172e-02  6.200e-03  4.890e+01  -5.116 5.21e-06 ***
# EnvironmentsV                   -1.565e-02  9.529e-03  4.860e+01  -1.643  0.10688    
# AccentuationConditionunaccented -1.914e-03  2.081e-03  1.086e+03  -0.920  0.35788    
# OrderRescale                    -1.635e-04  8.936e-05  1.052e+03  -1.830  0.06758 .  
# logWordFormFreq                 -7.878e-04  7.615e-04  4.750e+01  -1.034  0.30614    
# BaseInitialStressunstressed     -4.854e-03  5.833e-03  4.710e+01  -0.832  0.40951    
# LocSpeech                       -6.313e-03  5.586e-04  8.760e+02 -11.301  < 2e-16 ***
#   PrePausepause                    1.227e-02  3.889e-03  1.052e+03   3.155  0.00165 ** 
#   PostPausepause                   2.304e-03  2.420e-03  1.100e+03   0.952  0.34129   

#let's check the assumptions

qqnorm (residuals (disComplete.lmerBC))
qqline (residuals (disComplete.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(disComplete.lmerBC, disComplete, trim = 2.5)
# n.removed = 24 
# percent.removed = 2.154399 

disComplete2<-outliers$data

dim(disComplete2)
#[1]1090   85

dim(disComplete)
# 1114   84


# okay it seemes to have worked

disComplete.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + 
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = disComplete2)

summary(disComplete.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      5.538e-01  8.556e-03  3.490e+02  64.730  < 2e-16 ***
#   Environment#sV                   1.980e-02  6.921e-03  6.690e+01   2.862  0.00562 ** 
# Environments#V                  -3.141e-02  6.110e-03  5.090e+01  -5.141 4.41e-06 ***
# EnvironmentsV                   -1.688e-02  9.399e-03  5.060e+01  -1.796  0.07852 .  
# AccentuationConditionunaccented -3.741e-03  1.729e-03  1.054e+03  -2.164  0.03070 *  
#   OrderRescale                    -1.153e-04  7.287e-05  1.021e+03  -1.583  0.11377    
# logWordFormFreq                 -6.589e-04  7.529e-04  4.990e+01  -0.875  0.38572    
# BaseInitialStressunstressed     -5.243e-03  5.761e-03  4.930e+01  -0.910  0.36720    
# LocSpeech                       -5.306e-03  4.756e-04  9.928e+02 -11.157  < 2e-16 ***
#   PrePausepause                    1.287e-02  3.190e-03  1.023e+03   4.035 5.86e-05 ***
#   PostPausepause                   6.185e-04  1.996e-03  1.073e+03   0.310  0.75674    

qqnorm (residuals (disComplete.lmerBC2))
qqline (residuals (disComplete.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> disComplete.lmerBC2



#########################################################################################
#                                                                                       #
#                      Sdisplification of the model                                      #
#########################################################################################

summary(disComplete.lmerBC2)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.538e-01  8.556e-03  3.490e+02  64.730  < 2e-16 ***
#   Environment#sV                   1.980e-02  6.921e-03  6.690e+01   2.862  0.00562 ** 
#   Environments#V                  -3.141e-02  6.110e-03  5.090e+01  -5.141 4.41e-06 ***
#   EnvironmentsV                   -1.688e-02  9.399e-03  5.060e+01  -1.796  0.07852 .  
#   AccentuationConditionunaccented -3.741e-03  1.729e-03  1.054e+03  -2.164  0.03070 *  
#   OrderRescale                    -1.153e-04  7.287e-05  1.021e+03  -1.583  0.11377    
#   logWordFormFreq                 -6.589e-04  7.529e-04  4.990e+01  -0.875  0.38572    
#   BaseInitialStressunstressed     -5.243e-03  5.761e-03  4.930e+01  -0.910  0.36720    
#   LocSpeech                       -5.306e-03  4.756e-04  9.928e+02 -11.157  < 2e-16 ***
#   PrePausepause                    1.287e-02  3.190e-03  1.023e+03   4.035 5.86e-05 ***
#   PostPausepause                   6.185e-04  1.996e-03  1.073e+03   0.310  0.75674   

# let's throw out postpause

disComplete.lmerBC3 <- lmer(bc ~ Environment+  OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + 
                             PrePause + AccentuationCondition + 
                             (1|Item) + (1|Participant), data = disComplete2)
summary(disComplete.lmerBC3)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      5.548e-01  8.000e-03  3.085e+02  69.352  < 2e-16 ***
#   Environment#sV                   1.975e-02  6.936e-03  6.740e+01   2.848  0.00583 ** 
# Environments#V                  -3.141e-02  6.127e-03  5.140e+01  -5.127 4.53e-06 ***
# EnvironmentsV                   -1.677e-02  9.420e-03  5.100e+01  -1.781  0.08094 .  
# OrderRescale                    -1.168e-04  7.267e-05  1.023e+03  -1.607  0.10828    
# logWordFormFreq                 -6.610e-04  7.551e-04  5.040e+01  -0.875  0.38550    
# BaseInitialStressunstressed     -5.241e-03  5.777e-03  4.980e+01  -0.907  0.36869    
# LocSpeech                       -5.364e-03  4.421e-04  1.040e+03 -12.133  < 2e-16 ***
#   PrePausepause                    1.292e-02  3.184e-03  1.025e+03   4.058 5.33e-05 ***
#   AccentuationConditionunaccented -3.864e-03  1.674e-03  1.067e+03  -2.307  0.02122 * 
  
anova(disComplete.lmerBC2,disComplete.lmerBC3)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -4997.8 -4932.9 2511.9  -5023.8                         
# object 14 -4996.0 -4926.0 2512.0  -5024.0 0.1266      1      0.722

# model did not become worse


# let's throw out Freq

disComplete.lmerBC4 <- lmer(bc ~ Environment+  OrderRescale +
                              BaseInitialStress + LocSpeech + 
                              PrePause + AccentuationCondition + 
                              (1|Item) + (1|Participant), data = disComplete2)
summary(disComplete.lmerBC4)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.525e-01  7.567e-03  3.778e+02  73.023  < 2e-16 ***
#   Environment#sV                   1.748e-02  6.412e-03  7.320e+01   2.726  0.00802 ** 
#   Environments#V                  -3.151e-02  6.109e-03  5.230e+01  -5.158 3.91e-06 ***
#   EnvironmentsV                   -1.841e-02  9.207e-03  5.190e+01  -2.000  0.05074 .  
#   OrderRescale                    -1.183e-04  7.265e-05  1.024e+03  -1.629  0.10369    
#   BaseInitialStressunstressed     -4.582e-03  5.712e-03  5.070e+01  -0.802  0.42614    
#   LocSpeech                       -5.355e-03  4.419e-04  1.041e+03 -12.117  < 2e-16 ***
#   PrePausepause                    1.294e-02  3.184e-03  1.025e+03   4.065 5.16e-05 ***
#   AccentuationConditionunaccented -3.870e-03  1.674e-03  1.067e+03  -2.311  0.02100 *  

anova(disComplete.lmerBC3,disComplete.lmerBC4)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -4999.0 -4939.1 2511.5  -5023.0                         
# object 13 -4997.8 -4932.9 2511.9  -5023.8 0.8365      1     0.3604

# nothdisg has changed



# let's throw out stress

disComplete.lmerBC5 <- lmer(bc ~ Environment+  OrderRescale +
                               LocSpeech + 
                              PrePause + AccentuationCondition + 
                              (1|Item) + (1|Participant), data = disComplete2)
summary(disComplete.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      5.526e-01  7.559e-03  3.817e+02  73.097  < 2e-16 ***
#   Environment#sV                   1.745e-02  6.394e-03  7.470e+01   2.730  0.00790 ** 
# Environments#V                  -3.487e-02  4.432e-03  5.480e+01  -7.867 1.47e-10 ***
# EnvironmentsV                   -2.300e-02  7.199e-03  5.380e+01  -3.195  0.00234 ** 
#   OrderRescale                    -1.188e-04  7.265e-05  1.024e+03  -1.636  0.10220    
# LocSpeech                       -5.354e-03  4.418e-04  1.043e+03 -12.118  < 2e-16 ***
#   PrePausepause                    1.291e-02  3.184e-03  1.026e+03   4.055 5.38e-05 ***
#   AccentuationConditionunaccented -3.879e-03  1.674e-03  1.068e+03  -2.317  0.02069 *  

anova(disComplete.lmerBC4,disComplete.lmerBC5)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -5000.3 -4945.4 2511.2  -5022.3                         
# object 12 -4999.0 -4939.1 2511.5  -5023.0 0.6948      1     0.4045

# nothdisg has changed



# let's throw out Order

disComplete.lmerBC6  <- lmer(bc ~ Environment+  
                               LocSpeech + 
                               PrePause + AccentuationCondition + 
                               (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6)


# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.511e-01  7.513e-03  3.701e+02  73.352  < 2e-16 ***
#   Environment#sV                   1.733e-02  6.406e-03  7.460e+01   2.706  0.00844 ** 
#   Environments#V                  -3.492e-02  4.442e-03  5.470e+01  -7.862 1.51e-10 ***
#   EnvironmentsV                   -2.280e-02  7.214e-03  5.380e+01  -3.161  0.00259 ** 
#   LocSpeech                       -5.402e-03  4.412e-04  1.046e+03 -12.244  < 2e-16 ***
#   PrePausepause                    1.297e-02  3.186e-03  1.027e+03   4.073 5.00e-05 ***
#   AccentuationConditionunaccented -3.804e-03  1.675e-03  1.068e+03  -2.271  0.02334 *  
#   ---

anova(disComplete.lmerBC5, disComplete.lmerBC6)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    10 -4999.6 -4949.7 2509.8  -5019.6                        
# object 11 -5000.3 -4945.4 2511.2  -5022.3   2.7      1     0.1003

# not worse

# so that would be the fdisal model without interactions

visreg(disComplete.lmerBC6)

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal podist if view


# 1. Environment and accentuation and stress and pause


# Environment and stress


disComplete.lmerBC6EnvStr <- lmer(bc ~ Environment*BaseInitialStress + LocSpeech + PrePause+
                             (1|Item) + (1|Participant), data = disComplete2)

summary(disComplete.lmerBC6EnvStr)
# can't be tested since there are a few empty slots

table(disComplete2$Environment,disComplete2$BaseInitialStress)
# primary unstressed
# s#sV     232          0
# #sV      183          0
# s#V      156        428
# sV         0         91



# Environment and Acc

disComplete.lmerBC6EnvAcc <- lmer(bc ~ Environment*AccentuationCondition+
                                      BaseInitialStress + LocSpeech + PrePause+
                                      (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                     5.593e-01  7.588e-03  3.812e+02  73.709  < 2e-16 ***
#   Environment#sV                                  2.005e-02  6.684e-03  8.510e+01   3.000 0.003542 ** 
#   Environments#V                                 -3.820e-02  6.345e-03  6.010e+01  -6.020 1.13e-07 ***
#   EnvironmentsV                                  -2.555e-02  9.604e-03  6.070e+01  -2.660 0.009968 ** 
#   AccentuationConditionunaccented                -1.055e-02  3.013e-03  1.026e+03  -3.501 0.000484 ***
#   BaseInitialStressunstressed                    -4.647e-03  5.730e-03  5.070e+01  -0.811 0.421154    
#   LocSpeech                                      -5.527e-03  4.337e-04  1.048e+03 -12.746  < 2e-16 ***
#   PrePausepause                                   9.472e-03  3.153e-03  1.022e+03   3.004 0.002731 ** 
#   Environment#sV:AccentuationConditionunaccented -1.138e-02  4.395e-03  1.007e+03  -2.589 0.009755 ** 
#   Environments#V:AccentuationConditionunaccented  1.387e-02  3.436e-03  1.009e+03   4.037 5.83e-05 ***
#   EnvironmentsV:AccentuationConditionunaccented   1.570e-02  5.453e-03  1.006e+03   2.879 0.004079 ** 

visreg(disComplete.lmerBC6EnvAcc, "Environment", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the one without interactions


anova(disComplete.lmerBC6EnvAcc,disComplete.lmerBC6)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# ..1    10 -4999.6 -4949.7 2509.8  -5019.6                            
# object 14 -5045.6 -4975.7 2536.8  -5073.6 54.01      4  5.238e-11 ***

# it is better!



# Environment and PrePause
disComplete.lmerBC6EnvPause <- lmer(bc ~ Environment*PrePause+BaseInitialStress + LocSpeech + AccentuationCondition+
                                      (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvPause)
#Problem is that there seem to be empty cells:

table(disComplete2$Environment,disComplete2$PrePause)
#         no pause pause
# s#sV        6   226
# #sV       143    40
# s#V        20   564
# sV          0    91



# Accentuation and PrePause
disComplete.lmerBC6AccPause <- lmer(bc ~ Environment+ LocSpeech +
                                      AccentuationCondition*PrePause+(1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6AccPause)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                    5.596e-01  7.751e-03  4.029e+02  72.194  < 2e-16 ***
#   Environment#sV                                 1.688e-02  6.443e-03  7.390e+01   2.620  0.01068 *  
# Environments#V                                -3.491e-02  4.478e-03  5.470e+01  -7.796 1.94e-10 ***
# EnvironmentsV                                 -2.284e-02  7.273e-03  5.380e+01  -3.141  0.00274 ** 
#   LocSpeech                                     -5.409e-03  4.382e-04  1.050e+03 -12.344  < 2e-16 ***
#   AccentuationConditionunaccented               -1.745e-02  3.654e-03  1.022e+03  -4.775 2.06e-06 ***
#   PrePausepause                                  3.432e-03  3.889e-03  1.022e+03   0.882  0.37779    
# AccentuationConditionunaccented:PrePausepause  1.608e-02  3.826e-03  1.011e+03   4.202 2.88e-05 ***


#yes, 


visreg(disComplete.lmerBC6AccPause, "AccentuationCondition", by="PrePause",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the onther with interactions


anova(disComplete.lmerBC6EnvAcc,disComplete.lmerBC6AccPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    11 -5015.1 -4960.2 2518.5  -5037.1                             
# object 14 -5045.6 -4975.7 2536.8  -5073.6 36.516      3  5.825e-08 ***

# the other one is way better


# Stress and PrePause
disComplete.lmerBC6PauseStr <- lmer(bc ~ Environment+ PrePause*BaseInitialStress + AccentuationCondition+LocSpeech + 
                                      (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6PauseStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                5.442e-01  7.642e-03  3.886e+02  71.221  < 2e-16 ***
#   Environment#sV                             2.246e-02  6.496e-03  7.820e+01   3.457 0.000887 ***
# Environments#V                            -3.133e-02  6.084e-03  5.220e+01  -5.149 4.05e-06 ***
# EnvironmentsV                             -1.729e-02  9.170e-03  5.190e+01  -1.885 0.065010 .  
# PrePausepause                              1.963e-02  3.529e-03  1.024e+03   5.563 3.39e-08 ***
#   BaseInitialStressunstressed                2.666e-02  9.351e-03  3.186e+02   2.851 0.004643 ** 
#   AccentuationConditionunaccented           -3.836e-03  1.662e-03  1.067e+03  -2.308 0.021200 *  
#   LocSpeech                                 -5.374e-03  4.381e-04  1.043e+03 -12.268  < 2e-16 ***
#   PrePausepause:BaseInitialStressunstressed -3.240e-02  7.678e-03  1.026e+03  -4.220 2.66e-05 ***


visreg(disComplete.lmerBC6PauseStr, "BaseInitialStress", by="PrePause",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the onther with interactions


anova(disComplete.lmerBC6EnvAcc,disComplete.lmerBC6PauseStr)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    12 -5014.1 -4954.2 2519.1  -5038.1                             
# object 14 -5045.6 -4975.7 2536.8  -5073.6 35.514      2  1.942e-08 ***


# the first one is way better

# Stress and Acc
disComplete.lmerBC6AccStr <- lmer(bc ~ Environment+ AccentuationCondition*BaseInitialStress + PrePause+LocSpeech + 
                                      (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6AccStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  5.589e-01  7.506e-03  3.753e+02  74.463  < 2e-16 ***
#   Environment#sV                                               1.541e-02  6.403e-03  7.260e+01   2.407 0.018619 *  
# Environments#V                                              -3.168e-02  6.111e-03  5.220e+01  -5.184 3.57e-06 ***
# EnvironmentsV                                               -1.815e-02  9.210e-03  5.190e+01  -1.971 0.054083 .  
# AccentuationConditionunaccented                             -1.206e-02  2.082e-03  1.046e+03  -5.790 9.29e-09 ***
#   BaseInitialStressunstressed                                 -1.305e-02  5.860e-03  5.600e+01  -2.227 0.029991 *  
#   PrePausepause                                                1.071e-02  3.144e-03  1.026e+03   3.408 0.000681 ***
#   LocSpeech                                                   -5.532e-03  4.338e-04  1.048e+03 -12.751  < 2e-16 ***
#   AccentuationConditionunaccented:BaseInitialStressunstressed  1.773e-02  2.732e-03  1.013e+03   6.491 1.33e-10 ***

visreg(disComplete.lmerBC6AccStr, "BaseInitialStress", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the other one without interactions


anova(disComplete.lmerBC6EnvAcc,disComplete.lmerBC6EnvPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    13 -5011.5 -4946.6 2518.8  -5037.5                             
# object 14 -5045.6 -4975.7 2536.8  -5073.6 36.067      1  1.906e-09 ***


# the other one is better!

# so, we have 4 possible interactions

#1. Env*Acc
#2. Acc*Pause
#3. Stress*Acc
#4. Stress*Pause

# the best one so far is Env*Acc

# But let's try out combdisations

######
# First three way:

#EnvStressAcc

disComplete.lmerBC6EnvAccStr <- lmer(bc ~ Environment*AccentuationCondition*BaseInitialStress +PrePause+ LocSpeech + 
                                      (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccStr)
#not possible (dropping too many columns)


# EnvStress Pause
disComplete.lmerBC6EnvPauseStr <- lmer(bc ~ Environment*PrePause*BaseInitialStress + AccentuationCondition+ LocSpeech + 
                                         (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvPauseStr)
#no

# Env Acc Pause

disComplete.lmerBC6EnvPauseAcc <- lmer(bc ~ Environment*PrePause*AccentuationCondition+BaseInitialStress + LocSpeech + 
                                           (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvPauseAcc)
# no


# Acc Stress Pause
disComplete.lmerBC6AccPauseStr <- lmer(bc ~ Environment+AccentuationCondition*PrePause*BaseInitialStress + LocSpeech + 
                                           (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6AccPauseStr)

# Linear mixed model fit by REML 
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: bc ~ Environment + AccentuationCondition * PrePause * BaseInitialStress +  
#   LocSpeech + (1 | Item) + (1 | Participant)
# Data: disComplete2
# 
# REML criterion at convergence: -4974.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.0302 -0.6163 -0.0150  0.5989  3.3716 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 0.0001681 0.01297 
# Participant (Intercept) 0.0001526 0.01235 
# Residual                0.0004724 0.02173 
# Number of obs: 1090, groups:  Item, 59; Participant, 22
# 
# Fixed effects:
#   Estimate Std. Error         df
# (Intercept)                                                                5.578e-01  7.809e-03  4.225e+02
# Environment#sV                                                             2.011e-02  6.522e-03  7.740e+01
# Environments#V                                                            -3.148e-02  6.126e-03  5.230e+01
# EnvironmentsV                                                             -1.744e-02  9.234e-03  5.200e+01
# AccentuationConditionunaccented                                           -2.077e-02  3.666e-03  1.017e+03
# PrePausepause                                                              9.725e-03  4.108e-03  1.018e+03
# BaseInitialStressunstressed                                               -1.142e-02  1.465e-02  8.417e+02
# LocSpeech                                                                 -5.504e-03  4.299e-04  1.048e+03
# AccentuationConditionunaccented:PrePausepause                              1.282e-02  4.184e-03  1.008e+03
# AccentuationConditionunaccented:BaseInitialStressunstressed                5.196e-02  1.528e-02  1.014e+03
# PrePausepause:BaseInitialStressunstressed                                 -1.081e-04  1.365e-02  1.018e+03
# AccentuationConditionunaccented:PrePausepause:BaseInitialStressunstressed -3.946e-02  1.555e-02  1.014e+03
# t value Pr(>|t|)    
# (Intercept)                                                                71.426  < 2e-16 ***
#   Environment#sV                                                              3.083 0.002837 ** 
# Environments#V                                                             -5.139 4.18e-06 ***
# EnvironmentsV                                                              -1.888 0.064577 .  
# AccentuationConditionunaccented                                            -5.664 1.93e-08 ***
#   PrePausepause                                                               2.367 0.018120 *  
#   BaseInitialStressunstressed                                                -0.780 0.435870    
# LocSpeech                                                                 -12.802  < 2e-16 ***
#   AccentuationConditionunaccented:PrePausepause                               3.064 0.002240 ** 
#   AccentuationConditionunaccented:BaseInitialStressunstressed                 3.401 0.000697 ***
#   PrePausepause:BaseInitialStressunstressed                                  -0.008 0.993682    
# AccentuationConditionunaccented:PrePausepause:BaseInitialStressunstressed  -2.538 0.011291 *  
###############

anova(disComplete.lmerBC6AccPauseStr,disComplete.lmerBC6EnvAcc)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# ..1    14 -5045.6 -4975.7 2536.8  -5073.6                            
# object 15 -5056.9 -4981.9 2543.4  -5086.9 13.25      1  0.0002725 ***

# up until now, this one is the best, but let's also try a combination of 2-way interactions


# Acc*Pause and Env*Acc
disComplete.lmerBC6EnvAccPauseAcc <- lmer(bc ~ Environment*AccentuationCondition+AccentuationCondition*PrePause + LocSpeech + 
                                         (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccPauseAcc)
# no


# Stress*Pause and Env*Acc
disComplete.lmerBC6EnvAccPauseStress <- lmer(bc ~ Environment*AccentuationCondition+BaseInitialStress*PrePause + LocSpeech + 
                                            (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccPauseStress)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     5.534e-01  7.744e-03  4.072e+02  71.472  < 2e-16 ***
#   Environment#sV                                  2.381e-02  6.741e-03  8.970e+01   3.533 0.000652 ***
# Environments#V                                 -3.775e-02  6.314e-03  6.010e+01  -5.978 1.33e-07 ***
# EnvironmentsV                                  -2.476e-02  9.558e-03  6.080e+01  -2.591 0.011974 *  
#   AccentuationConditionunaccented                -1.039e-02  2.998e-03  1.025e+03  -3.465 0.000552 ***
#   BaseInitialStressunstressed                     2.040e-02  9.281e-03  3.095e+02   2.198 0.028725 *  
#   PrePausepause                                   1.498e-02  3.527e-03  1.020e+03   4.248 2.35e-05 ***
#   LocSpeech                                      -5.495e-03  4.316e-04  1.046e+03 -12.731  < 2e-16 ***
#   Environment#sV:AccentuationConditionunaccented -1.038e-02  4.383e-03  1.006e+03  -2.367 0.018112 *  
# Environments#V:AccentuationConditionunaccented  1.322e-02  3.425e-03  1.009e+03   3.862 0.000120 ***
# EnvironmentsV:AccentuationConditionunaccented   1.545e-02  5.426e-03  1.006e+03   2.847 0.004506 ** 
#   BaseInitialStressunstressed:PrePausepause      -2.591e-02  7.577e-03  1.022e+03  -3.419 0.000653 ***
# 

anova(disComplete.lmerBC6EnvAccPauseStress,disComplete.lmerBC6AccPauseStr)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 15 -5055.4 -4980.4 2542.7  -5085.4                            
# ..1    15 -5056.9 -4981.9 2543.4  -5086.9 1.499      0  < 2.2e-16 ***

# 3 way


# Stress*Acc and Env*Acc
disComplete.lmerBC6EnvAccStressAcc <- lmer(bc ~ Environment*AccentuationCondition+BaseInitialStress*AccentuationCondition+PrePause + LocSpeech + 
                                               (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccStressAcc)

# Fixed effects:
#   Estimate Std. Error         df t value
# (Intercept)                                                  5.597e-01  7.578e-03  3.824e+02  73.865
# Environment#sV                                               1.984e-02  6.685e-03  8.510e+01   2.968
# Environments#V                                              -3.535e-02  6.505e-03  6.620e+01  -5.434
# EnvironmentsV                                               -2.164e-02  9.804e-03  6.580e+01  -2.208
# AccentuationConditionunaccented                             -1.056e-02  3.009e-03  1.025e+03  -3.509
# BaseInitialStressunstressed                                 -8.605e-03  6.067e-03  6.360e+01  -1.418
# PrePausepause                                                9.281e-03  3.151e-03  1.022e+03   2.946
# LocSpeech                                                   -5.546e-03  4.331e-04  1.047e+03 -12.804
# Environment#sV:AccentuationConditionunaccented              -1.131e-02  4.389e-03  1.006e+03  -2.576
# Environments#V:AccentuationConditionunaccented               7.804e-03  4.590e-03  1.008e+03   1.700
# EnvironmentsV:AccentuationConditionunaccented                7.493e-03  6.828e-03  1.006e+03   1.097
# AccentuationConditionunaccented:BaseInitialStressunstressed  8.363e-03  4.200e-03  1.010e+03   1.991
# Pr(>|t|)    
# (Intercept)                                                  < 2e-16 ***
#   Environment#sV                                               0.00390 ** 
# Environments#V                                               8.5e-07 ***
# EnvironmentsV                                                0.03077 *  
#   AccentuationConditionunaccented                              0.00047 ***
#   BaseInitialStressunstressed                                  0.16098    
# PrePausepause                                                0.00330 ** 
#   LocSpeech                                                    < 2e-16 ***
#   Environment#sV:AccentuationConditionunaccented               0.01013 *  
# Environments#V:AccentuationConditionunaccented               0.08939 .  
# EnvironmentsV:AccentuationConditionunaccented                0.27274    
# AccentuationConditionunaccented:BaseInitialStressunstressed  0.04670 *  


anova(disComplete.lmerBC6EnvAccStressAcc,disComplete.lmerBC6AccPauseStr)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -5047.6 -4972.7 2538.8  -5077.6                             
# ..1    15 -5056.9 -4981.9 2543.4  -5086.9 9.2605      0  < 2.2e-16 ***

# 3way


# I want to try out one last thdisg - 3 2-way disteracrions



# Stress*Acc and Env*Acc
disComplete.lmerBC6EnvAccStressAccStressPAuse <- lmer(bc ~ Environment*AccentuationCondition+BaseInitialStress*AccentuationCondition+BaseInitialStress*PrePause + LocSpeech + 
                                             (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccStressAccStressPAuse)

# no!

# what about the 3-way interaction and the 2way if environmenr


disComplete.lmerBC6EnvAccAccPauseStr <- lmer(bc ~ Environment*AccentuationCondition+AccentuationCondition*PrePause*BaseInitialStress + LocSpeech + 
                                         (1|Item) + (1|Participant), data = disComplete2)


summary(disComplete.lmerBC6EnvAccAccPauseStr)

#no

# the best model is the one with the three interactiosn, however,
# the model with the interactions bteween Stress and Acc and Env and Acc makes much
# more sense from a theoretica pov, thereforw, that will be the final model

summary(disComplete.lmerBC6EnvAccStressAcc)
# Fixed effects:
#   Estimate Std. Error         df t value
# (Intercept)                                                  5.597e-01  7.578e-03  3.824e+02  73.865
# Environment#sV                                               1.984e-02  6.685e-03  8.510e+01   2.968
# Environments#V                                              -3.535e-02  6.505e-03  6.620e+01  -5.434
# EnvironmentsV                                               -2.164e-02  9.804e-03  6.580e+01  -2.208
# AccentuationConditionunaccented                             -1.056e-02  3.009e-03  1.025e+03  -3.509
# BaseInitialStressunstressed                                 -8.605e-03  6.067e-03  6.360e+01  -1.418
# PrePausepause                                                9.281e-03  3.151e-03  1.022e+03   2.946
# LocSpeech                                                   -5.546e-03  4.331e-04  1.047e+03 -12.804
# Environment#sV:AccentuationConditionunaccented              -1.131e-02  4.389e-03  1.006e+03  -2.576
# Environments#V:AccentuationConditionunaccented               7.804e-03  4.590e-03  1.008e+03   1.700
# EnvironmentsV:AccentuationConditionunaccented                7.493e-03  6.828e-03  1.006e+03   1.097
# AccentuationConditionunaccented:BaseInitialStressunstressed  8.363e-03  4.200e-03  1.010e+03   1.991
# Pr(>|t|)    
# (Intercept)                                                  < 2e-16 ***
#   Environment#sV                                               0.00390 ** 
# Environments#V                                               8.5e-07 ***
# EnvironmentsV                                                0.03077 *  
#   AccentuationConditionunaccented                              0.00047 ***
#   BaseInitialStressunstressed                                  0.16098    
# PrePausepause                                                0.00330 ** 
#   LocSpeech                                                    < 2e-16 ***
#   Environment#sV:AccentuationConditionunaccented               0.01013 *  
# Environments#V:AccentuationConditionunaccented               0.08939 .  
# EnvironmentsV:AccentuationConditionunaccented                0.27274    
# AccentuationConditionunaccented:BaseInitialStressunstressed  0.04670 *  

######################################################
# Summary: 
# There are a lot of interaction s significant, also the combdisations...but
# the best model is the one with 2 3 way interactions

visreg(disComplete.lmerBC6EnvAccStressAcc,"Environment", by="AccentuationCondition", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,
)


visreg(disComplete.lmerBC6EnvAccStressAcc,"BaseInitialStress", by="AccentuationCondition", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,
   )

# I need to rename some variabels for the plot...


disComplete2<-rename(disComplete2,AccentuationAnnotator=Accentuation)

disComplete2<-rename(disComplete2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(disComplete2$BaseInitialStress)
#[1] "primary"    "unstressed"



levels(disComplete2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(disComplete2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# need to rename the pause levels

levels(disComplete2$PrePause)
#[1] [1] "No Pause" "Pause"   



levels(disComplete2$PrePause)<-c("no pause"   , "pause")

levels(disComplete2$PrePause)
#[1] "no pause" "pause"   

#######################################################
# Since we have these problems with stress, I would like to try something out

# Let's recode Environment by including the stresss pattern

disComplete2$Environment2<-as.factor(paste(disComplete2$Environment, disComplete2$BaseInitialStress, sep = "-"))


levels(disComplete2$Environment2)
#[1] "#sV-stressed"   "s#sV-stressed"  "s#V-stressed"   "s#V-unstressed" "sV-unstressed" 


# let's change the reference level


disComplete2$Environment2 <- relevel (disComplete2$Environment2, ref= "sV-stressed")
disComplete2$Environment2 <- relevel (disComplete2$Environment2, ref= "#sV-stressed")
disComplete2$Environment2 <- relevel (disComplete2$Environment2, ref= "s#V-stressed")
disComplete2$Environment2 <- relevel (disComplete2$Environment2, ref= "s#V-unstressed")
disComplete2$Environment2 <- relevel (disComplete2$Environment2, ref= "s#sV-stressed")



# let's do a model with this

# but first we need ro 

dis_complete_model_Env2.lmer<-lmer(bc ~  Environment2+Accentuation+PrePause
                                      + LocSpeech                                             
                                    +(1|Participant)+ (1|Item) , data = disComplete2)                                 

summary(dis_complete_model_Env2.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 5.510e-01  7.519e-03  3.663e+02  73.282  < 2e-16 ***
#   Environment2s#V-unstressed -3.616e-02  4.710e-03  5.340e+01  -7.677 3.52e-10 ***
# Environment2s#V-stressed   -3.150e-02  6.121e-03  5.220e+01  -5.147 4.08e-06 ***
# Environment2#sV-stressed    1.736e-02  6.423e-03  7.310e+01   2.702  0.00856 ** 
# Environment2sV-unstressed  -2.280e-02  7.236e-03  5.270e+01  -3.150  0.00269 ** 
#   Accentuationunaccented     -3.795e-03  1.675e-03  1.068e+03  -2.266  0.02366 *  
#   PrePausepause               1.301e-02  3.186e-03  1.026e+03   4.083 4.79e-05 ***
#   LocSpeech                  -5.403e-03  4.413e-04  1.044e+03 -12.242  < 2e-16 ***

# let's check for interactions


dis_complete_model_Env2Int1.lmer<-lmer(bc ~  Environment2*Accentuation+PrePause
                                   + LocSpeech                                             
                                   +(1|Participant)+ (1|Item) , data = disComplete2)                                 

summary(dis_complete_model_Env2Int1.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        5.597e-01  7.578e-03  3.824e+02  73.865  < 2e-16 ***
#   Environment2s#V-unstressed                        -4.395e-02  5.027e-03  6.880e+01  -8.743 8.96e-13 ***
#   Environment2s#V-stressed                          -3.535e-02  6.505e-03  6.620e+01  -5.434 8.50e-07 ***
#   Environment2#sV-stressed                           1.984e-02  6.685e-03  8.510e+01   2.968 0.003895 ** 
#   Environment2sV-unstressed                         -3.025e-02  7.710e-03  6.750e+01  -3.923 0.000207 ***
#   Accentuationunaccented                            -1.056e-02  3.009e-03  1.025e+03  -3.509 0.000470 ***
#   PrePausepause                                      9.281e-03  3.151e-03  1.021e+03   2.946 0.003295 ** 
#   LocSpeech                                         -5.546e-03  4.331e-04  1.047e+03 -12.804  < 2e-16 ***
#   Environment2s#V-unstressed:Accentuationunaccented  1.617e-02  3.620e-03  1.009e+03   4.466 8.88e-06 ***
#   Environment2s#V-stressed:Accentuationunaccented    7.804e-03  4.590e-03  1.008e+03   1.700 0.089395 .  
#   Environment2#sV-stressed:Accentuationunaccented   -1.131e-02  4.389e-03  1.006e+03  -2.576 0.010127 *  
#   Environment2sV-unstressed:Accentuationunaccented   1.586e-02  5.446e-03  1.006e+03   2.911 0.003677 ** 



dis_complete_model_Env2Int2.lmer<-lmer(bc ~  Accentuation*PrePause+Environment2
                                       + LocSpeech                                             
                                       +(1|Participant)+ (1|Item) , data = disComplete2)                                 

summary(dis_complete_model_Env2Int2.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                           5.595e-01  7.758e-03  3.987e+02  72.125  < 2e-16 ***
#   Accentuationunaccented               -1.744e-02  3.654e-03  1.022e+03  -4.772 2.08e-06 ***
#   PrePausepause                         3.465e-03  3.890e-03  1.021e+03   0.891  0.37323    
#   Environment2s#V-unstressed           -3.614e-02  4.749e-03  5.340e+01  -7.609 4.54e-10 ***
#   Environment2s#V-stressed             -3.151e-02  6.173e-03  5.220e+01  -5.105 4.72e-06 ***
#   Environment2#sV-stressed              1.690e-02  6.462e-03  7.240e+01   2.616  0.01083 *  
#   Environment2sV-unstressed            -2.284e-02  7.297e-03  5.270e+01  -3.130  0.00285 ** 
#   LocSpeech                            -5.410e-03  4.383e-04  1.048e+03 -12.342  < 2e-16 ***
#   Accentuationunaccented:PrePausepause  1.607e-02  3.826e-03  1.011e+03   4.202 2.88e-05 ***


dis_complete_model_Env2Int3.lmer<-lmer(bc ~  PrePause*Accentuation+Environment2*Accentuation
                                       + LocSpeech                                             
                                       +(1|Participant)+ (1|Item) , data = disComplete2)                                 

summary(dis_complete_model_Env2Int3.lmer)

#no, only one is significant
# so we need to find out which model is better

anova(dis_complete_model_Env2Int2.lmer,dis_complete_model_Env2Int1.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -5013.8 -4953.9 2518.9  -5037.8                             
# ..1    15 -5047.6 -4972.7 2538.8  -5077.6 39.813      3  1.167e-08 ***
#   ---


# the first, so that will be our final model
#let's look at a visreg


ylim=c(20,180)

visreg(dis_complete_model_Env2Int1.lmer, "Environment2",by="Accentuation",
       ylab="duration dis milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim, band=F,
       overlay=TRUE ,line.par = list(col = c(('cornflowerblue'),('darkblue'))),cex=0.8)


# I want to rename the Environment levels

levels(disComplete2$Environment2)
#[1] "s#sV-stressed"  "s#V-unstressed" "s#V-stressed"   "#sV-stressed"   "sV-unstressed" 

levels(disComplete2$Environment2)<-c( "s#sV-str."  ,"s#V-unstr." ,"s#V-str." ,  "#sV-str."   ,"sV-unstr." )

levels(disComplete2$Environment2)
# [1] "s#sV-str."  "s#V-unstr." "s#V-str."   "#sV-str."   "sV-unstr." 

final_dis_complete_model.lmer<-lmer(bc ~  Environment2*Accentuation+PrePause
                                    + LocSpeech                                             
                                    +(1|Participant)+ (1|Item) , data = disComplete2)


summary(final_dis_complete_model.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                    5.597e-01  7.578e-03  3.824e+02  73.865  < 2e-16 ***
#   Environment2s#V-unstr.                        -4.395e-02  5.027e-03  6.880e+01  -8.743 8.96e-13 ***
# Environment2s#V-str.                          -3.535e-02  6.505e-03  6.620e+01  -5.434 8.50e-07 ***
# Environment2#sV-str.                           1.984e-02  6.685e-03  8.510e+01   2.968 0.003895 ** 
# Environment2sV-unstr.                         -3.025e-02  7.710e-03  6.750e+01  -3.923 0.000207 ***
#   Accentuationunaccented                        -1.056e-02  3.009e-03  1.025e+03  -3.509 0.000470 ***
#   PrePausepause                                  9.281e-03  3.151e-03  1.021e+03   2.946 0.003295 ** 
#   LocSpeech                                     -5.546e-03  4.331e-04  1.047e+03 -12.804  < 2e-16 ***
#   Environment2s#V-unstr.:Accentuationunaccented  1.617e-02  3.620e-03  1.009e+03   4.466 8.88e-06 ***
# Environment2s#V-str.:Accentuationunaccented    7.804e-03  4.590e-03  1.008e+03   1.700 0.089395 .  
# Environment2#sV-str.:Accentuationunaccented   -1.131e-02  4.389e-03  1.006e+03  -2.576 0.010127 *  
# Environment2sV-unstr.:Accentuationunaccented   1.586e-02  5.446e-03  1.006e+03   2.911 0.003677 ** 
  
  #############
# Let's get the  model for the dissertation


table_final_models<-as.data.frame(coef(summary(final_dis_complete_model.lmer)))

xtable(table_final_models,digits = 3)



#change directory for plots

# need to create the variable ConsonantDurMS

disComplete$ConsonantDurMS<-disComplete$ConsonantDur*1000

# disComplete$Environment <- relevel (disComplete$Environment, ref= "sV")
# disComplete$Environment <- relevel (disComplete$Environment, ref= "#sV")
# disComplete$Environment <- relevel (disComplete$Environment, ref= "s#V")
# disComplete$Environment <- relevel (disComplete$Environment, ref= "s#sV")
# 

# Let's recode Environment by including the stresss pattern

disComplete$Environment2<-as.factor(paste(disComplete$Environment, disComplete$BaseInitialStress, sep = "-"))


levels(disComplete$Environment2)
#[1] "#sV-primary"    "s#sV-primary"   "s#V-primary"    "s#V-unstressed" "sV-unstressed" 

levels(disComplete$Environment2)<-c("#sV-stressed"  , "s#sV-stressed",  "s#V-stressed"   ,"s#V-unstressed" ,"sV-unstressed")


# let's change the reference level


disComplete$Environment2 <- relevel (disComplete$Environment2, ref= "sV-unstressed")
disComplete$Environment2 <- relevel (disComplete$Environment2, ref= "#sV-stressed")
disComplete$Environment2 <- relevel (disComplete$Environment2, ref= "s#V-stressed")
disComplete$Environment2 <- relevel (disComplete$Environment2, ref= "s#V-unstressed")
disComplete$Environment2 <- relevel (disComplete$Environment2, ref= "s#sV-stressed")

levels(disComplete$Environment2)<-c( "s#sV-\nstr."  ,"s#V-\nunstr." ,"s#V-\nstr." ,  "#sV-\nstr."   ,"sV-\nunstr." )

levels(disComplete$Environment2)
# [1] "s#sV-str."  "s#V-unstr." "s#V-str."   "#sV-str."   "sV-unstr." 


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/Images/Experiment")

png("boxdis.png", units="cm", height=10, width=8.5, res=300, pointsize=8)
cols = list(col=c("dodgerblue3"),pch=c(1))
bwplot (ConsonantDurMS ~ Environment2, disComplete, ylab="duration dis milliseconds", 
        main="dis-", ylim=c(0,320), cex.axis=0.5,
        par.settings = list(
          plot.symbol=cols,
          box.rectangle = cols,
          #box.dot = cols,
          box.umbrella=cols 
        ))

dev.off()


##############################
# We should  plot the madis effect (not covariates)
###############################
# Plot madis effect



makeTransparent = function(..., alpha=0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
  
}

png("disModelCompleteinterEnvAcc.png", units="cm", height=12, width=20, res=300, pointsize=15)

ylim=c(20,180)

visreg(final_dis_complete_model.lmer, "Environment2",by="Accentuation",
       ylab="duration dis milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim, band=F,
       overlay=TRUE ,line.par = list(col = c(('cornflowerblue'),('darkblue'))),cex=0.8)



dev.off()




library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMdis packe to compare the disportance of the
# different factors.

# let's check a MuMdis with interactions


options(na.action = "na.fail") 


disComplete.lm2<- lm(ConsonantDur ~ Environment2*Accentuation + 
                      OrderRescale + logWordFormFreq + LocSpeech + 
                       PrePause*Accentuation + PostPause, 
                    data = disComplete2)

model_ranking2 <- dredge(disComplete.lm2)

model_average_2<-model.avg(model_ranking2)


summary(model_average_2)


# Relative variable importance: 
#                     Environment2 LocSpeech Accentuation Accentuation:Environment2 PrePause OrderRescale logWordFormFreq
# Importance:          1.00         1.00      1.00         1.00                      0.93     0.65         0.53           
# N containing models:  128          104       144           48                       128      104          104           

                  # Accentuation:PrePause PostPause
# Importance:          0.52                  0.49     
# N containing models:   48                   104         