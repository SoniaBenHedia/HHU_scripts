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


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses un/")


###########################################################################
# I will start with the complex dataset, and thus will need the complex dataset-
# In the following I will first take a look at the pertinent varoables
# and then fit a model
############################################################################



un <- read.csv("unComplete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(un)

# 'data.frame':	2615 obs. of  84 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  2806 2807 2808 2809 2810 2811 2812 2813 2814 2815 ...
# $ Item                       : Factor w/ 89 levels "nailed","named",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 51 levels "Experiment_1_participant_10",..: 28 15 1 8 6 29 16 2 9 7 ...
# $ ID                         : int  5335 2309 72 1316 953 5429 2499 221 1610 1275 ...
# $ Filename                   : Factor w/ 2615 levels "participant_1_A_10.TextGrid",..: 2380 1162 92 699 581 2487 1232 229 801 685 ...
# $ DeletionMorph              : Factor w/ 3 levels "L","N","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 2 2 1 2 2 2 1 2 1 1 ...
# $ Annotator                  : Factor w/ 6 levels "Lara","Mandy",..: 2 5 3 5 2 6 2 1 6 1 ...
# $ Order                      : int  174 47 121 17 38 6 51 66 202 268 ...
# $ WordDur                    : num  0.336 0.402 0.553 0.47 0.327 ...
# $ SyllNum                    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ SegNum                     : int  4 4 4 4 4 4 4 4 4 4 ...
# $ ConsonantDur               : num  0.0747 0.0841 0.1156 0.0991 0.0667 ...
# $ PrecSeg                    : Factor w/ 13 levels "?","@","{","2",..: NA NA NA NA NA NA NA NA NA NA ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ FollSeg                    : Factor w/ 56 levels "?","@","@i","@O",..: 7 24 24 17 7 24 24 7 24 26 ...
# $ FollSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur                 : num  0.155 0.164 0.182 0.195 0.121 ...
# $ PrePauseDur                : num  0 0 0 0 0 ...
# $ PostPauseDur               : num  0 0 0 0 0 ...
# $ SentenceDur                : num  2.74 3 1.48 3.45 2.64 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ GlottalStopDur             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ LocSpeech                  : num  11.9 9.94 7.24 8.52 12.23 ...
# $ AffixDur                   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ BaseDuration               : num  0.336 0.402 0.553 0.47 0.327 ...
# $ FirstSyllDur               : num  0.336 0.402 0.553 0.47 0.327 ...
# $ WordDurWithoutGlottalStop  : num  0.336 0.402 0.553 0.47 0.327 ...
# $ AffixDurWithoutGlottalStop : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Environment                : Factor w/ 4 levels "#nV","n#C","n#nV",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Affix                      : Factor w/ 1 level "un": NA NA NA NA NA NA NA NA NA NA ...
# $ WordFormFrequencyBNC       : int  230 230 230 230 230 230 230 230 230 230 ...
# $ WordFormFrequencyAllCOCA   : int  1741 1741 1741 1741 1741 1741 1741 1741 1741 1741 ...
# $ WordFormFrequencySpokenCOCA: int  251 251 251 251 251 251 251 251 251 251 ...
# $ Base                       : Factor w/ 69 levels "able","acquainted",..: NA NA NA NA NA NA NA NA NA NA ...
# $ WordLemmaFrequencyBNC      : int  13 13 13 13 13 13 13 13 13 13 ...
# $ BaseLemmaFrequencyBNC      : int  15741 15741 15741 15741 15741 15741 15741 15741 15741 15741 ...
# $ SyllPhon                   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ AffixStress                : Factor w/ 3 levels "debatable","secondary",..: NA NA NA NA NA NA NA NA NA NA ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": NA NA NA NA NA NA NA NA NA NA ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": NA NA NA NA NA NA NA NA NA NA ...
# $ Rating                     : int  NA NA NA NA NA NA NA NA NA NA ...
# $ TimeRating                 : num  NA NA NA NA NA NA NA NA NA NA ...
# $ TotalTime                  : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Age                        : int  24 55 19 20 57 19 47 19 21 23 ...
# $ Sex                        : Factor w/ 6 levels "female","Female",..: 1 4 5 2 1 1 3 1 1 5 ...
# $ L1                         : Factor w/ 10 levels "british","British",..: 5 5 5 5 5 5 10 5 1 9 ...
# $ Bilingual                  : Factor w/ 6 levels "I only know British English",..: 4 NA 4 4 4 3 3 1 3 4 ...
# $ Grow_Up_Region             : Factor w/ 42 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 37 31 15 1 34 15 40 13 22 11 ...
# $ Languages                  : Factor w/ 30 levels "Basic French",..: 25 2 1 1 7 12 9 15 13 25 ...
# $ Latin                      : Factor w/ 19 levels "2 years secondary school",..: 7 1 18 7 7 6 6 8 12 7 ...
# $ Profession_Studies         : Factor w/ 50 levels "2nd Year Meida Studies",..: 14 NA 18 2 35 6 43 28 36 50 ...
# $ University                 : Factor w/ 18 levels "Aberdeen University",..: 7 NA 7 12 11 7 17 7 2 7 ...
# $ Knowledge_English_Ling     : Factor w/ 22 levels "2 years","Currently in my 2nd year of the course at university",..: 3 5 9 6 6 18 5 8 5 6 ...
# $ Phonetics                  : Factor w/ 16 levels "A couple of lectures",..: 3 7 11 8 8 14 7 2 7 8 ...
# $ Phonology                  : Factor w/ 14 levels "A couple of lectures",..: 2 6 9 7 7 13 6 4 6 7 ...
# $ Morphology                 : Factor w/ 12 levels "currently studying",..: 1 4 8 5 5 11 4 7 4 5 ...
# $ Semantics                  : Factor w/ 12 levels "currently studying",..: 1 4 8 5 5 11 4 7 4 5 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 2 1 2 2 2 1 2 1 1 ...
# $ Experiment                 : Factor w/ 2 levels "Experiment_1",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  5.44 5.44 5.44 5.44 5.44 ...
# $ logBaseLemmaFreq           : num  9.66 9.66 9.66 9.66 9.66 ...
# $ logWordLemmaFreq           : num  2.56 2.56 2.56 2.56 2.56 ...
# $ RelFreq                    : num  0.000826 0.000826 0.000826 0.000826 0.000826 ...
# $ logRelFreq                 : num  -7.1 -7.1 -7.1 -7.1 -7.1 ...
# $ Root                       : Factor w/ 43 levels "knit","known",..: NA NA NA NA NA NA NA NA NA NA ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : num  NA NA NA NA NA NA NA NA NA NA ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": NA NA NA NA NA NA NA NA NA NA ...
# $ Consonant                  : Factor w/ 5 levels "b","l","n","O",..: NA NA NA NA NA NA NA NA NA NA ...
# $ Orthography                : Factor w/ 4 levels "kn","n","nn",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ median                     : int  NA NA NA NA NA NA NA NA NA NA ...
# $ TypeOfBase                 : Factor w/ 1 level "word": NA NA NA NA NA NA NA NA NA NA ...
# $ ConsonantDurMS             : num  74.7 84.1 115.6 99.1 66.7 ...
# $ PrePause                   : Factor w/ 2 levels "No Pause","Pause": 1 1 1 1 1 2 2 1 1 1 ...
# $ PostPause                  : Factor w/ 2 levels "No Pause","Pause": 1 1 1 1 1 2 2 1 1 2 ...
# $ GlobalSpeechRate           : num  3.28 3 2.71 2.61 3.41 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ StressPattern              : Factor w/ 7 levels "debatable-primary",..: 3 3 3 3 3 3 3 3 3 3 ...

un$X.1<-NULL
un$X<-NULL


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

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = un)
cor(un$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.1224273


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = un)
cor(un$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.7049382

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = un)
cor(un$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#[1] 0.6928415

# so aroun1d 69 percent of the variability can be explained by this! That's a lot



##              Do an initial model:
un$OrderRescale<-un$Order*0.1

un.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + (1|Item) + (1|Participant), data = un)

summary(un.lmer1)    

# Linear mixed model fit by REML 
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + (1 | Item) + (1 | Participant)
# Data: un
# 
# REML criterion at convergence: -11628.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.9022 -0.5792 -0.0452  0.5041  6.9087 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 5.908e-05 0.007687
# Participant (Intercept) 9.811e-05 0.009905
# Residual                5.974e-04 0.024441
# Number of obs: 2615, groups:  Item, 89; Participant, 51
# 
# Fixed effects:
#                                       Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      1.672e-01  5.415e-03  3.710e+02  30.884  < 2e-16 ***
#   Environmentn#C                  -9.254e-03  3.703e-03  1.191e+02  -2.499   0.0138 *  
#   Environmentn#nV                  3.583e-02  3.321e-03  8.180e+01  10.789  < 2e-16 ***
#   Environmentn#V                  -4.774e-02  3.282e-03  8.240e+01 -14.547  < 2e-16 ***
#   AccentuationConditionunaccented -6.259e-03  1.539e-03  1.732e+03  -4.066 4.99e-05 ***
#   OrderRescale                     6.374e-06  5.291e-05  2.547e+03   0.120   0.9041    
#   logWordFormFreq                 -7.653e-04  4.270e-04  7.840e+01  -1.792   0.0770 .  
#   BaseInitialStressunstressed     -5.589e-03  3.232e-03  8.160e+01  -1.729   0.0876 .  
#   LocSpeech                       -5.608e-03  3.160e-04  1.723e+03 -17.743  < 2e-16 ***
#   GlobalSpeechRate                 6.318e-04  1.375e-03  6.497e+02   0.460   0.6460    
#   PrePausePause                    1.594e-03  1.217e-03  2.561e+03   1.310   0.1903    
#   PostPausePause                   1.703e-03  1.400e-03  2.587e+03   1.216   0.2240    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envr#C Envrnmntn#nV Envirnmntn#V AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsPs
# Envrnmntn#C  -0.464                                                                                  
# Envrnmntn#nV -0.540  0.666                                                                           
# Envirnmntn#V -0.325  0.497  0.563                                                                    
# AccnttnCndt   0.204 -0.015 -0.033        0.042                                                       
# OrderRescal  -0.168 -0.015 -0.006       -0.014        0.036                                          
# lgWrdFrmFrq  -0.492  0.484  0.526        0.354        0.042  0.001                                   
# BsIntlStrss  -0.097  0.170  0.125       -0.323       -0.012 -0.007  0.232                            
# LocSpeech    -0.530 -0.084  0.045       -0.060       -0.199  0.010 -0.001 -0.138                     
# GloblSpchRt  -0.320  0.062  0.010       -0.059       -0.542 -0.021 -0.048  0.084 -0.172              
# PrePausePas  -0.165 -0.066 -0.089       -0.122        0.063  0.060  0.010 -0.022  0.050  0.109       
# PostPausePs  -0.329  0.002  0.029        0.006        0.297  0.034  0.010 -0.033  0.280  0.038 -0.141
# Let's have a look the the R2


cor(un$ConsonantDur, fitted(un.lmer1))^2
#[1]  0.7635669


#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own




# 1.  Loc Speech  and/or Global Speech


cor.test(un$LocSpeech,un$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  un$LocSpeech and un$GlobalSpeechRate
# t = 23.694, df = 2613, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3884711 0.4515913
# sample estimates:
# cor 
# 0.4205399 

un.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = un)

summary(un.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.754e-01  4.614e-03  2.598e+02  38.008   <2e-16 ***
#   LocSpeech        -6.965e-03  3.041e-04  2.358e+03 -22.904   <2e-16 ***
#   GlobalSpeechRate -2.087e-03  1.113e-03  1.157e+03  -1.875    0.061 .

cor(un$ConsonantDur, fitted(un.lmerSpeechRates))^2
#[1]  0.7632991



un.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = un)

summary(un.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.748e-01  4.586e-03  2.575e+02   38.12   <2e-16 ***
#   LocSpeech   -7.287e-03  2.540e-04  2.515e+03  -28.69   <2e-16 ***

cor(un$ConsonantDur, fitted(un.lmerLocSpeech))^2
#[1] 0.7626381


un.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = un)

summary(un.lmerGlobalSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.283e-01  4.922e-03  1.747e+02   26.06   <2e-16 ***
#   GlobalSpeechRate -1.719e-02  1.052e-03  2.136e+03  -16.34   <2e-16 ***


cor(un$ConsonantDur, fitted(un.lmerGlobalSpeech))^2
#[1] 0.7237957


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, Locpeech has a sign effect, GlobSpeech is
#   only marginally sign
# - The effect size of LocSpeech increases when it is the only variable in the model
# - The effect size of GlobalSpeech decreases when it is the only variable in the model (but becomes sign)
# - The effect direction never changes (no supression)
# - LocSpeech is the better predictir (higher R squared)
#################################################


##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################
# Now we have dealt with all collinearity problems: 
# - We will keep both Speech Rate variables but must be aware of the fact that their effect
#   size cannot be interpreted!
###############################################################################################



# Let's refit our model incorportaing the "right variables"

un.lmer3 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause +(1|Item) + (1|Participant), data = un)

summary(un.lmer3)
# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      1.672e-01  5.415e-03  3.710e+02  30.884  < 2e-16 ***
#   Environmentn#C                  -9.254e-03  3.703e-03  1.191e+02  -2.499   0.0138 *  
#   Environmentn#nV                  3.583e-02  3.321e-03  8.180e+01  10.789  < 2e-16 ***
#   Environmentn#V                  -4.774e-02  3.282e-03  8.240e+01 -14.547  < 2e-16 ***
#   AccentuationConditionunaccented -6.259e-03  1.539e-03  1.732e+03  -4.066 4.99e-05 ***
#   OrderRescale                     6.374e-06  5.291e-05  2.547e+03   0.120   0.9041    
#   logWordFormFreq                 -7.653e-04  4.270e-04  7.840e+01  -1.792   0.0770 .  
#   BaseInitialStressunstressed     -5.589e-03  3.232e-03  8.160e+01  -1.729   0.0876 .  
#   LocSpeech                       -5.608e-03  3.160e-04  1.723e+03 -17.743  < 2e-16 ***
#   GlobalSpeechRate                 6.318e-04  1.375e-03  6.497e+02   0.460   0.6460    
#   PrePausePause                    1.594e-03  1.217e-03  2.561e+03   1.310   0.1903    
#   PostPausePause                   1.703e-03  1.400e-03  2.587e+03   1.216   0.2240   


###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (un.lmer3))
qqline (residuals (un.lmer3))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Use log(ConsonantDur):
un.lmer4 <- lmer(log(ConsonantDur) ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause +(1|Item) + (1|Participant), data = un)

summary(un.lmer4)

# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -1.692e+00  5.218e-02  3.294e+02 -32.419  < 2e-16 ***
# Environmentn#C                  -5.216e-02  3.676e-02  1.103e+02  -1.419  0.15877    
# Environmentn#nV                  3.332e-01  3.338e-02  7.950e+01   9.982 1.11e-15 ***
# Environmentn#V                  -6.003e-01  3.298e-02  7.990e+01 -18.199  < 2e-16 ***
# AccentuationConditionunaccented -4.318e-02  1.435e-02  1.705e+03  -3.009  0.00266 ** 
# OrderRescale                    -3.074e-04  4.942e-04  2.541e+03  -0.622  0.53396    
# logWordFormFreq                 -5.685e-03  4.299e-03  7.690e+01  -1.322  0.18993    
# BaseInitialStressunstressed     -7.113e-02  3.249e-02  7.930e+01  -2.189  0.03155 *  
# LocSpeech                       -5.689e-02  2.980e-03  1.886e+03 -19.090  < 2e-16 ***
# GlobalSpeechRate                 3.113e-03  1.279e-02  6.312e+02   0.243  0.80784    
# PrePausePause                    2.757e-03  1.137e-02  2.553e+03   0.243  0.80832    
# PostPausePause                  -1.229e-02  1.308e-02  2.580e+03  -0.939  0.34766    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1   

# okay, let's check the assumptions again

qqnorm (residuals (un.lmer4))
qqline (residuals (un.lmer4))

# not much better


# Maybe a box-cox transformation will lead to a better
# distribuition of res. Let's try


un.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause  , data = un)

summary(un.lm)


# Call:
#   lm(formula = ConsonantDur ~ Environment + AccentuationCondition + 
#        OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
#        GlobalSpeechRate + PrePause + PostPause, data = un)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.098650 -0.017290 -0.002175  0.014699  0.151131 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      1.755e-01  3.916e-03  44.820  < 2e-16 ***
#   Environmentn#C                  -1.458e-02  2.299e-03  -6.340  2.7e-10 ***
#   Environmentn#nV                  3.299e-02  1.835e-03  17.975  < 2e-16 ***
#   Environmentn#V                  -4.677e-02  1.930e-03 -24.237  < 2e-16 ***
#   AccentuationConditionunaccented -5.241e-03  1.407e-03  -3.724 0.000200 ***
#   OrderRescale                    -1.910e-05  5.705e-05  -0.335 0.737842    
#   logWordFormFreq                 -9.068e-04  2.382e-04  -3.807 0.000144 ***
#   BaseInitialStressunstressed     -7.051e-03  1.874e-03  -3.763 0.000171 ***
#   LocSpeech                       -5.786e-03  2.683e-04 -21.563  < 2e-16 ***
#   GlobalSpeechRate                -8.619e-04  1.005e-03  -0.857 0.391329    
#   PrePausePause                    7.561e-04  1.219e-03   0.620 0.535253    
#   PostPausePause                   1.356e-03  1.417e-03   0.957 0.338608    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02726 on 2603 degrees of freedom
# Multiple R-squared:  0.6934,	Adjusted R-squared:  0.6921 
# F-statistic: 535.1 on 11 and 2603 DF,  p-value: < 2.2e-16

bc<-boxcox(un.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.1818182

un$bc <- un$ConsonantDur^lambda

un.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause+ (1|Item) + (1|Participant), data = un)

summary(un.lmerBC)

# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.312e-01  6.093e-03  3.314e+02 119.991  < 2e-16 ***
#   Environmentn#C                  -7.236e-03  4.290e-03  1.109e+02  -1.687 0.094461 .  
#   Environmentn#nV                  3.999e-02  3.895e-03  7.990e+01  10.268 4.44e-16 ***
#   Environmentn#V                  -6.822e-02  3.848e-03  8.020e+01 -17.729  < 2e-16 ***
#   AccentuationConditionunaccented -5.561e-03  1.674e-03  1.729e+03  -3.321 0.000914 ***
#   OrderRescale                    -2.847e-05  5.755e-05  2.541e+03  -0.495 0.620797    
#   logWordFormFreq                 -7.185e-04  5.016e-04  7.730e+01  -1.432 0.156063    
#   BaseInitialStressunstressed     -8.025e-03  3.791e-03  7.970e+01  -2.117 0.037406 *  
#   LocSpeech                       -6.676e-03  3.473e-04  1.895e+03 -19.223  < 2e-16 ***
#   GlobalSpeechRate                 4.611e-04  1.497e-03  6.499e+02   0.308 0.758113    
#   PrePausePause                    6.559e-04  1.324e-03  2.554e+03   0.495 0.620327    
#   PostPausePause                  -7.867e-04  1.524e-03  2.581e+03  -0.516 0.605741    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#let's check the assumptions

qqnorm (residuals (un.lmerBC))
qqline (residuals (un.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(un.lmerBC, un, trim = 2.5)
# n.removed = 66 
# percent.removed = 2.523901 

un2<-outliers$data

dim(un2)
#[1] 2549   85


dim(un)
#[1] 2615   84



# okay it seemes to have worked

un.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC2)

# Fixed effects:
#                                    Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.240e-01  5.511e-03  3.463e+02 131.369  < 2e-16 ***
#   Environmentn#C                  -8.529e-03  3.806e-03  1.123e+02  -2.241   0.0270 *  
#   Environmentn#nV                  3.936e-02  3.453e-03  8.060e+01  11.399  < 2e-16 ***
#   Environmentn#V                  -7.022e-02  3.409e-03  8.070e+01 -20.595  < 2e-16 ***
#   AccentuationConditionunaccented -7.540e-03  1.500e-03  1.918e+03  -5.026 5.48e-07 ***
#   OrderRescale                    -3.124e-05  5.079e-05  2.468e+03  -0.615   0.5386    
#   logWordFormFreq                 -8.053e-04  4.435e-04  7.720e+01  -1.816   0.0733 .  
#   BaseInitialStressunstressed     -8.161e-03  3.360e-03  8.020e+01  -2.429   0.0174 *  
#   LocSpeech                       -6.009e-03  3.143e-04  1.813e+03 -19.119  < 2e-16 ***
#   GlobalSpeechRate                 1.203e-03  1.365e-03  8.509e+02   0.881   0.3785    
#   PrePausePause                    2.617e-03  1.170e-03  2.495e+03   2.236   0.0254 *  
#   PostPausePause                   3.195e-04  1.352e-03  2.519e+03   0.236   0.8132   

qqnorm (residuals (un.lmerBC2))
qqline (residuals (un.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> un.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(un.lmerBC2)

# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.240e-01  5.511e-03  3.463e+02 131.369  < 2e-16 ***
#   Environmentn#C                  -8.529e-03  3.806e-03  1.123e+02  -2.241   0.0270 *  
#   Environmentn#nV                  3.936e-02  3.453e-03  8.060e+01  11.399  < 2e-16 ***
#   Environmentn#V                  -7.022e-02  3.409e-03  8.070e+01 -20.595  < 2e-16 ***
#   AccentuationConditionunaccented -7.540e-03  1.500e-03  1.918e+03  -5.026 5.48e-07 ***
#   OrderRescale                    -3.124e-05  5.079e-05  2.468e+03  -0.615   0.5386    
#   logWordFormFreq                 -8.053e-04  4.435e-04  7.720e+01  -1.816   0.0733 .  
#   BaseInitialStressunstressed     -8.161e-03  3.360e-03  8.020e+01  -2.429   0.0174 *  
#   LocSpeech                       -6.009e-03  3.143e-04  1.813e+03 -19.119  < 2e-16 ***
#   GlobalSpeechRate                 1.203e-03  1.365e-03  8.509e+02   0.881   0.3785    
#   PrePausePause                    2.617e-03  1.170e-03  2.495e+03   2.236   0.0254 *  
#   PostPausePause                   3.195e-04  1.352e-03  2.519e+03   0.236   0.8132    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1   

# let's throw out PostPause

un.lmerBC3 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC3)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.244e-01  5.223e-03  2.952e+02 138.696  < 2e-16 ***
#   Environmentn#C                  -8.523e-03  3.809e-03  1.125e+02  -2.238   0.0272 *  
#   Environmentn#nV                  3.934e-02  3.455e-03  8.080e+01  11.385  < 2e-16 ***
#   Environmentn#V                  -7.022e-02  3.413e-03  8.090e+01 -20.576  < 2e-16 ***
#   AccentuationConditionunaccented -7.638e-03  1.437e-03  1.828e+03  -5.314 1.21e-07 ***
#   OrderRescale                    -3.167e-05  5.074e-05  2.469e+03  -0.624   0.5325    
#   logWordFormFreq                 -8.061e-04  4.439e-04  7.740e+01  -1.816   0.0733 .  
#   BaseInitialStressunstressed     -8.130e-03  3.362e-03  8.030e+01  -2.419   0.0178 *  
#   LocSpeech                       -6.032e-03  3.005e-04  1.911e+03 -20.071  < 2e-16 ***
#   GlobalSpeechRate                 1.193e-03  1.364e-03  8.518e+02   0.875   0.3818    
#   PrePausePause                    2.653e-03  1.160e-03  2.501e+03   2.287   0.0223 * 

anova(un.lmerBC2,un.lmerBC3)

# Data: un2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   ..1:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   ..1:     (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     PostPause + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    14 -11696 -11614   5862   -11724                         
# object 15 -11694 -11606   5862   -11724 0.0657      1     0.7977

# model did not become worse


# let's throw out Order

un.lmerBC4<- lmer(bc ~ Environment+ AccentuationCondition+logWordFormFreq+
                     BaseInitialStress + LocSpeech + GlobalSpeechRate +
                     PrePause + (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC4)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.239e-01  5.164e-03  2.823e+02 140.180  < 2e-16 ***
#   Environmentn#C                  -8.554e-03  3.810e-03  1.124e+02  -2.245   0.0267 *  
#   Environmentn#nV                  3.933e-02  3.457e-03  8.080e+01  11.377  < 2e-16 ***
#   Environmentn#V                  -7.025e-02  3.414e-03  8.090e+01 -20.577  < 2e-16 ***
#   AccentuationConditionunaccented -7.609e-03  1.437e-03  1.838e+03  -5.296 1.32e-07 ***
#   logWordFormFreq                 -8.058e-04  4.441e-04  7.740e+01  -1.814   0.0735 .  
#   BaseInitialStressunstressed     -8.139e-03  3.363e-03  8.030e+01  -2.420   0.0178 *  
#   LocSpeech                       -6.033e-03  3.005e-04  1.913e+03 -20.075  < 2e-16 ***
#   GlobalSpeechRate                 1.171e-03  1.364e-03  8.581e+02   0.858   0.3909    
#   PrePausePause                    2.697e-03  1.158e-03  2.501e+03   2.329   0.0199 *     

# # nothing has changed

anova(un.lmerBC3,un.lmerBC4)

# Data: un2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + logWordFormFreq + 
#   ..1:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   ..1:     (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -11698 -11622 5861.8   -11724                         
# object 14 -11696 -11614 5862.0   -11724 0.3967      1     0.5288

# nothing has changed



# let's throw out GlobalSpeechRate

un.lmerBC5<- lmer(bc ~ Environment+ AccentuationCondition+logWordFormFreq+
                    BaseInitialStress + LocSpeech + 
                    PrePause + (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC5)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.253e-01  4.881e-03  2.277e+02 148.590  < 2e-16 ***
#   Environmentn#C                  -8.693e-03  3.794e-03  1.129e+02  -2.291   0.0238 *  
#   Environmentn#nV                  3.933e-02  3.443e-03  8.120e+01  11.422  < 2e-16 ***
#   Environmentn#V                  -7.009e-02  3.395e-03  8.080e+01 -20.645  < 2e-16 ***
#   AccentuationConditionunaccented -6.892e-03  1.160e-03  2.534e+03  -5.942 3.21e-09 ***
#   logWordFormFreq                 -7.882e-04  4.418e-04  7.750e+01  -1.784   0.0783 .  
#   BaseInitialStressunstressed     -8.388e-03  3.338e-03  7.970e+01  -2.513   0.0140 *  
#   LocSpeech                       -5.980e-03  2.951e-04  1.972e+03 -20.265  < 2e-16 ***
#   PrePausePause                    2.589e-03  1.151e-03  2.507e+03   2.250   0.0246 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(un.lmerBC4,un.lmerBC5)

# Data: un2
# Models:
# ..1: bc ~ Environment + AccentuationCondition + logWordFormFreq + 
#   ..1:     BaseInitialStress + LocSpeech + PrePause + (1 | Item) + (1 | 
#                                                                       ..1:     Participant)
# object: bc ~ Environment + AccentuationCondition + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -11699 -11629 5861.4   -11723                         
# object 13 -11698 -11622 5861.8   -11724 0.7011      1     0.4024

#nothing has changed


# let's throw out logWordFormFreq

un.lmerBC6<- lmer(bc ~ Environment+ AccentuationCondition+
                    BaseInitialStress + LocSpeech + 
                    PrePause + (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6)


# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.204e-01  3.996e-03  4.396e+02 180.289  < 2e-16 ***
#   Environmentn#C                  -5.347e-03  3.339e-03  1.274e+02  -1.602   0.1117    
#   Environmentn#nV                  4.255e-02  2.967e-03  8.360e+01  14.341  < 2e-16 ***
#   Environmentn#V                  -6.795e-02  3.221e-03  8.390e+01 -21.093  < 2e-16 ***
#   AccentuationConditionunaccented -6.828e-03  1.160e-03  2.536e+03  -5.884 4.53e-09 ***
#   BaseInitialStressunstressed     -6.953e-03  3.288e-03  8.130e+01  -2.115   0.0375 *  
#   LocSpeech                       -6.000e-03  2.956e-04  2.013e+03 -20.299  < 2e-16 ***
#   PrePausePause                    2.610e-03  1.151e-03  2.507e+03   2.268   0.0234 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(un.lmerBC5,un.lmerBC6)

# Data: un2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + BaseInitialStress + 
#   ..1:     LocSpeech + PrePause + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + PrePause + (1 | Item) + (1 | 
#                                                                          object:     Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    11 -11698 -11633 5859.7   -11720                           
# object 12 -11699 -11629 5861.4   -11723 3.3294      1    0.06805 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# the model is not worse (but significance is almost reaches, need to observe this
# when checking for interactions)

# so that would be the final model without interactions

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1.Environment and PrePause

# 2. Environment and accentuation

# (Stress and Environment cannot be tested)


# 3. Prepause and accentuation

# 4. Stress and accentuation

#5. PrePause and Stress


table(un2$Environment,un2$BaseInitialStress)

#       primary unstressed
# #nV      492         27
# n#C      426          0
# n#nV     911         40
# n#V      317        336
# Let's see



# 1a. Environment and PrePause

un.lmerBC6IntEnvPrePause<- lmer(bc ~ Environment*PrePause+ AccentuationCondition+
                    BaseInitialStress + LocSpeech + 
                     (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvPrePause)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      7.227e-01  4.011e-03  4.542e+02 180.167  < 2e-16 ***
#   Environmentn#C                  -7.577e-03  3.631e-03  1.761e+02  -2.087  0.03834 *  
#   Environmentn#nV                  3.495e-02  3.141e-03  1.050e+02  11.128  < 2e-16 ***
#   Environmentn#V                  -6.586e-02  3.455e-03  1.109e+02 -19.063  < 2e-16 ***
#   PrePausePause                   -7.037e-03  2.456e-03  2.489e+03  -2.865  0.00421 ** 
#   AccentuationConditionunaccented -7.108e-03  1.148e-03  2.534e+03  -6.192 6.91e-10 ***
#   BaseInitialStressunstressed     -6.749e-03  3.286e-03  8.120e+01  -2.054  0.04320 *  
#   LocSpeech                       -5.970e-03  2.929e-04  2.033e+03 -20.380  < 2e-16 ***
#   Environmentn#C:PrePausePause     9.570e-03  3.374e-03  2.492e+03   2.836  0.00461 ** 
#   Environmentn#nV:PrePausePause    1.843e-02  2.821e-03  2.461e+03   6.531 7.94e-11 ***
#   Environmentn#V:PrePausePause     1.964e-03  3.018e-03  2.454e+03   0.651  0.51527  


visreg(un.lmerBC6IntEnvPrePause)
visreg(un.lmerBC6IntEnvPrePause, "Environment",by="PrePause", trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
visreg(un.lmerBC6IntEnvPrePause, "PrePause",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

# there is an effect BUTI do not think it makes much sense, why is a base-intial nasal longer
# when NOT preceded by a pause

# is this model better than the one without an interaction?

anova(un.lmerBC6,un.lmerBC6IntEnvPrePause)
# Data: un2
# Models:
#   object: bc ~ Environment + AccentuationCondition + BaseInitialStress + 
#   object:     LocSpeech + PrePause + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * PrePause + AccentuationCondition + BaseInitialStress + 
#   ..1:     LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11698 -11633 5859.7   -11720                             
# ..1    14 -11756 -11674 5892.0   -11784 64.608      3  6.085e-14 ***

# yes, so keep that in mind

# 2. Environment and Accentuaton


un.lmerBC6IntEnvAcc<- lmer(bc ~ Environment*AccentuationCondition+PrePause+ 
                                  BaseInitialStress + LocSpeech + 
                                  (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvAcc)


# Fixed effects:
#                                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                      7.259e-01  4.023e-03  4.561e+02 180.443  < 2e-16 ***
#   Environmentn#C                                  -1.292e-02  3.668e-03  1.786e+02  -3.523 0.000541 ***
#   Environmentn#nV                                  4.071e-02  3.238e-03  1.149e+02  12.574  < 2e-16 ***
#   Environmentn#V                                  -8.074e-02  3.518e-03  1.154e+02 -22.950  < 2e-16 ***
#   AccentuationConditionunaccented                 -1.736e-02  2.174e-03  2.496e+03  -7.985 2.22e-15 ***
#   PrePausePause                                    2.377e-03  1.126e-03  2.503e+03   2.112 0.034806 *  
#   BaseInitialStressunstressed                     -6.603e-03  3.313e-03  8.120e+01  -1.993 0.049608 *  
#   LocSpeech                                       -6.014e-03  2.921e-04  2.054e+03 -20.591  < 2e-16 ***
#   Environmentn#C:AccentuationConditionunaccented   1.574e-02  2.986e-03  2.426e+03   5.270 1.49e-07 ***
#   Environmentn#nV:AccentuationConditionunaccented  3.811e-03  2.492e-03  2.424e+03   1.529 0.126312    
#   Environmentn#V:AccentuationConditionunaccented   2.517e-02  2.673e-03  2.418e+03   9.417  < 2e-16 ***
# there is an interaction! Let's have a look

visreg(un.lmerBC6IntEnvAcc)
visreg(un.lmerBC6IntEnvAcc, "AccentuationCondition",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
visreg(un.lmerBC6IntEnvAcc, "Environment",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

# okay basically it tells us that the difference between the accented unn and base nasals
#are even longer than
# the unaccented, wheter there is no difference between un1V in accented and un1accented position

# This is nothing exciting, let's see whether the model with the interacton is better
# than our previous one

anova(un.lmerBC6IntEnvPrePause,un.lmerBC6IntEnvAcc)

# object: bc ~ Environment * PrePause + AccentuationCondition + BaseInitialStress + 
#   object:     LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + PrePause + BaseInitialStress + 
#   ..1:     LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 14 -11756 -11674 5892.0   -11784                             
# ..1    14 -11811 -11729 5919.5   -11839 54.937      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# so it is better

cor(un2$bc, fitted(un.lmerBC6IntEnvAcc))^2
#[1] 0.8549863

cor(un2$bc, fitted(un.lmerBC6IntEnvPrePause))^2
#[1] 0.8514249


# What if I put in both interactions?

# Environment and Accentuaton, and Environment and PrePause

un.lmerBC6IntEnvAccPause<- lmer(bc ~ Environment*AccentuationCondition+Environment*PrePause+ 
                             BaseInitialStress + LocSpeech + 
                             (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvAccPause)
# Fixed effects:
#                                                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                      7.313e-01  4.076e-03  4.934e+02 179.407  < 2e-16 ***
# Environmentn#C                                  -2.180e-02  4.213e-03  3.061e+02  -5.175 4.13e-07 ***
# Environmentn#nV                                  2.831e-02  3.546e-03  1.682e+02   7.985 2.10e-13 ***
# Environmentn#V                                  -8.480e-02  3.883e-03  1.742e+02 -21.835  < 2e-16 ***
# AccentuationConditionunaccented                 -2.286e-02  2.277e-03  2.497e+03 -10.040  < 2e-16 ***
# PrePausePause                                   -1.375e-02  2.533e-03  2.486e+03  -5.429 6.20e-08 ***
# BaseInitialStressunstressed                     -6.790e-03  3.290e-03  8.120e+01  -2.064 0.042230 *  
# LocSpeech                                       -5.864e-03  2.900e-04  2.058e+03 -20.223  < 2e-16 ***
# Environmentn#C:AccentuationConditionunaccented   2.199e-02  3.187e-03  2.441e+03   6.901 6.55e-12 ***
# Environmentn#nV:AccentuationConditionunaccented  1.106e-02  2.615e-03  2.430e+03   4.229 2.44e-05 ***
# Environmentn#V:AccentuationConditionunaccented   2.937e-02  2.782e-03  2.421e+03  10.557  < 2e-16 ***
# Environmentn#C:PrePausePause                     1.888e-02  3.560e-03  2.497e+03   5.303 1.24e-07 ***
# Environmentn#nV:PrePausePause                    2.393e-02  2.929e-03  2.463e+03   8.171 4.44e-16 ***
# Environmentn#V:PrePausePause                     1.210e-02  3.108e-03  2.454e+03   3.893 0.000102 ***


# Both are significant!
visreg(un.lmerBC6IntEnvAccPause, "AccentuationCondition",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
visreg(un.lmerBC6IntEnvAccPause, "PrePause",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

#What about dreifach-Ineraktion

un.lmerBC6IntEnvAccPauseAll<- lmer(bc ~ Environment*AccentuationCondition*PrePause+ 
                                     BaseInitialStress + LocSpeech + 
                                     (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvAccPauseAll)

# NO, so is the one with the two interactions better than the one with one?


anova(un.lmerBC6IntEnvAcc,un.lmerBC6IntEnvAccPause)
# Data: un2
# Models:
#   object: bc ~ Environment * AccentuationCondition + PrePause + BaseInitialStress + 
#   object:     LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + Environment * PrePause + 
#   ..1:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 14 -11811 -11729 5919.5   -11839                             
# ..1    17 -11876 -11777 5955.2   -11910 71.319      3  2.227e-15 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# yeah so that is teh current one

# 3. Prepause and accentuation

un.lmerBC6IntAccPause<- lmer(bc ~ Environment+AccentuationCondition*PrePause+ 
                                     BaseInitialStress + LocSpeech + 
                                     (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccPause)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                    7.229e-01  4.022e-03  4.579e+02 179.712  < 2e-16 ***
#   Environmentn#C                                -5.650e-03  3.311e-03  1.284e+02  -1.706   0.0904 .  
# Environmentn#nV                                4.234e-02  2.938e-03  8.380e+01  14.410  < 2e-16 ***
# Environmentn#V                                -6.833e-02  3.190e-03  8.400e+01 -21.419  < 2e-16 ***
# AccentuationConditionunaccented               -1.190e-02  1.600e-03  2.535e+03  -7.440 1.38e-13 ***
#   PrePausePause                                 -2.050e-03  1.536e-03  2.494e+03  -1.334   0.1823    
# BaseInitialStressunstressed                   -6.805e-03  3.255e-03  8.130e+01  -2.091   0.0397 *  
#   LocSpeech                                     -5.912e-03  2.948e-04  1.997e+03 -20.054  < 2e-16 ***
#   AccentuationConditionunaccented:PrePausePause  9.150e-03  2.004e-03  2.474e+03   4.565 5.24e-06 ***

visreg(un.lmerBC6IntAccPause, "PrePause",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
# hmmm

#let's see whether it is better than teh other models

anova(un.lmerBC6IntAccPause,un.lmerBC6IntEnvAccPause)
# Data: un2
# Models:
#   object: bc ~ Environment + AccentuationCondition * PrePause + BaseInitialStress + 
#   object:     LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + Environment * PrePause + 
#   ..1:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -11716 -11646 5870.2   -11740                             
# ..1    17 -11876 -11777 5955.2   -11910 170.03      5  < 2.2e-16 ***



# no, but what about other cmbinations

#1. int env acc and int pase acc


un.lmerBC6IntAccPauseEnvAcc<- lmer(bc ~ Environment*AccentuationCondition+AccentuationCondition*PrePause+ 
                               BaseInitialStress + LocSpeech + 
                               (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccPauseEnvAcc)


# Fixed effects:
#                                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                      7.269e-01  4.034e-03  4.644e+02 180.184  < 2e-16 ***
#   Environmentn#C                                  -1.228e-02  3.660e-03  1.809e+02  -3.355 0.000967 ***
#   Environmentn#nV                                  4.149e-02  3.233e-03  1.167e+02  12.834  < 2e-16 ***
#   Environmentn#V                                  -7.992e-02  3.512e-03  1.172e+02 -22.754  < 2e-16 ***
#   AccentuationConditionunaccented                 -1.907e-02  2.250e-03  2.498e+03  -8.476  < 2e-16 ***
#   PrePausePause                                   -5.916e-04  1.530e-03  2.490e+03  -0.387 0.699038    
#   BaseInitialStressunstressed                     -6.498e-03  3.294e-03  8.110e+01  -1.973 0.051944 .  
#   LocSpeech                                       -5.979e-03  2.918e-04  2.044e+03 -20.492  < 2e-16 ***
#   Environmentn#C:AccentuationConditionunaccented   1.411e-02  3.036e-03  2.427e+03   4.647 3.54e-06 ***
#   Environmentn#nV:AccentuationConditionunaccented  1.969e-03  2.571e-03  2.425e+03   0.766 0.443733    
#   Environmentn#V:AccentuationConditionunaccented   2.310e-02  2.765e-03  2.420e+03   8.353  < 2e-16 ***
#   AccentuationConditionunaccented:PrePausePause    5.865e-03  2.047e-03  2.471e+03   2.866 0.004198 ** 

visreg(un.lmerBC6IntAccPauseEnvAcc, "PrePause",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
visreg(un.lmerBC6IntAccPauseEnvAcc, "Environment",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

# okay that makes sense

#let's see whether it is better than teh other models

anova(un.lmerBC6IntAccPauseEnvAcc,un.lmerBC6IntEnvAccPause)

# ..1: bc ~ Environment * AccentuationCondition + Environment * PrePause + 
#   ..1:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -11817 -11730 5923.6   -11847                             
# ..1    17 -11876 -11777 5955.2   -11910 63.066      2   2.02e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#un.lmerBC6IntEnvAccPause is better

#2. int env pause and acc pause


un.lmerBC6IntAccPauseEnvPause<- lmer(bc ~ Environment*PrePause+AccentuationCondition*PrePause+ 
                               BaseInitialStress + LocSpeech + 
                               (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccPauseEnvPause)


#   (Intercept)                                    7.248e-01  4.032e-03  4.697e+02 179.783  < 2e-16 ***
#   Environmentn#C                                -7.709e-03  3.605e-03  1.777e+02  -2.138  0.03385 *  
#   Environmentn#nV                                3.503e-02  3.114e-03  1.053e+02  11.249  < 2e-16 ***
#   Environmentn#V                                -6.586e-02  3.425e-03  1.112e+02 -19.227  < 2e-16 ***
#   PrePausePause                                 -1.072e-02  2.590e-03  2.485e+03  -4.139 3.61e-05 ***
#   AccentuationConditionunaccented               -1.189e-02  1.582e-03  2.531e+03  -7.519 7.64e-14 ***
#   BaseInitialStressunstressed                   -6.585e-03  3.255e-03  8.120e+01  -2.023  0.04638 *  
#   LocSpeech                                     -5.885e-03  2.923e-04  2.018e+03 -20.135  < 2e-16 ***
#   Environmentn#C:PrePausePause                   8.911e-03  3.366e-03  2.491e+03   2.648  0.00816 ** 
#   Environmentn#nV:PrePausePause                  1.755e-02  2.818e-03  2.460e+03   6.228 5.55e-10 ***
#   Environmentn#V:PrePausePause                   9.531e-04  3.017e-03  2.453e+03   0.316  0.75206    
#   PrePausePause:AccentuationConditionunaccented  8.662e-03  1.987e-03  2.470e+03   4.359 1.36e-05 ***

visreg(un.lmerBC6IntAccPauseEnvPause, "PrePause",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
visreg(un.lmerBC6IntAccPauseEnvPause, "PrePause",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

#let's see whether it is better than teh other models

anova(un.lmerBC6IntAccPauseEnvPause,un.lmerBC6IntEnvAccPause)

# Data: un2
# Models:
#   object: bc ~ Environment * PrePause + AccentuationCondition * PrePause + 
#   object:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + Environment * PrePause + 
#   ..1:     BaseInitialStress + LocSpeech + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -11773 -11686 5901.6   -11803                             
# ..1    17 -11876 -11777 5955.2   -11910 107.23      2  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# still un.lmerBC6IntEnvAccPause

# 4. Stress and accentuation


un.lmerBC6IntAccStress<- lmer(bc ~ Environment+AccentuationCondition*  
                                       BaseInitialStress +PrePause+ LocSpeech + 
                                       (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccStress)

# Fixed effects:
#                                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                  7.217e-01  3.988e-03  4.379e+02 180.998  < 2e-16 ***
#   Environmentn#C                                              -5.436e-03  3.345e-03  1.263e+02  -1.625   0.1066    
#   Environmentn#nV                                              4.252e-02  2.980e-03  8.370e+01  14.271  < 2e-16 ***
#   Environmentn#V                                              -6.783e-02  3.234e-03  8.390e+01 -20.970  < 2e-16 ***
#   AccentuationConditionunaccented                             -9.527e-03  1.218e-03  2.536e+03  -7.822 7.55e-15 ***
#   BaseInitialStressunstressed                                 -1.557e-02  3.538e-03  1.071e+02  -4.399 2.58e-05 ***
#   PrePausePause                                                2.629e-03  1.140e-03  2.506e+03   2.306   0.0212 *  
#   LocSpeech                                                   -6.002e-03  2.934e-04  2.040e+03 -20.457  < 2e-16 ***
#   AccentuationConditionunaccented:BaseInitialStressunstressed  1.705e-02  2.511e-03  2.422e+03   6.789 1.41e-11 ***


visreg(un.lmerBC6IntAccStress, "AccentuationCondition",by="BaseInitialStress", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)


anova(un.lmerBC6IntAccStress,un.lmerBC6IntEnvAccPause)

# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -11741 -11671 5882.6   -11765                             
# ..1    17 -11876 -11777 5955.2   -11910 145.18      5  < 2.2e-16 ***


# the other one is till better, but again we need to test other combinations of interactions

# 1. Stress and Acc and Env and Pause

un.lmerBC6IntAccStressEnvPause<- lmer(bc ~ Environment*PrePause+AccentuationCondition*  
                                BaseInitialStress + LocSpeech + 
                                (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccStressEnvPause)

# Fixed effects:
#                                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                  7.242e-01  4.007e-03  4.539e+02 180.726  < 2e-16 ***
#   Environmentn#C                                              -7.235e-03  3.630e-03  1.742e+02  -1.993  0.04779 *  
#   Environmentn#nV                                              3.508e-02  3.146e-03  1.046e+02  11.150  < 2e-16 ***
#   Environmentn#V                                              -6.705e-02  3.464e-03  1.109e+02 -19.354  < 2e-16 ***
#   PrePausePause                                               -7.546e-03  2.439e-03  2.487e+03  -3.093  0.00200 ** 
#   AccentuationConditionunaccented                             -9.582e-03  1.209e-03  2.533e+03  -7.926 3.33e-15 ***
#   BaseInitialStressunstressed                                 -1.473e-02  3.540e-03  1.082e+02  -4.160 6.40e-05 ***
#   LocSpeech                                                   -5.984e-03  2.911e-04  2.054e+03 -20.556  < 2e-16 ***
#   Environmentn#C:PrePausePause                                 9.050e-03  3.350e-03  2.490e+03   2.701  0.00695 ** 
#   Environmentn#nV:PrePausePause                                1.843e-02  2.800e-03  2.459e+03   6.582 5.66e-11 ***
#   Environmentn#V:PrePausePause                                 4.420e-03  3.022e-03  2.453e+03   1.463  0.14367    
#   AccentuationConditionunaccented:BaseInitialStressunstressed  1.561e-02  2.529e-03  2.421e+03   6.171 7.94e-10 ***



anova(un.lmerBC6IntAccStressEnvPause,un.lmerBC6IntEnvAccPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -11792 -11704 5911.0   -11822                             
# ..1    17 -11876 -11777 5955.2   -11910 88.397      2  < 2.2e-16 ***

# other one is still better

# 2. Acc and Stress and Env and Acc


un.lmerBC6IntAccStressEnvAcc<- lmer(bc ~ Environment*AccentuationCondition+AccentuationCondition*  
                                        BaseInitialStress +PrePause+ LocSpeech + 
                                        (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccStressEnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  7.258e-01  4.021e-03  4.561e+02 180.497  < 2e-16 ***
#   Environmentn#C                                              -1.316e-02  3.669e-03  1.790e+02  -3.587 0.000431 ***
#   Environmentn#nV                                              4.066e-02  3.237e-03  1.150e+02  12.560  < 2e-16 ***
#   Environmentn#V                                              -7.929e-02  3.589e-03  1.252e+02 -22.091  < 2e-16 ***
#  AccentuationConditionunaccented                             -1.771e-02  2.180e-03  2.496e+03  -8.127 6.66e-16 ***
#   BaseInitialStressunstressed                                 -9.755e-03  3.664e-03  1.213e+02  -2.662 0.008817 ** 
#   PrePausePause                                                2.456e-03  1.126e-03  2.502e+03   2.182 0.029227 *  
#   LocSpeech                                                   -5.991e-03  2.921e-04  2.051e+03 -20.507  < 2e-16 ***
#   Environmentn#C:AccentuationConditionunaccented               1.608e-02  2.989e-03  2.426e+03   5.378 8.24e-08 ***
#   Environmentn#nV:AccentuationConditionunaccented              3.886e-03  2.491e-03  2.423e+03   1.560 0.118845    
#   Environmentn#V:AccentuationConditionunaccented               2.236e-02  3.016e-03  2.416e+03   7.414 1.69e-13 ***
#   AccentuationConditionunaccented:BaseInitialStressunstressed  6.090e-03  3.030e-03  2.419e+03   2.010 0.044524 *

anova(un.lmerBC6IntAccStressEnvAcc,un.lmerBC6IntEnvAccPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -11813 -11725 5921.5   -11843                             
# ..1    17 -11876 -11777 5955.2   -11910 67.267      2  2.472e-15 ***

# other one is still better

#3. Stress and Pause and Acc and Pause

un.lmerBC6IntAccStressPauseAcc<- lmer(bc ~ Environment+PrePause*AccentuationCondition+AccentuationCondition*  
                                      BaseInitialStress + LocSpeech + 
                                      (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntAccStressPauseAcc)

# Fixed effects:
#                                                               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  7.238e-01  4.014e-03  4.551e+02 180.302  < 2e-16 ***
# Environmentn#C                                              -5.691e-03  3.321e-03  1.272e+02  -1.713   0.0891 .  
# Environmentn#nV                                              4.234e-02  2.954e-03  8.380e+01  14.332  < 2e-16 ***
# Environmentn#V                                              -6.816e-02  3.207e-03  8.400e+01 -21.253  < 2e-16 ***
# PrePausePause                                               -1.370e-03  1.528e-03  2.492e+03  -0.897   0.3700    
# AccentuationConditionunaccented                             -1.372e-02  1.613e-03  2.532e+03  -8.504  < 2e-16 ***
# BaseInitialStressunstressed                                 -1.493e-02  3.514e-03  1.079e+02  -4.249 4.57e-05 ***
# LocSpeech                                                   -5.926e-03  2.929e-04  2.025e+03 -20.231  < 2e-16 ***
# PrePausePause:AccentuationConditionunaccented                7.851e-03  1.998e-03  2.472e+03   3.929 8.78e-05 ***
# AccentuationConditionunaccented:BaseInitialStressunstressed  1.605e-02  2.517e-03  2.421e+03   6.374 2.19e-10 ***


anova(un.lmerBC6IntAccStressPauseAcc,un.lmerBC6IntEnvAccPause)
# object 13 -11755 -11679 5890.3   -11781                             
# ..1    17 -11876 -11777 5955.2   -11910 129.72      4  < 2.2e-16 ***

# still better

#5. PrePause and Stress




un.lmerBC6IntPauseStress<- lmer(bc ~ Environment+AccentuationCondition+  
                                BaseInitialStress*PrePause+ LocSpeech + 
                                (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntPauseStress)

# Fixed effects:
#                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                7.197e-01  4.020e-03  4.404e+02 179.020  < 2e-16 ***
#   Environmentn#C                            -5.497e-03  3.368e-03  1.260e+02  -1.632  0.10514    
#   Environmentn#nV                            4.243e-02  3.000e-03  8.350e+01  14.145  < 2e-16 ***
#   Environmentn#V                            -6.800e-02  3.256e-03  8.370e+01 -20.883  < 2e-16 ***
#   AccentuationConditionunaccented           -6.939e-03  1.160e-03  2.536e+03  -5.980 2.54e-09 ***
#   BaseInitialStressunstressed               -2.751e-03  3.708e-03  1.244e+02  -0.742  0.45954    
#   PrePausePause                              3.614e-03  1.216e-03  2.509e+03   2.973  0.00298 ** 
#   LocSpeech                                 -5.968e-03  2.961e-04  2.036e+03 -20.158  < 2e-16 ***
#   BaseInitialStressunstressed:PrePausePause -6.924e-03  2.716e-03  2.478e+03  -2.549  0.01086 *  

visreg(un.lmerBC6IntPauseStress, "PrePause",by="BaseInitialStress", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)


anova(un.lmerBC6IntPauseStress,un.lmerBC6IntEnvAccPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -11702 -11632 5862.9   -11726                             
# ..1    17 -11876 -11777 5955.2   -11910 184.48      5  < 2.2e-16 ***

# other is still better

# need to check the combinations of interactions again

#1. Pause and Stress AND Environment and Pause

un.lmerBC6IntPauseStressEnvPause<- lmer(bc ~ Environment*PrePause+AccentuationCondition+  
                                        BaseInitialStress*PrePause + LocSpeech + 
                                        (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntPauseStressEnvPause)
#NO

#2. Pause and Stress AND Environment and Accentuation

un.lmerBC6IntPauseStressEnveAcc<- lmer(bc ~ Environment*AccentuationCondition+  
                                        BaseInitialStress*PrePause + LocSpeech + 
                                        (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntPauseStressEnveAcc)
#NO

#3. Pause and Stress AND Pause and Accentuation

un.lmerBC6IntStressPauseAccPause<- lmer(bc ~ Environment+PrePause*AccentuationCondition+  
                                        BaseInitialStress*PrePause + LocSpeech + 
                                        (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntStressPauseAccPause)

# Fixed effects:
#                                               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                    7.222e-01  4.045e-03  4.576e+02 178.529  < 2e-16 ***
# Environmentn#C                                -5.816e-03  3.342e-03  1.269e+02  -1.740  0.08420 .  
# Environmentn#nV                                4.221e-02  2.972e-03  8.360e+01  14.200  < 2e-16 ***
# Environmentn#V                                -6.839e-02  3.226e-03  8.380e+01 -21.198  < 2e-16 ***
# PrePausePause                                 -1.079e-03  1.575e-03  2.495e+03  -0.685  0.49321    
# AccentuationConditionunaccented               -1.213e-02  1.600e-03  2.533e+03  -7.579 4.84e-14 ***
# BaseInitialStressunstressed                   -2.318e-03  3.677e-03  1.251e+02  -0.631  0.52951    
# LocSpeech                                     -5.876e-03  2.953e-04  2.022e+03 -19.899  < 2e-16 ***
# PrePausePause:AccentuationConditionunaccented  9.347e-03  2.003e-03  2.472e+03   4.667 3.22e-06 ***
# PrePausePause:BaseInitialStressunstressed     -7.385e-03  2.707e-03  2.477e+03  -2.729  0.00641 ** 

anova(un.lmerBC6IntStressPauseAccPause,un.lmerBC6IntEnvAccPause)
# object 13 -11722 -11646 5873.8   -11748                            
#..1    17 -11876 -11777 5955.2   -11910 162.7      4  < 2.2e-16 ***

# still better

#4. Pause and Stress AND Stress and Accentuation

un.lmerBC6IntStressPauseAccStress<- lmer(bc ~ Environment+PrePause*BaseInitialStress+AccentuationCondition*  
                                        BaseInitialStress + LocSpeech + 
                                        (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntStressPauseAccStress)
#NO


#################################
# 3 ways
################
# we already tested the 3 way interaction environment, accentuation, pause (not sign),
# let us also test the 3 way interaction accentuation, pause and stress

un.lmerBC6IntPauseAccStress3way<- lmer(bc ~ Environment+PrePause*BaseInitialStress*AccentuationCondition + LocSpeech + 
                                           (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntPauseAccStress3way)
#NO

################
# Last thing to test is whether the model becomes better if we have athird intercation


# Env*pause, Env*Acc and Pause*Acc
un.lmerBC6IntEnvPauseEnvAccPauseAcc<- lmer(bc ~ Environment*PrePause + Environment*AccentuationCondition + BaseInitialStress+
                                         LocSpeech + PrePause*AccentuationCondition+
                                         (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvPauseEnvAccPauseAcc)


#NO

# Env*pause, Env*Acc and Stess*Acc

un.lmerBC6IntEnvPauseEnvAccStressAcc<- lmer(bc ~ Environment*PrePause + Environment*AccentuationCondition + 
                                             LocSpeech + BaseInitialStress*AccentuationCondition+
                                             (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvPauseEnvAccStressAcc)

# NO

# Env*pause, Env*Acc and Pause*Stress

un.lmerBC6IntEnvPauseEnvAccStressPause<- lmer(bc ~ Environment*PrePause + Environment*AccentuationCondition + 
                                              LocSpeech + BaseInitialStress*PrePause+
                                              (1|Item) + (1|Participant), data = un2)

summary(un.lmerBC6IntEnvPauseEnvAccStressPause)

# NO

##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# I tested the interactiond between the variables Environment, accentiuation, prepause,
# and stress. While all of them were significant, none of the 3-way interactions
# was, the final model has 2 interactions


visreg(un.lmerBC6IntEnvAccPause, "AccentuationCondition",by="Environment" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="accentuation by environment",ylim=c(0,180), cex.axis=0.8)

# doubles and base-words have a longer nasal when accented

visreg(un.lmerBC6IntEnvAccPause, "PrePause",by="Environment" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="accentuation by environment",ylim=c(0,180), cex.axis=0.8)

# doubles are longer when preceded by a pause, the others not, I do not know why


#############################################################
# The final model:

summary(un.lmerBC6IntEnvAccPause)

# Fixed effects:
#                                                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                      7.313e-01  4.076e-03  4.934e+02 179.407  < 2e-16 ***
# Environmentn#C                                  -2.180e-02  4.213e-03  3.061e+02  -5.175 4.13e-07 ***
# Environmentn#nV                                  2.831e-02  3.546e-03  1.682e+02   7.985 2.10e-13 ***
# Environmentn#V                                  -8.480e-02  3.883e-03  1.742e+02 -21.835  < 2e-16 ***
# AccentuationConditionunaccented                 -2.286e-02  2.277e-03  2.497e+03 -10.040  < 2e-16 ***
# PrePausePause                                   -1.375e-02  2.533e-03  2.486e+03  -5.429 6.20e-08 ***
# BaseInitialStressunstressed                     -6.790e-03  3.290e-03  8.120e+01  -2.064 0.042230 *  
# LocSpeech                                       -5.864e-03  2.900e-04  2.058e+03 -20.223  < 2e-16 ***
# Environmentn#C:AccentuationConditionunaccented   2.199e-02  3.187e-03  2.441e+03   6.901 6.55e-12 ***
# Environmentn#nV:AccentuationConditionunaccented  1.106e-02  2.615e-03  2.430e+03   4.229 2.44e-05 ***
# Environmentn#V:AccentuationConditionunaccented   2.937e-02  2.782e-03  2.421e+03  10.557  < 2e-16 ***
# Environmentn#C:PrePausePause                     1.888e-02  3.560e-03  2.497e+03   5.303 1.24e-07 ***
# Environmentn#nV:PrePausePause                    2.393e-02  2.929e-03  2.463e+03   8.171 4.44e-16 ***
# Environmentn#V:PrePausePause                     1.210e-02  3.108e-03  2.454e+03   3.893 0.000102 ***
# ---

  
  lambda
#[1] 0.1818182


# I need to rename some variabels for the plot...


un2<-rename(un2,AccentuationAnnotator=Accentuation)

un2<-rename(un2,Accentuation=AccentuationCondition)

levels(un2$PrePause)
#[1] "No Pause" "Pause"   

levels(un2$PrePause)<-c("no pause","pause")


final_un_model.lmer<-lmer(bc ~  Environment*Accentuation+ LocSpeech+ Environment*PrePause+                                                + LocSpeech  + PrePause+
                                    BaseInitialStress+ 
                                    (1|Participant)+ (1|Item) , data = un2)


summary(final_un_model.lmer)

# Fixed effects:
#                                         Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                             7.313e-01  4.076e-03  4.934e+02 179.407  < 2e-16 ***
# Environmentn#C                         -2.180e-02  4.213e-03  3.061e+02  -5.175 4.13e-07 ***
# Environmentn#nV                         2.831e-02  3.546e-03  1.682e+02   7.985 2.10e-13 ***
# Environmentn#V                         -8.480e-02  3.883e-03  1.742e+02 -21.835  < 2e-16 ***
# Accentuationunaccented                 -2.286e-02  2.277e-03  2.497e+03 -10.040  < 2e-16 ***
# LocSpeech                              -5.864e-03  2.900e-04  2.058e+03 -20.223  < 2e-16 ***
# PrePausepause                          -1.375e-02  2.533e-03  2.486e+03  -5.429 6.20e-08 ***
# BaseInitialStressunstressed            -6.790e-03  3.290e-03  8.120e+01  -2.064 0.042230 *  
# Environmentn#C:Accentuationunaccented   2.199e-02  3.187e-03  2.441e+03   6.901 6.55e-12 ***
# Environmentn#nV:Accentuationunaccented  1.106e-02  2.615e-03  2.430e+03   4.229 2.44e-05 ***
# Environmentn#V:Accentuationunaccented   2.937e-02  2.782e-03  2.421e+03  10.557  < 2e-16 ***
# Environmentn#C:PrePausepause            1.888e-02  3.560e-03  2.497e+03   5.303 1.24e-07 ***
# Environmentn#nV:PrePausepause           2.393e-02  2.929e-03  2.463e+03   8.171 4.44e-16 ***
# Environmentn#V:PrePausepause            1.210e-02  3.108e-03  2.454e+03   3.893 0.000102 ***


##############################
# We should  plot  the effects

visreg(final_un_model.lmer)

###############################
# Plot main effect

# we need n#nV als reference levels

un2$Environment <- relevel (un2$Environment, ref= "n#nV")

# redo the final model

final_un_model.lmer2<-lmer(bc ~  Environment*Accentuation+ LocSpeech+ Environment*PrePause+                                                + LocSpeech  + PrePause+
                            BaseInitialStress+ 
                            (1|Participant)+ (1|Item) , data = un2)


summary(final_un_model.lmer2)

# Fixed effects:
#                                         Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                            7.596e-01  3.929e-03  4.852e+02 193.333  < 2e-16 ***
# Environment#nV                        -2.831e-02  3.546e-03  1.682e+02  -7.985 2.10e-13 ***
# Environmentn#C                        -5.011e-02  3.895e-03  2.504e+02 -12.865  < 2e-16 ***
# Environmentn#V                        -1.131e-01  3.798e-03  1.715e+02 -29.779  < 2e-16 ***
# Accentuationunaccented                -1.180e-02  1.600e-03  2.480e+03  -7.377 2.20e-13 ***
# LocSpeech                             -5.864e-03  2.900e-04  2.058e+03 -20.223  < 2e-16 ***
# PrePausepause                          1.018e-02  1.633e-03  2.466e+03   6.234 5.33e-10 ***
# BaseInitialStressunstressed           -6.790e-03  3.290e-03  8.120e+01  -2.064   0.0422 *  
# Environment#nV:Accentuationunaccented -1.106e-02  2.615e-03  2.430e+03  -4.229 2.44e-05 ***
# Environmentn#C:Accentuationunaccented  1.094e-02  2.774e-03  2.420e+03   3.943 8.28e-05 ***
# Environmentn#V:Accentuationunaccented  1.832e-02  2.360e-03  2.422e+03   7.761 1.24e-14 ***
# Environment#nV:PrePausepause          -2.393e-02  2.929e-03  2.463e+03  -8.171 4.44e-16 ***
# Environmentn#C:PrePausepause          -5.048e-03  2.916e-03  2.479e+03  -1.731   0.0835 .  
# Environmentn#V:PrePausepause          -1.183e-02  2.466e-03  2.439e+03  -4.798 1.70e-06 ***
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")

# we need to changethe ref level for the data set

un$Environment <- relevel (un$Environment, ref= "n#V")
un$Environment <- relevel (un$Environment, ref= "n#C")
un$Environment <- relevel (un$Environment, ref= "n#nV")


png("boxUn.png", units="cm", height=10, width=7, res=300, pointsize=8)
#bwplot (ConsonantDurMS ~ Environment, un, ylab="duration in milliseconds", main="un-", ylim=c(0,320), cex.axis=0.5)


cols = list(col=c("dodgerblue3"),pch=c(1))
bwplot (ConsonantDurMS ~ Environment, un, ylab="duration in milliseconds", 
        main="un-", ylim=c(0,320), cex.axis=0.5,
        par.settings = list(
          plot.symbol=cols,
          box.rectangle = cols,
          #box.dot = cols,
          box.umbrella=cols 
        ))

dev.off()

ylim=c(20,180)

png("unModelCompleteInterEnvAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der Estimate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

#visreg(final_un_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)




visreg(final_un_model.lmer2, "Environment",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)




dev.off()


png("unModelCompleteInterEnvPause.png", units="cm", height=12, width=14, res=300, pointsize=15)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der Estimate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

#visreg(final_un_model.lmer2, "Environment",by="PrePause",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by pause before item",ylim=ylim,cex.axis=0.9,par.settings=par)
visreg(final_un_model.lmer2, "Environment",by="PrePause",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()


library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 


un.lm2<-lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause, 
   data = un2)

model_ranking <- dredge(un.lm2)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Relative variable importance: 
#   Environment LocSpeech Accentuation logWordFormFreq
# Importance:          1.00        1.00      1.00         1.00           
# N containing models:  256         256       256          256           
# BaseInitialStress PrePause PostPause OrderRescale
# Importance:          1.00              0.83     0.50      0.30        
# N containing models:  256               256      256       256        
# GlobalSpeechRate
# Importance:          0.27            
# N containing models:  256  

# check mumin with interaction



options(na.action = "na.fail") 


un.lm3<-lm(ConsonantDur ~ Environment*Accentuation*BaseInitialStress+ PrePause*Accentuation*BaseInitialStress+Environment*PrePause + 
               OrderRescale + logWordFormFreq +  LocSpeech + 
               GlobalSpeechRate +  PostPause  , 
             data = un2)

model_ranking2 <- dredge(un.lm3)

model_average_2<-model.avg(model_ranking2)


summary(model_average_2)


# Relative variable importance: 
#   Accentuation BaseInitialStress Environment LocSpeech Accentuation:Environment PrePause
# Importance:          1.00         1.00              1.00        1.00      1.00                     1.00    
# N containing models: 3680         3680              3648        2128      1792                     3648    
# BaseInitialStress:Environment Environment:PrePause logWordFormFreq
# Importance:          1.00                          1.00                 1.00           
# N containing models: 1792                          1600                 2128           
# Accentuation:BaseInitialStress PostPause Accentuation:PrePause BaseInitialStress:PrePause
# Importance:          0.85                           0.53      0.41                  0.41                      
# N containing models: 1952                           2128      1792                  1792                      
# Accentuation:BaseInitialStress:Environment OrderRescale GlobalSpeechRate
# Importance:          0.32                                       0.29         0.27            
# N containing models:  352                                       2128         2128            
# Accentuation:BaseInitialStress:PrePause
# Importance:          0.10                                   
# N containing models:  352    


# I need the output in Latex

table_final_models<-as.data.frame(coef(summary(final_un_model.lmer2)))

xtable(table_final_models,digits = 6)


# So, this is our final model: The effects are
#   1. Doubles longer than singles --> un geminates: This effect is even stronger when the word is accented
#   2. The longer the word, the longer the consonant and the more segments in the word,
#      the shorter the consonant (speech rate as a phonetic effect)
#   3. After a pause /n/ is longer
#  4. The longer the preceding segment, the shorter the consonant




###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

options(scipen=0)

summary(final_un_model.lmer2)
#   Fixed effects:
#                                       Estimate Std. Error         df t value
# (Intercept)                            7.596e-01  3.929e-03  4.852e+02 193.333
# Environment#nV                        -2.831e-02  3.546e-03  1.682e+02  -7.985
# Environmentn#C                        -5.011e-02  3.895e-03  2.504e+02 -12.865
# Environmentn#V                        -1.131e-01  3.798e-03  1.715e+02 -29.779
# Accentuationunaccented                -1.180e-02  1.600e-03  2.480e+03  -7.377
# LocSpeech                             -5.864e-03  2.900e-04  2.058e+03 -20.223
# PrePausepause                          1.018e-02  1.633e-03  2.466e+03   6.234
# BaseInitialStressunstressed           -6.790e-03  3.290e-03  8.120e+01  -2.064
# Environment#nV:Accentuationunaccented -1.106e-02  2.615e-03  2.430e+03  -4.229
# Environmentn#C:Accentuationunaccented  1.094e-02  2.774e-03  2.420e+03   3.943
# Environmentn#V:Accentuationunaccented  1.832e-02  2.360e-03  2.422e+03   7.761
# Environment#nV:PrePausepause          -2.393e-02  2.929e-03  2.463e+03  -8.171
# Environmentn#C:PrePausepause          -5.048e-03  2.916e-03  2.479e+03  -1.731
# Environmentn#V:PrePausepause          -1.183e-02  2.466e-03  2.439e+03  -4.798

#                                           Pr(>|t|)    
# (Intercept)                            < 2e-16 ***
#   Environment#nV                        2.10e-13 ***
# Environmentn#C                         < 2e-16 ***
# Environmentn#V                         < 2e-16 ***
# Accentuationunaccented                2.20e-13 ***
#   LocSpeech                              < 2e-16 ***
#   PrePausepause                         5.33e-10 ***
#   BaseInitialStressunstressed             0.0422 *  
#   Environment#nV:Accentuationunaccented 2.44e-05 ***
# Environmentn#C:Accentuationunaccented 8.28e-05 ***
# Environmentn#V:Accentuationunaccented 1.24e-14 ***
# Environment#nV:PrePausepause          4.44e-16 ***
# Environmentn#C:PrePausepause            0.0835 .  
# Environmentn#V:PrePausepause          1.70e-06 ***

visreg(final_un_model.lmer2)


# Conditions used in construction of plot


# Conditions used in construction of plot
# Environment: n#nV
# LocSpeech: 10.75551
# PrePause: pause
# BaseInitialStress: primary
# Participant: Experiment_1_participant_30
# Item: unnumbered



intercept =  7.596e-01
LocCondition= 10.75551
estSpeech= -5.864e-03

estStress=-6.790e-03

# PrePause: pause
estPause= 1.018e-02

# Accentuation: accented
estUnaccented=-1.180e-02

EstEnvironmentunV= -1.131e-01
EstEnvironmentunC= -5.011e-02
EstEnvironmentunBase= -2.831e-02

EstunVunaccented= 1.183e-02
EstunCunaccented= 1.094e-02
EstunBaseunaccented= -1.106e-02


EsunVPause= -1.832e-02
EstunCPause= -5.048e-03
EstunBasePause= -2.393e-02

ranef(final_un_model.lmer2)$Participant
# Participant:Experiment_1_participant_30
EstParticipant=1.323866e-02

ranef(final_un_model.lmer2)$Item
# Item: unnumbered
EstItem=9.953078e-03

visreg(final_un_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, cond=list(PrePause="no pause"),rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)


visreg(final_un_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, cond=list(PrePause="pause"),rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)


visreg(final_un_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)



