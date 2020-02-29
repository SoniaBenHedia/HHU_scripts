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



unComplex <- read.csv("unComplex.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(unComplex)



# 'data.frame':	2067 obs. of  84 variables:
#   $ X.1                        : int  550 551 552 553 554 555 556 557 558 559 ...
# $ X                          : int  3432 3433 3434 3435 3436 3437 3438 3439 3440 3441 ...
# $ Item                       : Factor w/ 69 levels "unable","unacquainted",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 51 levels "Experiment_1_participant_10",..: 5 2 9 21 22 20 14 18 26 10 ...
# $ ID                         : int  820 194 1548 3554 4323 3332 2230 2967 4843 1858 ...
# $ Filename                   : Factor w/ 2067 levels "participant_1_A_10.TextGrid",..: 454 195 618 1208 1212 1153 889 1066 1575 690 ...
# $ DeletionMorph              : Factor w/ 3 levels "L","N","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 2 2 2 1 2 2 1 1 1 1 ...
# $ Annotator                  : Factor w/ 6 levels "Lara","Mandy",..: 5 6 3 3 6 5 1 2 6 5 ...
# $ Order                      : int  120 11 108 298 22 219 198 250 295 311 ...
# $ WordDur                    : num  0.381 0.493 0.388 0.42 0.374 ...
# $ SyllNum                    : int  3 3 3 3 3 3 3 3 3 3 ...
# $ SegNum                     : int  5 5 5 6 5 5 5 5 5 5 ...
# $ ConsonantDur               : num  0.0514 0.0738 0.0575 0.0443 0.0538 ...
# $ PrecSeg                    : Factor w/ 12 levels "@","{","2","a",..: 12 8 1 12 12 12 12 5 12 12 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0762 0.0631 0.045 0.0533 0.0564 ...
# $ FollSeg                    : Factor w/ 54 levels "?","@","@i","@O",..: 15 24 24 24 24 24 26 24 24 17 ...
# $ FollSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur                 : num  0.126 0.171 0.15 0.212 0.149 ...
# $ PrePauseDur                : num  0.091 0 0 0.132 0 ...
# $ PostPauseDur               : num  0 0 0 0 0 ...
# $ SentenceDur                : num  4.19 3.52 3.12 3.17 2.59 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ GlottalStopDur             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ LocSpeech                  : num  13.1 10.2 12.9 14.3 13.4 ...
# $ AffixDur                   : num  0.1276 0.137 0.1025 0.0976 0.1102 ...
# $ BaseDuration               : num  0.253 0.356 0.286 0.322 0.264 ...
# $ FirstSyllDur               : num  0.1276 0.137 0.1025 0.0976 0.1102 ...
# $ WordDurWithoutGlottalStop  : num  0.381 0.493 0.388 0.42 0.374 ...
# $ AffixDurWithoutGlottalStop : num  0.1276 0.137 0.1025 0.0976 0.1102 ...
# $ Environment                : Factor w/ 3 levels "n#C","n#nV","n#V": 3 3 3 3 3 3 3 3 3 3 ...
# $ Affix                      : Factor w/ 1 level "un": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  6141 6141 6141 6141 6141 6141 6141 6141 6141 6141 ...
# $ WordFormFrequencyAllCOCA   : int  17018 17018 17018 17018 17018 17018 17018 17018 17018 17018 ...
# $ WordFormFrequencySpokenCOCA: int  1802 1802 1802 1802 1802 1802 1802 1802 1802 1802 ...
# $ Base                       : Factor w/ 69 levels "able","acquainted",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ BaseLemmaFrequencyBNC      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ SyllPhon                   : int  3 3 3 3 3 3 3 3 3 3 ...
# $ AffixStress                : Factor w/ 3 levels "debatable","secondary",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": 2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  1 1 1 1 1 1 2 1 1 1 ...
# $ TimeRating                 : num  1222 1005 1054 1221 1315 ...
# $ TotalTime                  : num  991 805 889 1006 1129 ...
# $ Age                        : int  19 19 21 61 19 19 30 19 20 19 ...
# $ Sex                        : Factor w/ 6 levels "female","Female",..: 2 1 1 2 4 2 2 5 5 4 ...
# $ L1                         : Factor w/ 10 levels "british","British",..: 8 5 1 2 5 5 8 5 5 2 ...
# $ Bilingual                  : Factor w/ 6 levels "I only know British English",..: 4 1 3 3 3 4 4 4 4 6 ...
# $ Grow_Up_Region             : Factor w/ 42 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 25 13 22 39 28 19 9 7 26 5 ...
# $ Languages                  : Factor w/ 30 levels "Basic French",..: 25 15 13 10 16 5 25 25 25 6 ...
# $ Latin                      : Factor w/ 19 levels "2 years secondary school",..: 7 8 12 6 6 7 7 7 2 4 ...
# $ Profession_Studies         : Factor w/ 50 levels "2nd Year Meida Studies",..: 22 28 36 44 17 48 24 11 10 25 ...
# $ University                 : Factor w/ 18 levels "Aberdeen University",..: 8 7 2 15 7 18 6 7 3 3 ...
# $ Knowledge_English_Ling     : Factor w/ 22 levels "2 years","Currently in my 2nd year of the course at university",..: 15 8 5 11 20 6 6 6 6 7 ...
# $ Phonetics                  : Factor w/ 16 levels "A couple of lectures",..: 14 2 7 13 13 8 4 8 8 8 ...
# $ Phonology                  : Factor w/ 14 levels "A couple of lectures",..: 13 4 6 12 12 7 3 6 7 7 ...
# $ Morphology                 : Factor w/ 12 levels "currently studying",..: 11 7 4 10 10 5 2 5 5 5 ...
# $ Semantics                  : Factor w/ 12 levels "currently studying",..: 11 7 4 10 10 5 2 5 5 5 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 2 2 2 2 2 1 1 1 1 ...
# $ Experiment                 : Factor w/ 2 levels "Experiment_1",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  8.72 8.72 8.72 8.72 8.72 ...
# $ logBaseLemmaFreq           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ logWordLemmaFreq           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ RelFreq                    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ logRelFreq                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Root                       : Factor w/ 43 levels "knit","known",..: NA NA NA NA NA NA NA NA NA NA ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : num  NA NA NA NA NA NA NA NA NA NA ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": NA NA NA NA NA NA NA NA NA NA ...
# $ Consonant                  : Factor w/ 5 levels "b","l","n","O",..: NA NA NA NA NA NA NA NA NA NA ...
# $ Orthography                : Factor w/ 4 levels "kn","n","nn",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ median                     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ TypeOfBase                 : Factor w/ 1 level "word": 1 1 1 1 1 1 1 1 1 1 ...
# $ ConsonantDurMS             : num  51.4 73.8 57.5 44.3 53.8 ...
# $ PrePause                   : Factor w/ 2 levels "No Pause","Pause": 2 1 1 2 1 1 1 2 2 2 ...
# $ PostPause                  : Factor w/ 2 levels "No Pause","Pause": 1 1 1 1 1 1 1 2 1 2 ...
# $ GlobalSpeechRate           : num  2.15 2.55 2.88 2.84 3.47 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ StressPattern              : Factor w/ 7 levels "debatable-primary",..: 7 7 7 7 7 7 7 7 7 7 ...

unComplex$X.1<-NULL
unComplex$X<-NULL


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

# - PrecSegDur (in model in which affixed words are compared)
# - logRelFreq (in model in which affixed words are compared)
# - Type of Root in model in which affixed words are compared)
# - ST in model in which affixed words are compared)
# - St Rating? 

# Let's see whether it males sense to include the decomposability measures (only
# makes sense if we have variability)

# 1. Semantic Transparency

table(unComplex$SemanticTransparency)
# opaque transparent 
# 47        2020 

#probably only one items

unique(unComplex[unComplex$SemanticTransparency=="opaque","Item"])
#[1] unnerve

# yes, so we can't include Semantic Transparency


# 2. Type of Base

table(unComplex$TypeOfRoot)
# bound  word 
# 228  1839

#probably only one items

unique(unComplex[unComplex$TypeOfRoot=="bound","Item"])
#[1] unnatural    unnavigable  unnecessary  unnegotiable unneutral   

# 5 and only with doubles, so we include it later to see, but nor tight now...

# 3. Rating

table(unComplex$Rating)
# 1    2    3    4 
# 1868  129   37    5 

unique(unComplex[unComplex$Rating==4,"Item"])
#5

unique(unComplex[unComplex$Rating==3,"Item"])
# some more, but I am not sure, does not really seem to make sense
# let's include it and then see

######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = unComplex)
cor(unComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.1201191


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = unComplex)
cor(unComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.7049382

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = unComplex)
cor(unComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.7674499

# so aroun1d 77 percent of the variability can be explained by this! That's a lot



##              Do an initial model:
unComplex$OrderRescale<-unComplex$Order*0.1

unComplex.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + PrecSegDur+
                           logRelFreq+ TypeOfRoot+(1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmer1)    

# Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + PrecSegDur + logRelFreq + TypeOfRoot +      (1 | Item) + (1 | Participant)
# Data: unComplex
# 
# REML criterion at convergence: -9473.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.4186 -0.5684 -0.0547  0.4932  7.6733 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 3.716e-05 0.006096
# Participant (Intercept) 9.122e-05 0.009551
# Residual                5.096e-04 0.022574
# Number of obs: 2067, groups:  Item, 69; Participant, 51
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.376e-01  7.593e-03  3.322e+02  18.127  < 2e-16 ***
#   Environmentn#nV                  4.822e-02  2.592e-03  7.640e+01  18.604  < 2e-16 ***
# Environmentn#V                  -4.097e-02  3.077e-03  1.091e+02 -13.316  < 2e-16 ***
# AccentuationConditionunaccented -3.929e-03  1.579e-03  1.412e+03  -2.488   0.0130 *  
#   OrderRescale                    -3.130e-05  5.470e-05  2.009e+03  -0.572   0.5673    
# logWordFormFreq                 -2.744e-04  4.165e-04  6.760e+01  -0.659   0.5122    
# BaseInitialStressunstressed     -3.055e-03  2.979e-03  6.740e+01  -1.026   0.3087    
# LocSpeech                       -4.350e-03  3.590e-04  1.255e+03 -12.115  < 2e-16 ***
#   GlobalSpeechRate                -4.076e-04  1.375e-03  5.912e+02  -0.297   0.7669    
# PrePausePause                    5.549e-03  1.270e-03  2.026e+03   4.369 1.31e-05 ***
#   PostPausePause                   3.397e-03  1.446e-03  2.049e+03   2.349   0.0189 *  
#   PrecSegDur                      -3.627e-02  2.486e-02  2.024e+03  -1.459   0.1447    
# logRelFreq                       7.476e-05  3.045e-04  2.124e+02   0.246   0.8063    
# TypeOfRootword                   6.959e-03  3.680e-03  4.920e+01   1.891   0.0645 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envrnmntn#nV Envirnmntn#V AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsPs PstPsP
# Envrnmntn#nV -0.411                                                                                  
# Envirnmntn#V -0.042  0.486                                                                           
# AccnttnCndt   0.181 -0.020        0.061                                                              
# OrderRescal  -0.123  0.021        0.003        0.044                                                 
# lgWrdFrmFrq  -0.253  0.013       -0.202        0.033 -0.011                                          
# BsIntlStrss  -0.095 -0.040       -0.491       -0.024 -0.020  0.309                                   
# LocSpeech    -0.641  0.206        0.008       -0.197  0.018  0.008 -0.144                            
# GloblSpchRt  -0.212 -0.087       -0.126       -0.558 -0.038 -0.024  0.109 -0.133                     
# PrePausePas  -0.247 -0.005       -0.051        0.069  0.059  0.011 -0.044  0.146  0.063              
# PostPausePs  -0.293  0.051        0.001        0.284  0.029  0.029 -0.029  0.294  0.025 -0.124       
# PrecSegDur   -0.479  0.019       -0.054       -0.061  0.007  0.013 -0.018  0.342  0.026  0.199  0.038
# logRelFreq    0.219  0.082       -0.010        0.062  0.058 -0.278 -0.185 -0.002 -0.110  0.017 -0.036
# TypeOfRtwrd  -0.561  0.316       -0.111       -0.008 -0.003  0.193  0.189  0.086 -0.023  0.001  0.039
# PrcSgD lgRlFr
# Envrnmntn#nV              
# Envirnmntn#V              
# AccnttnCndt               
# OrderRescal               
# lgWrdFrmFrq               
# BsIntlStrss               
# LocSpeech                 
# GloblSpchRt               
# PrePausePas               
# PostPausePs               
# PrecSegDur                
# logRelFreq    0.027       
# TypeOfRtwrd   0.013 -0.097

# Let's have a look the the R2


cor(unComplex$ConsonantDur, fitted(unComplex.lmer1))^2
#[1]  0.8129508


#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own


# 1.logWordFormFreq & logRelFreq

# Model woth both 
unComplex.lmerFrequencies <- lmer(ConsonantDur ~ logWordFormFreq+ logRelFreq+ (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmerFrequencies)    


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.011e-01  7.521e-03  9.110e+01  13.440   <2e-16 ***
#   logWordFormFreq -2.787e-03  2.022e-03  8.430e+01  -1.378    0.172    
# logRelFreq       4.691e-04  4.934e-04  1.285e+03   0.951    0.342   


cor(unComplex$ConsonantDur, fitted(unComplex.lmerFrequencies))^2
#[1] 0.767799


# only Word Form Freq

unComplex.lmerWordFrequency <- lmer(ConsonantDur ~ logWordFormFreq + (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmerWordFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)      0.099328   0.007232 83.190000  13.734   <2e-16 ***
#   logWordFormFreq -0.002605   0.001995 84.330000  -1.306    0.195     

cor(unComplex$ConsonantDur, fitted(unComplex.lmerWordFrequency))^2
#[1] 0.7676051


# only RelFreq
unComplex.lmerRelFrequency <- lmer(ConsonantDur ~ logRelFreq+  (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmerRelFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 9.402e-02  5.499e-03 9.080e+01   17.10   <2e-16 ***
#   logRelFreq  4.078e-04  4.916e-04 1.260e+03    0.83    0.407    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

cor(unComplex$ConsonantDur, fitted(unComplex.lmerRelFrequency))^2
#[1] 0.7676229



# So, WordFormFreq is never a significant predictor, neither is RelFreq. There is
# no supression effect. Thus, we do not need to worry about it. However, it would
# be intersting to see whether a categorical variable of WordFormFreq might have 
# an effect

# only Categorical WordFormFreq

unComplex.lmerWordFreqCategorical <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmerWordFreqCategorical)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                 0.089218   0.010614 69.270000   8.406 3.52e-12 ***
#   WordFreqCategoricalLowFreq  0.006927   0.012392 65.790000   0.559    0.578    
# WordFreqCategoricalMidFreq -0.001827   0.015645 65.830000  -0.117    0.907    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WrFCLF
# WrdFrqCtgLF -0.834       
# WrdFrqCtgMF -0.661  0.566

# No effect! However, one thing I would also like to check is whether the presense of Item
# as a random effect has an influence

# Let us see what happen if we have a model without a random effect for Item and bith Variables

unComplex.lmerFrequenciesWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ logWordFormFreq+ (1|Participant), data = unComplex)

summary(unComplex.lmerFrequenciesWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      8.686e-02  2.614e-03  1.614e+02  33.225   <2e-16 ***
#   logRelFreq      -5.431e-03  3.866e-04  1.469e+03 -14.050   <2e-16 ***
#   logWordFormFreq  3.494e-05  4.506e-04  2.039e+03   0.078    0.938    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgRlFr
# logRelFreq   0.572       
# lgWrdFrmFrq -0.562 -0.324

cor(unComplex$ConsonantDur, fitted(unComplex.lmerFrequenciesWithoutItem))^2
#[1] 0.1878763


# RelFreq without Item
unComplex.lmerRelFrequencyWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ (1|Participant), data = unComplex)

summary(unComplex.lmerRelFrequencyWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  8.698e-02  2.163e-03  8.110e+01   40.21   <2e-16 ***
#   logRelFreq  -5.422e-03  3.657e-04  1.513e+03  -14.83   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# logRelFreq 0.498 

cor(unComplex$ConsonantDur, fitted(unComplex.lmerRelFrequencyWithoutItem))^2
#[1]0.1878827

# WordFreq without Item

unComplex.lmerWordFreqWithoutItem <- lmer(ConsonantDur ~ logWordFormFreq+ (1|Participant), data = unComplex)

summary(unComplex.lmerWordFreqWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.078e-01  2.693e-03  7.130e+01  40.027  < 2e-16 ***
#   logWordFormFreq -1.972e-03  4.431e-04  2.020e+03  -4.449 9.08e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# lgWrdFrmFrq -0.401

cor(unComplex$ConsonantDur, fitted(unComplex.lmerWordFreqWithoutItem))^2
#[1] 0.1275656



unComplex.lmerWordFreqCategoricalWiithoutItem <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Participant), data = unComplex)

summary(unComplex.lmerWordFreqCategoricalWiithoutItem)

# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                9.819e-02  3.151e-03 1.279e+02  31.163  < 2e-16 ***
#   WordFreqCategoricalLowFreq 8.082e-03  2.606e-03 2.017e+03   3.101  0.00195 ** 
#   WordFreqCategoricalMidFreq 3.756e-04  3.320e-03 2.017e+03   0.113  0.90994    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WrFCLF
# WrdFrqCtgLF -0.592       
# WrdFrqCtgMF -0.464  0.561

cor(unComplex$ConsonantDur, fitted(unComplex.lmerWordFreqCategoricalWiithoutItem))^2
#[1] 0.1254296

#####################################
# Summary Coll. Frequencies:
#- Both measures only becomes significant when the random effect of Item is
# removed. The models become much worse then
# - There is no supression effect.
#################################################


# 2.  Loc Speech  and/or Global Speech


cor.test(unComplex$LocSpeech,unComplex$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  unComplex$LocSpeech and unComplex$GlobalSpeechRate
# t = 20.692, df = 2065, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3780510 0.4494943
# sample estimates:
#       cor 
# 0.4144109 

unComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.617e-01  5.536e-03  1.594e+02  29.207  < 2e-16 ***
#   LocSpeech        -5.718e-03  3.262e-04  1.891e+03 -17.529  < 2e-16 ***
#   GlobalSpeechRate -2.937e-03  1.118e-03  9.234e+02  -2.626  0.00878 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) LcSpch
# LocSpeech   -0.448       
# GloblSpchRt -0.051 -0.520

cor(unComplex$ConsonantDur, fitted(unComplex.lmerSpeechRates))^2
#[1]  0.8098319



unComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.611e-01  5.503e-03  1.586e+02   29.29   <2e-16 ***
#   LocSpeech   -6.180e-03  2.787e-04  1.968e+03  -22.18   <2e-16 ***

cor(unComplex$ConsonantDur, fitted(unComplex.lmerLocSpeech))^2
#[1] 0.8085955


unComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerGlobalSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.200e-01  5.687e-03  1.120e+02   21.10   <2e-16 ***
#   GlobalSpeechRate -1.406e-02  1.058e-03  1.594e+03  -13.29   <2e-16 ***


cor(unComplex$ConsonantDur, fitted(unComplex.lmerGlobalSpeech))^2
#[1] 0.7877536


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, bothe have a sign effect
# - The effect size of LocSpeech increases when it is the only variable in the model
# - The effect size of GlobalSpeech decreases when it is the only variable in the model
# - The effect direction never changes (no supression)
# - Not a big difference in R2 between the different models
#################################################


##############################################################
# The decomposability variables

unComplex.lmerRating<- lmer(ConsonantDur ~ Rating + (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerRating)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  9.633e-02  5.658e-03  1.047e+02   17.03   <2e-16 ***
#   Rating      -2.493e-03  1.781e-03  1.959e+03   -1.40    0.162      

# not significant

unComplex.lmerTypeOfBase<- lmer(ConsonantDur ~ TypeOfRoot + (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerTypeOfBase)
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)     0.13578    0.01789 67.12000   7.591  1.3e-10 ***
#   TypeOfRootword -0.04626    0.01849 65.97000  -2.502   0.0148 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# TypeOfRtwrd -0.958

# so only this mifhr have an effect (RelFreq not...) Let's see when we have both in one 
# model

unComplex.lmerTypeOfBaseRelFreq<- lmer(ConsonantDur ~ TypeOfRoot +logRelFreq+ (1|Item)+(1|Participant), data = unComplex)

summary(unComplex.lmerTypeOfBaseRelFreq)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     1.375e-01  1.819e-02  6.740e+01   7.562 1.44e-10 ***
#   TypeOfRootword -4.689e-02  1.870e-02  6.490e+01  -2.507   0.0147 *  
#   logRelFreq      4.243e-04  4.907e-04  1.254e+03   0.865   0.3874    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) TypOfR
# TypeOfRtwrd -0.956       
# logRelFreq   0.112 -0.039

# Type of root becomes signgicant - thats not good (but Type O base is sth. diff..)

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################
# Now we have dealt with all collinearity problems: 
# - We will keep both frequency variables even though they are never significanr
# - We will keep both Speech Rate variables but must be aware of the fact that their effect
#   size cannot be interpreted!
# - Rating is never significant, neither is RelFreq. Since I don't have all ratings.
#   tYpe Of base only becomes significant when RelFreq is in model (need to watch out for that)
###############################################################################################



# Let's refit our model incorportaing the "right variables"

unComplex.lmer3 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                           logRelFreq+ TypeOfRoot + (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmer3)
# Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + PrecSegDur + logRelFreq + TypeOfRoot +      (1 | Item) + (1 | Participant)
# Data: unComplex
# 
# REML criterion at convergence: -9473.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.4186 -0.5684 -0.0547  0.4932  7.6733 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 3.716e-05 0.006096
# Participant (Intercept) 9.122e-05 0.009551
# Residual                5.096e-04 0.022574
# Number of obs: 2067, groups:  Item, 69; Participant, 51
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.376e-01  7.593e-03  3.322e+02  18.127  < 2e-16 ***
#   Environmentn#nV                  4.822e-02  2.592e-03  7.640e+01  18.604  < 2e-16 ***
# Environmentn#V                  -4.097e-02  3.077e-03  1.091e+02 -13.316  < 2e-16 ***
# AccentuationConditionunaccented -3.929e-03  1.579e-03  1.412e+03  -2.488   0.0130 *  
#   OrderRescale                    -3.130e-05  5.470e-05  2.009e+03  -0.572   0.5673    
# logWordFormFreq                 -2.744e-04  4.165e-04  6.760e+01  -0.659   0.5122    
# BaseInitialStressunstressed     -3.055e-03  2.979e-03  6.740e+01  -1.026   0.3087    
# LocSpeech                       -4.350e-03  3.590e-04  1.255e+03 -12.115  < 2e-16 ***
#   GlobalSpeechRate                -4.076e-04  1.375e-03  5.912e+02  -0.297   0.7669    
# PrePausePause                    5.549e-03  1.270e-03  2.026e+03   4.369 1.31e-05 ***
#   PostPausePause                   3.397e-03  1.446e-03  2.049e+03   2.349   0.0189 *  
#   PrecSegDur                      -3.627e-02  2.486e-02  2.024e+03  -1.459   0.1447    
# logRelFreq                       7.476e-05  3.045e-04  2.124e+02   0.246   0.8063    
# TypeOfRootword                   6.959e-03  3.680e-03  4.920e+01   1.891   0.0645 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envrnmntn#nV Envirnmntn#V AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsPs PstPsP
# Envrnmntn#nV -0.411                                                                                  
# Envirnmntn#V -0.042  0.486                                                                           
# AccnttnCndt   0.181 -0.020        0.061                                                              
# OrderRescal  -0.123  0.021        0.003        0.044                                                 
# lgWrdFrmFrq  -0.253  0.013       -0.202        0.033 -0.011                                          
# BsIntlStrss  -0.095 -0.040       -0.491       -0.024 -0.020  0.309                                   
# LocSpeech    -0.641  0.206        0.008       -0.197  0.018  0.008 -0.144                            
# GloblSpchRt  -0.212 -0.087       -0.126       -0.558 -0.038 -0.024  0.109 -0.133                     
# PrePausePas  -0.247 -0.005       -0.051        0.069  0.059  0.011 -0.044  0.146  0.063              
# PostPausePs  -0.293  0.051        0.001        0.284  0.029  0.029 -0.029  0.294  0.025 -0.124       
# PrecSegDur   -0.479  0.019       -0.054       -0.061  0.007  0.013 -0.018  0.342  0.026  0.199  0.038
# logRelFreq    0.219  0.082       -0.010        0.062  0.058 -0.278 -0.185 -0.002 -0.110  0.017 -0.036
# TypeOfRtwrd  -0.561  0.316       -0.111       -0.008 -0.003  0.193  0.189  0.086 -0.023  0.001  0.039
# PrcSgD lgRlFr
# Envrnmntn#nV              
# Envirnmntn#V              
# AccnttnCndt               
# OrderRescal               
# lgWrdFrmFrq               
# BsIntlStrss               
# LocSpeech                 
# GloblSpchRt               
# PrePausePas               
# PostPausePs               
# PrecSegDur                
# logRelFreq    0.027       
# TypeOfRtwrd   0.013 -0.097

#############
# Note that type of word behaves weird..- changes effect size, I'll take it out
###############################################


unComplex.lmer4 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                          BaseInitialStress + LocSpeech + GlobalSpeechRate +
                          PrePause + PostPause + PrecSegDur+
                          logRelFreq+  (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmer4)


# Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + PrecSegDur + logRelFreq + (1 | Item) +      (1 | Participant)
# Data: unComplex
# 
# REML criterion at convergence: -9479.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.3982 -0.5789 -0.0573  0.4884  7.6227 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 4.024e-05 0.006343
# Participant (Intercept) 8.962e-05 0.009467
# Residual                5.095e-04 0.022573
# Number of obs: 2067, groups:  Item, 69; Participant, 51
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.460e-01  6.312e-03  1.045e+03  23.137  < 2e-16 ***
#   Environmentn#nV                  4.665e-02  2.517e-03  8.260e+01  18.533  < 2e-16 ***
# Environmentn#V                  -4.035e-02  3.120e-03  1.104e+02 -12.933  < 2e-16 ***
# AccentuationConditionunaccented -3.873e-03  1.578e-03  1.407e+03  -2.454   0.0143 *  
#   OrderRescale                    -3.086e-05  5.471e-05  2.008e+03  -0.564   0.5727    
# logWordFormFreq                 -4.295e-04  4.194e-04  6.860e+01  -1.024   0.3094    
# BaseInitialStressunstressed     -4.078e-03  3.001e-03  6.960e+01  -1.359   0.1786    
# LocSpeech                       -4.434e-03  3.592e-04  1.280e+03 -12.344  < 2e-16 ***
#   GlobalSpeechRate                -3.325e-04  1.373e-03  5.921e+02  -0.242   0.8087    
# PrePausePause                    5.533e-03  1.270e-03  2.025e+03   4.356 1.39e-05 ***
#   PostPausePause                   3.263e-03  1.445e-03  2.050e+03   2.258   0.0241 *  
#   PrecSegDur                      -3.733e-02  2.486e-02  2.023e+03  -1.502   0.1334    
# logRelFreq                       1.451e-04  3.073e-04  2.237e+02   0.472   0.6373    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (unComplex.lmer4))
qqline (residuals (unComplex.lmer4))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Use log(ConsonantDur):
unComplex.lmer4 <- lmer(log(ConsonantDur) ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                           logRelFreq+  (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmer4)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -1.923e+00  5.724e-02  1.015e+03 -33.596  < 2e-16 ***
#   Environmentn#nV                  4.036e-01  2.384e-02  7.550e+01  16.931  < 2e-16 ***
# Environmentn#V                  -5.687e-01  2.934e-02  9.810e+01 -19.385  < 2e-16 ***
# AccentuationConditionunaccented -2.196e-02  1.417e-02  1.418e+03  -1.550   0.1214    
# OrderRescale                    -7.404e-04  4.906e-04  2.002e+03  -1.509   0.1314    
# logWordFormFreq                 -8.200e-04  3.991e-03  6.450e+01  -0.205   0.8379    
# BaseInitialStressunstressed     -5.303e-02  2.855e-02  6.500e+01  -1.857   0.0678 .  
# LocSpeech                       -4.125e-02  3.251e-03  1.367e+03 -12.686  < 2e-16 ***
#   GlobalSpeechRate                -8.284e-03  1.236e-02  6.072e+02  -0.670   0.5029    
# PrePausePause                    4.647e-02  1.139e-02  2.020e+03   4.079 4.71e-05 ***
#   PostPausePause                   7.329e-03  1.297e-02  2.047e+03   0.565   0.5721    
# PrecSegDur                      -2.823e-01  2.230e-01  2.019e+03  -1.266   0.2058    
# logRelFreq                      -7.328e-04  2.844e-03  2.318e+02  -0.258   0.7969       

# okay, let's check the assumptions again

qqnorm (residuals (unComplex.lmer4))
qqline (residuals (unComplex.lmer4))

# not much better


# Maybe a box-cox transformation will lead to a better
# distribuition of res. Let's try


unComplex.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause + PrecSegDur+
                    logRelFreq , data = unComplex)

summary(unComplex.lm)


# Call:
#   lm(formula = ConsonantDur ~ Environment + AccentuationCondition + 
#        OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
#        GlobalSpeechRate + PrePause + PostPause + PrecSegDur + logRelFreq, 
#      data = unComplex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.096315 -0.015553 -0.001714  0.013393  0.147300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      1.494e-01  5.436e-03  27.483  < 2e-16 ***
#   Environmentn#nV                  4.815e-02  1.635e-03  29.450  < 2e-16 ***
# Environmentn#V                  -3.532e-02  2.183e-03 -16.174  < 2e-16 ***
# AccentuationConditionunaccented -3.170e-03  1.445e-03  -2.193 0.028401 *  
#   OrderRescale                    -3.948e-05  5.913e-05  -0.668 0.504470    
# logWordFormFreq                 -6.377e-04  2.558e-04  -2.493 0.012743 *  
#   BaseInitialStressunstressed     -4.904e-03  1.893e-03  -2.590 0.009655 ** 
#   LocSpeech                       -4.928e-03  3.159e-04 -15.598  < 2e-16 ***
#   GlobalSpeechRate                -4.475e-04  1.009e-03  -0.444 0.657358    
# PrePausePause                    4.189e-03  1.260e-03   3.324 0.000903 ***
#   PostPausePause                   3.192e-03  1.459e-03   2.188 0.028790 *  
#   PrecSegDur                      -2.176e-02  2.435e-02  -0.894 0.371607    
# logRelFreq                       1.635e-04  2.238e-04   0.731 0.465132    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02515 on 2054 degrees of freedom
# Multiple R-squared:  0.7576,	Adjusted R-squared:  0.7562 
# F-statistic: 534.9 on 12 and 2054 DF,  p-value: < 2.2e-16

bc<-boxcox(unComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.1010101

unComplex$bc <- unComplex$ConsonantDur^lambda

unComplex.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause + PrecSegDur+
                            logRelFreq+ (1|Item) + (1|Participant), data = unComplex)

summary(unComplex.lmerBC)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.231e-01  4.513e-03  1.018e+03 182.393  < 2e-16 ***
#   Environmentn#nV                  3.269e-02  1.883e-03  7.590e+01  17.357  < 2e-16 ***
# Environmentn#V                  -4.375e-02  2.317e-03  9.850e+01 -18.884  < 2e-16 ***
# AccentuationConditionunaccented -1.874e-03  1.117e-03  1.429e+03  -1.678   0.0937 .  
# OrderRescale                    -5.523e-05  3.861e-05  2.002e+03  -1.430   0.1528    
# logWordFormFreq                 -9.543e-05  3.153e-04  6.500e+01  -0.303   0.7631    
# BaseInitialStressunstressed     -4.101e-03  2.256e-03  6.540e+01  -1.818   0.0737 .  
# LocSpeech                       -3.290e-03  2.561e-04  1.378e+03 -12.845  < 2e-16 ***
#   GlobalSpeechRate                -6.095e-04  9.752e-04  6.168e+02  -0.625   0.5322    
# PrePausePause                    3.778e-03  8.970e-04  2.022e+03   4.212 2.64e-05 ***
#   PostPausePause                   7.898e-04  1.021e-03  2.047e+03   0.773   0.4394    
# PrecSegDur                      -2.311e-02  1.756e-02  2.021e+03  -1.316   0.1884    
# logRelFreq                      -4.190e-05  2.245e-04  2.342e+02  -0.187   0.8521    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#let's check the assumptions

qqnorm (residuals (unComplex.lmerBC))
qqline (residuals (unComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(unComplex.lmerBC, unComplex, trim = 2.5)
# n.removed = 50 
# percent.removed = 2.418965 

unComplex2<-outliers$data

dim(unComplex2)
#[1] 2017   85

dim(unComplex)
#[1] 2067   84


# okay it seemes to have worked

unComplex.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             logRelFreq+ (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.239e-01  4.056e-03  1.013e+03 203.162  < 2e-16 ***
#   Environmentn#nV                  3.264e-02  1.695e-03  7.630e+01  19.249  < 2e-16 ***
# Environmentn#V                  -4.400e-02  2.087e-03  9.940e+01 -21.085  < 2e-16 ***
# AccentuationConditionunaccented -3.066e-03  9.995e-04  1.582e+03  -3.068  0.00219 ** 
#   OrderRescale                    -5.426e-05  3.404e-05  1.948e+03  -1.594  0.11111    
# logWordFormFreq                 -1.569e-04  2.842e-04  6.600e+01  -0.552  0.58278    
# BaseInitialStressunstressed     -3.524e-03  2.040e-03  6.710e+01  -1.727  0.08871 .  
# LocSpeech                       -3.272e-03  2.291e-04  1.386e+03 -14.281  < 2e-16 ***
#   GlobalSpeechRate                -1.153e-04  8.901e-04  7.964e+02  -0.130  0.89695    
# PrePausePause                    4.069e-03  7.891e-04  1.979e+03   5.157 2.77e-07 ***
#   PostPausePause                   7.009e-04  9.020e-04  1.993e+03   0.777  0.43720    
# PrecSegDur                      -4.021e-02  1.563e-02  1.980e+03  -2.573  0.01014 *  
#   logRelFreq                      -1.434e-04  2.011e-04  2.409e+02  -0.713  0.47654    

qqnorm (residuals (unComplex.lmerBC2))
qqline (residuals (unComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> unComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(unComplex.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.239e-01  4.056e-03  1.013e+03 203.162  < 2e-16 ***
#   Environmentn#nV                  3.264e-02  1.695e-03  7.630e+01  19.249  < 2e-16 ***
# Environmentn#V                  -4.400e-02  2.087e-03  9.940e+01 -21.085  < 2e-16 ***
# AccentuationConditionunaccented -3.066e-03  9.995e-04  1.582e+03  -3.068  0.00219 ** 
#   OrderRescale                    -5.426e-05  3.404e-05  1.948e+03  -1.594  0.11111    
# logWordFormFreq                 -1.569e-04  2.842e-04  6.600e+01  -0.552  0.58278    
# BaseInitialStressunstressed     -3.524e-03  2.040e-03  6.710e+01  -1.727  0.08871 .  
# LocSpeech                       -3.272e-03  2.291e-04  1.386e+03 -14.281  < 2e-16 ***
#   GlobalSpeechRate                -1.153e-04  8.901e-04  7.964e+02  -0.130  0.89695    
# PrePausePause                    4.069e-03  7.891e-04  1.979e+03   5.157 2.77e-07 ***
#   PostPausePause                   7.009e-04  9.020e-04  1.993e+03   0.777  0.43720    
# PrecSegDur                      -4.021e-02  1.563e-02  1.980e+03  -2.573  0.01014 *  
#   logRelFreq                      -1.434e-04  2.011e-04  2.409e+02  -0.713  0.47654    

# let's throw out PostPause

unComplex.lmerBC3 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             logRelFreq+(1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC3)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.250e-01  3.838e-03  9.498e+02 214.962  < 2e-16 ***
#   Environmentn#nV                  3.258e-02  1.698e-03  7.610e+01  19.190  < 2e-16 ***
# Environmentn#V                  -4.401e-02  2.091e-03  9.920e+01 -21.048  < 2e-16 ***
# AccentuationConditionunaccented -3.278e-03  9.607e-04  1.538e+03  -3.412 0.000662 ***
#   OrderRescale                    -5.504e-05  3.402e-05  1.949e+03  -1.618 0.105853    
# logWordFormFreq                 -1.607e-04  2.849e-04  6.600e+01  -0.564 0.574548    
# BaseInitialStressunstressed     -3.466e-03  2.044e-03  6.710e+01  -1.696 0.094545 .  
# LocSpeech                       -3.326e-03  2.188e-04  1.465e+03 -15.201  < 2e-16 ***
#   GlobalSpeechRate                -1.300e-04  8.894e-04  7.941e+02  -0.146 0.883835    
# PrePausePause                    4.140e-03  7.835e-04  1.985e+03   5.283 1.41e-07 ***
#   PrecSegDur                      -4.071e-02  1.561e-02  1.980e+03  -2.608 0.009189 ** 
#   logRelFreq                      -1.388e-04  2.013e-04  2.415e+02  -0.689 0.491275 

anova(unComplex.lmerBC2,unComplex.lmerBC3)

# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   ..1:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   ..1:     PrecSegDur + logRelFreq + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     PostPause + PrecSegDur + logRelFreq + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    15 -11314 -11230 5671.8   -11344                         
# object 16 -11312 -11222 5672.1   -11344 0.6252      1     0.4291

# model did not become worse


# let's throw out WordFormFreq

unComplex.lmerBC4 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             logRelFreq+ (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC4)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.245e-01  3.766e-03  1.052e+03 218.920  < 2e-16 ***
#   Environmentn#nV                  3.254e-02  1.687e-03  7.740e+01  19.282  < 2e-16 ***
# Environmentn#V                  -4.423e-02  2.043e-03  1.030e+02 -21.654  < 2e-16 ***
# AccentuationConditionunaccented -3.264e-03  9.602e-04  1.549e+03  -3.400 0.000692 ***
#   OrderRescale                    -5.522e-05  3.402e-05  1.949e+03  -1.623 0.104696    
# BaseInitialStressunstressed     -3.148e-03  1.952e-03  6.730e+01  -1.613 0.111449    
# LocSpeech                       -3.325e-03  2.186e-04  1.468e+03 -15.211  < 2e-16 ***
#   GlobalSpeechRate                -1.441e-04  8.893e-04  8.016e+02  -0.162 0.871306    
# PrePausePause                    4.146e-03  7.834e-04  1.985e+03   5.292 1.34e-07 ***
#   PrecSegDur                      -4.062e-02  1.561e-02  1.980e+03  -2.602 0.009330 ** 
#   logRelFreq                      -1.688e-04  1.941e-04  2.141e+02  -0.869 0.385599      

# # nothing has changed

anova(unComplex.lmerBC3,unComplex.lmerBC4)

# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   ..1:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + logRelFreq + 
#   ..1:     (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     PrecSegDur + logRelFreq + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    14 -11315 -11237 5671.6   -11343                        
# object 15 -11314 -11230 5671.8   -11344 0.343      1     0.5581

# nothing has changed



# let's throw out logRelFreq

unComplex.lmerBC5 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.251e-01  3.721e-03  1.046e+03 221.718  < 2e-16 ***
#   Environmentn#nV                  3.271e-02  1.684e-03  7.780e+01  19.421  < 2e-16 ***
# Environmentn#V                  -4.433e-02  2.048e-03  1.037e+02 -21.646  < 2e-16 ***
# AccentuationConditionunaccented -3.199e-03  9.569e-04  1.489e+03  -3.343 0.000849 ***
#   OrderRescale                    -5.360e-05  3.396e-05  1.951e+03  -1.578 0.114649    
# BaseInitialStressunstressed     -3.311e-03  1.952e-03  6.790e+01  -1.696 0.094449 .  
# LocSpeech                       -3.326e-03  2.187e-04  1.493e+03 -15.210  < 2e-16 ***
#   GlobalSpeechRate                -2.188e-04  8.831e-04  7.113e+02  -0.248 0.804365    
# PrePausePause                    4.155e-03  7.831e-04  1.987e+03   5.305 1.25e-07 ***
#   PrecSegDur                      -4.021e-02  1.560e-02  1.982e+03  -2.578 0.010017 *  

anova(unComplex.lmerBC4,unComplex.lmerBC5)

# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   ..1:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + (1 | 
#                                                                      ..1:     Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   object:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + logRelFreq + 
#   object:     (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -11316 -11244 5671.2   -11342                         
# object 14 -11315 -11237 5671.6   -11343 0.7987      1     0.3715

#nothing has changed


# let's throw out GlobalSpeech

unComplex.lmerBC6 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + 
                             PrePause + PrecSegDur+ 
                             (1|Item) + (1|Participant), data = unComplex2)


summary(unComplex.lmerBC6)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.248e-01  3.572e-03  1.073e+03 230.944  < 2e-16 ***
#   Environmentn#nV                  3.268e-02  1.682e-03  7.750e+01  19.426  < 2e-16 ***
# Environmentn#V                  -4.439e-02  2.035e-03  1.009e+02 -21.813  < 2e-16 ***
# AccentuationConditionunaccented -3.339e-03  7.651e-04  1.998e+03  -4.364 1.34e-05 ***
#   OrderRescale                    -5.399e-05  3.392e-05  1.948e+03  -1.592   0.1116    
# BaseInitialStressunstressed     -3.255e-03  1.941e-03  6.650e+01  -1.677   0.0983 .  
# LocSpeech                       -3.336e-03  2.163e-04  1.536e+03 -15.419  < 2e-16 ***
#   PrePausePause                    4.165e-03  7.814e-04  1.989e+03   5.330 1.09e-07 ***
#   PrecSegDur                      -4.009e-02  1.559e-02  1.983e+03  -2.572   0.0102 *  

anova(unComplex.lmerBC5,unComplex.lmerBC6)


# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   ..1:     LocSpeech + PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   object:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + (1 | 
#                                                                         object:     Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -11318 -11251 5671.2   -11342                         
# object 13 -11316 -11244 5671.2   -11342 0.0628      1     0.8021

# nothing has changed


# let's throw out Order

unComplex.lmerBC7 <- lmer(bc ~ Environment+ AccentuationCondition+ BaseInitialStress+
                             LocSpeech + 
                             PrePause + PrecSegDur+ 
                              (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC7)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.239e-01  3.522e-03  1.034e+03 233.942  < 2e-16 ***
#   Environmentn#nV                  3.271e-02  1.684e-03  7.740e+01  19.426  < 2e-16 ***
# Environmentn#V                  -4.439e-02  2.037e-03  1.008e+02 -21.796  < 2e-16 ***
# AccentuationConditionunaccented -3.315e-03  7.652e-04  1.999e+03  -4.333 1.55e-05 ***
#   BaseInitialStressunstressed     -3.265e-03  1.943e-03  6.640e+01  -1.680   0.0976 .  
# LocSpeech                       -3.335e-03  2.164e-04  1.538e+03 -15.410  < 2e-16 ***
#   PrePausePause                    4.246e-03  7.801e-04  1.990e+03   5.443 5.89e-08 ***
#   PrecSegDur                      -4.007e-02  1.559e-02  1.984e+03  -2.570   0.0102 *  

anova(unComplex.lmerBC6,unComplex.lmerBC7)

# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + BaseInitialStress + 
#   ..1:     LocSpeech + PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   object:     LocSpeech + PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -11318 -11256 5669.9   -11340                         
# object 12 -11318 -11251 5671.2   -11342 2.5487      1     0.1104

#still no difference


# let's throw out Stress

unComplex.lmerBC8 <- lmer(bc ~ Environment+ AccentuationCondition+
                             LocSpeech + 
                             PrePause + PrecSegDur+ 
                             (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC8)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.245e-01  3.514e-03  9.983e+02 234.654  < 2e-16 ***
#   Environmentn#nV                  3.250e-02  1.708e-03  7.920e+01  19.027  < 2e-16 ***
# Environmentn#V                  -4.600e-02  1.821e-03  1.187e+02 -25.254  < 2e-16 ***
# AccentuationConditionunaccented -3.214e-03  7.640e-04  1.998e+03  -4.207 2.70e-05 ***
#   LocSpeech                       -3.392e-03  2.149e-04  1.471e+03 -15.782  < 2e-16 ***
#   PrePausePause                    4.167e-03  7.789e-04  1.995e+03   5.349 9.84e-08 ***
#   PrecSegDur                      -4.075e-02  1.559e-02  1.984e+03  -2.613  0.00903 ** 

anova(unComplex.lmerBC7,unComplex.lmerBC8)
# Data: unComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + LocSpeech + PrePause + 
#   ..1:     PrecSegDur + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + BaseInitialStress + 
#   object:     LocSpeech + PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    10 -11317 -11261 5668.5   -11337                           
# object 11 -11318 -11256 5669.9   -11340 2.8952      1    0.08884 .


# still no difference

# so that would be the final model without interactions

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1. a) RelFreq+ Accentuation
#   b) Rel Freq and Environment

# 2. Environment and accentuation

# 3. Type of Root  and accentuation

#- 4. Rating and accentuation

#.5 Stress and accentuation


# Let's see



# 1a. RelFreq and Accentuaton

unComplex.lmerBC9InteractionRelFreqAccent<- lmer(bc ~ Environment+ logRelFreq*AccentuationCondition+
                                                   LocSpeech + 
                                                   PrePause + PrecSegDur+ 
                                                   (1|Item) + (1|Participant), data = unComplex2)
summary(unComplex.lmerBC9InteractionRelFreqAccent)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                 8.233e-01  3.555e-03  9.985e+02 231.567  < 2e-16 ***
#   Environmentn#nV                             3.233e-02  1.705e-03  7.860e+01  18.969  < 2e-16 ***
# Environmentn#V                             -4.571e-02  1.825e-03  1.189e+02 -25.047  < 2e-16 ***
# logRelFreq                                 -4.736e-04  2.196e-04  3.651e+02  -2.157  0.03164 *  
#   AccentuationConditionunaccented            -1.538e-03  9.678e-04  1.994e+03  -1.590  0.11208    
# LocSpeech                                  -3.414e-03  2.146e-04  1.451e+03 -15.905  < 2e-16 ***
#   PrePausePause                               4.158e-03  7.778e-04  1.993e+03   5.346    1e-07 ***
#   PrecSegDur                                 -4.007e-02  1.558e-02  1.982e+03  -2.572  0.01020 *  
#   logRelFreq:AccentuationConditionunaccented  5.626e-04  1.976e-04  1.913e+03   2.847  0.00447 

visreg(unComplex.lmerBC9InteractionRelFreqAccent)
visreg(unComplex.lmerBC9InteractionRelFreqAccent, "logRelFreq",by="AccentuationCondition", trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

# there is an effect BUT let's be cautios

# is this model better than the one without an interaction?

anova(unComplex.lmerBC8,unComplex.lmerBC9InteractionRelFreqAccent)
# Data: unComplex2
# Models:
#   object: bc ~ Environment + AccentuationCondition + LocSpeech + PrePause + 
#   object:     PrecSegDur + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment + logRelFreq * AccentuationCondition + LocSpeech + 
#   ..1:     PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 10 -11317 -11261 5668.5   -11337                           
# ..1    12 -11322 -11255 5673.0   -11346 8.9975      2    0.01112 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# yes, so keep that in mind

# 1b. RelFreq and Environment

unComplex.lmerBC9InteractionRelFreqEnv<- lmer(bc ~ Environment*logRelFreq+AccentuationCondition+
                                                   LocSpeech + 
                                                   PrePause + PrecSegDur+ 
                                                   (1|Item) + (1|Participant), data = unComplex2)
summary(unComplex.lmerBC9InteractionRelFreqEnv)

# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      8.216e-01  3.956e-03  5.117e+02 207.686  < 2e-16 ***
#   Environmentn#nV                  3.491e-02  2.752e-03  1.117e+02  12.687  < 2e-16 ***
#   Environmentn#V                  -4.279e-02  2.575e-03  1.004e+02 -16.618  < 2e-16 ***
#   logRelFreq                      -7.602e-04  3.942e-04  7.360e+01  -1.929  0.05762 .  
#   AccentuationConditionunaccented -3.187e-03  7.645e-04  1.994e+03  -4.169 3.19e-05 ***
#   LocSpeech                       -3.400e-03  2.152e-04  1.448e+03 -15.802  < 2e-16 ***
#   PrePausePause                    4.163e-03  7.790e-04  1.995e+03   5.344 1.01e-07 ***
#   PrecSegDur                      -4.082e-02  1.560e-02  1.984e+03  -2.617  0.00895 ** 
#   Environmentn#nV:logRelFreq       5.924e-04  4.681e-04  1.204e+02   1.266  0.20810    
#   Environmentn#V:logRelFreq        1.275e-03  5.973e-04  7.020e+01   2.135  0.03623 *  

visreg(unComplex.lmerBC9InteractionRelFreqEnv, "logRelFreq",by="Environment", trans= function(x) x^(1/lambda)*1000,  ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)
# the distribution of relfreq across environments does not allow for a valid testing of the effect
# in interaction

# 2. Environment and Accentuaton

unComplex.lmerBC9InteractionCatAccent<- lmer(bc ~ Environment*AccentuationCondition+
                                               LocSpeech + 
                                               PrePause + PrecSegDur+ 
                                               (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC9InteractionCatAccent)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                      8.249e-01  3.519e-03  1.016e+03 234.387  < 2e-16 ***
#   Environmentn#nV                                  3.589e-02  1.881e-03  1.116e+02  19.082  < 2e-16 ***
# Environmentn#V                                  -4.890e-02  2.002e-03  1.657e+02 -24.419  < 2e-16 ***
# AccentuationConditionunaccented                 -1.290e-03  1.394e-03  1.946e+03  -0.925  0.35508    
# LocSpeech                                       -3.506e-03  2.117e-04  1.508e+03 -16.563  < 2e-16 ***
#   PrePausePause                                    4.072e-03  7.639e-04  1.992e+03   5.330 1.09e-07 ***
#   PrecSegDur                                      -3.868e-02  1.527e-02  1.981e+03  -2.533  0.01139 *  
#   Environmentn#nV:AccentuationConditionunaccented -7.358e-03  1.589e-03  1.901e+03  -4.632 3.87e-06 ***
# Environmentn#V:AccentuationConditionunaccented   5.466e-03  1.711e-03  1.914e+03   3.194  0.00143 **

# there is an interaction! Let's have a look

visreg(unComplex.lmerBC9InteractionCatAccent)
visreg(unComplex.lmerBC9InteractionCatAccent, "AccentuationCondition",by="Environment", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.5)

# okay basically it tells us that the difference between the accented unn are even longer than
# the un1accented, wheter there is no difference between un1V in accented and un1accented position

# This is nothing exciting, let's see whether the model with the interacton is better
# than our previous one

anova(unComplex.lmerBC9InteractionRelFreqAccent,unComplex.lmerBC9InteractionCatAccent)

# Data: unComplex2
# Models:
#   object: bc ~ Environment + logRelFreq * AccentuationCondition + LocSpeech + 
#   object:     PrePause + PrecSegDur + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * AccentuationCondition + LocSpeech + PrePause + 
#   ..1:     PrecSegDur + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -11322 -11255 5673.0   -11346                             
# ..1    12 -11398 -11331 5711.1   -11422 76.222      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# so it is better

cor(unComplex2$bc, fitted(unComplex.lmerBC9InteractionCatAccent))^2
#[1] 0.8970399

cor(unComplex2$bc, fitted(unComplex.lmerBC9InteractionRelFreqAccent))^2
#[1] 0.8926444


# What if I put in both interactions?



# 2. Environment and Accentuaton, and RelFreq and Accentuaton

unComplex.lmerBC9InteractionCatAccentFreq<- lmer(bc ~ Environment*AccentuationCondition+logRelFreq*AccentuationCondition+
                                               LocSpeech + 
                                               PrePause + PrecSegDur+ 
                                               (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC9InteractionCatAccentFreq)

# only the onew with environment!

#What about dreifach-Ineraktion

unComplex.lmerBC9InteractionCatAccentFreqAll<- lmer(bc ~ Environment*AccentuationCondition*logRelFreq+
                                                   LocSpeech + 
                                                   PrePause + PrecSegDur+ 
                                                   (1|Item) + (1|Participant), data = unComplex2)

summary(unComplex.lmerBC9InteractionCatAccentFreqAll)

# NO, so the one with environment and accentuation is the "current one"

#3. TypeOfRoot and Accentuaton

unComplex.lmerBC9InteractionRootAccent<- lmer(bc ~ Environment+ TypeOfRoot*AccentuationCondition+
                                                          LocSpeech + 
                                                          PrePause + PrecSegDur+ 
                                                          (1|Item) + (1|Participant), data = unComplex2)
  #no

#4. Environment and stress



unComplex.lmerBC9InteractionCatStress<- lmer(bc ~ Environment*BaseInitialStress+AccentuationCondition+
                                               LocSpeech + 
                                               PrePause + PrecSegDur+ 
                                               (1|Item) + (1|Participant), data = unComplex2)



# Problem there are not all combinations

table(unComplex2$Environment,unComplex2$BaseInitialStress)
#       primary unstressed
# n#C      424          0
# n#nV     907         40
# n#V      312        334

# but there seems to be an effect otherwise
summary(unComplex.lmerBC9InteractionCatStress)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                  8.237e-01  3.499e-03  1.025e+03 235.422  < 2e-16 ***
#   Environmentn#nV                              3.321e-02  1.638e-03  7.540e+01  20.283  < 2e-16 ***
# Environmentn#V                              -4.532e-02  2.013e-03  1.006e+02 -22.517  < 2e-16 ***
# BaseInitialStressunstressed                 -1.478e-03  2.017e-03  6.670e+01  -0.733   0.4661    
# AccentuationConditionunaccented             -3.328e-03  7.641e-04  1.993e+03  -4.355 1.40e-05 ***
#   LocSpeech                                   -3.323e-03  2.153e-04  1.454e+03 -15.431  < 2e-16 ***
#   PrePausePause                                4.280e-03  7.797e-04  1.991e+03   5.490 4.54e-08 ***
#   PrecSegDur                                  -3.977e-02  1.559e-02  1.986e+03  -2.552   0.0108 *  
#   Environmentn#nV:BaseInitialStressunstressed -1.278e-02  5.295e-03  5.230e+01  -2.413   0.0193 *  


visreg(unComplex.lmerBC9InteractionCatStress, "BaseInitialStress",by="Environment" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="stress by environment",ylim=c(0,180), cex.axis=0.8)
visreg(unComplex.lmerBC9InteractionCatStress, "AccentuationCondition",by="Environment" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="accentuation by environment",ylim=c(0,180), cex.axis=0.8)



#5. Stress and Accentuation
unComplex.lmerBC9InteractionAccStress<- lmer(bc ~  Environment+BaseInitialStress*AccentuationCondition
                                             + LocSpeech  + PrePause+
                                                PrecSegDur+ 
                                                (1|Participant)+ (1|Item) , data = unComplex2)


summary(unComplex.lmerBC9InteractionAccStress)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  8.246e-01  3.501e-03  1.032e+03 235.547  < 2e-16 ***
#   Environmentn#nV                                              3.272e-02  1.682e-03  7.720e+01  19.451  < 2e-16 ***
# Environmentn#V                                              -4.432e-02  2.033e-03  1.003e+02 -21.801  < 2e-16 ***
# BaseInitialStressunstressed                                 -7.922e-03  2.096e-03  9.010e+01  -3.779 0.000282 ***
#   AccentuationConditionunaccented                             -5.031e-03  8.128e-04  2.000e+03  -6.190 7.28e-10 ***
#   LocSpeech                                                   -3.347e-03  2.148e-04  1.551e+03 -15.582  < 2e-16 ***
#   PrePausePause                                                4.267e-03  7.735e-04  1.988e+03   5.516 3.91e-08 ***
#   PrecSegDur                                                  -3.787e-02  1.547e-02  1.983e+03  -2.449 0.014425 *  
#   BaseInitialStressunstressed:AccentuationConditionunaccented  9.399e-03  1.592e-03  1.914e+03   5.906 4.15e-09 ***

visreg(unComplex.lmerBC9InteractionAccStress, "BaseInitialStress",by="AccentuationCondition" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="accentuation by environment",ylim=c(0,180), cex.axis=0.8)

# okay makes sense: when accented primary longer


# let's what happens if we put in both interactions


unComplex.lmerBC9InteractionAccStressEnv<- lmer(bc ~  Environment*AccentuationCondition+BaseInitialStress*AccentuationCondition
                                             + LocSpeech  + PrePause+
                                               PrecSegDur+ 
                                               (1|Participant)+ (1|Item) , data = unComplex2)


summary(unComplex.lmerBC9InteractionAccStressEnv)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  8.241e-01  3.529e-03  1.050e+03 233.488  < 2e-16 ***
#   Environmentn#nV                                              3.618e-02  1.864e-03  1.100e+02  19.410  < 2e-16 ***
# Environmentn#V                                              -4.649e-02  2.278e-03  1.487e+02 -20.407  < 2e-16 ***
# AccentuationConditionunaccented                             -1.375e-03  1.394e-03  1.944e+03  -0.986   0.3242    
# BaseInitialStressunstressed                                 -4.698e-03  2.202e-03  1.036e+02  -2.133   0.0353 *  
#   LocSpeech                                                   -3.442e-03  2.133e-04  1.577e+03 -16.138  < 2e-16 ***
#   PrePausePause                                                4.190e-03  7.651e-04  1.986e+03   5.476 4.90e-08 ***
#   PrecSegDur                                                  -3.789e-02  1.527e-02  1.980e+03  -2.482   0.0132 *  
#   Environmentn#nV:AccentuationConditionunaccented             -7.510e-03  1.590e-03  1.900e+03  -4.723 2.49e-06 ***
# Environmentn#V:AccentuationConditionunaccented               3.618e-03  1.988e-03  1.915e+03   1.820   0.0690 .  
# AccentuationConditionunaccented:BaseInitialStressunstressed  3.461e-03  1.946e-03  1.914e+03   1.779   0.0755 .  

#only marginally significant. So let's see which model is the best


anova(unComplex.lmerBC9InteractionAccStress,unComplex.lmerBC9InteractionCatAccent)

# Data: unComplex2
# Models:
#   object: bc ~ Environment + BaseInitialStress * AccentuationCondition + 
#   object:     LocSpeech + PrePause + PrecSegDur + (1 | Participant) + (1 | 
#                                                                          object:     Item)
# ..1: bc ~ Environment * AccentuationCondition + LocSpeech + PrePause + 
#   ..1:     PrecSegDur + (1 | Item) + (1 | Participant)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -11350 -11283 5687.3   -11374                             
# ..1    12 -11398 -11331 5711.1   -11422 47.669      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# the one with environment and acc is better


anova(unComplex.lmerBC9InteractionAccStressEnv,unComplex.lmerBC9InteractionCatAccent)

# Data: unComplex2
# Models:
#   ..1: bc ~ Environment * AccentuationCondition + LocSpeech + PrePause + 
#   ..1:     PrecSegDur + (1 | Item) + (1 | Participant)
# object: bc ~ Environment * AccentuationCondition + BaseInitialStress * 
#   object:     AccentuationCondition + LocSpeech + PrePause + PrecSegDur + 
#   object:     (1 | Participant) + (1 | Item)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    12 -11398 -11331 5711.1   -11422                           
# object 14 -11400 -11321 5713.8   -11428 5.4658      2    0.06503 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# the second one is not better (only marginaly significant), i.e. the one
# with the Environment/Accentuation interaction is the best model


unComplex.lmerBC9InteractionAccEnv2<- lmer(bc ~  Environment*AccentuationCondition+BaseInitialStress+LocSpeech+ PrePause+                                                + LocSpeech  + PrePause+
                                                  PrecSegDur+ 
                                                  (1|Participant)+ (1|Item) , data = unComplex2)


summary(unComplex.lmerBC9InteractionAccEnv2)
# no

# Last:Rating and Acc


unComplex.lmerBC9InteractionAccRat<- lmer(bc ~  Environment*AccentuationCondition*Rating+LocSpeech+ PrePause+                                                + LocSpeech  + PrePause+
                                             PrecSegDur+ 
                                             (1|Participant)+ (1|Item) , data = unComplex2)


summary(unComplex.lmerBC9InteractionAccRat)


# no


##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# I tested teh interactions of variables of interst (RelFreq, Type of Base, Rating,Environment) and Accentuation
# also tested stress and accentuation
# stress and environment could not be tested - not all combnations! no n#c wirh unstressed ....

# Our final model has one interaction. Accentuation*Catgoty


visreg(unComplex.lmerBC9InteractionCatAccent, "AccentuationCondition",by="Environment" ,trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", xlab="accentuation by environment",ylim=c(0,180), cex.axis=0.8)


# for unn we can see a length difference between accented and un1accented,
# for unV and unC there is no length difference betweenaccented and un1accented 
# (could be due to the fact that there is "more material" in doubles)





#############################################################
# The final model:

summary(unComplex.lmerBC9InteractionCatAccent)

# Fixed effects:
#                                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                      8.249e-01  3.519e-03  1.016e+03 234.387  < 2e-16 ***
#   Environmentn#nV                                  3.589e-02  1.881e-03  1.116e+02  19.082  < 2e-16 ***
#   Environmentn#V                                  -4.890e-02  2.002e-03  1.657e+02 -24.419  < 2e-16 ***
#   AccentuationConditionunaccented                 -1.290e-03  1.394e-03  1.946e+03  -0.925  0.35508    
#   LocSpeech                                       -3.506e-03  2.117e-04  1.508e+03 -16.563  < 2e-16 ***
#   PrePausePause                                    4.072e-03  7.639e-04  1.992e+03   5.330 1.09e-07 ***
#   PrecSegDur                                      -3.868e-02  1.527e-02  1.981e+03  -2.533  0.01139 *  
#   Environmentn#nV:AccentuationConditionunaccented -7.358e-03  1.589e-03  1.901e+03  -4.632 3.87e-06 ***
#   Environmentn#V:AccentuationConditionunaccented   5.466e-03  1.711e-03  1.914e+03   3.194  0.00143 **
lambda
#[1] 0.1010101


# I need to rename some variabels for the plot...

unComplex2<-rename(unComplex2,PrecedingSegmentDuration=PrecSegDur)

unComplex2<-rename(unComplex2,AccentuationAnnotator=Accentuation)

unComplex2<-rename(unComplex2,Accentuation=AccentuationCondition)

levels(unComplex2$PrePause)
#[1] "No Pause" "Pause"   

levels(unComplex2$PrePause)<-c("no pause","pause")


final_un_complex_model.lmer<-lmer(bc ~  Environment*Accentuation+ LocSpeech+ PrePause+                                                + LocSpeech  + PrePause+
                                    PrecedingSegmentDuration+ 
                                    (1|Participant)+ (1|Item) , data = unComplex2)


summary(final_un_complex_model.lmer)


# Fixed effects:
#                                         Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                             8.249e-01  3.519e-03  1.016e+03 234.387  < 2e-16 ***
# Environmentn#nV                         3.589e-02  1.881e-03  1.116e+02  19.082  < 2e-16 ***
# Environmentn#V                         -4.890e-02  2.002e-03  1.657e+02 -24.419  < 2e-16 ***
# Accentuationunaccented                 -1.290e-03  1.394e-03  1.946e+03  -0.925  0.35508    
# LocSpeech                              -3.506e-03  2.117e-04  1.508e+03 -16.563  < 2e-16 ***
# PrePausepause                           4.072e-03  7.639e-04  1.992e+03   5.330 1.09e-07 ***
# PrecedingSegmentDuration               -3.868e-02  1.527e-02  1.981e+03  -2.533  0.01139 *  
# Environmentn#nV:Accentuationunaccented -7.358e-03  1.589e-03  1.901e+03  -4.632 3.87e-06 ***
# Environmentn#V:Accentuationunaccented   5.466e-03  1.711e-03  1.914e+03   3.194  0.00143 ** 

#############################################################
# Let's now look at each factors contribution to the model
###############################################################

############################################################
# Do we need random effects?
#############################################

# Speaker

unComplex.finalWithoutSpeaker <-lmer(bc ~  Environment*Accentuation+ LocSpeech+ PrePause+                                                + LocSpeech  + PrePause+
                                          PrecedingSegmentDuration+ 
                                          (1|Item) , data = unComplex2)

summary(unComplex.finalWithoutSpeaker)


# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                             8.338e-01  3.421e-03  1.051e+03 243.717  < 2e-16 ***
#   Environmentn#nV                         3.612e-02  1.986e-03  9.950e+01  18.184  < 2e-16 ***
#   Environmentn#V                         -4.582e-02  1.984e-03  1.164e+02 -23.100  < 2e-16 ***
#   Accentuationunaccented                  5.989e-05  1.528e-03  1.966e+03   0.039  0.96873    
#   LocSpeech                              -4.417e-03  2.055e-04  1.707e+03 -21.494  < 2e-16 ***
#   PrePausepause                           2.317e-03  7.527e-04  1.990e+03   3.078  0.00211 ** 
#   PrecedingSegmentDuration               -3.474e-02  1.503e-02  1.983e+03  -2.311  0.02092 *  
#   Environmentn#nV:Accentuationunaccented -7.129e-03  1.771e-03  1.947e+03  -4.025 5.92e-05 ***
#   Environmentn#V:Accentuationunaccented   5.916e-03  1.901e-03  1.954e+03   3.111  0.00189 ** 

cor(unComplex2$bc, fitted(unComplex.finalWithoutSpeaker))^2
#[1] 0.8683155



unComplex.finalWithoutItem <-lmer(bc ~  Environment*Accentuation+ LocSpeech+ PrePause+                                                + LocSpeech  + PrePause+
                                       PrecedingSegmentDuration+ 
                                       (1|Participant) , data = unComplex2)

summary(unComplex.finalWithoutItem)

# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                             8.193e-01  3.204e-03  1.402e+03 255.743  < 2e-16 ***
#   Environmentn#nV                         3.634e-02  1.274e-03  2.008e+03  28.533  < 2e-16 ***
#   Environmentn#V                         -4.903e-02  1.519e-03  1.901e+03 -32.283  < 2e-16 ***
#   Accentuationunaccented                 -2.261e-03  1.453e-03  1.974e+03  -1.556  0.11990    
#   LocSpeech                              -3.095e-03  1.851e-04  2.007e+03 -16.718  < 2e-16 ***
#   PrePausepause                           4.337e-03  7.861e-04  2.007e+03   5.517 3.89e-08 ***
#   PrecedingSegmentDuration               -2.567e-02  1.583e-02  2.007e+03  -1.622  0.10490    
#   Environmentn#nV:Accentuationunaccented -7.152e-03  1.673e-03  1.960e+03  -4.276 2.00e-05 ***
#   Environmentn#V:Accentuationunaccented   5.477e-03  1.796e-03  1.963e+03   3.049  0.00233 ** 

cor(unComplex2$bc, fitted(unComplex.finalWithoutItem))^2
#[1] 0.8821893



anova(unComplex.finalWithoutItem,final_un_complex_model.lmer)

# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11285 -11223 5653.3   -11307                             
# ..1    12 -11398 -11331 5711.1   -11422 115.55      1  < 2.2e-16 ***

anova(unComplex.finalWithoutSpeaker,final_un_complex_model.lmer)

# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11081 -11019 5551.4   -11103                             
# ..1    12 -11398 -11331 5711.1   -11422 319.44      1  < 2.2e-16 ***




# Now, let's see how much each factor explains - we will take a look at the ACI for that

# Let's create models in which one of the preditor variables is missing



unComplex.finalWithoutEnvironment<-lmer(bc ~ Accentuation+ LocSpeech+ PrePause+                                                
                                    PrecedingSegmentDuration+ (1|Item)+
                                    (1|Participant) , data = unComplex2)



unComplex.finalWithoutInteraction <-lmer(bc ~  Environment+Accentuation+LocSpeech+ PrePause+                                                
                                            PrecedingSegmentDuration+ (1|Item)+
                                            (1|Participant) , data = unComplex2)

unComplex.finalWithoutLocSpeech <-lmer(bc ~  Environment*Accentuation+ PrePause+                                                
                                            PrecedingSegmentDuration+ (1|Item)+
                                            (1|Participant) , data = unComplex2)

unComplex.finalWithoutPrepause <-lmer(bc ~   Environment*Accentuation+ LocSpeech+ PrecedingSegmentDuration+
                                        (1|Item)+
                                        (1|Participant) , data = unComplex2)

unComplex.finalWithoutPrecedingSeg <-lmer(bc ~  Environment*Accentuation+ LocSpeech+ PrePause+
                                             (1|Item)+
                                            (1|Participant) , data = unComplex2)


unComplex.finalWithoutAcc <-lmer(bc ~  Environment+ LocSpeech+ PrePause+ PrecedingSegmentDuration+
                                            (1|Item)+
                                            (1|Participant) , data = unComplex2)

  ###########################################################################
# Now, let's have a look at the contribution of each factor
###################################################################


anova(unComplex.finalWithoutSpeaker,final_un_complex_model.lmer)
#         Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11081 -11019 5551.4   -11103                             
# ..1    12 -11398 -11331 5711.1   -11422 319.44      1  < 2.2e-16 ***

11398-11081
#[1] 317

anova(unComplex.finalWithoutItem,final_un_complex_model.lmer)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11285 -11223 5653.3   -11307                             
# ..1    12 -11398 -11331 5711.1   -11422 115.55      1  < 2.2e-16 ***

11398-11285 
#113

anova(unComplex.finalWithoutInteraction,final_un_complex_model.lmer)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -11317 -11261 5668.5   -11337                             
# ..1    12 -11398 -11331 5711.1   -11422 85.219      2  < 2.2e-16 ***
11398-11317
#81



anova(unComplex.finalWithoutEnvironment,final_un_complex_model.lmer)
# Df    AIC    BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object  8 -11067 -11023 5541.7   -11083                                        
# ..1    12 -11398 -11331 5711.1   -11422 338.76      4 < 0.00000000000000022 ***

11398-11067 
#331



anova(unComplex.finalWithoutLocSpeech,final_un_complex_model.lmer)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -11144 -11082 5583.1   -11166                             
# ..1    12 -11398 -11331 5711.1   -11422 256.03      1  < 2.2e-16 ***
11398-11144 
#254

anova(unComplex.finalWithoutPrecedingSeg,final_un_complex_model.lmer)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)  
# object 11 -11394 -11332 5707.9   -11416                          
# ..1    12 -11398 -11331 5711.1   -11422  6.36      1    0.01167 *
11398-11394 
#4

anova(final_un_complex_model.lmer,unComplex.finalWithoutPrepause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    11 -11372 -11310 5697.0   -11394                             
# object 12 -11398 -11331 5711.1   -11422 28.241      1  1.071e-07 ***
11398-11372
# 26

anova(final_un_complex_model.lmer,unComplex.finalWithoutAcc)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# ..1     9 -11301 -11251 5659.7   -11319                            
# object 12 -11398 -11331 5711.1   -11422 102.8      3  < 2.2e-16 ***
11398-11301
# 97


########################################################
# When we look at the contribution of each factor in the model 
# we see the following picture
#######################################################################


# Let's put these numbers in a table

AIC_decrease_unComplex<-matrix(c(331, 317,254,113, 97,81,26,4),ncol=8,byrow=TRUE)
colnames(AIC_decrease_unComplex)<-c("Environment", "Speaker", "Local-\nSpeechRate", "Item","Accentuation", "Env. * Acc.", "PrePause",
                                    "Preceding-\nSegmentDuration")
rownames(AIC_decrease_unComplex)<-c("Decrease in AIC")
AIC_decrease_unComplex <- as.table(AIC_decrease_unComplex)
AIC_decrease_unComplex


#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



# plot effect sizes


png("AICdecreaseUnComplex.png", units="cm", height=10, width=17, res=300, pointsize=09)


par(mar=c(2.6,8.1, 1.1, 2), xpd=TRUE, cex=0.9)

barplot((AIC_decrease_unComplex),horiz=T, col="lightgrey",  names.arg =colnames(AIC_decrease_unComplex), las=2, xaxt="n",border="lightgrey")

xx<-barplot(AIC_decrease_unComplex, horiz=T, col="lightgrey",names.arg =colnames(AIC_decrease_unComplex), las=2, xaxt="n",border="lightgrey")

text(y = xx, x = AIC_decrease_unComplex ,label = AIC_decrease_unComplex, pos = 4, cex = 0.8, col = "black")

title(xlab="AIC increase", line=0, cex.lab=1.1)

dev.off()

# what is lambda


lambda
#[1] 0.1010101


# so we can see that Environment is the most important factor!!!


##############################
# We should also plot all the effects

#################################
# plotting covariates

# I will not include these plots in the dissertation! 


ylim=c(20,180)
# 
# png("unModelSpeechRate.png", units="cm", height=15, width=15, res=300, pointsize=15)
# 
# par(mfrow=c(1,1))
# visreg(final_un_complex_model.lmer, "LocSpeech", trans= function(x) (x^(1/0.1010101))*1000,alpha = 0.05, rug=F, ylab="duration in milliseconds", xlab="local speech rate",ylim=ylim, cex.axis=0.9)
# 
# dev.off()
# 
# png("unModelPrePause.png", units="cm", height=15, width=15, res=300, pointsize=15)
# 
# par(mfrow=c(1,1))
# visreg(final_un_complex_model.lmer, "PrePause", trans= function(x) (x^(1/0.1010101))*1000, rug=F,band=TRUE,xlab="pause before item",ylab=" ",ylim=ylim, cex.axis=0.9)
# 
# dev.off()
# 
# png("unModelPrecSegDur.png", units="cm", height=15, width=15, res=300, pointsize=15)
# 
# par(mfrow=c(1,1))
# visreg(final_un_complex_model.lmer, "PrecedingSegmentDuration", trans= function(x) (x^(1/0.1010101))*1000, rug=F, ylab="duration in milliseconds", xlab="preceding segment duration",ylim=ylim, cex.axis=0.9)
# 
# dev.off()


###############################
# Plot main effect

# we need n#nV als reference levels

unComplex2$Environment <- relevel (unComplex2$Environment, ref= "n#nV")

# redo the final model

final_un_complex_model.lmer2<-lmer(bc ~  Environment*Accentuation+ LocSpeech+ PrePause+
                                     PrecedingSegmentDuration+(1|Item)+
                                     (1|Participant) , data = unComplex2)

summary(final_un_complex_model.lmer2)

# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                            8.608e-01  3.253e-03  9.325e+02 264.576  < 2e-16 ***
#   Environmentn#C                        -3.589e-02  1.881e-03  1.116e+02 -19.082  < 2e-16 ***
#   Environmentn#V                        -8.479e-02  1.764e-03  1.046e+02 -48.059  < 2e-16 ***
#   Accentuationunaccented                -8.648e-03  9.744e-04  1.976e+03  -8.875  < 2e-16 ***
#   LocSpeech                             -3.506e-03  2.117e-04  1.508e+03 -16.563  < 2e-16 ***
#   PrePausepause                          4.072e-03  7.639e-04  1.992e+03   5.330 1.09e-07 ***
#   PrecedingSegmentDuration              -3.868e-02  1.527e-02  1.981e+03  -2.533   0.0114 *  
#   Environmentn#C:Accentuationunaccented  7.358e-03  1.589e-03  1.901e+03   4.632 3.87e-06 ***
#   Environmentn#V:Accentuationunaccented  1.282e-02  1.394e-03  1.912e+03   9.199  < 2e-16 ***


# png("unModelInterCatAcc.png", units="cm", height=14, width=16, res=300, pointsize=15)
# 
# par <- trellis.par.get()
# 
# #par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
# #par$plot.line$col <- default # das ist die Farbe der Estimate-Linie 
# #par$strip.border<-1
# par$fontsize <- list(text=15) # das ist glaub ich logisch :)
# par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
# par$panel.background$col <- "white" # das macht das weiß im plot weg
# 
# visreg(final_un_complex_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/0.1010101))*1000, rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)
# 
# dev.off()

png("unModelInterCatAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)


visreg(final_un_complex_model.lmer2, "Environment",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/0.1010101))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


dev.off()

library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 


unComplex.lm2<-lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause + PrecedingSegmentDuration + logRelFreq, 
   data = unComplex2)

model_ranking <- dredge(unComplex.lm2)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Relative variable importance: 
#                 Environment LocSpeech PrePause Accentuation logWordFormFreq BaseInitialStress
# Importance:          1.00        1.00      1.00     1.00         0.97            0.88             
# N containing models: 1024        1024      1024     1024         1024            1024             
#                PostPause PrecedingSegmentDuration OrderRescale logRelFreq GlobalSpeechRate
# Importance:          0.66      0.61                     0.41         0.31       0.30            
# N containing models: 1024      1024                     1024         1024       1024  

# I need the output in Latex

table_final_models<-as.data.frame(coef(summary(final_un_complex_model.lmer2)))

xtable(table_final_models,digits = 6)


# So, this is our final model: The effects are
#   1. Doubles longer than singles --> un geminates: This effect is even stronger when the word is accented
#   2. The longer the word, the longer the consonant and the more segments in the word,
#      the shorter the consonant (speech rate as a phonetic effect)
#   3. After a pause /n/ is longer
#  4. The longer the preceding segment, the shorter the consonant

# let's check a MuMin with interactions

options(na.action = "na.fail") 


unComplex.lm3<-lm(ConsonantDur ~ Environment*Accentuation*BaseInitialStress+ PrePause*Accentuation*BaseInitialStress+Environment*logRelFreq + 
                    OrderRescale + logWordFormFreq +  LocSpeech + 
                    GlobalSpeechRate +  PostPause + PrecedingSegmentDuration , 
                  data = unComplex2)

model_ranking2 <- dredge(unComplex.lm3)

model_average_2<-model.avg(model_ranking2)


summary(model_average_2)

# Relative variable importance: 
#                  Accentuation Environment LocSpeech Accentuation:Environment BaseInitialStress
# Importance:           1.00         1.00        1.00      1.00                     1.00            
# N containing models: 12544        12288        7360      5952                    12544            
#                      BaseInitialStress:Environment PrePause logWordFormFreq logRelFreq Environment:logRelFreq PostPause
# Importance:           1.00                          1.00     0.97            0.93       0.90                   0.84    
# N containing models:  5952                         11392     7360            9408       4096                   7360    
#             BaseInitialStress:PrePause Accentuation:BaseInitialStress OrderRescale GlobalSpeechRate
# Importance:           0.75                       0.67                           0.62         0.55           
# N containing models:  5568                       6528                           7360         7360           
# PrecedingSegmentDuration Accentuation:PrePause Accentuation:BaseInitialStress:Environment
# Importance:           0.54                     0.48                  0.33                                     
# N containing models:  7360                     5568                  1152                                     
# Accentuation:BaseInitialStress:PrePause
# Importance:           0.09                                  
# N containing models:  1088    

###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

options(scipen=0)

summary(final_un_complex_model.lmer2)
#   (Intercept)                           8.61e-01   3.25e-03  9.33e+02  264.58  < 2e-16 ***
#   Environmentn#C                        -3.59e-02   1.88e-03  1.12e+02  -19.08  < 2e-16 ***
#   Environmentn#V                        -8.48e-02   1.76e-03  1.05e+02  -48.06  < 2e-16 ***
#   Accentuationunaccented                -8.65e-03   9.74e-04  1.98e+03   -8.88  < 2e-16 ***
#   LocSpeech                             -3.51e-03   2.12e-04  1.51e+03  -16.56  < 2e-16 ***
#   PrePausepause                          4.07e-03   7.64e-04  1.99e+03    5.33  1.1e-07 ***
#   PrecedingSegmentDuration              -3.87e-02   1.53e-02  1.98e+03   -2.53    0.011 *  
#   Environmentn#C:Accentuationunaccented  7.36e-03   1.59e-03  1.90e+03    4.63  3.9e-06 ***
#   Environmentn#V:Accentuationunaccented  1.28e-02   1.39e-03  1.91e+03    9.20  < 2e-16 ***

visreg(final_un_complex_model.lmer2)

# Conditions used in construction of plot


# Conditions used in construction of plot
# Accentuation: accented
# LocSpeech: 10.8
# PrePause: pause
# PrecedingSegmentDuration: 0.0837
# Item: unnumbered
# Participant: Experiment_1_participant_17
# Environment: n#nV



intercept =  8.61e-01
LocCondition= 10.8
estSpeech= -3.51e-03
PrecCondition = -3.87e-02
estPrec= -0.038683

# PrePause: pause
estPause= 4.07e-03

# Accentuation: accented
estUnaccented=-8.65e-03

EstEnvironmentunV= -8.48e-02
EstEnvironmentunC= -3.59e-02

EstAccentuationunVunaccented= 1.28e-02
EstAccentuationunCunaccented= 7.36e-03


ranef(final_un_complex_model.lmer2)$Participant
# Participant:Experiment_1_participant_17
EstParticipant=0.002737

ranef(final_un_complex_model.lmer2)$Item
# Item: unnumbered
EstItem=6.17e-03

visreg(final_un_complex_model.lmer2, "Environment",by="Accentuation",ylab="duration in milliseconds", trans= function(x) (x^(1/0.1010101))*1000, rug=F, xlab="environment by accentuation",ylim=ylim,cex.axis=0.9,par.settings=par)

#levels unV unaccented

((intercept+(LocCondition*estSpeech)+(PrecCondition*estPrec)+
    estUnaccented+EstEnvironmentunV+EstAccentuationunVunaccented)^(1/lambda))*1000
#[1] 53.5

unVUnaccented=53.5

#levels unC unaccented

((intercept+(LocCondition*estSpeech)+(PrecCondition*estPrec)+
    estUnaccented+EstEnvironmentunC+EstAccentuationunCunaccented)^(1/lambda))*1000
#[1] 93.8

unCUnaccented=93.8


#levels unC accented

((intercept+(LocCondition*estSpeech)+(PrecCondition*estPrec)+
    EstEnvironmentunC)^(1/lambda))*1000
#95.4

unCAccented=95.4

#levels unV accented

((intercept+(LocCondition*estSpeech)+(PrecCondition*estPrec)+
    EstEnvironmentunV)^(1/lambda))*1000

#50.6

unVAccented=50.6

#levels unn unaccented

((intercept+(LocCondition*estSpeech)+
    estUnaccented+(PrecCondition*estPrec))^(1/lambda))*1000

#133

unnUnaccented=133


#levels unn accented

((intercept+(LocCondition*estSpeech)+
    (PrecCondition*estPrec))^(1/lambda))*1000

#148

unnAccented=148

######################################
# Unterschiede ausrechenen

# Double single accented

unnAccented- unVAccented
#97.4

unnAccented- unCAccented
#52.6


# Double single unaccented

unnUnaccented- unVUnaccented
#79.5

unnUnaccented- unCUnaccented
#39.2

# Singletons

# accented

unVAccented- unCAccented
#[1] -44.8

# unaccented

unVUnaccented- unCUnaccented
#[1] -40.3
