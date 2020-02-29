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



imComplex <- read.csv("imComplex.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(imComplex)

# 'data.frame':	1177 obs. of  84 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Item                       : Factor w/ 47 levels "immaculate","immaterial",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 29 levels "Experiment_1_participant_10",..: 3 14 21 17 5 29 11 9 6 26 ...
# $ ID                         : int  442 2149 3423 2655 918 5437 3740 1524 949 4679 ...
# $ Filename                   : Factor w/ 1177 levels "Participant_10_108.TextGrid",..: 105 522 816 639 202 1139 459 342 207 1008 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 2 levels "Accented","Unaccented": 2 2 2 1 2 2 1 2 1 2 ...
# $ Annotator                  : Factor w/ 5 levels "Lara","Mandy",..: 2 5 2 5 2 5 1 3 2 2 ...
# $ Order                      : int  117 58 72 3 293 18 299 64 32 8 ...
# $ WordDur                    : num  0.75 0.52 0.655 0.774 0.546 ...
# $ SyllNum                    : int  4 4 4 4 3 4 4 4 4 4 ...
# $ SegNum                     : int  9 8 9 9 8 9 9 9 9 8 ...
# $ ConsonantDur               : num  0.087 0.0958 0.076 0.1299 0.0839 ...
# $ PrecSeg                    : Factor w/ 6 levels "@","{","e","i",..: 5 5 5 5 5 5 5 5 4 5 ...
# $ PrecSegVC                  : Factor w/ 1 level "V": 1 1 1 1 1 1 1 1 1 1 ...
# $ PrecSegDur                 : num  0.0507 0.0752 0.0459 0.0507 0.0642 ...
# $ FollSeg                    : Factor w/ 37 levels "?","@","@e","@U",..: 5 5 5 5 9 5 5 5 9 5 ...
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
# $ Environment                : Factor w/ 2 levels "m#C","m#mV": 2 2 2 2 2 2 2 2 2 2 ...
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
# $ StressPattern              : Factor w/ 5 levels "primary-unstressed",..: 4 4 4 4 4 4 4 4 4 4 ...
# ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ ConsonantDurMS             : num  87 95.8 76 129.9 83.9 ...
# $ PrePause                   : Factor w/ 2 levels "no pause","pause": 1 1 1 1 2 2 2 2 2 1 ...
# $ PostPause                  : Factor w/ 2 levels "no pause","pause": 2 1 1 2 1 2 2 2 2 1 ...
# $ GlobalSpeechRate           : num  1.97 3.8 2.6 2.16 1.88 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ StressPattern              : Factor w/ 5 levels "primary-unstressed",..: 4 4 4 4 4 4 4 4 4 4 ...


imComplex$X.1<-NULL
imComplex$X<-NULL

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
# - SemanticTransparency in model in which affixed words are compared)
# - Affix in which affixed words are compared)
# - Rating in model in which affixed words are compared)
# Let's see whether it males sense to include the decomposability measures (only
# makes sense if we have variability)

########################################################################
# The decomposability measures(I need to analyze them and their relations
# in a separate analysis before I can decide how to procede...)

# probabaly influence of each individual variable and the PC

# 1. Semantic Transparency

table(imComplex$SemanticTransparency)
# opaque transparent 
# 206         971 




# 2. Type of Base

table(imComplex$TypeOfBase)
# bound  word 
# 109  1068 


table(imComplex$TypeOfBase,imComplex$SemanticTransparency)
#         opaque transparent
# bound    109           0
# word      97         971


#yeah....


# 3. Rating

table(imComplex$Rating)
# 1    2    3    4 
# 702 182 134 157 




table(imComplex$Rating,imComplex$TypeOfBase)
#   bound word
# 1    13  689
# 2     7  175
# 3    18  116
# 4    71   86


(table(imComplex$Rating,imComplex$SemanticTransparency))
#     opaque transparent
# 1     47         655
# 2     28         154
# 3     39          95
# 4     91          66


pairscor.fnc(imComplex [, c("SemanticTransparency", "TypeOfBase","Rating","logRelFreq","Affix")])

# we find high correlations, so we will test each variable's influence individually
# and do a PC: We will do that later


######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = imComplex)
cor(imComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.2249228


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = imComplex)
cor(imComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.1631191

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = imComplex)
cor(imComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.3796132

# so aroim1d 38 percent of the variability can be explained by this! That's a lot

##########################################################
##              Do an initial model:
imComplex$OrderRescale<-imComplex$Order*0.1

imComplex.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + PrecSegDur+
                          (1|Item) + (1|Participant), data = imComplex)


summary(imComplex.lmer1)    

# Linear mixed model fit by REML 
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + PrecSegDur + (1 | Item) + (1 | Participant)
# Data: imComplex
# 
# REML criterion at convergence: -5685.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8715 -0.6053 -0.0697  0.4647  5.9011 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 8.855e-05 0.009410
# Participant (Intercept) 7.031e-05 0.008385
# Residual                3.733e-04 0.019322
# Number of obs: 1177, groups:  Item, 47; Participant, 29
# 
# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      1.589e-01  7.460e-03  5.980e+02  21.303  < 2e-16 ***
#   Environmentm#mV                  4.224e-03  3.158e-03  4.350e+01   1.338  0.18800    
#   AccentuationConditionunaccented  5.283e-03  1.859e-03  9.341e+02   2.841  0.00459 ** 
#   OrderRescale                    -3.273e-05  6.356e-05  1.124e+03  -0.515  0.60673    
#   logWordFormFreq                  2.212e-04  6.499e-04  4.360e+01   0.340  0.73520    
#   BaseInitialStressunstressed     -6.926e-03  3.276e-03  4.390e+01  -2.114  0.04019 *  
#   LocSpeech                       -3.784e-03  3.818e-04  9.818e+02  -9.911  < 2e-16 ***
#   GlobalSpeechRate                -1.031e-02  2.201e-03  7.809e+02  -4.683 3.33e-06 ***
#   PrePausepause                    7.606e-04  1.473e-03  1.140e+03   0.516  0.60577    
#   PostPausepause                   3.501e-04  1.636e-03  1.145e+03   0.214  0.83055    
#   PrecSegDur                      -7.594e-03  3.167e-02  1.145e+03  -0.240  0.81057    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envr#V AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsps PstPsp
# Envrnmntm#V -0.089                                                               
# AccnttnCndt  0.309  0.022                                                        
# OrderRescal -0.221 -0.001 -0.053                                                 
# lgWrdFrmFrq -0.206 -0.247  0.048 -0.009                                          
# BsIntlStrss -0.047  0.136 -0.015  0.000 -0.173                                   
# LocSpeech   -0.465  0.072 -0.124  0.032 -0.017 -0.054                            
# GloblSpchRt -0.505 -0.082 -0.535  0.078 -0.066  0.008 -0.301                     
# PrePausepas -0.311 -0.040 -0.062  0.007  0.011 -0.082  0.016  0.276              
# PostPauseps -0.349 -0.010  0.237 -0.007 -0.032 -0.010  0.072  0.264 -0.059       
# PrecSegDur  -0.481 -0.020 -0.021  0.067  0.009 -0.080  0.262  0.069  0.069  0.060

cor(imComplex$ConsonantDur, fitted(imComplex.lmer1))^2
#[1] 0.5015761


#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own


# 1.logWordFormFreq & logRelFreq

# Model woth both 
imComplex.lmerFrequencies <- lmer(ConsonantDur ~ logWordFormFreq+ logRelFreq+ (1|Item) + (1|Participant), data = imComplex)

summary(imComplex.lmerFrequencies)    


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      0.0923928  0.0037498 69.5300000  24.639   <2e-16 ***
#   logWordFormFreq -0.0001909  0.0006549 45.2600000  -0.291    0.772    
# logRelFreq      -0.0008319  0.0006640 44.2000000  -1.253    0.217   


cor(imComplex$ConsonantDur, fitted(imComplex.lmerFrequencies))^2
#[1] 0.3795251


# only Word Form Freq

imComplex.lmerWordFrequency <- lmer(ConsonantDur ~ logWordFormFreq + (1|Item) + (1|Participant), data = imComplex)

summary(imComplex.lmerWordFrequency)    
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      0.0934161  0.0036743 69.7500000  25.424   <2e-16 ***
#   logWordFormFreq -0.0003932  0.0006388 46.2000000  -0.616    0.541   

cor(imComplex$ConsonantDur, fitted(imComplex.lmerWordFrequency))^2
#[1] 0.3796467


# only RelFreq
imComplex.lmerRelFrequency <- lmer(ConsonantDur ~ logRelFreq+  (1|Item) + (1|Participant), data = imComplex)

summary(imComplex.lmerRelFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  0.0916643  0.0027859 44.0100000  32.903   <2e-16 ***
#   logRelFreq  -0.0008797  0.0006370 45.0900000  -1.381    0.174   
# ---

cor(imComplex$ConsonantDur, fitted(imComplex.lmerRelFrequency))^2
#[1]0.3794485



# So, WordFormFreq is never a significant predictor, neither is RelFreq. There is
# no supression effect. Thus, we do not need to worry about it. However, it would
# be intersting to see whether a categorical variable of WordFormFreq might have 
# an effect

# only Categorical WordFormFreq

imComplex.lmerWordFreqCategorical <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Item) + (1|Participant), data = imComplex)

summary(imComplex.lmerWordFreqCategorical)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                 0.089573   0.003320 61.890000  26.978   <2e-16 ***
#   WordFreqCategoricalLowFreq  0.002183   0.003835 45.290000   0.569    0.572    
# WordFreqCategoricalMidFreq  0.005398   0.003509 44.280000   1.538    0.131  

# No effect! However, one thing I would also like to check is whether the presense of Item
# as a random effect has an influence

# Let us see what happen if we have a model without a random effect for Item and bith Variables

imComplex.lmerFrequenciesWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ logWordFormFreq+ (1|Participant), data = imComplex)

summary(imComplex.lmerFrequenciesWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      9.243e-02  2.715e-03  4.150e+01  34.041  < 2e-16 ***
#   logRelFreq      -8.652e-04  2.958e-04  1.146e+03  -2.925  0.00352 ** 
#   logWordFormFreq -1.745e-04  2.992e-04  1.148e+03  -0.583  0.55975    

cor(imComplex$ConsonantDur, fitted(imComplex.lmerFrequenciesWithoutItem))^2
#[1] 0.2317319


# RelFreq without Item
imComplex.lmerRelFrequencyWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ (1|Participant), data = imComplex)

summary(imComplex.lmerRelFrequencyWithoutItem)

# Fixed effects:
#Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  9.174e-02  2.442e-03  2.730e+01  37.571   <2e-16 ***
#   logRelFreq  -9.094e-04  2.859e-04  1.147e+03  -3.182   0.0015 ** 

cor(imComplex$ConsonantDur, fitted(imComplex.lmerRelFrequencyWithoutItem))^2
#[1]0.2315113

# WordFreq qithout Item

imComplex.lmerWordFreqWithoutItem <- lmer(ConsonantDur ~ logWordFormFreq+ (1|Participant), data = imComplex)

summary(imComplex.lmerWordFreqWithoutItem)

# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      9.358e-02  2.695e-03  3.970e+01  34.728   <2e-16 ***
#   logWordFormFreq -3.989e-04  2.901e-04  1.148e+03  -1.375    0.169    

cor(imComplex$ConsonantDur, fitted(imComplex.lmerWordFreqWithoutItem))^2
#[1] 0.2261615



imComplex.lmerWordFreqCategoricalWiithoutItem <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Participant), data = imComplex)

summary(imComplex.lmerWordFreqCategoricalWiithoutItem)

# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                8.962e-02  2.569e-03 3.280e+01   34.89  < 2e-16 ***
#   WordFreqCategoricalLowFreq 1.858e-03  1.769e-03 1.147e+03    1.05 0.293772    
# WordFreqCategoricalMidFreq 5.957e-03  1.576e-03 1.146e+03    3.78 0.000165 ***

cor(imComplex$ConsonantDur, fitted(imComplex.lmerWordFreqCategoricalWiithoutItem))^2
#[1] 0.2345427

#####################################
# Summary Coll. Frequencies:
#- Both measures only becomes significant when the random effect of Item is
# removed. The models become much worse then
# - There is no supression effect.
#################################################


# 2.  Loc Speech  and/or Global Speech


cor.test(imComplex$LocSpeech,imComplex$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  imComplex$LocSpeech and imComplex$GlobalSpeechRate
# t = 22.97, df = 1175, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.5159523 0.5948977
# sample estimates:
# cor 
# 0.5566806 

imComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.514e-01  4.137e-03  4.094e+02  36.604  < 2e-16 ***
#   LocSpeech        -3.694e-03  3.642e-04  1.004e+03 -10.141  < 2e-16 ***
#   GlobalSpeechRate -6.553e-03  1.524e-03  1.107e+03  -4.299 1.86e-05 ***
#   ---
cor(imComplex$ConsonantDur, fitted(imComplex.lmerSpeechRates))^2
#[1]  0.4980444



imComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.475e-01  4.089e-03  3.754e+02   36.07   <2e-16 ***
#   LocSpeech   -4.673e-03  2.855e-04  1.161e+03  -16.36   <2e-16 ***

cor(imComplex$ConsonantDur, fitted(imComplex.lmerLocSpeech))^2
#[1] 0.4911797


options(scipen=999)
imComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = imComplex)

print(summary(imComplex.lmerGlobalSpeech),digits=6)

# Fixed effects:
#   Estimate    Std. Error            df  t value               Pr(>|t|)    
# (Intercept)         0.13051835    0.00382936  255.17800000  34.0836 < 0.000000000000000222 ***
#   GlobalSpeechRate   -0.01624980    0.00123800 1108.17000000 -13.1258 < 0.000000000000000222 ***


cor(imComplex$ConsonantDur, fitted(imComplex.lmerGlobalSpeech))^2
#[1] 0.4586942


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, bothe have a sign effect
# - The effect size of LocSpeech decreases when it is the only variable in the model
# - The effect size of GlobalSpeech increases when it is the only variable in the model
# - The effect direction never changes (no supression)
# - Not a big difference in R2 between the different models
#################################################


##############################################################
# The decomposability variables

############
#Rating

imComplex.lmerRating<- lmer(ConsonantDur ~ Rating + (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerRating)
# 
# Estimate   Std. Error           df t value            Pr(>|t|)    
# (Intercept)    0.0916711    0.0030954   65.4000000  29.615 <0.0000000000000002 ***
#   Rating         0.0001508    0.0007488 1003.5000000   0.201                0.84  

# not significant


########
#Type of base

imComplex.lmerTypeOfBase<- lmer(ConsonantDur ~ TypeOfBase + (1|Item)+(1|Participant), data = imComplex)


summary(imComplex.lmerTypeOfBase)
# Estimate Std. Error        df t value            Pr(>|t|)    
# (Intercept)     0.086606   0.005660 59.890000  15.301 <0.0000000000000002 ***
#   TypeOfBaseword  0.005848   0.005395 44.520000   1.084               0.284  

# not significant

######################
# RelFreq
imComplex.lmerRelFreq<- lmer(ConsonantDur ~ logRelFreq+ (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerRelFreq)
# Fixed effects:
#   Estimate Std. Error         df t value            Pr(>|t|)    
# (Intercept)  0.0916643  0.0027859 44.0100000  32.903 <0.0000000000000002 ***
#   logRelFreq  -0.0008797  0.0006370 45.0900000  -1.381               0.174    

# not significant


##############
# Semantic Trans


imComplex.lmerST<- lmer(ConsonantDur ~ SemanticTransparency+ (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerST)


#SemanticTransparencytransparent -0.0008351  0.0040780 45.4900000  -0.205               0.839    


# not significant


##############
# Affix


imComplex.lmerAffix<- lmer(ConsonantDur ~ Affix+ (1|Item)+(1|Participant), data = imComplex)

summary(imComplex.lmerAffix)


# Fixed effects:
#   Estimate Std. Error         df t value            Pr(>|t|)    
# (Intercept)  0.0917447  0.0038223 68.7300000  24.002 <0.0000000000000002 ***
#   AffixNeg     0.0002716  0.0035134 45.3400000   0.077               0.939    

# no

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################
# Now we have dealt with all collinearity problems: 
# - We will keep both frequency variables even though they are never significanr
# - We will keep both Speech Rate variables but must be aware of the fact that their effect
#   size cannot be interpreted!
# - None of the decomposability variables is significant individually, we will conduct a PC
###############################################################################################


###########################################################################
#           PC Analysis                                                   #
###########################################################################


library(pls)


# For the PC, we need to recode all the variables, so that they are numeric

# # also they need to "point in the same direction" --> the higher
# the less decomposable

# RelFreq is fine
# Rating is fine

#Affix

levels(imComplex$Affix)
#[1] "Loc" "Neg"


imComplex$Affix <- relevel (imComplex$Affix, ref= "Neg")

imComplex$NumAffix<-as.numeric(imComplex$Affix)

table(imComplex$Affix,imComplex$NumAffix)

#       1   2
# Neg 862   0
# Loc   0 315

# good


#Type pf base

levels(imComplex$TypeOfBase)
#[1] "bound" "word"     

imComplex$TypeOfBase <- relevel (imComplex$TypeOfBase, ref= "word"   )

imComplex$NumTypeOfBase<-as.numeric(imComplex$TypeOfBase)

table(imComplex$TypeOfBase,imComplex$NumTypeOfBase)
#         1    2
# word  1068    0
# bound    0  109


#Smenatic Transparency

levels(imComplex$SemanticTransparency)
#[1] "opaque"      "transparent"

imComplex$SemanticTransparency <- relevel (imComplex$SemanticTransparency, ref= "transparent"   )

imComplex$NumSemanticTransparency<-as.numeric(imComplex$SemanticTransparency)

table(imComplex$SemanticTransparency,imComplex$NumSemanticTransparency)
#               1   2
# transparent 971   0
# opaque        0 206

# one further problem is that the variables are on 
# different scales - so we need to change this

imComplex$ScaledSemanticTransparency<-as.numeric(scale(imComplex$NumSemanticTransparency))
summary(imComplex$ScaledSemanticTransparency)

imComplex$ScaledRating<-as.numeric(scale(imComplex$Rating))
summary(imComplex$ScaledRating)

imComplex$ScaledTypeOfBase<-as.numeric(scale(imComplex$NumTypeOfBase))
summary(imComplex$ScaledTypeOfBase)

imComplex$ScaledAffix<-as.numeric(scale(imComplex$NumAffix))
summary(imComplex$ScaledAffix)

imComplex$ScaledRelFreq<-as.numeric(scale(imComplex$logRelFreq))
summary(imComplex$ScaledRelFreq)



# so now they are scaled

# there are 2 observations without a rating....in order to do the PC analysis we need
# a new data set...

imComplexRating<-imComplex[!is.na(imComplex$Rating),]

decomposability.pc <- prcomp(imComplexRating[, c("ScaledAffix", "ScaledRelFreq","ScaledRating","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability.pc)

# Importance of components:
#                        PC1    PC2    PC3    PC4     PC5
# Standard deviation     1.6451 0.9599 0.7629 0.7145 0.51981
# Proportion of Variance 0.5423 0.1846 0.1166 0.1023 0.05414
# Cumulative Proportion  0.5423 0.7269 0.8436 0.9459 1.00000

# so first two are the most important ones

decomposability.pc$rotation
#                             PC1         PC2        PC3         PC4         PC5
# ScaledAffix                0.4370240 -0.40733672  0.1537836 -0.78504625  0.05603405
# ScaledRelFreq              0.3141205  0.80787453  0.4074907 -0.14685516  0.24708686
# ScaledRating               0.4312801  0.27531107 -0.8510857 -0.07590436 -0.08996087
# ScaledTypeOfBase           0.5263440 -0.06957913  0.2907265  0.33450665 -0.72229087
# ScaledSemanticTransparency 0.4972904 -0.31745567  0.0378568  0.49444896  0.63719009

# PC1: Reall all measures
# PC 2: also mixture, except fot ype of base
# PC 3_ Rating, RelFreq, Type of Base
# PC4: Affix, SemTrans



xtable(decomposability.pc$rotation, digits=3)

# let's try out a model with PC1 and PC 2 as predictor variables

imComplexRating$PCDec1 <- decomposability.pc$x[, 1]
imComplexRating$PCDec2 <- decomposability.pc$x[, 2]
imComplexRating$PCDec3 <- decomposability.pc$x[, 3]
imComplexRating$PCDec4 <- decomposability.pc$x[, 4]

pairscor.fnc(imComplexRating [, c("PCDec1", "PCDec2", "PCDec3", "PCDec4", "Affix", 
                                  "logRelFreq", "SemanticTransparency", "Rating", "TypeOfBase")])


pairscor.fnc(imComplexRating [, c("PCDec1", "PCDec4", "Affix", 
                                  "logRelFreq", "SemanticTransparency", "Rating", "TypeOfBase")])


# let's see whether this has an influence


im_Complex_PC.lmer<-lmer(ConsonantDur ~ PCDec1+PCDec2+PCDec3+PCDec4 + 
                         (1|Item) + (1|Participant), data = imComplexRating)


summary(im_Complex_PC.lmer)

# Fixed effects:
#               Estimate  Std. Error          df t value            Pr(>|t|)    
# (Intercept)   0.0919165   0.0027955  43.9800000  32.880 <0.0000000000000002 ***
# PCDec1       -0.0006315   0.0009355  43.5400000  -0.675               0.503    
# PCDec2       -0.0019913   0.0015521  44.8600000  -1.283               0.206    
# PCDec3       -0.0014971   0.0011232 244.3100000  -1.333               0.184    
# PCDec4        0.0001010   0.0021528  43.4400000   0.047               0.963    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) PCDec1 PCDec2 PCDec3
# PCDec1  0.010                     
# PCDec2 -0.002 -0.035              
# PCDec3  0.007  0.308  0.324       
# PCDec4 -0.005  0.022  0.017 -0.116


# We need to spell out this model (for the appendix of the dissertation)


###########Initial PC-model

############################################################


imComplexPC.lmer <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                          BaseInitialStress + LocSpeech + GlobalSpeechRate +
                          PrePause + PostPause + PrecSegDur+
                           PCDec1+PCDec2+PCDec3+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating)

(summary(imComplexPC.lmer))
# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.15881198    0.00746634  596.10000000  21.270 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00544439    0.00327709   40.40000000   1.661              0.10439    
# AccentuationConditionunaccented    0.00551322    0.00186142  924.40000000   2.962              0.00314 ** 
#   OrderRescale                      -0.00003584    0.00006374 1120.40000000  -0.562              0.57402    
# logWordFormFreq                    0.00046402    0.00066111   40.80000000   0.702              0.48674    
# BaseInitialStressunstressed       -0.00838248    0.00324044   41.10000000  -2.587              0.01333 *  
#   LocSpeech                         -0.00379913    0.00038154  950.60000000  -9.957 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.01056700    0.00221175  771.80000000  -4.778           0.00000212 ***
#   PrePausepause                      0.00072217    0.00147462 1135.30000000   0.490              0.62441    
# PostPausepause                     0.00048770    0.00163891 1139.90000000   0.298              0.76608    
# PrecSegDur                        -0.00930626    0.03167969 1141.00000000  -0.294              0.76899    
# PCDec1                            -0.00080022    0.00090633   40.00000000  -0.883              0.38255    
# PCDec2                            -0.00127006    0.00151956   40.60000000  -0.836              0.40816    
# PCDec3                            -0.00058429    0.00106749  188.30000000  -0.547              0.58479    
# PCDec4                            -0.00455380    0.00218114   40.30000000  -2.088              0.04317 *  

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (imComplexPC.lmer))
qqline (residuals (imComplexPC.lmer))

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


imComplexPC.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                   BaseInitialStress + LocSpeech + GlobalSpeechRate +
                   PrePause + PostPause + PrecSegDur+
                   PCDec1+PCDec2+PCDec3+PCDec4, data = imComplexRating)

summary(imComplexPC.lm)

# Coefficients:
#   Estimate  Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.16117697  0.00634194  25.414 < 0.0000000000000002 ***
#   Environmentm#mV                  0.00570017  0.00151120   3.772              0.00017 ***
# AccentuationConditionunaccented  0.00515642  0.00179495   2.873              0.00414 ** 
#   OrderRescale                    -0.00001463  0.00007225  -0.203              0.83954    
# logWordFormFreq                  0.00050250  0.00030881   1.627              0.10396    
# BaseInitialStressunstressed     -0.00890892  0.00152620  -5.837       0.000000006878 ***
#   LocSpeech                       -0.00381721  0.00033331 -11.452 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.01130638  0.00181904  -6.216       0.000000000712 ***
#   PrePausepause                    0.00110238  0.00156564   0.704              0.48151    
# PostPausepause                  -0.00115139  0.00176762  -0.651              0.51493    
# PrecSegDur                      -0.01532981  0.03364135  -0.456              0.64870    
# PCDec1                          -0.00072012  0.00041617  -1.730              0.08383 .  
# PCDec2                          -0.00134287  0.00071047  -1.890              0.05899 .  
# PCDec3                          -0.00100803  0.00088927  -1.134              0.25722    
# PCDec4                          -0.00498239  0.00100289  -4.968       0.000000777756 ***

bc<-boxcox(imComplexPC.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.1010101

imComplexRating$bc <- imComplexRating$ConsonantDur^lambda

imComplexPC.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                           PCDec1+PCDec2+PCDec3+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating)

summary(imComplexPC.lmerBC)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.83736687    0.00621479  582.30000000 134.738 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00412703    0.00282494   40.30000000   1.461              0.15178    
# AccentuationConditionunaccented    0.00403967    0.00153182  983.20000000   2.637              0.00849 ** 
#   OrderRescale                      -0.00003949    0.00005205 1118.00000000  -0.759              0.44824    
# logWordFormFreq                    0.00034503    0.00056975   40.70000000   0.606              0.54815    
# BaseInitialStressunstressed       -0.00800042    0.00279209   41.00000000  -2.865              0.00654 ** 
#   LocSpeech                         -0.00314789    0.00031374  992.00000000 -10.034 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00798813    0.00182804  863.30000000  -4.370            0.0000139 ***
#   PrePausepause                      0.00103273    0.00120592 1133.00000000   0.856              0.39197    
# PostPausepause                     0.00030241    0.00133994 1136.10000000   0.226              0.82149    
# PrecSegDur                        -0.00909464    0.02590799 1137.90000000  -0.351              0.72563    
# PCDec1                            -0.00053377    0.00078142   40.00000000  -0.683              0.49849    
# PCDec2                            -0.00109258    0.00130967   40.50000000  -0.834              0.40904    
# PCDec3                            -0.00043358    0.00089394  171.90000000  -0.485              0.62828    
# PCDec4                            -0.00363226    0.00188033   40.30000000  -1.932              0.06044 .  

#let's check the assumptions

qqnorm (residuals (imComplexPC.lmerBC))
qqline (residuals (imComplexPC.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(imComplexPC.lmerBC, imComplexRating, trim = 2.5)
# n.removed = 25 
# percent.removed = 2.12766 

imComplexRating2<-outliers$data

dim(imComplexRating2)
#[1] 1150   97

dim(imComplexRating)
#[1]1175   96


# okay it seemes to have worked

imComplexPC.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             PCDec1+PCDec2+PCDec3+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.83723782    0.00578311  559.70000000 144.773 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00436091    0.00269749   40.30000000   1.617              0.11375    
#   AccentuationConditionunaccented    0.00432392    0.00141357 1000.70000000   3.059              0.00228 ** 
#   OrderRescale                      -0.00004545    0.00004711 1090.60000000  -0.965              0.33489    
#   logWordFormFreq                    0.00026417    0.00054355   40.60000000   0.486              0.62957    
#   BaseInitialStressunstressed       -0.00846506    0.00266328   40.80000000  -3.178              0.00282 ** 
#   LocSpeech                         -0.00325446    0.00029434  988.00000000 -11.057 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00737224    0.00169450  923.10000000  -4.351            0.0000151 ***
#   PrePausepause                      0.00180056    0.00109308 1104.80000000   1.647              0.09979 .  
#   PostPausepause                     0.00045076    0.00122131 1107.40000000   0.369              0.71214    
#   PrecSegDur                        -0.01423768    0.02358548 1109.00000000  -0.604              0.54619    
#   PCDec1                            -0.00044137    0.00074572   39.90000000  -0.592              0.55727    
#   PCDec2                            -0.00141371    0.00124964   40.40000000  -1.131              0.26460    
#   PCDec3                            -0.00041624    0.00082783  155.50000000  -0.503              0.61581    
#   PCDec4                            -0.00366402    0.00179504   40.30000000  -2.041              0.04782 *  

qqnorm (residuals (imComplexPC.lmerBC2))
qqline (residuals (imComplexPC.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> imComplexPC.lmerBC2


###########
# Simplification of PC-model

# 1. Post Pause


imComplexPC.lmerBC3 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause +  PrecSegDur+
                              PCDec1+PCDec2+PCDec3+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC3)


# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.83799420    0.00541893  495.10000000 154.642 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00437491    0.00270108   40.40000000   1.620              0.11309    
# AccentuationConditionunaccented    0.00420945    0.00137679  961.80000000   3.057              0.00229 ** 
#   OrderRescale                      -0.00004527    0.00004709 1091.60000000  -0.961              0.33653    
# logWordFormFreq                    0.00026789    0.00054423   40.60000000   0.492              0.62520    
# BaseInitialStressunstressed       -0.00845989    0.00266704   40.80000000  -3.172              0.00287 ** 
#   LocSpeech                         -0.00326415    0.00029331  994.60000000 -11.129 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00753611    0.00163497  925.20000000  -4.609           0.00000461 ***
#   PrePausepause                      0.00182568    0.00109031 1106.30000000   1.674              0.09432 .  
# PrecSegDur                        -0.01478617    0.02353244 1109.50000000  -0.628              0.52992    
# PCDec1                            -0.00043159    0.00074631   39.90000000  -0.578              0.56632    
# PCDec2                            -0.00141262    0.00125142   40.40000000  -1.129              0.26563    
# PCDec3                            -0.00040841    0.00082805  155.10000000  -0.493              0.62256    
# PCDec4                            -0.00367684    0.00179729   40.30000000  -2.046              0.04735 * 

# 2. Word Form Frq


imComplexPC.lmerBC4 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause +  PrecSegDur+
                              PCDec1+PCDec2+PCDec3+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC4)


# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.83864265    0.00523295  644.10000000 160.262 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00474179    0.00257304   41.60000000   1.843              0.07249 .  
# AccentuationConditionunaccented    0.00417076    0.00137493  967.20000000   3.033              0.00248 ** 
#   OrderRescale                      -0.00004515    0.00004708 1091.90000000  -0.959              0.33781    
# BaseInitialStressunstressed       -0.00823235    0.00260212   41.80000000  -3.164              0.00290 ** 
#   LocSpeech                         -0.00325967    0.00029301  994.60000000 -11.125 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00749436    0.00163206  927.30000000  -4.592             0.000005 ***
#   PrePausepause                      0.00182244    0.00109016 1106.80000000   1.672              0.09486 .  
# PrecSegDur                        -0.01481051    0.02352956 1110.00000000  -0.629              0.52919    
# PCDec1                            -0.00035322    0.00072261   41.10000000  -0.489              0.62758    
# PCDec2                            -0.00128865    0.00121463   41.60000000  -1.061              0.29484    
# PCDec3                            -0.00031887    0.00080297  170.70000000  -0.397              0.69178    
# PCDec4                            -0.00366742    0.00178075   41.30000000  -2.059              0.04579 *  



# 3.PC 3


imComplexPC.lmerBC5 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause +  PrecSegDur+
                              PCDec1+PCDec2+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC5)


# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.83868217    0.00522754  646.00000000 160.435 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00489650    0.00253576   43.10000000   1.931              0.06008 .  
# AccentuationConditionunaccented    0.00418092    0.00137416  968.40000000   3.043              0.00241 ** 
#   OrderRescale                      -0.00004563    0.00004705 1093.80000000  -0.970              0.33239    
# BaseInitialStressunstressed       -0.00827409    0.00259210   42.80000000  -3.192              0.00265 ** 
#   LocSpeech                         -0.00326301    0.00029267  994.20000000 -11.149 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00750584    0.00163107  928.00000000  -4.602           0.00000477 ***
#   PrePausepause                      0.00180655    0.00108907 1108.30000000   1.659              0.09744 .  
# PrecSegDur                        -0.01487227    0.02352206 1110.90000000  -0.632              0.52734    
# PCDec1                            -0.00025251    0.00067470   55.40000000  -0.374              0.70965    
# PCDec2                            -0.00109619    0.00111036   61.00000000  -0.987              0.32743    
# PCDec4                            -0.00379633    0.00174560   44.30000000  -2.175              0.03503 *  


# 4.PC 1


imComplexPC.lmerBC6 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause +  PrecSegDur+
                              PCDec2+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC6)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.83870410    0.00521966  651.40000000 160.682 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00482430    0.00250645   44.60000000   1.925              0.06065 .  
#   AccentuationConditionunaccented    0.00419192    0.00137346  970.80000000   3.052              0.00233 ** 
#   OrderRescale                      -0.00004568    0.00004705 1094.10000000  -0.971              0.33182    
#   BaseInitialStressunstressed       -0.00827645    0.00256973   43.60000000  -3.221              0.00242 ** 
#   LocSpeech                         -0.00325306    0.00029174  985.70000000 -11.151 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00755185    0.00162614  926.80000000  -4.644           0.00000391 ***
#   PrePausepause                      0.00180350    0.00108883 1109.60000000   1.656              0.09793 .  
#   PrecSegDur                        -0.01480499    0.02351872 1111.30000000  -0.629              0.52915    
#   PCDec2                            -0.00118509    0.00107724   76.10000000  -1.100              0.27475    
#   PCDec4                            -0.00373444    0.00172319   46.50000000  -2.167              0.03538 *  

# 5.PrecSegDur


imComplexPC.lmerBC7 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause + 
                              PCDec2+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC7)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.83708095    0.00454357  471.40000000 184.234 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00479440    0.00249989   44.70000000   1.918              0.06154 .  
#   AccentuationConditionunaccented    0.00416128    0.00137249  971.00000000   3.032              0.00249 ** 
#   OrderRescale                      -0.00004354    0.00004691 1095.20000000  -0.928              0.35356    
#   BaseInitialStressunstressed       -0.00838690    0.00255746   43.30000000  -3.279              0.00206 ** 
#   LocSpeech                         -0.00320513    0.00028183  997.10000000 -11.373 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00749680    0.00162343  924.60000000  -4.618           0.00000443 ***
#   PrePausepause                      0.00185858    0.00108518 1110.60000000   1.713              0.08705 .  
#   PCDec2                            -0.00118428    0.00107519   76.00000000  -1.101              0.27417    
#   PCDec4                            -0.00370362    0.00171838   46.50000000  -2.155              0.03635 * 

# 5.Order


imComplexPC.lmerBC8 <- lmer(bc ~ Environment+ AccentuationCondition+ 
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause + 
                              PCDec2+PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC8)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
# (Intercept)                        0.8360640    0.0044038  440.6000000 189.851 < 0.0000000000000002 ***
#   Environmentm#mV                    0.0048099    0.0025037   44.7000000   1.921              0.06111 .  
# AccentuationConditionunaccented    0.0041063    0.0013706  971.1000000   2.996              0.00281 ** 
#   BaseInitialStressunstressed       -0.0083703    0.0025614   43.4000000  -3.268              0.00212 ** 
#   LocSpeech                         -0.0031987    0.0002817  998.3000000 -11.353 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.0073948    0.0016191  924.1000000  -4.567           0.00000561 ***
#   PrePausepause                      0.0018657    0.0010851 1111.6000000   1.719              0.08581 .  
# PCDec2                            -0.0011569    0.0010760   76.1000000  -1.075              0.28568    
# PCDec4                            -0.0037149    0.0017209   46.5000000  -2.159              0.03607 *  


# 6.Pc2


imComplexPC.lmerBC9 <- lmer(bc ~ Environment+ AccentuationCondition+ 
                              BaseInitialStress + LocSpeech + GlobalSpeechRate +
                              PrePause + 
                              PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
#   (Intercept)                        0.8358654    0.0044031  444.3000000 189.836 < 0.0000000000000002 ***
#   Environmentm#mV                    0.0051117    0.0024900   44.6000000   2.053              0.04598 *  
#   AccentuationConditionunaccented    0.0040903    0.0013708  973.4000000   2.984              0.00292 ** 
#   BaseInitialStressunstressed       -0.0083678    0.0025636   44.0000000  -3.264              0.00213 ** 
#   LocSpeech                         -0.0032037    0.0002817 1000.2000000 -11.371 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.0073469    0.0016190  928.0000000  -4.538           0.00000643 ***
#   PrePausepause                      0.0019002    0.0010846 1111.3000000   1.752              0.08006 .  
#   PCDec4                            -0.0036091    0.0017196   48.4000000  -2.099              0.04107 *  

#########now interactions


# 1. Environment and accentuation and stress and pause

# Environment and stress


imComplexPC.lmerBC9EnvStress <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition 
                                        + LocSpeech + GlobalSpeechRate +
                                       PrePause + 
                                       PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)


summary(imComplexPC.lmerBC9EnvStress)

# Fixed effects:
#                                                 Estimate   Std. Error           df t value             Pr(>|t|)    
#   (Intercept)                                    0.8313401    0.0042948  459.4000000 193.569 < 0.0000000000000002 ***
#   Environmentm#mV                                0.0120107    0.0023003   41.3000000   5.221           0.00000541 ***
#   BaseInitialStressunstressed                   -0.0007153    0.0024399   40.2000000  -0.293              0.77090    
#   AccentuationConditionunaccented                0.0039093    0.0013679  988.5000000   2.858              0.00435 ** 
#   LocSpeech                                     -0.0029833    0.0002730  767.1000000 -10.926 < 0.0000000000000002 ***
#   GlobalSpeechRate                              -0.0077619    0.0015967  906.7000000  -4.861           0.00000137 ***
#   PrePausepause                                  0.0020290    0.0010825 1118.3000000   1.874              0.06115 .  
#   PCDec4                                        -0.0043294    0.0013430   42.6000000  -3.224              0.00243 ** 
#   Environmentm#mV:BaseInitialStressunstressed   -0.0218009    0.0040367   40.0000000  -5.401           0.00000327 ***
# 


# is this better thaqb the one whtout interaction

anova(imComplexPC.lmerBC9,imComplexPC.lmerBC9EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df  Pr(>Chisq)    
# object 11 -6365.6 -6310.1 3193.8  -6387.6                              
# ..1    12 -6387.5 -6326.9 3205.7  -6411.5 23.901      1 0.000001014 ***


#YES

# Environment and Acc
imComplexPC.lmerBC9EnvAcc <- lmer(bc ~ Environment*AccentuationCondition+ 
                                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                                    PrePause + 
                                    PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9EnvAcc)

# no interaction


# Environment and PrePause
imComplexPC.lmerBC9EnvPause <- lmer(bc ~ Environment*PrePause+ AccentuationCondition+ 
                                      BaseInitialStress + LocSpeech + GlobalSpeechRate +
                                      PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9EnvPause)

# no interaction



# Accentuation and PrePause
imComplexPC.lmerBC9AccPause <- lmer(bc ~ Environment+ AccentuationCondition*PrePause+ 
                                      BaseInitialStress + LocSpeech + GlobalSpeechRate +
                                      PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9AccPause)

# no interaction

# Stress and PrePause
imComplexPC.lmerBC9StressPause <- lmer(bc ~ Environment+ AccentuationCondition+ 
                                         BaseInitialStress*PrePause + LocSpeech + GlobalSpeechRate +
                                         PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9StressPause)

# no interaction

# Stress and Acc
imComplexPC.lmerBC9StressAcc <- lmer(bc ~ Environment+ AccentuationCondition*BaseInitialStress + 
                                       LocSpeech + GlobalSpeechRate +
                                       PrePause + 
                                       PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9StressAcc)

# Fixed effects:
#   Estimate  Std. Error          df t value
# (Intercept)                                                    0.835863    0.004397  443.800000 190.087
# Environmentm#mV                                                0.005138    0.002488   44.600000   2.066
# AccentuationConditionunaccented                                0.002628    0.001515 1028.700000   1.734
# BaseInitialStressunstressed                                   -0.010388    0.002715   55.400000  -3.827
# LocSpeech                                                     -0.003158    0.000282  999.900000 -11.199
# GlobalSpeechRate                                              -0.007265    0.001617  928.600000  -4.494
# PrePausepause                                                  0.001859    0.001083 1110.200000   1.717
# PCDec4                                                        -0.003632    0.001718   48.400000  -2.114
# AccentuationConditionunaccented:BaseInitialStressunstressed    0.004089    0.001821 1079.500000   2.245
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                         0.044694 *  
# AccentuationConditionunaccented                                         0.083227 .  
# BaseInitialStressunstressed                                             0.000332 ***
#   LocSpeech                                                   < 0.0000000000000002 ***
#   GlobalSpeechRate                                                      0.00000788 ***
#   PrePausepause                                                           0.086259 .  
# PCDec4                                                                  0.039669 *  
#   AccentuationConditionunaccented:BaseInitialStressunstressed             0.024996 *  

# Let's see whether this is better than the one with the other interaction


anova(imComplexPC.lmerBC9StressAcc,imComplexPC.lmerBC9EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 12 -6368.6 -6308.1 3196.3  -6392.6                                        
# ..1    12 -6387.5 -6326.9 3205.7  -6411.5 18.846      0 < 0.00000000000000022 ***


# not the iother is better, but what if we have both interactions?

# Stress and Acc and Stress and Environment
imComplexPC.lmerBC9StressAccStressEnv <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition*BaseInitialStress+ 
                                                 LocSpeech + GlobalSpeechRate +
                                                PrePause + 
                                                PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9StressAccStressEnv)


# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                                    0.8313291    0.0042871  458.9000000 193.914
# Environmentm#mV                                                0.0120651    0.0022908   41.3000000   5.267
# BaseInitialStressunstressed                                   -0.0027596    0.0025859   51.6000000  -1.067
# AccentuationConditionunaccented                                0.0024070    0.0015120 1042.4000000   1.592
# LocSpeech                                                     -0.0029357    0.0002732  764.4000000 -10.748
# GlobalSpeechRate                                              -0.0076827    0.0015941  906.8000000  -4.819
# PrePausepause                                                  0.0019874    0.0010807 1117.3000000   1.839
# PCDec4                                                        -0.0043466    0.0013374   42.6000000  -3.250
# Environmentm#mV:BaseInitialStressunstressed                   -0.0218962    0.0040197   40.1000000  -5.447
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.0042062    0.0018213 1080.3000000   2.309
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                       0.00000465 ***
# BaseInitialStressunstressed                                              0.29086    
# AccentuationConditionunaccented                                          0.11171    
# LocSpeech                                                   < 0.0000000000000002 ***
#   GlobalSpeechRate                                                      0.00000169 ***
#   PrePausepause                                                            0.06618 .  
# PCDec4                                                                   0.00226 ** 
#   Environmentm#mV:BaseInitialStressunstressed                           0.00000281 ***
# BaseInitialStressunstressed:AccentuationConditionunaccented              0.02111 *  

# let's see whether this is better than the prev. best model
# anova(imComplexPC.lmerBC9StressAccStressEnv,imComplexPC.lmerBC9EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    12 -6387.5 -6326.9 3205.7  -6411.5                           
# object 13 -6390.9 -6325.2 3208.4  -6416.9 5.3794      1    0.02038 *

# yes, that is the best model so far


# what about a three way interaction


imComplexPC.lmerBC9StressAccEnv <- lmer(bc ~ Environment*AccentuationCondition*BaseInitialStress
                                        + LocSpeech + GlobalSpeechRate +
                                          PrePause + 
                                          PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9StressAccEnv)


# no, okay!

# So now, this would be our final model:

summary(imComplexPC.lmerBC9StressAccStressEnv)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                                    0.8313291    0.0042871  458.9000000 193.914
# Environmentm#mV                                                0.0120651    0.0022908   41.3000000   5.267
# BaseInitialStressunstressed                                   -0.0027596    0.0025859   51.6000000  -1.067
# AccentuationConditionunaccented                                0.0024070    0.0015120 1042.4000000   1.592
# LocSpeech                                                     -0.0029357    0.0002732  764.4000000 -10.748
# GlobalSpeechRate                                              -0.0076827    0.0015941  906.8000000  -4.819
# PrePausepause                                                  0.0019874    0.0010807 1117.3000000   1.839
# PCDec4                                                        -0.0043466    0.0013374   42.6000000  -3.250
# Environmentm#mV:BaseInitialStressunstressed                   -0.0218962    0.0040197   40.1000000  -5.447
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.0042062    0.0018213 1080.3000000   2.309
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                       0.00000465 ***
# BaseInitialStressunstressed                                              0.29086    
# AccentuationConditionunaccented                                          0.11171    
# LocSpeech                                                   < 0.0000000000000002 ***
#   GlobalSpeechRate                                                      0.00000169 ***
#   PrePausepause                                                            0.06618 .  
# PCDec4                                                                   0.00226 ** 
#   Environmentm#mV:BaseInitialStressunstressed                           0.00000281 ***
# BaseInitialStressunstressed:AccentuationConditionunaccented              0.02111 *  

# let' see whether we need PrePause

imComplexPC.lmerBC9StressAccStressEnv2 <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition*BaseInitialStress+ 
                                                LocSpeech + GlobalSpeechRate +
                                                PCDec4+ (1|Item) + (1|Participant), data = imComplexRating2)

summary(imComplexPC.lmerBC9StressAccStressEnv2)
# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                                    0.8343611    0.0039760  378.5000000 209.851
# Environmentm#mV                                                0.0122363    0.0023137   41.4000000   5.289
# BaseInitialStressunstressed                                   -0.0024843    0.0026071   51.3000000  -0.953
# AccentuationConditionunaccented                                0.0024930    0.0015123 1040.4000000   1.649
# LocSpeech                                                     -0.0029399    0.0002738  777.5000000 -10.737
# GlobalSpeechRate                                              -0.0085355    0.0015255  864.8000000  -5.595
# PCDec4                                                        -0.0043839    0.0013517   42.8000000  -3.243
# Environmentm#mV:BaseInitialStressunstressed                   -0.0218348    0.0040643   40.3000000  -5.372
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.0042612    0.0018225 1081.3000000   2.338
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                     0.0000043192 ***
# BaseInitialStressunstressed                                              0.34511    
# AccentuationConditionunaccented                                          0.09954 .  
# LocSpeech                                                   < 0.0000000000000002 ***
#   GlobalSpeechRate                                                    0.0000000295 ***
#   PCDec4                                                                   0.00229 ** 
#   Environmentm#mV:BaseInitialStressunstressed                         0.0000035264 ***
# BaseInitialStressunstressed:AccentuationConditionunaccented              0.01956 *  


anova(imComplexPC.lmerBC9StressAccStressEnv2,imComplexPC.lmerBC9StressAccStressEnv)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 12 -6389.4 -6328.8 3206.7  -6413.4                           
# ..1    13 -6390.9 -6325.2 3208.4  -6416.9 3.4702      1    0.06249 .

# no so, this is our final model

# let's have alook

visreg(imComplexPC.lmerBC9StressAccStressEnv2)

visreg(imComplexPC.lmerBC9StressAccStressEnv2, "Environment", by="BaseInitialStress")
visreg(imComplexPC.lmerBC9StressAccStressEnv2, "AccentuationCondition", by="BaseInitialStress")

# so higher speech rate - shorter nasal
# when primarily stressed, double slightly longer
# interaction betweem stress and acc
# the higher PC Dec 4, the shorter the nasal

#   PCDec4 is mostly composed of Affix, ST and 


#############################################################
# The final model:

summary(imComplexPC.lmerBC9StressAccStressEnv2)

lambda
#[1] 0.1010101


# I need to rename some variabels for the plot...


imComplexRating2<-rename(imComplexRating2,AccentuationAnnotator=Accentuation)

imComplexRating2<-rename(imComplexRating2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(imComplexRating2$BaseInitialStress)
#[1] "primary"    "unstressed"


levels(imComplexRating2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(imComplexRating2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# also need to change ref levels for environment

imComplexRating2$Environment <- relevel (imComplexRating2$Environment, ref= "m#mV")


final_im_complex_PC_model.lmer<-lmer(bc ~  Environment*BaseInitialStress+BaseInitialStress*Accentuation+LocSpeech+ GlobalSpeechRate+                                              
                                    PCDec4+(1|Participant)+ (1|Item) , data = imComplexRating2)                                 


summary(final_im_complex_PC_model.lmer)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                           0.8465974    0.0041214  356.5000000 205.413
# Environmentm#C                                       -0.0122363    0.0023137   41.4000000  -5.289
# BaseInitialStressunstressed                          -0.0243191    0.0034159   47.0000000  -7.119
# Accentuationunaccented                                0.0024930    0.0015123 1040.4000000   1.649
# LocSpeech                                            -0.0029399    0.0002738  777.5000000 -10.737
# GlobalSpeechRate                                     -0.0085355    0.0015255  864.8000000  -5.595
# PCDec4                                               -0.0043839    0.0013517   42.8000000  -3.243
# Environmentm#C:BaseInitialStressunstressed            0.0218348    0.0040643   40.3000000   5.372
# BaseInitialStressunstressed:Accentuationunaccented    0.0042612    0.0018225 1081.3000000   2.338
# Pr(>|t|)    
# (Intercept)                                        < 0.0000000000000002 ***
#   Environmentm#C                                            0.00000431915 ***
# BaseInitialStressunstressed                               0.00000000542 ***
#   Accentuationunaccented                                          0.09954 .  
# LocSpeech                                          < 0.0000000000000002 ***
#   GlobalSpeechRate                                          0.00000002954 ***
#   PCDec4                                                          0.00229 ** 
#   Environmentm#C:BaseInitialStressunstressed                0.00000352642 ***
# BaseInitialStressunstressed:Accentuationunaccented              0.01956 *  


#############
# Let's get the  model for the dissertation


table_final_model_PC<-as.data.frame(coef(summary(final_im_complex_PC_model.lmer)))

xtable(table_final_model_PC,digits = 3)



setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



##############################
# We should also plot the  effect
###############################
# Plot main effect

png("imModelPC.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der Estimate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

#visreg(final_im_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylim=ylim,cex.axis=0.9,par.settings=par)


visreg(final_im_complex_PC_model.lmer, "PCDec4",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
        xlab="PC 4",ylim=ylim,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()

library(dplyr)

imComplexRating2 %>% 
  filter(PCDec4 < -1) %>% 
  dplyr::select (Item, PCDec4) %>%
  group_by(Item, PCDec4) %>%
  summarize(N=n()) %>%
  ungroup() %>%
  arrange(Item, N) %>% 
  print(n=23)

imComplexRating2 %>%
  filter(!(Affix == "Loc" & SemanticTransparency == "transparent" & TypeOfBase == "word")) %>%
  dplyr::select(Item) %>% 
  unique()

imComplexRating2 %>% 
  filter(PCDec4 > 1) %>% 
  dplyr::select (Item, PCDec4) %>%
  group_by(Item, PCDec4) %>%
  summarize(N=n()) %>%
  ungroup() %>%
  arrange(Item, N) %>% 
  print(n=100)


########################
# SO; NO INFLUENCE OF SEGMENTABILITY MEASURES
##############################################################

# Let's refit our model incorportaing the "right variables"

imComplex.lmer3 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                           Affix+ (1|Item) + (1|Participant), data = imComplex)

(summary(imComplex.lmer3))
# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.16165681    0.00813514  418.20000000  19.871 < 0.0000000000000002 ***
# Environmentm#mV                    0.00449522    0.00318679   42.70000000   1.411              0.16561    
# AccentuationConditionunaccented    0.00535597    0.00186101  927.60000000   2.878              0.00409 ** 
# OrderRescale                      -0.00003335    0.00006357 1124.10000000  -0.525              0.59995    
# logWordFormFreq                    0.00018293    0.00065409   42.60000000   0.280              0.78108    
# BaseInitialStressunstressed       -0.00720655    0.00330568   43.00000000  -2.180              0.03477 *  
# LocSpeech                         -0.00377523    0.00038215  980.80000000  -9.879 < 0.0000000000000002 ***
# GlobalSpeechRate                  -0.01051094    0.00221422  774.70000000  -4.747           0.00000246 ***
# PrePausepause                      0.00070999    0.00147457 1137.30000000   0.481              0.63026    
# PostPausepause                     0.00028924    0.00163720 1142.30000000   0.177              0.85980    
# PrecSegDur                        -0.00824478    0.03168363 1144.00000000  -0.260              0.79474    
# AffixNeg                          -0.00290583    0.00348080   42.50000000  -0.835              0.40849 



###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (imComplex.lmer3))
qqline (residuals (imComplex.lmer3))

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


imComplex.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause + PrecSegDur+Affix, data = imComplex)

summary(imComplex.lm)

# Coefficients:
#                                      Estimate  Std. Error t value             Pr(>|t|)    
#   (Intercept)                      0.16289319  0.00665637  24.472 < 0.0000000000000002 ***
#   Environmentm#mV                  0.00486508  0.00144907   3.357             0.000812 ***
#   AccentuationConditionunaccented  0.00460222  0.00180989   2.543             0.011125 *  
#   OrderRescale                    -0.00001726  0.00007254  -0.238             0.811998    
#   logWordFormFreq                  0.00015862  0.00029701   0.534             0.593397    
#   BaseInitialStressunstressed     -0.00756151  0.00151840  -4.980         0.0000007321 ***
#   LocSpeech                       -0.00366229  0.00033351 -10.981 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.01118880  0.00183287  -6.105         0.0000000014 ***
#   PrePausepause                    0.00099289  0.00158124   0.628             0.530179    
#   PostPausepause                  -0.00156272  0.00178154  -0.877             0.380574    
#   PrecSegDur                      -0.00947082  0.03394417  -0.279             0.780285    
#   AffixNeg                        -0.00330242  0.00156350  -2.112             0.034882 *  

bc<-boxcox(imComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.1010101

imComplex$bc <- imComplex$ConsonantDur^lambda

imComplex.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause + PrecSegDur+
                            Affix+ (1|Item) + (1|Participant), data = imComplex)

summary(imComplex.lmerBC)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.83988790    0.00677776  403.10000000 123.918 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00340978    0.00271486   42.70000000   1.256               0.2160    
# AccentuationConditionunaccented    0.00391990    0.00153177  986.90000000   2.559               0.0106 *  
#   OrderRescale                      -0.00003734    0.00005193 1121.80000000  -0.719               0.4722    
# logWordFormFreq                    0.00012769    0.00055728   42.60000000   0.229               0.8199    
# BaseInitialStressunstressed       -0.00710740    0.00281574   43.00000000  -2.524               0.0154 *  
#   LocSpeech                         -0.00312803    0.00031386 1011.10000000  -9.966 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00795684    0.00182961  866.00000000  -4.349            0.0000153 ***
#   PrePausepause                      0.00103056    0.00120604 1135.20000000   0.854               0.3930    
# PostPausepause                     0.00013534    0.00133872 1138.90000000   0.101               0.9195    
# PrecSegDur                        -0.00820927    0.02591515 1141.30000000  -0.317               0.7515    
# AffixNeg                          -0.00268068    0.00296586   42.50000000  -0.904               0.3712 

#let's check the assumptions

qqnorm (residuals (imComplex.lmerBC))
qqline (residuals (imComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(imComplex.lmerBC, imComplex, trim = 2.5)
# n.removed = 24 
# percent.removed = 2.039082 

imComplex2<-outliers$data

dim(imComplex2)
#[1] 1153   94

dim(imComplex)
#[1] 1177   93


# okay it seemes to have worked

imComplex.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             Affix+ (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.84011568    0.00633333  382.40000000 132.650 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00394372    0.00259550   42.80000000   1.519              0.13601    
# AccentuationConditionunaccented    0.00401380    0.00141561  998.00000000   2.835              0.00467 ** 
#   OrderRescale                      -0.00004549    0.00004715 1095.50000000  -0.965              0.33484    
# logWordFormFreq                   -0.00001050    0.00053205   42.40000000  -0.020              0.98435    
# BaseInitialStressunstressed       -0.00763890    0.00268946   42.90000000  -2.840              0.00687 ** 
#   LocSpeech                         -0.00320710    0.00029531 1004.20000000 -10.860 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00732698    0.00170068  919.70000000  -4.308            0.0000182 ***
#   PrePausepause                      0.00178706    0.00109699 1108.40000000   1.629              0.10359    
# PostPausepause                     0.00016975    0.00122393 1111.40000000   0.139              0.88972    
# PrecSegDur                        -0.01266314    0.02366983 1113.90000000  -0.535              0.59276    
# AffixNeg                          -0.00323340    0.00283288   42.40000000  -1.141              0.26011

qqnorm (residuals (imComplex.lmerBC2))
qqline (residuals (imComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> imComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(imComplex.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.84011568    0.00633333  382.40000000 132.650 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00394372    0.00259550   42.80000000   1.519              0.13601    
# AccentuationConditionunaccented    0.00401380    0.00141561  998.00000000   2.835              0.00467 ** 
#   OrderRescale                      -0.00004549    0.00004715 1095.50000000  -0.965              0.33484    
# logWordFormFreq                   -0.00001050    0.00053205   42.40000000  -0.020              0.98435    
# BaseInitialStressunstressed       -0.00763890    0.00268946   42.90000000  -2.840              0.00687 ** 
#   LocSpeech                         -0.00320710    0.00029531 1004.20000000 -10.860 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00732698    0.00170068  919.70000000  -4.308            0.0000182 ***
#   PrePausepause                      0.00178706    0.00109699 1108.40000000   1.629              0.10359    
# PostPausepause                     0.00016975    0.00122393 1111.40000000   0.139              0.88972    
# PrecSegDur                        -0.01266314    0.02366983 1113.90000000  -0.535              0.59276    
# AffixNeg                          -0.00323340    0.00283288   42.40000000  -1.141              0.26011

# let's throw out PostPause

imComplex.lmerBC3 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             Affix+(1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC3)

# Fixed effects:
#                                        Estimate     Std. Error             df t value             Pr(>|t|)    
#   (Intercept)                        0.840411524    0.005975256  327.400000000 140.649 < 0.0000000000000002 ***
#   Environmentm#mV                    0.003947436    0.002596252   42.800000000   1.520              0.13576    
#   AccentuationConditionunaccented    0.003971098    0.001379381  959.400000000   2.879              0.00408 ** 
#   OrderRescale                      -0.000045413    0.000047128 1096.400000000  -0.964              0.33545    
#   logWordFormFreq                   -0.000008609    0.000532070   42.400000000  -0.016              0.98717    
#   BaseInitialStressunstressed       -0.007637074    0.002690371   42.900000000  -2.839              0.00689 ** 
#   LocSpeech                         -0.003210857    0.000294197 1009.700000000 -10.914 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.007388738    0.001641193  923.400000000  -4.502           0.00000759 ***
#   PrePausepause                      0.001796555    0.001094189 1109.900000000   1.642              0.10089    
#   PrecSegDur                        -0.012877835    0.023612983 1114.400000000  -0.545              0.58561    
#   AffixNeg                          -0.003248187    0.002831911   42.300000000  -1.147              0.25783 

anova(imComplex.lmerBC2,imComplex.lmerBC3)

# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   ..1:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   ..1:     PrecSegDur + Affix + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     PostPause + PrecSegDur + Affix + (1 | Item) + (1 | Participant)
#        Df   AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    14 -6365 -6294.3 3196.5    -6393                         
# object 15 -6363 -6287.2 3196.5    -6393 0.0219      1     0.8824

# model did not become worse


# let's throw out WordFormFreq

imComplex.lmerBC4 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             Affix+ (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC4)

# Fixed effects:
#                                       Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.84034187    0.00575885  445.70000000 145.922 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00393874    0.00248279   44.00000000   1.586              0.11980    
#   AccentuationConditionunaccented    0.00396614    0.00137775  964.20000000   2.879              0.00408 ** 
#   OrderRescale                      -0.00004549    0.00004712 1096.90000000  -0.965              0.33458    
#   BaseInitialStressunstressed       -0.00764657    0.00262317   44.00000000  -2.915              0.00558 ** 
#   LocSpeech                         -0.00320720    0.00029381 1007.90000000 -10.916 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00739150    0.00163842  925.70000000  -4.511           0.00000727 ***
#   PrePausepause                      0.00179979    0.00109407 1110.30000000   1.645              0.10024    
#   PrecSegDur                        -0.01277052    0.02360947 1114.90000000  -0.541              0.58868    
#   AffixNeg                          -0.00324467    0.00279181   43.40000000  -1.162              0.25150    

# # nothing has changed

anova(imComplex.lmerBC3,imComplex.lmerBC4)

# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   ..1:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + Affix + 
#   ..1:     (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + logWordFormFreq + 
#   object:     BaseInitialStress + LocSpeech + GlobalSpeechRate + PrePause + 
#   object:     PrecSegDur + Affix + (1 | Item) + (1 | Participant)
# Df   AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -6367 -6301.3 3196.5    -6393                         
# object 14 -6365 -6294.3 3196.5    -6393 0.0003      1     0.9851

# nothing has changed



# let's throw out PrecSegDur

imComplex.lmerBC5 <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + Affix+ 
                             (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC5)

# Fixed effects:
#                                       Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.83891774    0.00512636  320.80000000 163.648 < 0.0000000000000002 ***
#   Environmentm#mV                    0.00392131    0.00247651   44.10000000   1.583              0.12048    
#   AccentuationConditionunaccented    0.00394046    0.00137681  964.50000000   2.862              0.00430 ** 
#   OrderRescale                      -0.00004357    0.00004697 1098.10000000  -0.928              0.35380    
#   BaseInitialStressunstressed       -0.00774592    0.00261036   43.60000000  -2.967              0.00486 ** 
#   LocSpeech                         -0.00316577    0.00028392 1020.70000000 -11.150 < 0.0000000000000002 ***
#   GlobalSpeechRate                  -0.00734416    0.00163572  924.10000000  -4.490           0.00000803 ***
#   PrePausepause                      0.00184762    0.00109033 1111.30000000   1.695              0.09044 .  
#   AffixNeg                          -0.00321861    0.00278454   43.50000000  -1.156              0.25404   

anova(imComplex.lmerBC4,imComplex.lmerBC5)

# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   ..1:     LocSpeech + GlobalSpeechRate + PrePause + Affix + (1 | Item) + 
#   ..1:     (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   object:     LocSpeech + GlobalSpeechRate + PrePause + PrecSegDur + Affix + 
#   object:     (1 | Item) + (1 | Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -6368.7 -6308.1 3196.3  -6392.7                         
# object 13 -6367.0 -6301.3 3196.5  -6393.0 0.2796      1      0.597

#nothing has changed


# let's throw out Order

imComplex.lmerBC6 <- lmer(bc ~ Environment+ AccentuationCondition+GlobalSpeechRate +
                             BaseInitialStress + LocSpeech + 
                             PrePause + Affix+ 
                             (1|Item) + (1|Participant), data = imComplex2)


summary(imComplex.lmerBC6)

# Fixed effects:
#                                       Estimate   Std. Error           df t value             Pr(>|t|)    
#   (Intercept)                        0.8378854    0.0049998  296.6000000 167.583 < 0.0000000000000002 ***
#   Environmentm#mV                    0.0039233    0.0024807   44.1000000   1.582              0.12090    
#   AccentuationConditionunaccented    0.0038818    0.0013747  964.5000000   2.824              0.00484 ** 
#   GlobalSpeechRate                  -0.0072411    0.0016312  923.2000000  -4.439            0.0000101 ***
#   BaseInitialStressunstressed       -0.0077250    0.0026147   43.7000000  -2.954              0.00503 ** 
#   LocSpeech                         -0.0031594    0.0002838 1021.8000000 -11.131 < 0.0000000000000002 ***
#   PrePausepause                      0.0018545    0.0010902 1112.3000000   1.701              0.08920 .  
#   AffixNeg                          -0.0031915    0.0027891   43.5000000  -1.144              0.25876 

anova(imComplex.lmerBC5,imComplex.lmerBC6)


# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + GlobalSpeechRate + 
#   ..1:     BaseInitialStress + LocSpeech + PrePause + Affix + (1 | Item) + 
#   ..1:     (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + OrderRescale + BaseInitialStress + 
#   object:     LocSpeech + GlobalSpeechRate + PrePause + Affix + (1 | Item) + 
#   object:     (1 | Participant)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    11 -6369.8 -6314.3 3195.9  -6391.8                        
# object 12 -6368.7 -6308.1 3196.3  -6392.7 0.869      1     0.3512

# nothing has changed


# let's throw out Affix

imComplex.lmerBC7 <- lmer(bc ~ Environment+ AccentuationCondition+ GlobalSpeechRate+
                             LocSpeech + 
                             PrePause + BaseInitialStress+ 
                              (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7)


#   Fixed effects:
#                                     Estimate   Std. Error           df t value             Pr(>|t|)    
#   (Intercept)                        0.8352471    0.0044361  438.8000000 188.282 < 0.0000000000000002 ***
#   Environmentm#mV                    0.0036805    0.0024800   44.9000000   1.484              0.14478    
#  AccentuationConditionunaccented    0.0037904    0.0013724  972.0000000   2.762              0.00586 ** 
#  GlobalSpeechRate                  -0.0070570    0.0016237  926.1000000  -4.346            0.0000154 ***
#   LocSpeech                         -0.0031731    0.0002837 1025.6000000 -11.185 < 0.0000000000000002 ***
#   PrePausepause                      0.0018965    0.0010896 1114.4000000   1.741              0.08203 .  
#    BaseInitialStressunstressed       -0.0073713    0.0026053   44.6000000  -2.829              0.00697 ** 

anova(imComplex.lmerBC6,imComplex.lmerBC7)

# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + GlobalSpeechRate + 
#   ..1:     LocSpeech + PrePause + BaseInitialStress + (1 | Item) + (1 | 
#                                                                       ..1:     Participant)
# object: bc ~ Environment + AccentuationCondition + GlobalSpeechRate + 
#   object:     BaseInitialStress + LocSpeech + PrePause + Affix + (1 | Item) + 
#   object:     (1 | Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    10 -6370.4 -6319.9 3195.2  -6390.4                         
# object 11 -6369.8 -6314.3 3195.9  -6391.8 1.3974      1     0.2372

#still no difference


# let's throw out PrePause

imComplex.lmerBC8 <- lmer(bc ~ Environment+ AccentuationCondition+ GlobalSpeechRate+
                            LocSpeech + 
                            BaseInitialStress+ 
                            (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC8)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
# (Intercept)                        0.8381049    0.0041307  362.1000000 202.897 < 0.0000000000000002 ***
#   Environmentm#mV                    0.0038498    0.0024925   44.8000000   1.545              0.12949    
# AccentuationConditionunaccented    0.0038894    0.0013720  969.3000000   2.835              0.00468 ** 
#   GlobalSpeechRate                  -0.0078863    0.0015535  883.2000000  -5.077          0.000000469 ***
#   LocSpeech                         -0.0031721    0.0002841 1029.7000000 -11.167 < 0.0000000000000002 ***
#   BaseInitialStressunstressed       -0.0070534    0.0026141   44.2000000  -2.698              0.00984 ** 

anova(imComplex.lmerBC7,imComplex.lmerBC8)
# Data: imComplex2
# Models:
#   ..1: bc ~ Environment + AccentuationCondition + GlobalSpeechRate + 
#   ..1:     LocSpeech + BaseInitialStress + (1 | Item) + (1 | Participant)
# object: bc ~ Environment + AccentuationCondition + GlobalSpeechRate + 
#   object:     LocSpeech + PrePause + BaseInitialStress + (1 | Item) + (1 | 
#                                                                          object:     Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1     9 -6369.4 -6323.9 3193.7  -6387.4                           
# object 10 -6370.4 -6319.9 3195.2  -6390.4 3.0544      1    0.08052 .

# lmerBc 7 might be better - let's wait for the interactions, otherwise throw it out (nly
#marginally sign. difference)

# so that would be the final model without interactions



###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1. Environment and accentuation and stress and pause

# 2. Decomposability measures and accentuation

# 3. Decomposability measures and stress 

# 4. Decomposability measures and environment




# Let's see



# 1. Environment and accentuation and stress and pause

# Environment and stress


imComplex.lmerBC7EnvStress <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition+ GlobalSpeechRate+
                            LocSpeech + 
                            PrePause +  
                            (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvStress)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
# (Intercept)                                    0.8309644    0.0043615  452.9000000 190.524 < 0.0000000000000002 ***
#   Environmentm#mV                                0.0098793    0.0024303   42.2000000   4.065             0.000205 ***
# BaseInitialStressunstressed                    0.0000675    0.0026731   41.3000000   0.025             0.979977    
# AccentuationConditionunaccented                0.0035517    0.0013698  986.1000000   2.593             0.009662 ** 
#   GlobalSpeechRate                              -0.0072464    0.0016071  915.8000000  -4.509           0.00000736 ***
#   LocSpeech                                     -0.0029970    0.0002779  872.3000000 -10.785 < 0.0000000000000002 ***
#   PrePausepause                                  0.0020116    0.0010881 1119.3000000   1.849             0.064769 .  
# Environmentm#mV:BaseInitialStressunstressed   -0.0205602    0.0044313   41.4000000  -4.640           0.00003488 ***

# is this better thaqb the one whtout interaction

anova(imComplex.lmerBC7,imComplex.lmerBC7EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -6370.4 -6319.9 3195.2  -6390.4                             
# ..1    11 -6386.9 -6331.4 3204.5  -6408.9 18.526      1 0.00001676 ***


#YES

# Environment and Acc
imComplex.lmerBC7EnvAcc <- lmer(bc ~ Environment*AccentuationCondition+ GlobalSpeechRate+
                            LocSpeech + 
                            PrePause + BaseInitialStress+ 
                            (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvAcc)

# no interaction


# Environment and PrePause
imComplex.lmerBC7EnvPause <- lmer(bc ~ Environment*PrePause +AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                   BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvPause)

# no interaction



# Accentuation and PrePause
imComplex.lmerBC7AccPause <- lmer(bc ~ Environment+PrePause*AccentuationCondition+ GlobalSpeechRate+
                                    LocSpeech + 
                                    BaseInitialStress+ 
                                    (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7AccPause)

# no interaction

# Stress and PrePause
imComplex.lmerBC7StressPause <- lmer(bc ~ Environment+PrePause*BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                    LocSpeech + 
                                    (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7StressPause)

# no interaction

# Stress and Acc
imComplex.lmerBC7StressAcc <- lmer(bc ~ Environment+PrePause+BaseInitialStress*AccentuationCondition+ GlobalSpeechRate+
                                       LocSpeech + 
                                       (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7StressAcc)

# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                                    0.8352440    0.0044303  438.3000000 188.529
# Environmentm#mV                                                0.0036956    0.0024775   44.9000000   1.492
# PrePausepause                                                  0.0018555    0.0010877 1113.3000000   1.706
# BaseInitialStressunstressed                                   -0.0094107    0.0027535   55.8000000  -3.418
# AccentuationConditionunaccented                                0.0023113    0.0015171 1028.3000000   1.523
# GlobalSpeechRate                                              -0.0069741    0.0016215  926.9000000  -4.301
# LocSpeech                                                     -0.0031270    0.0002839 1025.2000000 -11.014
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.0041478    0.0018283 1082.4000000   2.269
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                          0.14277    
# PrePausepause                                                            0.08832 .  
# BaseInitialStressunstressed                                              0.00119 ** 
#   AccentuationConditionunaccented                                          0.12796    
# GlobalSpeechRate                                                       0.0000188 ***
#   LocSpeech                                                   < 0.0000000000000002 ***
#   BaseInitialStressunstressed:AccentuationConditionunaccented              0.02348 *  


# Let's see whether this is better than the one with the other interaction


anova(imComplex.lmerBC7StressAcc,imComplex.lmerBC7EnvStress)
# Data: imComplex2
# Models:
#   object: bc ~ Environment + PrePause + BaseInitialStress * AccentuationCondition + 
#   object:     GlobalSpeechRate + LocSpeech + (1 | Item) + (1 | Participant)
# ..1: bc ~ Environment * BaseInitialStress + AccentuationCondition + 
#   ..1:     GlobalSpeechRate + LocSpeech + PrePause + (1 | Item) + (1 | 
#                                                                      ..1:     Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 11 -6373.6 -6318.0 3197.8  -6395.6                                        
# ..1    11 -6386.9 -6331.4 3204.5  -6408.9 13.364      0 < 0.00000000000000022 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# not the iother is better, but what if we have both interactions?

# Stress and Acc and Stress and Environment
imComplex.lmerBC7StressAccStressEnv <- lmer(bc ~ Environment*BaseInitialStress+PrePause+BaseInitialStress*AccentuationCondition+ GlobalSpeechRate+
                                     LocSpeech + 
                                     (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7StressAccStressEnv)


# Fixed effects:
#   Estimate  Std. Error          df t value
# (Intercept)                                                    0.830948    0.004354  452.600000 190.843
# Environmentm#mV                                                0.009921    0.002422   42.200000   4.096
# BaseInitialStressunstressed                                   -0.001984    0.002808   51.100000  -0.707
# PrePausepause                                                  0.001970    0.001086 1118.200000   1.814
# AccentuationConditionunaccented                                0.002042    0.001514 1040.500000   1.349
# GlobalSpeechRate                                              -0.007163    0.001605  916.400000  -4.464
# LocSpeech                                                     -0.002950    0.000278  870.400000 -10.609
# Environmentm#mV:BaseInitialStressunstressed                   -0.020647    0.004416   41.400000  -4.675
# BaseInitialStressunstressed:AccentuationConditionunaccented    0.004233    0.001828 1082.900000   2.316
# Pr(>|t|)    
# (Intercept)                                                 < 0.0000000000000002 ***
#   Environmentm#mV                                                         0.000186 ***
# BaseInitialStressunstressed                                             0.483014    
# PrePausepause                                                           0.069965 .  
# AccentuationConditionunaccented                                         0.177759    
# GlobalSpeechRate                                                      0.00000905 ***
#   LocSpeech                                                   < 0.0000000000000002 ***
#   Environmentm#mV:BaseInitialStressunstressed                           0.00003110 ***
# BaseInitialStressunstressed:AccentuationConditionunaccented             0.020759 *  


# let's see whether this is better than the prev. best model
anova(imComplex.lmerBC7StressAccStressEnv,imComplex.lmerBC7EnvStress)
# Data: imComplex2
# Models:
#   ..1: bc ~ Environment * BaseInitialStress + AccentuationCondition + 
#   ..1:     GlobalSpeechRate + LocSpeech + PrePause + (1 | Item) + (1 | 
#                                                                      ..1:     Participant)
# object: bc ~ Environment * BaseInitialStress + PrePause + BaseInitialStress * 
#   object:     AccentuationCondition + GlobalSpeechRate + LocSpeech + (1 | 
#                                                                         object:     Item) + (1 | Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    11 -6386.9 -6331.4 3204.5  -6408.9                           
# object 12 -6390.3 -6329.7 3207.2  -6414.3 5.3943      1     0.0202 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# yes, that is the best model so far


# what about a three way interaction


imComplex.lmerBC7StressAccEnv <- lmer(bc ~ Environment*BaseInitialStress*AccentuationCondition+PrePause+ GlobalSpeechRate+
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7StressAccEnv)


# no, okay!


################################################
# 2. Decomposability measures and accentuation

imComplex.lmerBC7AccRelFreq <- lmer(bc ~ Environment+logRelFreq*AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7AccRelFreq)

# no
#############

imComplex.lmerBC7SemTAcc <- lmer(bc ~ Environment+SemanticTransparency*AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7SemTAcc)


# no

imComplex.lmerBC7RootAcc <- lmer(bc ~ Environment+TypeOfRoot*AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RootAcc)
# no

imComplex.lmerBC7RatingAcc <- lmer(bc ~ Environment+Rating*AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + BaseInitialStress+ 
                                  (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RatingAcc)

# no

############################################
# 3. Decomposability measures and stress 


imComplex.lmerBC7StrRelFreq <- lmer(bc ~ Environment+logRelFreq*BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                      LocSpeech + 
                                      PrePause + 
                                      (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7StrRelFreq)


#no
###############

imComplex.lmerBC7SemTStr <- lmer(bc ~ Environment+SemanticTransparency*BaseInitialStress+ GlobalSpeechRate+
                                   LocSpeech + 
                                   PrePause +AccentuationCondition+ 
                                   (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7SemTStr )


# no

imComplex.lmerBC7RootStr  <- lmer(bc ~ Environment+TypeOfRoot*BaseInitialStress+ GlobalSpeechRate+
                                   LocSpeech + 
                                   PrePause+AccentuationCondition+ 
                                   (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RootStr )
# no

imComplex.lmerBC7RatingStr  <- lmer(bc ~ Environment+Rating*BaseInitialStress+ GlobalSpeechRate+
                                     LocSpeech + 
                                     PrePause+AccentuationCondition+ 
                                     (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RatingStr )

# no



##############################################################
## 4. Decomposability measures and environment

imComplex.lmerBC7EnvRelFreq <- lmer(bc ~ Environment*logRelFreq+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                      LocSpeech + 
                                      PrePause + 
                                      (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvRelFreq)


#cannot be tested?
###############

imComplex.lmerBC7SemTEnv <- lmer(bc ~ Environment*SemanticTransparency+BaseInitialStress+ GlobalSpeechRate+
                                   LocSpeech + 
                                   PrePause +AccentuationCondition+ 
                                   (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7SemTEnv )


# no

imComplex.lmerBC7RootEnv  <- lmer(bc ~ Environment*TypeOfRoot+BaseInitialStress+ GlobalSpeechRate+
                                    LocSpeech + 
                                    PrePause+AccentuationCondition+ 
                                    (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RootEnv )
# no

imComplex.lmerBC7RatingEnv  <- lmer(bc ~ Environment*Rating+BaseInitialStress+ GlobalSpeechRate+
                                      LocSpeech + 
                                      PrePause+AccentuationCondition+ 
                                      (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7RatingEnv )

# no



imComplex.lmerBC7EnvAffix<- lmer(bc ~ Environment*Affix+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                      LocSpeech + 
                                      PrePause + 
                                      (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvAffix)


# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                        0.8332242    0.0044718  401.7000000 186.329
# Environmentm#mV                    0.0067838    0.0027582   43.3000000   2.460
# AffixLoc                           0.0072543    0.0033271   42.4000000   2.180
# BaseInitialStressunstressed       -0.0070936    0.0025347   42.3000000  -2.799
# AccentuationConditionunaccented    0.0039498    0.0013750  964.0000000   2.873
# GlobalSpeechRate                  -0.0074470    0.0016316  921.9000000  -4.564
# LocSpeech                         -0.0031175    0.0002831  997.1000000 -11.013
# PrePausepause                      0.0018740    0.0010900 1112.9000000   1.719
# Environmentm#mV:AffixLoc          -0.0115541    0.0055929   41.6000000  -2.066

# Pr(>|t|)    
# (Intercept)                     < 0.0000000000000002 ***
#   Environmentm#mV                              0.01798 *  
# AffixLoc                                     0.03483 *  
#   BaseInitialStressunstressed                  0.00770 ** 
#   AccentuationConditionunaccented              0.00416 ** 
#   GlobalSpeechRate                          0.00000569 ***
#   LocSpeech                       < 0.0000000000000002 ***
#   PrePausepause                                0.08586 .  
# Environmentm#mV:AffixLoc                     0.04510 *  

# yes?

# let's see

visreg(imComplex.lmerBC7EnvAffix,"Environment", by= "Affix", trans= function(x) (x^(1/0.1010101))*1000, rug=F, ylab="duration in milliseconds", xlab="environment by stress", cex.axis=0.9,ylim=c(20,180))


# okay, I think there is something gong on with stress...

# let's see what happens if we have a 3way interactiosn



imComplex.lmerBC7EnvAffixStress<- lmer(bc ~ Environment*Affix*BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                   LocSpeech + 
                                   PrePause + 
                                   (1|Item) + (1|Participant), data = imComplex2)

summary(imComplex.lmerBC7EnvAffixStress)

#no but

# let's first see how many types there are of each category


# Neg double unstressed
unique(imComplex2[imComplex2$BaseInitialStress=="unstressed" &
                    imComplex2$Environment=="m#mV" &
                    imComplex2$Affix== "Neg",c("Item","AffixStress" )])

#       Item AffixStress
# 28  immaterial   secondary
# 53    immature   secondary
# 136 immemorial  unstressed

# Neg double stressed
unique(imComplex2[imComplex2$BaseInitialStress=="primary" &
                    imComplex2$Environment=="m#mV" &
                    imComplex2$Affix== "Neg", c("Item","AffixStress" ) ])


# [1] immaculate   immeasurable immediate    immitigable  immobile     immoderate   immodest    
# [8] immoral      immortal     immotile     immovable    immutable   

# all unstressed affix

#Neg single stressed
unique(imComplex2[imComplex2$BaseInitialStress=="primary" &
                    imComplex2$Environment=="m#C" &
                    imComplex2$Affix== "Neg", c("Item","AffixStress" )])

# [1] impalpable    imparity      impartible    impassible    imperforable  imperishable  implausible  
# [8] impolitic     imponderable  impossible    impracticable impractical   improvident  

# all unstressed affix

# Neg single unstressed
unique(imComplex2[imComplex2$BaseInitialStress=="unstressed" &
                    imComplex2$Environment=="m#C" &
                    imComplex2$Affix== "Neg", c("Item","AffixStress" )])


#             Item    AffixStress
# 632    imperceivable  unstressed
# 657    imperfectible  unstressed
# 725    impermissible  unstressed
# 750    imperturbable  unstressed
# 974        impotence     primary
# 1052       imprecise  unstressed
# 1080 imprescriptible  unstressed

# mostly unstressed!!!


# Loc double stressed
unique(imComplex2[imComplex2$BaseInitialStress=="primary" &
                    imComplex2$Environment=="m#mV" &
                    imComplex2$Affix== "Loc", c("Item","AffixStress" ) ])

#[1] immerse   immixture

# unstressed affix

# Loc double unstressed
unique(imComplex2[imComplex2$BaseInitialStress=="unstressed" &
                    imComplex2$Environment=="m#mV" &
                    imComplex2$Affix== "Loc", c("Item","AffixStress" ) ])

#[1] immigrant imminent 

# primary affix stress

# Loc single stressed
unique(imComplex2[imComplex2$BaseInitialStress=="primary" &
                    imComplex2$Environment=="m#C" &
                    imComplex2$Affix== "Loc", c("Item","AffixStress" )])

#[1] impanel   impassion implicit  implode   imprison 

# unstressed affix

# Loc single unstressed
unique(imComplex2[imComplex2$BaseInitialStress=="unstressed" &
                    imComplex2$Environment=="m#C" &
                    imComplex2$Affix== "Loc", c("Item","AffixStress" )])


#[1] implant import  imprint

# primary !

####### okay, let's see whether this model is better or worse than the other


anova(imComplex.lmerBC7EnvAffix,imComplex.lmerBC7StressAccStressEnv)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 12 -6372.3 -6311.7 3198.2  -6396.3                                        
# ..1    12 -6390.3 -6329.7 3207.2  -6414.3 18.017      0 < 0.00000000000000022 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# the first one is better, we will take that model as our final model (also
# because of the uneven distribution of stress in the first one)


#############
# let's check the interactions with PC

imComplex.lmerBC7EnvPC1 <- lmer(ConsonantDur ~ Environment*PCDec1+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                      LocSpeech + 
                                      PrePause + 
                                      (1|Item) + (1|Participant), data = imComplexRating)

summary(imComplex.lmerBC7EnvPC1)

#no


imComplex.lmerBC7EnvPC2 <- lmer(ConsonantDur ~ Environment*PCDec2+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + 
                                  (1|Item) + (1|Participant), data = imComplexRating)

summary(imComplex.lmerBC7EnvPC2)

#no

imComplex.lmerBC7EnvPC3 <- lmer(ConsonantDur ~ Environment*PCDec3+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + 
                                  (1|Item) + (1|Participant), data = imComplexRating)

summary(imComplex.lmerBC7EnvPC3)
#no

imComplex.lmerBC7EnvPC4 <- lmer(ConsonantDur ~ Environment*PCDec4+BaseInitialStress+AccentuationCondition+ GlobalSpeechRate+
                                  LocSpeech + 
                                  PrePause + 
                                  (1|Item) + (1|Participant), data = imComplexRating)

summary(imComplex.lmerBC7EnvPC4)

#no



##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# I tested teh interactions of variables of interst (RelFreq, Type of Base, Rating,Environment) and Accentuation
# also tested stress and accentuation

# Our final model has two 2-way interactions;

# stress and environment

visreg(imComplex.lmerBC7StressAccStressEnv,"Environment", by= "BaseInitialStress", trans= function(x) (x^(1/0.1010101))*1000, rug=F, ylab="duration in milliseconds", xlab="environment by stress", cex.axis=0.9,ylim=c(20,180))

# when base initial syllable has primary stress doubles are minimally longer

#############################################################
# The final model:

summary(imComplex.lmerBC7StressAccStressEnv)

  lambda
#[1] 0.1010101


# I need to rename some variabels for the plot...


imComplex2<-rename(imComplex2,AccentuationAnnotator=Accentuation)

imComplex2<-rename(imComplex2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(imComplex2$BaseInitialStress)
#[1] "primary"    "unstressed"


levels(imComplex2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(imComplex2$BaseInitialStress)

summary(final_im_complex_model.lmer)
#[1] "stressed"   "unstressed"


# also need to change ref levels for environment

imComplex2$Environment <- relevel (imComplex2$Environment, ref= "m#mV")


final_im_complex_model.lmer<-lmer(bc ~  Environment*BaseInitialStress+BaseInitialStress*Accentuation+LocSpeech+ GlobalSpeechRate+                                              
                                    +(1|Participant)+ (1|Item) , data = imComplex2)                                 


summary(final_im_complex_model.lmer)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)
# (Intercept)                                           0.8440391    0.0041255  343.3000000 204.592 < 0.0000000000000002
# Environmentm#C                                       -0.0100730    0.0024472   42.3000000  -4.116             0.000175
# BaseInitialStressunstressed                          -0.0222799    0.0036784   47.4000000  -6.057          0.000000215
# Accentuationunaccented                                0.0021319    0.0015145 1038.3000000   1.408             0.159529
# LocSpeech                                            -0.0029532    0.0002786  882.2000000 -10.600 < 0.0000000000000002
# GlobalSpeechRate                                     -0.0080156    0.0015356  874.7000000  -5.220          0.000000224
# Environmentm#C:BaseInitialStressunstressed            0.0205754    0.0044653   41.6000000   4.608          0.000038213
# BaseInitialStressunstressed:Accentuationunaccented    0.0042868    0.0018292 1084.0000000   2.344             0.019283
# 
# (Intercept)                                        ***
#   Environmentm#C                                     ***
# BaseInitialStressunstressed                        ***
#   Accentuationunaccented                                
# LocSpeech                                          ***
#   GlobalSpeechRate                                   ***
#   Environmentm#C:BaseInitialStressunstressed         ***
# BaseInitialStressunstressed:Accentuationunaccented *  


visreg(final_im_complex_model.lmer, trans= function(x) (x^(1/0.1010101))*1000, rug=F, ylab="duration in milliseconds", cex.axis=0.9,ylim=c(20,180))



#############
# Let's get the two models for the dissertation


table_final_models<-as.data.frame(coef(summary(final_im_complex_model.lmer)))

xtable(table_final_models,digits = 3)


#############################################################
# Let's now look at each factors contribution to the model
###############################################################

############################################################
# Do we need random effects?
#############################################

# Speaker

imComplex.finalWithoutSpeaker <-lmer(bc ~  Environment*BaseInitialStress+ BaseInitialStress*Accentuation+LocSpeech+                                                 + LocSpeech  + 
                                       GlobalSpeechRate+  
                                          (1|Item) , data = imComplex2)

summary(imComplex.finalWithoutSpeaker)


# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)
# (Intercept)                                           0.8498990    0.0033334  263.4000000 254.965 < 0.0000000000000002
# Environmentm#C                                       -0.0093256    0.0026599   41.8000000  -3.506             0.001100
# BaseInitialStressunstressed                          -0.0214535    0.0040076   47.2000000  -5.353       0.000002496511
# Accentuationunaccented                                0.0033906    0.0014082 1114.6000000   2.408             0.016213
# LocSpeech                                            -0.0036140    0.0002914  914.6000000 -12.404 < 0.0000000000000002
# GlobalSpeechRate                                     -0.0077040    0.0012462 1106.1000000  -6.182       0.000000000889
# Environmentm#C:BaseInitialStressunstressed            0.0192510    0.0048589   41.3000000   3.962             0.000288
# BaseInitialStressunstressed:Accentuationunaccented    0.0044892    0.0020318 1105.7000000   2.209             0.027348
# 
# (Intercept)                                        ***
#   Environmentm#C                                     ** 
# BaseInitialStressunstressed                        ***
#   Accentuationunaccented                             *  
#   LocSpeech                                          ***
#   GlobalSpeechRate                                   ***
#   Environmentm#C:BaseInitialStressunstressed         ***
# BaseInitialStressunstressed:Accentuationunaccented *  

cor(imComplex2$bc, fitted(imComplex.finalWithoutSpeaker))^2
#[1] 0.4476056



imComplex.finalWithoutItem <-lmer(bc ~  Environment*BaseInitialStress+ BaseInitialStress*Accentuation+LocSpeech+                                                 + LocSpeech  + 
                                              GlobalSpeechRate+  
                                              (1|Participant) , data = imComplex2)
summary(imComplex.finalWithoutItem)


# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)
# (Intercept)                                           0.8348599    0.0038504  352.1000000 216.822 < 0.0000000000000002
# Environmentm#C                                       -0.0110896    0.0011318 1122.1000000  -9.799 < 0.0000000000000002
# BaseInitialStressunstressed                          -0.0239884    0.0019037 1118.7000000 -12.601 < 0.0000000000000002
# Accentuationunaccented                               -0.0002985    0.0015770 1084.0000000  -0.189               0.8499
# LocSpeech                                            -0.0021033    0.0002344 1137.1000000  -8.971 < 0.0000000000000002
# GlobalSpeechRate                                     -0.0076162    0.0014640  919.3000000  -5.202          0.000000243
# Environmentm#C:BaseInitialStressunstressed            0.0227552    0.0020419 1118.1000000  11.144 < 0.0000000000000002
# BaseInitialStressunstressed:Accentuationunaccented    0.0046463    0.0019623 1121.1000000   2.368               0.0181
# 
# (Intercept)                                        ***
#   Environmentm#C                                     ***
# BaseInitialStressunstressed                        ***
#   Accentuationunaccented                                
# LocSpeech                                          ***
#   GlobalSpeechRate                                   ***
#   Environmentm#C:BaseInitialStressunstressed         ***
# BaseInitialStressunstressed:Accentuationunaccented * 

cor(imComplex2$bc, fitted(imComplex.finalWithoutItem))^2
#[1] 0.4872115



anova(imComplex.finalWithoutItem,final_im_complex_model.lmer)

# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 10 -6297.3 -6246.8 3158.6  -6317.3                                        
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 93.696      1 < 0.00000000000000022 ***

anova(imComplex.finalWithoutSpeaker,final_im_complex_model.lmer)

# Df     AIC     BIC logLik deviance Chisq Chi Df            Pr(>Chisq)    
# object 10 -6194.7 -6144.2 3107.3  -6214.7                                       
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 196.3      1 < 0.00000000000000022 ***




# Now, let's see how much each factor explains - we will take a look at the ACI for that

# Let's create models in which one of the preditor variables is missing

imComplex.finalWithoutInteraction1 <-lmer(bc ~  Environment+ BaseInitialStress*Accentuation+LocSpeech+ GlobalSpeechRate+                                                + LocSpeech  + 
                                     (1|Item)+
                                    (1|Participant) , data = imComplex2)



imComplex.finalWithoutInteraction2 <-lmer(bc ~  Environment*BaseInitialStress+Accentuation+LocSpeech+ GlobalSpeechRate+                                                + LocSpeech  + 
                                            (1|Item)+
                                            (1|Participant) , data = imComplex2)


imComplex.finalWithoutEnvironment <-lmer(bc ~  BaseInitialStress*Accentuation+LocSpeech+ GlobalSpeechRate+                                                + LocSpeech  + 
                                            (1|Item)+
                                            (1|Participant) , data = imComplex2)


imComplex.finalWithoutStress <-lmer(bc ~  Environment+Accentuation+LocSpeech+ GlobalSpeechRate+                                                + LocSpeech  + 
                                            (1|Item)+
                                            (1|Participant) , data = imComplex2)


imComplex.finalWithoutAccentuation <-lmer(bc ~  Environment*BaseInitialStress+LocSpeech+ GlobalSpeechRate+                                                + LocSpeech  + 
                                      (1|Item)+
                                      (1|Participant) , data = imComplex2)

imComplex.finalWithoutLocSpeech <-lmer(bc ~  Environment*BaseInitialStress+BaseInitialStress*Accentuation+GlobalSpeechRate+                                               
                                            (1|Item)+
                                            (1|Participant) , data = imComplex2)

imComplex.finalWithoutGlobalSpeech <-lmer(bc ~  Environment*BaseInitialStress+BaseInitialStress*Accentuation+LocSpeech+                                                 + LocSpeech  + 
                                         (1|Item)+
                                         (1|Participant) , data = imComplex2)

  ###########################################################################
# Now, let's have a look at the contribution of each factor
###################################################################


anova(imComplex.finalWithoutSpeaker,final_im_complex_model.lmer)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -6194.7 -6144.2 3107.3  -6214.7                                       
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 196.3      1 < 0.00000000000000022 ***

6389.0-6194.7 
#[1] 194.3

anova(imComplex.finalWithoutItem,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 10 -6297.3 -6246.8 3158.6  -6317.3                                        
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 93.696      1 < 0.00000000000000022 ***

6389.0-6297.3 
#91.7

anova(imComplex.finalWithoutInteraction1,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -6372.6 -6322.1 3196.3  -6392.6                             
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 18.338      1 0.00001849 ***
6389.0-6372.6
#16.4

anova(imComplex.finalWithoutInteraction2,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 10 -6385.5 -6335.0 3202.7  -6405.5                           
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 5.5207      1    0.01879 *
# ---
6389.0-6385.5
#3.5

anova(imComplex.finalWithoutEnvironment,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  9 -6372.2 -6326.7 3195.1  -6390.2                             
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 20.817      2 0.00003018 ***

6389.0-6372.2 
#16.8

anova(imComplex.finalWithoutAccentuation,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# object  9 -6380.6 -6335.1 3199.3  -6398.6                            
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 12.428      2   0.002001 **
6389.0-6380.6
#8.4

anova(imComplex.finalWithoutStress,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# object  8 -6364.2 -6323.8 3190.1  -6380.2                               
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 30.742      3 0.0000009631 ***

6389.0-6364.2
#[1] 24.8


anova(imComplex.finalWithoutLocSpeech,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 10 -6285.1 -6234.6 3152.5  -6305.1                                        
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 105.91      1 < 0.00000000000000022 ***
6389.0-6285.1 
#103.9

anova(imComplex.finalWithoutGlobalSpeech,final_im_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# object 10 -6363.9 -6313.4 3192.0  -6383.9                               
# ..1    11 -6389.0 -6333.4 3205.5  -6411.0 27.048      1 0.0000001985 ***
# #   ---
6389.0-6363.9 
#25.1

########################################################
# When we look at the contribution of each factor in the model without
# the interaction, we see the following picture
#######################################################################


# Let's put these numbers in a table

AIC_decrease_imComplex<-matrix(c(194,104,92, 25, 25, 17, 16, 8, 4),ncol=9,byrow=TRUE)
colnames(AIC_decrease_imComplex)<-c("Speaker",  "Local-\nSpeechRate", "Item", "Base-\nInitialStress", 
                                    "Global-\nSpeechRate", "Environment", "Env. * Stress", 
                                    "Accentuation","Stress * Acc.")
rownames(AIC_decrease_imComplex)<-c("Decrease in AIC")
AIC_decrease_imComplex <- as.table(AIC_decrease_imComplex)
AIC_decrease_imComplex


#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



# plot effect sizes


png("AICdecreaseImComplex.png", units="cm", height=10, width=17, res=300, pointsize=09)


par(mar=c(2.6,8.1, 1.1, 2), xpd=TRUE, cex=0.9)

barplot((AIC_decrease_imComplex),horiz=T, col="lightgrey",  names.arg =colnames(AIC_decrease_imComplex), las=2, xaxt="n")

xx<-barplot(AIC_decrease_imComplex, horiz=T, col="lightgrey",names.arg =colnames(AIC_decrease_imComplex), las=2, xaxt="n", border="lightgrey")

text(y = xx, x = AIC_decrease_imComplex ,label = AIC_decrease_imComplex, pos = 4, cex = 0.8, col = "black")

title(xlab="AIC increase", line=0, cex.lab=1.1)

dev.off()

# what is lambda


lambda
#[1] 0.1010101




##############################
# We should also plot the main effect (not covariates)
###############################
# Plot main effect

png("imModelInterEnvStress.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der Estimate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

#visreg(final_im_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylim=ylim,cex.axis=0.9,par.settings=par)


visreg(final_im_complex_model.lmer, "Environment",by="BaseInitialStress",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()




# what is lambda


lambda
#[1] 0.1010101


# so we can see that Environment is the most important factor!!!


library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 


imComplex.lm1<- lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause + PrecSegDur + PCDec1+PCDec2+PCDec3+
       PCDec4, 
   data = imComplexRating)

model_ranking <- dredge(imComplex.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Relative variable importance: 
#   LocSpeech GlobalSpeechRate BaseInitialStress PCDec4 Environment PCDec2 PCDec1
# Importance:          1.00      1.00             1.00              1.00   1.00        0.61   0.57  
# N containing models: 8192      8192             8192              8192   8192        8192   8192  
# PostPause Accentuation logWordFormFreq PCDec3 PrePause PrecSegDur OrderRescale
# Importance:          0.53      0.49         0.39            0.36   0.33     0.28       0.27        
# N containing models: 8192      8192         8192            8192   8192     8192       8192  
# 


# let's try a MuMin with interacrions

ImComplex.lm2<- lm(ConsonantDur ~ Environment*Accentuation*BaseInitialStress + 
                     OrderRescale + logWordFormFreq +  + LocSpeech + 
                     GlobalSpeechRate + Accentuation*BaseInitialStress*PrePause + 
                     PostPause + PrecSegDur + Environment*PCDec1+Environment*PCDec2+Environment*PCDec3+
                     Environment*PCDec4, 
                   data = imComplexRating)

model_ranking2 <- dredge(ImComplex.lm2)

model_average_2<-model.avg(model_ranking2)

summary(model_average_2)
Relative variable importance: 
  BaseInitialStress Environment LocSpeech
Importance:            1.00              1.00        1.00   
N containing models: 304640            331776      175616   
BaseInitialStress:Environment GlobalSpeechRate PCDec4
Importance:            1.00                          1.00             1.00
N containing models: 160704                        175616           230912
Accentuation logWordFormFreq PCDec2 PCDec1
Importance:            0.84         0.75            0.66   0.64
N containing models: 304640       175616          230912 230912
Environment:PCDec4 PostPause PrePause
Importance:            0.58               0.57      0.55  
N containing models: 110592             175616    273536  
Accentuation:Environment Accentuation:BaseInitialStress
Importance:            0.51                     0.49                        
N containing models: 160704                   161664                        
PCDec3 Environment:PCDec1 OrderRescale PrecSegDur
Importance:            0.47   0.43               0.27         0.27    
N containing models: 230912 110592             175616       175616    
Environment:PCDec3 BaseInitialStress:PrePause
Importance:            0.25               0.21                    
N containing models: 110592             135744                    
Environment:PCDec2 Accentuation:PrePause
Importance:            0.21               0.20               
N containing models: 110592             135744               
Accentuation:BaseInitialStress:Environment
Importance:            0.15                                    
N containing models:  31104                                    
Accentuation:BaseInitialStress:PrePause
Importance:            0.03                                 
N containing models:  26944                                 
BaseInitialStress Environment LocSpeech
Importance:            1.00              1.00        1.00   
N containing models: 304640            331776      175616   
BaseInitialStress:Environment GlobalSpeechRate PCDec4
Importance:            1.00                          1.00             1.00
N containing models: 160704                        175616           230912
Accentuation logWordFormFreq PCDec2 PCDec1
Importance:            0.84         0.75            0.66   0.64
N containing models: 304640       175616          230912 230912
Environment:PCDec4 PostPause PrePause
Importance:            0.58               0.57      0.55  
N containing models: 110592             175616    273536  
Accentuation:Environment Accentuation:BaseInitialStress
Importance:            0.51                     0.49                        
N containing models: 160704                   161664                        
PCDec3 Environment:PCDec1 OrderRescale PrecSegDur
Importance:            0.47   0.43               0.27         0.27    
N containing models: 230912 110592             175616       175616    
Environment:PCDec3 BaseInitialStress:PrePause
Importance:            0.25               0.21                    
N containing models: 110592             135744                    
Environment:PCDec2 Accentuation:PrePause
Importance:            0.21               0.20               
N containing models: 110592             135744               
Accentuation:BaseInitialStress:Environment
Importance:            0.15                                    
N containing models:  31104                                    
Accentuation:BaseInitialStress:PrePause
Importance:            0.03                                 
N containing models:  26944 
########## so it seems the decompabiliyt varibales might be important
# in an lm

# let's see

imComplexDC.lm <- lm(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                          BaseInitialStress + LocSpeech + GlobalSpeechRate +
                          PrePause+ PostPause + PrecSegDur+
                          PCDec1 +PCDec2+PCDec3+PCDec4, data = imComplexRating)


imComplexDC.lmReduced <-stepAIC(imComplexDC.lm)

summary (imComplexDC.lmReduced)


# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.1594900  0.0037372  42.677 < 0.0000000000000002 ***
#   Environmentm#mV                  0.0059731  0.0014801   4.036     0.00005802110967 ***
# AccentuationConditionunaccented  0.0054778  0.0016885   3.244              0.00121 ** 
#   logWordFormFreq                  0.0004269  0.0003029   1.410              0.15891    
# BaseInitialStressunstressed     -0.0087833  0.0014802  -5.934     0.00000000389871 ***
#   LocSpeech                       -0.0037908  0.0003212 -11.800 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.0111839  0.0015723  -7.113     0.00000000000198 ***
#   PCDec1                          -0.0007136  0.0004119  -1.732              0.08347 .  
# PCDec2                          -0.0013071  0.0007069  -1.849              0.06470 .  
# PCDec4                          -0.0049865  0.0009970  -5.001     0.00000065710605 ***

# now interactions

imComplexDC.lmInt1 <- lm(ConsonantDur ~ Environment*PCDec1+ AccentuationCondition+  
                       BaseInitialStress + LocSpeech + GlobalSpeechRate,
                     data = imComplexRating)

summary(imComplexDC.lmInt1)

# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.1580676  0.0037219  42.470 < 0.0000000000000002 ***
#   Environmentm#mV                  0.0049380  0.0013799   3.578              0.00036 ***
# PCDec1                           0.0007437  0.0006251   1.190              0.23439    
# AccentuationConditionunaccented  0.0049707  0.0016953   2.932              0.00343 ** 
#   BaseInitialStressunstressed     -0.0065171  0.0014407  -4.524       0.000006697858 ***
#   LocSpeech                       -0.0036427  0.0003234 -11.263 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.0106345  0.0015780  -6.739       0.000000000025 ***
#   Environmentm#mV:PCDec1          -0.0023909  0.0008254  -2.897              0.00384 ** 
# Residual standard error: 0.02276 on 1167 degrees of freedom
# Multiple R-squared:  0.2667,	Adjusted R-squared:  0.2623 
# F-statistic: 60.65 on 7 and 1167 DF,  p-value: < 0.00000000000000022



imComplexDC.lmInt2 <- lm(ConsonantDur ~ Environment*PCDec2+ AccentuationCondition+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate,
                         data = imComplexRating)

summary(imComplexDC.lmInt2)

# no

imComplexDC.lmInt3 <- lm(ConsonantDur ~ Environment*PCDec2+ AccentuationCondition+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate,
                         data = imComplexRating)

summary(imComplexDC.lmInt3)

# no

imComplexDC.lmInt4<- lm(ConsonantDur ~ Environment*PCDec4+ AccentuationCondition+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate,
                         data = imComplexRating)

summary(imComplexDC.lmInt4)
# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.158499   0.003723  42.578 < 0.0000000000000002 ***
#   Environmentm#mV                  0.005978   0.001431   4.179   0.0000315163125176 ***
# PCDec4                          -0.007544   0.001173  -6.430   0.0000000001860211 ***
#   AccentuationConditionunaccented  0.005307   0.001673   3.172              0.00156 ** 
#   BaseInitialStressunstressed     -0.007907   0.001458  -5.422   0.0000000716150779 ***
#   LocSpeech                       -0.003567   0.000320 -11.148 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.011633   0.001544  -7.534   0.0000000000000986 ***
#   Environmentm#mV:PCDec4           0.008481   0.002101   4.036   0.0000578188129685 ***

# Residual standard error: 0.02246 on 1167 degrees of freedom
# Multiple R-squared:  0.2856,	Adjusted R-squared:  0.2813 
# F-statistic: 66.65 on 7 and 1167 DF,  p-value: < 0.00000000000000022

visreg(imComplexDC.lmInt4, "Environment", by="PCDec4")

visreg(imComplexDC.lmInt1, "Environment", by="PCDec1")

# The model with the second interaction is better, but what if we have
# both interactions?

imComplexDC.lmInt14<- lm(ConsonantDur ~ Environment*PCDec4+Environment*PCDec1+ AccentuationCondition+  
                          BaseInitialStress + LocSpeech + GlobalSpeechRate,
                        data = imComplexRating)

summary(imComplexDC.lmInt14)


# Call:
#   lm(formula = ConsonantDur ~ Environment * PCDec4 + Environment * 
#        PCDec1 + AccentuationCondition + BaseInitialStress + LocSpeech + 
#        GlobalSpeechRate, data = imComplexRating)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069597 -0.013360 -0.002118  0.011033  0.121017 
# 
# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.1582560  0.0037099  42.658 < 0.0000000000000002 ***
#   Environmentm#mV                  0.0060440  0.0014257   4.239    0.000024187888296 ***
# PCDec4                          -0.0074778  0.0011692  -6.396    0.000000000231178 ***
#   PCDec1                           0.0007070  0.0006151   1.149              0.25060    
# AccentuationConditionunaccented  0.0054655  0.0016715   3.270              0.00111 ** 
#   BaseInitialStressunstressed     -0.0074265  0.0014607  -5.084    0.000000429843185 ***
#   LocSpeech                       -0.0035274  0.0003222 -10.947 < 0.0000000000000002 ***
#   GlobalSpeechRate                -0.0118271  0.0015629  -7.567    0.000000000000077 ***
#   Environmentm#mV:PCDec4           0.0086529  0.0020941   4.132    0.000038534806618 ***
# Environmentm#mV:PCDec1          -0.0023223  0.0008126  -2.858              0.00434 ** 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02238 on 1165 degrees of freedom
# Multiple R-squared:  0.292,	Adjusted R-squared:  0.2866 
# F-statistic: 53.39 on 9 and 1165 DF,  p-value: < 0.00000000000000022


# bith are signifiant....


# what about one interaction and the ither as a "nromal factor"


imComplexDC.lmInt1_4<- lm(ConsonantDur ~ PCDec4+Environment*PCDec1+ AccentuationCondition+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate,
                         data = imComplexRating)

summary(imComplexDC.lmInt1_4)

# model is worse

imComplexDC.lmInt4_1<- lm(ConsonantDur ~ Environment*PCDec4+PCDec1+ AccentuationCondition+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate,
                         data = imComplexRating)

summary(imComplexDC.lmInt4_1)


# no effect of Dec 1

# what if we have the interaction as in the lmer, okay


imComplexDC.lmInt5<- lm(ConsonantDur ~ Environment*BaseInitialStress+ PCDec4*Environment+ AccentuationCondition  
                             + LocSpeech + GlobalSpeechRate,
                          data = imComplexRating)

summary(imComplexDC.lmInt5)


# (Intercept)                                                  0.1540065  0.0036274  42.456 < 0.0000000000000002
# Environmentm#mV                                              0.0156953  0.0016507   9.508 < 0.0000000000000002
# BaseInitialStressunstressed                                 -0.0019264  0.0022011  -0.875              0.38165
# PCDec4                                                      -0.0056126  0.0009587  -5.855 0.000000006216035258
# PCDec1                                                       0.0010322  0.0005967   1.730              0.08393
# AccentuationConditionunaccented                              0.0027757  0.0018783   1.478              0.13974
# LocSpeech                                                   -0.0031745  0.0003137 -10.121 < 0.0000000000000002
# GlobalSpeechRate                                            -0.0125322  0.0015151  -8.271 0.000000000000000359
# Environmentm#mV:BaseInitialStressunstressed                 -0.0273916  0.0028833  -9.500 < 0.0000000000000002
# Environmentm#mV:PCDec1                                      -0.0020600  0.0007868  -2.618              0.00896
# BaseInitialStressunstressed:AccentuationConditionunaccented  0.0073854  0.0027284   2.707              0.00689
# 
# (Intercept)                                                 ***
#   Environmentm#mV                                             ***
# BaseInitialStressunstressed                                    
# PCDec4                                                      ***
#   PCDec1                                                      .  
# AccentuationConditionunaccented                                
# LocSpeech                                                   ***
#   GlobalSpeechRate                                            ***
#   Environmentm#mV:BaseInitialStressunstressed                 ***
# Environmentm#mV:PCDec1                                      ** 
# BaseInitialStressunstressed:AccentuationConditionunaccented ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02167 on 1164 degrees of freedom
# Multiple R-squared:  0.3369,	Adjusted R-squared:  0.3312 
# F-statistic: 59.14 on 10 and 1164 DF,  p-value: < 0.00000000000000022

# so the best lm model has thre interactions
visreg(imComplexDC.lmInt5)

visreg(imComplexDC.lmInt5, "Environment", by="BaseInitialStress")

visreg(imComplexDC.lmInt5, "BaseInitialStress", by="AccentuationCondition")


visreg(imComplexDC.lmInt5, "Environment", by="PCDec1")
visreg(imComplexDC.lmInt5, "PCDec1", by="Environment")

visreg(imComplexDC.lmInt5, "PCDec4")

#############
# I'll not include this in the dissertation, the effect sizes are so small
# and it is just onfusing,,,
############################################

###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

summary(final_im_complex_model.lmer)


# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                           0.8440391    0.0041255  343.3000000 204.592
# Environmentm#C                                       -0.0100730    0.0024472   42.3000000  -4.116
# BaseInitialStressunstressed                          -0.0222799    0.0036784   47.4000000  -6.057
# Accentuationunaccented                                0.0021319    0.0015145 1038.3000000   1.408
# LocSpeech                                            -0.0029532    0.0002786  882.2000000 -10.600
# GlobalSpeechRate                                     -0.0080156    0.0015356  874.7000000  -5.220
# Environmentm#C:BaseInitialStressunstressed            0.0205754    0.0044653   41.6000000   4.608
# BaseInitialStressunstressed:Accentuationunaccented    0.0042868    0.0018292 1084.0000000   2.344

visreg(final_im_complex_model.lmer)


# Environment: m#C
# Accentuation: accented
# LocSpeech: 11.78276
# GlobalSpeechRate: 2.314826
# Participant: Experiment_1_participant_2
# Item: immoderate


intercept =  0.8440391

LocCondition= 11.78276
estSpeech= -0.0029532 

GlobCondition= 2.314826
estGlobal= -0.0080156

EstEnvironmentimC= -0.0100730

EstUnstressed=-0.0222799 

InteractionEst= 0.0205754

visreg(final_im_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylim=ylim,cex.axis=0.9,par.settings=par)

#level imm stressed
((intercept+(LocCondition*estSpeech)+(GlobCondition*estGlobal))^(1/lambda))*1000
#[1] 97.77964

immStressed= 97.77964

#level imC stressed
((intercept+(LocCondition*estSpeech)+(GlobCondition*estGlobal)+EstEnvironmentimC)^(1/lambda))*1000
#[1] 86.1237

imCStressed= 86.1237

#level imC Unstressed
((intercept+(LocCondition*estSpeech)+(GlobCondition*estGlobal)+EstUnstressed+EstEnvironmentimC + InteractionEst)^(1/lambda))*1000
#[1] 84.27995

imCUnstressed= 84.27995

#level imm Unstressed
((intercept+(LocCondition*estSpeech)+(GlobCondition*estGlobal)+EstUnstressed)^(1/lambda))*1000
#[1] 73.68134

immUnstressed= 73.68134

# im1terschiede:

# Double single accented

immStressed-imCStressed
#11.65594

immUnstressed-imCUnstressed
#-10.59861

################
# Need some distributional information, unstressed - singletons vs doubles (Affix stress)

# we need a subset

imComplexUnstressedBase<-imComplex2[imComplex2$BaseInitialStress=="unstressed",]

# the problem cases are primarily stressed on the affix


imComplexUnstressedBase[imComplexUnstressedBase$AffixStress=="Problem","AffixStress"]<-"primary"



table(imComplexUnstressedBase$Environment,imComplexUnstressedBase$AffixStress)
#         primary Problem secondary unstressed
# m#mV      52       0        51         19
# m#C      106       0         0        135

# no
