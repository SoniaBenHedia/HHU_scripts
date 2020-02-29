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


###########################################################################
# I will start with the complex dataset, and thus will need the complex dataset-
# In the following I will first take a look at the pertinent varoables
# and then fit a model
############################################################################



InComplex <- read.csv("InComplex.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(InComplex)

# 'data.frame':	1155 obs. of  83 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 ...
# $ Item                       : Factor w/ 50 levels "inact","inappeasable",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 29 levels "Experiment_1_participant_10",..: 20 24 22 4 27 10 25 12 21 18 ...
# $ ID                         : int  3201 4144 4340 651 5011 1773 4506 2030 3556 2989 ...
# $ Filename                   : Factor w/ 1155 levels "Participant_10_10.TextGrid",..: 746 908 840 148 1065 380 947 489 831 695 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 2 levels "Accented","Unaccented": 2 1 2 2 1 1 1 1 1 1 ...
# $ Annotator                  : Factor w/ 5 levels "Lara","Mandy",..: 1 2 5 5 4 1 4 3 3 2 ...
# $ Order                      : int  10 40 60 157 258 165 34 284 303 286 ...
# $ WordDur                    : num  0.418 0.763 0.329 0.418 0.617 ...
# $ SyllNum                    : int  2 2 2 2 2 2 2 2 2 2 ...
# $ SegNum                     : int  6 5 5 5 5 5 5 5 5 5 ...
# $ ConsonantDur               : num  0.0517 0.0656 0.0568 0.0439 0.0615 ...
# $ PrecSeg                    : Factor w/ 5 levels "@","{","i","I",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ PrecSegVC                  : Factor w/ 1 level "V": 1 1 1 1 1 1 1 1 1 1 ...
# $ PrecSegDur                 : num  0.0561 0.0663 0.0449 0.0378 0.1358 ...
# $ FollSeg                    : Factor w/ 37 levels "@","@U","{","{kt",..: 3 3 3 3 5 3 3 3 3 3 ...
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
# $ Environment                : Factor w/ 3 levels "n#C","n#nV","n#V": 3 3 3 3 3 3 3 3 3 3 ...
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

InComplex$X.1<-NULL
InComplex$X<-NULL

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

# Let's see whether it makes sense to include the decomposability measures (only
# makes sense if we have variability)

########################################################################
# The decomposability measures(I need to analyze them and their relations
# in a separate analysis before I can decide how to procede...)

# probabaly influence of each individual variable and the PC

# 1. Semantic Transparency

table(InComplex$SemanticTransparency)
# opaque transparent 
#  193         962 




# 2. Type of Base

table(InComplex$TypeOfBase)
# bound  word 
# 138  1017


table(InComplex$TypeOfBase,InComplex$SemanticTransparency)
#         opaque transparent
# bound    121          17
# word      72         945


#yeah....


# 3. Rating

table(InComplex$Rating)
# 1    2    3    4 
# 724 143 114 137 




table(InComplex$Rating,InComplex$TypeOfBase)
#     bound word
# 1    24  700
# 2    13  130
# 3    28   86
# 4    68   69


(table(InComplex$Rating,InComplex$SemanticTransparency))
#     opaque transparent
# 1     43         681
# 2     22         121
# 3     43          71
# 4     79          58


pairscor.fnc(InComplex [, c("SemanticTransparency", "TypeOfBase","Rating","logRelFreq","Affix")])

# we find high correlations, so we will test each variable's influence individually
# and do a PC: We will do that later


######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = InComplex)
cor(InComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.0808807


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = InComplex)
cor(InComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.5948826

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = InComplex)
cor(InComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.6772357

# so aroIn1d 68 percent of the variability can be explained by this! That's a lot. Most is explained
# by item

##########################################################
##              Do an initial model:
InComplex$OrderRescale<-InComplex$Order*0.1

InComplex.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + PrecSegDur+
                          (1|Item) + (1|Participant), data = InComplex)


summary(InComplex.lmer1)    

# Linear mixed model fit by REML 
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: ConsonantDur ~ Environment + AccentuationCondition + OrderRescale +  
#   logWordFormFreq + BaseInitialStress + LocSpeech + GlobalSpeechRate +  
#   PrePause + PostPause + PrecSegDur + (1 | Item) + (1 | Participant)
# Data: InComplex
# 
# REML criterion at convergence: -5892.1
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.6310 -0.6218 -0.0737  0.5098  6.2639 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 6.734e-05 0.008206
# Participant (Intercept) 4.221e-05 0.006497
# Residual                2.819e-04 0.016789
# Number of obs: 1155, groups:  Item, 50; Participant, 29
# 
# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      1.428e-01  6.445e-03  5.141e+02  22.152   <2e-16 ***
#   Environmentn#nV                 -1.271e-02  4.939e-03  4.580e+01  -2.574   0.0134 *  
#   Environmentn#V                  -3.942e-02  3.068e-03  4.850e+01 -12.846   <2e-16 ***
#   AccentuationConditionunaccented  1.846e-03  1.563e-03  8.947e+02   1.181   0.2378    
#   OrderRescale                    -1.477e-05  5.615e-05  1.106e+03  -0.263   0.7926    
#   logWordFormFreq                  5.819e-04  5.287e-04  4.660e+01   1.101   0.2767    
#   BaseInitialStressunstressed     -2.939e-03  2.853e-03  4.620e+01  -1.030   0.3083    
#   LocSpeech                       -2.973e-03  3.321e-04  1.056e+03  -8.953   <2e-16 ***
#   GlobalSpeechRate                -5.127e-03  1.845e-03  7.390e+02  -2.778   0.0056 ** 
#   PrePausePause                    1.836e-03  1.251e-03  1.115e+03   1.468   0.1424    
#   PostPausePause                  -3.308e-03  1.406e-03  1.118e+03  -2.352   0.0188 *  
#   PrecSegDur                      -6.144e-02  2.767e-02  1.115e+03  -2.221   0.0266 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envrnmntn#nV Envirnmntn#V AccntC OrdrRs lgWrFF BsIntS LcSpch GlblSR PrPsPs PstPsP
# Envrnmntn#nV -0.132                                                                                  
# Envirnmntn#V -0.127  0.259                                                                           
# AccnttnCndt   0.251  0.004        0.015                                                              
# OrderRescal  -0.156  0.002        0.005       -0.018                                                 
# lgWrdFrmFrq  -0.266 -0.009        0.214        0.072 -0.026                                          
# BsIntlStrss  -0.204  0.039       -0.387       -0.026  0.005  0.079                                   
# LocSpeech    -0.498  0.026       -0.159       -0.034  0.044  0.056  0.066                            
# GloblSpchRt  -0.421 -0.019        0.033       -0.570 -0.003 -0.119  0.008 -0.326                     
# PrePausePas  -0.226 -0.006       -0.010       -0.023  0.078 -0.040 -0.008 -0.055  0.218              
# PostPausePs  -0.390  0.005       -0.021        0.207 -0.052 -0.002  0.029  0.148  0.249 -0.042       
# PrecSegDur   -0.463 -0.028       -0.089       -0.039 -0.035  0.000 -0.005  0.302  0.035  0.015  0.073

cor(InComplex$ConsonantDur, fitted(InComplex.lmer1))^2
#[1] 0.7130087


#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slInming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own


# 1.logWordFormFreq & logRelFreq

# Model woth both 
InComplex.lmerFrequencies <- lmer(ConsonantDur ~ logWordFormFreq+ logRelFreq+ (1|Item) + (1|Participant), data = InComplex)

summary(InComplex.lmerFrequencies)    


# Fixed effects:
#                     Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)     6.367e-02  5.318e-03 5.549e+01  11.974   <2e-16 ***
#   logWordFormFreq 2.398e-03  1.291e-03 4.699e+01   1.857   0.0696 .  
#   logRelFreq      1.298e-03  8.644e-04 4.686e+01   1.502   0.1398    


cor(InComplex$ConsonantDur, fitted(InComplex.lmerFrequencies))^2
#[1] 0.6771709


# only Word Form Freq

InComplex.lmerWordFrequency <- lmer(ConsonantDur ~ logWordFormFreq + (1|Item) + (1|Participant), data = InComplex)

summary(InComplex.lmerWordFrequency)    

# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)      0.060762   0.005009 57.630000  12.130   <2e-16 ***
#   logWordFormFreq  0.002934   0.001257 47.960000   2.334   0.0238 *  

cor(InComplex$ConsonantDur, fitted(InComplex.lmerWordFrequency))^2
#[1] 0.6771858


# only RelFreq
InComplex.lmerRelFrequency <- lmer(ConsonantDur ~ logRelFreq+  (1|Item) + (1|Participant), data = InComplex)

summary(InComplex.lmerRelFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept) 7.091e-02  3.690e-03 6.442e+01  19.215   <2e-16 ***
#   logRelFreq  1.742e-03  8.519e-04 4.791e+01   2.045   0.0463 *  

cor(InComplex$ConsonantDur, fitted(InComplex.lmerRelFrequency))^2
#[1]0.6772068




#####################################
# Summary Coll. Frequencies:
# So, both factors become signficant when they are the only variables in the model.
# When both are in the model, none is significant. However, they do not supress each other
#################################################


# 2.  Loc Speech  and/or Global Speech


cor.test(InComplex$LocSpeech,InComplex$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  InComplex$LocSpeech and InComplex$GlobalSpeechRate
# t = 19.953, df = 1153, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4624632 0.5482893
# sample estimates:
#       cor 
# 0.5066303 

InComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.078e-01  4.635e-03  2.062e+02  23.262   <2e-16 ***
#   LocSpeech        -2.753e-03  3.262e-04  1.143e+03  -8.439   <2e-16 ***
#   GlobalSpeechRate -2.312e-03  1.324e-03  1.096e+03  -1.745   0.0812 .  
# 

cor(InComplex$ConsonantDur, fitted(InComplex.lmerSpeechRates))^2
#[1]  0.7110499



InComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.068e-01  4.577e-03  2.047e+02   23.34   <2e-16 ***
#   LocSpeech   -3.103e-03  2.585e-04  1.114e+03  -12.00   <2e-16 ***

cor(InComplex$ConsonantDur, fitted(InComplex.lmerLocSpeech))^2
#[1] 0.709937


#options(scipen=999)
InComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = InComplex)

print(summary(InComplex.lmerGlobalSpeech),digits=6)

# Fixed effects:
#   Estimate   Std. Error           df  t value   Pr(>|t|)    
# (Intercept)       8.98628e-02  4.39950e-03  1.28712e+02 20.42568 < 2.22e-16 ***
#   GlobalSpeechRate -9.13923e-03  1.08004e-03  1.04235e+03 -8.46196 < 2.22e-16 ***

cor(InComplex$ConsonantDur, fitted(InComplex.lmerGlobalSpeech))^2
#[1] 0.6959133


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, only Loc is sign.
# - The effect size of LocSpeech decreases when it is the only variable in the model
# - The effect size of GlobalSpeech increases when it is the only variable in the model and it
#   becomes sign
# - The effect direction never changes (no supression)
# - Not a big difference in R2 between the different models
#################################################


##############################################################
# The decomposability variables

############
#Rating

InComplex.lmerRating<- lmer(ConsonantDur ~ Rating + (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerRating)
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 6.769e-02  3.886e-03 8.080e+01  17.416   <2e-16 ***
#   Rating      8.675e-04  7.174e-04 1.100e+03   1.209    0.227   
# not significant


########
#Type of base

InComplex.lmerTypeOfBase<- lmer(ConsonantDur ~ TypeOfBase + (1|Item)+(1|Participant), data = InComplex)


summary(InComplex.lmerTypeOfBase)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)     0.083603   0.008872 51.770000   9.423 8.11e-13 ***
#   TypeOfBaseword -0.016918   0.009417 48.700000  -1.797   0.0786 .  

# not significant

######################
# RelFreq
InComplex.lmerRelFreq<- lmer(ConsonantDur ~ logRelFreq+ (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerRelFreq)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 7.091e-02  3.690e-03 6.442e+01  19.215   <2e-16 ***
#   logRelFreq  1.742e-03  8.519e-04 4.791e+01   2.045   0.0463 *   

#  significant!!!!


##############
# Semantic Trans


InComplex.lmerST<- lmer(ConsonantDur ~ SemanticTransparency+ (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerST)


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      0.084134   0.007729 52.360000   10.89 4.66e-15 ***
#   SemanticTransparencytransparent -0.018397   0.008361 48.400000   -2.20   0.0326 * 

#  significant


##############
# Affix


InComplex.lmerAffix<- lmer(ConsonantDur ~ Affix+ (1|Item)+(1|Participant), data = InComplex)

summary(InComplex.lmerAffix)


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  0.086030   0.006459 53.820000  13.320  < 2e-16 ***
#   AffixNeg    -0.022350   0.007194 48.230000  -3.107  0.00317 **   

# significant

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################
# Now we have dealt with all collinearity problems: 
# - We will keep both frequency variables even though they are never significanr
# - We will keep both Speech Rate variables but must be aware of the fact that their effect
#   size cannot be interpreted!
# # ST, Affix and RelFreq are significant when being the only variable in the model
#   - This needs to be investigated later on (after simplification)
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

levels(InComplex$Affix)
#[1] "Loc" "Neg"


InComplex$Affix <- relevel (InComplex$Affix, ref= "Neg")

InComplex$NumAffix<-as.numeric(InComplex$Affix)

table(InComplex$Affix,InComplex$NumAffix)

#        1   2
# Neg 885   0
# Loc   0 270

# good


#Type pf base

levels(InComplex$TypeOfBase)
#[1] "bound" "word"     

InComplex$TypeOfBase <- relevel (InComplex$TypeOfBase, ref= "word"   )

InComplex$NumTypeOfBase<-as.numeric(InComplex$TypeOfBase)

table(InComplex$TypeOfBase,InComplex$NumTypeOfBase)
#       1    2
# word  1017    0
# bound    0  138


#Smenatic Transparency

levels(InComplex$SemanticTransparency)
#[1] "opaque"      "transparent"

InComplex$SemanticTransparency <- relevel (InComplex$SemanticTransparency, ref= "transparent"   )

InComplex$NumSemanticTransparency<-as.numeric(InComplex$SemanticTransparency)

table(InComplex$SemanticTransparency,InComplex$NumSemanticTransparency)
#              1   2
# transparent 962   0
# opaque        0 193

# one further problem is that the variables are on 
# different scales - so we need to change this

InComplex$ScaledSemanticTransparency<-as.numeric(scale(InComplex$NumSemanticTransparency))
summary(InComplex$ScaledSemanticTransparency)

InComplex$ScaledRating<-as.numeric(scale(InComplex$Rating))
summary(InComplex$ScaledRating)

InComplex$ScaledTypeOfBase<-as.numeric(scale(InComplex$NumTypeOfBase))
summary(InComplex$ScaledTypeOfBase)

InComplex$ScaledAffix<-as.numeric(scale(InComplex$NumAffix))
summary(InComplex$ScaledAffix)

InComplex$ScaledRelFreq<-as.numeric(scale(InComplex$logRelFreq))
summary(InComplex$ScaledRelFreq)



# so now they are scaled

# there are quite a  observations without a rating....in order to do the PC analysis we need
# a new data set...

InComplexRating<-InComplex[!is.na(InComplex$Rating),]

decomposability.pc <- prcomp(InComplexRating[, c("ScaledAffix", "ScaledRelFreq","ScaledRating","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability.pc)

# Importance of components:
#                         PC1    PC2    PC3     PC4    PC5
# Standard deviation     1.7703 0.8659 0.7785 0.61866 0.3677
# Proportion of Variance 0.6258 0.1497 0.1210 0.07643 0.0270
# Cumulative Proportion  0.6258 0.7755 0.8966 0.97300 1.0000

# so first three are the most Inportant ones

decomposability.pc$rotation
#                               PC1         PC2         PC3         PC4         PC5
# ScaledAffix                -0.4568198  0.59087172  0.02229637 -0.32412304 -0.58020117
# ScaledRelFreq              -0.3904374 -0.72367825 -0.34726269 -0.39095894 -0.22451755
# ScaledRating               -0.3913149 -0.25190402  0.88195871  0.03624244  0.06520998
# ScaledTypeOfBase           -0.4689925 -0.03729805 -0.24236801  0.83598594 -0.14505235
# ScaledSemanticTransparency -0.5155511  0.24962698 -0.20571359 -0.20471777  0.76659373

# PC1: Reall all measures
# PC 2: also mixture, except fot ype of base
# PC 3_ Rating, RelFreq, Type of Base
# PC4: Affix, SemTrans



xtable(decomposability.pc$rotation, digits=3)

# let's try out a model with PC1 and PC 2 and PC 3 as predictor variables

InComplexRating$PCDec1 <- decomposability.pc$x[, 1]
InComplexRating$PCDec2 <- decomposability.pc$x[, 2]
InComplexRating$PCDec3 <- decomposability.pc$x[, 3]
InComplexRating$PCDec4 <- decomposability.pc$x[, 4]

pairscor.fnc(InComplexRating [, c("PCDec1", "PCDec2", "PCDec3", "PCDec4", "Affix", 
                                  "logRelFreq", "SemanticTransparency", "Rating", "TypeOfBase")])


# let's see whether this has an influence


In_Complex_PC.lmer<-lmer(ConsonantDur ~ PCDec1+PCDec2+PCDec3+PCDec4 + 
                         (1|Item) + (1|Participant), data = InComplexRating)


summary(In_Complex_PC.lmer)

# Fixed effects:
#                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)  0.0690166  0.0035393 63.0400000  19.500  < 2e-16 ***
#   PCDec1      -0.0049812  0.0017281 46.4700000  -2.882  0.00596 ** 
#   PCDec2       0.0014721  0.0035565 46.2000000   0.414  0.68085    
#   PCDec3      -0.0007769  0.0014962 87.8100000  -0.519  0.60489    
#   PCDec4      -0.0051314  0.0049615 46.2900000  -1.034  0.30639    
# ---


# maybe PC 1: I guess that means we need to make a whole analysis, i.e. stepweise exludision....

#######################################################
# PS Model:


InComplexPC.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                          BaseInitialStress + LocSpeech + GlobalSpeechRate +
                          PrePause+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                          (1|Item) + (1|Participant), data = InComplexRating)


summary(InComplexPC.lmer1)    

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.442e-01  6.554e-03  5.068e+02  22.002  < 2e-16 ***
#   Environmentn#nV                 -9.958e-03  5.267e-03  4.410e+01  -1.891  0.06523 .  
# Environmentn#V                  -4.073e-02  3.200e-03  4.420e+01 -12.731 2.22e-16 ***
# AccentuationConditionunaccented  2.430e-03  1.606e-03  8.538e+02   1.513  0.13072    
# OrderRescale                    -1.774e-05  5.752e-05  1.068e+03  -0.308  0.75788    
# logWordFormFreq                  7.210e-04  5.300e-04  4.470e+01   1.360  0.18055    
# BaseInitialStressunstressed     -3.619e-03  2.927e-03  4.390e+01  -1.236  0.22296    
# LocSpeech                       -2.944e-03  3.456e-04  1.098e+03  -8.521  < 2e-16 ***
#   GlobalSpeechRate                -5.655e-03  1.900e-03  7.074e+02  -2.977  0.00301 ** 
#   PrePausePause                    1.910e-03  1.273e-03  1.073e+03   1.500  0.13384    
# PostPausePause                  -3.283e-03  1.437e-03  1.079e+03  -2.286  0.02247 *  
#   PrecSegDur                      -6.546e-02  2.841e-02  1.076e+03  -2.304  0.02143 *  
#   PCDec1                           9.224e-04  7.651e-04  4.730e+01   1.206  0.23398    
# PCDec2                           2.359e-03  1.602e-03  4.740e+01   1.472  0.14750    
# PCDec3                           1.165e-03  9.168e-04  2.252e+02   1.270  0.20532    




###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (InComplexPC.lmer1))
qqline (residuals (InComplexPC.lmer1))

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


InComplexBC.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                     BaseInitialStress + LocSpeech + GlobalSpeechRate +
                     PrePause+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3,
                   data = InComplexRating)

summary(InComplexBC.lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      1.481e-01  5.595e-03  26.461  < 2e-16 ***
#   Environmentn#nV                 -8.530e-03  2.538e-03  -3.361 0.000803 ***
# Environmentn#V                  -4.022e-02  1.536e-03 -26.186  < 2e-16 ***
# AccentuationConditionunaccented  1.621e-03  1.575e-03   1.029 0.303504    
# OrderRescale                    -9.412e-06  6.423e-05  -0.147 0.883517    
# logWordFormFreq                  7.331e-04  2.539e-04   2.887 0.003960 ** 
#   BaseInitialStressunstressed     -3.860e-03  1.388e-03  -2.780 0.005530 ** 
#   LocSpeech                       -3.111e-03  3.443e-04  -9.038  < 2e-16 ***
#   GlobalSpeechRate                -5.850e-03  1.652e-03  -3.542 0.000414 ***
#   PrePausePause                    1.116e-03  1.346e-03   0.830 0.406961    
# PostPausePause                  -6.384e-03  1.501e-03  -4.253 2.28e-05 ***
#   PrecSegDur                      -5.644e-02  2.957e-02  -1.909 0.056541 .  
# PCDec1                           1.103e-03  3.863e-04   2.856 0.004373 ** 
#   PCDec2                           2.682e-03  8.225e-04   3.262 0.001142 ** 
#   PCDec3                           8.398e-04  7.641e-04   1.099 0.272033 

bc<-boxcox(InComplexBC.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.06060606

InComplexRating$bc <- InComplexRating$ConsonantDur^lambda

########## Let's redo the model


InComplexPC.lmer2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating)


summary(InComplexPC.lmer2)    

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.976e-01  4.566e-03  5.396e+02 196.613  < 2e-16 ***
#   Environmentn#nV                 -7.756e-03  3.539e-03  4.420e+01  -2.192  0.03371 *  
# Environmentn#V                  -2.987e-02  2.150e-03  4.420e+01 -13.894  < 2e-16 ***
# AccentuationConditionunaccented  7.489e-04  1.127e-03  9.070e+02   0.664  0.50660    
# OrderRescale                    -2.109e-05  4.006e-05  1.068e+03  -0.527  0.59862    
# logWordFormFreq                  3.430e-04  3.562e-04  4.470e+01   0.963  0.34082    
# BaseInitialStressunstressed     -4.272e-03  1.967e-03  4.390e+01  -2.172  0.03528 *  
#   LocSpeech                       -1.980e-03  2.404e-04  1.095e+03  -8.234 4.44e-16 ***
#   GlobalSpeechRate                -2.534e-03  1.336e-03  7.857e+02  -1.897  0.05819 .  
# PrePausePause                    1.372e-03  8.881e-04  1.076e+03   1.545  0.12265    
# PostPausePause                  -2.378e-03  1.002e-03  1.082e+03  -2.373  0.01781 *  
#   PrecSegDur                      -5.192e-02  1.982e-02  1.080e+03  -2.619  0.00894 ** 
#   PCDec1                           5.185e-04  5.147e-04  4.750e+01   1.007  0.31882    
# PCDec2                           1.476e-03  1.078e-03  4.770e+01   1.369  0.17729    
# PCDec3                           5.371e-04  6.292e-04  2.380e+02   0.854  0.39417  


###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (InComplexPC.lmer2))
qqline (residuals (InComplexPC.lmer2))



# looks better but we need to exclude outliers


outliers<-romr.fnc(InComplexPC.lmer2, InComplexRating, trim = 2.5)
# n.removed = 21 
# percent.removed = 1.878354 

InComplexRating2<-outliers$data

dim(InComplexRating2)
#[1] 1097   96

dim(InComplexRating)
#[1] 1118   95

# okay it worked, now let's refit the model


InComplexPC.lmer3 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer3)  

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.956e-01  4.229e-03  5.081e+02 211.789  < 2e-16 ***
#   Environmentn#nV                 -8.396e-03  3.388e-03  4.410e+01  -2.478  0.01711 *  
# Environmentn#V                  -3.019e-02  2.052e-03  4.360e+01 -14.717  < 2e-16 ***
# AccentuationConditionunaccented  5.776e-04  1.040e-03  9.371e+02   0.555  0.57874    
# OrderRescale                    -3.329e-05  3.618e-05  1.042e+03  -0.920  0.35779    
# logWordFormFreq                  3.274e-04  3.399e-04  4.410e+01   0.963  0.34068    
# BaseInitialStressunstressed     -3.399e-03  1.879e-03  4.350e+01  -1.809  0.07732 .  
# LocSpeech                       -1.901e-03  2.201e-04  1.073e+03  -8.638  < 2e-16 ***
#   GlobalSpeechRate                -2.106e-03  1.233e-03  8.729e+02  -1.709  0.08787 .  
# PrePausePause                    6.170e-04  8.040e-04  1.051e+03   0.767  0.44302    
# PostPausePause                  -1.847e-03  9.191e-04  1.058e+03  -2.009  0.04475 *  
#   PrecSegDur                      -4.780e-02  1.800e-02  1.058e+03  -2.655  0.00806 ** 
#   PCDec1                           5.263e-04  4.901e-04  4.650e+01   1.074  0.28847    
# PCDec2                           1.472e-03  1.027e-03  4.670e+01   1.434  0.15831    
# PCDec3                           5.970e-04  5.811e-04  2.149e+02   1.027  0.30535 


qqnorm (residuals (InComplexPC.lmer3))
qqline (residuals (InComplexPC.lmer3))

# looks good. let's now simplify the model

###############
# Simplification of PC-model

# first, let's throw out Accentuation

InComplexPC.lmer4 <- lmer(bc ~ Environment+  OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer4)  


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.950e-01  4.077e-03  4.924e+02 219.549  < 2e-16 ***
#   Environmentn#nV             -8.419e-03  3.387e-03  4.410e+01  -2.486  0.01680 *  
# Environmentn#V              -3.021e-02  2.051e-03  4.360e+01 -14.730  < 2e-16 ***
# OrderRescale                -3.313e-05  3.617e-05  1.044e+03  -0.916  0.35989    
# logWordFormFreq              3.142e-04  3.390e-04  4.360e+01   0.927  0.35917    
# BaseInitialStressunstressed -3.364e-03  1.877e-03  4.340e+01  -1.792  0.08011 .  
# LocSpeech                   -1.897e-03  2.199e-04  1.073e+03  -8.628  < 2e-16 ***
#   GlobalSpeechRate            -1.710e-03  1.005e-03  1.068e+03  -1.702  0.08910 .  
# PrePausePause                6.272e-04  8.036e-04  1.052e+03   0.781  0.43524    
# PostPausePause              -1.959e-03  8.967e-04  1.061e+03  -2.184  0.02916 *  
#   PrecSegDur                  -4.733e-02  1.798e-02  1.059e+03  -2.633  0.00859 ** 
#   PCDec1                       5.282e-04  4.901e-04  4.650e+01   1.078  0.28669    
# PCDec2                       1.458e-03  1.026e-03  4.670e+01   1.420  0.16215    
# PCDec3                       5.839e-04  5.804e-04  2.143e+02   1.006  0.31553  


# good, nothing has changed, next let's throw out PrePause



InComplexPC.lmer5 <- lmer(bc ~ Environment+  OrderRescale +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            logWordFormFreq+ PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer5)  


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.957e-01  3.980e-03  4.636e+02 225.029  < 2e-16 ***
#   Environmentn#nV             -8.367e-03  3.387e-03  4.400e+01  -2.470  0.01746 *  
# Environmentn#V              -3.016e-02  2.051e-03  4.350e+01 -14.709  < 2e-16 ***
# OrderRescale                -3.544e-05  3.604e-05  1.044e+03  -0.983  0.32574    
# BaseInitialStressunstressed -3.371e-03  1.878e-03  4.340e+01  -1.795  0.07959 .  
# LocSpeech                   -1.886e-03  2.193e-04  1.075e+03  -8.598  < 2e-16 ***
#   GlobalSpeechRate            -1.907e-03  9.725e-04  1.060e+03  -1.961  0.05009 .  
# logWordFormFreq              3.237e-04  3.389e-04  4.360e+01   0.955  0.34478    
# PostPausePause              -1.934e-03  8.959e-04  1.062e+03  -2.159  0.03107 *  
#   PrecSegDur                  -4.746e-02  1.798e-02  1.060e+03  -2.640  0.00841 ** 
#   PCDec1                       5.130e-04  4.898e-04  4.630e+01   1.047  0.30034    
# PCDec2                       1.489e-03  1.026e-03  4.650e+01   1.451  0.15337    
# PCDec3                       5.941e-04  5.803e-04  2.140e+02   1.024  0.30712   
# good,now PC 1


# good, nothing has changed, next let's throw out Freq



InComplexPC.lmer6 <- lmer(bc ~ Environment+  OrderRescale +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PostPause + PrecSegDur+PCDec1+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer6)  


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.968e-01  3.813e-03  6.540e+02 235.216  < 2e-16 ***
#   Environmentn#nV             -8.434e-03  3.388e-03  4.520e+01  -2.490  0.01653 *  
# Environmentn#V              -3.052e-02  2.016e-03  4.470e+01 -15.137  < 2e-16 ***
# OrderRescale                -3.444e-05  3.603e-05  1.046e+03  -0.956  0.33927    
# BaseInitialStressunstressed -3.476e-03  1.875e-03  4.470e+01  -1.854  0.07039 .  
# LocSpeech                   -1.897e-03  2.190e-04  1.075e+03  -8.665  < 2e-16 ***
#   GlobalSpeechRate            -1.818e-03  9.679e-04  1.066e+03  -1.878  0.06063 .  
# PostPausePause              -1.909e-03  8.954e-04  1.064e+03  -2.132  0.03324 *  
#   PrecSegDur                  -4.747e-02  1.797e-02  1.060e+03  -2.641  0.00838 ** 
#   PCDec1                       4.700e-04  4.878e-04  4.760e+01   0.964  0.34015    
# PCDec2                       1.419e-03  1.024e-03  4.770e+01   1.386  0.17220    
# PCDec3                       5.584e-04  5.792e-04  2.211e+02   0.964  0.33601    




# good, nothing has changed, next let's throw out Pc1



InComplexPC.lmer7<- lmer(bc ~ Environment+  OrderRescale +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PostPause + PrecSegDur+PCDec2+PCDec3+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer7)  


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.962e-01  3.759e-03  6.620e+02 238.408  < 2e-16 ***
#   Environmentn#nV             -8.594e-03  3.381e-03  4.630e+01  -2.542  0.01445 *  
# Environmentn#V              -2.993e-02  1.920e-03  4.770e+01 -15.588  < 2e-16 ***
# OrderRescale                -3.344e-05  3.601e-05  1.047e+03  -0.929  0.35328    
# BaseInitialStressunstressed -3.429e-03  1.873e-03  4.560e+01  -1.831  0.07368 .  
# LocSpeech                   -1.864e-03  2.162e-04  1.043e+03  -8.620  < 2e-16 ***
#   GlobalSpeechRate            -1.906e-03  9.636e-04  1.067e+03  -1.978  0.04820 *  
#   PostPausePause              -1.914e-03  8.954e-04  1.064e+03  -2.137  0.03281 *  
#   PrecSegDur                  -4.688e-02  1.796e-02  1.062e+03  -2.610  0.00919 ** 
#   PCDec2                       1.384e-03  1.022e-03  4.890e+01   1.354  0.18192    
# PCDec3                       3.971e-04  5.543e-04  3.717e+02   0.716  0.47413  



# good, nothing has changed, next let's throw out Pc3



InComplexPC.lmer8<- lmer(bc ~ Environment+  OrderRescale +
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PostPause + PrecSegDur+PCDec2+
                           (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer8)  
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                  8.963e-01  3.754e-03  6.619e+02 238.770  < 2e-16 ***
#   Environmentn#nV             -9.001e-03  3.327e-03  4.880e+01  -2.706  0.00936 ** 
#   Environmentn#V              -2.997e-02  1.916e-03  4.810e+01 -15.637  < 2e-16 ***
#   OrderRescale                -3.431e-05  3.599e-05  1.048e+03  -0.953  0.34064    
#   BaseInitialStressunstressed -3.317e-03  1.863e-03  4.690e+01  -1.780  0.08149 .  
#   LocSpeech                   -1.877e-03  2.153e-04  1.040e+03  -8.717  < 2e-16 ***
#   GlobalSpeechRate            -1.885e-03  9.629e-04  1.067e+03  -1.958  0.05048 .  
#   PostPausePause              -1.902e-03  8.951e-04  1.064e+03  -2.125  0.03378 *  
#   PrecSegDur                  -4.678e-02  1.796e-02  1.063e+03  -2.605  0.00932 ** 
#   PCDec2                       1.078e-03  9.266e-04  8.610e+01   1.164  0.24784    
# 


# good, nothing has changed, next let's throw out Order



InComplexPC.lmer9<- lmer(bc ~ Environment+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PostPause + PrecSegDur+PCDec2+
                           (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer9)  

# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.958e-01  3.711e-03  6.469e+02 241.406  < 2e-16 ***
#   Environmentn#nV             -8.990e-03  3.324e-03  4.880e+01  -2.705  0.00939 ** 
# Environmentn#V              -2.995e-02  1.915e-03  4.810e+01 -15.640  < 2e-16 ***
# BaseInitialStressunstressed -3.301e-03  1.862e-03  4.690e+01  -1.773  0.08267 .  
# LocSpeech                   -1.869e-03  2.151e-04  1.040e+03  -8.685  < 2e-16 ***
#   GlobalSpeechRate            -1.911e-03  9.625e-04  1.068e+03  -1.985  0.04740 *  
#   PostPausePause              -1.933e-03  8.944e-04  1.066e+03  -2.161  0.03091 *  
#   PrecSegDur                  -4.774e-02  1.793e-02  1.064e+03  -2.662  0.00788 ** 
#   PCDec2                       1.075e-03  9.260e-04  8.610e+01   1.161  0.24891   


# good, nothing has changed, next let's throw out Pc2



InComplexPC.lmer10<- lmer(bc ~ Environment+  
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PostPause + PrecSegDur+
                           (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer10)  


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.957e-01  3.717e-03  6.472e+02 240.970  < 2e-16 ***
#   Environmentn#nV             -1.024e-02  3.184e-03  4.660e+01  -3.216  0.00236 ** 
# Environmentn#V              -3.012e-02  1.932e-03  4.940e+01 -15.593  < 2e-16 ***
# BaseInitialStressunstressed -2.798e-03  1.832e-03  4.700e+01  -1.527  0.13353    
# LocSpeech                   -1.902e-03  2.134e-04  9.998e+02  -8.916  < 2e-16 ***
#   GlobalSpeechRate            -1.766e-03  9.544e-04  1.064e+03  -1.850  0.06458 .  
# PostPausePause              -1.908e-03  8.940e-04  1.067e+03  -2.134  0.03303 *  
#   PrecSegDur                  -4.775e-02  1.793e-02  1.064e+03  -2.663  0.00785 ** 



# good, nothing has changed, next let's throw out stress



InComplexPC.lmer11<- lmer(bc ~ Environment+  
                            + LocSpeech + GlobalSpeechRate +
                            PostPause + PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer11) 


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       8.946e-01  3.652e-03  6.797e+02 244.972  < 2e-16 ***
#   Environmentn#nV  -1.004e-02  3.225e-03  4.740e+01  -3.114  0.00312 ** 
# Environmentn#V   -3.134e-02  1.779e-03  4.950e+01 -17.616  < 2e-16 ***
# LocSpeech        -1.882e-03  2.132e-04  1.001e+03  -8.826  < 2e-16 ***
#   GlobalSpeechRate -1.767e-03  9.552e-04  1.067e+03  -1.850  0.06466 .  
# PostPausePause   -1.863e-03  8.937e-04  1.068e+03  -2.084  0.03739 *  
#   PrecSegDur       -4.798e-02  1.793e-02  1.063e+03  -2.676  0.00756 ** 


anova(InComplexPC.lmer10,InComplexPC.lmer11)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    10 -6707.9 -6657.9 3364.0  -6727.9                         
# object 11 -6708.4 -6653.4 3365.2  -6730.4 2.4503      1     0.1175

# good!

# Now Global Speech Rate



InComplexPC.lmer12<- lmer(bc ~ Environment+  
                            + LocSpeech + 
                            PostPause + PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer12) 

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      8.921e-01  3.393e-03  5.823e+02 262.894  < 2e-16 ***
#   Environmentn#nV -1.019e-02  3.213e-03  4.730e+01  -3.170  0.00267 ** 
# Environmentn#V  -3.109e-02  1.768e-03  4.900e+01 -17.585  < 2e-16 ***
# LocSpeech       -2.047e-03  1.936e-04  1.070e+03 -10.570  < 2e-16 ***
#   PostPausePause  -1.042e-03  7.777e-04  1.077e+03  -1.339  0.18075    
# PrecSegDur      -4.785e-02  1.795e-02  1.064e+03  -2.665  0.00781 ** 


anova(InComplexPC.lmer11,InComplexPC.lmer12)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1     9 -6706.5 -6661.5 3362.2  -6724.5                           
# object 10 -6707.9 -6657.9 3364.0  -6727.9 3.4208      1    0.06438 .


# hm okay, there is a connection between pause and speech rate - makes sense
# let's see what happens if we also throw out pause
# and what happens if we throw put pause but leave speech arte in


InComplexPC.lmer13<- lmer(bc ~ Environment+  
                            + LocSpeech + 
                             PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      8.901e-01  3.069e-03  4.954e+02 290.032  < 2e-16 ***
#   Environmentn#nV -1.013e-02  3.215e-03  4.730e+01  -3.150  0.00283 ** 
# Environmentn#V  -3.122e-02  1.766e-03  4.870e+01 -17.680  < 2e-16 ***
# LocSpeech       -1.928e-03  1.730e-04  1.088e+03 -11.147  < 2e-16 ***
#   PrecSegDur      -4.595e-02  1.790e-02  1.064e+03  -2.566  0.01041 *  



anova(InComplexPC.lmer12,InComplexPC.lmer13)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1     8 -6706.7 -6666.7 3361.3  -6722.7                         
# object  9 -6706.5 -6661.5 3362.2  -6724.5 1.8072      1     0.1788

#ä okay no difference here

InComplexPC.lmer14<- lmer(bc ~ Environment+  GlobalSpeechRate
                            + LocSpeech + 
                            PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer14)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       8.905e-01  3.106e-03  5.170e+02 286.734  < 2e-16 ***
#   Environmentn#nV  -1.004e-02  3.224e-03  4.740e+01  -3.115  0.00312 ** 
# Environmentn#V   -3.139e-02  1.779e-03  4.950e+01 -17.646  < 2e-16 ***
# GlobalSpeechRate -7.857e-04  8.317e-04  1.076e+03  -0.945  0.34499    
# LocSpeech        -1.814e-03  2.112e-04  9.936e+02  -8.589  < 2e-16 ***
#   PrecSegDur       -4.534e-02  1.791e-02  1.064e+03  -2.531  0.01151 *  


# okay, not even significant. So this would be our final
# PC model without interactiosn


summary(InComplexPC.lmer13)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      8.901e-01  3.069e-03  4.954e+02 290.032  < 2e-16 ***
#   Environmentn#nV -1.013e-02  3.215e-03  4.730e+01  -3.150  0.00283 ** 
# Environmentn#V  -3.122e-02  1.766e-03  4.870e+01 -17.680  < 2e-16 ***
# LocSpeech       -1.928e-03  1.730e-04  1.088e+03 -11.147  < 2e-16 ***
#   PrecSegDur      -4.595e-02  1.790e-02  1.064e+03  -2.566  0.01041 *  

# now we need to check for interactions! Note PC is sign!

#############################################
# Interactions PC-Analyses
#############################################


# 1. Environment and PCs

# 2. Env and Stress and Accentuation

# 3. Stress and PC

# 4. Acc and PC


# 1. Environment and PCs


InComplexPC.lmer13EnvPC1<- lmer(bc ~ Environment*PCDec1+  
                            + LocSpeech + 
                            PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvPC1)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)             8.908e-01  3.081e-03  5.430e+02 289.133  < 2e-16 ***
#   Environmentn#nV        -5.878e-03  3.094e-03  4.510e+01  -1.900  0.06385 .  
#   Environmentn#V         -3.101e-02  1.631e-03  4.550e+01 -19.011  < 2e-16 ***
#   PCDec1                  4.861e-04  5.446e-04  5.980e+01   0.893  0.37567    
#   LocSpeech              -1.948e-03  1.739e-04  1.085e+03 -11.204  < 2e-16 ***
#   PrecSegDur             -4.631e-02  1.793e-02  1.066e+03  -2.583  0.00992 ** 
#   Environmentn#nV:PCDec1  3.958e-03  1.385e-03  6.040e+01   2.858  0.00584 ** 
#   Environmentn#V:PCDec1  -1.478e-03  8.982e-04  5.640e+01  -1.646  0.10537    

visreg(InComplexPC.lmer13EnvPC1, "PCDec1", by="Environment")


# because of distribution not valid

InComplexPC.lmer13EnvPC2<- lmer(bc ~ Environment*PCDec2+  
                            + LocSpeech + 
                            PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvPC2)

visreg(InComplexPC.lmer13EnvPC2, "PCDec2", by="Environment")
# okay same thing! but effect is the other way around? Really, no!

InComplexPC.lmer13EnvPC3<- lmer(bc ~ Environment*PCDec3+  
                            + LocSpeech + 
                            PrecSegDur+
                            (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvPC3)
# No


# SO - the interactions do not makes sense to consider

# 2. Env and Stress and Accentuation

InComplexPC.lmer13EnvStress<- lmer(bc ~ Environment*BaseInitialStress+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvStress)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                  8.877e-01  3.014e-03  4.831e+02 294.493  < 2e-16 ***
#   Environmentn#nV                             -3.004e-03  2.760e-03  4.140e+01  -1.088  0.28270    
# Environmentn#V                              -2.406e-02  2.045e-03  4.330e+01 -11.765 4.44e-15 ***
# BaseInitialStressunstressed                  6.383e-03  2.167e-03  4.350e+01   2.946  0.00516 ** 
#   LocSpeech                                   -1.889e-03  1.701e-04  1.025e+03 -11.105  < 2e-16 ***
#   PrecSegDur                                  -4.641e-02  1.785e-02  1.073e+03  -2.600  0.00946 ** 
#   Environmentn#nV:BaseInitialStressunstressed -2.753e-02  5.548e-03  4.570e+01  -4.961 1.02e-05 ***
# Environmentn#V:BaseInitialStressunstressed  -1.351e-02  2.886e-03  4.290e+01  -4.683 2.85e-05 ***


visreg(InComplexPC.lmer13EnvStress, "BaseInitialStress", by="Environment")
visreg(InComplexPC.lmer13EnvStress, "Environment", by="BaseInitialStress")
 # okay this makes perfect sense. Let's see whether this model is better than 
# the other


anova(InComplexPC.lmer13,InComplexPC.lmer13EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  8 -6706.7 -6666.7 3361.3  -6722.7                             
# ..1    11 -6732.1 -6677.1 3377.0  -6754.1 31.372      3  7.096e-07 ***

#yes!

InComplexPC.lmer13EnvAcc<- lmer(bc ~ Environment*AccentuationCondition+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvAcc)
# 
# (Intercept)                                      8.907e-01  3.179e-03  5.078e+02 280.136  < 2e-16 ***
#   Environmentn#nV                                 -8.249e-03  3.452e-03  6.280e+01  -2.389  0.01989 *  
# Environmentn#V                                  -3.293e-02  1.897e-03  6.440e+01 -17.360  < 2e-16 ***
# AccentuationConditionunaccented                 -1.619e-03  1.118e-03  1.054e+03  -1.448  0.14783    
# LocSpeech                                       -1.902e-03  1.970e-04  1.056e+03  -9.658  < 2e-16 ***
#   PrecSegDur                                      -4.691e-02  1.788e-02  1.062e+03  -2.624  0.00882 ** 
#   Environmentn#nV:AccentuationConditionunaccented -3.700e-03  2.522e-03  1.019e+03  -1.467  0.14273    
# Environmentn#V:AccentuationConditionunaccented   3.371e-03  1.354e-03  1.023e+03   2.490  0.01295 *  

# yes! 


visreg(InComplexPC.lmer13EnvAcc, "AccentuationCondition", by="Environment")
visreg(InComplexPC.lmer13EnvAcc, "Environment", by="AccentuationCondition")

# okay this makes perfect sense but effect is weak


anova(InComplexPC.lmer13EnvAcc,InComplexPC.lmer13EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -6712.5 -6657.5 3367.3  -6734.5                             
# ..1    11 -6732.1 -6677.1 3377.0  -6754.1 19.552      0  < 2.2e-16 ***


# other model is better!

InComplexPC.lmer13StressAcc<- lmer(bc ~ Environment+AccentuationCondition*BaseInitialStress+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13StressAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  8.923e-01  3.265e-03  4.775e+02 273.318  < 2e-16 ***
#   Environmentn#nV                                             -1.023e-02  3.183e-03  4.660e+01  -3.213  0.00238 ** 
# Environmentn#V                                              -2.997e-02  1.926e-03  4.900e+01 -15.560  < 2e-16 ***
# AccentuationConditionunaccented                             -2.099e-03  1.014e-03  1.051e+03  -2.070  0.03872 *  
#   BaseInitialStressunstressed                                 -4.656e-03  1.942e-03  5.920e+01  -2.397  0.01969 *  
#   LocSpeech                                                   -1.945e-03  1.971e-04  1.060e+03  -9.870  < 2e-16 ***
#   PrecSegDur                                                  -4.908e-02  1.792e-02  1.063e+03  -2.740  0.00625 ** 
#   AccentuationConditionunaccented:BaseInitialStressunstressed  3.916e-03  1.287e-03  1.023e+03   3.044  0.00239 **


visreg(InComplexPC.lmer13StressAcc, "AccentuationCondition", by="BaseInitialStress")
visreg(InComplexPC.lmer13StressAcc, "BaseInitialStress", by="AccentuationCondition")

# okay this makes perfect sense but effect is weak


anova(InComplexPC.lmer13StressAcc,InComplexPC.lmer13EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -6712.2 -6657.2 3367.1  -6734.2                             
# ..1    11 -6732.1 -6677.1 3377.0  -6754.1 19.834      0  < 2.2e-16 ***

# other model is clearly better


# What about a 3-way interaction


InComplexPC.lmer13EnvStressAcc<- lmer(bc ~ Environment*AccentuationCondition*BaseInitialStress+  
                                     + LocSpeech + 
                                     PrecSegDur+
                                     (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvStressAcc)

anova(InComplexPC.lmer13EnvStressAcc,InComplexPC.lmer13EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# ..1    11 -6732.1 -6677.1 3377.0  -6754.1                            
# object 17 -6739.6 -6654.6 3386.8  -6773.6 19.536      6   0.003348 **

# what about 2 2 way interactions?


# 1. Stress and Env and Stress and Acc

InComplexPC.lmer13EnvStressEnvAcc<- lmer(bc ~ Environment*AccentuationCondition+ Environment*BaseInitialStress+  
                                        + LocSpeech + 
                                        PrecSegDur+
                                        (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvStressEnvAcc)

visreg(InComplexPC.lmer13EnvStressEnvAcc, "Environment", by ="AccentuationCondition", overlay=T)

visreg(InComplexPC.lmer13EnvStressEnvAcc, "Environment", by ="BaseInitialStress", overlay=T)

anova(InComplexPC.lmer13EnvStressAcc,InComplexPC.lmer13EnvStressEnvAcc)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    14 -6738.5 -6668.5 3383.3  -6766.5                           
# object 17 -6739.6 -6654.6 3386.8  -6773.6 7.0678      3    0.06977 .

# same 


# 2. Stress and Env, Stress and Acc
InComplexPC.lmer13EnvStressStressAcc<- lmer(bc ~ Environment*AccentuationCondition+ AccentuationCondition*BaseInitialStress+  
                                           + LocSpeech + 
                                           PrecSegDur+
                                           (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvStressStressAcc)

# not sign

# okay this seems to be the best model so far

visreg(InComplexPC.lmer13StressAcc, "Environment", by="BaseInitialStress",
       overlay=T,cond=list(AccentuationCondition="accented"))

visreg(InComplexPC.lmer13StressAcc, "Environment", by="BaseInitialStress",
       overlay=T,cond=list(AccentuationCondition="unaccented"))



# 3. Stress and PC


InComplexPC.lmer13StressPC1<- lmer(bc ~ Environment+BaseInitialStress*PCDec1+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13StressPC1)
# no

InComplexPC.lmer13StressPC2<- lmer(bc ~ Environment+BaseInitialStress*PCDec2+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13StressPC2)
# no


InComplexPC.lmer13StressPC3<- lmer(bc ~ Environment+BaseInitialStress*PCDec3+  
                                  + LocSpeech + 
                                  PrecSegDur+
                                  (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13StressPC3)

#no

# 4. Acc and PC


InComplexPC.lmer13AccPC1<- lmer(bc ~ Environment+AccentuationCondition*PCDec1+  
                                     + LocSpeech + 
                                     PrecSegDur+
                                     (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13AccPC1)

anova(InComplexPC.lmer13EnvStressAcc,InComplexPC.lmer13AccPC1)
# other model is clearly better

InComplexPC.lmer13AccPC2<- lmer(bc ~ Environment+AccentuationCondition*PCDec2+  
                                     + LocSpeech + 
                                     PrecSegDur+
                                     (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13AccPC2)

#no

InComplexPC.lmer13AccPC3<- lmer(bc ~ Environment+AccentuationCondition*PCDec3+  
                                     + LocSpeech + 
                                     PrecSegDur+
                                     (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13AccPC3)
# no



##################
# SO NO EFFECT OF SEGEMENTABILIYT IN TERMS OF PC
# Final model would be either 


summary(InComplexPC.lmer13EnvStressAcc)

# or

summary(InComplexPC.lmer13EnvStressEnvAcc)



# we need to (as in the final model) change reference level etc. - probably the 3-way
# interaction will then vanish


InComplexRating2<-rename(InComplexRating2,AccentuationAnnotator=Accentuation)

InComplexRating2<-rename(InComplexRating2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(InComplexRating2$BaseInitialStress)
#[1] "prInary"    "unstressed"


levels(InComplexRating2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(InComplexRating2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# also need to change ref levels for environment

InComplexRating2$Environment <- relevel (InComplexRating2$Environment, ref= "n#nV")


# So, let's refit


InComplexPC.lmer13EnvStressAcc2<- lmer(bc ~ Environment*Accentuation*BaseInitialStress+  
                                        + LocSpeech + 
                                        PrecSegDur+
                                        (1|Item) + (1|Participant), data = InComplexRating2)


summary(InComplexPC.lmer13EnvStressAcc2)

# yep its gone, so the following the the final model:


InComplexPC.Final<- lmer(bc ~ Environment*Accentuation+ Environment*BaseInitialStress+  
                                           + LocSpeech + 
                                           PrecSegDur+
                                           (1|Item) + (1|Participant), data = InComplexRating2)

summary(InComplexPC.Final)

# Fixed effects:
#                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                 8.872e-01  3.978e-03  2.074e+02 223.012  < 2e-16 ***
#   Environmentn#C                              9.451e-04  3.023e-03  6.090e+01   0.313 0.755602    
#   Environmentn#V                             -2.482e-02  3.220e-03  5.820e+01  -7.707 1.86e-10 ***
#   Accentuationunaccented                     -5.614e-03  2.325e-03  1.025e+03  -2.415 0.015915 *  
#   BaseInitialStressunstressed                -2.132e-02  5.087e-03  4.650e+01  -4.191 0.000123 ***
#   LocSpeech                                  -1.849e-03  1.926e-04  9.329e+02  -9.598  < 2e-16 ***
#   PrecSegDur                                 -4.719e-02  1.782e-02  1.071e+03  -2.649 0.008201 ** 
#   Environmentn#C:Accentuationunaccented       3.866e-03  2.523e-03  1.019e+03   1.532 0.125712    
#   Environmentn#V:Accentuationunaccented       7.281e-03  2.458e-03  1.019e+03   2.963 0.003122 ** 
#   Environmentn#C:BaseInitialStressunstressed  2.785e-02  5.515e-03  4.570e+01   5.050 7.53e-06 ***
#   Environmentn#V:BaseInitialStressunstressed  1.421e-02  5.428e-03  4.590e+01   2.617 0.011960 *  


#############
# Let's get the two models for the dissertation


table_final_model_PC<-as.data.frame(coef(summary(InComplexPC.Final)))

xtable(table_final_model_PC,digits = 3)


########################
# Normal model
##############################################################

# Let's refit our model incorportaing the "right variables"

InComplex.lmer3 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                           Affix+ (1|Item) + (1|Participant), data = InComplex)

(summary(InComplex.lmer3))

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.424e-01  6.614e-03  4.775e+02  21.528  < 2e-16 ***
#   Environmentn#nV                 -1.254e-02  5.020e-03  4.490e+01  -2.498  0.01621 *  
# Environmentn#V                  -3.913e-02  3.247e-03  4.560e+01 -12.052 8.88e-16 ***
# AccentuationConditionunaccented  1.857e-03  1.563e-03  8.929e+02   1.188  0.23518    
# OrderRescale                    -1.437e-05  5.618e-05  1.105e+03  -0.256  0.79813    
# logWordFormFreq                  5.686e-04  5.355e-04  4.540e+01   1.062  0.29391    
# BaseInitialStressunstressed     -3.030e-03  2.895e-03  4.490e+01  -1.046  0.30098    
# LocSpeech                       -2.952e-03  3.405e-04  1.138e+03  -8.670  < 2e-16 ***
#   GlobalSpeechRate                -5.206e-03  1.866e-03  7.422e+02  -2.791  0.00539 ** 
#   PrePausePause                    1.816e-03  1.252e-03  1.111e+03   1.450  0.14730    
# PostPausePause                  -3.310e-03  1.406e-03  1.117e+03  -2.354  0.01876 *  
#   PrecSegDur                      -6.130e-02  2.768e-02  1.113e+03  -2.214  0.02701 *  
#   AffixLoc                         9.774e-04  3.315e-03  4.950e+01   0.295  0.76935    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 




###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (InComplex.lmer3))
qqline (residuals (InComplex.lmer3))

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


InComplex.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause + PrecSegDur+Affix, data = InComplex)

summary(InComplex.lm)

# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      1.457e-01  5.635e-03  25.859  < 2e-16 ***
#   Environmentn#nV                 -1.142e-02  2.338e-03  -4.882 1.20e-06 ***
#   Environmentn#V                  -3.888e-02  1.515e-03 -25.657  < 2e-16 ***
#   AccentuationConditionunaccented  1.378e-03  1.544e-03   0.892 0.372405    
#   OrderRescale                     7.333e-06  6.327e-05   0.116 0.907749    
#   logWordFormFreq                  5.344e-04  2.468e-04   2.165 0.030571 *  
#   BaseInitialStressunstressed     -2.767e-03  1.329e-03  -2.082 0.037572 *  
#   LocSpeech                       -3.090e-03  3.394e-04  -9.105  < 2e-16 ***
#   GlobalSpeechRate                -5.576e-03  1.616e-03  -3.450 0.000582 ***
#   PrePausePause                    7.492e-04  1.315e-03   0.570 0.569017    
#   PostPausePause                  -6.095e-03  1.479e-03  -4.121 4.04e-05 ***
#   PrecSegDur                      -4.985e-02  2.906e-02  -1.715 0.086534 .  
#   AffixLoc                         1.097e-03  1.673e-03   0.656 0.512086  

bc<-boxcox(InComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.06060606

InComplex$bc <- InComplex$ConsonantDur^lambda

InComplex.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause + PrecSegDur+
                            Affix+ (1|Item) + (1|Participant), data = InComplex)

summary(InComplex.lmerBC)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      8.963e-01  4.603e-03  5.150e+02 194.696  < 2e-16 ***
#   Environmentn#nV                 -9.303e-03  3.356e-03  4.490e+01  -2.772  0.00807 ** 
#   Environmentn#V                  -2.891e-02  2.171e-03  4.560e+01 -13.320  < 2e-16 ***
#   AccentuationConditionunaccented  3.597e-04  1.099e-03  9.458e+02   0.327  0.74346    
#   OrderRescale                    -1.422e-05  3.921e-05  1.105e+03  -0.363  0.71699    
#   logWordFormFreq                  2.306e-04  3.580e-04  4.540e+01   0.644  0.52283    
#   BaseInitialStressunstressed     -3.946e-03  1.935e-03  4.480e+01  -2.039  0.04738 *  
#   LocSpeech                       -1.976e-03  2.374e-04  1.134e+03  -8.325 2.22e-16 ***
#   GlobalSpeechRate                -2.244e-03  1.314e-03  8.204e+02  -1.708  0.08795 .  
#   PrePausePause                    1.290e-03  8.753e-04  1.114e+03   1.473  0.14095    
#   PostPausePause                  -2.347e-03  9.827e-04  1.121e+03  -2.388  0.01710 *  
#   PrecSegDur                      -4.881e-02  1.935e-02  1.117e+03  -2.522  0.01180 *  
#   AffixLoc                         1.040e-03  2.221e-03  4.990e+01   0.468  0.64158  


#let's check the assumptions

qqnorm (residuals (InComplex.lmerBC))
qqline (residuals (InComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(InComplex.lmerBC, InComplex, trim = 2.5)
# n.removed = 22 
# percent.removed = 1.904762 

InComplex2<-outliers$data

dim(InComplex2)
#[1] 1133   92

dim(InComplex)
#[1] 1155   91


# okay it seemes to have worked

InComplex.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             Affix+ (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC2)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      8.948e-01  4.275e-03  4.760e+02 209.319  < 2e-16 ***
#   Environmentn#nV                 -9.955e-03  3.234e-03  4.470e+01  -3.078  0.00355 ** 
#   Environmentn#V                  -2.925e-02  2.085e-03  4.480e+01 -14.024  < 2e-16 ***
#   AccentuationConditionunaccented  1.063e-04  1.012e-03  9.780e+02   0.105  0.91636    
#   OrderRescale                    -2.500e-05  3.536e-05  1.078e+03  -0.707  0.47977    
#   logWordFormFreq                  2.117e-04  3.440e-04  4.470e+01   0.616  0.54131    
#   BaseInitialStressunstressed     -3.146e-03  1.862e-03  4.430e+01  -1.690  0.09808 .  
#   LocSpeech                       -1.908e-03  2.172e-04  1.112e+03  -8.787  < 2e-16 ***
#   GlobalSpeechRate                -1.885e-03  1.212e-03  9.102e+02  -1.555  0.12019    
#   PrePausePause                    5.287e-04  7.920e-04  1.089e+03   0.668  0.50458    
#   PostPausePause                  -1.970e-03  9.017e-04  1.096e+03  -2.184  0.02916 *  
#   PrecSegDur                      -4.640e-02  1.755e-02  1.093e+03  -2.644  0.00831 ** 
#   AffixLoc                         1.018e-03  2.129e-03  4.870e+01   0.478  0.63469           0.26011

qqnorm (residuals (InComplex.lmerBC2))
qqline (residuals (InComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> InComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      SInplification of the model                                      #
#########################################################################################

summary(InComplex.lmerBC2)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      8.948e-01  4.275e-03  4.760e+02 209.319  < 2e-16 ***
#   Environmentn#nV                 -9.955e-03  3.234e-03  4.470e+01  -3.078  0.00355 ** 
#   Environmentn#V                  -2.925e-02  2.085e-03  4.480e+01 -14.024  < 2e-16 ***
#   AccentuationConditionunaccented  1.063e-04  1.012e-03  9.780e+02   0.105  0.91636    
#   OrderRescale                    -2.500e-05  3.536e-05  1.078e+03  -0.707  0.47977    
#   logWordFormFreq                  2.117e-04  3.440e-04  4.470e+01   0.616  0.54131    
#   BaseInitialStressunstressed     -3.146e-03  1.862e-03  4.430e+01  -1.690  0.09808 .  
#   LocSpeech                       -1.908e-03  2.172e-04  1.112e+03  -8.787  < 2e-16 ***
#   GlobalSpeechRate                -1.885e-03  1.212e-03  9.102e+02  -1.555  0.12019    
#   PrePausePause                    5.287e-04  7.920e-04  1.089e+03   0.668  0.50458    
#   PostPausePause                  -1.970e-03  9.017e-04  1.096e+03  -2.184  0.02916 *  
#   PrecSegDur                      -4.640e-02  1.755e-02  1.093e+03  -2.644  0.00831 ** 
#   AffixLoc                         1.018e-03  2.129e-03  4.870e+01   0.478  0.63469    

# let's throw out Accentuaion

InComplex.lmerBC3 <- lmer(bc ~ Environment+ PostPause+OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + GlobalSpeechRate +
                             PrePause + PrecSegDur+ 
                             Affix+(1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC3)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                  8.947e-01  4.134e-03  4.567e+02 216.408  < 2e-16 ***
#   Environmentn#nV             -9.956e-03  3.233e-03  4.470e+01  -3.079  0.00354 ** 
#   Environmentn#V              -2.925e-02  2.085e-03  4.480e+01 -14.031  < 2e-16 ***
#   PostPausePause              -1.990e-03  8.806e-04  1.099e+03  -2.260  0.02401 *  
#   OrderRescale                -2.494e-05  3.534e-05  1.079e+03  -0.706  0.48055    
#   logWordFormFreq              2.093e-04  3.432e-04  4.430e+01   0.610  0.54508    
#   BaseInitialStressunstressed -3.139e-03  1.860e-03  4.420e+01  -1.688  0.09855 .  
#   LocSpeech                   -1.908e-03  2.169e-04  1.112e+03  -8.793  < 2e-16 ***
#   GlobalSpeechRate            -1.812e-03  9.906e-04  1.106e+03  -1.829  0.06765 .  
#   PrePausePause                5.300e-04  7.916e-04  1.091e+03   0.670  0.50327    
#   PrecSegDur                  -4.634e-02  1.753e-02  1.094e+03  -2.643  0.00832 ** 
#   AffixLoc                     1.013e-03  2.129e-03  4.870e+01   0.476  0.63610                0.25783 

anova(InComplex.lmerBC2,InComplex.lmerBC3)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    15 -6939.2 -6863.7 3484.6  -6969.2                         
# object 16 -6937.2 -6856.7 3484.6  -6969.2 0.0108      1     0.9172

# model did not become worse


# let's throw out Affix

InComplex.lmerBC4 <- lmer(bc ~ Environment+ PostPause+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PrecSegDur+ 
                            (1|Item) + (1|Participant), data = InComplex2)
summary(InComplex.lmerBC4)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.951e-01  4.022e-03  4.950e+02 222.564  < 2e-16 ***
#   Environmentn#nV             -1.014e-02  3.185e-03  4.550e+01  -3.183  0.00263 ** 
# Environmentn#V              -2.955e-02  1.973e-03  4.760e+01 -14.980  < 2e-16 ***
# PostPausePause              -1.985e-03  8.804e-04  1.100e+03  -2.254  0.02436 *  
#   OrderRescale                -2.536e-05  3.533e-05  1.081e+03  -0.718  0.47298    
# logWordFormFreq              2.233e-04  3.393e-04  4.550e+01   0.658  0.51393    
# BaseInitialStressunstressed -3.047e-03  1.836e-03  4.560e+01  -1.659  0.10391    
# LocSpeech                   -1.930e-03  2.116e-04  1.033e+03  -9.121  < 2e-16 ***
#   GlobalSpeechRate            -1.736e-03  9.775e-04  1.107e+03  -1.776  0.07597 .  
# PrePausePause                5.488e-04  7.905e-04  1.095e+03   0.694  0.48764    
# PrecSegDur                  -4.652e-02  1.752e-02  1.096e+03  -2.655  0.00804 ** 


 # nothing has changed

anova(InComplex.lmerBC3,InComplex.lmerBC4)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    14 -6941.0 -6870.5 3484.5  -6969.0                         
# object 15 -6939.2 -6863.7 3484.6  -6969.2 0.2588      1     0.6109

# nothing has changed



# let's throw out Word From Freq

InComplex.lmerBC5 <- lmer(bc ~ Environment+ PostPause+OrderRescale +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PrecSegDur+ 
                            (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  8.959e-01  3.842e-03  7.100e+02 233.191  < 2e-16 ***
#   Environmentn#nV             -1.012e-02  3.167e-03  4.660e+01  -3.195  0.00251 ** 
# Environmentn#V              -2.983e-02  1.916e-03  4.900e+01 -15.567  < 2e-16 ***
# PostPausePause              -1.973e-03  8.801e-04  1.101e+03  -2.241  0.02519 *  
#   OrderRescale                -2.468e-05  3.531e-05  1.082e+03  -0.699  0.48468    
# BaseInitialStressunstressed -3.145e-03  1.820e-03  4.680e+01  -1.728  0.09052 .  
# LocSpeech                   -1.938e-03  2.111e-04  1.026e+03  -9.182  < 2e-16 ***
#   GlobalSpeechRate            -1.672e-03  9.725e-04  1.100e+03  -1.719  0.08582 .  
# PrePausePause                5.673e-04  7.900e-04  1.097e+03   0.718  0.47284    
# PrecSegDur                  -4.654e-02  1.752e-02  1.096e+03  -2.657  0.00801 ** 

anova(InComplex.lmerBC4,InComplex.lmerBC5)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -6942.5 -6877.1 3484.2  -6968.5                         
# object 14 -6941.0 -6870.5 3484.5  -6969.0 0.4764      1     0.4901

#nothing has changed


# let's throw out Order

InComplex.lmerBC6 <- lmer(bc ~ Environment+ PostPause +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrePause + PrecSegDur+ 
                            (1|Item) + (1|Participant), data = InComplex2)


summary(InComplex.lmerBC6)

# Fixed effects:
#                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                  8.954e-01  3.786e-03  6.910e+02 236.507  < 2e-16 ***
#   Environmentn#nV             -1.011e-02  3.165e-03  4.660e+01  -3.193  0.00252 ** 
#   Environmentn#V              -2.982e-02  1.915e-03  4.900e+01 -15.569  < 2e-16 ***
#   PostPausePause              -1.997e-03  8.792e-04  1.102e+03  -2.271  0.02333 *  
#   BaseInitialStressunstressed -3.135e-03  1.819e-03  4.680e+01  -1.724  0.09135 .  
#   LocSpeech                   -1.932e-03  2.108e-04  1.027e+03  -9.162  < 2e-16 ***
#   GlobalSpeechRate            -1.679e-03  9.722e-04  1.101e+03  -1.727  0.08451 .  
#   PrePausePause                6.106e-04  7.874e-04  1.098e+03   0.775  0.43826    
#   PrecSegDur                  -4.708e-02  1.750e-02  1.097e+03  -2.691  0.00724 **             0.25876 

anova(InComplex.lmerBC5,InComplex.lmerBC6)


# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -6944.0 -6883.6 3484.0  -6968.0                         
# object 13 -6942.5 -6877.1 3484.2  -6968.5 0.4905      1     0.4837

# nothing has changed


# let's throw out PrePause

InComplex.lmerBC7 <- lmer(bc ~ Environment+ PostPause +
                            BaseInitialStress + LocSpeech + GlobalSpeechRate +
                            PrecSegDur+ 
                            (1|Item) + (1|Participant), data = InComplex2)


summary(InComplex.lmerBC7)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                  8.961e-01  3.678e-03  6.519e+02 243.628  < 2e-16 ***
#   Environmentn#nV             -1.009e-02  3.168e-03  4.660e+01  -3.186  0.00257 ** 
#   Environmentn#V              -2.981e-02  1.917e-03  4.900e+01 -15.553  < 2e-16 ***
#   PostPausePause              -1.975e-03  8.784e-04  1.104e+03  -2.248  0.02475 *  
#   BaseInitialStressunstressed -3.130e-03  1.820e-03  4.680e+01  -1.720  0.09207 .  
#   LocSpeech                   -1.923e-03  2.105e-04  1.032e+03  -9.137  < 2e-16 ***
#   GlobalSpeechRate            -1.863e-03  9.427e-04  1.097e+03  -1.976  0.04841 *  
#   PrecSegDur                  -4.730e-02  1.749e-02  1.098e+03  -2.704  0.00696 **    

anova(InComplex.lmerBC6,InComplex.lmerBC7)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -6945.4 -6890.0 3483.7  -6967.4                         
# object 12 -6944.0 -6883.6 3484.0  -6968.0 0.6024      1     0.4377

#still no difference


# let's throw out Stress

InComplex.lmerBC8 <- lmer(bc ~ Environment+ PostPause +
                            LocSpeech + GlobalSpeechRate +
                            PrecSegDur+ 
                            (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8)

# Fixed effects:
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)       8.949e-01  3.618e-03  6.832e+02 247.350  < 2e-16 ***
#   Environmentn#nV  -9.868e-03  3.226e-03  4.730e+01  -3.058  0.00366 ** 
#   Environmentn#V   -3.118e-02  1.776e-03  4.910e+01 -17.554  < 2e-16 ***
#   PostPausePause   -1.925e-03  8.782e-04  1.104e+03  -2.192  0.02858 *  
#   LocSpeech        -1.902e-03  2.105e-04  1.037e+03  -9.035  < 2e-16 ***
#   GlobalSpeechRate -1.865e-03  9.438e-04  1.101e+03  -1.976  0.04837 *  
#   PrecSegDur       -4.763e-02  1.750e-02  1.097e+03  -2.722  0.00658 **         

 anova(InComplex.lmerBC7,InComplex.lmerBC8)
# Df     AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    10 -6944.3 -6894 3482.2  -6964.3                           
# object 11 -6945.4 -6890 3483.7  -6967.4 3.0889      1    0.07883 .

# lmerBc 7 might be better - let's wait for the interactions, otherwise throw it out (nly
#marginally sign. difference)

# so that would be the final model without interactions

################################
# Check influence of RelFreq and ST (showed to be signigifcant when
# only variables in model)

 
 
 InComplex.lmerBC8SemT <- lmer(bc ~ Environment+ SemanticTransparency +PostPause +
                             LocSpeech + GlobalSpeechRate +
                             PrecSegDur+ 
                             (1|Item) + (1|Participant), data = InComplex2)
 
 summary(InComplex.lmerBC8SemT)
 # not significant
 
 
 InComplex.lmerBC8RelF <- lmer(bc ~ Environment+ logRelFreq+ PostPause +
                             LocSpeech + GlobalSpeechRate +
                             PrecSegDur+ 
                             (1|Item) + (1|Participant), data = InComplex2)
 
 summary(InComplex.lmerBC8RelF)
 
# not significant, so InComplex.lmerBC8 would be the final model without inter-
 #actions
 
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


# Let's see



# 1. Environment and accentuation and stress and pause

# Environment and stress


InComplex.lmerBC8EnvStress <- lmer(bc ~ Environment*BaseInitialStress + PostPause +
                                     LocSpeech + GlobalSpeechRate +
                                     PrecSegDur+ 
                                     (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvStress)

# Fixed effects:
#                                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                  8.926e-01  3.590e-03  6.788e+02 248.614  < 2e-16 ***
#   Environmentn#nV                             -3.058e-03  2.812e-03  4.160e+01  -1.088  0.28304    
#   Environmentn#V                              -2.396e-02  2.076e-03  4.290e+01 -11.538 9.77e-15 ***
#   BaseInitialStressunstressed                  5.728e-03  2.201e-03  4.340e+01   2.602  0.01262 *  
#   PostPausePause                              -1.818e-03  8.758e-04  1.112e+03  -2.076  0.03809 *  
#   LocSpeech                                   -1.843e-03  2.049e-04  8.460e+02  -8.995  < 2e-16 ***
#   GlobalSpeechRate                            -1.970e-03  9.290e-04  1.047e+03  -2.120  0.03420 *  
#   PrecSegDur                                  -4.743e-02  1.745e-02  1.106e+03  -2.718  0.00667 ** 
#   Environmentn#nV:BaseInitialStressunstressed -2.637e-02  5.651e-03  4.590e+01  -4.666 2.68e-05 ***
#   Environmentn#V:BaseInitialStressunstressed  -1.325e-02  2.938e-03  4.310e+01  -4.510 4.92e-05 ***

# is this better thaqb the one whtout interaction

anova(InComplex.lmerBC8,InComplex.lmerBC8EnvStress)

# Df     AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -6944.3 -6894 3482.2  -6964.3                             
# ..1    13 -6968.4 -6903 3497.2  -6994.4 30.118      3  1.303e-06 ***


#YES

# let's have a look at it

visreg(InComplex.lmerBC8EnvStress, "Environment", by ="BaseInitialStress",
       overlay=T, trans= function(x) x^(1/lambda)*1000)


# makes sense

# Environment and Acc
InComplex.lmerBC8EnvAcc <- lmer(bc ~ Environment*AccentuationCondition + PostPause +
                                  LocSpeech + GlobalSpeechRate +
                                  PrecSegDur+ 
                                  (1|Item) + (1|Participant), data = InComplex2)
summary(InComplex.lmerBC8EnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value
# (Intercept)                                      8.955e-01  3.795e-03  6.556e+02 235.998
# Environmentn#nV                                 -8.253e-03  3.456e-03  6.230e+01  -2.388
# Environmentn#V                                  -3.298e-02  1.899e-03  6.390e+01 -17.363
# AccentuationConditionunaccented                 -1.748e-03  1.283e-03  1.070e+03  -1.363
# PostPausePause                                  -1.933e-03  8.959e-04  1.098e+03  -2.158
# LocSpeech                                       -1.874e-03  2.104e-04  1.036e+03  -8.910
# GlobalSpeechRate                                -1.880e-03  1.162e-03  8.800e+02  -1.618
# PrecSegDur                                      -4.841e-02  1.744e-02  1.094e+03  -2.776
# Environmentn#nV:AccentuationConditionunaccented -3.157e-03  2.498e-03  1.053e+03  -1.264
# Environmentn#V:AccentuationConditionunaccented   3.540e-03  1.321e-03  1.057e+03   2.681
# Pr(>|t|)    
# (Intercept)                                      < 2e-16 ***
#   Environmentn#nV                                  0.01997 *  
# Environmentn#V                                   < 2e-16 ***
# AccentuationConditionunaccented                  0.17315    
# PostPausePause                                   0.03115 *  
#   LocSpeech                                        < 2e-16 ***
#   GlobalSpeechRate                                 0.10597    
# PrecSegDur                                       0.00560 ** 
#   Environmentn#nV:AccentuationConditionunaccented  0.20652    
# Environmentn#V:AccentuationConditionunaccented   0.00746 ** 


visreg(InComplex.lmerBC8EnvAcc, "Environment", by ="AccentuationCondition",
       overlay=T, trans= function(x) x^(1/lambda)*1000)

# barely anything but let' see whether this model is better than
# the other

anova(InComplex.lmerBC8EnvAcc,InComplex.lmerBC8EnvStress)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 -6950.4 -6884.9 3488.2  -6976.4                             
# ..1    13 -6968.4 -6903.0 3497.2  -6994.4 18.054      0  < 2.2e-16 ***


# the other is definetal better, what if both interactions are in?

# Environment and Acc
InComplex.lmerBC8EnvAccEnvStress <- lmer(bc ~ Environment*AccentuationCondition + PostPause +
                                  LocSpeech + GlobalSpeechRate +
                                    Environment*BaseInitialStress+PrecSegDur+ 
                                  (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvAccEnvStress)

# both are significant, but is this model better?

anova(InComplex.lmerBC8EnvAccEnvStress,InComplex.lmerBC8EnvStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# ..1    13 -6968.4 -6903.0 3497.2  -6994.4                            
# object 16 -6975.2 -6894.7 3503.6  -7007.2 12.796      3   0.005099 **
#   ---

# yes, it is. What about a 3-way-interaction

InComplex.lmerBC8EnvAccStress <- lmer(bc ~ Environment*AccentuationCondition*BaseInitialStress + PostPause +
                                           LocSpeech + GlobalSpeechRate +
                                           +PrecSegDur+ 
                                           (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvAccStress)

# yes, let's have a look?

visreg(InComplex.lmerBC8EnvAccStress, "Environment", by ="BaseInitialStress", 
       cond=list(AccentuationCondition="accented"), 
       overlay=T, trans= function(x) x^(1/lambda)*1000)

visreg(InComplex.lmerBC8EnvAccStress, "Environment", by ="BaseInitialStress", 
       cond=list(AccentuationCondition="unaccented"), 
       overlay=T, trans= function(x) x^(1/lambda)*1000)

anova(InComplex.lmerBC8EnvAccEnvStress,InComplex.lmerBC8EnvAccStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 16 -6975.2 -6894.7 3503.6  -7007.2                           
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7 6.4519      3    0.09158 .

# basically the same, and both models show the same...3-way is a little better

# Environment and PrePause
InComplex.lmerBC8EnvPause <- lmer(bc ~ Environment*PrePause + PostPause +
                                    LocSpeech + GlobalSpeechRate +
                                    PrecSegDur+ 
                                    (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvPause)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                    8.923e-01  3.741e-03  7.350e+02 238.520  < 2e-16 ***
#   Environmentn#nV               -6.206e-03  3.572e-03  7.430e+01  -1.737 0.086487 .  
#   Environmentn#V                -2.810e-02  1.955e-03  7.590e+01 -14.373  < 2e-16 ***
#   PrePausePause                  3.708e-03  1.141e-03  1.085e+03   3.249 0.001194 ** 
#   PostPausePause                -2.121e-03  8.762e-04  1.102e+03  -2.420 0.015663 *  
#   LocSpeech                     -1.845e-03  2.103e-04  1.025e+03  -8.775  < 2e-16 ***
#   GlobalSpeechRate              -2.005e-03  9.731e-04  1.103e+03  -2.060 0.039637 *  
#   PrecSegDur                    -4.724e-02  1.741e-02  1.094e+03  -2.713 0.006775 ** 
#   Environmentn#nV:PrePausePause -5.878e-03  2.632e-03  1.073e+03  -2.233 0.025728 *  
#   Environmentn#V:PrePausePause  -5.023e-03  1.407e-03  1.068e+03  -3.570 0.000373 ***

visreg(InComplex.lmerBC8EnvPause, "Environment", by ="PrePause", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


# does not really make sense, I don't even know why I would look at this...

# Accentuation and PrePause
InComplex.lmerBC8AccPause <- lmer(bc ~ Environment+ Accentuation*PrePause + PostPause +
                                    LocSpeech + GlobalSpeechRate +
                                    PrecSegDur+ 
                                    (1|Item) + (1|Participant), data = InComplex2)
summary(InComplex.lmerBC8AccPause)

# no interaction

# Stress and PrePause
InComplex.lmerBC8StressPause <- lmer(bc ~ Environment+ PrePause*BaseInitialStress + PostPause +
                                       LocSpeech + GlobalSpeechRate +
                                       PrecSegDur+ 
                                       (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8StressPause)

# no interaction

# Stress and Acc
InComplex.lmerBC8StressAcc <- lmer(bc ~ Environment+BaseInitialStress*AccentuationCondition + PostPause +
                                     LocSpeech + GlobalSpeechRate +
                                     PrecSegDur+ 
                                     (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8StressAcc)

# Fixed effects:
#                                                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                  8.972e-01  3.864e-03  6.353e+02 232.184  < 2e-16 ***
#   Environmentn#nV                                             -9.979e-03  3.171e-03  4.660e+01  -3.147  0.00287 ** 
#   Environmentn#V                                              -2.976e-02  1.918e-03  4.900e+01 -15.515  < 2e-16 ***
#   BaseInitialStressunstressed                                 -4.966e-03  1.929e-03  5.870e+01  -2.575  0.01256 *  
#   AccentuationConditionunaccented                             -1.902e-03  1.208e-03  1.060e+03  -1.574  0.11576    
#   PostPausePause                                              -1.900e-03  8.967e-04  1.099e+03  -2.118  0.03436 *  
#   LocSpeech                                                   -1.911e-03  2.102e-04  1.036e+03  -9.091  < 2e-16 ***
#   GlobalSpeechRate                                            -1.946e-03  1.160e-03  8.745e+02  -1.677  0.09384 .  
#   PrecSegDur                                                  -5.035e-02  1.748e-02  1.096e+03  -2.880  0.00406 ** 
#   BaseInitialStressunstressed:AccentuationConditionunaccented  3.664e-03  1.258e-03  1.056e+03   2.913  0.00366 **


visreg(InComplex.lmerBC8StressAcc, "BaseInitialStress", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


# tiny effect....

# this effect is already included in the 3-way interaction - we will use that model
# (unless one of the below is better)

################################################
# 2. Decomposability measures and accentuation

InComplex.lmerBC8AccRelFreq <- lmer(bc ~ Environment+ AccentuationCondition*logRelFreq + PostPause +
                                      LocSpeech + GlobalSpeechRate +
                                      PrecSegDur+ 
                                      (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8AccRelFreq)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                 8.951e-01  3.788e-03  6.417e+02 236.294  < 2e-16 ***
#   Environmentn#nV                            -9.248e-03  3.431e-03  4.620e+01  -2.695 0.009778 ** 
#   Environmentn#V                             -3.148e-02  1.876e-03  4.760e+01 -16.783  < 2e-16 ***
#   AccentuationConditionunaccented            -7.387e-04  1.026e-03  1.005e+03  -0.720 0.471840    
#   logRelFreq                                  1.432e-04  2.634e-04  5.420e+01   0.544 0.588993    
#   PostPausePause                             -1.986e-03  8.953e-04  1.098e+03  -2.219 0.026719 *  
#   LocSpeech                                  -1.893e-03  2.100e-04  1.040e+03  -9.016  < 2e-16 ***
#   GlobalSpeechRate                           -1.826e-03  1.162e-03  8.892e+02  -1.572 0.116358    
#   PrecSegDur                                 -4.846e-02  1.744e-02  1.093e+03  -2.779 0.005550 ** 
#   AccentuationConditionunaccented:logRelFreq -5.660e-04  1.640e-04  1.058e+03  -3.452 0.000579 ***

visreg(InComplex.lmerBC8AccRelFreq, "logRelFreq", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)

#no! Distribution, effect siz

anova(InComplex.lmerBC8AccRelFreq,InComplex.lmerBC8EnvAccStress)
# Df     AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 -6950.4 -6885 3488.2  -6976.4                             
# ..1    19 -6975.7 -6880 3506.8  -7013.7 37.217      6  1.597e-06 ***


# the other model is better, letr's see if we add the interaction with env and stress




InComplex.lmerBC8EnvStressAccRelFreq <- lmer(bc ~ Environment*BaseInitialStress*AccentuationCondition+ AccentuationCondition*logRelFreq + PostPause +
                                      LocSpeech + GlobalSpeechRate +
                                      PrecSegDur+ 
                                      (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvStressAccRelFreq)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                                  8.932e-01  3.796e-03  6.495e+02 235.317  < 2e-16 ***
#   Environmentn#nV                                                             -1.349e-03  3.265e-03  6.360e+01  -0.413  0.68102    
# Environmentn#V                                                              -2.306e-02  2.453e-03  6.590e+01  -9.401 8.62e-14 ***
# BaseInitialStressunstressed                                                  7.261e-03  2.611e-03  6.770e+01   2.781  0.00702 ** 
#   AccentuationConditionunaccented                                             -9.257e-04  1.444e-03  1.088e+03  -0.641  0.52149    
# logRelFreq                                                                   1.960e-04  2.199e-04  6.140e+01   0.891  0.37631    
# PostPausePause                                                              -1.882e-03  8.926e-04  1.101e+03  -2.109  0.03518 *  
#   LocSpeech                                                                   -1.823e-03  2.046e-04  8.616e+02  -8.909  < 2e-16 ***
#   GlobalSpeechRate                                                            -2.023e-03  1.142e-03  8.586e+02  -1.772  0.07680 .  
# PrecSegDur                                                                  -5.005e-02  1.738e-02  1.096e+03  -2.879  0.00406 ** 
#   Environmentn#nV:BaseInitialStressunstressed                                 -2.993e-02  6.445e-03  7.220e+01  -4.645 1.49e-05 ***
# Environmentn#V:BaseInitialStressunstressed                                  -1.717e-02  3.423e-03  6.790e+01  -5.015 4.05e-06 ***
# Environmentn#nV:AccentuationConditionunaccented                             -3.008e-03  2.904e-03  1.050e+03  -1.036  0.30059    
# Environmentn#V:AccentuationConditionunaccented                              -1.850e-03  2.209e-03  1.054e+03  -0.837  0.40254    
# BaseInitialStressunstressed:AccentuationConditionunaccented                 -2.995e-03  2.331e-03  1.054e+03  -1.285  0.19913    
# AccentuationConditionunaccented:logRelFreq                                  -4.526e-04  1.987e-04  1.056e+03  -2.278  0.02295 *  
#   Environmentn#nV:BaseInitialStressunstressed:AccentuationConditionunaccented  6.851e-03  6.169e-03  1.051e+03   1.110  0.26706    
# Environmentn#V:BaseInitialStressunstressed:AccentuationConditionunaccented   7.616e-03  3.056e-03  1.055e+03   2.492  0.01286 *  

anova(InComplex.lmerBC8EnvStressAccRelFreq,InComplex.lmerBC8EnvAccStress)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)  
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7                          
# object 21 -6976.8 -6871.1 3509.4  -7018.8 5.152      2    0.07608 .

# not a real difference...so what - we will take the model which makes more sense!
# That is the 3- way intercation model

visreg(InComplex.lmerBC8EnvStressAccRelFreq, "logRelFreq", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)



InComplex.lmerBC8SemTAcc <- lmer(bc ~ Environment+ AccentuationCondition*SemanticTransparency + PostPause +
                                   LocSpeech + GlobalSpeechRate +
                                   PrecSegDur+ 
                                   (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8SemTAcc)


# Fixed effects:
#                                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                 8.948e-01  3.902e-03  6.053e+02 229.333  < 2e-16 ***
#   Environmentn#nV                                            -1.002e-02  3.272e-03  4.640e+01  -3.062  0.00365 ** 
#   Environmentn#V                                             -3.152e-02  1.867e-03  4.660e+01 -16.879  < 2e-16 ***
#   AccentuationConditionunaccented                             8.204e-04  1.041e-03  1.017e+03   0.788  0.43073    
#   SemanticTransparencyopaque                                  1.228e-03  2.474e-03  6.200e+01   0.497  0.62126    
#   PostPausePause                                             -1.939e-03  8.961e-04  1.098e+03  -2.164  0.03067 *  
#   LocSpeech                                                  -1.901e-03  2.126e-04  1.086e+03  -8.939  < 2e-16 ***
#   GlobalSpeechRate                                           -1.819e-03  1.166e-03  8.925e+02  -1.559  0.11927    
#   PrecSegDur                                                 -4.698e-02  1.746e-02  1.093e+03  -2.691  0.00723 ** 
#   AccentuationConditionunaccented:SemanticTransparencyopaque -5.199e-03  1.689e-03  1.060e+03  -3.079  0.00213 ** 


visreg(InComplex.lmerBC8SemTAcc, "SemanticTransparency", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)

visreg(InComplex.lmerBC8SemTAcc, "AccentuationCondition", by ="SemanticTransparency", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


anova(InComplex.lmerBC8SemTAcc,InComplex.lmerBC8EnvAccStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 -6948.1 -6882.7 3487.0  -6974.1                             
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7 39.567      6  5.541e-07 ***

# yeah, the other one is better


# what if add environment and stress

InComplex.lmerBC8EnvStressSemTAcc <- lmer(bc ~ Environment*BaseInitialStress+ AccentuationCondition*SemanticTransparency + PostPause +
                                   LocSpeech + GlobalSpeechRate +
                                   PrecSegDur+ 
                                   (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8EnvStressSemTAcc)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                 8.918e-01  3.870e-03  6.277e+02 230.447  < 2e-16 ***
#   Environmentn#nV                                            -2.866e-03  2.922e-03  4.110e+01  -0.981  0.33240    
#   Environmentn#V                                             -2.387e-02  2.130e-03  4.180e+01 -11.206 3.60e-14 ***
#   BaseInitialStressunstressed                                 5.821e-03  2.240e-03  4.270e+01   2.599  0.01278 *  
#   AccentuationConditionunaccented                             8.836e-04  1.038e-03  1.030e+03   0.851  0.39491    
#   SemanticTransparencyopaque                                  3.192e-03  2.066e-03  6.370e+01   1.545  0.12726    
#   PostPausePause                                             -1.834e-03  8.937e-04  1.106e+03  -2.051  0.04046 *  
#   LocSpeech                                                  -1.814e-03  2.086e-04  9.792e+02  -8.695  < 2e-16 ***
#   GlobalSpeechRate                                           -2.048e-03  1.151e-03  8.774e+02  -1.780  0.07547 .  
#   PrecSegDur                                                 -4.630e-02  1.742e-02  1.100e+03  -2.658  0.00798 ** 
#   Environmentn#nV:BaseInitialStressunstressed                -2.714e-02  6.047e-03  4.520e+01  -4.489 4.90e-05 ***
#   Environmentn#V:BaseInitialStressunstressed                 -1.331e-02  2.989e-03  4.200e+01  -4.453 6.15e-05 ***
#   AccentuationConditionunaccented:SemanticTransparencyopaque -5.219e-03  1.688e-03  1.061e+03  -3.091  0.00204 ** 


anova(InComplex.lmerBC8EnvStressSemTAcc,InComplex.lmerBC8EnvAccStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 16 -6972.0 -6891.4 3502.0  -7004.0                           
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7 9.7129      3    0.02117 *

# okay the other is better - that is our final model!!!!

InComplex.lmerBC8RootAcc <- lmer(bc ~ Environment+ AccentuationCondition*TypeOfBase + PostPause +
                                   LocSpeech + GlobalSpeechRate +
                                   PrecSegDur+ 
                                   (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8RootAcc)
# no

InComplex.lmerBC8RatingAcc <- lmer(bc ~ Environment+ AccentuationCondition*Rating + PostPause +
                                     LocSpeech + GlobalSpeechRate +
                                     PrecSegDur+ 
                                     (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8RatingAcc)

# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                             8.942e-01  3.961e-03  6.861e+02 225.783  < 2e-16 ***
#   Environmentn#nV                        -1.007e-02  3.214e-03  4.730e+01  -3.133  0.00297 ** 
#   Environmentn#V                         -3.145e-02  1.785e-03  5.050e+01 -17.619  < 2e-16 ***
#   AccentuationConditionunaccented         3.409e-03  1.453e-03  1.045e+03   2.346  0.01914 *  
#   Rating                                  7.260e-04  4.895e-04  1.058e+03   1.483  0.13838    
#   PostPausePause                         -1.881e-03  9.125e-04  1.061e+03  -2.061  0.03957 *  
#   LocSpeech                              -1.888e-03  2.126e-04  1.014e+03  -8.883  < 2e-16 ***
#   GlobalSpeechRate                       -2.139e-03  1.175e-03  8.409e+02  -1.820  0.06905 .  
#   PrecSegDur                             -4.975e-02  1.785e-02  1.059e+03  -2.788  0.00540 ** 
#   AccentuationConditionunaccented:Rating -1.778e-03  6.000e-04  1.025e+03  -2.963  0.00311 ** 



visreg(InComplex.lmerBC8RatingAcc, "Rating", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)

visreg(InComplex.lmerBC8RatingAcc, "AccentuationCondition", by ="Rating", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


# no


InComplex.lmerBC8AffixAcc <- lmer(bc ~ Environment+ AccentuationCondition*Affix + PostPause +
                                     LocSpeech + GlobalSpeechRate +
                                     PrecSegDur+ 
                                     (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8AffixAcc)

visreg(InComplex.lmerBC8AffixAcc, "Affix", by ="AccentuationCondition", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


visreg(InComplex.lmerBC8AffixAcc, "AccentuationCondition", by ="Affix", 
       overlay=T, trans= function(x) x^(1/lambda)*1000)


# mk
anova(InComplex.lmerBC8AffixAcc,InComplex.lmerBC8EnvAccStress)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 -6944.5 -6879.1 3485.3  -6970.5                             
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7 43.126      6  1.101e-07 ***


# the other one is way better

############################################
# 3. Decomposability measures and stress 


InComplex.lmerBC8StressRelFreq <- lmer(bc ~ Environment+ BaseInitialStress*logRelFreq + PostPause +
                                      LocSpeech + GlobalSpeechRate +
                                      PrecSegDur+ 
                                      (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8StressRelFreq)

# no
#############

InComplex.lmerBC8SemTStress <- lmer(bc ~ Environment+ BaseInitialStress*SemanticTransparency + PostPause +
                                   LocSpeech + GlobalSpeechRate +
                                   PrecSegDur+ 
                                   (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8SemTStress)


# no

InComplex.lmerBC8RootStress <- lmer(bc ~ Environment+ BaseInitialStress*TypeOfBase + PostPause +
                                   LocSpeech + GlobalSpeechRate +
                                   PrecSegDur+ 
                                   (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8RootStress)
# no

InComplex.lmerBC8RatingStress <- lmer(bc ~ Environment+ BaseInitialStress*Rating + PostPause +
                                     LocSpeech + GlobalSpeechRate +
                                     PrecSegDur+ 
                                     (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8RatingStress)

# no

InComplex.lmerBC8AffixStress <- lmer(bc ~ Environment+ BaseInitialStress*Affix + PostPause +
                                    LocSpeech + GlobalSpeechRate +
                                    PrecSegDur+ 
                                    (1|Item) + (1|Participant), data = InComplex2)

summary(InComplex.lmerBC8AffixStress)

# no


##############################################################################################
#             Summary interactions   --> SInplification of our model                        ##
##############################################################################################

# I tested teh interactions of variables of interst (RelFreq, Type of Base, Rating,Environment) and Accentuation
# also tested stress and accentuation

# Our final model has a 3-way interaction;

# stress and environment and accentuation

visreg(InComplex.lmerBC8EnvAccStress,"Environment", by= "BaseInitialStress",
       overlay=T, cond=list(AccentuationCondition="unaccented"),
       trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds",
       xlab="environment by stress", cex.axis=0.9,ylim=c(20,180))


visreg(InComplex.lmerBC8EnvAccStress,"Environment", by= "BaseInitialStress",
       overlay=T, cond=list(AccentuationCondition="accented"),
       trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds",
       xlab="environment by stress", cex.axis=0.9,ylim=c(20,180))

# when base initial syllable has prInary stress doubles are as long as singletons
# follwed by a consonant, but longer than singletons followed by a vowel

# when base-inital syllable is unstressed, they are as long as singletons followed
# by a vowel, i.e. they only seem to geminate when primary stress on base-intial syllable

# we only have 4 types with double n - how are they distributed (stress)


unique(InComplex[InComplex$Environment=="n#nV", c("Item","BaseInitialStress")])

#           Item BaseInitialStress
# 559   innervate        unstressed
# 578   innocuous           primary
# 599  innominate           primary
# 622 innumerable           primary

# so one type does not geminate - innocucous


#############################################################
# The final model:

summary(InComplex.lmerBC8EnvAccStress)

# Fixed effects:
#   Estimate Std. Error         df t value
# (Intercept)                                                                  8.933e-01  3.776e-03  6.559e+02 236.574
# Environmentn#nV                                                             -6.365e-04  3.131e-03  6.590e+01  -0.203
# Environmentn#V                                                              -2.373e-02  2.317e-03  6.850e+01 -10.244
# AccentuationConditionunaccented                                             -1.313e-03  1.436e-03  1.089e+03  -0.915
# BaseInitialStressunstressed                                                  6.540e-03  2.458e-03  6.940e+01   2.661
# PostPausePause                                                              -1.800e-03  8.933e-04  1.104e+03  -2.015
# LocSpeech                                                                   -1.820e-03  2.043e-04  8.448e+02  -8.909
# GlobalSpeechRate                                                            -2.036e-03  1.141e-03  8.500e+02  -1.784
# PrecSegDur                                                                  -5.038e-02  1.740e-02  1.100e+03  -2.895
# Environmentn#nV:AccentuationConditionunaccented                             -4.556e-03  2.832e-03  1.052e+03  -1.609
# Environmentn#V:AccentuationConditionunaccented                              -2.795e-04  2.103e-03  1.054e+03  -0.133
# Environmentn#nV:BaseInitialStressunstressed                                 -2.945e-02  6.344e-03  7.510e+01  -4.642
# Environmentn#V:BaseInitialStressunstressed                                  -1.646e-02  3.292e-03  6.990e+01  -4.999
# AccentuationConditionunaccented:BaseInitialStressunstressed                 -1.270e-03  2.209e-03  1.055e+03  -0.575
# Environmentn#nV:AccentuationConditionunaccented:BaseInitialStressunstressed  5.595e-03  6.159e-03  1.053e+03   0.908
# Environmentn#V:AccentuationConditionunaccented:BaseInitialStressunstressed   5.916e-03  2.971e-03  1.056e+03   1.991
# Pr(>|t|)    
# (Intercept)                                                                  < 2e-16 ***
#   Environmentn#nV                                                              0.83952    
# Environmentn#V                                                              1.78e-15 ***
# AccentuationConditionunaccented                                              0.36060    
# BaseInitialStressunstressed                                                  0.00968 ** 
#   PostPausePause                                                               0.04410 *  
#   LocSpeech                                                                    < 2e-16 ***
#   GlobalSpeechRate                                                             0.07477 .  
# PrecSegDur                                                                   0.00387 ** 
#   Environmentn#nV:AccentuationConditionunaccented                              0.10796    
# Environmentn#V:AccentuationConditionunaccented                               0.89432    
# Environmentn#nV:BaseInitialStressunstressed                                 1.44e-05 ***
# Environmentn#V:BaseInitialStressunstressed                                  4.10e-06 ***
# AccentuationConditionunaccented:BaseInitialStressunstressed                  0.56535    
# Environmentn#nV:AccentuationConditionunaccented:BaseInitialStressunstressed  0.36386    
# Environmentn#V:AccentuationConditionunaccented:BaseInitialStressunstressed   0.04668 *  



  lambda
#[1] 0.06060606


# I need to rename some variabels for the plot...


InComplex2<-rename(InComplex2,AccentuationAnnotator=Accentuation)

InComplex2<-rename(InComplex2,Accentuation=AccentuationCondition)

# need to rename the stress levels

levels(InComplex2$BaseInitialStress)
#[1] "prInary"    "unstressed"


levels(InComplex2$BaseInitialStress)<-c("stressed"   , "unstressed")

levels(InComplex2$BaseInitialStress)
#[1] "stressed"   "unstressed"


# also need to change ref levels for environment

InComplex2$Environment <- relevel (InComplex2$Environment, ref= "n#nV")


final_In_complex_model.lmer<-lmer(bc ~  Environment*BaseInitialStress*Accentuation+LocSpeech+ GlobalSpeechRate+                                              
                                    +PrecSegDur+ PostPause+ (1|Participant)+ (1|Item) , data = InComplex2)                                 


summary(final_In_complex_model.lmer)

# Fixed effects:
#                                                                       Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                                        8.927e-01  4.522e-03  3.131e+02 197.406  < 2e-16 ***
#   Environmentn#C                                                     6.365e-04  3.131e-03  6.590e+01   0.203 0.839516    
#   Environmentn#V                                                    -2.310e-02  3.405e-03  6.840e+01  -6.782 3.42e-09 ***
#   BaseInitialStressunstressed                                       -2.291e-02  5.856e-03  7.640e+01  -3.912 0.000197 ***
#   Accentuationunaccented                                            -5.869e-03  2.694e-03  1.076e+03  -2.178 0.029597 *  
#   LocSpeech                                                         -1.820e-03  2.043e-04  8.448e+02  -8.909  < 2e-16 ***
#   GlobalSpeechRate                                                  -2.036e-03  1.141e-03  8.500e+02  -1.784 0.074771 .  
#   PrecSegDur                                                        -5.038e-02  1.740e-02  1.100e+03  -2.895 0.003871 ** 
#   PostPausePause                                                    -1.800e-03  8.933e-04  1.104e+03  -2.015 0.044101 *  
#   Environmentn#C:BaseInitialStressunstressed                         2.945e-02  6.344e-03  7.510e+01   4.642 1.44e-05 ***
#   Environmentn#V:BaseInitialStressunstressed                         1.299e-02  6.251e-03  7.560e+01   2.078 0.041101 *  
#   Environmentn#C:Accentuationunaccented                              4.556e-03  2.832e-03  1.052e+03   1.609 0.107956    
#   Environmentn#V:Accentuationunaccented                              4.276e-03  3.083e-03  1.051e+03   1.387 0.165709    
#   BaseInitialStressunstressed:Accentuationunaccented                 4.324e-03  5.734e-03  1.051e+03   0.754 0.450911    
#   Environmentn#C:BaseInitialStressunstressed:Accentuationunaccented -5.595e-03  6.159e-03  1.053e+03  -0.908 0.363863    
#   Environmentn#V:BaseInitialStressunstressed:Accentuationunaccented  3.209e-04  6.071e-03  1.051e+03   0.053 0.957861   


# AHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH


# If one changes the reference level, the 3-way intercation (which showed a very weak effect anyways
# vanishes!) The question is, what do I want to do?


# Let' see whether the second best model (which is actually not significantly worse), still has its intercations


final_In_complex_model.lmer2<-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+ GlobalSpeechRate+                                              
               +PrecSegDur+ PostPause+ (1|Participant)+ (1|Item) , data = InComplex2) )

summary(final_In_complex_model.lmer2)

# Fixed effects:
#                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                 8.920e-01  4.485e-03  3.038e+02 198.866  < 2e-16 ***
#   Environmentn#C                              1.280e-03  3.061e-03  6.010e+01   0.418 0.677179    
#   Environmentn#V                             -2.446e-02  3.266e-03  5.780e+01  -7.488 4.53e-10 ***
#   BaseInitialStressunstressed                -2.081e-02  5.181e-03  4.690e+01  -4.017 0.000212 ***
#   Accentuationunaccented                     -5.025e-03  2.428e-03  1.081e+03  -2.070 0.038719 *  
#   LocSpeech                                  -1.816e-03  2.046e-04  8.447e+02  -8.878  < 2e-16 ***
#   GlobalSpeechRate                           -2.015e-03  1.143e-03  8.527e+02  -1.763 0.078335 .  
#   PrecSegDur                                 -4.828e-02  1.739e-02  1.103e+03  -2.775 0.005606 ** 
#   PostPausePause                             -1.812e-03  8.933e-04  1.106e+03  -2.029 0.042718 *  
#   Environmentn#C:BaseInitialStressunstressed  2.669e-02  5.612e-03  4.600e+01   4.756 1.98e-05 ***
#   Environmentn#V:BaseInitialStressunstressed  1.328e-02  5.532e-03  4.640e+01   2.400 0.020436 *  
#   Environmentn#C:Accentuationunaccented       3.302e-03  2.498e-03  1.054e+03   1.322 0.186532    
#   Environmentn#V:Accentuationunaccented       6.916e-03  2.436e-03  1.053e+03   2.839 0.004611 ** 

anova(final_In_complex_model.lmer2,final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 16 -6975.2 -6894.7 3503.6  -7007.2                           
# ..1    19 -6975.7 -6880.0 3506.8  -7013.7 6.4519      3    0.09158 .


visreg(final_In_complex_model.lmer2, trans= function(x) (x^(1/lambda))*1000,ylim=c(20,180))

# There is a strange connection between Gloabal Speech Rate and Post Pause

# well, if there is a pause - speech rate is lower...

# let's see


plot(InComplex$PostPause,InComplex$GlobalSpeechRate)

# let's throw out Post Pause

final_In_complex_model.lmer3<-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+ GlobalSpeechRate+                                              
                                      +PrecSegDur+  (1|Participant)+ (1|Item) , data = InComplex2) )

summary(final_In_complex_model.lmer3)

# Globalspeech Rate is not sign anymore, let's throw it ot tooo



final_In_complex_model.lmer4<-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                      +PrecSegDur+  (1|Participant)+ (1|Item) , data = InComplex2) )

summary(final_In_complex_model.lmer4)

#   (Intercept)                                 8.870e-01  3.963e-03  2.013e+02 223.837  < 2e-16 ***
#   Environmentn#C                              1.087e-03  3.043e-03  6.000e+01   0.357 0.722054    
#   Environmentn#V                             -2.466e-02  3.246e-03  5.770e+01  -7.595 3.03e-10 ***
#   BaseInitialStressunstressed                -2.129e-02  5.145e-03  4.660e+01  -4.139 0.000145 ***
#   Accentuationunaccented                     -5.630e-03  2.310e-03  1.060e+03  -2.437 0.014963 *  
#   LocSpeech                                  -1.843e-03  1.899e-04  9.757e+02  -9.702  < 2e-16 ***
#   PrecSegDur                                 -4.557e-02  1.738e-02  1.104e+03  -2.621 0.008880 ** 
#   Environmentn#C:BaseInitialStressunstressed  2.723e-02  5.572e-03  4.570e+01   4.887 1.30e-05 ***
#   Environmentn#V:BaseInitialStressunstressed  1.403e-02  5.488e-03  4.600e+01   2.557 0.013908 *  
#   Environmentn#C:Accentuationunaccented       3.567e-03  2.499e-03  1.054e+03   1.428 0.153691    
#   Environmentn#V:Accentuationunaccented       7.075e-03  2.438e-03  1.054e+03   2.902 0.003788 ** 


anova(final_In_complex_model.lmer2,final_In_complex_model.lmer4)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5                           
# object 16 -6975.2 -6894.7 3503.6  -7007.2 5.6809      2     0.0584 .


# okay, THAT is our final model!!!!!!!!

Final_In_complex_model.lmer<-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                      PrecSegDur+  (1|Participant)+ (1|Item) , data = InComplex2) )



visreg(Final_In_complex_model.lmer, trans= function(x) (x^(1/lambda))*1000, rug=F, ylab="duration in milliseconds", cex.axis=0.9,ylim=c(20,180))



#############
# Let's get the two models for the dissertation


table_final_models<-as.data.frame(coef(summary(Final_In_complex_model.lmer)))

xtable(table_final_models,digits = 3)


#############################################################
# Let's now look at each factors contribution to the model
###############################################################

############################################################
# Do we need random effects?
#############################################

# Speaker

InComplex.finalWithoutSpeaker <-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                          +PrecSegDur+   (1|Item) , data = InComplex2) )


summary(InComplex.finalWithoutSpeaker)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                 8.933e-01  4.026e-03  1.733e+02 221.896  < 2e-16 ***
#   Environmentn#C                              1.543e-03  3.291e-03  6.070e+01   0.469 0.640971    
# Environmentn#V                             -2.326e-02  3.507e-03  5.810e+01  -6.632 1.21e-08 ***
# BaseInitialStressunstressed                -2.194e-02  5.542e-03  4.640e+01  -3.960 0.000256 ***
#   Accentuationunaccented                     -4.472e-03  2.573e-03  1.081e+03  -1.738 0.082459 .  
# LocSpeech                                  -2.399e-03  1.846e-04  1.039e+03 -12.995  < 2e-16 ***
#   PrecSegDur                                 -5.921e-02  1.775e-02  1.105e+03  -3.336 0.000879 ***
#   Environmentn#C:BaseInitialStressunstressed  2.711e-02  6.000e-03  4.540e+01   4.518 4.43e-05 ***
# Environmentn#V:BaseInitialStressunstressed  1.455e-02  5.909e-03  4.570e+01   2.462 0.017647 *  
# Environmentn#C:Accentuationunaccented       3.724e-03  2.793e-03  1.079e+03   1.333 0.182735    
# Environmentn#V:Accentuationunaccented       6.968e-03  2.724e-03  1.079e+03   2.558 0.010662 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

cor(InComplex2$bc, fitted(InComplex.finalWithoutSpeaker))^2
#[1] 0.7091633



InComplex.finalWithoutItem <-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                     +PrecSegDur+  (1|Participant) , data = InComplex2) )


summary(InComplex.finalWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                 8.852e-01  3.283e-03  8.286e+02 269.655  < 2e-16 ***
#   Environmentn#C                              1.980e-04  1.992e-03  1.097e+03   0.099  0.92082    
# Environmentn#V                             -2.545e-02  2.087e-03  1.097e+03 -12.193  < 2e-16 ***
# BaseInitialStressunstressed                -2.146e-02  2.996e-03  1.096e+03  -7.164 1.44e-12 ***
#   Accentuationunaccented                     -6.523e-03  2.418e-03  1.097e+03  -2.698  0.00709 ** 
#   LocSpeech                                  -1.656e-03  1.739e-04  1.122e+03  -9.524  < 2e-16 ***
#   PrecSegDur                                 -3.826e-02  1.789e-02  1.122e+03  -2.139  0.03265 *  
#   Environmentn#C:BaseInitialStressunstressed  2.827e-02  3.202e-03  1.096e+03   8.830  < 2e-16 ***
# Environmentn#V:BaseInitialStressunstressed  1.387e-02  3.168e-03  1.096e+03   4.377 1.32e-05 ***
# Environmentn#C:Accentuationunaccented       4.293e-03  2.621e-03  1.096e+03   1.638  0.10170    
# Environmentn#V:Accentuationunaccented       7.811e-03  2.558e-03  1.096e+03   3.054  0.00232 ** 

cor(InComplex2$bc, fitted(InComplex.finalWithoutItem))^2
#[1] 0.7424636


# Now, let's see how much each factor explains - we will take a look at the ACI for that

# Let's create models in which one of the preditor variables is missing

InComplex.finalWithoutInteraction1 <-(lmer(bc ~  Environment+BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                             +PrecSegDur+  (1|Item)+(1|Participant) , data = InComplex2) )


InComplex.finalWithoutInteraction2 <-(lmer(bc ~  Environment*BaseInitialStress+Environment+Accentuation+LocSpeech+                                               
                                           (1|Item)+PrecSegDur+  (1|Participant) , data = InComplex2) )



InComplex.finalWithoutEnvironment<-(lmer(bc ~  BaseInitialStress+Accentuation+LocSpeech+                                               
                                           +PrecSegDur+   (1|Item)+  (1|Participant) , data = InComplex2) )



InComplex.finalWithoutStress <-(lmer(bc ~  Environment*Accentuation+LocSpeech+                                               
                                       +PrecSegDur+   (1|Item)+  (1|Participant) , data = InComplex2) )



InComplex.finalWithoutAccentuation <-(lmer(bc ~  Environment*BaseInitialStress+LocSpeech+                                               
                                             +PrecSegDur+   (1|Item)+  (1|Participant) , data = InComplex2) )


InComplex.finalWithoutLocSpeech <-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation                                             
                                          +PrecSegDur+   (1|Item)+  (1|Participant) , data = InComplex2) )



InComplex.finalWithoutPrecSeg <-(lmer(bc ~  Environment*BaseInitialStress+Environment*Accentuation+LocSpeech+                                               
                                        +   (1|Item)+(1|Participant) , data = InComplex2) )



  ###########################################################################
# Now, let's have a look at the contribution of each factor
###################################################################


anova(InComplex.finalWithoutSpeaker,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 -6790.6 -6725.1 3408.3  -6816.6                             
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 184.98      1  < 2.2e-16 ***

6973.5-6790.6
#[1] 182.9

anova(InComplex.finalWithoutItem,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 13 -6929.0 -6863.5 3477.5  -6955.0                             
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 46.565      1  8.862e-12 ***

6973.5-6929.0
#44.5

anova(InComplex.finalWithoutInteraction1,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -6949.3 -6888.9 3486.7  -6973.3                             
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 28.232      2  7.406e-07 ***
6973.5-6949.3
#24.2

anova(InComplex.finalWithoutInteraction2,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 12 -6964.8 -6904.4 3494.4  -6988.8                           
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 12.71      2   0.001738 **

6973.5-6964.8
#8.7

anova(InComplex.finalWithoutEnvironment,Final_In_complex_model.lmer)
# object  8 -6853.5 -6813.2 3434.7  -6869.5                             
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 132.08      6  < 2.2e-16 ***

6973.5-6853.5
# 120

anova(InComplex.finalWithoutAccentuation,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# object 11 -6966.5 -6911.2 3494.3  -6988.5                            
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 12.985      3    0.00467 **
6973.5-6966.5
#7

anova(InComplex.finalWithoutStress,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# object 11 -6948.5 -6893.2 3485.3  -6970.5                            
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5    31      3    8.5e-07 ***

6973.5-6948.5
#[1] 25


anova(InComplex.finalWithoutLocSpeech,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object 13 -6885.1 -6819.7 3455.6  -6911.1                            
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 90.43      1  < 2.2e-16 ***
6973.5-6885.1 
#88.4

anova(InComplex.finalWithoutPrecSeg,Final_In_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# object 13 -6968.8 -6903.3 3497.4  -6994.8                            
# ..1    14 -6973.5 -6903.1 3500.8  -7001.5 6.7724      1   0.009258 **
# #   ---
6973.5-6968.8 
#4.7

########################################################
# When we look at the contribution of each factor in the model without
# the interaction, we see the following picture
#######################################################################


# Let's put these numbers in a table

AIC_decrease_InComplex<-matrix(c(183,120,88, 45, 25, 24, 9, 7, 5),ncol=9,byrow=TRUE)
colnames(AIC_decrease_InComplex)<-c("Speaker", "Environment", "Local-\nSpeechRate", "Item", "Base-\nInitialStress", 
                                    "Env. * Stress", 
                                    "Env. * Acc.","Accentuation","Preceding-\nSegmentDuration"  )
rownames(AIC_decrease_InComplex)<-c("Decrease in AIC")
AIC_decrease_InComplex <- as.table(AIC_decrease_InComplex)
AIC_decrease_InComplex


#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



# plot effect sizes


png("AICdecreaseInComplex.png", units="cm", height=10, width=17, res=300, pointsize=09)


par(mar=c(2.6,8.1, 1.1, 2), xpd=TRUE, cex=0.9)

barplot((AIC_decrease_InComplex),horiz=T, col="lightgrey",  names.arg =colnames(AIC_decrease_InComplex), las=2, xaxt="n")

xx<-barplot(AIC_decrease_InComplex, horiz=T, col="lightgrey",names.arg =colnames(AIC_decrease_InComplex), las=2, xaxt="n", border="lightgrey")

text(y = xx, x = AIC_decrease_InComplex ,label = AIC_decrease_InComplex, pos = 4, cex = 0.8, col = "black")

title(xlab="AIC increase", line=0, cex.lab=1.1)

dev.off()

# what is lambda


lambda
#[1] 0.06060606




##############################
# We should also plot the main effect (not covariates)
###############################
# Plot main effect

png("InModelInterEnvStress.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(Final_In_complex_model.lmer, "Environment",by="BaseInitialStress",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()



png("InModelInterEnvAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(Final_In_complex_model.lmer, "Environment",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()


library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the Inportance of the
# different factors.

options(na.action = "na.fail") 


InComplex.lm1<- lm(ConsonantDur ~ Environment + Accentuation + 
     OrderRescale + logWordFormFreq + BaseInitialStress + LocSpeech + 
     GlobalSpeechRate + PrePause + PostPause + PrecSegDur + PCDec1+PCDec2+PCDec3+
       PCDec4, 
   data = InComplexRating)

model_ranking <- dredge(InComplex.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Relative variable importance: 
#   Environment LocSpeech PostPause PCDec4 GlobalSpeechRate PCDec2 PCDec1 BaseInitialStress
# Importance:          1.00        1.00      1.00      0.99   0.96             0.91   0.89   0.84             
# N containing models: 8192        8192      8192      8192   8192             8192   8192   8192             
# logWordFormFreq PrecSegDur PrePause Accentuation PCDec3 OrderRescale
# Importance:          0.72            0.62       0.44     0.38         0.37   0.27        
# N containing models: 8192            8192       8192     8192         8192   8192  


# let's check MuMin with interactions

options(na.action = "na.fail") 


InComplex.lm2<- lm(ConsonantDur ~ Environment*Accentuation*BaseInitialStress + 
                     OrderRescale + logWordFormFreq +  + LocSpeech + 
                     GlobalSpeechRate + Accentuation*BaseInitialStress*PrePause + 
                     PostPause + PrecSegDur + Environment*PCDec1+Environment*PCDec2+Environment*PCDec3+
                     Environment*PCDec4, 
                   data = InComplexRating)

model_ranking2 <- dredge(InComplex.lm2)

model_average_2<-model.avg(model_ranking2)
summary(model_average_2)


# Relative variable importance: 
#   Environment LocSpeech BaseInitialStress BaseInitialStress:Environment Accentuation
# Importance:            1.00        1.00      1.00              1.00                          1.00      
# N containing models: 331776      175616    304640            160704                        304640      
# Accentuation:Environment Accentuation:BaseInitialStress PostPause
# Importance:            1.00                     1.00                           1.00   
# N containing models: 160704                   161664                         175616   
# Accentuation:BaseInitialStress:Environment GlobalSpeechRate PrePause
# Importance:            1.00                                       0.98             0.97  
# N containing models:  31104                                     175616           273536  
# BaseInitialStress:PrePause Accentuation:PrePause
# Importance:            0.93                       0.92               
# N containing models: 135744                     135744               
# Accentuation:BaseInitialStress:PrePause PCDec3 PCDec2 Environment:PCDec3 PCDec4
# Importance:            0.91                                    0.90   0.87   0.86               0.82
# N containing models:  26944                                  230912 230912 110592             230912
# Environment:PCDec4 PrecSegDur PCDec1 logWordFormFreq Environment:PCDec1
# Importance:            0.75               0.72       0.67   0.56            0.55            
# N containing models: 110592             175616     230912 175616          110592            
# Environment:PCDec2 OrderRescale
# Importance:            0.53               0.26      
# N containing models: 110592             175616    


###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

summary(Final_In_complex_model.lmer)


# Fixed effects:
#                                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                 8.870e-01  3.963e-03  2.013e+02 223.837  < 2e-16 ***
#   Environmentn#C                              1.087e-03  3.043e-03  6.000e+01   0.357 0.722054    
#   Environmentn#V                             -2.466e-02  3.246e-03  5.770e+01  -7.595 3.03e-10 ***
#   BaseInitialStressunstressed                -2.129e-02  5.145e-03  4.660e+01  -4.139 0.000145 ***
#   Accentuationunaccented                     -5.630e-03  2.310e-03  1.060e+03  -2.437 0.014963 *  
#   LocSpeech                                  -1.843e-03  1.899e-04  9.757e+02  -9.702  < 2e-16 ***
#   PrecSegDur                                 -4.557e-02  1.738e-02  1.104e+03  -2.621 0.008880 ** 
#   Environmentn#C:BaseInitialStressunstressed  2.723e-02  5.572e-03  4.570e+01   4.887 1.30e-05 ***
#   Environmentn#V:BaseInitialStressunstressed  1.403e-02  5.488e-03  4.600e+01   2.557 0.013908 *  
#   Environmentn#C:Accentuationunaccented       3.567e-03  2.499e-03  1.054e+03   1.428 0.153691    
#   Environmentn#V:Accentuationunaccented       7.075e-03  2.438e-03  1.054e+03   2.902 0.003788 ** 


visreg(final_In_complex_model.lmer)


# Conditions used in construction of plot
# Environment: n#V
# Accentuation: accented
# LocSpeech: 12.21309
# GlobalSpeechRate: 2.240557
# PrecSegDur: 0.06327784
# PostPause: No Pause
# Participant: Experiment_1_participant_4
# Item: intake


intercept =  8.870e-01

LocCondition= 12.21309
estSpeech= -1.843e-03

PrecSegCondition= 0.06327784
estPrecSeg= -4.557e-02

EstEnvironmentInC= 1.087e-03

EstEnvironmentInV= -2.466e-02

EstUnstressed=-2.129e-02

estUnaccented= -4.557e-02


InteractionEst1nCUnstressed= 2.723e-02
InteractionEst1nVUnstressed= 1.403e-02


InteractionEst2nCUnaccented= 3.567e-03
InteractionEst2nVUnaccented= 7.075e-03



visreg(Final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylim=ylim,cex.axis=0.9,par.settings=par)

#level Inn stressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg))^(1/lambda))*1000
#[1] 85.62645

InnStressed= 85.62645

#level InC stressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg)+EstEnvironmentInC)^(1/lambda))*1000
#[1] 87.42642

InCStressed= 87.42642

#level InC Unstressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg)+EstUnstressed+EstEnvironmentInC+InteractionEst1nCUnstressed)^(1/lambda))*1000
#[1] 97.90693

InCUnstressed= 97.90693


#level InV stressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg)+EstEnvironmentInV)^(1/lambda))*1000
#[1] 87.42642

InVStressed= 53.03008

#level InV Unstressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg)+EstUnstressed+EstEnvironmentInV+InteractionEst1nVUnstressed)^(1/lambda))*1000
#[1] 45.92951

InVUnstressed= 45.92951



#level Inn Unstressed
((intercept+(LocCondition*estSpeech)+(PrecSegCondition*estPrecSeg)+EstUnstressed)^(1/lambda))*1000
#[1] 56.6654

InnUnstressed= 56.6654

# Unterschiede:

# Double single stressed

InnStressed-InCStressed
#-1.79997

InnStressed-InVStressed
#32.59637

InnUnstressed-InCUnstressed
#-41.24153

InnUnstressed-InVUnstressed
#[1] 10.73589


# Unterschiede zwischen singletons

InVUnstressed-InCUnstressed
#[1] -51.97742

InVStressed-InCStressed
#[1] -34.39634

