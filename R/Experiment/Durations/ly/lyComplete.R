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


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses ly/")



lyComplete <- read.csv("lyComplete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(lyComplete)
# 'data.frame':	1645 obs. of  87 variables:
#   $ X.1                        : int  8 11 18 19 20 21 22 23 24 25 ...
# $ X                          : int  4650 4653 4660 4661 4662 4663 4664 4665 4666 4667 ...
# $ Item                       : Factor w/ 103 levels "aerobical","aerobically",..: 1 1 2 2 2 2 2 2 2 2 ...
# $ Participant                : Factor w/ 22 levels "participant_10A_Experiment_2",..: 2 4 1 11 17 12 16 19 3 9 ...
# $ ID                         : int  248 1220 7 2661 3774 2808 3630 4560 839 2118 ...
# $ Filename                   : Factor w/ 1645 levels "participant_1_A_101.TextGrid",..: 238 427 146 809 1204 848 1149 1425 308 609 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 2 2 2 2 1 1 1 1 2 1 ...
# $ Annotator                  : Factor w/ 4 levels "Mandy","Simon",..: 3 3 1 1 1 3 2 3 4 4 ...
# $ Order                      : int  150 49 108 319 115 236 212 41 100 142 ...
# $ WordDur                    : num  0.641 0.603 0.747 0.617 0.736 ...
# $ SyllNum                    : int  4 4 4 4 4 4 4 4 4 4 ...
# $ SegNum                     : int  8 8 8 8 8 8 8 8 8 9 ...
# $ ConsonantDur               : num  0.0632 0.1234 0.0604 0.0361 0.0448 ...
# $ PrecSeg                    : Factor w/ 54 levels "@","@O","@u",..: 1 1 26 26 26 26 26 26 26 26 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 1 1 1 1 1 1 1 1 ...
# $ PrecSegDur                 : num  0.0506 0.0456 0.1194 0.0934 0.1317 ...
# $ FollSeg                    : Factor w/ 6 levels "@","aI","e","eI",..: NA NA 5 5 5 5 5 5 5 5 ...
# $ FollSegVC                  : Factor w/ 1 level "V": 1 1 1 1 1 1 1 1 1 1 ...
# $ FollSegDur                 : num  0 0 0.1107 0.1215 0.0891 ...
# $ PrePauseDur                : num  0.0615 0.1818 0.046 0 0 ...
# $ PostPauseDur               : num  0 0 0 0 0 ...
# $ SentenceDur                : num  10.18 5.97 6.61 4.06 7.92 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ GlottalStopDur             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ LocSpeech                  : num  12.5 13.3 10.7 13 10.9 ...
# $ AffixDur                   : num  NA NA 0.171 0.158 0.134 ...
# $ BaseDuration               : num  0.641 0.603 0.576 0.459 0.602 ...
# $ FirstSyllDur               : num  0.1222 0.0266 0.1098 0.0688 0.1055 ...
# $ WordDurWithoutGlottalStop  : num  0.641 0.603 0.747 0.617 0.736 ...
# $ AffixDurWithoutGlottalStop : num  NA NA 0.171 0.158 0.134 ...
# $ Affix                      : Factor w/ 1 level "ly": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  1 1 32 32 32 32 32 32 32 32 ...
# $ WordFormFrequencyAllCOCA   : int  0 0 86 86 86 86 86 86 86 86 ...
# $ WordFormFrequencySpokenCOCA: int  NA NA NA NA NA NA NA NA NA NA ...
# $ Base                       : Factor w/ 61 levels "aerobical","agricultural",..: NA NA 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  1 1 32 32 32 32 32 32 32 32 ...
# $ BaseLemmaFrequencyBNC      : int  NA NA 1 1 1 1 1 1 1 1 ...
# $ SyllPhon                   : int  0 0 NA NA NA NA NA NA NA NA ...
# $ AffixStress                : Factor w/ 1 level "unstressed": NA NA 1 1 1 1 1 1 1 1 ...
# $ BaseInitialStress          : logi  NA NA NA NA NA NA ...
# $ SemanticTransparency       : Factor w/ 2 levels "simplex","transparent": NA NA 2 2 2 2 2 2 2 2 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": NA NA 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  NA NA 2 3 3 3 1 3 1 2 ...
# $ TimeRating                 : num  NA NA 691 764 681 ...
# $ TotalTime                  : num  NA NA 786 1073 793 ...
# $ Age                        : int  NA NA 25 65 33 19 20 21 31 20 ...
# $ Sex                        : Factor w/ 2 levels "female","male": NA NA 1 2 2 1 2 2 2 1 ...
# $ L1                         : Factor w/ 2 levels "British English",..: NA NA 1 1 1 1 1 1 1 1 ...
# $ Bilingual                  : Factor w/ 1 level "no": NA NA 1 1 1 1 1 1 1 1 ...
# $ Grow_Up_Region             : Factor w/ 21 levels "Aberdeen. Scotland",..: NA NA 19 1 8 21 2 11 5 7 ...
# $ Languages                  : Factor w/ 14 levels "Basic French",..: NA NA 12 5 12 12 8 3 9 10 ...
# $ Latin                      : Factor w/ 6 levels "I know a few words and phrases. I have never studied it",..: NA NA 2 6 2 2 2 5 4 2 ...
# $ Profession_Studies         : Factor w/ 22 levels "2nd Year Meida Studies",..: NA NA 7 4 12 1 3 18 14 11 ...
# $ University                 : Factor w/ 9 levels "Aberdeen University",..: NA NA 4 1 2 5 4 9 8 2 ...
# $ Knowledge_English_Ling     : Factor w/ 10 levels "2 years","no",..: NA NA 5 2 2 2 2 2 10 1 ...
# $ Phonetics                  : Factor w/ 6 levels "I went to a few lectures on phonetics in my first year of university.",..: NA NA 4 2 2 2 2 1 2 4 ...
# $ Phonology                  : Factor w/ 5 levels "no","The above lecture also covered phonology.",..: NA NA 4 1 1 1 1 2 1 4 ...
# $ Morphology                 : Factor w/ 4 levels "no","Year 1 ARU",..: NA NA 3 1 1 1 1 1 1 3 ...
# $ Semantics                  : Factor w/ 4 levels "no","Year 1 ARU",..: NA NA 3 1 1 1 1 1 1 3 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 1 2 2 2 2 1 1 1 1 2 ...
# $ Experiment                 : Factor w/ 1 level "Experiment_2": 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  0 0 3.47 3.47 3.47 ...
# $ logBaseLemmaFreq           : num  NA NA 0 0 0 0 0 0 0 0 ...
# $ logWordLemmaFreq           : num  0 0 3.47 3.47 3.47 ...
# $ RelFreq                    : num  NA NA 32 32 32 32 32 32 32 32 ...
# $ logRelFreq                 : num  NA NA 3.47 3.47 3.47 ...
# $ Root                       : Factor w/ 61 levels "aerobical","agricultural",..: NA NA 1 1 1 1 1 1 1 1 ...
# $ BaseFinalStress            : Factor w/ 2 levels "primary","unstressed": 2 2 2 2 2 2 2 2 2 2 ...
# $ SuffixAdjSuffix            : Factor w/ 3 levels "al","ful","none": 1 1 1 1 1 1 1 1 1 1 ...
# $ LastSyllDur                : num  0.22 0.269 0.171 0.251 0.134 ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": 1 1 2 2 2 2 2 2 2 2 ...
# $ Consonant                  : Factor w/ 2 levels "l","L": 1 1 2 1 1 1 1 1 1 1 ...
# $ Orthography                : Factor w/ 4 levels "l","le","lely",..: 1 1 4 4 4 4 4 4 4 4 ...
# $ median                     : int  NA NA 2 2 2 2 2 2 2 2 ...
# $ TypeOfBase                 : Factor w/ 1 level "word": NA NA 1 1 1 1 1 1 1 1 ...
# $ Environment2               : Factor w/ 4 levels "V#l","Vl","Vl#",..: 3 3 4 4 4 4 4 4 4 4 ...
# $ syllabicity                : Factor w/ 2 levels "no","yes": 1 1 2 2 2 2 2 2 2 2 ...
# $ Environment                : Factor w/ 5 levels "#l","l","l#",..: 3 3 5 5 5 5 5 5 5 5 ...
# $ Environment4               : Factor w/ 10 levels "syllabic Vl#l-al",..: 5 5 1 1 1 1 1 1 1 1 ...
# $ TypeOfL                    : Factor w/ 2 levels "approximant",..: 1 1 2 1 1 1 1 1 1 1 ...
# $ ConsonantDurMS             : num  63.2 123.4 60.4 36.1 44.8 ...
# $ PrePause                   : Factor w/ 2 levels "No Pause","Pause": 2 2 2 1 1 1 2 2 2 1 ...
# $ PostPause                  : Factor w/ 2 levels "No Pause","Pause": 1 1 1 1 1 2 2 2 2 2 ...
# $ GlobalSpeechRate           : num  0.589 1.842 1.664 2.707 1.389 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 2 2 3 3 3 3 3 3 3 3 ...

lyComplete$X.1<-NULL
  
lyComplete$X<-NULL

###############################################################
#   Summary: variables to include                            ##
###############################################################

## We are going to include the following predictors:
## We are going to include the following predictors:

# - Item (rand. effect)
# - Participant (rand. effect)

# - Order
# - Environment
# - logWordFormFreq
# - BaseFinalStress
# - Accentuation
# - Type of L
# - Orthography

# Note that there is a complex relation between orthography. Type of L and
# Environment 

# Also, I have not coded the base words for syllabicity yet,

table(lyComplete$Environment,lyComplete$syllabicity)
            # no yes
# #l        609   0
# l         201   0
# l#        218  21
# l#l       464   0
# syll. l#l   0 132

# I need to enrich the Environment variable (for syll. base words)

lyComplete$Environment<-factor(paste(lyComplete$Environment,lyComplete$syllabicity, sep="-"))


levels(lyComplete$Environment)
[1] "#l-no"         "l-no"          "l#-no"         "l#-yes"        "l#l-no"        "syll. l#l-yes"

levels(lyComplete$Environment)<-c("#l,", "l","l#", "syll.l#","l#l","syll. l#l")

levels(lyComplete$Environment)
#[1] "#l,"       "l"         "l#"        "syll.l#"   "l#l"       "syll. l#l"

table(lyComplete$Environment,lyComplete$Orthography)
# l  le lely  ll
# #l,       609   0    0   0
# l           0   0    0 201
# l#        115 103    0   0
# syll.l#    21   0    0   0
# l#l         0   0  151 313
# syll. l#l   0   0    0 132


table(lyComplete$Environment,lyComplete$TypeOfL)
# approximant tap
# #l,               563  46
# l                 196   5
# l#                215   3
# syll.l#            21   0
# l#l               444  20
# syll. l#l         125   7

# this is actually okay


table(lyComplete$Orthography,lyComplete$TypeOfL)
#             approximant tap
# l            698  47
# le           101   2
# lely         146   5
# ll           619  27

# This is also okay

# We shoudl probably do teh same thing as in the model with only complex words, i.e. combine
# the two factors orthgraohy and environment





lyComplete$Environment2<-as.factor(paste(lyComplete$Environment,lyComplete$Orthography, sep="-"))

levels(lyComplete$Environment2)
# [1] "#l,-l"        "l-ll"         "l#-l"         "l#-le"        "l#l-lely"     "l#l-ll"      
# [7] "syll. l#l-ll" "syll.l#-l"   


levels(lyComplete$Environment2)<-c("#l" ,"l","l#-l", "l#-le" ,"l#l-lely",  "l#l-ll"       ,"syll. l#l","syll.l#")

levels(lyComplete$Environment2)
# [1] "#l"        "l"         "l#-l"      "l#-le"     "l#l-lely"  "l#l-ll"    "syll. l#l" "syll.l#" 

table(lyComplete$Environment2)
# #l         l      l#-l     l#-le  l#l-lely    l#l-ll syll. l#l   syll.l# 
# 609       201       115       103       151       313       132        21 

# - Loc Speech  and/or Global Speech
# - PrePauseDur or PrePause
# - PostPauseDur or PostPause

# - PrecSegDur


######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = lyComplete)
cor(lyComplete$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.08288557


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = lyComplete)
cor(lyComplete$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.332384

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = lyComplete)
cor(lyComplete$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.4101062

# so around 41 percent of the variability can be explained by this! That's a lot



##              Do an initial model:
lyComplete$OrderRescale<-lyComplete$Order*0.1



lyComplete.lmer1 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                          OrderRescale +
                          logWordFormFreq+
                          BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                       PrePause+ PostPause + PrecSegDur+
                            
                        (1|Item) + (1|Participant), data = lyComplete)


summary(lyComplete.lmer1)    
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      9.966e-02  5.197e-03  4.259e+02  19.176  < 2e-16 ***
#   Environment2l                   -5.867e-03  3.581e-03  7.540e+01  -1.639  0.10545    
# Environment2l#-l                 3.797e-02  2.874e-03  1.466e+02  13.211  < 2e-16 ***
# Environment2l#-le                3.236e-02  3.793e-03  1.074e+02   8.532 1.02e-13 ***
# Environment2l#l-lely             7.615e-03  3.484e-03  7.500e+01   2.186  0.03194 *  
# Environment2l#l-ll              -4.642e-03  2.335e-03  7.900e+01  -1.988  0.05027 .  
# Environment2syll. l#l            3.059e-03  2.755e-03  1.377e+02   1.111  0.26871    
# Environment2syll.l#              4.743e-02  4.758e-03  5.962e+02   9.968  < 2e-16 ***
# AccentuationConditionunaccented  5.565e-04  1.186e-03  1.336e+03   0.469  0.63900    
# OrderRescale                    -7.920e-05  4.721e-05  1.578e+03  -1.678  0.09363 .  
# logWordFormFreq                 -7.688e-04  3.014e-04  8.370e+01  -2.550  0.01259 *  
#   BaseFinalStressunstressed       -2.435e-03  2.578e-03  9.420e+01  -0.945  0.34715    
# LocSpeech                       -3.888e-03  2.683e-04  1.460e+03 -14.491  < 2e-16 ***
#   GlobalSpeechRate                -5.280e-04  9.849e-04  8.262e+02  -0.536  0.59205    
# TypeOfLtap                      -5.781e-03  2.149e-03  1.562e+03  -2.690  0.00723 ** 
#   PrePausePause                    1.825e-03  1.087e-03  1.456e+03   1.678  0.09351 .  
# PostPausePause                   1.286e-02  1.427e-03  1.574e+03   9.011  < 2e-16 ***
#   PrecSegDur                      -6.620e-02  1.283e-02  1.531e+03  -5.160 2.80e-07 ***

cor(lyComplete$ConsonantDur, fitted(lyComplete.lmer1))^2
#[1]  0.5299878

visreg(lyComplete.lmer1)

#######################################################################################
# Dealing with collinearity                                                          #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own



# 1.  Loc Speech  and/or Global Speech


cor.test(lyComplete$LocSpeech,lyComplete$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  lyComplete$LocSpeech and lyComplete$GlobalSpeechRate
# t = 9.3524, df = 1643, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1784325 0.2702189
# sample estimates:
#       cor 
# 0.2248244  


# not too bad

lyComplete.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = lyComplete)

summary(lyComplete.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.089e-01  3.236e-03  4.501e+02  33.663   <2e-16 ***
#   LocSpeech        -3.966e-03  2.332e-04  1.640e+03 -17.010   <2e-16 ***
#   GlobalSpeechRate -9.410e-04  8.432e-04  1.131e+03  -1.116    0.265   

cor(lyComplete$ConsonantDur, fitted(lyComplete.lmerSpeechRates))^2
#[1]  0.50134



lyComplete.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = lyComplete)

summary(lyComplete.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.080e-01  3.131e-03  4.330e+02   34.49   <2e-16 ***
#   LocSpeech   -4.023e-03  2.276e-04  1.632e+03  -17.68   <2e-16 ***

cor(lyComplete$ConsonantDur, fitted(lyComplete.lmerLocSpeech))^2
#[1] 0.5014159


lyComplete.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = lyComplete)

summary(lyComplete.lmerGlobalSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       7.082e-02  2.590e-03  1.430e+02  27.342  < 2e-16 ***
#   GlobalSpeechRate -4.042e-03  9.004e-04  1.226e+03  -4.489 7.82e-06 ***


cor(lyComplete$ConsonantDur, fitted(lyComplete.lmerGlobalSpeech))^2
#[1] 0.4149674


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, only Loc is signidifcanr
# - The effect direction never changes (no supression)
# - so we can leave both variables in
#################################################


# Let's refit our model incorportaing the "right variables" (its actually the same..)

lyComplete.lmer3 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                          OrderRescale +
                          logWordFormFreq+
                          BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                          PrePause+ PostPause + PrecSegDur+
                          (1|Item) + (1|Participant), data = lyComplete)

summary(lyComplete.lmer3)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      9.966e-02  5.197e-03  4.259e+02  19.176  < 2e-16 ***
#   Environment2l                   -5.867e-03  3.581e-03  7.540e+01  -1.639  0.10545    
# Environment2l#-l                 3.797e-02  2.874e-03  1.466e+02  13.211  < 2e-16 ***
# Environment2l#-le                3.236e-02  3.793e-03  1.074e+02   8.532 1.02e-13 ***
# Environment2l#l-lely             7.615e-03  3.484e-03  7.500e+01   2.186  0.03194 *  
# Environment2l#l-ll              -4.642e-03  2.335e-03  7.900e+01  -1.988  0.05027 .  
# Environment2syll. l#l            3.059e-03  2.755e-03  1.377e+02   1.111  0.26871    
# Environment2syll.l#              4.743e-02  4.758e-03  5.962e+02   9.968  < 2e-16 ***
# AccentuationConditionunaccented  5.565e-04  1.186e-03  1.336e+03   0.469  0.63900    
# OrderRescale                    -7.920e-05  4.721e-05  1.578e+03  -1.678  0.09363 .  
# logWordFormFreq                 -7.688e-04  3.014e-04  8.370e+01  -2.550  0.01259 *  
#   BaseFinalStressunstressed       -2.435e-03  2.578e-03  9.420e+01  -0.945  0.34715    
# LocSpeech                       -3.888e-03  2.683e-04  1.460e+03 -14.491  < 2e-16 ***
#   GlobalSpeechRate                -5.280e-04  9.849e-04  8.262e+02  -0.536  0.59205    
# TypeOfLtap                      -5.781e-03  2.149e-03  1.562e+03  -2.690  0.00723 ** 
#   PrePausePause                    1.825e-03  1.087e-03  1.456e+03   1.678  0.09351 .  
# PostPausePause                   1.286e-02  1.427e-03  1.574e+03   9.011  < 2e-16 ***
#   PrecSegDur                      -6.620e-02  1.283e-02  1.531e+03  -5.160 2.80e-07 ***

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (lyComplete.lmer3))
qqline (residuals (lyComplete.lmer3))

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


lyComplete.lm<-lm(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                     OrderRescale +
                     logWordFormFreq+
                     BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                     PrePause+ PostPause + PrecSegDur
                     , data = lyComplete)

summary(lyComplete.lm)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      9.457e-02  4.090e-03  23.120  < 2e-16 ***
#   Environment2l                   -4.707e-03  2.073e-03  -2.270  0.02332 *  
#   Environment2l#-l                 3.765e-02  2.145e-03  17.552  < 2e-16 ***
# Environment2l#-le                3.037e-02  2.565e-03  11.838  < 2e-16 ***
# Environment2l#l-lely             6.210e-03  2.018e-03   3.077  0.00212 ** 
# Environment2l#l-ll              -2.598e-03  1.375e-03  -1.889  0.05904 .  
# Environment2syll. l#l           -2.055e-03  1.932e-03  -1.064  0.28766    
# Environment2syll.l#              4.642e-02  4.434e-03  10.469  < 2e-16 ***
# AccentuationConditionunaccented  1.277e-03  1.129e-03   1.132  0.25782    
# OrderRescale                    -8.777e-05  5.016e-05  -1.750  0.08034 .  
# logWordFormFreq                 -7.884e-04  1.789e-04  -4.406 1.12e-05 ***
#   BaseFinalStressunstressed       -4.066e-04  1.634e-03  -0.249  0.80345    
# LocSpeech                       -3.629e-03  2.442e-04 -14.862  < 2e-16 ***
#   GlobalSpeechRate                -1.624e-03  7.877e-04  -2.062  0.03937 *  
#   TypeOfLtap                      -5.642e-03  2.216e-03  -2.545  0.01100 *  
#   PrePausePause                    3.025e-03  1.000e-03   3.025  0.00253 ** 
#   PostPausePause                   1.224e-02  1.425e-03   8.587  < 2e-16 ***
#   PrecSegDur                      -3.836e-02  1.229e-02  -3.120  0.00184 ** 

bc<-boxcox(lyComplete.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.06060606

lyComplete$bc <- lyComplete$ConsonantDur^lambda

lyComplete.lmerBC <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                           OrderRescale +
                           logWordFormFreq+
                           BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                           PrePause+ PostPause + PrecSegDur+
                           (1|Item) + (1|Participant), data = lyComplete)

summary(lyComplete.lmerBC)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.782e-01  4.086e-03  4.444e+02 214.942  < 2e-16 ***
#   Environment2l                   -4.390e-03  2.843e-03  8.220e+01  -1.544 0.126364    
# Environment2l#-l                 2.421e-02  2.272e-03  1.570e+02  10.655  < 2e-16 ***
# Environment2l#-le                2.124e-02  3.005e-03  1.159e+02   7.070 1.25e-10 ***
# Environment2l#l-lely             5.001e-03  2.766e-03  8.180e+01   1.808 0.074273 .  
# Environment2l#l-ll              -5.182e-03  1.854e-03  8.600e+01  -2.796 0.006387 ** 
# Environment2syll. l#l            7.222e-04  2.179e-03  1.481e+02   0.331 0.740738    
# Environment2syll.l#              3.235e-02  3.740e-03  6.185e+02   8.648  < 2e-16 ***
# AccentuationConditionunaccented  9.096e-04  9.276e-04  1.331e+03   0.981 0.326997    
# OrderRescale                    -5.947e-05  3.694e-05  1.580e+03  -1.610 0.107549    
# logWordFormFreq                 -5.500e-04  2.392e-04  9.110e+01  -2.300 0.023746 *  
#   BaseFinalStressunstressed       -1.199e-03  2.044e-03  1.021e+02  -0.587 0.558570    
# LocSpeech                       -3.405e-03  2.102e-04  1.481e+03 -16.196  < 2e-16 ***
#   GlobalSpeechRate                -8.184e-04  7.699e-04  8.150e+02  -1.063 0.288109    
# TypeOfLtap                      -6.104e-03  1.681e-03  1.566e+03  -3.630 0.000292 ***
#   PrePausePause                    1.241e-03  8.520e-04  1.481e+03   1.457 0.145281    
# PostPausePause                   8.284e-03  1.117e-03  1.575e+03   7.419 1.92e-13 ***
#   PrecSegDur                      -6.051e-02  1.005e-02  1.548e+03  -6.021 2.17e-09 ***

#let's check the assumptions

qqnorm (residuals (lyComplete.lmerBC))
qqline (residuals (lyComplete.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(lyComplete.lmerBC, lyComplete, trim = 2.5)
# n.removed = 30 
# percent.removed = 1.823708 

lyComplete2<-outliers$data

dim(lyComplete2)
#[1]  1615   88


dim(lyComplete)
#[1]1645   87


# okay it seemes to have worked

lyComplete.lmerBC2 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                            OrderRescale +
                            logWordFormFreq+
                            BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                            PrePause+ PostPause + PrecSegDur+
                            (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.763e-01  3.857e-03  4.468e+02 227.174  < 2e-16 ***
#   Environment2l                   -4.941e-03  2.692e-03  8.430e+01  -1.835  0.06996 .  
# Environment2l#-l                 2.454e-02  2.148e-03  1.596e+02  11.426  < 2e-16 ***
# Environment2l#-le                2.184e-02  2.835e-03  1.173e+02   7.704 4.64e-12 ***
# Environment2l#l-lely             5.237e-03  2.630e-03  8.510e+01   1.991  0.04969 *  
# Environment2l#l-ll              -5.532e-03  1.755e-03  8.830e+01  -3.152  0.00221 ** 
# Environment2syll. l#l            5.258e-04  2.065e-03  1.533e+02   0.255  0.79936    
# Environment2syll.l#              3.373e-02  3.510e-03  6.352e+02   9.609  < 2e-16 ***
# AccentuationConditionunaccented  6.501e-04  8.614e-04  1.374e+03   0.755  0.45058    
# OrderRescale                    -5.797e-05  3.426e-05  1.550e+03  -1.692  0.09080 .  
# logWordFormFreq                 -4.895e-04  2.268e-04  9.360e+01  -2.158  0.03345 *  
#   BaseFinalStressunstressed       -1.308e-03  1.934e-03  1.046e+02  -0.676  0.50047    
# LocSpeech                       -3.247e-03  1.957e-04  1.482e+03 -16.597  < 2e-16 ***
#   GlobalSpeechRate                -4.747e-04  7.196e-04  9.277e+02  -0.660  0.50968    
# TypeOfLtap                      -6.685e-03  1.548e-03  1.538e+03  -4.319 1.67e-05 ***
#   PrePausePause                    9.029e-04  7.919e-04  1.483e+03   1.140  0.25444    
# PostPausePause                   8.766e-03  1.035e-03  1.553e+03   8.469  < 2e-16 ***
#   PrecSegDur                      -6.220e-02  9.386e-03  1.532e+03  -6.627 4.73e-11 ***

qqnorm (residuals (lyComplete.lmerBC2))
qqline (residuals (lyComplete.lmerBC2))

# this looks okayish (not reallly good)

# let me try whether it works better withh log



lyComplete.lmer3LOG <- lmer(log(ConsonantDur) ~ Environment2+ AccentuationCondition+ 
                           OrderRescale +
                           logWordFormFreq+
                           BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                           PrePause+ PostPause + PrecSegDur+
                           (1|Item) + (1|Participant), data = lyComplete)

summary(lyComplete.lmer3LOG)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -2.125e+00  8.025e-02  4.461e+02 -26.478  < 2e-16 ***
#   Environment2l                   -8.564e-02  5.578e-02  8.250e+01  -1.535 0.128562    
# Environment2l#-l                 4.673e-01  4.462e-02  1.579e+02  10.474  < 2e-16 ***
# Environment2l#-le                4.105e-01  5.898e-02  1.164e+02   6.961 2.13e-10 ***
# Environment2l#l-lely             9.616e-02  5.427e-02  8.200e+01   1.772 0.080158 .  
# Environment2l#l-ll              -1.030e-01  3.637e-02  8.640e+01  -2.833 0.005741 ** 
# Environment2syll. l#l            1.167e-02  4.278e-02  1.488e+02   0.273 0.785363    
# Environment2syll.l#              6.266e-01  7.351e-02  6.220e+02   8.525  < 2e-16 ***
# AccentuationConditionunaccented  1.845e-02  1.824e-02  1.326e+03   1.012 0.311947    
# OrderRescale                    -1.159e-03  7.265e-04  1.581e+03  -1.596 0.110705    
# logWordFormFreq                 -1.073e-02  4.693e-03  9.150e+01  -2.287 0.024514 *  
#   BaseFinalStressunstressed       -2.282e-02  4.011e-02  1.025e+02  -0.569 0.570530    
# LocSpeech                       -6.705e-02  4.133e-03  1.478e+03 -16.223  < 2e-16 ***
#   GlobalSpeechRate                -1.652e-02  1.513e-02  8.061e+02  -1.092 0.275332    
# TypeOfLtap                      -1.216e-01  3.307e-02  1.566e+03  -3.678 0.000243 ***
#   PrePausePause                    2.418e-02  1.675e-02  1.479e+03   1.444 0.149083    
# PostPausePause                   1.602e-01  2.196e-02  1.575e+03   7.295 4.71e-13 ***
#   PrecSegDur                      -1.195e+00  1.976e-01  1.547e+03  -6.046 1.86e-09 ***

par(mfrow=c(1,1))


qqnorm (residuals (lyComplete.lmer3LOG))
qqline (residuals (lyComplete.lmer3LOG))

# let's see what happens if we remove residuals


outliers<-romr.fnc(lyComplete.lmer3LOG, lyComplete, trim = 2.5)
# n.removed = 30 
# percent.removed = 1.823708 

lyComplete3<-outliers$data

dim(lyComplete3)
#[1]  1615   88


dim(lyComplete)
#[1]1645   87




lyComplete.lmer3LOG2 <- lmer(log(ConsonantDur) ~ Environment2+ AccentuationCondition+ 
                              OrderRescale +
                              logWordFormFreq+
                              BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                              PrePause+ PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete3)

summary(lyComplete.lmer3LOG2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -2.149e+00  7.550e-02  4.474e+02 -28.466  < 2e-16 ***
#   Environment2l                   -9.825e-02  5.275e-02  8.440e+01  -1.863  0.06598 .  
# Environment2l#-l                 4.711e-01  4.210e-02  1.602e+02  11.190  < 2e-16 ***
# Environment2l#-le                4.196e-01  5.557e-02  1.176e+02   7.551 1.02e-11 ***
# Environment2l#l-lely             1.101e-01  5.154e-02  8.530e+01   2.136  0.03552 *  
# Environment2l#l-ll              -1.099e-01  3.439e-02  8.840e+01  -3.196  0.00193 ** 
# Environment2syll. l#l            9.434e-03  4.048e-02  1.536e+02   0.233  0.81601    
# Environment2syll.l#              6.514e-01  6.886e-02  6.371e+02   9.460  < 2e-16 ***
# AccentuationConditionunaccented  1.280e-02  1.689e-02  1.364e+03   0.758  0.44872    
# OrderRescale                    -1.175e-03  6.720e-04  1.550e+03  -1.749  0.08051 .  
# logWordFormFreq                 -9.326e-03  4.444e-03  9.380e+01  -2.099  0.03853 *  
#   BaseFinalStressunstressed       -2.748e-02  3.790e-02  1.047e+02  -0.725  0.47011    
# LocSpeech                       -6.445e-02  3.834e-03  1.480e+03 -16.809  < 2e-16 ***
#   GlobalSpeechRate                -9.269e-03  1.410e-02  9.111e+02  -0.657  0.51115    
# TypeOfLtap                      -1.346e-01  3.036e-02  1.538e+03  -4.432 1.00e-05 ***
#   PrePausePause                    1.722e-02  1.553e-02  1.481e+03   1.109  0.26780    
# PostPausePause                   1.656e-01  2.035e-02  1.553e+03   8.136 8.88e-16 ***
#   PrecSegDur                      -1.253e+00  1.841e-01  1.532e+03  -6.809 1.40e-11 ***

par(mfrow=c(1,1))


qqnorm (residuals (lyComplete.lmer3LOG2))
qqline (residuals (lyComplete.lmer3LOG2))


# this is actually worse than the other model.



# So, we will work with the okayish model
qqnorm (residuals (lyComplete.lmerBC2))
qqline (residuals (lyComplete.lmerBC2))

# we will check the model after the fitting again

#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(lyComplete.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.763e-01  3.857e-03  4.468e+02 227.174  < 2e-16 ***
#   Environment2l                   -4.941e-03  2.692e-03  8.430e+01  -1.835  0.06996 .  
# Environment2l#-l                 2.454e-02  2.148e-03  1.596e+02  11.426  < 2e-16 ***
# Environment2l#-le                2.184e-02  2.835e-03  1.173e+02   7.704 4.64e-12 ***
# Environment2l#l-lely             5.237e-03  2.630e-03  8.510e+01   1.991  0.04969 *  
# Environment2l#l-ll              -5.532e-03  1.755e-03  8.830e+01  -3.152  0.00221 ** 
# Environment2syll. l#l            5.258e-04  2.065e-03  1.533e+02   0.255  0.79936    
# Environment2syll.l#              3.373e-02  3.510e-03  6.352e+02   9.609  < 2e-16 ***
# AccentuationConditionunaccented  6.501e-04  8.614e-04  1.374e+03   0.755  0.45058    
# OrderRescale                    -5.797e-05  3.426e-05  1.550e+03  -1.692  0.09080 .  
# logWordFormFreq                 -4.895e-04  2.268e-04  9.360e+01  -2.158  0.03345 *  
#   BaseFinalStressunstressed       -1.308e-03  1.934e-03  1.046e+02  -0.676  0.50047    
# LocSpeech                       -3.247e-03  1.957e-04  1.482e+03 -16.597  < 2e-16 ***
#   GlobalSpeechRate                -4.747e-04  7.196e-04  9.277e+02  -0.660  0.50968    
# TypeOfLtap                      -6.685e-03  1.548e-03  1.538e+03  -4.319 1.67e-05 ***
#   PrePausePause                    9.029e-04  7.919e-04  1.483e+03   1.140  0.25444    
# PostPausePause                   8.766e-03  1.035e-03  1.553e+03   8.469  < 2e-16 ***
#   PrecSegDur                      -6.220e-02  9.386e-03  1.532e+03  -6.627 4.73e-11 ***

# let's throw out Stress

lyComplete.lmerBC3 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                             OrderRescale +
                             logWordFormFreq+
                              LocSpeech + GlobalSpeechRate +TypeOfL+
                             PrePause+ PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC3)

# Fixed effects:
#                                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.750e-01  3.354e-03  6.271e+02 260.866  < 2e-16 ***
# Environment2l                   -3.840e-03  2.133e-03  7.870e+01  -1.800   0.0756 .  
# Environment2l#-l                 2.446e-02  2.138e-03  1.622e+02  11.439  < 2e-16 ***
# Environment2l#-le                2.211e-02  2.797e-03  1.273e+02   7.905 1.11e-12 ***
# Environment2l#l-lely             5.696e-03  2.529e-03  9.260e+01   2.252   0.0267 *  
# Environment2l#l-ll              -5.609e-03  1.744e-03  8.960e+01  -3.217   0.0018 ** 
# Environment2syll. l#l            2.840e-04  2.031e-03  1.494e+02   0.140   0.8890    
# Environment2syll.l#              3.348e-02  3.485e-03  6.417e+02   9.604  < 2e-16 ***
# AccentuationConditionunaccented  6.825e-04  8.600e-04  1.378e+03   0.794   0.4276    
# OrderRescale                    -5.779e-05  3.426e-05  1.550e+03  -1.687   0.0918 .  
# logWordFormFreq                 -4.565e-04  2.206e-04  9.670e+01  -2.070   0.0412 *  
# LocSpeech                       -3.254e-03  1.952e-04  1.472e+03 -16.670  < 2e-16 ***
# GlobalSpeechRate                -4.633e-04  7.191e-04  9.217e+02  -0.644   0.5196    
# TypeOfLtap                      -6.662e-03  1.547e-03  1.540e+03  -4.305 1.77e-05 ***
# PrePausePause                    9.469e-04  7.893e-04  1.469e+03   1.200   0.2305    
# PostPausePause                   8.739e-03  1.034e-03  1.556e+03   8.450  < 2e-16 ***
# PrecSegDur                      -6.041e-02  9.035e-03  1.291e+03  -6.686 3.40e-11 ***

anova(lyComplete.lmerBC2,lyComplete.lmerBC3)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    20 -9364.2 -9256.5 4702.1  -9404.2                         
# object 21 -9362.7 -9249.6 4702.3  -9404.7 0.4697      1     0.4931

# model did not become worse


# let's throw out Global Speech Rate

lyComplete.lmerBC4 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                             OrderRescale +
                             logWordFormFreq+
                             LocSpeech + TypeOfL+
                             PrePause+ PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC4)

# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.744e-01  3.215e-03  6.338e+02 271.970  < 2e-16 ***
# Environment2l                   -3.867e-03  2.135e-03  7.860e+01  -1.811  0.07389 .  
# Environment2l#-l                 2.455e-02  2.135e-03  1.608e+02  11.497  < 2e-16 ***
# Environment2l#-le                2.220e-02  2.796e-03  1.266e+02   7.943 9.26e-13 ***
# Environment2l#l-lely             5.760e-03  2.529e-03  9.230e+01   2.277  0.02507 *  
# Environment2l#l-ll              -5.572e-03  1.744e-03  8.940e+01  -3.195  0.00193 ** 
# Environment2syll. l#l            3.369e-04  2.031e-03  1.489e+02   0.166  0.86849    
# Environment2syll.l#              3.353e-02  3.485e-03  6.405e+02   9.620  < 2e-16 ***
# AccentuationConditionunaccented  3.784e-04  7.196e-04  1.564e+03   0.526  0.59907    
# OrderRescale                    -6.146e-05  3.377e-05  1.544e+03  -1.820  0.06898 .  
# logWordFormFreq                 -4.660e-04  2.202e-04  9.590e+01  -2.116  0.03696 *  
# LocSpeech                       -3.246e-03  1.948e-04  1.483e+03 -16.663  < 2e-16 ***
# TypeOfLtap                      -6.683e-03  1.547e-03  1.541e+03  -4.321 1.65e-05 ***
# PrePausePause                    9.429e-04  7.892e-04  1.471e+03   1.195  0.23239    
# PostPausePause                   8.720e-03  1.034e-03  1.557e+03   8.437  < 2e-16 ***
# PrecSegDur                      -6.035e-02  9.033e-03  1.293e+03  -6.681 3.51e-11 ***

anova(lyComplete.lmerBC3,lyComplete.lmerBC4)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    19 -9365.8 -9263.4 4701.9  -9403.8                         
# object 20 -9364.2 -9256.5 4702.1  -9404.2 0.4422      1     0.5061

# nothing has changed


# let's throw ou accent

lyComplete.lmerBC5 <- lmer(bc ~ Environment2+  
                             OrderRescale +
                             logWordFormFreq+
                             LocSpeech + TypeOfL+
                             PrePause+ PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)


summary(lyComplete.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            8.742e-01  3.202e-03  6.350e+02 273.030  < 2e-16 ***
# Environment2l         -3.865e-03  2.133e-03  7.860e+01  -1.812  0.07388 .  
# Environment2l#-l       2.452e-02  2.133e-03  1.606e+02  11.492  < 2e-16 ***
# Environment2l#-le      2.225e-02  2.793e-03  1.266e+02   7.965 8.21e-13 ***
# Environment2l#l-lely   5.831e-03  2.524e-03  9.200e+01   2.310  0.02314 *  
# Environment2l#l-ll    -5.598e-03  1.742e-03  8.930e+01  -3.213  0.00183 ** 
# Environment2syll. l#l  3.718e-04  2.029e-03  1.489e+02   0.183  0.85486    
# Environment2syll.l#    3.355e-02  3.484e-03  6.407e+02   9.630  < 2e-16 ***
# OrderRescale          -6.260e-05  3.369e-05  1.543e+03  -1.858  0.06338 .  
# logWordFormFreq       -4.640e-04  2.201e-04  9.590e+01  -2.108  0.03762 *  
# LocSpeech             -3.207e-03  1.803e-04  1.516e+03 -17.791  < 2e-16 ***
# TypeOfLtap            -6.656e-03  1.545e-03  1.542e+03  -4.307 1.76e-05 ***
# PrePausePause          8.768e-04  7.790e-04  1.467e+03   1.126  0.26051    
# PostPausePause         8.638e-03  1.021e-03  1.564e+03   8.457  < 2e-16 ***
# PrecSegDur            -6.007e-02  9.014e-03  1.284e+03  -6.664 3.95e-11 ***

anova(lyComplete.lmerBC5,lyComplete.lmerBC4)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 18 -9367.5 -9270.5 4701.8  -9403.5                         
# ..1    19 -9365.8 -9263.4 4701.9  -9403.8 0.2762      1     0.5992


# so it did nnot become worse

# let's throw aout PrePause

lyComplete.lmerBC6 <- lmer(bc ~ Environment2+  
                             OrderRescale +
                             logWordFormFreq+
                             LocSpeech + TypeOfL+
                             PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC6)


# Fixed effects:
#                         Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            8.746e-01  3.192e-03  6.454e+02 274.014  < 2e-16 ***
# Environment2l         -3.730e-03  2.146e-03  7.870e+01  -1.738  0.08609 .  
# Environment2l#-l       2.457e-02  2.144e-03  1.603e+02  11.459  < 2e-16 ***
# Environment2l#-le      2.211e-02  2.806e-03  1.262e+02   7.879 1.32e-12 ***
# Environment2l#l-lely   5.672e-03  2.537e-03  9.180e+01   2.236  0.02780 *  
# Environment2l#l-ll    -5.563e-03  1.754e-03  8.970e+01  -3.172  0.00207 ** 
# Environment2syll. l#l  4.071e-04  2.040e-03  1.490e+02   0.200  0.84210    
# Environment2syll.l#    3.357e-02  3.491e-03  6.367e+02   9.617  < 2e-16 ***
# OrderRescale          -6.357e-05  3.367e-05  1.544e+03  -1.888  0.05924 .  
# logWordFormFreq       -4.644e-04  2.215e-04  9.630e+01  -2.096  0.03868 *  
# LocSpeech             -3.208e-03  1.803e-04  1.519e+03 -17.785  < 2e-16 ***
# TypeOfLtap            -6.678e-03  1.545e-03  1.543e+03  -4.323 1.64e-05 ***
# PostPausePause         8.728e-03  1.018e-03  1.566e+03   8.575  < 2e-16 ***
# PrecSegDur            -5.982e-02  9.015e-03  1.285e+03  -6.635 4.76e-11 ***

anova(lyComplete.lmerBC5,lyComplete.lmerBC6)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    17 -9368.1 -9276.6 4701.1  -9402.1                        
# object 18 -9367.5 -9270.5 4701.8  -9403.5 1.368      1     0.2422


# model di not become worse. let's throw out Order

lyComplete.lmerBC7 <- lmer(bc ~ Environment2+  
                             logWordFormFreq+
                             LocSpeech + TypeOfL+
                             PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            8.738e-01  3.164e-03  6.219e+02 276.163  < 2e-16 ***
#   Environment2l         -3.775e-03  2.152e-03  7.850e+01  -1.754  0.08326 .  
# Environment2l#-l       2.461e-02  2.148e-03  1.597e+02  11.454  < 2e-16 ***
# Environment2l#-le      2.209e-02  2.813e-03  1.258e+02   7.853 1.54e-12 ***
# Environment2l#l-lely   5.553e-03  2.543e-03  9.150e+01   2.184  0.03155 *  
# Environment2l#l-ll    -5.525e-03  1.759e-03  8.950e+01  -3.142  0.00228 ** 
# Environment2syll. l#l  3.163e-04  2.044e-03  1.484e+02   0.155  0.87723    
# Environment2syll.l#    3.345e-02  3.495e-03  6.341e+02   9.572  < 2e-16 ***
# logWordFormFreq       -4.698e-04  2.221e-04  9.620e+01  -2.115  0.03701 *  
#   LocSpeech             -3.233e-03  1.801e-04  1.526e+03 -17.955  < 2e-16 ***
#   TypeOfLtap            -6.618e-03  1.546e-03  1.543e+03  -4.282 1.97e-05 ***
#   PostPausePause         8.749e-03  1.019e-03  1.567e+03   8.590  < 2e-16 ***
#   PrecSegDur            -5.990e-02  9.024e-03  1.288e+03  -6.638 4.67e-11 ***

anova(lyComplete.lmerBC7,lyComplete.lmerBC6)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 16 -9366.5 -9280.3 4699.3  -9398.5                           
# ..1    17 -9368.1 -9276.6 4701.1  -9402.1 3.6075      1    0.05752 .


# so that would be the final model without interactions



visreg(lyComplete.lmerBC7,  
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5, ylim=c(30,120))

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view



# 1. Post Pause+ Environment

# 2. Environment and accentuation

# 3. Accen. Post Pause

# 4. Stress and Environment (let's see whether this makes sense)

table(lyComplete2$Environment2,lyComplete2$BaseFinalStress)


# Let's see
# primary unstressed
# #l            101        501
# l             201          0
# l#-l           30         82
# l#-le          64         38
# l#l-lely       91         54
# l#l-ll         42        266
# syll. l#l       0        125
# syll.l#         0         20

# I am not sure, let's just try it


# 1. Post Pause+ Environment
lyComplete.lmerBC7InterPauseEnvironment <- lmer(bc ~ Environment2*PostPause+  
                             logWordFormFreq+
                             LocSpeech + TypeOfL
                              + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterPauseEnvironment)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                           8.779e-01  3.328e-03  7.129e+02 263.784  < 2e-16 ***
#   Environment2l                         5.968e-04  3.533e-03  4.946e+02   0.169  0.86593    
# Environment2l#-l                      1.463e-02  2.689e-03  3.587e+02   5.440 9.88e-08 ***
# Environment2l#-le                     1.601e-02  3.152e-03  2.030e+02   5.079 8.56e-07 ***
# Environment2l#l-lely                  2.103e-03  4.268e-03  5.950e+02   0.493  0.62234    
# Environment2l#l-ll                   -2.867e-03  3.085e-03  6.709e+02  -0.929  0.35307    
# Environment2syll. l#l                 3.957e-03  3.364e-03  7.968e+02   1.176  0.23982    
# Environment2syll.l#                   2.958e-02  3.836e-03  8.071e+02   7.711 3.66e-14 ***
# PostPausePause                        4.813e-03  1.626e-03  1.536e+03   2.959  0.00313 ** 
#   logWordFormFreq                      -4.278e-04  2.214e-04  9.630e+01  -1.933  0.05621 .  
# LocSpeech                            -3.239e-03  1.763e-04  1.532e+03 -18.376  < 2e-16 ***
#   TypeOfLtap                           -6.677e-03  1.505e-03  1.534e+03  -4.435 9.85e-06 ***
#   PrecSegDur                           -6.882e-02  8.924e-03  1.297e+03  -7.712 2.44e-14 ***
#   Environment2l:PostPausePause         -4.817e-03  3.165e-03  1.510e+03  -1.522  0.12821    
# Environment2l#-l:PostPausePause       2.069e-02  2.953e-03  1.571e+03   7.007 3.59e-12 ***
# Environment2l#-le:PostPausePause      1.458e-02  3.053e-03  1.523e+03   4.775 1.97e-06 ***
# Environment2l#l-lely:PostPausePause   4.849e-03  3.853e-03  1.520e+03   1.259  0.20831    
# Environment2l#l-ll:PostPausePause    -3.129e-03  2.837e-03  1.501e+03  -1.103  0.27023    
# Environment2syll. l#l:PostPausePause -4.199e-03  3.256e-03  1.507e+03  -1.290  0.19738    
# Environment2syll.l#:PostPausePause    1.102e-02  8.230e-03  1.577e+03   1.339  0.18078    
# ---

# yes and it makes total sense

visreg(lyComplete.lmerBC7InterPauseEnvironment, "Environment2", by="PostPause", overlay=T)

# good

# 2. Environment and accentuation

lyComplete.lmerBC7InterAccEnvironment <- lmer(bc ~ Environment2*AccentuationCondition+
                                                PostPause+  
                                                  logWordFormFreq+
                                                  LocSpeech + TypeOfL
                                                + PrecSegDur+
                                                  (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterAccEnvironment)
# Linear mixed model fit by REML 
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: bc ~ Environment2 * AccentuationCondition + PostPause + logWordFormFreq +  
#   LocSpeech + TypeOfL + PrecSegDur + (1 | Item) + (1 | Participant)
# Data: lyComplete2
# 
# REML criterion at convergence: -9206.3
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.91096 -0.62351  0.03308  0.65958  2.65682 
# 
# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 2.877e-05 0.005363
# Participant (Intercept) 2.000e-05 0.004472
# Residual                1.534e-04 0.012385
# Number of obs: 1615, groups:  Item, 103; Participant, 22
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            8.756e-01  3.211e-03  6.219e+02 272.649  < 2e-16 ***
#   Environment2l                                         -3.773e-03  2.389e-03  1.162e+02  -1.579 0.116986    
# Environment2l#-l                                       2.836e-02  2.539e-03  2.765e+02  11.169  < 2e-16 ***
# Environment2l#-le                                      2.858e-02  3.192e-03  1.986e+02   8.953 2.22e-16 ***
# Environment2l#l-lely                                   9.823e-03  2.826e-03  1.350e+02   3.475 0.000687 ***
# Environment2l#l-ll                                    -5.575e-03  1.942e-03  1.304e+02  -2.870 0.004784 ** 
# Environment2syll. l#l                                 -2.904e-04  2.541e-03  3.183e+02  -0.114 0.909092    
# Environment2syll.l#                                    3.946e-02  6.718e-03  1.442e+03   5.874 5.29e-09 ***
# AccentuationConditionunaccented                        2.098e-03  1.058e-03  1.507e+03   1.984 0.047421 *  
#   PostPausePause                                         7.760e-03  1.042e-03  1.550e+03   7.445 1.60e-13 ***
#   logWordFormFreq                                       -4.523e-04  2.234e-04  9.620e+01  -2.024 0.045691 *  
#   LocSpeech                                             -3.351e-03  1.934e-04  1.482e+03 -17.326  < 2e-16 ***
#   TypeOfLtap                                            -6.735e-03  1.533e-03  1.534e+03  -4.394 1.19e-05 ***
#   PrecSegDur                                            -6.836e-02  9.078e-03  1.286e+03  -7.530 9.50e-14 ***
#   Environment2l:AccentuationConditionunaccented          1.731e-04  2.033e-03  1.487e+03   0.085 0.932158    
# Environment2l#-l:AccentuationConditionunaccented      -7.997e-03  2.659e-03  1.559e+03  -3.007 0.002677 ** 
# Environment2l#-le:AccentuationConditionunaccented     -1.212e-02  2.762e-03  1.534e+03  -4.389 1.22e-05 ***
# Environment2l#l-lely:AccentuationConditionunaccented  -7.509e-03  2.316e-03  1.491e+03  -3.242 0.001211 ** 
# Environment2l#l-ll:AccentuationConditionunaccented     1.160e-04  1.759e-03  1.493e+03   0.066 0.947449    
# Environment2syll. l#l:AccentuationConditionunaccented  1.021e-03  2.518e-03  1.505e+03   0.405 0.685235    
# Environment2syll.l#:AccentuationConditionunaccented   -9.276e-03  7.398e-03  1.577e+03  -1.254 0.210093  
# okay, let's see


visreg(lyComplete.lmerBC7InterAccEnvironment, "Environment2", by="AccentuationCondition", overlay=T)


# again for base words they are longer when accented

# 3. Accen. Post Pause

lyComplete.lmerBC7InterPauseAcc <- lmer(bc ~ Environment2+AccentuationCondition*PostPause+  
                                                  logWordFormFreq+
                                                  LocSpeech + TypeOfL
                                                + PrecSegDur+
                                                  (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterPauseAcc)


# no effect!

# 4. Stress and Environment

lyComplete.lmerBC7InterStressEnvironment <- lmer(bc ~ Environment2*BaseFinalStress+PostPause+  
                                                  logWordFormFreq+
                                                  LocSpeech + TypeOfL
                                                + PrecSegDur+
                                                  (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterStressEnvironment)

# no, okay, so we have 2 interactions Env*Acc and Env*PostPause

# let's look at a three way interaction

# 5. Env*PstPause*Acc

lyComplete.lmerBC7InterAccPauseEnvironment <- lmer(bc ~ Environment2*AccentuationCondition*PostPause+  
                                                   logWordFormFreq+
                                                   LocSpeech + TypeOfL
                                                 + PrecSegDur+
                                                   (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterAccPauseEnvironment)
# not significant

# so what if we have both interactions

# 6. Acc and Env, and Env and Paise

lyComplete.lmerBC7InterAccEnvPauseEnvironment <- lmer(bc ~ Environment2*AccentuationCondition+Environment2*PostPause+  
                                                     logWordFormFreq+
                                                     LocSpeech + TypeOfL
                                                   + PrecSegDur+
                                                     (1|Item) + (1|Participant), data = lyComplete2)

summary(lyComplete.lmerBC7InterAccEnvPauseEnvironment)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            8.778e-01  3.392e-03  7.297e+02 258.760  < 2e-16 ***
#   Environment2l                                          1.004e-03  3.870e-03  6.325e+02   0.260 0.795329    
# Environment2l#-l                                       1.425e-02  3.402e-03  6.569e+02   4.187 3.21e-05 ***
# Environment2l#-le                                      2.153e-02  3.828e-03  3.979e+02   5.623 3.54e-08 ***
# Environment2l#l-lely                                   8.708e-03  4.785e-03  7.856e+02   1.820 0.069145 .  
# Environment2l#l-ll                                    -2.647e-03  3.314e-03  7.976e+02  -0.799 0.424781    
# Environment2syll. l#l                                  3.370e-03  3.750e-03  9.842e+02   0.899 0.368993    
# Environment2syll.l#                                    3.586e-02  1.286e-02  1.570e+03   2.788 0.005366 ** 
# AccentuationConditionunaccented                        1.743e-03  1.046e-03  1.500e+03   1.667 0.095718 .  
# PostPausePause                                         5.224e-03  1.640e-03  1.527e+03   3.186 0.001471 ** 
#   logWordFormFreq                                       -4.081e-04  2.226e-04  9.610e+01  -1.833 0.069844 .  
# LocSpeech                                             -3.327e-03  1.904e-04  1.486e+03 -17.471  < 2e-16 ***
#   TypeOfLtap                                            -6.724e-03  1.502e-03  1.526e+03  -4.477 8.13e-06 ***
#   PrecSegDur                                            -7.183e-02  8.972e-03  1.297e+03  -8.006 2.66e-15 ***
#   Environment2l:AccentuationConditionunaccented         -5.517e-04  2.032e-03  1.483e+03  -0.272 0.785994    
# Environment2l#-l:AccentuationConditionunaccented       5.815e-04  2.872e-03  1.555e+03   0.202 0.839577    
# Environment2l#-le:AccentuationConditionunaccented     -7.805e-03  2.998e-03  1.531e+03  -2.603 0.009325 ** 
# Environment2l#l-lely:AccentuationConditionunaccented  -7.405e-03  2.350e-03  1.486e+03  -3.151 0.001659 ** 
# Environment2l#l-ll:AccentuationConditionunaccented    -2.435e-04  1.746e-03  1.485e+03  -0.139 0.889083    
# Environment2syll. l#l:AccentuationConditionunaccented  1.143e-03  2.473e-03  1.496e+03   0.462 0.644057    
# Environment2syll.l#:AccentuationConditionunaccented   -6.984e-03  1.315e-02  1.566e+03  -0.531 0.595398    
# Environment2l:PostPausePause                          -4.911e-03  3.220e-03  1.505e+03  -1.525 0.127384    
# Environment2l#-l:PostPausePause                        2.125e-02  3.223e-03  1.566e+03   6.594 5.85e-11 ***
# Environment2l#-le:PostPausePause                       1.117e-02  3.353e-03  1.520e+03   3.330 0.000888 ***
# Environment2l#l-lely:PostPausePause                    1.705e-03  3.985e-03  1.513e+03   0.428 0.668889    
# Environment2l#l-ll:PostPausePause                     -3.189e-03  2.866e-03  1.493e+03  -1.113 0.265897    
# Environment2syll. l#l:PostPausePause                  -4.451e-03  3.257e-03  1.498e+03  -1.367 0.171948    
# Environment2syll.l#:PostPausePause                     5.555e-03  1.483e-02  1.569e+03   0.375 0.708003    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# let's see whether this is really the best model


anova(lyComplete.lmerBC7,lyComplete.lmerBC7InterAccEnvPauseEnvironment)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 16 -9366.5 -9280.3 4699.3  -9398.5                             
# ..1    31 -9446.6 -9279.6 4754.3  -9508.6 110.04     15  < 2.2e-16 ***

anova(lyComplete.lmerBC7InterPauseEnvironment,lyComplete.lmerBC7InterAccEnvPauseEnvironment)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 23 -9443.8 -9319.9 4744.9  -9489.8                           
# ..1    31 -9446.6 -9279.6 4754.3  -9508.6 18.803      8    0.01595 **

anova(lyComplete.lmerBC7InterAccEnvironment,lyComplete.lmerBC7InterAccEnvPauseEnvironment)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 24 -9388.2 -9258.9 4718.1  -9436.2                             
# ..1    31 -9446.6 -9279.6 4754.3  -9508.6 72.345      7  4.955e-13 ***


# yes so that is the best
##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# So there are two interactions: 

# 1. Acc and Environment 


visreg(lyComplete.lmerBC7InterAccEnvPauseEnvironment, "Environment2",by="AccentuationCondition", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds",overlay=T, 
       cex.axis=0.5)

# 2. Environment and Pst Pause


visreg(lyComplete.lmerBC7InterAccEnvPauseEnvironment, "Environment2",by="PostPause", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds",overlay=T, 
       cex.axis=0.5)



#############################################################
# The final model:

summary(lyComplete.lmerBC7InterAccEnvPauseEnvironment)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            8.778e-01  3.392e-03  7.297e+02 258.760  < 2e-16 ***
#   Environment2l                                          1.004e-03  3.870e-03  6.325e+02   0.260 0.795329    
# Environment2l#-l                                       1.425e-02  3.402e-03  6.569e+02   4.187 3.21e-05 ***
# Environment2l#-le                                      2.153e-02  3.828e-03  3.979e+02   5.623 3.54e-08 ***
# Environment2l#l-lely                                   8.708e-03  4.785e-03  7.856e+02   1.820 0.069145 .  
# Environment2l#l-ll                                    -2.647e-03  3.314e-03  7.976e+02  -0.799 0.424781    
# Environment2syll. l#l                                  3.370e-03  3.750e-03  9.842e+02   0.899 0.368993    
# Environment2syll.l#                                    3.586e-02  1.286e-02  1.570e+03   2.788 0.005366 ** 
# AccentuationConditionunaccented                        1.743e-03  1.046e-03  1.500e+03   1.667 0.095718 .  
# PostPausePause                                         5.224e-03  1.640e-03  1.527e+03   3.186 0.001471 ** 
#   logWordFormFreq                                       -4.081e-04  2.226e-04  9.610e+01  -1.833 0.069844 .  
# LocSpeech                                             -3.327e-03  1.904e-04  1.486e+03 -17.471  < 2e-16 ***
#   TypeOfLtap                                            -6.724e-03  1.502e-03  1.526e+03  -4.477 8.13e-06 ***
#   PrecSegDur                                            -7.183e-02  8.972e-03  1.297e+03  -8.006 2.66e-15 ***
#   Environment2l:AccentuationConditionunaccented         -5.517e-04  2.032e-03  1.483e+03  -0.272 0.785994    
# Environment2l#-l:AccentuationConditionunaccented       5.815e-04  2.872e-03  1.555e+03   0.202 0.839577    
# Environment2l#-le:AccentuationConditionunaccented     -7.805e-03  2.998e-03  1.531e+03  -2.603 0.009325 ** 
# Environment2l#l-lely:AccentuationConditionunaccented  -7.405e-03  2.350e-03  1.486e+03  -3.151 0.001659 ** 
# Environment2l#l-ll:AccentuationConditionunaccented    -2.435e-04  1.746e-03  1.485e+03  -0.139 0.889083    
# Environment2syll. l#l:AccentuationConditionunaccented  1.143e-03  2.473e-03  1.496e+03   0.462 0.644057    
# Environment2syll.l#:AccentuationConditionunaccented   -6.984e-03  1.315e-02  1.566e+03  -0.531 0.595398    
# Environment2l:PostPausePause                          -4.911e-03  3.220e-03  1.505e+03  -1.525 0.127384    
# Environment2l#-l:PostPausePause                        2.125e-02  3.223e-03  1.566e+03   6.594 5.85e-11 ***
# Environment2l#-le:PostPausePause                       1.117e-02  3.353e-03  1.520e+03   3.330 0.000888 ***
# Environment2l#l-lely:PostPausePause                    1.705e-03  3.985e-03  1.513e+03   0.428 0.668889    
# Environment2l#l-ll:PostPausePause                     -3.189e-03  2.866e-03  1.493e+03  -1.113 0.265897    
# Environment2syll. l#l:PostPausePause                  -4.451e-03  3.257e-03  1.498e+03  -1.367 0.171948    
# Environment2syll.l#:PostPausePause                     5.555e-03  1.483e-02  1.569e+03   0.375 0.708003

# I need to rename some variabels for the plot...

lyComplete2<-rename(lyComplete2,PrecedingSegmentDuration=PrecSegDur)

lyComplete2<-rename(lyComplete2,AccentuationAnnotator=Accentuation)

lyComplete2<-rename(lyComplete2,Accentuation=AccentuationCondition)

levels(lyComplete2$PostPause)
#[1] "No Pause" "Pause"   

levels(lyComplete2$PostPause)<-c("no pause","pause")

levels(lyComplete2$Environment2)
#[1] [1] "#l"        "l"         "l#-l"      "l#-le"     "l#l-lely"  "l#l-ll"    "syll. l#l" "syll.l#"  


levels(lyComplete2$Environment2) <-c( "#l-<l>"  ,      "l-<ll>"    ,     "l#-<l>"  ,    "l#-<le>" ,    "l#l-<lel>" , "l#l-<ll>" ,   "syll.l#l-<ll>",    "syll.l#-<l>")



# we need the following order

# "l#l-<ll>", "syll.l#l-<ll>", "l#l-<lel>", "#l-<l>" , "l#-<l>", syll"l#-<l>"        "l#-<le>" ,  "l-<ll>"


# so, let's do it
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l-<ll>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#-<le>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "syll.l#-<l>" )
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#-<l>" )
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "#l-<l>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#l-<lel>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "syll.l#l-<ll>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#l-<ll>")



final_ly_complex_model.lmer<- lmer(bc ~ Environment2*Accentuation+Environment2*PostPause+  
                                     logWordFormFreq+
                                     LocSpeech + TypeOfL
                                   + PrecedingSegmentDuration+
                                     (1|Item) + (1|Participant), data = lyComplete2)


summary(final_ly_complex_model.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                       8.752e-01  3.982e-03  8.947e+02 219.785  < 2e-16 ***
#   Environment2syll.l#l-<ll>                         6.017e-03  3.967e-03  1.543e+03   1.517 0.129525    
# Environment2l#l-<lel>                             1.136e-02  5.213e-03  8.717e+02   2.178 0.029641 *  
# Environment2#l-<l>                                2.647e-03  3.314e-03  7.976e+02   0.799 0.424781    
# Environment2l#-<l>                                1.689e-02  3.884e-03  8.504e+02   4.349 1.54e-05 ***
# Environment2syll.l#-<l>                           3.851e-02  1.300e-02  1.570e+03   2.962 0.003103 ** 
# Environment2l#-<le>                               2.417e-02  4.319e-03  5.423e+02   5.597 3.46e-08 ***
# Environment2l-<ll>                                3.651e-03  4.346e-03  7.591e+02   0.840 0.401137    
# Accentuationunaccented                            1.500e-03  1.455e-03  1.505e+03   1.031 0.302721    
# PostPausepause                                    2.035e-03  2.440e-03  1.504e+03   0.834 0.404521    
# logWordFormFreq                                  -4.081e-04  2.226e-04  9.610e+01  -1.833 0.069844 .  
# LocSpeech                                        -3.327e-03  1.904e-04  1.486e+03 -17.471  < 2e-16 ***
#   TypeOfLtap                                       -6.724e-03  1.502e-03  1.526e+03  -4.477 8.13e-06 ***
#   PrecedingSegmentDuration                         -7.183e-02  8.972e-03  1.297e+03  -8.006 2.66e-15 ***
#   Environment2syll.l#l-<ll>:Accentuationunaccented  1.386e-03  2.682e-03  1.501e+03   0.517 0.605346    
# Environment2l#l-<lel>:Accentuationunaccented     -7.161e-03  2.556e-03  1.488e+03  -2.802 0.005147 ** 
# Environment2#l-<l>:Accentuationunaccented         2.435e-04  1.746e-03  1.485e+03   0.139 0.889083    
# Environment2l#-<l>:Accentuationunaccented         8.251e-04  3.044e-03  1.550e+03   0.271 0.786363    
# Environment2syll.l#-<l>:Accentuationunaccented   -6.741e-03  1.319e-02  1.565e+03  -0.511 0.609445    
# Environment2l#-<le>:Accentuationunaccented       -7.562e-03  3.160e-03  1.527e+03  -2.393 0.016821 *  
# Environment2l-<ll>:Accentuationunaccented        -3.082e-04  2.268e-03  1.487e+03  -0.136 0.891947    
# Environment2syll.l#l-<ll>:PostPausepause         -1.262e-03  3.739e-03  1.498e+03  -0.337 0.735826    
# Environment2l#l-<lel>:PostPausepause              4.894e-03  4.384e-03  1.510e+03   1.116 0.264399    
# Environment2#l-<l>:PostPausepause                 3.189e-03  2.866e-03  1.493e+03   1.113 0.265897    
# Environment2l#-<l>:PostPausepause                 2.444e-02  3.674e-03  1.548e+03   6.652 4.00e-11 ***
# Environment2syll.l#-<l>:PostPausepause            8.744e-03  1.495e-02  1.569e+03   0.585 0.558573    
# Environment2l#-<le>:PostPausepause                1.436e-02  3.797e-03  1.511e+03   3.780 0.000163 ***
# Environment2l-<ll>:PostPausepause                -1.722e-03  3.693e-03  1.500e+03  -0.466 0.641153   

#########################################################################
# Let's try out a model with a simple environment structure

lyComplete$SimpleEnvironment<-as.factor(lyComplete$Environment2)

levels(lyComplete$SimpleEnvironment)
#[1] "#l"        "l"         "l#-l"      "l#-le"     "l#l-lely"  "l#l-ll"    "syll. l#l" "syll.l#"  

levels(lyComplete$SimpleEnvironment)<-c( "#l"   ,     "l"      ,   "l#"   ,   "l#"   ,  "l#l",  "l#l",    "l#l", "l#")  

# levels(lyComplete$SimpleEnvironment)
# [1] "#l"  "l"   "l#"  "l#l"

levels(lyComplete$Orthography)
#[1] "l"    "le"   "lely" "ll"  

levels(lyComplete$Orthography)<-c("l", "le", "lel", "ll")


###############################################
##              Do a second initial model:



lyComplete.lmer1a <- lmer(ConsonantDur ~ SimpleEnvironment+ Orthography+syllabicity+ AccentuationCondition+ 
                           OrderRescale +
                           logWordFormFreq+
                           BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                           PrePause+ PostPause + PrecSegDur+
                           (1|Item) + (1|Participant), data = lyComplete)


summary(lyComplete.lmer1a)    
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      9.973e-02  5.196e-03  4.277e+02  19.192  < 2e-16 ***
#   SimpleEnvironmentl              -5.854e-03  3.585e-03  7.620e+01  -1.633 0.106629    
# SimpleEnvironmentl#              3.822e-02  2.784e-03  1.383e+02  13.725  < 2e-16 ***
# SimpleEnvironmentl#l            -4.744e-03  2.320e-03  7.780e+01  -2.044 0.044307 *  
# Orthographyle                   -5.825e-03  4.025e-03  1.176e+02  -1.447 0.150573    
# Orthographylel                   1.239e-02  3.772e-03  8.110e+01   3.284 0.001511 ** 
#   syllabicityyes                   8.037e-03  2.109e-03  1.388e+03   3.811 0.000144 ***
#   AccentuationConditionunaccented  5.526e-04  1.186e-03  1.337e+03   0.466 0.641258    
# OrderRescale                    -7.902e-05  4.719e-05  1.579e+03  -1.674 0.094232 .  
# logWordFormFreq                 -7.707e-04  3.018e-04  8.450e+01  -2.554 0.012444 *  
#   BaseFinalStressunstressed       -2.422e-03  2.580e-03  9.520e+01  -0.939 0.350341    
# LocSpeech                       -3.892e-03  2.682e-04  1.463e+03 -14.510  < 2e-16 ***
#   GlobalSpeechRate                -5.211e-04  9.845e-04  8.283e+02  -0.529 0.596711    
# TypeOfLtap                      -5.778e-03  2.149e-03  1.563e+03  -2.689 0.007237 ** 
#   PrePausePause                    1.827e-03  1.087e-03  1.460e+03   1.680 0.093101 .  
# PostPausePause                   1.283e-02  1.424e-03  1.574e+03   9.008  < 2e-16 ***
#   PrecSegDur                      -6.653e-02  1.280e-02  1.531e+03  -5.199 2.27e-07 ***

cor(lyComplete$ConsonantDur, fitted(lyComplete.lmer1))^2
#[1]  0.5301043

visreg(lyComplete.lmer1a)


###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (lyComplete.lmer1a))
qqline (residuals (lyComplete.lmer1a))

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


lyComplete.lmerBCa <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+  AccentuationCondition+ 
                            OrderRescale +
                            logWordFormFreq+
                            BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                            PrePause+ PostPause + PrecSegDur+
                            (1|Item) + (1|Participant), data = lyComplete)

summary(lyComplete.lmerBCa)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.783e-01  4.088e-03  4.467e+02 214.829  < 2e-16 ***
#   SimpleEnvironmentl              -4.372e-03  2.852e-03  8.330e+01  -1.533 0.129099    
# SimpleEnvironmentl#              2.453e-02  2.206e-03  1.485e+02  11.119  < 2e-16 ***
# SimpleEnvironmentl#l            -5.312e-03  1.846e-03  8.510e+01  -2.878 0.005062 ** 
# Orthographyle                   -3.245e-03  3.192e-03  1.268e+02  -1.017 0.311310    
# Orthographylel                   1.035e-02  2.999e-03  8.850e+01   3.451 0.000858 ***
#   syllabicityyes                   6.337e-03  1.653e-03  1.423e+03   3.833 0.000132 ***
#   AccentuationConditionunaccented  9.046e-04  9.273e-04  1.332e+03   0.975 0.329490    
# OrderRescale                    -5.925e-05  3.692e-05  1.581e+03  -1.605 0.108739    
# logWordFormFreq                 -5.523e-04  2.399e-04  9.240e+01  -2.302 0.023561 *  
#   BaseFinalStressunstressed       -1.181e-03  2.049e-03  1.034e+02  -0.576 0.565547    
# LocSpeech                       -3.409e-03  2.102e-04  1.486e+03 -16.222  < 2e-16 ***
#   GlobalSpeechRate                -8.098e-04  7.696e-04  8.178e+02  -1.052 0.293040    
# TypeOfLtap                      -6.098e-03  1.681e-03  1.567e+03  -3.628 0.000294 ***
#   PrePausePause                    1.243e-03  8.520e-04  1.487e+03   1.459 0.144724    
# PostPausePause                   8.246e-03  1.114e-03  1.576e+03   7.400 2.20e-13 ***
#   PrecSegDur                      -6.095e-02  1.003e-02  1.550e+03  -6.080 1.51e-09 ***

#let's check the assumptions

qqnorm (residuals (lyComplete.lmerBCa))
qqline (residuals (lyComplete.lmerBCa))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(lyComplete.lmerBCa, lyComplete, trim = 2.5)
# n.removed = 30 
# percent.removed = 1.823708 

lyComplete2a<-outliers$data

dim(lyComplete2a)
#[1]  1615   88


dim(lyComplete)
#[1]1645   87


# okay it seemes to have worked

lyComplete.lmerBC2a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+AccentuationCondition+ 
                             OrderRescale +
                             logWordFormFreq+
                             BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                             PrePause+ PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC2a)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.764e-01  3.863e-03  4.479e+02 226.875  < 2e-16 ***
#   SimpleEnvironmentl              -4.922e-03  2.708e-03  8.540e+01  -1.818  0.07264 .  
# SimpleEnvironmentl#              2.498e-02  2.090e-03  1.505e+02  11.950  < 2e-16 ***
# SimpleEnvironmentl#l            -5.710e-03  1.753e-03  8.730e+01  -3.257  0.00161 ** 
# Orthographyle                   -3.080e-03  3.020e-03  1.280e+02  -1.020  0.30973    
# Orthographylel                   1.100e-02  2.858e-03  9.200e+01   3.848  0.00022 ***
#   syllabicityyes                   6.666e-03  1.548e-03  1.439e+03   4.305 1.78e-05 ***
#   AccentuationConditionunaccented  6.390e-04  8.611e-04  1.376e+03   0.742  0.45816    
# OrderRescale                    -5.796e-05  3.424e-05  1.551e+03  -1.693  0.09073 .  
# logWordFormFreq                 -4.945e-04  2.280e-04  9.480e+01  -2.169  0.03257 *  
#   BaseFinalStressunstressed       -1.289e-03  1.944e-03  1.058e+02  -0.663  0.50866    
# LocSpeech                       -3.253e-03  1.957e-04  1.488e+03 -16.628  < 2e-16 ***
#   GlobalSpeechRate                -4.613e-04  7.194e-04  9.311e+02  -0.641  0.52157    
# TypeOfLtap                      -6.680e-03  1.547e-03  1.539e+03  -4.317 1.68e-05 ***
#   PrePausePause                    8.999e-04  7.922e-04  1.489e+03   1.136  0.25614    
# PostPausePause                   8.712e-03  1.033e-03  1.553e+03   8.434  < 2e-16 ***
#   PrecSegDur                      -6.285e-02  9.365e-03  1.535e+03  -6.711 2.71e-11 ***

qqnorm (residuals (lyComplete.lmerBC2))
qqline (residuals (lyComplete.lmerBC2))

# this looks okayish (not reallly good)

# let's simplify

# first Glob Speech

lyComplete.lmerBC3a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+AccentuationCondition+ 
                              OrderRescale +
                              logWordFormFreq+
                              BaseFinalStress + LocSpeech + TypeOfL+
                              PrePause+ PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC3a)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.758e-01  3.729e-03  4.174e+02 234.869  < 2e-16 ***
#   SimpleEnvironmentl              -4.923e-03  2.710e-03  8.530e+01  -1.816 0.072806 .  
# SimpleEnvironmentl#              2.506e-02  2.088e-03  1.494e+02  12.002  < 2e-16 ***
# SimpleEnvironmentl#l            -5.672e-03  1.754e-03  8.710e+01  -3.234 0.001723 ** 
# Orthographyle                   -3.059e-03  3.022e-03  1.278e+02  -1.013 0.313205    
# Orthographylel                   1.103e-02  2.860e-03  9.180e+01   3.858 0.000212 ***
#   syllabicityyes                   6.668e-03  1.548e-03  1.440e+03   4.307 1.77e-05 ***
#   AccentuationConditionunaccented  3.371e-04  7.217e-04  1.554e+03   0.467 0.640472    
# OrderRescale                    -6.161e-05  3.376e-05  1.545e+03  -1.825 0.068236 .  
# logWordFormFreq                 -5.031e-04  2.278e-04  9.420e+01  -2.209 0.029586 *  
#   BaseFinalStressunstressed       -1.259e-03  1.945e-03  1.055e+02  -0.647 0.518749    
# LocSpeech                       -3.246e-03  1.953e-04  1.499e+03 -16.621  < 2e-16 ***
#   TypeOfLtap                      -6.701e-03  1.546e-03  1.540e+03  -4.333 1.57e-05 ***
#   PrePausePause                    8.969e-04  7.921e-04  1.491e+03   1.132 0.257676    
# PostPausePause                   8.693e-03  1.032e-03  1.554e+03   8.421  < 2e-16 ***
#   PrecSegDur                      -6.274e-02  9.361e-03  1.535e+03  -6.702 2.87e-11 ***

anova(lyComplete.lmerBC2a,lyComplete.lmerBC3a)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    19 -9365.4 -9263.1 4701.7  -9403.4                         
# object 20 -9363.9 -9256.1 4701.9  -9403.9 0.4386      1     0.5078

# okay, good, now Stress


lyComplete.lmerBC4a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+AccentuationCondition+ 
                              OrderRescale +
                              logWordFormFreq+LocSpeech + TypeOfL+
                              PrePause+ PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC4a)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.745e-01  3.214e-03  6.358e+02 272.108  < 2e-16 ***
#   SimpleEnvironmentl              -3.862e-03  2.148e-03  7.970e+01  -1.798   0.0759 .  
# SimpleEnvironmentl#              2.498e-02  2.078e-03  1.520e+02  12.023  < 2e-16 ***
# SimpleEnvironmentl#l            -5.747e-03  1.742e-03  8.840e+01  -3.299   0.0014 ** 
# Orthographyle                   -2.723e-03  2.965e-03  1.433e+02  -0.918   0.3600    
# Orthographylel                   1.155e-02  2.731e-03  1.036e+02   4.230 5.06e-05 ***
#   syllabicityyes                   6.508e-03  1.531e-03  1.381e+03   4.249 2.29e-05 ***
#   AccentuationConditionunaccented  3.753e-04  7.193e-04  1.564e+03   0.522   0.6020    
# OrderRescale                    -6.135e-05  3.376e-05  1.545e+03  -1.817   0.0694 .  
# logWordFormFreq                 -4.710e-04  2.213e-04  9.720e+01  -2.128   0.0359 *  
#   LocSpeech                       -3.252e-03  1.948e-04  1.490e+03 -16.694  < 2e-16 ***
#   TypeOfLtap                      -6.678e-03  1.546e-03  1.542e+03  -4.320 1.66e-05 ***
#   PrePausePause                    9.392e-04  7.895e-04  1.478e+03   1.190   0.2344    
# PostPausePause                   8.668e-03  1.031e-03  1.557e+03   8.404  < 2e-16 ***
#   PrecSegDur                      -6.103e-02  9.012e-03  1.299e+03  -6.772 1.91e-11 ***

anova(lyComplete.lmerBC4a,lyComplete.lmerBC3a)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 18 -9367.0 -9270.1 4701.5  -9403.0                         
# ..1    19 -9365.4 -9263.1 4701.7  -9403.4 0.4293      1     0.5123



# okay now Acc

lyComplete.lmerBC5a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+ 
                              OrderRescale +
                              logWordFormFreq+LocSpeech + TypeOfL+
                              PrePause+ PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC5a)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)           8.744e-01  3.201e-03  6.373e+02 273.180  < 2e-16 ***
#   SimpleEnvironmentl   -3.860e-03  2.147e-03  7.970e+01  -1.798  0.07592 .  
# SimpleEnvironmentl#   2.495e-02  2.076e-03  1.518e+02  12.018  < 2e-16 ***
# SimpleEnvironmentl#l -5.772e-03  1.740e-03  8.830e+01  -3.316  0.00132 ** 
# Orthographyle        -2.648e-03  2.960e-03  1.432e+02  -0.894  0.37257    
# Orthographylel        1.165e-02  2.724e-03  1.031e+02   4.276 4.26e-05 ***
#   syllabicityyes        6.567e-03  1.527e-03  1.390e+03   4.301 1.82e-05 ***
#   OrderRescale         -6.248e-05  3.368e-05  1.544e+03  -1.855  0.06376 .  
# logWordFormFreq      -4.690e-04  2.212e-04  9.720e+01  -2.120  0.03653 *  
#   LocSpeech            -3.214e-03  1.802e-04  1.523e+03 -17.829  < 2e-16 ***
#   TypeOfLtap           -6.651e-03  1.545e-03  1.543e+03  -4.306 1.77e-05 ***
#   PrePausePause         8.735e-04  7.792e-04  1.475e+03   1.121  0.26243    
# PostPausePause        8.587e-03  1.019e-03  1.564e+03   8.424  < 2e-16 ***
#   PrecSegDur           -6.075e-02  8.992e-03  1.291e+03  -6.755 2.15e-11 ***

anova(lyComplete.lmerBC4a,lyComplete.lmerBC5a)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    17 -9368.7 -9277.2 4701.4  -9402.7                         
# object 18 -9367.0 -9270.1 4701.5  -9403.0 0.2725      1     0.6017


# now PrePause

lyComplete.lmerBC6a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+ 
                              OrderRescale +
                              logWordFormFreq+LocSpeech + TypeOfL+
                              PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC6a)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)           8.748e-01  3.190e-03  6.472e+02 274.192  < 2e-16 ***
#   SimpleEnvironmentl   -3.726e-03  2.159e-03  7.980e+01  -1.726  0.08824 .  
# SimpleEnvironmentl#   2.499e-02  2.086e-03  1.514e+02  11.981  < 2e-16 ***
# SimpleEnvironmentl#l -5.736e-03  1.752e-03  8.870e+01  -3.274  0.00151 ** 
# Orthographyle        -2.829e-03  2.971e-03  1.421e+02  -0.952  0.34262    
# Orthographylel        1.145e-02  2.734e-03  1.025e+02   4.188 5.97e-05 ***
#   syllabicityyes        6.562e-03  1.528e-03  1.397e+03   4.296 1.86e-05 ***
#   OrderRescale         -6.345e-05  3.366e-05  1.545e+03  -1.885  0.05962 .  
# logWordFormFreq      -4.695e-04  2.226e-04  9.760e+01  -2.109  0.03754 *  
#   LocSpeech            -3.214e-03  1.803e-04  1.526e+03 -17.823  < 2e-16 ***
#   TypeOfLtap           -6.673e-03  1.544e-03  1.544e+03  -4.321 1.65e-05 ***
#   PostPausePause        8.677e-03  1.016e-03  1.567e+03   8.542  < 2e-16 ***
#   PrecSegDur           -6.049e-02  8.993e-03  1.292e+03  -6.727 2.60e-11 ***

anova(lyComplete.lmerBC6a,lyComplete.lmerBC5a)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 16 -9369.4 -9283.2 4700.7  -9401.4                         
# ..1    17 -9368.7 -9277.2 4701.4  -9402.7 1.3537      1     0.2446


# now order


lyComplete.lmerBC7a <- lmer(bc ~ SimpleEnvironment+ Orthography+syllabicity+ 
                              logWordFormFreq+LocSpeech + TypeOfL+
                              PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = lyComplete2a)

summary(lyComplete.lmerBC7a)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)           8.740e-01  3.162e-03  6.236e+02 276.353  < 2e-16 ***
#   SimpleEnvironmentl   -3.771e-03  2.165e-03  7.960e+01  -1.742  0.08534 .  
#   SimpleEnvironmentl#   2.503e-02  2.090e-03  1.509e+02  11.974  < 2e-16 ***
#   SimpleEnvironmentl#l -5.696e-03  1.756e-03  8.850e+01  -3.243  0.00167 ** 
#   Orthographyle        -2.890e-03  2.977e-03  1.417e+02  -0.971  0.33328    
#   Orthographylel        1.129e-02  2.740e-03  1.021e+02   4.122 7.67e-05 ***
#   syllabicityyes        6.427e-03  1.527e-03  1.400e+03   4.208 2.74e-05 ***
#   logWordFormFreq      -4.748e-04  2.232e-04  9.740e+01  -2.127  0.03592 *  
#   LocSpeech            -3.239e-03  1.800e-04  1.532e+03 -17.993  < 2e-16 ***
#   TypeOfLtap           -6.613e-03  1.545e-03  1.544e+03  -4.280 1.98e-05 ***
#   PostPausePause        8.698e-03  1.017e-03  1.567e+03   8.557  < 2e-16 ***
#   PrecSegDur           -6.057e-02  9.003e-03  1.295e+03  -6.728 2.57e-11 ***

anova(lyComplete.lmerBC6a,lyComplete.lmerBC7a)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    15 -9367.8 -9287.0 4698.9  -9397.8                           
# object 16 -9369.4 -9283.2 4700.7  -9401.4 3.5928      1    0.05803 .
# ---

#okay, let's see'




visreg(lyComplete.lmerBC7a, 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", 
       cex.axis=0.5)


# this really does not work out...we need to use the model we used before with all
# the different levels..

#############
# Let's get the  model for the dissertation


table_final_models<-as.data.frame(coef(summary(final_ly_complex_model.lmer)))

xtable(table_final_models,digits = 3)



#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



##############################
# We should also plot all the main effects

#################################

ylim=c(30,130)


# I decided to use a different level order in the plots ( so its parallel to the one in the complex
# model, therefore I need to change the level order and maske another final model with a diff.
# ref level for environment)

# I need the following order



levels(lyComplete2$Environment2)
# [1] "l#l-<ll>"      "syll.l#l-<ll>" "l#l-<lel>"     "#l-<l>"        "l#-<l>"        "syll.l#-<l>"  
# [7] "l#-<le>"       "l-<ll>"   


# we need the following order

#"#l-<l>" ,"l#l-<lel>",  "l#l-<ll>", "syll.l#l-<ll>","l#-<le>" ,   "l#-<l>" , "syll. l#-<l>"   ,      "l-<ll>"



# so, let's do it
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l-<ll>")

lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "syll.l#-<l>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#-<l>" )
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#-<le>")

lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "syll.l#l-<ll>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#l-<ll>")
lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "l#l-<lel>")


lyComplete2$Environment2 <- relevel (lyComplete2$Environment2, ref= "#l-<l>")




final_ly_complete_model.lmer<- lmer(bc ~ Environment2*Accentuation+Environment2*PostPause+  
                                     logWordFormFreq+
                                     LocSpeech + TypeOfL
                                   + PrecedingSegmentDuration+
                                     (1|Item) + (1|Participant), data = lyComplete2)

###############################
# Plot main effect

png("LyModelCompleteInterEnvAcc.png", units="cm", height=12, width=27, res=300, pointsize=15)


# visreg(final_ly_complex_model.lmer, "Environment2",by="Accentuation",
#        ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
#        rug=F, xlab="environment",ylim=ylim,band=F,
#        overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


visreg(final_ly_complete_model.lmer, "Environment2",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


dev.off()

png("LyModelCompleteInterEnvPause.png", units="cm", height=12, width=27, res=300, pointsize=15)


# visreg(final_ly_complex_model.lmer, "Environment2",by="PostPause",
#        ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
#        rug=F, xlab="environment",ylim=ylim,band=F,
#        overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)

visreg(final_ly_complete_model.lmer, "Environment2",by="PostPause",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


dev.off()

################
# so, since we have "new" Environment levels, we need to plot the raw distributions
# and make the pertinent T-tests etc


# first the plot

# we need to rename the levsl (Zeilenumsprung)

levels(lyComplete$Environment2)
#[1] "#l"        "l"         "l#-l"      "l#-le"     "l#l-lely"  "l#l-ll"    "syll. l#l" "syll.l#" 

levels(lyComplete$Environment2) <-c( "#l-\n<l>"  ,      "l-\n<ll>"    ,     "l#-\n<l>"  ,    "l#-\n<le>" ,    "l#l-\n<lel>" , "l#l-\n<ll>" ,   "syll.l#l-\n<ll>",   "syll.l#-\n<l>")



# we need the following order

# "l#l-<ll>", "syll.l#l-<ll>", "l#l-<lel>", "#l-<l>" , "l#-<l>" ,, "syll. l#-<l>"        "l#-<le>" ,  "l-<ll>"


# so, let's do it
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "l-\n<ll>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "l#-\n<le>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "syll.l#-\n<l>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "l#-\n<l>" )
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "#l-\n<l>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "l#l-\n<lel>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "syll.l#l-\n<ll>")
lyComplete$Environment2 <- relevel (lyComplete$Environment2, ref= "l#l-\n<ll>")


png("boxLy.png", units="cm", height=10, width=13, res=300, pointsize=8)
#bwplot (ConsonantDurMS ~ Environment, un, ylab="duration in milliseconds", main="un-", ylim=c(0,320), cex.axis=0.5)


cols = list(col=c("dodgerblue3"),pch=c(1))
bwplot (ConsonantDurMS ~ Environment2, lyComplete, ylab="duration in milliseconds", 
        main="ly-", ylim=c(0,320), cex.axis=0.5,
        par.settings = list(
          plot.symbol=cols,
          box.rectangle = cols,
          #box.dot = cols,
          box.umbrella=cols 
        ))

dev.off()


#### now the figures (distrbution and one-way comparisons)

#1. Environment2

table(lyComplete$Environment2)
# l#l-\n<ll> syll.l#l-\n<ll>     l#l-\n<lel>        #l-\n<l>        l#-\n<l>   syll.l#-\n<l>       l#-\n<le>        l-\n<ll> 
# 313             132             151             609             115              21             103             201 

#2. ConsonantDur

# all

summary (lyComplete$ConsonantDurMS)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  14.83   45.08   55.75   60.23   68.79  177.80 



sd (lyComplete$ConsonantDurMS)
#[1]  24.64634

# only l#l-\n<ll>

summary (lyComplete[lyComplete$Environment2=="l#l-\n<ll>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  15.49   41.74   50.66   53.52   60.70  157.70 


sd(lyComplete[lyComplete$Environment2=="l#l-\n<ll>",]$ConsonantDurMS)
#18.45273



# only syll.l#l-\n<ll> 

summary (lyComplete[lyComplete$Environment2=="syll.l#l-\n<ll>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  16.76   37.10   46.78   51.07   64.08  123.40 


sd(lyComplete[lyComplete$Environment2=="syll.l#l-\n<ll>" ,]$ConsonantDurMS)
#19.88526

# only l#l-\n<lel>

summary (lyComplete[lyComplete$Environment2=="l#l-\n<lel>",]$ConsonantDurMS)
              
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  17.56   51.17   64.39   69.09   80.77  166.40 

sd(lyComplete[lyComplete$Environment2=="l#l-\n<lel>",]$ConsonantDurMS)
#26.90965

# only #l-\n<l> 

summary (lyComplete[lyComplete$Environment2=="#l-\n<l>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   14.83   45.93   55.67   56.20   65.94  123.20 


sd(lyComplete[lyComplete$Environment2=="#l-\n<l>",]$ConsonantDurMS)
#15.26382

# only l#-\n<l>

summary (lyComplete[lyComplete$Environment2=="l#-\n<l>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.02   54.74   75.00   85.92  106.00  177.80 
 


sd(lyComplete[lyComplete$Environment2=="l#-\n<l>",]$ConsonantDurMS)
#40.47495

# only syll-l#-\n<l>

summary (lyComplete[lyComplete$Environment2=="syll.l#-\n<l>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  32.25   69.41   81.97   90.24  110.80  168.00 
 



sd(lyComplete[lyComplete$Environment2=="syll.l#-\n<l>",]$ConsonantDurMS)
#34.08452

# only l#-\n<le>

summary (lyComplete[lyComplete$Environment2=="l#-\n<le>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  33.36   54.21   74.89   84.35  108.40  172.60 


sd(lyComplete[lyComplete$Environment2=="l#-\n<le>",]$ConsonantDurMS)
#35.01737

# only l-\n<ll>

summary (lyComplete[lyComplete$Environment2=="l-\n<ll>",]$ConsonantDurMS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  22.04   41.64   52.01   52.05   59.81   99.36 




sd(lyComplete[lyComplete$Environment2=="l-\n<ll>",]$ConsonantDurMS)
#14.16636


# we should see whether differences are significant


anova.ly<-aov(ConsonantDurMS~Environment2,data=lyComplete)

summary(anova.ly)
# Df Sum Sq Mean Sq F value Pr(>F)    
# Environment2    7 215120   30731   64.21 <2e-16 ***
#   Residuals    1637 783515     479        


TukeyHSD(anova.ly)

# $Environment2
# diff        lwr        upr     p adj
# syll.l#l-\n<ll>-l#l-\n<ll>     -2.4458803  -9.336422   4.444661 0.9615198
# l#l-\n<lel>-l#l-\n<ll>         15.5731415   8.994581  22.151702 0.0000000
# #l-\n<l>-l#l-\n<ll>             2.6803147  -1.937294   7.297924 0.6463397
# l#-\n<l>-l#l-\n<ll>            32.4048607  25.164949  39.644773 0.0000000
# syll.l#-\n<l>-l#l-\n<ll>       36.7246719  21.758045  51.691298 0.0000000
# l#-\n<le>-l#l-\n<ll>           30.8282275  23.286195  38.370260 0.0000000
# l-\n<ll>-l#l-\n<ll>            -1.4709436  -7.472223   4.530336 0.9956195
# l#l-\n<lel>-syll.l#l-\n<ll>    18.0190218  10.107678  25.930366 0.0000000
# #l-\n<l>-syll.l#l-\n<ll>        5.1261950  -1.248303  11.500693 0.2225145
# l#-\n<l>-syll.l#l-\n<ll>       34.8507410  26.381488  43.319994 0.0000000
# syll.l#-\n<l>-syll.l#l-\n<ll>  39.1705523  23.572097  54.769008 0.0000000
# l#-\n<le>-syll.l#l-\n<ll>      33.2741078  24.545181  42.003035 0.0000000
# l-\n<ll>-syll.l#l-\n<ll>        0.9749368  -6.463294   8.413167 0.9999273
# #l-\n<l>-l#l-\n<lel>          -12.8928268 -18.928729  -6.856925 0.0000000
# l#-\n<l>-l#l-\n<lel>           16.8317192   8.614291  25.049148 0.0000000
# syll.l#-\n<l>-l#l-\n<lel>      21.1515305   5.688358  36.614703 0.0009077
# l#-\n<le>-l#l-\n<lel>          15.2550860   6.770273  23.739899 0.0000016
# l-\n<ll>-l#l-\n<lel>          -17.0440850 -24.194271  -9.893899 0.0000000
# l#-\n<l>-#l-\n<l>              29.7245460  22.973917  36.475175 0.0000000
# syll.l#-\n<l>-#l-\n<l>         34.0443573  19.308192  48.780523 0.0000000
# l#-\n<le>-#l-\n<l>             28.1479128  21.074236  35.221590 0.0000000
# l-\n<ll>-#l-\n<l>              -4.1512582  -9.552186   1.249669 0.2763713
#   syll.l#-\n<l>-l#-\n<l>          4.3198113 -11.436095  20.075718 0.9912937
# l#-\n<le>-l#-\n<l>             -1.5766332 -10.583904   7.430638 0.9994967
# l-\n<ll>-l#-\n<l>             -33.8758042 -41.638795 -26.112813 0.0000000
# l#-\n<le>-syll.l#-\n<l>        -5.8964445 -21.793441  10.000552 0.9512812
# l-\n<ll>-syll.l#-\n<l>        -38.1956155 -53.422159 -22.969073 0.0000000
# l-\n<ll>-l#-\n<le>            -32.2991710 -40.344664 -24.253678 0.0000000

Tuk.ly2<-glht(anova.ly,linfct=mcp(Environment2="Tukey"))

summary(Tuk.ly2)

#Linear Hypotheses:
# Estimate Std. Error t value Pr(>|t|)    
# syll.l#l-\n<ll> - l#l-\n<ll> == 0     -2.4459     2.2705  -1.077    0.955    
# l#l-\n<lel> - l#l-\n<ll> == 0         15.5731     2.1677   7.184   <0.001 ***
# #l-\n<l> - l#l-\n<ll> == 0             2.6803     1.5215   1.762    0.617    
# l#-\n<l> - l#l-\n<ll> == 0            32.4049     2.3856  13.583   <0.001 ***
# syll.l#-\n<l> - l#l-\n<ll> == 0       36.7247     4.9316   7.447   <0.001 ***
# l#-\n<le> - l#l-\n<ll> == 0           30.8282     2.4852  12.405   <0.001 ***
# l-\n<ll> - l#l-\n<ll> == 0            -1.4709     1.9775  -0.744    0.995    
# l#l-\n<lel> - syll.l#l-\n<ll> == 0    18.0190     2.6069   6.912   <0.001 ***
# #l-\n<l> - syll.l#l-\n<ll> == 0        5.1262     2.1005   2.441    0.201    
# l#-\n<l> - syll.l#l-\n<ll> == 0       34.8507     2.7907  12.488   <0.001 ***
# syll.l#-\n<l> - syll.l#l-\n<ll> == 0  39.1706     5.1398   7.621   <0.001 ***
# l#-\n<le> - syll.l#l-\n<ll> == 0      33.2741     2.8763  11.569   <0.001 ***
# l-\n<ll> - syll.l#l-\n<ll> == 0        0.9749     2.4510   0.398    1.000    
# #l-\n<l> - l#l-\n<lel> == 0          -12.8928     1.9889  -6.482   <0.001 ***
# l#-\n<l> - l#l-\n<lel> == 0           16.8317     2.7077   6.216   <0.001 ***
# syll.l#-\n<l> - l#l-\n<lel> == 0      21.1515     5.0952   4.151   <0.001 ***
# l#-\n<le> - l#l-\n<lel> == 0          15.2551     2.7958   5.456   <0.001 ***
# l-\n<ll> - l#l-\n<lel> == 0          -17.0441     2.3560  -7.234   <0.001 ***
# l#-\n<l> - #l-\n<l> == 0              29.7245     2.2244  13.363   <0.001 ***
# syll.l#-\n<l> - #l-\n<l> == 0         34.0444     4.8557   7.011   <0.001 ***
# l#-\n<le> - #l-\n<l> == 0             28.1479     2.3308  12.076   <0.001 ***
# l-\n<ll> - #l-\n<l> == 0              -4.1513     1.7797  -2.333    0.251    
#   syll.l#-\n<l> - l#-\n<l> == 0          4.3198     5.1917   0.832    0.990    
# l#-\n<le> - l#-\n<l> == 0             -1.5766     2.9680  -0.531    0.999    
# l-\n<ll> - l#-\n<l> == 0             -33.8758     2.5580 -13.243   <0.001 ***
# l#-\n<le> - syll.l#-\n<l> == 0        -5.8964     5.2382  -1.126    0.944    
# l-\n<ll> - syll.l#-\n<l> == 0        -38.1956     5.0173  -7.613   <0.001 ***
# l-\n<ll> - l#-\n<le> == 0            -32.2992     2.6511 -12.183   <0.001 ***

library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 

lyComplete.lm<-lm(bc~Environment2*Accentuation*PostPause+
                   LocSpeech+GlobalSpeechRate+PrecedingSegmentDuration+
                    BaseFinalStress+OrderRescale+logWordFormFreq+PrePause+
                   TypeOfL, data = lyComplete2)

summary(lyComplete.lm)

# Call:
#   lm(formula = bc ~ Environment2 * Accentuation * PostPause + LocSpeech + 
#        GlobalSpeechRate + PrecedingSegmentDuration + BaseFinalStress + 
#        OrderRescale + logWordFormFreq + PrePause + TypeOfL, data = lyComplete2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.042295 -0.009056 -0.000169  0.009279  0.041495 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                      8.726e-01  5.419e-03 161.026  < 2e-16 ***
#   Environment2syll.l#l-<ll>                                       -4.479e-03  6.699e-03  -0.669 0.503807    
# Environment2l#l-<lel>                                            2.649e-03  1.452e-02   0.182 0.855247    
# Environment2#l-<l>                                               1.321e-03  5.481e-03   0.241 0.809598    
# Environment2l#-<l>                                               1.570e-02  5.501e-03   2.854 0.004377 ** 
# Environment2syll.l#-<l>                                          3.229e-02  1.449e-02   2.229 0.025941 *  
# Environment2l#-<le>                                              1.920e-02  5.680e-03   3.381 0.000741 ***
# Environment2l-<ll>                                              -6.114e-03  7.747e-03  -0.789 0.430101    
# Accentuationunaccented                                           3.555e-03  5.496e-03   0.647 0.517838    
# PostPausepause                                                   2.640e-03  4.719e-03   0.559 0.575945    
# LocSpeech                                                       -3.022e-03  1.774e-04 -17.039  < 2e-16 ***
#   GlobalSpeechRate                                                -1.278e-03  5.729e-04  -2.231 0.025808 *  
#   PrecedingSegmentDuration                                        -4.097e-02  9.012e-03  -4.546 5.89e-06 ***
#   BaseFinalStressunstressed                                       -2.949e-04  1.184e-03  -0.249 0.803411    
# OrderRescale                                                    -4.734e-05  3.637e-05  -1.302 0.193273    
# logWordFormFreq                                                 -5.196e-04  1.292e-04  -4.022 6.04e-05 ***
#   PrePausePause                                                    1.692e-03  7.227e-04   2.342 0.019317 *  
#   TypeOfLtap                                                      -7.304e-03  1.584e-03  -4.612 4.31e-06 ***
#   Environment2syll.l#l-<ll>:Accentuationunaccented                 9.692e-03  8.143e-03   1.190 0.234138    
# Environment2l#l-<lel>:Accentuationunaccented                    -4.809e-03  1.528e-02  -0.315 0.753041    
# Environment2#l-<l>:Accentuationunaccented                       -4.265e-04  6.524e-03  -0.065 0.947887    
# Environment2l#-<l>:Accentuationunaccented                       -5.032e-04  6.562e-03  -0.077 0.938887    
# Environment2syll.l#-<l>:Accentuationunaccented                  -3.064e-03  1.519e-02  -0.202 0.840145    
# Environment2l#-<le>:Accentuationunaccented                      -6.558e-03  6.632e-03  -0.989 0.322879    
# Environment2l-<ll>:Accentuationunaccented                        8.221e-03  8.848e-03   0.929 0.352986    
# Environment2syll.l#l-<ll>:PostPausepause                         3.913e-03  7.084e-03   0.552 0.580732    
# Environment2l#l-<lel>:PostPausepause                             9.634e-03  1.462e-02   0.659 0.510059    
# Environment2#l-<l>:PostPausepause                                3.032e-03  5.645e-03   0.537 0.591244    
# Environment2l#-<l>:PostPausepause                                2.290e-02  6.073e-03   3.770 0.000169 ***
# Environment2syll.l#-<l>:PostPausepause                           1.360e-02  1.658e-02   0.820 0.412253    
# Environment2l#-<le>:PostPausepause                               1.446e-02  6.244e-03   2.315 0.020732 *  
# Environment2l-<ll>:PostPausepause                                6.367e-03  7.873e-03   0.809 0.418786    
# Accentuationunaccented:PostPausepause                           -4.442e-04  5.731e-03  -0.078 0.938230    
# Environment2syll.l#l-<ll>:Accentuationunaccented:PostPausepause -9.682e-03  8.752e-03  -1.106 0.268794    
# Environment2l#l-<lel>:Accentuationunaccented:PostPausepause     -3.333e-03  1.556e-02  -0.214 0.830358    
# Environment2#l-<l>:Accentuationunaccented:PostPausepause        -1.390e-04  6.841e-03  -0.020 0.983786    
# Environment2l#-<l>:Accentuationunaccented:PostPausepause         2.209e-03  8.383e-03   0.264 0.792190    
# Environment2syll.l#-<l>:Accentuationunaccented:PostPausepause           NA         NA      NA       NA    
# Environment2l#-<le>:Accentuationunaccented:PostPausepause       -3.472e-03  8.781e-03  -0.395 0.692573    
# Environment2l-<ll>:Accentuationunaccented:PostPausepause        -1.076e-02  9.240e-03  -1.164 0.244480    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01372 on 1576 degrees of freedom
# Multiple R-squared:  0.4389,	Adjusted R-squared:  0.4253 
# F-statistic: 32.44 on 38 and 1576 DF,  p-value: < 2.2e-16


model_ranking <- dredge(lyComplete.lm)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Relative variable importance: 
#   Environment2 LocSpeech PostPause Environment2:PostPause
# Importance:              1            1         1         1                 
# N containing models:  3584         2432      3584      1536                 
# PrecedingSegmentDuration TypeOfL logWordFormFreq Accentuation PrePause
# Importance:              1                        1       1            0.91         0.86   
# N containing models:  2432                     2432    2432            3584         2432   
# GlobalSpeechRate Accentuation:Environment2 OrderRescale Accentuation:PostPause
# Importance:           0.81             0.56                      0.48         0.39                 
# N containing models:  2432             1536                      2432         1536                 
# BaseFinalStress Accentuation:Environment2:PostPause
# Importance:           0.27           <0.01                              
# N containing models:  2432             256  