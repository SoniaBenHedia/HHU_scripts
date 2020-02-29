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


###########################################################################
# I will start with the complex dataset, and thus will need the complex dataset-
# In the following I will first take a look at the pertinent varoables
# and then fit a model
############################################################################



lyComplex <- read.csv("lyComplex.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(lyComplex)
# 'data.frame':	1205 obs. of  87 variables:
#   $ X.1                        : int  18 19 20 21 22 23 24 25 26 27 ...
# $ X                          : int  4660 4661 4662 4663 4664 4665 4666 4667 4668 4669 ...
# $ Item                       : Factor w/ 61 levels "aerobically",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 22 levels "participant_10A_Experiment_2",..: 1 11 17 12 16 19 3 9 18 4 ...
# $ ID                         : int  7 2661 3774 2808 3630 4560 839 2118 4066 1082 ...
# $ Filename                   : Factor w/ 1205 levels "participant_1_A_101.TextGrid",..: 114 596 887 629 849 1040 223 446 957 277 ...
# $ DeletionMorph              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 2 2 1 1 1 1 2 1 1 3 ...
# $ Annotator                  : Factor w/ 4 levels "Mandy","Simon",..: 1 1 1 3 2 3 4 4 1 1 ...
# $ Order                      : int  108 319 115 236 212 41 100 142 238 154 ...
# $ WordDur                    : num  0.747 0.617 0.736 0.876 0.921 ...
# $ SyllNum                    : int  4 4 4 4 4 4 4 4 4 5 ...
# $ SegNum                     : int  8 8 8 8 8 8 8 9 8 9 ...
# $ ConsonantDur               : num  0.0604 0.0361 0.0448 0.0481 0.0252 ...
# $ PrecSeg                    : Factor w/ 52 levels "@","@O","@u",..: 24 24 24 24 24 24 24 24 24 24 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 1 1 1 1 1 1 1 1 1 1 ...
# $ PrecSegDur                 : num  0.1194 0.0934 0.1317 0.1542 0.1479 ...
# $ FollSeg                    : Factor w/ 6 levels "@","aI","e","eI",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ FollSegVC                  : Factor w/ 1 level "V": 1 1 1 1 1 1 1 1 1 1 ...
# $ FollSegDur                 : num  0.1107 0.1215 0.0891 0.1853 0.2868 ...
# $ PrePauseDur                : num  0.046 0 0 0 0.297 ...
# $ PostPauseDur               : num  0 0 0 0.164 0.842 ...
# $ SentenceDur                : num  6.61 4.06 7.92 4.42 7.27 ...
# $ GlottalStop                : Factor w/ 2 levels "GlottalStop",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ GlottalStopDur             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ LocSpeech                  : num  10.71 12.98 10.87 9.14 8.68 ...
# $ AffixDur                   : num  0.171 0.158 0.134 0.233 0.312 ...
# $ BaseDuration               : num  0.576 0.459 0.602 0.642 0.609 ...
# $ FirstSyllDur               : num  0.1098 0.0688 0.1055 0.1071 0.0706 ...
# $ WordDurWithoutGlottalStop  : num  0.747 0.617 0.736 0.876 0.921 ...
# $ AffixDurWithoutGlottalStop : num  0.171 0.158 0.134 0.233 0.312 ...
# $ Affix                      : Factor w/ 1 level "ly": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  32 32 32 32 32 32 32 32 32 32 ...
# $ WordFormFrequencyAllCOCA   : int  86 86 86 86 86 86 86 86 86 86 ...
# $ WordFormFrequencySpokenCOCA: int  NA NA NA NA NA NA NA NA NA NA ...
# $ Base                       : Factor w/ 61 levels "aerobical","agricultural",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  32 32 32 32 32 32 32 32 32 32 ...
# $ BaseLemmaFrequencyBNC      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ SyllPhon                   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ AffixStress                : Factor w/ 1 level "unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseInitialStress          : logi  NA NA NA NA NA NA ...
# $ SemanticTransparency       : Factor w/ 1 level "transparent": 1 1 1 1 1 1 1 1 1 1 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  2 3 3 3 1 3 1 2 4 1 ...
# $ TimeRating                 : num  691 764 681 872 629 ...
# $ TotalTime                  : num  786 1073 793 997 785 ...
# $ Age                        : int  25 65 33 19 20 21 31 20 23 20 ...
# $ Sex                        : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 2 1 2 2 ...
# $ L1                         : Factor w/ 2 levels "British English",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Bilingual                  : Factor w/ 1 level "no": 1 1 1 1 1 1 1 1 1 1 ...
# $ Grow_Up_Region             : Factor w/ 21 levels "Aberdeen. Scotland",..: 19 1 8 21 2 11 5 7 16 6 ...
# $ Languages                  : Factor w/ 14 levels "Basic French",..: 12 5 12 12 8 3 9 10 12 7 ...
# $ Latin                      : Factor w/ 6 levels "I know a few words and phrases. I have never studied it",..: 2 6 2 2 2 5 4 2 2 2 ...
# $ Profession_Studies         : Factor w/ 22 levels "2nd Year Meida Studies",..: 7 4 12 1 3 18 14 11 15 8 ...
# $ University                 : Factor w/ 9 levels "Aberdeen University",..: 4 1 2 5 4 9 8 2 4 4 ...
# $ Knowledge_English_Ling     : Factor w/ 10 levels "2 years","no",..: 5 2 2 2 2 2 10 1 2 4 ...
# $ Phonetics                  : Factor w/ 6 levels "I went to a few lectures on phonetics in my first year of university.",..: 4 2 2 2 2 1 2 4 2 6 ...
# $ Phonology                  : Factor w/ 5 levels "no","The above lecture also covered phonology.",..: 4 1 1 1 1 2 1 4 1 5 ...
# $ Morphology                 : Factor w/ 4 levels "no","Year 1 ARU",..: 3 1 1 1 1 1 1 3 1 4 ...
# $ Semantics                  : Factor w/ 4 levels "no","Year 1 ARU",..: 3 1 1 1 1 1 1 3 1 4 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 2 2 1 1 1 1 2 1 2 ...
# $ Experiment                 : Factor w/ 1 level "Experiment_2": 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  3.47 3.47 3.47 3.47 3.47 ...
# $ logBaseLemmaFreq           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ logWordLemmaFreq           : num  3.47 3.47 3.47 3.47 3.47 ...
# $ RelFreq                    : num  32 32 32 32 32 32 32 32 32 32 ...
# $ logRelFreq                 : num  3.47 3.47 3.47 3.47 3.47 ...
# $ Root                       : Factor w/ 61 levels "aerobical","agricultural",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFinalStress            : Factor w/ 2 levels "primary","unstressed": 2 2 2 2 2 2 2 2 2 2 ...
# $ SuffixAdjSuffix            : Factor w/ 3 levels "al","ful","none": 1 1 1 1 1 1 1 1 1 1 ...
# $ LastSyllDur                : num  0.171 0.251 0.134 0.388 0.46 ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
# $ Consonant                  : Factor w/ 2 levels "l","L": 2 1 1 1 1 1 1 1 1 1 ...
# $ Orthography                : Factor w/ 3 levels "l","lely","ll": 3 3 3 3 3 3 3 3 3 3 ...
# $ median                     : int  2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfBase                 : Factor w/ 1 level "word": 1 1 1 1 1 1 1 1 1 1 ...
# $ Environment2               : Factor w/ 2 levels "V#l","Vl#l": 2 2 2 2 2 2 2 2 2 2 ...
# $ syllabicity                : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
# $ Environment                : Factor w/ 3 levels "#l","l#l","syll. l#l": 3 3 3 3 3 3 3 3 3 3 ...
# $ Environment4               : Factor w/ 6 levels "syllabic Vl#l-al",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ TypeOfL                    : Factor w/ 2 levels "approximant",..: 2 1 1 1 1 1 1 1 1 1 ...
# $ ConsonantDurMS             : num  60.4 36.1 44.8 48.1 25.2 ...
# $ PrePause                   : Factor w/ 2 levels "No Pause","Pause": 2 1 1 1 2 2 2 1 1 2 ...
# $ PostPause                  : Factor w/ 2 levels "No Pause","Pause": 1 1 1 2 2 2 2 2 2 2 ...
# $ GlobalSpeechRate           : num  1.664 2.707 1.389 1.357 0.826 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 3 3 3 3 3 3 3 3 3 3 ...

lyComplex$X.1<-NULL
  
lyComplex$X<-NULL

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

table(ly3$Environment,ly3$Orthography)
#                 l  le lely  ll
# Vl#l            0   0  151 313
# syllabic Vl#l   0   0    0 132
# V#l           609   0    0   0
# Vl#           444 155    0   0
# Vl              0   0    0 201

table(ly3$Environment,ly3$TypeOfL)
#                   vocalized approximant tap
# Vl#l                  0         444  20
# syllabic Vl#l         0         125   7
# V#l                   0         563  46
# Vl#                 356         240   3
# Vl                    0         196   5


table(ly3$Orthography,ly3$TypeOfL)
#         vocalized approximant tap
# l          307         699  47
# le          49         104   2
# lely         0         146   5
# ll           0         619  27


# - Loc Speech  and/or Global Speech
# - PrePauseDur or PrePause
# - PostPauseDur or PostPause

# - PrecSegDur (in model in which affixed words are compared)
# - logRelFreq (in model in which affixed words are compared)
# - Rating


# Let's see whether it makes sense to include the decomposability measures (only
# makes sense if we have variability)

# 1. Semantic Transparency

table(lyComplex$SemanticTransparency)
# only tran

# 2. Type of Base

table(lyComplex$TypeOfBase)
# word 
# 1205


# 3. Rating

table(lyComplex$Rating)
# 1   2   3   4 
# 747 213 182  63


# so RelFreq and Rating can be included
######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = lyComplex)
cor(lyComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.1145605


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = lyComplex)
cor(lyComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.1971687

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = lyComplex)
cor(lyComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.313036

# so around 31 percent of the variability can be explained by this! That's a lot



##              Do an initial model:
lyComplex$OrderRescale<-lyComplex$Order*0.1

# there is something wrong with Orthography, let's see
table(lyComplex$Environment,lyComplex$Orthography)
#             l lely  ll
# #l        609    0   0
# l#l         0  151 313
# syll. l#l   0    0 132

# We should just paste them

lyComplex$Environment2<-as.factor(paste(lyComplex$Environment,lyComplex$Orthography, sep="-"))

levels(lyComplex$Environment2)
#[1] "#l-l"         "l#l-lely"     "l#l-ll"       "syll. l#l-ll"

levels(lyComplex$Environment2)<-c("#l"        , "l#l-lely"   ,  "l#l-ll"       ,"syll. l#l")

levels(lyComplex$Environment2)
#[1] "#l"        "l#l-lely"  "l#l-ll"    "syll. l#l"

lyComplex.lmer1 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                          OrderRescale +
                          logWordFormFreq+
                          BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                       PrePause+ PostPause + PrecSegDur+
                           logRelFreq+ Rating+ 
                        (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmer1)    
# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      9.751e-02  6.101e-03  2.615e+02  15.981  < 2e-16 ***
#   Environment2l#l-lely             5.747e-03  3.040e-03  5.580e+01   1.891 0.063863 .  
#   Environment2l#l-ll              -2.839e-03  2.057e-03  5.590e+01  -1.380 0.173136    
#   Environment2syll. l#l            4.789e-03  2.579e-03  1.227e+02   1.857 0.065765 .  
#   AccentuationConditionunaccented  1.489e-03  1.177e-03  1.026e+03   1.265 0.206249    
#   OrderRescale                    -7.808e-05  4.679e-05  1.156e+03  -1.669 0.095456 .  
#   logWordFormFreq                  8.596e-05  3.623e-04  5.390e+01   0.237 0.813336    
#   BaseFinalStressunstressed       -3.431e-05  2.592e-03  6.010e+01  -0.013 0.989485    
#   LocSpeech                       -3.613e-03  2.761e-04  1.128e+03 -13.090  < 2e-16 ***
#   GlobalSpeechRate                -1.373e-03  9.835e-04  6.959e+02  -1.397 0.162992    
#   TypeOfLtap                      -5.830e-03  1.942e-03  1.156e+03  -3.002 0.002743 ** 
#   PrePausePause                    1.212e-03  1.068e-03  1.066e+03   1.135 0.256805    
#   PostPausePause                   2.611e-03  1.524e-03  1.145e+03   1.713 0.086965 .  
#   PrecSegDur                      -5.510e-02  1.237e-02  1.115e+03  -4.455 9.22e-06 ***
#   logRelFreq                      -1.806e-03  5.188e-04  5.700e+01  -3.481 0.000966 ***
#   Rating                          -1.073e-03  6.101e-04  1.049e+03  -1.759 0.078917 .  

cor(lyComplex$ConsonantDur, fitted(lyComplex.lmer1))^2
#[1]  0.4363695


#######################################################################################
# Dealing with collinearity                                                          #
######################################################################################

# Before slimming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own


# 1.logWordFormFreq & logRelFreq

# Model woth both 
lyComplex.lmerFrequencies <- lmer(ConsonantDur ~ logWordFormFreq+ logRelFreq+ (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmerFrequencies)    


# Fixed effects:
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)      0.0484415  0.0025950 73.0000000  18.667  < 2e-16 ***
#   logWordFormFreq  0.0002565  0.0003355 56.1000000   0.764    0.448    
#   logRelFreq      -0.0026814  0.0004329 57.1500000  -6.194 6.85e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerFrequencies))^2
#[1] 0.3058333


# only Word Form Freq

lyComplex.lmerWordFrequency <- lmer(ConsonantDur ~ logWordFormFreq + (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmerWordFrequency)    


# Random effects:
#   Groups      Name        Variance  Std.Dev.
# Item        (Intercept) 5.708e-05 0.007555
# Participant (Intercept) 4.033e-05 0.006350
# Residual                2.684e-04 0.016384
# Number of obs: 1205, groups:  Item, 61; Participant, 22

cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerWordFrequency))^2
#[1] 0.3122465


# only RelFreq
lyComplex.lmerRelFrequency <- lmer(ConsonantDur ~ logRelFreq+  (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmerRelFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  0.049816   0.001868 48.310000  26.668  < 2e-16 ***
#   logRelFreq  -0.002503   0.000364 57.610000  -6.878 4.84e-09 ***

cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerRelFrequency))^2
#[1] 0.3057559


###################################################
# Summary coll. frqs
#
# So, there is a supression effect. Since WordFreq is the better predictor, I will use that one
# In the end I will try which one is better (but using WorFormFreq is also pretty convenient
# since I do not have to worry about coll. wrt to Rating and WordForm)


# 2.  Loc Speech  and/or Global Speech


cor.test(lyComplex$LocSpeech,lyComplex$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  lyComplex$LocSpeech and lyComplex$GlobalSpeechRate
# t = 7.7703, df = 1203, p-value = 1.665e-14
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1641652 0.2717280
# sample estimates:
# cor 
# 0.2186105 


# not too bad

lyComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = lyComplex)

summary(lyComplex.lmerSpeechRates)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       9.443e-02  2.966e-03  3.836e+02  31.832   <2e-16 ***
#   LocSpeech        -3.309e-03  2.268e-04  1.076e+03 -14.588   <2e-16 ***
#   GlobalSpeechRate -9.754e-04  8.110e-04  9.910e+02  -1.203    0.229   

cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerSpeechRates))^2
#[1]  0.4125961



lyComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = lyComplex)

summary(lyComplex.lmerLocSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  9.341e-02  2.847e-03  3.654e+02   32.81   <2e-16 ***
#   LocSpeech   -3.364e-03  2.225e-04  1.097e+03  -15.12   <2e-16 ***

cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerLocSpeech))^2
#[1] 0.4125093


lyComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = lyComplex)

summary(lyComplex.lmerGlobalSpeech)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       6.201e-02  2.216e-03  1.079e+02  27.980  < 2e-16 ***
#   GlobalSpeechRate -3.291e-03  8.634e-04  9.999e+02  -3.812 0.000146 ***


cor(lyComplex$ConsonantDur, fitted(lyComplex.lmerGlobalSpeech))^2
#[1] 0.3188371


#####################################
# Summary Coll. Speech Rates:
# - When both Speech Rates rae in the model, bothe have a sign effect
# - The effect size of LocSpeech increases when it is the only variable in the model
# - The effect size of GlobalSpeech decreases when it is the only variable in the model
# - The effect direction never changes (no supression)
# - so we can leave both variables in
#################################################


# Let's refit our model incorportaing the "right variables"

lyComplex.lmer3 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                          OrderRescale +
                          logWordFormFreq+
                          BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                          PrePause+ PostPause + PrecSegDur+
                           Rating+ 
                          (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmer3)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      1.080e-01  5.556e-03  3.295e+02  19.445  < 2e-16 ***
#   Environment2l#l-lely             7.689e-03  3.261e-03  5.640e+01   2.358  0.02186 *  
#   Environment2l#l-ll              -5.202e-03  2.127e-03  6.080e+01  -2.446  0.01737 *  
#   Environment2syll. l#l            1.299e-03  2.480e-03  1.023e+02   0.524  0.60165    
#   AccentuationConditionunaccented  1.630e-03  1.177e-03  1.023e+03   1.385  0.16642    
#   OrderRescale                    -7.335e-05  4.685e-05  1.152e+03  -1.566  0.11770    
#   logWordFormFreq                 -5.948e-04  3.331e-04  5.120e+01  -1.786  0.08003 .  
#   BaseFinalStressunstressed       -9.215e-04  2.808e-03  5.990e+01  -0.328  0.74396    
#   LocSpeech                       -3.737e-03  2.769e-04  1.140e+03 -13.498  < 2e-16 ***
#   GlobalSpeechRate                -1.359e-03  9.844e-04  6.913e+02  -1.381  0.16777    
#   TypeOfLtap                      -5.797e-03  1.944e-03  1.152e+03  -2.982  0.00293 ** 
#   PrePausePause                    1.026e-03  1.078e-03  1.119e+03   0.952  0.34139    
#   PostPausePause                   2.460e-03  1.526e-03  1.141e+03   1.612  0.10718    
#   PrecSegDur                      -5.816e-02  1.246e-02  1.150e+03  -4.667 3.41e-06 ***
#   Rating                          -1.164e-03  6.124e-04  1.049e+03  -1.901  0.05760 .  

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (lyComplex.lmer3))
qqline (residuals (lyComplex.lmer3))

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


lyComplex.lm<-lm(ConsonantDur ~ Environment2+ AccentuationCondition+ 
                     OrderRescale +
                     logWordFormFreq+
                     BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                     PrePause+ PostPause + PrecSegDur+
                     Rating, data = lyComplex)

summary(lyComplex.lm)


# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      9.866e-02  4.351e-03  22.675  < 2e-16 ***
#   Environment2l#l-lely             6.054e-03  1.815e-03   3.336 0.000877 ***
#   Environment2l#l-ll              -3.040e-03  1.220e-03  -2.492 0.012822 *  
#   Environment2syll. l#l           -4.240e-03  1.705e-03  -2.487 0.013006 *  
#   AccentuationConditionunaccented  1.922e-03  1.149e-03   1.672 0.094760 .  
#   OrderRescale                    -8.638e-05  5.063e-05  -1.706 0.088238 .  
#   logWordFormFreq                 -6.776e-04  1.759e-04  -3.851 0.000124 ***
#   BaseFinalStressunstressed        1.460e-03  1.622e-03   0.900 0.368165    
#   LocSpeech                       -3.332e-03  2.575e-04 -12.939  < 2e-16 ***
#   GlobalSpeechRate                -1.472e-03  8.134e-04  -1.810 0.070586 .  
#   TypeOfLtap                      -6.042e-03  2.031e-03  -2.975 0.002985 ** 
#   PrePausePause                    2.096e-03  9.969e-04   2.103 0.035692 *  
#   PostPausePause                   3.446e-03  1.523e-03   2.263 0.023826 *  
#   PrecSegDur                      -2.324e-02  1.206e-02  -1.927 0.054209 .  
#   Rating                          -1.556e-03  5.421e-04  -2.870 0.004182 ** 

bc<-boxcox(lyComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.2626263

lyComplex$bc <- lyComplex$ConsonantDur^lambda

lyComplex.lmerBC <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                           OrderRescale +
                           logWordFormFreq+
                           BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                           PrePause+ PostPause + PrecSegDur+
                           Rating+ 
                           (1|Item) + (1|Participant), data = lyComplex)

summary(lyComplex.lmerBC)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      5.801e-01  1.181e-02  3.247e+02  49.125  < 2e-16 ***
#   Environment2l#l-lely             1.160e-02  6.963e-03  5.600e+01   1.666 0.101258    
# Environment2l#l-ll              -1.304e-02  4.541e-03  6.020e+01  -2.872 0.005630 ** 
# Environment2syll. l#l            1.887e-04  5.288e-03  1.011e+02   0.036 0.971604    
# AccentuationConditionunaccented  4.305e-03  2.497e-03  1.019e+03   1.724 0.084982 .  
# OrderRescale                    -1.324e-04  9.940e-05  1.152e+03  -1.332 0.183142    
# logWordFormFreq                 -1.259e-03  7.114e-04  5.080e+01  -1.769 0.082861 .  
# BaseFinalStressunstressed       -2.540e-03  5.996e-03  5.940e+01  -0.424 0.673408    
# LocSpeech                       -8.270e-03  5.876e-04  1.140e+03 -14.076  < 2e-16 ***
#   GlobalSpeechRate                -3.145e-03  2.087e-03  6.828e+02  -1.507 0.132269    
# TypeOfLtap                      -1.468e-02  4.125e-03  1.152e+03  -3.560 0.000386 ***
#   PrePausePause                    2.032e-03  2.289e-03  1.121e+03   0.888 0.374853    
# PostPausePause                   6.472e-03  3.236e-03  1.140e+03   2.000 0.045741 *  
#   PrecSegDur                      -1.366e-01  2.645e-02  1.152e+03  -5.163 2.86e-07 ***
#   Rating                          -2.475e-03  1.299e-03  1.044e+03  -1.905 0.057004 .  

#let's check the assumptions

qqnorm (residuals (lyComplex.lmerBC))
qqline (residuals (lyComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(lyComplex.lmerBC, lyComplex, trim = 2.5)
# n.removed = 27 
# percent.removed = 2.240664  

lyComplex2<-outliers$data

dim(lyComplex2)
#[1]  1178   88


dim(lyComplex)
#[1]1205   87


# okay it seemes to have worked

lyComplex.lmerBC2 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                            OrderRescale +
                            logWordFormFreq+
                            BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                            PrePause+ PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC2)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.746e-01  1.108e-02  3.177e+02  51.863  < 2e-16 ***
#   Environment2l#l-lely             1.280e-02  6.650e-03  5.660e+01   1.924 0.059340 .  
#   Environment2l#l-ll              -1.506e-02  4.315e-03  5.990e+01  -3.490 0.000913 ***
#   Environment2syll. l#l            1.018e-04  4.994e-03  9.960e+01   0.020 0.983781    
#   AccentuationConditionunaccented  2.980e-03  2.278e-03  1.039e+03   1.308 0.191065    
#   OrderRescale                    -1.282e-04  9.050e-05  1.121e+03  -1.417 0.156783    
#   logWordFormFreq                 -9.604e-04  6.786e-04  5.110e+01  -1.415 0.163049    
#   BaseFinalStressunstressed       -2.800e-03  5.710e-03  5.950e+01  -0.490 0.625671    
#   LocSpeech                       -7.609e-03  5.401e-04  1.136e+03 -14.086  < 2e-16 ***
#   GlobalSpeechRate                -3.041e-03  1.916e-03  7.682e+02  -1.587 0.112873    
#   TypeOfLtap                      -1.495e-02  3.717e-03  1.123e+03  -4.021 6.17e-05 ***
#   PrePausePause                    5.667e-04  2.096e-03  1.118e+03   0.270 0.786938    
#   PostPausePause                   7.955e-03  2.936e-03  1.122e+03   2.710 0.006836 ** 
#   PrecSegDur                      -1.491e-01  2.431e-02  1.140e+03  -6.134 1.18e-09 ***
#   Rating                          -2.916e-03  1.185e-03  1.071e+03  -2.460 0.014062 * 

qqnorm (residuals (lyComplex.lmerBC2))
qqline (residuals (lyComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> lyComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(lyComplex.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.746e-01  1.108e-02  3.177e+02  51.863  < 2e-16 ***
#   Environment2l#l-lely             1.280e-02  6.650e-03  5.660e+01   1.924 0.059340 .  
#   Environment2l#l-ll              -1.506e-02  4.315e-03  5.990e+01  -3.490 0.000913 ***
#   Environment2syll. l#l            1.018e-04  4.994e-03  9.960e+01   0.020 0.983781    
#   AccentuationConditionunaccented  2.980e-03  2.278e-03  1.039e+03   1.308 0.191065    
#   OrderRescale                    -1.282e-04  9.050e-05  1.121e+03  -1.417 0.156783    
#   logWordFormFreq                 -9.604e-04  6.786e-04  5.110e+01  -1.415 0.163049    
#   BaseFinalStressunstressed       -2.800e-03  5.710e-03  5.950e+01  -0.490 0.625671    
#   LocSpeech                       -7.609e-03  5.401e-04  1.136e+03 -14.086  < 2e-16 ***
#   GlobalSpeechRate                -3.041e-03  1.916e-03  7.682e+02  -1.587 0.112873    
#   TypeOfLtap                      -1.495e-02  3.717e-03  1.123e+03  -4.021 6.17e-05 ***
#   PrePausePause                    5.667e-04  2.096e-03  1.118e+03   0.270 0.786938    
#   PostPausePause                   7.955e-03  2.936e-03  1.122e+03   2.710 0.006836 ** 
#   PrecSegDur                      -1.491e-01  2.431e-02  1.140e+03  -6.134 1.18e-09 ***
#   Rating                          -2.916e-03  1.185e-03  1.071e+03  -2.460 0.014062 * 


# let's throw out PrePause

lyComplex.lmerBC3 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                            OrderRescale +
                            logWordFormFreq+
                            BaseFinalStress + LocSpeech + GlobalSpeechRate +TypeOfL+
                             PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC3)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.748e-01  1.106e-02  3.201e+02  51.948  < 2e-16 ***
#   Environment2l#l-lely             1.272e-02  6.652e-03  5.680e+01   1.912 0.060918 .  
#   Environment2l#l-ll              -1.504e-02  4.319e-03  6.030e+01  -3.482 0.000933 ***
#   Environment2syll. l#l            1.471e-04  4.998e-03  1.001e+02   0.029 0.976580    
#   AccentuationConditionunaccented  2.886e-03  2.250e-03  1.050e+03   1.283 0.199950    
#   OrderRescale                    -1.291e-04  9.039e-05  1.122e+03  -1.428 0.153456    
#   logWordFormFreq                 -9.566e-04  6.795e-04  5.150e+01  -1.408 0.165155    
#   BaseFinalStressunstressed       -2.849e-03  5.715e-03  5.980e+01  -0.499 0.619952    
#   LocSpeech                       -7.599e-03  5.384e-04  1.134e+03 -14.113  < 2e-16 ***
#   GlobalSpeechRate                -3.027e-03  1.915e-03  7.742e+02  -1.581 0.114295    
#   TypeOfLtap                      -1.497e-02  3.714e-03  1.124e+03  -4.031 5.92e-05 ***
#   PostPausePause                   7.996e-03  2.930e-03  1.125e+03   2.729 0.006453 ** 
#   PrecSegDur                      -1.491e-01  2.429e-02  1.141e+03  -6.135 1.17e-09 ***
#   Rating                          -2.901e-03  1.184e-03  1.072e+03  -2.451 0.014420 *

anova(lyComplex.lmerBC2,lyComplex.lmerBC3)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    17 -4896.9 -4810.7 2465.5  -4930.9                         
# object 18 -4895.0 -4803.7 2465.5  -4931.0 0.0965      1     0.7561

# model did not become worse


# let's throw out Stress

lyComplex.lmerBC4 <- lmer(bc ~ Environment2+ AccentuationCondition+ 
                            OrderRescale +
                            logWordFormFreq+
                            LocSpeech + GlobalSpeechRate +TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC4)

# Fixed effects:
#                                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                      5.717e-01  9.322e-03  5.670e+02  61.329  < 2e-16 ***
#   Environment2l#l-lely             1.381e-02  6.214e-03  6.500e+01   2.223 0.029694 *  
#   Environment2l#l-ll              -1.523e-02  4.265e-03  6.220e+01  -3.570 0.000695 ***
#   Environment2syll. l#l           -3.689e-04  4.884e-03  9.900e+01  -0.076 0.939930    
#   AccentuationConditionunaccented  2.911e-03  2.250e-03  1.050e+03   1.294 0.196010    
#   OrderRescale                    -1.294e-04  9.039e-05  1.122e+03  -1.431 0.152600    
#   logWordFormFreq                 -8.628e-04  6.465e-04  5.360e+01  -1.334 0.187693    
#   LocSpeech                       -7.599e-03  5.380e-04  1.134e+03 -14.125  < 2e-16 ***
#   GlobalSpeechRate                -3.003e-03  1.913e-03  7.729e+02  -1.570 0.116917    
#   TypeOfLtap                      -1.494e-02  3.713e-03  1.125e+03  -4.024 6.11e-05 ***
#   PostPausePause                   8.019e-03  2.930e-03  1.125e+03   2.737 0.006298 ** 
#   PrecSegDur                      -1.456e-01  2.349e-02  9.893e+02  -6.201 8.25e-10 ***
#   Rating                          -2.888e-03  1.183e-03  1.072e+03  -2.441 0.014797 *  

anova(lyComplex.lmerBC3,lyComplex.lmerBC4)

# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1    16 -4898.7 -4817.5 2465.3  -4930.7                        
# object 17 -4896.9 -4810.7 2465.5  -4930.9 0.251      1     0.6164

# nothing has changed


# let's throw ou accent

lyComplex.lmerBC5 <- lmer(bc ~ Environment2+  
                            OrderRescale +
                            logWordFormFreq+
                            LocSpeech + GlobalSpeechRate +TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC5)
# Fixed effects:
#                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)            5.685e-01  8.986e-03  5.863e+02  63.258  < 2e-16 ***
#   Environment2l#l-lely   1.442e-02  6.197e-03  6.440e+01   2.326 0.023153 *  
#   Environment2l#l-ll    -1.528e-02  4.265e-03  6.210e+01  -3.583 0.000668 ***
#   Environment2syll. l#l -9.982e-05  4.879e-03  9.870e+01  -0.020 0.983720    
#   OrderRescale          -1.454e-04  8.956e-05  1.116e+03  -1.624 0.104678    
#   logWordFormFreq       -8.712e-04  6.465e-04  5.350e+01  -1.348 0.183487    
#   LocSpeech             -7.349e-03  5.025e-04  1.152e+03 -14.627  < 2e-16 ***
#   GlobalSpeechRate      -1.586e-03  1.570e-03  1.035e+03  -1.010 0.312765    
#   TypeOfLtap            -1.487e-02  3.713e-03  1.126e+03  -4.004 6.63e-05 ***
#   PostPausePause         7.605e-03  2.913e-03  1.128e+03   2.611 0.009161 ** 
#   PrecSegDur            -1.433e-01  2.342e-02  9.812e+02  -6.118 1.37e-09 ***
#   Rating                -2.923e-03  1.183e-03  1.074e+03  -2.470 0.013669 *

anova(lyComplex.lmerBC5,lyComplex.lmerBC4)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 15 -4899.0 -4822.9 2464.5  -4929.0                         
# ..1    16 -4898.7 -4817.5 2465.3  -4930.7 1.6956      1     0.1929


# so it did nnot become worse

# let's throw aout GlobSpeech

lyComplex.lmerBC6 <- lmer(bc ~ Environment2+  
                            OrderRescale +
                            logWordFormFreq+
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC6)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            5.670e-01  8.869e-03  5.851e+02  63.928  < 2e-16 ***
#   Environment2l#l-lely   1.445e-02  6.209e-03  6.440e+01   2.327 0.023116 *  
#   Environment2l#l-ll    -1.504e-02  4.266e-03  6.180e+01  -3.526 0.000802 ***
#   Environment2syll. l#l  3.029e-05  4.886e-03  9.850e+01   0.006 0.995066    
#   OrderRescale          -1.564e-04  8.889e-05  1.118e+03  -1.759 0.078848 .  
#   logWordFormFreq       -9.068e-04  6.469e-04  5.330e+01  -1.402 0.166784    
#   LocSpeech             -7.440e-03  4.946e-04  1.157e+03 -15.042  < 2e-16 ***
#   TypeOfLtap            -1.504e-02  3.709e-03  1.127e+03  -4.055 5.35e-05 ***
#   PostPausePause         7.697e-03  2.912e-03  1.129e+03   2.644 0.008318 ** 
#   PrecSegDur            -1.442e-01  2.341e-02  9.835e+02  -6.162 1.05e-09 ***
#   Rating                -2.873e-03  1.182e-03  1.083e+03  -2.429 0.015290 *  

anova(lyComplex.lmerBC5,lyComplex.lmerBC6)
#Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    14 -4899.9 -4828.9 2464.0  -4927.9                         
# object 15 -4899.0 -4822.9 2464.5  -4929.0 1.0482      1     0.3059


# model di not become worse. let's throw out freq

lyComplex.lmerBC7 <- lmer(bc ~ Environment2+  
                            OrderRescale +
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC7)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)            5.643e-01  8.645e-03  6.706e+02  65.268  < 2e-16 ***
#   Environment2l#l-lely   1.576e-02  6.211e-03  6.550e+01   2.538 0.013549 *  
#   Environment2l#l-ll    -1.599e-02  4.266e-03  6.270e+01  -3.748 0.000391 ***
#   Environment2syll. l#l -2.232e-04  4.924e-03  1.023e+02  -0.045 0.963934    
#   OrderRescale          -1.578e-04  8.888e-05  1.118e+03  -1.775 0.076142 .  
#   LocSpeech             -7.439e-03  4.949e-04  1.160e+03 -15.032  < 2e-16 ***
#   TypeOfLtap            -1.512e-02  3.708e-03  1.128e+03  -4.077 4.89e-05 ***
#   PostPausePause         7.801e-03  2.910e-03  1.131e+03   2.681 0.007457 ** 
#   PrecSegDur            -1.484e-01  2.330e-02  9.678e+02  -6.367 2.96e-10 ***
#   Rating                -2.885e-03  1.183e-03  1.084e+03  -2.439 0.014889 *

anova(lyComplex.lmerBC7,lyComplex.lmerBC6)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 13 -4899.8 -4833.9 2462.9  -4925.8                         
# ..1    14 -4899.9 -4828.9 2464.0  -4927.9 2.0814      1     0.1491

# not worse, let's throw ou Order

lyComplex.lmerBC8 <- lmer(bc ~ Environment2+  
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC8)
# Fixed effects:
                        #   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)            5.622e-01  8.572e-03  6.552e+02  65.586  < 2e-16 ***
#   Environment2l#l-lely   1.542e-02  6.224e-03  6.550e+01   2.477 0.015825 *  
#   Environment2l#l-ll    -1.588e-02  4.276e-03  6.270e+01  -3.714 0.000436 ***
#   Environment2syll. l#l -4.960e-04  4.932e-03  1.022e+02  -0.101 0.920095    
#   LocSpeech             -7.506e-03  4.940e-04  1.162e+03 -15.194  < 2e-16 ***
#   TypeOfLtap            -1.492e-02  3.710e-03  1.129e+03  -4.021 6.18e-05 ***
#   PostPausePause         7.762e-03  2.913e-03  1.132e+03   2.665 0.007809 ** 
#   PrecSegDur            -1.478e-01  2.332e-02  9.698e+02  -6.337 3.58e-10 ***
#   Rating                -2.869e-03  1.184e-03  1.084e+03  -2.423 0.015542 * 


anova(lyComplex.lmerBC7,lyComplex.lmerBC8)


# so that would be the final model without interactions
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    12 -4898.7 -4837.8 2461.3  -4922.7                           
# object 13 -4899.8 -4833.9 2462.9  -4925.8 3.1743      1     0.0748 .

# okay! That, in principle, would be the final model iwthout interactions

lyComplex.lmerBC8a <- lmer(bc ~ Environment2+  
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            logWordFormFreq+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC8a)
# no

lyComplex.lmerBC8b <- lmer(bc ~ Environment2+  
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            logRelFreq+ logWordFormFreq+
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC8b)

# Fixed effects:
#                         Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)            5.423e-01  9.874e-03  3.362e+02  54.925  < 2e-16 ***
#   Environment2l#l-lely   1.059e-02  5.971e-03  6.330e+01   1.773  0.08100 .  
#   Environment2l#l-ll    -9.050e-03  4.204e-03  5.560e+01  -2.153  0.03570 *  
#   Environment2syll. l#l  7.819e-03  5.139e-03  1.128e+02   1.522  0.13092    
#   LocSpeech             -7.336e-03  4.935e-04  1.154e+03 -14.864  < 2e-16 ***
#   TypeOfLtap            -1.511e-02  3.713e-03  1.133e+03  -4.069 5.06e-05 ***
#   PostPausePause         7.873e-03  2.917e-03  1.135e+03   2.699  0.00706 ** 
#   PrecSegDur            -1.404e-01  2.322e-02  9.167e+02  -6.049 2.13e-09 ***
#   logRelFreq            -3.477e-03  1.070e-03  5.880e+01  -3.250  0.00191 ** 
#   logWordFormFreq        3.156e-04  7.186e-04  5.720e+01   0.439  0.66212  


lyComplex.lmerBC8c <- lmer(bc ~ Environment2+  
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            logRelFreq+ 
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC8c)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            5.442e-01  8.817e-03  5.119e+02  61.716  < 2e-16 ***
#   Environment2l#l-lely   1.056e-02  5.925e-03  6.420e+01   1.782 0.079448 .  
#   Environment2l#l-ll    -9.130e-03  4.164e-03  5.700e+01  -2.193 0.032433 *  
#   Environment2syll. l#l  7.309e-03  4.999e-03  1.057e+02   1.462 0.146659    
#   LocSpeech             -7.339e-03  4.926e-04  1.152e+03 -14.899  < 2e-16 ***
#   TypeOfLtap            -1.510e-02  3.712e-03  1.133e+03  -4.066 5.11e-05 ***
#   PostPausePause         7.838e-03  2.915e-03  1.138e+03   2.689 0.007268 ** 
#   PrecSegDur            -1.390e-01  2.307e-02  8.873e+02  -6.024 2.49e-09 ***
#   logRelFreq            -3.222e-03  8.919e-04  5.550e+01  -3.612 0.000654 ***



lyComplex.lmerBC8d <- lmer(bc ~ Environment2+  
                            LocSpeech + TypeOfL+
                            PostPause + PrecSegDur+
                            Rating+ logRelFreq+
                            (1|Item) + (1|Participant), data = lyComplex2)
summary(lyComplex.lmerBC8d)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)            5.494e-01  9.088e-03  5.407e+02  60.455  < 2e-16 ***
#   Environment2l#l-lely   9.702e-03  5.946e-03  6.460e+01   1.632 0.107587    
#   Environment2l#l-ll    -1.039e-02  4.208e-03  5.890e+01  -2.470 0.016420 *  
#   Environment2syll. l#l  5.931e-03  5.040e-03  1.083e+02   1.177 0.241866    
#   LocSpeech             -7.341e-03  4.917e-04  1.151e+03 -14.928  < 2e-16 ***
#   TypeOfLtap            -1.491e-02  3.706e-03  1.132e+03  -4.023 6.13e-05 ***
#   PostPausePause         7.908e-03  2.909e-03  1.136e+03   2.719 0.006656 ** 
#   PrecSegDur            -1.394e-01  2.304e-02  8.908e+02  -6.051 2.12e-09 ***
#   Rating                -2.699e-03  1.180e-03  1.085e+03  -2.288 0.022347 *  
#   logRelFreq            -3.133e-03  8.944e-04  5.570e+01  -3.503 0.000914 ***



# so let's see which one is the best model
anova(lyComplex.lmerBC8,lyComplex.lmerBC8d)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -4898.7 -4837.8 2461.3  -4922.7                             
# ..1    13 -4908.6 -4842.6 2467.3  -4934.6 11.897      1  0.0005622 ***

anova(lyComplex.lmerBC8,lyComplex.lmerBC8c)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 12 -4898.7 -4837.8 2461.3  -4922.7                             
# ..1    12 -4905.3 -4844.4 2464.7  -4929.3 6.6461      0  < 2.2e-16 ***

anova(lyComplex.lmerBC8c,lyComplex.lmerBC8d)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)  
# object 12 -4905.3 -4844.4 2464.7  -4929.3                          
# ..1    13 -4908.6 -4842.6 2467.3  -4934.6 5.251      1    0.02193 *


# so lyComplex.lmerBC8d is the best model so faer



visreg(lyComplex.lmerBC8d,  
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5, ylim=c(30,80))

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1. RelFreq+ Environment

# 2. Environment and accentuation

# 3.Rating and environment

# 4. Accen. Post Pause


# Let's see



# 1. RelFreq and Environment

lyComplex.lmerBC8dInteractionRelFreqEnv<- lmer(bc ~ Environment2*logRelFreq+  
                                                   LocSpeech + TypeOfL+
                                                   PostPause + PrecSegDur+
                                                   Rating+ 
                                                   (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC8dInteractionRelFreqEnv)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                       5.572e-01  9.844e-03  3.602e+02  56.603  < 2e-16 ***
#   Environment2l#l-lely              2.293e-03  1.162e-02  5.200e+01   0.197  0.84429    
#   Environment2l#l-ll               -2.018e-02  6.565e-03  6.220e+01  -3.074  0.00314 ** 
#   Environment2syll. l#l            -4.552e-03  6.496e-03  6.460e+01  -0.701  0.48596    
#   logRelFreq                       -5.193e-04  1.459e-03  5.090e+01  -0.356  0.72331    
#   LocSpeech                        -7.334e-03  4.914e-04  1.145e+03 -14.924  < 2e-16 ***
#   TypeOfLtap                       -1.476e-02  3.707e-03  1.132e+03  -3.981  7.3e-05 ***
#   PostPausePause                    8.044e-03  2.908e-03  1.137e+03   2.767  0.00576 ** 
#   PrecSegDur                       -1.394e-01  2.299e-02  8.529e+02  -6.064  2.0e-09 ***
#   Rating                           -2.599e-03  1.178e-03  1.082e+03  -2.205  0.02763 *  
#   Environment2l#l-lely:logRelFreq  -2.460e-03  2.386e-03  5.120e+01  -1.031  0.30734    
#   Environment2l#l-ll:logRelFreq    -4.019e-03  2.150e-03  6.550e+01  -1.870  0.06601 .  
#   Environment2syll. l#l:logRelFreq -5.835e-03  2.255e-03  8.120e+01  -2.587  0.01145 * 

visreg(lyComplex.lmerBC8dInteractionRelFreqEnv, "logRelFreq",by="Environment2", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", 
        cex.axis=0.5)

# hm


anova(lyComplex.lmerBC8dInteractionRelFreqEnv,lyComplex.lmerBC8d)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    13 -4908.6 -4842.6 2467.3  -4934.6                           
# object 16 -4909.8 -4828.6 2470.9  -4941.8 7.2246      3    0.06507 .


# the model is not really better

# 2. Acc and Environment

lyComplex.lmerBC8dInteractionAccEnv<- lmer(bc ~ Environment2*AccentuationCondition+logRelFreq+  
                                             LocSpeech + TypeOfL+
                                             PostPause + PrecSegDur+
                                             Rating+ 
                                             (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC8dInteractionAccEnv)
# Fixed effects:
#                                                           Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                            5.507e-01  9.177e-03  5.430e+02  60.010  < 2e-16 ***
#   Environment2l#l-lely                                   1.904e-02  6.579e-03  9.450e+01   2.894 0.004723 ** 
#   Environment2l#l-ll                                    -1.024e-02  4.596e-03  8.320e+01  -2.227 0.028641 *  
#   Environment2syll. l#l                                  3.980e-03  6.092e-03  2.126e+02   0.653 0.514263    
#   AccentuationConditionunaccented                        2.750e-03  2.449e-03  1.106e+03   1.123 0.261617    
#   logRelFreq                                            -3.041e-03  8.963e-04  5.560e+01  -3.392 0.001283 ** 
#   LocSpeech                                             -7.504e-03  5.326e-04  1.133e+03 -14.092  < 2e-16 ***
#   TypeOfLtap                                            -1.481e-02  3.696e-03  1.128e+03  -4.008 6.53e-05 ***
#   PostPausePause                                         7.589e-03  2.919e-03  1.130e+03   2.599 0.009459 ** 
#   PrecSegDur                                            -1.459e-01  2.313e-02  9.028e+02  -6.307 4.44e-10 ***
#   Rating                                                -2.576e-03  1.177e-03  1.081e+03  -2.189 0.028843 *  
#   Environment2l#l-lely:AccentuationConditionunaccented  -1.779e-02  5.278e-03  1.094e+03  -3.370 0.000778 ***
#   Environment2l#l-ll:AccentuationConditionunaccented    -2.749e-04  4.002e-03  1.095e+03  -0.069 0.945255    
#   Environment2syll. l#l:AccentuationConditionunaccented  2.781e-03  5.725e-03  1.102e+03   0.486 0.627204   



visreg(lyComplex.lmerBC8dInteractionAccEnv, "Environment2",by="AccentuationCondition", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5)

# okay, only for lely words there is a difference


anova(lyComplex.lmerBC8dInteractionAccEnv,lyComplex.lmerBC8d)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    13 -4908.6 -4842.6 2467.3  -4934.6                           
# object 17 -4913.7 -4827.5 2473.8  -4947.7 13.144      4     0.0106 *

# okay it is better

# 3. Rating and Environment

lyComplex.lmerBC8dInteractionRatingEnv<- lmer(bc ~ Environment2*Rating+logRelFreq+  
                                                 LocSpeech + TypeOfL+
                                                 PostPause + PrecSegDur+
                                                 (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC8dInteractionRatingEnv)

# no interaction




# 4. Acc and PostPause

lyComplex.lmerBC8dInteractionAccPause<- lmer(bc ~ Environment2+Rating+logRelFreq+  
                                                LocSpeech + TypeOfL+
                                                PostPause*AccentuationCondition + PrecSegDur+
                                                (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC8dInteractionAccPause)

# no interaction

##################
# It kinda looks like RelFre interacts with no of segments, we need to code this


lyComplex2$NumCon<-as.factor(lyComplex2$Environment)


levels(lyComplex2$NumCon)
#[1] "#l"        "l#l"       "syll. l#l"

levels(lyComplex2$NumCon)<-c( "single"       , "double" ,      "double")



# so let's try an additional model



lyComplex.lmerBC9dInteractionRelFreqNum<- lmer(bc ~ Environment2+NumCon*logRelFreq+  
                                                 LocSpeech + TypeOfL+
                                                 PostPause + PrecSegDur+
                                                 Rating+ 
                                                 (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC9dInteractionRelFreqNum)
visreg(lyComplex.lmerBC9dInteractionRelFreqNum, "logRelFreq", by="NumCon")

visreg(lyComplex.lmerBC9dInteractionRelFreqNum, "Environment2")



lyComplex.lmerBC10dInteractionRelFreqNum<- lmer(bc ~ NumCon*logRelFreq+  
                                                 LocSpeech + TypeOfL+
                                                 PostPause + PrecSegDur+
                                                 Rating+ 
                                                 (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC10dInteractionRelFreqNum)
visreg(lyComplex.lmerBC10dInteractionRelFreqNum, "logRelFreq", by="NumCon")



##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# So there are two interactions: 

# 1. RelFreq and Envirobment (but this model is not better than the one without interactions)

# - The problem is that I can see that, all three double levels bahave similarly


visreg(lyComplex.lmerBC8dInteractionRelFreqEnv, "logRelFreq",by="Environment2", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", 
       cex.axis=0.5)

# So, recoded it as double and single - then we find a clear interaction
visreg(lyComplex.lmerBC9dInteractionRelFreqNum, "logRelFreq", by="NumCon")

# but the how can I still test the effect of Environment (levels are very similar, is it valid
# to still have the variable Environment in there?)

# Compare its effect in both models


visreg(lyComplex.lmerBC8dInteractionRelFreqEnv, "Environment2",by="logRelFreq", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5)


visreg(lyComplex.lmerBC9dInteractionRelFreqNum, "Environment2", 
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", 
       cex.axis=0.5)

# they differ....

# So, what do I do?

# 1. Ignore the interaction (as the model is worse than the one without, when Environment is considered)

# 2. Only code for NumCons (and ignore the fact that there are other differenced between the double 
# environments)

# 3. Take the model with the interaction (even though not all levels are significant, and say that
# this might be due to the number of ovbservations but that it looks like doubles behave differently than
# singletons) - maybe do an additonal model (2)



########################################
#### There is a second interaction#####
# 2. Environment and Accentuation (for lely word)

########################################################################
# let's see whether both interactions are significant in the same model

lyComplex.lmerBC8dInteractionAccEnvRelEnv<- lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                               LocSpeech + TypeOfL+
                                               PostPause+Environment2*AccentuationCondition + PrecSegDur+
                                               (1|Item) + (1|Participant), data = lyComplex2)

summary(lyComplex.lmerBC8dInteractionAccEnvRelEnv)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            5.584e-01  9.934e-03  3.637e+02  56.204  < 2e-16 ***
#   Environment2l#l-lely                                   1.261e-02  1.205e-02  5.930e+01   1.047 0.299502    
# Environment2l#l-ll                                    -1.996e-02  6.813e-03  7.140e+01  -2.929 0.004564 ** 
# Environment2syll. l#l                                 -6.304e-03  7.317e-03  1.009e+02  -0.861 0.391017    
# logRelFreq                                            -4.665e-04  1.462e-03  5.090e+01  -0.319 0.750937    
# Rating                                                -2.479e-03  1.176e-03  1.078e+03  -2.109 0.035211 *  
#   LocSpeech                                             -7.494e-03  5.323e-04  1.127e+03 -14.078  < 2e-16 ***
#   TypeOfLtap                                            -1.466e-02  3.697e-03  1.128e+03  -3.965 7.81e-05 ***
#   PostPausePause                                         7.718e-03  2.918e-03  1.131e+03   2.645 0.008290 ** 
#   AccentuationConditionunaccented                        2.776e-03  2.448e-03  1.106e+03   1.134 0.257069    
# PrecSegDur                                            -1.458e-01  2.309e-02  8.668e+02  -6.315 4.31e-10 ***
#   Environment2l#l-lely:logRelFreq                       -2.253e-03  2.391e-03  5.120e+01  -0.942 0.350465    
# Environment2l#l-ll:logRelFreq                         -4.045e-03  2.152e-03  6.530e+01  -1.879 0.064668 .  
# Environment2syll. l#l:logRelFreq                      -5.820e-03  2.256e-03  8.080e+01  -2.579 0.011706 *  
# Environment2l#l-lely:AccentuationConditionunaccented  -1.787e-02  5.279e-03  1.093e+03  -3.386 0.000734 ***
# Environment2l#l-ll:AccentuationConditionunaccented    -3.901e-04  4.001e-03  1.096e+03  -0.097 0.922349    
# Environment2syll. l#l:AccentuationConditionunaccented  2.580e-03  5.723e-03  1.104e+03   0.451 0.652245  


visreg(lyComplex.lmerBC8dInteractionAccEnvRelEnv, "Environment2", by="AccentuationCondition",
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5)



visreg(lyComplex.lmerBC8dInteractionAccEnvRelEnv, "Environment2", by="logRelFreq",
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", overlay=T,
       cex.axis=0.5)




visreg(lyComplex.lmerBC8dInteractionAccEnvRelEnv, "logRelFreq", by="Environment2",
       trans= function(x) x^(1/lambda)*1000, rug=T, ylab="duration in milliseconds", 
       cex.axis=0.5)


anova(lyComplex.lmerBC8dInteractionAccEnvRelEnv,lyComplex.lmerBC8dInteractionAccEnv)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1    17 -4913.7 -4827.5 2473.8  -4947.7                           
# object 20 -4914.9 -4813.5 2477.5  -4954.9 7.2282      3    0.06497 .


# okay this is our final model

#############################################################
# The final model:

summary(lyComplex.lmerBC8dInteractionAccEnvRelEnv)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                            5.584e-01  9.934e-03  3.637e+02  56.204  < 2e-16 ***
#   Environment2l#l-lely                                   1.261e-02  1.205e-02  5.930e+01   1.047 0.299502    
#   Environment2l#l-ll                                    -1.996e-02  6.813e-03  7.140e+01  -2.929 0.004564 ** 
#   Environment2syll. l#l                                 -6.304e-03  7.317e-03  1.009e+02  -0.861 0.391017    
#   logRelFreq                                            -4.665e-04  1.462e-03  5.090e+01  -0.319 0.750937    
#   Rating                                                -2.479e-03  1.176e-03  1.078e+03  -2.109 0.035211 *  
#   LocSpeech                                             -7.494e-03  5.323e-04  1.127e+03 -14.078  < 2e-16 ***
#   TypeOfLtap                                            -1.466e-02  3.697e-03  1.128e+03  -3.965 7.81e-05 ***
#   PostPausePause                                         7.718e-03  2.918e-03  1.131e+03   2.645 0.008290 ** 
#   AccentuationConditionunaccented                        2.776e-03  2.448e-03  1.106e+03   1.134 0.257069    
#   PrecSegDur                                            -1.458e-01  2.309e-02  8.668e+02  -6.315 4.31e-10 ***
#   Environment2l#l-lely:logRelFreq                       -2.253e-03  2.391e-03  5.120e+01  -0.942 0.350465    
#   Environment2l#l-ll:logRelFreq                         -4.045e-03  2.152e-03  6.530e+01  -1.879 0.064668 .  
#   Environment2syll. l#l:logRelFreq                      -5.820e-03  2.256e-03  8.080e+01  -2.579 0.011706 *  
#   Environment2l#l-lely:AccentuationConditionunaccented  -1.787e-02  5.279e-03  1.093e+03  -3.386 0.000734 ***
#   Environment2l#l-ll:AccentuationConditionunaccented    -3.901e-04  4.001e-03  1.096e+03  -0.097 0.922349    
#   Environment2syll. l#l:AccentuationConditionunaccented  2.580e-03  5.723e-03  1.104e+03   0.451 0.652245    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# I need to rename some variabels for the plot...

lyComplex2<-rename(lyComplex2,PrecedingSegmentDuration=PrecSegDur)

lyComplex2<-rename(lyComplex2,AccentuationAnnotator=Accentuation)

lyComplex2<-rename(lyComplex2,Accentuation=AccentuationCondition)

levels(lyComplex2$PostPause)
#[1] "No Pause" "Pause"   

levels(lyComplex2$PostPause)<-c("no pause","pause")


levels(lyComplex2$Environment2)
#[1]"#l"        "l#l-lely"  "l#l-ll"    "syll. l#l"  

levels(lyComplex2$Environment2)<-c("#l-<l>",  "l#l-<lel>" , "l#l-<ll>" ,   "syll.l#l-<ll>")

levels(lyComplex2$Environment2)
#[1] "#l"        "l#l-lely"  "l#l-ll"    "syll. l#l"


final_ly_complex_model.lmer<-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                    LocSpeech + TypeOfL+
                                    PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+
                                    (1|Item) + (1|Participant), data = lyComplex2)

summary(final_ly_complex_model.lmer)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                       5.584e-01  9.934e-03  3.637e+02  56.204  < 2e-16 ***
#   Environment2l#l-<lel>                             1.261e-02  1.205e-02  5.930e+01   1.047 0.299502    
# Environment2l#l-<ll>                             -1.996e-02  6.813e-03  7.140e+01  -2.929 0.004564 ** 
# Environment2syll.l#l-<ll>                        -6.304e-03  7.317e-03  1.009e+02  -0.861 0.391017    
# logRelFreq                                       -4.665e-04  1.462e-03  5.090e+01  -0.319 0.750937    
# Rating                                           -2.479e-03  1.176e-03  1.078e+03  -2.109 0.035211 *  
#   LocSpeech                                        -7.494e-03  5.323e-04  1.127e+03 -14.078  < 2e-16 ***
#   TypeOfLtap                                       -1.466e-02  3.697e-03  1.128e+03  -3.965 7.81e-05 ***
#   PostPausepause                                    7.718e-03  2.918e-03  1.131e+03   2.645 0.008290 ** 
#   Accentuationunaccented                            2.776e-03  2.448e-03  1.106e+03   1.134 0.257069    
# PrecedingSegmentDuration                         -1.458e-01  2.309e-02  8.668e+02  -6.315 4.31e-10 ***
#   Environment2l#l-<lel>:logRelFreq                 -2.253e-03  2.391e-03  5.120e+01  -0.942 0.350465    
# Environment2l#l-<ll>:logRelFreq                  -4.045e-03  2.152e-03  6.530e+01  -1.879 0.064668 .  
# Environment2syll.l#l-<ll>:logRelFreq             -5.820e-03  2.256e-03  8.080e+01  -2.579 0.011706 *  
# Environment2l#l-<lel>:Accentuationunaccented     -1.787e-02  5.279e-03  1.093e+03  -3.386 0.000734 ***
# Environment2l#l-<ll>:Accentuationunaccented      -3.901e-04  4.001e-03  1.096e+03  -0.097 0.922349    
# Environment2syll.l#l-<ll>:Accentuationunaccented  2.580e-03  5.723e-03  1.103e+03   0.451 0.652245    

#############
# Let's get the  model for the dissertation


table_final_models<-as.data.frame(coef(summary(final_ly_complex_model.lmer)))

xtable(table_final_models,digits = 3)



#############################################################
# Let's now look at each factors contribution to the model
###############################################################

############################################################
# Do we need random effects?
#############################################

# Speaker

lyComplex.finalWithoutSpeaker <-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                       LocSpeech + TypeOfL+
                                       PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+
                                       (1|Item) , data = lyComplex2)


summary(lyComplex.finalWithoutSpeaker)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                   5.592e-01  9.432e-03  3.508e+02  59.295  < 2e-16 ***
#   Environment2l#l-lely                          8.094e-03  1.208e-02  6.080e+01   0.670  0.50551    
# Environment2l#l-ll                           -1.903e-02  6.866e-03  7.330e+01  -2.772  0.00706 ** 
# Environment2syll. l#l                        -8.456e-03  7.432e-03  1.079e+02  -1.138  0.25778    
# logRelFreq                                   -4.691e-04  1.459e-03  5.120e+01  -0.322  0.74913    
# Rating                                       -3.167e-03  1.041e-03  1.161e+03  -3.041  0.00241 ** 
#   LocSpeech                                    -7.761e-03  5.189e-04  1.138e+03 -14.956  < 2e-16 ***
#   TypeOfLtap                                   -1.486e-02  3.753e-03  1.130e+03  -3.960 7.97e-05 ***
#   PostPausepause                                8.375e-03  2.834e-03  1.132e+03   2.955  0.00319 ** 
#   Accentuationunaccented                        3.525e-03  2.603e-03  1.117e+03   1.354  0.17591    
# PrecedingSegmentDuration                     -1.116e-01  2.385e-02  8.245e+02  -4.682 3.32e-06 ***
#   Environment2l#l-lely:logRelFreq              -2.154e-03  2.388e-03  5.160e+01  -0.902  0.37132    
# Environment2l#l-ll:logRelFreq                -3.901e-03  2.166e-03  6.580e+01  -1.801  0.07630 .  
# Environment2syll. l#l:logRelFreq             -5.533e-03  2.283e-03  8.350e+01  -2.423  0.01754 *  
# Environment2l#l-lely:Accentuationunaccented  -1.766e-02  5.679e-03  1.115e+03  -3.110  0.00192 ** 
# Environment2l#l-ll:Accentuationunaccented     6.006e-04  4.295e-03  1.116e+03   0.140  0.88881    
# Environment2syll. l#l:Accentuationunaccented  2.805e-03  6.121e-03  1.123e+03   0.458  0.64679 

cor(lyComplex2$bc, fitted(lyComplex.finalWithoutSpeaker))^2
#[1] 0.3797677



lyComplex.finalWithoutItem <-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                    LocSpeech + TypeOfL+
                                    PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+
                                    (1|Participant), data = lyComplex2)

summary(lyComplex.finalWithoutItem)



# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                   5.363e-01  8.514e-03  7.733e+02  62.986  < 2e-16 ***
#   Environment2l#l-lely                          9.715e-03  6.898e-03  1.142e+03   1.408 0.159269    
# Environment2l#l-ll                           -1.734e-02  4.134e-03  1.143e+03  -4.194 2.95e-05 ***
# Environment2syll. l#l                        -8.641e-03  5.183e-03  1.146e+03  -1.667 0.095796 .  
# logRelFreq                                   -8.747e-04  7.612e-04  1.141e+03  -1.149 0.250754    
# Rating                                       -2.004e-03  1.157e-03  1.088e+03  -1.732 0.083574 .  
# LocSpeech                                    -6.203e-03  5.079e-04  1.160e+03 -12.212  < 2e-16 ***
#   TypeOfLtap                                   -1.646e-02  3.864e-03  1.161e+03  -4.260 2.21e-05 ***
#   PostPausepause                                9.709e-03  3.038e-03  1.155e+03   3.196 0.001433 ** 
#   Accentuationunaccented                        1.498e-03  2.585e-03  1.147e+03   0.579 0.562500    
# PrecedingSegmentDuration                     -9.106e-02  2.006e-02  1.152e+03  -4.538 6.26e-06 ***
#   Environment2l#l-lely:logRelFreq              -2.246e-03  1.254e-03  1.141e+03  -1.791 0.073542 .  
# Environment2l#l-ll:logRelFreq                -3.445e-03  1.210e-03  1.141e+03  -2.847 0.004489 ** 
# Environment2syll. l#l:logRelFreq             -5.246e-03  1.429e-03  1.144e+03  -3.670 0.000253 ***
# Environment2l#l-lely:Accentuationunaccented  -1.762e-02  5.594e-03  1.141e+03  -3.150 0.001673 ** 
# Environment2l#l-ll:Accentuationunaccented     2.204e-04  4.236e-03  1.142e+03   0.052 0.958526    
# Environment2syll. l#l:Accentuationunaccented  3.720e-03  6.032e-03  1.144e+03   0.617 0.537527  

cor(lyComplex2$bc, fitted(lyComplex.finalWithoutItem))^2
#[1]  0.3806


# note that the models are very bad...


anova(lyComplex.finalWithoutItem,final_ly_complex_model.lmer)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 19 -4853.4 -4757.1 2445.7  -4891.4                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 63.519      1  1.588e-15 ***

anova(lyComplex.finalWithoutSpeaker,final_ly_complex_model.lmer)

# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 19 -4798.9 -4702.6 2418.5  -4836.9                            
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9   118      1  < 2.2e-16 ***
#   ---

# Now, let's see how much each factor explains - we will take a look at the ACI for that

# Let's create models in which one of the preditor variables is missing

lyComplex.finalWithoutInteraction1 <-lmer(bc ~ logRelFreq+Rating+  
                                          LocSpeech + TypeOfL+
                                          PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+
                                          (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutEnvironment <-lmer(bc ~ logRelFreq+Rating+  
                                        LocSpeech + TypeOfL+
                                        PostPause+Accentuation+ PrecedingSegmentDuration+
                                        (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutAccentuation<-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                               LocSpeech + TypeOfL+
                                               PostPause+ PrecedingSegmentDuration+
                                               (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutInteraction2<- lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                          LocSpeech + TypeOfL+
                                          PostPause+Accentuation+ PrecedingSegmentDuration+
                                          (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutLocSpeech <-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                        TypeOfL+
                                       PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+
                                       (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutPostPause <- lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                      LocSpeech + TypeOfL+
                                      Environment2*Accentuation+ PrecedingSegmentDuration+
                                      (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutRelFreq <-lmer(bc ~ Rating+  
                                            LocSpeech + TypeOfL+
                                            PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+ 
                                            (1|Item) + (1|Participant), data = lyComplex2)


lyComplex.finalWithoutTypeOfL <-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                            LocSpeech + 
                                            PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+ 
                                            (1|Item) + (1|Participant), data = lyComplex2)

lyComplex.finalWithoutPrecedingSeg <-lmer(bc ~ Environment2*logRelFreq+Rating+logRelFreq+  
                                            LocSpeech + TypeOfL+
                                            PostPause+Environment2*Accentuation+ 
                                            (1|Item) + (1|Participant), data = lyComplex2)

lyComplex.finalWithoutRating <-lmer(bc ~ Environment2*logRelFreq+  
                                       LocSpeech + 
                                       PostPause+Environment2*Accentuation+ PrecedingSegmentDuration+ 
                                       (1|Item) + (1|Participant), data = lyComplex2)



###########################################################################
# Now, let's have a look at the contribution of each factor
###################################################################


anova(lyComplex.finalWithoutSpeaker,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 19 -4798.9 -4702.6 2418.5  -4836.9                            
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9   118      1  < 2.2e-16 ***

4914.9-4798.9 
#[1] 116

anova(lyComplex.finalWithoutItem,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 19 -4853.4 -4757.1 2445.7  -4891.4                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 63.519      1  1.588e-15 ***

4914.9-4853.4 
#61.5

anova(lyComplex.finalWithoutInteraction1,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 17 -4913.7 -4827.5 2473.8  -4947.7                           
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 7.2282      3    0.06497 .
4914.9-4913.7
#1,2

anova(lyComplex.finalWithoutInteraction2,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# object 17 -4907.9 -4821.7 2471.0  -4941.9                            
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 13.003      3   0.004629 **

4914.9-4907.9
#7

anova(lyComplex.finalWithoutEnvironment,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 11 -4890.2 -4834.4 2456.1  -4912.2                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 42.721      9  2.425e-06 ***

4914.9-4890.2 
#24.7

anova(lyComplex.finalWithoutAccentuation,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 16 -4909.8 -4828.6 2470.9  -4941.8                           
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 13.147      4    0.01058 *
4914.9-4909.8
#5,1


anova(lyComplex.finalWithoutRelFreq,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# object 16 -4904.5 -4823.3 2468.2  -4936.5                            
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 18.446      4   0.001009 **
4914.9-4904.5
#[1] 10.4


anova(lyComplex.finalWithoutLocSpeech,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 19 -4737.9 -4641.6 2388.0  -4775.9                            
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9   179      1  < 2.2e-16 ***
4914.9-4737.9 
#177

anova(lyComplex.finalWithoutPrecedingSeg,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 19 -4879.2 -4782.8 2458.6  -4917.2                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 37.754      1  8.026e-10 ***
# #   ---
4914.9-4879.2 
#35.7

anova(final_ly_complex_model.lmer,lyComplex.finalWithoutPostPause)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# ..1    19 -4909.7 -4813.4 2473.9  -4947.7                            
# object 20 -4914.9 -4813.5 2477.5  -4954.9 7.2039      1   0.007275 **
4914.9-4909.7
# 5.2


anova(lyComplex.finalWithoutRating,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 18 -4898.2 -4806.9 2467.1  -4934.2                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 20.699      2  3.201e-05 ***
-4914.9-4898.2
#[1] 16.7


anova(lyComplex.finalWithoutTypeOfL,final_ly_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 19 -4901.0 -4804.7 2469.5  -4939.0                             
# ..1    20 -4914.9 -4813.5 2477.5  -4954.9 15.918      1  6.614e-05 ***
4914.9-4901.0
#[1] 13.9

########################################################
# When we look at the contribution of each factor in the model without
# the interaction, we see the following picture
#######################################################################


# Let's put these numbers in a table

AIC_decrease_lyComplex<-matrix(c(177,116,63,62,25,17,14,10,7,5,5,1),ncol=12,byrow=TRUE)
colnames(AIC_decrease_lyComplex)<-c("Local-\nSpeechRate", "Speaker","Preceding-\nSegmentDuration","Item", "Environment","SemanticTrans-\nparencyRating", 
                                    "TypeOfL","logRelative-\nFrequency", "Env. * Acc.", "Accentuation",
                                    "PostPause",  "Env. * logRelFr.")
rownames(AIC_decrease_lyComplex)<-c("Decrease in AIC")
AIC_decrease_lyComplex <- as.table(AIC_decrease_lyComplex)
AIC_decrease_lyComplex



#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



# plot effect sizes


png("AICdecreaseLYComplex.png", units="cm", height=11, width=18, res=300, pointsize=09)


par(mar=c(2.6,8.1, 1.1, 2), xpd=TRUE, cex=0.9)

barplot((AIC_decrease_lyComplex),horiz=T, col="lightgrey",  names.arg =colnames(AIC_decrease_lyComplex), las=2, xaxt="n",border="lightgrey")

xx<-barplot(AIC_decrease_lyComplex, horiz=T, col="lightgrey",names.arg =colnames(AIC_decrease_lyComplex), las=2, xaxt="n",border="lightgrey")

text(y = xx, x = AIC_decrease_lyComplex ,label = AIC_decrease_lyComplex, pos = 4, cex = 0.8, col = "black")

title(xlab="AIC increase", line=0, cex.lab=1.1)

dev.off()



##############################
# We should also plot all the main effects

#################################

ylim=c(30,100)


###############################
# Plot main effect

png("LyModelInterEnvAcc.png", units="cm", height=12, width=16, res=300, pointsize=15)


visreg(final_ly_complex_model.lmer, "Environment2",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


dev.off()

png("LyModelInterEnvRelFreq.png", units="cm", height=15, width=16.5, res=300, pointsize=15)

par(oma = c(4, 1, 1, 1))


visreg(final_ly_complex_model.lmer, "Environment2",by="logRelFreq",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','mediumaquamarine','darkblue')),
       legend= F,cex=0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 0, 0, 0), 
    new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("logRel-\nFreq=-5.063","logRel-\nFreq=-2.939","logRel-\nFreq=0"), xpd = TRUE, horiz = TRUE, inset = c(0, 
                                                                            0),
       bty = "n", lwd=3,col =c('cornflowerblue','mediumaquamarine','darkblue'), cex =1)

dev.off()


png("LyModelInterRelFreqEnv.png", units="cm", height=16, width=22, res=300, pointsize=15)
# 
par <- trellis.par.get()
# 
 #par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
par$plot.line$col <- "cornflowerblue" # das ist die Farbe der Estimate-Linie 
# #par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg
# 

visreg(final_ly_complex_model.lmer, "logRelFreq",by="Environment2",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=T, xlab="logRelativeFrequency",ylim=ylim,band=F,
       #line.par = list(col = 'cornflowerblue')
       par.settings=par,cex=0.8)


dev.off()


png("LyModelRating.png", units="cm", height=12, width=14, res=300, pointsize=15)


visreg(final_ly_complex_model.lmer, "Rating",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
        xlab="SemanticTransparency\nRating",line.par = list(col ='cornflowerblue'),
                                                            ylim=ylim,band=F,cex=0.8)


dev.off()


library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 

lyComplex.lm<-lm(bc~Environment2*Accentuation+ Environment2*Rating+
                   Environment2*logRelFreq+Accentuation*PostPause+
                   LocSpeech+GlobalSpeechRate+PrecedingSegmentDuration+
                   BaseFinalStress+OrderRescale+logWordFormFreq+PrePause+
                   TypeOfL, data = lyComplex2)

summary(lyComplex.lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   5.438e-01  1.112e-02  48.908  < 2e-16 ***
#   Environment2l#l-lely                          8.787e-03  9.695e-03   0.906 0.364942    
# Environment2l#l-ll                           -1.445e-02  6.142e-03  -2.354 0.018763 *  
# Environment2syll. l#l                        -1.061e-02  8.655e-03  -1.226 0.220544    
# Accentuationunaccented                        7.664e-03  6.360e-03   1.205 0.228482    
# Rating                                       -1.888e-03  1.320e-03  -1.430 0.152880    
# logRelFreq                                   -3.336e-04  9.848e-04  -0.339 0.734868    
# PostPausepause                                1.118e-02  5.341e-03   2.093 0.036542 *  
#   LocSpeech                                    -6.663e-03  5.110e-04 -13.037  < 2e-16 ***
#   GlobalSpeechRate                             -4.043e-03  1.582e-03  -2.556 0.010712 *  
#   PrecedingSegmentDuration                     -6.158e-02  2.380e-02  -2.587 0.009796 ** 
#   BaseFinalStressunstressed                     3.243e-03  3.165e-03   1.025 0.305701    
# OrderRescale                                 -1.331e-04  9.841e-05  -1.352 0.176614    
# logWordFormFreq                              -4.061e-04  4.416e-04  -0.920 0.357871    
# PrePausePause                                 2.858e-03  1.945e-03   1.469 0.142044    
# TypeOfLtap                                   -1.575e-02  3.902e-03  -4.036 5.79e-05 ***
#   Environment2l#l-lely:Accentuationunaccented  -1.796e-02  5.944e-03  -3.021 0.002575 ** 
# Environment2l#l-ll:Accentuationunaccented     9.189e-04  4.500e-03   0.204 0.838234    
# Environment2syll. l#l:Accentuationunaccented  4.280e-03  6.407e-03   0.668 0.504207    
# Environment2l#l-lely:Rating                  -8.188e-04  3.722e-03  -0.220 0.825934    
# Environment2l#l-ll:Rating                    -2.035e-03  2.624e-03  -0.776 0.438174    
# Environment2syll. l#l:Rating                 -1.165e-03  4.171e-03  -0.279 0.780124    
# Environment2l#l-lely:logRelFreq              -2.205e-03  1.359e-03  -1.623 0.104863    
# Environment2l#l-ll:logRelFreq                -3.476e-03  1.308e-03  -2.658 0.007973 ** 
# Environment2syll. l#l:logRelFreq             -5.367e-03  1.624e-03  -3.306 0.000977 ***
# Accentuationunaccented:PostPausepause        -2.517e-03  6.390e-03  -0.394 0.693766  

model_ranking <- dredge(lyComplex.lm)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Relative variable importance: 
#                     LocSpeech logRelFreq Environment2 TypeOfL PostPause Environment2:logRelFreq Accentuation
# Importance:           1.00      1.00       1.00         1.00    0.99      0.98                    0.97       
# N containing models: 11776     14848      18432        11776   14592      6144                   16896       

#                       PrecedingSegmentDuration GlobalSpeechRate Accentuation:Environment2 Rating PrePause OrderRescale
# Importance:           0.96                     0.95             0.91                      0.86   0.52     0.49       
# N containing models: 11776                    11776             6912                     14848  11776    11776       

#                       BaseFinalStress logWordFormFreq Accentuation:PostPause Environment2:Rating
# Importance:           0.43            0.40            0.27                   0.05              
# N containing models: 11776           11776            5632                   6144  

###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

summary(final_ly_complex_model.lmer)
# Fixed effects:
#                                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                                   5.584e-01  9.934e-03  3.637e+02  56.204  < 2e-16 ***
#   Environment2l#l-lely                          1.261e-02  1.205e-02  5.930e+01   1.047 0.299502    
#   Environment2l#l-ll                           -1.996e-02  6.813e-03  7.140e+01  -2.929 0.004564 ** 
#   Environment2syll. l#l                        -6.304e-03  7.317e-03  1.009e+02  -0.861 0.391017    
#   logRelFreq                                   -4.665e-04  1.462e-03  5.090e+01  -0.319 0.750937    
#   Rating                                       -2.479e-03  1.176e-03  1.078e+03  -2.109 0.035211 *  
#   LocSpeech                                    -7.494e-03  5.323e-04  1.127e+03 -14.078  < 2e-16 ***
#   TypeOfLtap                                   -1.466e-02  3.697e-03  1.128e+03  -3.965 7.81e-05 ***
#   PostPausepause                                7.718e-03  2.918e-03  1.131e+03   2.645 0.008290 ** 
#   Accentuationunaccented                        2.776e-03  2.448e-03  1.106e+03   1.134 0.257069    
#   PrecedingSegmentDuration                     -1.458e-01  2.309e-02  8.668e+02  -6.315 4.31e-10 ***
#   Environment2l#l-lely:logRelFreq              -2.253e-03  2.391e-03  5.120e+01  -0.942 0.350465    
#   Environment2l#l-ll:logRelFreq                -4.045e-03  2.152e-03  6.530e+01  -1.879 0.064668 .  
#   Environment2syll. l#l:logRelFreq             -5.820e-03  2.256e-03  8.080e+01  -2.579 0.011706 *  
#   Environment2l#l-lely:Accentuationunaccented  -1.787e-02  5.279e-03  1.093e+03  -3.386 0.000734 ***
#   Environment2l#l-ll:Accentuationunaccented    -3.901e-04  4.001e-03  1.096e+03  -0.097 0.922349    
#   Environment2syll. l#l:Accentuationunaccented  2.580e-03  5.723e-03  1.103e+03   0.451 0.652245   

visreg(final_ly_complex_model.lmer)


# Conditions used in construction of plot
intercept = 5.584e-01

## LocSpeech: 
LocCondition= 10.78101
estSpeech= -7.494e-03 

# PrecSegDur: 
PrecCondition = 0.06682001
estPrec= -1.458e-01

# Rating
RatingCondition= 1
estRating= -2.479e-03
  
#TypeOfL (approx is default)
TypeOfLTapEst=-1.466e-02

# PostPause (pause is defaulr)
PostPausePauseEst=7.718e-03

# RelFreq
RelFreqCond=-2.639057
RelFreqEst= -4.665e-04

CondLowRelFreq= -5.063
CondMidRelFreq= -2.939
CondHighRelFreq= 0

  
  # Environment
# Environment: ly1V
EstEnvironmentlly= -1.996e-02 
EstEnvironmentlely= 1.261e-02
EstEnvironmentlySyl= -6.304e-03


# Accentuation: Accented
EstAccentuationUnaccented= 2.776e-03 

# Interaction1

EstEnvironmentllyUnac=-3.901e-04  
EstEnvironmentlelyUnac=-1.787e-02 
EstEnvironmentlySylUnac= 2.580e-03 


# Interaction2
EstRelFreqLely=-2.253e-03
EstRelFreqll=-4.045e-03 
EstRelFreqSyllLL=-5.820e-03


visreg(final_ly_complex_model.lmer, "Environment2",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=T, xlab="environment",ylim=ylim,band=F,overlay=T,
       line.par = list(col = c('cornflowerblue','darkblue'))
       ,par.settings=par,cex=0.8)

########## Muss noch ausgerechnet werden!!###########
# Levels accented (as in first plot)

((intercept+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 58.42942

Lyaccented= 58.42942

#levels lely accented

((intercept+EstEnvironmentlely+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 64.56828

Lelyaccented= 64.56828

#levels lly accented

((intercept+EstEnvironmentlly+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 49.60656

Llyaccented= 49.60656

#levels syll. lly accented

((intercept+EstEnvironmentlySyl+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 55.52733

LlySyllaccented= 55.52733


################
# Levels unaccented (as in first plot)

((intercept+EstAccentuationUnaccented+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 59.7422

LyUnaccented= 59.7422

#levels lely unaccented

((intercept+EstAccentuationUnaccented+EstEnvironmentlelyUnac+EstEnvironmentlely+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 57.27286

LelyUnaccented= 57.27286

#levels lly unaccented

((intercept+EstAccentuationUnaccented+EstEnvironmentllyUnac+EstEnvironmentlly+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 50.60573

LlyUnaccented= 50.60573

#levels syll. lly unaccented

((intercept+EstAccentuationUnaccented+EstEnvironmentlySylUnac+EstEnvironmentlySyl+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+(RelFreqCond*RelFreqEst)+PostPausePauseEst)^(1/lambda))*1000
#[1] 57.98602

LlySyllUnaccented= 57.98602

###############################
# Now, let's do the same for the different Rel-Freq ranges (items are accented)



visreg(final_ly_complex_model.lmer, "Environment2",by="logRelFreq",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=T, xlab="environment",ylim=ylim,band=F,overlay=T,
       line.par = list(col = c('cornflowerblue','darkblue','green'))
       ,par.settings=par,cex=0.8)


############ Low Freq (high dec)



((intercept+(RelFreqEst*CondLowRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 58.96157

LyLowRelFreq= 58.96157

#levels lely accented

((intercept+EstEnvironmentlely+(CondLowRelFreq*EstRelFreqLely)+(RelFreqEst*CondLowRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1]71.13089

LelyLowFrq= 71.13089

#levels lly accented

((intercept+EstEnvironmentlly+(CondLowRelFreq*EstRelFreqll)+(RelFreqEst*CondLowRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 59.2074

LlyLowFreq= 59.2074

#levels syll. lly accented

((intercept+EstEnvironmentlySyl+(CondLowRelFreq*EstRelFreqSyllLL)+(RelFreqEst*CondLowRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 70.66877

LlySyllLowFreq= 70.66877

############ MidFreq (mid dec)


((intercept+(RelFreqEst*CondMidRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 58.49507

LyMidRelFreq= 58.49507

#levels lely accented

((intercept+EstEnvironmentlely+(CondMidRelFreq*EstRelFreqLely)+(RelFreqEst*CondMidRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1]68.04923

LelyMidFrq= 68.04923

#levels lly accented

((intercept+EstEnvironmentlly+(CondMidRelFreq*EstRelFreqll)+(RelFreqEst*CondMidRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 54.79555

LlyMidFreq= 54.79555

#levels syll. lly accented

((intercept+EstEnvironmentlySyl+(CondMidRelFreq*EstRelFreqSyllLL)+(RelFreqEst*CondMidRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 63.72961

LlySyllMidFreq= 63.72961

############ HighFreq (Low dec)


((intercept+(RelFreqEst*CondHighRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 57.85408

LyHighRelFreq= 57.85408

#levels lely accented

((intercept+EstEnvironmentlely+(CondHighRelFreq*EstRelFreqLely)+(RelFreqEst*CondHighRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1]63.9489

LelyHighFrq= 63.9489

#levels lly accented

((intercept+EstEnvironmentlly+(CondHighRelFreq*EstRelFreqll)+(RelFreqEst*CondHighRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 49.09672

LlyHighFreq= 49.09672

#levels syll. lly accented

((intercept+EstEnvironmentlySyl+(CondHighRelFreq*EstRelFreqSyllLL)+(RelFreqEst*CondHighRelFreq)+(LocCondition*estSpeech)+(PrecCondition*estPrec)+(RatingCondition*estRating)+PostPausePauseEst)^(1/lambda))*1000
#[1] 54.97322

LlySyllHighFreq= 54.97322
