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


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses dis/")


###########################################################################
# I will start with the complex dataset, and thus will need the complex dataset-
# In the following I will first take a look at the pertinent varoables
# and then fit a model
############################################################################



disComplex <- read.csv("disComplex.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(disComplex)

# 'data.frame':	829 obs. of  85 variables:
#   $ X.1                        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                          : int  5000 5001 5002 5003 5004 5005 5006 5007 5008 5009 ...
# $ Item                       : Factor w/ 45 levels "disabled","disabuse",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant                : Factor w/ 22 levels "participant_10A_Experiment_2",..: 9 17 1 5 21 15 3 8 19 11 ...
# $ ID                         : int  2245 3955 152 1296 4898 3347 1045 1892 4531 2677 ...
# $ Filename                   : Factor w/ 829 levels "participant_1_A_110.TextGrid",..: 330 637 91 229 769 516 184 262 702 412 ...
# $ DeletionMorph              : Factor w/ 2 levels "N","V": 1 1 1 1 1 1 1 1 1 1 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 1 2 2 1 1 1 1 2 1 2 ...
# $ Annotator                  : Factor w/ 4 levels "Mandy","Simon",..: 3 4 4 3 4 4 1 3 3 4 ...
# $ Order                      : int  323 72 316 155 224 103 99 116 301 34 ...
# $ WordDur                    : num  0.456 0.538 0.656 0.625 0.755 ...
# $ SyllNum                    : int  3 3 3 3 3 3 3 3 3 3 ...
# $ SegNum                     : int  7 7 7 8 7 7 7 8 7 7 ...
# $ ConsonantDur               : num  0.0887 0.099 0.1299 0.105 0.0928 ...
# $ PrecSeg                    : Factor w/ 7 levels "@","A","d","f",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0455 0.0326 0.0473 0.0711 0.0744 ...
# $ FollSeg                    : Factor w/ 33 levels "@","@O","@U",..: 18 18 18 18 12 18 18 18 18 18 ...
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
# $ Environment2                : Factor w/ 2 levels "s#sV","s#V": 2 2 2 2 2 2 2 2 2 2 ...
# $ Affix                      : Factor w/ 1 level "dis": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  3095 3095 3095 3095 3095 3095 3095 3095 3095 3095 ...
# $ WordFormFrequencyAllCOCA   : int  6368 6368 6368 6368 6368 6368 6368 6368 6368 6368 ...
# $ WordFormFrequencySpokenCOCA: int  812 812 812 812 812 812 812 812 812 812 ...
# $ Base                       : Factor w/ 45 levels "abled","abuse",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFrequencyBNC      : int  3095 3095 3095 3095 3095 3095 3095 3095 3095 3095 ...
# $ BaseLemmaFrequencyBNC      : int  4 4 4 4 4 4 4 4 4 4 ...
# $ SyllPhon                   : int  3 3 3 3 3 3 3 3 3 3 ...
# $ AffixStress                : Factor w/ 1 level "unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseInitialStress          : Factor w/ 2 levels "primary","unstressed": 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": 2 2 2 2 2 2 2 2 2 2 ...
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
# $ Consonant                  : Factor w/ 6 levels "I","I@","l?",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Orthography                : Factor w/ 3 levels "s","sC","ss": 1 1 1 1 1 1 1 1 1 1 ...
# $ median                     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ ConsonantDurMS             : num  88.7 99 129.9 105 92.8 ...
# $ PrePause                   : Factor w/ 2 levels "no pause","pause": 2 2 2 1 2 2 2 2 2 2 ...
# $ PostPause                  : Factor w/ 2 levels "no pause","pause": 1 1 1 1 2 1 2 1 1 1 ...
# $ GlobalSpeechRate           : num  2.506 1.244 0.857 0.969 0.91 ...
# $ WordFreqCategorical        : Factor w/ 3 levels "HighFreq","LowFreq",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Environment22               : Factor w/ 3 levels "s#sV-stressed",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ StressPattern              : Factor w/ 2 levels "unstressed-primary",..: 1 1 1 1 1 1 1 1 1 1 ...

disComplex$X.1<-NULL
disComplex$X<-NULL

###############################################################
#   Summary: variables to include                            ##
###############################################################

## We are going to include the following predictors:

# - Item (rand. effect)
# - Participant (rand. effect)

# - Order
# - Environment22
# - logWordFormFreq
# - Accentuation

# - Loc Speech  and/or Global Speech
# - PrePause
# - PostPause

# - PrecSegDur 
# - logRelFreq 
# - Type of Base 
# - SemanticTransparency 
# - Rating

# Let's see whether it males sense to include the decomposability measures (only
# makes sense if we have variability)

########################################################################
# The decomposability measures(I need to analyze them and their relations
# in a separate analysis before I can decide how to procede...)
#########################################################################

# probabaly influence of each individual variable and the PC

# 1. Semantic Transparency

table(disComplex$SemanticTransparency)
# opaque transparent 
# 162         667 




# 2. Type of Base

table(disComplex$TypeOfBase)
# bound  word 
#  70   759  


table(disComplex$TypeOfBase,disComplex$SemanticTransparency)
# opaque transparent
# bound     70           0
# word      92         667

#is okayish


# 3. Rating

table(disComplex$Rating)
# 1    2    3    4 
#590 119  69  51 



######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = disComplex)
cor(disComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.1999072


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = disComplex)
cor(disComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.3028347

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = disComplex)
cor(disComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.4945594

# so arodis1d 49 percent of the variability can be explained by this! That's a lot


##              Do an initial model:
disComplex$OrderRescale<-disComplex$Order*0.1

disComplex.lmer1 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause + PrecSegDur+
                          (1|Item) + (1|Participant), data = disComplex)


summary(disComplex.lmer1)    

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.783e-01  8.808e-03  5.195e+02  20.240  < 2e-16 ***
#   Environment2s#V-stressed        -2.529e-02  3.572e-03  3.760e+01  -7.082 2.02e-08 ***
# Environment2s#V-unstressed      -2.854e-02  2.840e-03  4.130e+01 -10.049 1.18e-12 ***
# AccentuationConditionunaccented -1.309e-03  2.189e-03  7.498e+02  -0.598    0.550    
# OrderRescale                    -8.298e-05  7.003e-05  7.827e+02  -1.185    0.236    
# logWordFormFreq                 -1.515e-04  5.326e-04  3.650e+01  -0.285    0.778    
# LocSpeech                       -3.677e-03  4.404e-04  5.720e+02  -8.350 4.44e-16 ***
#   GlobalSpeechRate                -1.038e-03  2.073e-03  7.064e+02  -0.501    0.617    
# PrePausepause                    1.062e-03  4.022e-03  7.875e+02   0.264    0.792    
# PostPausepause                  -7.599e-05  1.927e-03  8.160e+02  -0.039    0.969    
# PrecSegDur                       4.707e-02  5.915e-02  7.540e+02   0.796    0.426    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) Envrnmnt2s#V-s Envrnmnt2s#V-n AccntC OrdrRs lgWrFF LcSpch GlblSR PrPsps PstPsp
# Envrnmnt2s#V-s -0.098                                                                               
# Envrnmnt2s#V-n -0.099  0.470                                                                        
# AccnttnCndt     0.156  0.035          0.017                                                         
# OrderRescal    -0.083  0.006         -0.019          0.129                                          
# lgWrdFrmFrq    -0.179 -0.017          0.162          0.048 -0.024                                   
# LocSpeech      -0.678 -0.054         -0.109         -0.251 -0.035 -0.022                            
# GloblSpchRt    -0.217 -0.026          0.002         -0.654 -0.168 -0.039 -0.006                     
# PrePausepas    -0.439  0.035          0.022          0.010  0.020  0.003 -0.003  0.053              
# PostPauseps    -0.320  0.013          0.019          0.175  0.071 -0.005  0.332  0.053 -0.004       
# PrecSegDur     -0.474 -0.080         -0.190          0.069 -0.012  0.000  0.275  0.013 -0.076 -0.032

cor(disComplex$ConsonantDur, fitted(disComplex.lmer1))^2
#[1] 0.5751389


#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before sldisming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own


# 1.logWordFormFreq & logRelFreq

# Model woth both 
disComplex.lmerFrequencies <- lmer(ConsonantDur ~ logWordFormFreq+ logRelFreq+ (1|Item) + (1|Participant), data = disComplex)

summary(disComplex.lmerFrequencies)    


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      0.1170573  0.0071746 52.3500000  16.315   <2e-16 ***
#   logWordFormFreq -0.0002135  0.0013847 41.4900000  -0.154    0.878    
# logRelFreq       0.0005978  0.0007587 42.0700000   0.788    0.435   

cor(disComplex$ConsonantDur, fitted(disComplex.lmerFrequencies))^2
#[1] 0.494534


# only Word Form Freq

disComplex.lmerWordFrequency <- lmer(ConsonantDur ~ logWordFormFreq + (1|Item) + (1|Participant), data = disComplex)

summary(disComplex.lmerWordFrequency)    

# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)     1.127e-01  4.521e-03 6.040e+01  24.924   <2e-16 ***
#   logWordFormFreq 5.318e-04  1.007e-03 4.241e+01   0.528      0.6    

cor(disComplex$ConsonantDur, fitted(disComplex.lmerWordFrequency))^2
#[1] 0.4945513


# only RelFreq
disComplex.lmerRelFrequency <- lmer(ConsonantDur ~ logRelFreq+  (1|Item) + (1|Participant), data = disComplex)

summary(disComplex.lmerRelFrequency)    


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 1.161e-01  3.984e-03 5.819e+01  29.154   <2e-16 ***
#   logRelFreq  5.179e-04  5.478e-04 4.303e+01   0.945     0.35  

cor(disComplex$ConsonantDur, fitted(disComplex.lmerRelFrequency))^2
#[1]0.4945093



# So, neither is ever a sign. predictor

# only Categorical WordFormFreq

disComplex.lmerWordFreqCategorical <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Item) + (1|Participant), data = disComplex)

summary(disComplex.lmerWordFreqCategorical)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                 0.119320   0.005320 57.930000  22.427   <2e-16 ***
#   WordFreqCategoricalLowFreq -0.007045   0.005773 41.410000  -1.220    0.229    
# WordFreqCategoricalMidFreq -0.006274   0.006376 41.850000  -0.984    0.331    

# No effect! However, one thing I would also like to check is whether the presense of Item
# as a random effect has an influence

# Let us see what happen if we have a model without a random effect for Item and bith Variables

disComplex.lmerFrequenciesWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ logWordFormFreq+ (1|Participant), data = disComplex)

summary(disComplex.lmerFrequenciesWithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.156e-01  3.579e-03  6.750e+01  32.296   <2e-16 ***
#   logRelFreq       5.314e-04  2.887e-04  8.068e+02   1.841   0.0661 .  
# logWordFormFreq -2.106e-05  5.133e-04  8.084e+02  -0.041   0.9673    

cor(disComplex$ConsonantDur, fitted(disComplex.lmerFrequenciesWithoutItem))^2
#[1] 0.2057699



# RelFreq without Item
disComplex.lmerRelFrequencyWithoutItem <- lmer(ConsonantDur ~ logRelFreq+ (1|Participant), data = disComplex)

summary(disComplex.lmerRelFrequencyWithoutItem)

# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 1.155e-01  2.789e-03 2.540e+01  41.412   <2e-16 ***
#   logRelFreq  5.232e-04  2.099e-04 8.075e+02   2.492   0.0129 *  

cor(disComplex$ConsonantDur, fitted(disComplex.lmerRelFrequencyWithoutItem))^2
#[1] 0.2057697


# WordFreq qithout Item

disComplex.lmerWordFreqWithoutItem <- lmer(ConsonantDur ~ logWordFormFreq+ (1|Participant), data = disComplex)

summary(disComplex.lmerWordFreqWithoutItem)

# (Intercept)     1.116e-01  2.880e-03 2.860e+01  38.760   <2e-16 ***
#   logWordFormFreq 6.269e-04  3.740e-04 8.090e+02   1.676   0.0941 .    

cor(disComplex$ConsonantDur, fitted(disComplex.lmerWordFreqWithoutItem))^2
#[1] 0.2026006



disComplex.lmerWordFreqCategoricalWiithoutItem <- lmer(ConsonantDur ~ WordFreqCategorical+ (1|Participant), data = disComplex)

summary(disComplex.lmerWordFreqCategoricalWiithoutItem)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                  0.118996   0.003075  37.100000  38.703  < 2e-16 ***
#   WordFreqCategoricalLowFreq  -0.007525   0.002156 808.000000  -3.491 0.000507 ***
#   WordFreqCategoricalMidFreq  -0.007020   0.002395 805.800000  -2.931 0.003476 ** 

cor(disComplex$ConsonantDur, fitted(disComplex.lmerWordFreqCategoricalWiithoutItem))^2
#[1] 0.2126973

#####################################
# Summary Coll. Frequencies:
#- Freq only become significant when the random effect of Item is
# removed. The models become much worse then
# - There is s suprssion effect. We will first only take Rel Freq
#################################################


# 2.  Loc Speech  and/or Global Speech


cor.test(disComplex$LocSpeech,disComplex$GlobalSpeechRate)



disComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = disComplex)

summary(disComplex.lmerSpeechRates)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)       1.686e-01  5.215e-03  3.132e+02  32.325   <2e-16 ***
#   LocSpeech        -4.282e-03  3.863e-04  8.232e+02 -11.085   <2e-16 ***
#   GlobalSpeechRate -1.419e-03  1.529e-03  8.048e+02  -0.928    0.354  

cor(disComplex$ConsonantDur, fitted(disComplex.lmerSpeechRates))^2
#[1] 0.5801314



disComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = disComplex)

summary(disComplex.lmerLocSpeech)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.688e-01  5.220e-03  3.095e+02   32.34   <2e-16 ***
#   LocSpeech   -4.445e-03  3.431e-04  8.201e+02  -12.96   <2e-16 ***

cor(disComplex$ConsonantDur, fitted(disComplex.lmerLocSpeech))^2
#[1] 0.580083


disComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = disComplex)

print(summary(disComplex.lmerGlobalSpeech),digits=3)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)        0.12582    0.00370  87.00000    34.0  < 2e-16 ***
#   GlobalSpeechRate  -0.00919    0.00146 781.00000    -6.3  4.9e-10 ***

cor(disComplex$ConsonantDur, fitted(disComplex.lmerGlobalSpeech))^2
#[1] 0.5158509


#####################################
# Summary Coll. Speech Rates:
#
# no supression effect bur LocSpeech seems to be the better predictor
# we will include both for now
#################################################


##############################################################
# The decomposability variables

############
#Rating

disComplex.lmerRating<- lmer(ConsonantDur ~ Rating + (1|Item)+(1|Participant), data = disComplex)

summary(disComplex.lmerRating)
   
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.144e-01  3.920e-03  7.340e+01  29.198   <2e-16 ***
#   Rating      -1.404e-04  1.208e-03  7.353e+02  -0.116    0.908    
#  not significant


########
#Type of base

disComplex.lmerTypeOfBase<- lmer(ConsonantDur ~ TypeOfBase + (1|Item)+(1|Participant), data = disComplex)


summary(disComplex.lmerTypeOfBase)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)     0.129533   0.007222 58.390000  17.935   <2e-16 ***
#   TypeOfBaseword -0.017122   0.007146 46.550000  -2.396   0.0206 *  

# significant

######################
# RelFreq
disComplex.lmerRelFreq<- lmer(ConsonantDur ~ logRelFreq+ (1|Item)+(1|Participant), data = disComplex)

summary(disComplex.lmerRelFreq)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 1.161e-01  3.984e-03 5.819e+01  29.154   <2e-16 ***
#   logRelFreq  5.179e-04  5.478e-04 4.303e+01   0.945     0.35    
# not significant

##############
# Semantic Trans


disComplex.lmerST<- lmer(ConsonantDur ~ SemanticTransparency+ (1|Item)+(1|Participant), data = disComplex)

summary(disComplex.lmerST)


# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      0.124456   0.005342 61.910000  23.296   <2e-16 ***
#   SemanticTransparencytransparent -0.013076   0.005312 44.370000  -2.462   0.0178 *              0.0178 *
#   
# we need to check the effect of ST at the end...

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################
# Now we have dealt with all collinearity problems: 
# - We will keep only keep WordForm frequency variables (supession)
# - We will keep both Speech Rate variables but must be aware of the fact that their effect
#   size cannot be interpreted!
# - Dec: we need to check the effect of ST and Type of Base at the end (+PC)
###############################################################################################



# Let's refit our model incorportaing the "right variables"

disComplex.lmer3 <- lmer(ConsonantDur ~ Environment2+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                            LocSpeech + GlobalSpeechRate +
                           PrePause + PostPause + PrecSegDur+
                            (1|Item) + (1|Participant), data = disComplex)

(summary(disComplex.lmer3))

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      1.783e-01  8.808e-03  5.195e+02  20.240  < 2e-16 ***
#   Environment2s#V-stressed        -2.529e-02  3.572e-03  3.760e+01  -7.082 2.02e-08 ***
# Environment2s#V-unstressed      -2.854e-02  2.840e-03  4.130e+01 -10.049 1.18e-12 ***
# AccentuationConditionunaccented -1.309e-03  2.189e-03  7.498e+02  -0.598    0.550    
# OrderRescale                    -8.298e-05  7.003e-05  7.827e+02  -1.185    0.236    
# logWordFormFreq                 -1.515e-04  5.326e-04  3.650e+01  -0.285    0.778    
# LocSpeech                       -3.677e-03  4.404e-04  5.720e+02  -8.350 4.44e-16 ***
#   GlobalSpeechRate                -1.038e-03  2.073e-03  7.064e+02  -0.501    0.617    
# PrePausepause                    1.062e-03  4.022e-03  7.875e+02   0.264    0.792    
# PostPausepause                  -7.599e-05  1.927e-03  8.160e+02  -0.039    0.969    
# PrecSegDur                       4.707e-02  5.915e-02  7.540e+02   0.796    0.426  

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (disComplex.lmer3))
qqline (residuals (disComplex.lmer3))

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


disComplex.lm<-lm(ConsonantDur ~ Environment2+ AccentuationCondition+OrderRescale +logWordFormFreq+
                     LocSpeech + GlobalSpeechRate +
                    PrePause + PostPause + PrecSegDur, data = disComplex)

summary(disComplex.lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      1.972e-01  7.763e-03  25.398  < 2e-16 ***
#   Environment2s#V-stressed        -2.579e-02  2.263e-03 -11.397  < 2e-16 ***
# Environment2s#V-unstressed      -2.877e-02  1.856e-03 -15.500  < 2e-16 ***
# AccentuationConditionunaccented  1.797e-03  2.062e-03   0.871    0.384    
# OrderRescale                    -1.115e-04  7.883e-05  -1.414    0.158    
# logWordFormFreq                 -1.435e-05  3.327e-04  -0.043    0.966    
# LocSpeech                       -3.790e-03  3.981e-04  -9.521  < 2e-16 ***
#   GlobalSpeechRate                -7.478e-03  1.765e-03  -4.236 2.53e-05 ***
#   PrePausepause                   -3.549e-03  4.420e-03  -0.803    0.422    
# PostPausepause                  -2.971e-03  1.873e-03  -1.586    0.113    
# PrecSegDur                      -3.380e-02  5.120e-02  -0.660    0.509   

bc<-boxcox(disComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.2626263

disComplex$bc <- disComplex$ConsonantDur^lambda

disComplex.lmerBC <- lmer(bc ~ Environment2+ AccentuationCondition+OrderRescale +logWordFormFreq+
                             LocSpeech + GlobalSpeechRate +
                            PrePause + PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = disComplex)

summary(disComplex.lmerBC)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      6.453e-01  1.110e-02  5.216e+02  58.155  < 2e-16 ***
#   Environment2s#V-stressed        -2.954e-02  4.477e-03  3.820e+01  -6.598 8.40e-08 ***
# Environment2s#V-unstressed      -3.362e-02  3.561e-03  4.200e+01  -9.440 6.11e-12 ***
# AccentuationConditionunaccented -1.698e-03  2.761e-03  7.491e+02  -0.615    0.539    
# OrderRescale                    -9.027e-05  8.835e-05  7.832e+02  -1.022    0.307    
# logWordFormFreq                 -1.741e-04  6.675e-04  3.710e+01  -0.261    0.796    
# LocSpeech                       -4.587e-03  5.549e-04  5.702e+02  -8.266 8.88e-16 ***
#   GlobalSpeechRate                -2.526e-03  2.614e-03  7.048e+02  -0.966    0.334    
# PrePausepause                    3.192e-04  5.074e-03  7.881e+02   0.063    0.950    
# PostPausepause                  -1.021e-03  2.431e-03  8.160e+02  -0.420    0.675    
# PrecSegDur                       2.977e-02  7.459e-02  7.529e+02   0.399    0.690  

#let's check the assumptions

qqnorm (residuals (disComplex.lmerBC))
qqline (residuals (disComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(disComplex.lmerBC, disComplex, trim = 2.5)
# n.removed = 16 
# percent.removed = 1.930036 

disComplex2<-outliers$data

dim(disComplex2)
#[1] 813  86


dim(disComplex)
#[1]  829  85




# okay it seemes to have worked

disComplex.lmerBC2 <- lmer(bc ~ Environment2+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                              LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      6.419e-01  9.535e-03  4.885e+02  67.320  < 2e-16 ***
#   Environment2s#V-stressed        -2.848e-02  4.692e-03  3.950e+01  -6.070 3.93e-07 ***
# Environment2s#V-unstressed      -3.251e-02  3.700e-03  4.220e+01  -8.785 4.44e-11 ***
# AccentuationConditionunaccented -3.049e-04  2.242e-03  7.546e+02  -0.136    0.892    
# OrderRescale                    -2.524e-05  7.081e-05  7.579e+02  -0.356    0.722    
# logWordFormFreq                 -2.365e-04  7.023e-04  3.880e+01  -0.337    0.738    
# LocSpeech                       -4.228e-03  4.692e-04  7.222e+02  -9.011  < 2e-16 ***
#   GlobalSpeechRate                -3.404e-03  2.136e-03  7.333e+02  -1.594    0.111    
# PrePausepause                    2.631e-03  4.046e-03  7.618e+02   0.650    0.516    
# PostPausepause                  -1.651e-03  1.973e-03  7.946e+02  -0.837    0.403    
# PrecSegDur                      -3.945e-02  6.260e-02  7.577e+02  -0.630    0.529    

qqnorm (residuals (disComplex.lmerBC2))
qqline (residuals (disComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> disComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(disComplex.lmerBC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      6.419e-01  9.535e-03  4.885e+02  67.320  < 2e-16 ***
#   Environment2s#V-stressed        -2.848e-02  4.692e-03  3.950e+01  -6.070 3.93e-07 ***
#   Environment2s#V-unstressed      -3.251e-02  3.700e-03  4.220e+01  -8.785 4.44e-11 ***
#   AccentuationConditionunaccented -3.049e-04  2.242e-03  7.546e+02  -0.136    0.892    
  # OrderRescale                    -2.524e-05  7.081e-05  7.579e+02  -0.356    0.722    
#   logWordFormFreq                 -2.365e-04  7.023e-04  3.880e+01  -0.337    0.738    
#   LocSpeech                       -4.228e-03  4.692e-04  7.222e+02  -9.011  < 2e-16 ***
#   GlobalSpeechRate                -3.404e-03  2.136e-03  7.333e+02  -1.594    0.111    
#   PrePausepause                    2.631e-03  4.046e-03  7.618e+02   0.650    0.516    
#   PostPausepause                  -1.651e-03  1.973e-03  7.946e+02  -0.837    0.403    
#   PrecSegDur                      -3.945e-02  6.260e-02  7.577e+02  -0.630    0.529  


# let's throw out Accentuation

disComplex.lmerBC3 <- lmer(bc ~ Environment2+ OrderRescale +logWordFormFreq+
                              LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC3)

# Fixed effects:
#                               Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                 6.421e-01  9.420e-03  4.910e+02  68.167  < 2e-16 ***
#   Environment2s#V-stressed   -2.846e-02  4.692e-03  3.960e+01  -6.065 3.98e-07 ***
#   Environment2s#V-unstressed -3.250e-02  3.702e-03  4.230e+01  -8.779 4.46e-11 ***
#   OrderRescale               -2.398e-05  7.013e-05  7.566e+02  -0.342   0.7325    
#   logWordFormFreq            -2.331e-04  7.023e-04  3.880e+01  -0.332   0.7418    
#   LocSpeech                  -4.246e-03  4.542e-04  7.245e+02  -9.349  < 2e-16 ***
#   GlobalSpeechRate           -3.597e-03  1.616e-03  7.909e+02  -2.226   0.0263 *  
#   PrePausepause               2.633e-03  4.044e-03  7.629e+02   0.651   0.5152    
#   PostPausepause             -1.612e-03  1.942e-03  7.973e+02  -0.830   0.4070    
#   PrecSegDur                 -3.892e-02  6.235e-02  7.621e+02  -0.624   0.5327

anova(disComplex.lmerBC2,disComplex.lmerBC3)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -4010.1 -3949.0 2018.0  -4036.1                         
# object 14 -4008.1 -3942.3 2018.1  -4036.1 0.0177      1     0.8941

# model did not become worse


# let's throw out Freq

disComplex.lmerBC4 <- lmer(bc ~ Environment2+ OrderRescale +
                              LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC4)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.413e-01  9.135e-03  5.779e+02  70.205  < 2e-16 ***
#   Environment2s#V-stressed   -2.850e-02  4.638e-03  4.050e+01  -6.144 2.82e-07 ***
# Environment2s#V-unstressed -3.231e-02  3.613e-03  4.340e+01  -8.942 2.12e-11 ***
# OrderRescale               -2.443e-05  7.011e-05  7.575e+02  -0.348   0.7276    
# LocSpeech                  -4.239e-03  4.535e-04  7.243e+02  -9.346  < 2e-16 ***
#   GlobalSpeechRate           -3.613e-03  1.616e-03  7.915e+02  -2.236   0.0256 *  
#   PrePausepause               2.627e-03  4.044e-03  7.631e+02   0.650   0.5160    
# PostPausepause             -1.605e-03  1.942e-03  7.982e+02  -0.826   0.4088    
# PrecSegDur                 -3.845e-02  6.234e-02  7.627e+02  -0.617   0.5375                   


# # nothing has changed

anova(disComplex.lmerBC3,disComplex.lmerBC4)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    12 -4012.0 -3955.6   2018  -4036.0                         
# object 13 -4010.1 -3949.0   2018  -4036.1 0.1185      1     0.7307

# nothing has changed



# let's throw out Order

disComplex.lmerBC5 <- lmer(bc ~ Environment2+
                              LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + PrecSegDur+
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC5)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                 6.410e-01  9.082e-03  5.724e+02  70.574  < 2e-16 ***
#   Environment2s#V-stressed   -2.850e-02  4.636e-03  4.050e+01  -6.147 2.80e-07 ***
#   Environment2s#V-unstressed -3.232e-02  3.611e-03  4.340e+01  -8.951 2.07e-11 ***
#   LocSpeech                  -4.240e-03  4.533e-04  7.251e+02  -9.354  < 2e-16 ***
#   GlobalSpeechRate           -3.668e-03  1.607e-03  7.929e+02  -2.283   0.0227 *  
#   PrePausepause               2.656e-03  4.040e-03  7.643e+02   0.657   0.5111    
#   PostPausepause             -1.575e-03  1.939e-03  7.992e+02  -0.812   0.4167    
#   PrecSegDur                 -3.902e-02  6.228e-02  7.635e+02  -0.627   0.5312 

anova(disComplex.lmerBC4,disComplex.lmerBC5)

# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -4013.9 -3962.1 2017.9  -4035.9                         
# object 12 -4012.0 -3955.6 2018.0  -4036.0 0.1213      1     0.7276

#nothing has changed


# let's throw out PrecSeg

disComplex.lmerBC6 <- lmer(bc ~ Environment2+
                              LocSpeech + GlobalSpeechRate +
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC6)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.380e-01  7.792e-03  4.806e+02  81.880  < 2e-16 ***
#   Environment2s#V-stressed   -2.867e-02  4.605e-03  4.040e+01  -6.226 2.18e-07 ***
# Environment2s#V-unstressed -3.268e-02  3.549e-03  4.160e+01  -9.208 1.38e-11 ***
# LocSpeech                  -4.145e-03  4.291e-04  7.364e+02  -9.659  < 2e-16 ***
#   GlobalSpeechRate           -3.601e-03  1.603e-03  7.954e+02  -2.247   0.0249 *  
#   PrePausepause               2.454e-03  4.026e-03  7.652e+02   0.610   0.5423    
# PostPausepause             -1.592e-03  1.938e-03  8.006e+02  -0.822   0.4115  

anova(disComplex.lmerBC5,disComplex.lmerBC6)


# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    10 -4015.5 -3968.5 2017.7  -4035.5                         
# object 11 -4013.9 -3962.1 2017.9  -4035.9 0.3807      1     0.5372            

# nothing has changed


# let's throw out PrePause

disComplex.lmerBC7 <- lmer(bc ~ Environment2+
                              LocSpeech + GlobalSpeechRate +
                             PostPause + 
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC7)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.405e-01  6.552e-03  3.137e+02  97.757  < 2e-16 ***
#   Environment2s#V-stressed   -2.873e-02  4.599e-03  4.040e+01  -6.248 2.03e-07 ***
# Environment2s#V-unstressed -3.269e-02  3.546e-03  4.160e+01  -9.221 1.32e-11 ***
# LocSpeech                  -4.150e-03  4.289e-04  7.366e+02  -9.676  < 2e-16 ***
#   GlobalSpeechRate           -3.696e-03  1.595e-03  7.962e+02  -2.317   0.0208 *  
#   PostPausepause             -1.582e-03  1.937e-03  8.017e+02  -0.817   0.4142   

anova(disComplex.lmerBC6,disComplex.lmerBC7)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1     9 -4017.1 -3974.8 2017.6  -4035.1                         
# object 10 -4015.5 -3968.5 2017.7  -4035.5 0.3634      1     0.5466

 #still no difference

# let's throw out post pause



disComplex.lmerBC8 <- lmer(bc ~ Environment2+
                             + LocSpeech + GlobalSpeechRate +
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBC8)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.377e-01  5.563e-03  2.143e+02 114.621  < 2e-16 ***
#   Environment2s#V-stressed   -2.873e-02  4.567e-03  4.070e+01  -6.291 1.72e-07 ***
# Environment2s#V-unstressed -3.268e-02  3.521e-03  4.190e+01  -9.283 1.01e-11 ***
# LocSpeech                  -3.992e-03  3.853e-04  7.688e+02 -10.360  < 2e-16 ***
#   GlobalSpeechRate           -3.404e-03  1.555e-03  8.045e+02  -2.189   0.0289 *  

anova(disComplex.lmerBC7,disComplex.lmerBC8)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# ..1     8 -4018.5 -3980.9 2017.2  -4034.5                        
# object  9 -4017.1 -3974.8 2017.6  -4035.1 0.649      1     0.4205



# so that would be the final model without interactions

###########################################################################
#           Checking for dec. effects                                     #
###########################################################################
  
# I will include each variable at a time and see whether it has an effect  

  
# RelFreq  
disComplex.lmerBC9RelFreq <- lmer(bc ~  Environment2+ LocSpeech + GlobalSpeechRate +logRelFreq+
                               (1|Item) + (1|Participant), data = disComplex2)
  
summary(disComplex.lmerBC9RelFreq)


# no

################Semantic Transparency

disComplex.lmerBC9SemT <- lmer(bc ~  Environment2+ LocSpeech + GlobalSpeechRate +SemanticTransparency+
                               (1|Item) + (1|Participant), data = disComplex2)
  
summary(disComplex.lmerBC9SemT)
  
#NO


#####Type of Base
disComplex.lmerBC9TypeBase <- lmer(bc ~  Environment2+ LocSpeech + GlobalSpeechRate + TypeOfBase+
                               (1|Item) + (1|Participant), data = disComplex2)
  
summary(disComplex.lmerBC9TypeBase)
  
# no

##########Rating
disComplex.lmerBC9Rating <- lmer(bc ~  Environment2+ LocSpeech + GlobalSpeechRate +Rating+
                               (1|Item) + (1|Participant), data = disComplex2)
  
summary(disComplex.lmerBC9Rating)
  
# no


##############################
# Let us check for interactions now








###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view

# There are actually which I would consider to be of interest

# 1. Environment2 and accentuation  

#.2.  Decomposability measures and Environment2

# 3. PrePause and accentuation


# Let's see



# 1. Environment2 and accentuation


disComplex.lmerBC7EnvAcc<- lmer(bc ~ Environment2*AccentuationCondition+ LocSpeech + GlobalSpeechRate+
                               (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7EnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                 6.434e-01  6.229e-03  2.725e+02 103.288  < 2e-16 ***
#   Environment2s#V-stressed                                   -3.144e-02  4.903e-03  5.540e+01  -6.413 3.35e-08 ***
# Environment2s#V-unstressed                                 -3.900e-02  3.796e-03  5.790e+01 -10.274 1.15e-14 ***
# AccentuationConditionunaccented                            -7.528e-03  2.996e-03  7.767e+02  -2.513   0.0122 *  
#   LocSpeech                                                  -4.163e-03  4.136e-04  7.616e+02 -10.066  < 2e-16 ***
#   GlobalSpeechRate                                           -3.392e-03  2.078e-03  7.460e+02  -1.632   0.1030    
# Environment2s#V-stressed:AccentuationConditionunaccented    5.530e-03  3.911e-03  7.472e+02   1.414   0.1578    
# Environment2s#V-unstressed:AccentuationConditionunaccented  1.312e-02  3.083e-03  7.481e+02   4.256 2.35e-05 ***

anova(disComplex.lmerBC7,disComplex.lmerBC7EnvAcc)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  9 -4017.1 -3974.8 2017.6  -4035.1                             
# ..1    11 -4031.2 -3979.5 2026.6  -4053.2 18.067      2  0.0001193 ***

# yeah it is better

############################################
# 2. Decomposability measures andEnvironment2

# RelFreq
disComplex.lmerBC7PC1RelEnv<- lmer(bc ~ Environment2*logRelFreq+LocSpeech + GlobalSpeechRate+
                                        (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7PC1RelEnv)

# no
#############

#SemTr
disComplex.lmerBC7PC1STEnv<- lmer(bc ~ Environment2*SemanticTransparency+LocSpeech + GlobalSpeechRate+
                                       (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7PC1STEnv)

#no
##############

#TypeOfBase

disComplex.lmerBC7PC1BaseEnv<- lmer(bc ~ Environment2*TypeOfBase+LocSpeech + GlobalSpeechRate+
                                         (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7PC1BaseEnv)
# not testable
########################

#Rating
disComplex.lmerBC7PC1RatingEnv<- lmer(bc ~ Environment2*Rating+LocSpeech + GlobalSpeechRate+
                                           (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7PC1RatingEnv)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                        6.436e-01  6.260e-03  2.838e+02 102.811  < 2e-16 ***
#   Environment2s#V-stressed          -3.317e-02  6.327e-03  1.286e+02  -5.244 6.29e-07 ***
# Environment2s#V-unstressed        -4.388e-02  5.141e-03  1.462e+02  -8.535 1.64e-14 ***
# Rating                            -2.826e-03  1.406e-03  5.386e+02  -2.010  0.04495 *  
#   LocSpeech                         -3.982e-03  3.838e-04  7.591e+02 -10.376  < 2e-16 ***
#   GlobalSpeechRate                  -3.310e-03  1.551e-03  8.021e+02  -2.135  0.03309 *  
#   Environment2s#V-stressed:Rating    1.540e-03  2.837e-03  7.868e+02   0.543  0.58740    
# Environment2s#V-unstressed:Rating  7.068e-03  2.458e-03  7.952e+02   2.876  0.00414 ** 

visreg(disComplex.lmerBC7PC1RatingEnv, "Rating", by="Environment2")

# yes, find an effect but only for unstressed s#V, and if we look at the distribution
# of the rating for that variables/Environment we see that the effect is just cause by a few items

table(disComplex2$Environment2,disComplex2$Rating)
#                 1   2   3   4
# s#sV-stressed  103  40  42  46
# s#V-stressed   118  23  15   0
# s#V-unstressed 361  53   9   3

###############################

#####################################
#5. Pause and accentuation

disComplex.lmerBC7PC1AccPause<- lmer(bc ~ Environment2+PrePause*AccentuationCondition+
                                        LocSpeech + GlobalSpeechRate+
                                        (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7PC1AccPause)
##############################################################################################
#            Summary interactions   --> Sdisplification of our model                        ##
##############################################################################################

# I tested the interactoins of Env and the decomposability variables, as well as the one
# between the two prosodic variables prePause and Acc, and Acc and Env (only one interaction
# was sign)
# Our final model has one interaction Env*Accentuation


#############################################################
# The final model:

summary(disComplex.lmerBC7EnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                 6.434e-01  6.229e-03  2.725e+02 103.288  < 2e-16 ***
#   Environment2s#V-stressed                                   -3.144e-02  4.903e-03  5.540e+01  -6.413 3.35e-08 ***
# Environment2s#V-unstressed                                 -3.900e-02  3.796e-03  5.790e+01 -10.274 1.15e-14 ***
# AccentuationConditionunaccented                            -7.528e-03  2.996e-03  7.767e+02  -2.513   0.0122 *  
#   LocSpeech                                                  -4.163e-03  4.136e-04  7.616e+02 -10.066  < 2e-16 ***
#   GlobalSpeechRate                                           -3.392e-03  2.078e-03  7.460e+02  -1.632   0.1030    
# Environment2s#V-stressed:AccentuationConditionunaccented    5.530e-03  3.911e-03  7.472e+02   1.414   0.1578    
# Environment2s#V-unstressed:AccentuationConditionunaccented  1.312e-02  3.083e-03  7.481e+02   4.256 2.35e-05 ***


# now GlobSpeech is no longer significant - what happens if we throw it out



disComplex.lmerBC7EnvAcc2<- lmer(bc ~ Environment2*AccentuationCondition+ LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)


summary(disComplex.lmerBC7EnvAcc2)

#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                 6.407e-01  6.089e-03  2.518e+02 105.233  < 2e-16 ***
#   Environment2s#V-stressed                                   -3.181e-02  4.914e-03  5.500e+01  -6.472 2.75e-08 ***
# Environment2s#V-unstressed                                 -3.904e-02  3.809e-03  5.770e+01 -10.250 1.29e-14 ***
# AccentuationConditionunaccented                            -1.004e-02  2.576e-03  7.652e+02  -3.898 0.000105 ***
#   LocSpeech                                                  -4.196e-03  4.132e-04  7.670e+02 -10.153  < 2e-16 ***
#   Environment2s#V-stressed:AccentuationConditionunaccented    5.908e-03  3.903e-03  7.480e+02   1.514 0.130542    
# Environment2s#V-unstressed:AccentuationConditionunaccented  1.323e-02  3.082e-03  7.495e+02   4.294 1.98e-05 ***

anova(disComplex.lmerBC7EnvAcc2,disComplex.lmerBC7EnvAcc)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 10 -4030.5 -3983.5 2025.2  -4050.5                         
# ..1    11 -4031.2 -3979.5 2026.6  -4053.2 2.6776      1     0.1018

# no difference, so we will take that model

# I need to rename some variabels for the plot...


disComplex2<-rename(disComplex2,AccentuationAnnotator=Accentuation)

disComplex2<-rename(disComplex2,Accentuation=AccentuationCondition)

levels(disComplex2$Environment2)


final_dis_complex_model.lmer<- lmer(bc ~ Environment2*Accentuation+ LocSpeech + 
                                      (1|Item) + (1|Participant), data = disComplex2)
summary(final_dis_complex_model.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.407e-01  6.089e-03  2.518e+02 105.233  < 2e-16 ***
#   Environment2s#V-stressed                          -3.181e-02  4.914e-03  5.500e+01  -6.472 2.75e-08 ***
# Environment2s#V-unstressed                        -3.904e-02  3.809e-03  5.770e+01 -10.250 1.29e-14 ***
# Accentuationunaccented                            -1.004e-02  2.576e-03  7.652e+02  -3.898 0.000105 ***
#   LocSpeech                                         -4.196e-03  4.132e-04  7.670e+02 -10.153  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    5.908e-03  3.903e-03  7.480e+02   1.514 0.130542    
# Environment2s#V-unstressed:Accentuationunaccented  1.323e-02  3.082e-03  7.495e+02   4.294 1.98e-05 ***



visreg(final_dis_complex_model.lmer, trans= function(x) (x^(1/lambda))*1000, 
       rug=F, ylab="duration in milliseconds", cex.axis=0.9,ylim=c(20,180))

#############
# Let's get the two models for the dissertation


table_final_models<-as.data.frame(coef(summary(final_dis_complex_model.lmer)))

xtable(table_final_models,digits = 3)


#############################################################
# Let's now look at each factors contribution to the model
###############################################################

############################################################
# Do we need random effects?
#############################################

# Speaker

disComplex.finalWithoutSpeaker <- lmer(bc ~ Environment2*Accentuation+ LocSpeech + 
                                         (1|Item) , data = disComplex2)
summary(disComplex.finalWithoutSpeaker)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.579e-01  5.670e-03  3.165e+02 116.020  < 2e-16 ***
#   Environment2s#V-stressed                          -3.012e-02  5.395e-03  5.630e+01  -5.583 7.06e-07 ***
# Environment2s#V-unstressed                        -3.965e-02  4.187e-03  5.930e+01  -9.470 1.80e-13 ***
# Accentuationunaccented                            -7.003e-03  2.987e-03  7.757e+02  -2.345   0.0193 *  
#   LocSpeech                                         -5.691e-03  4.127e-04  7.893e+02 -13.789  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    3.652e-03  4.575e-03  7.657e+02   0.798   0.4250    
# Environment2s#V-unstressed:Accentuationunaccented  1.569e-02  3.611e-03  7.662e+02   4.344 1.58e-05 ***

 cor(disComplex2$bc, fitted(disComplex.finalWithoutSpeaker))^2
#[1] 0.5095168



disComplex.finalWithoutItem <- lmer(bc ~ Environment2*Accentuation+ LocSpeech + 
                                       (1|Participant), data = disComplex2)

summary(disComplex.finalWithoutItem)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.314e-01  5.379e-03  2.299e+02 117.381  < 2e-16 ***
#   Environment2s#V-stressed                          -3.337e-02  2.959e-03  7.865e+02 -11.281  < 2e-16 ***
# Environment2s#V-unstressed                        -4.057e-02  2.346e-03  7.868e+02 -17.293  < 2e-16 ***
# Accentuationunaccented                            -1.320e-02  2.795e-03  7.892e+02  -4.721 2.78e-06 ***
#   LocSpeech                                         -3.230e-03  3.789e-04  8.044e+02  -8.525  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    6.760e-03  4.305e-03  7.875e+02   1.570    0.117    
# Environment2s#V-unstressed:Accentuationunaccented  1.440e-02  3.393e-03  7.873e+02   4.244 2.46e-05 ***

cor(disComplex2$bc, fitted(disComplex.finalWithoutItem))^2
#[1] 0.5638909


# Now, let's see how much each factor explains - we will take a look at the ACI for that

# Let's create models in which one of the preditor variables is missing

disComplex.finalWithoutInteraction1 <- lmer(bc ~ Environment2+Accentuation+ LocSpeech + 
                                              (1|Item) + (1|Participant), data = disComplex2)

disComplex.finalWithoutEnvironment2<- lmer(bc ~ Accentuation+ LocSpeech + 
                                             (1|Item) + (1|Participant), data = disComplex2)

disComplex.finalWithoutAccentuation<- lmer(bc ~ Environment2+ LocSpeech + 
                                                   (1|Item) + (1|Participant), data = disComplex2)



disComplex.finalWithoutLocSpeech <- lmer(bc ~ Environment2*Accentuation+ 
                                           (1|Item) + (1|Participant), data = disComplex2)



###########################################################################
# Now, let's have a look at the contribution of each factor
###################################################################


anova(disComplex.finalWithoutSpeaker,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  9 -3823.7 -3781.4 1920.8  -3841.7                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 208.81      1  < 2.2e-16 ***

4030.5-3823.7
#[1] 206.8

anova(disComplex.finalWithoutItem,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  9 -3944.5 -3902.2 1981.2  -3962.5                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 88.019      1  < 2.2e-16 ***

4030.5-3944.5
#86

anova(disComplex.finalWithoutInteraction1,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  8 -4015.6 -3978.0 2015.8  -4031.6                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 18.867      2  7.999e-05 ***
4030.5-4015.6
#14.9


anova(disComplex.finalWithoutEnvironment2,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  6 -3969.1 -3940.9 1990.5  -3981.1                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 69.436      4  2.985e-14 ***
4030.5-3969.1
#61.4

anova(disComplex.finalWithoutAccentuation,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  7 -4015.6 -3982.7 2014.8  -4029.6                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 20.907      3  0.0001101 ***
4030.5-4015.6
#14.9

anova(disComplex.finalWithoutLocSpeech,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object  9 -3936.9 -3894.6 1977.4  -3954.9                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 95.631      1  < 2.2e-16 ***
4030.5-3936.9  
#93.6


########################################################
# When we look at the contribution of each factor in the model without
# the interaction, we see the following picture
#######################################################################


# Let's put these numbers in a table

AIC_decrease_disComplex<-matrix(c(207,94,86,61, 15,15),ncol=6,byrow=TRUE)
colnames(AIC_decrease_disComplex)<-c("Speaker",  "Local-\nSpeechRate", "Item", "Environment","Env. * Acc.", 
                                    "Accentuation")
rownames(AIC_decrease_disComplex)<-c("Decrease in AIC")
AIC_decrease_disComplex <- as.table(AIC_decrease_disComplex)
AIC_decrease_disComplex

# This is an alternative way of creating a table. The trick is that 
# you can assign names to the rows and the columns directly in the calls
# to rbind() and c():
# table2 <- rbind("Decrease in AIC"= c("Speaker"=207,
#                                      "Local-\nSpeechRate"=94,
#                                      "Item"=86,
#                                      "Environment"=61,
#                                      "Interaction 1\n(Env. & Acc.)"=15,
#                                      "Accentuation"=15),
#                 "R-Squared"=c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99))


#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Experiment")



# plot effect sizes


png("AICdecreaseDisComplex.png", units="cm", height=7.5, width=17, res=300, pointsize=09)


par(mar=c(2.6,8.1, 1.1, 2), xpd=TRUE, cex=0.9)

barplot((AIC_decrease_disComplex),horiz=T, col="lightgrey",  names.arg =colnames(AIC_decrease_disComplex), las=2, xaxt="n")

xx<-barplot(AIC_decrease_disComplex, horiz=T, col="lightgrey",names.arg =colnames(AIC_decrease_disComplex), las=2, xaxt="n", border="lightgrey")

text(y = xx, x = AIC_decrease_disComplex ,label = AIC_decrease_disComplex, pos = 4, cex = 0.8, col = "black")

title(xlab="AIC increase", line=0, cex.lab=1.1)

dev.off()


##############################
# We should also plot the main effect (not covariates)
###############################
# Plot main effect


# I need to rename the variable level of env, otherwise plot does not show
# them, they are too long

levels(disComplex2$Environment2)

#[1] "s#sV-stressed"  "s#V-stressed"   "s#V-unstressed"

levels(disComplex2$Environment2)<-c("s#sV-str."  ,"s#V-str." ,  "s#V-unstr.")
#[1] "s#sV-str."  "s#V-str."   "s#V-unstr."


Final_dis_complex_model.lmer<- lmer(bc ~ Environment2 * Accentuation + LocSpeech + (1 | Item) + (1 |      Participant), data=disComplex2)

png("DisModelInterEnvAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)

ylim=c(20,180)

par <- trellis.par.get()

#par <- lapply(par, function(x) replace(x, names(x) == "lwd", 4)) # das ist Liniendicke #
#par$plot.line$col <- default # das ist die Farbe der EstInate-Linie 
#par$strip.border<-1
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß In plot weg

#visreg(final_In_complex_model.lmer, "Environment",by="BaseInitialStress",ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, rug=F, xlab="environment by base-initial stress",ylIn=ylIn,cex.axis=0.9,par.settings=par)


visreg(Final_dis_complex_model.lmer, "Environment2",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)



dev.off()




###########################
# Pc-Analysis: Decomposability
##################################


# PCP Anylse


library(pls)


# we need to recode all the variables, so that they are numeric

# # also they need to "point in the same direction" --> the higher
# the less decomposable

# RelFreq is fine



#Type pf base

levels(disComplex2$TypeOfBase)
#[1] "bound" "word" 

disComplex2$TypeOfBase <- relevel (disComplex2$TypeOfBase, ref= "word"   )

disComplex2$TypeOfBaseNum<-as.numeric(disComplex2$TypeOfBase)

table(disComplex2$TypeOfBase,disComplex2$TypeOfBaseNum)
#        1   2
# word  746   0
# bound   0  67


#Smenatic Transparency

levels(disComplex2$SemanticTransparency)
#[1] "opaque"      "transparent"

disComplex2$SemanticTransparency <- relevel (disComplex2$SemanticTransparency, ref= "transparent")

disComplex2$NumSemanticTransparency<-as.numeric(disComplex2$SemanticTransparency)

table(disComplex2$SemanticTransparency,disComplex2$NumSemanticTransparency)
#               1   2
# transparent 656   0
# opaque        0 157

# Rating must be numeric for this


disComplex2$RatingNum<-as.numeric(disComplex2$Rating)

# one further problem is that the variables are on 
# different scales - so we need to change this

disComplex2$ScaledSemanticTransparency<-as.numeric(scale(disComplex2$NumSemanticTransparency))
summary(disComplex2$ScaledSemanticTransparency)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.4889 -0.4889 -0.4889  0.0000 -0.4889  2.0430 


disComplex2$ScaledRating<-as.numeric(scale(disComplex2$RatingNum))
summary(disComplex2$RatingNum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   1.000   1.000   1.486   2.000   4.000 



disComplex2$ScaledTypeOfBase<-as.numeric(scale(disComplex2$TypeOfBaseNum))
summary(disComplex2$TypeOfBaseNum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   1.000   1.000   1.082   1.000   2.000 

disComplex2$ScaledRelFreq<-as.numeric(scale(disComplex2$logRelFreq))
summary(disComplex2$ScaledRelFreq)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.65700 -0.64470 -0.07489  0.00000  0.34290  2.55300 


decomposability.pc <- prcomp(disComplex2[, c("ScaledRelFreq","ScaledRating","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability.pc)
# Importance of components:
#   PC1    PC2    PC3     PC4
# Standard deviation     1.6049 0.7680 0.6687 0.62242
# Proportion of Variance 0.6439 0.1475 0.1118 0.09685
# Cumulative Proportion  0.6439 0.7913 0.9032 1.00000


# Firast 3 important

decomposability.pc$rotation
#                               PC1         PC2        PC3           PC4
# ScaledRelFreq              -0.4637228  0.81665797  0.3435149  0.0053338588
# ScaledRating               -0.4886444 -0.55918361  0.6697312  0.0006682097
# ScaledTypeOfBase           -0.5227198 -0.09773224 -0.4622753 -0.7095871973
# ScaledSemanticTransparency -0.5224479 -0.10407626 -0.4687848  0.7045971281

# PC1 is a micture of all

# PC 2 is mostyl RelFreq and Rating

# Pc 3 is mostly RelFreq but also others


# let's see whether they influence the model

disComplex2$PCDec1 <- decomposability.pc$x[, 1]
disComplex2$PCDec2 <- decomposability.pc$x[, 2]
disComplex2$PCDec3 <- decomposability.pc$x[, 3]
disComplex2$PCDec4 <- decomposability.pc$x[, 4]

# let's see whether this has an influence


disComplex.lmerBCPC<- lmer(bc ~ Environment2+ Accentuation+ OrderRescale+PrePause
                              + LocSpeech + PrecSegDur +PCDec1+PCDec2+PCDec3+PCDec4+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.386e-01  8.821e-03  4.821e+02  72.395  < 2e-16 ***
#   Environment2s#V-stressed   -3.206e-02  4.994e-03  3.810e+01  -6.420 1.49e-07 ***
# Environment2s#V-unstressed -3.699e-02  4.383e-03  3.960e+01  -8.438 2.23e-10 ***
# Accentuationunaccented     -2.320e-03  1.640e-03  7.882e+02  -1.414   0.1577    
# OrderRescale               -4.094e-05  6.972e-05  7.579e+02  -0.587   0.5572    
# PrePausepause               3.218e-03  4.050e-03  7.614e+02   0.795   0.4271    
# LocSpeech                  -4.098e-03  4.435e-04  7.612e+02  -9.241  < 2e-16 ***
#   PrecSegDur                 -4.219e-02  6.278e-02  7.680e+02  -0.672   0.5018    
# PCDec1                      2.069e-03  1.078e-03  4.230e+01   1.920   0.0617 .  
# PCDec2                      6.454e-05  1.788e-03  4.750e+01   0.036   0.9714    
# PCDec3                      1.075e-03  1.787e-03  6.930e+01   0.602   0.5494    
# PCDec4                     -3.680e-04  2.579e-03  3.910e+01  -0.143   0.8873   

# let's simplify, first Pc2

disComplex.lmerBCPC2<- lmer(bc ~ Environment2+ Accentuation+ OrderRescale+PrePause
                           + LocSpeech + PrecSegDur +PCDec1+PCDec3+PCDec4+
                             (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC2)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.385e-01  8.790e-03  4.868e+02  72.637  < 2e-16 ***
#   Environment2s#V-stressed   -3.205e-02  4.927e-03  3.890e+01  -6.506 1.04e-07 ***
# Environment2s#V-unstressed -3.697e-02  4.306e-03  4.020e+01  -8.588 1.23e-10 ***
# Accentuationunaccented     -2.339e-03  1.637e-03  7.911e+02  -1.429   0.1534    
# OrderRescale               -4.099e-05  6.971e-05  7.585e+02  -0.588   0.5567    
# PrePausepause               3.208e-03  4.048e-03  7.626e+02   0.793   0.4282    
# LocSpeech                  -4.089e-03  4.420e-04  7.546e+02  -9.251  < 2e-16 ***
#   PrecSegDur                 -4.187e-02  6.276e-02  7.688e+02  -0.667   0.5048    
# PCDec1                      2.070e-03  1.064e-03  4.410e+01   1.945   0.0581 .  
# PCDec3                      1.042e-03  1.509e-03  2.513e+02   0.691   0.4903    
# PCDec4                     -3.672e-04  2.548e-03  4.020e+01  -0.144   0.8861  

anova(disComplex.lmerBCPC2,disComplex.lmerBCPC)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 14 -4008.9 -3943.1 2018.5  -4036.9                        
# ..1    15 -4006.9 -3936.4 2018.5  -4036.9 7e-04      1     0.9789

# next,PC 4

disComplex.lmerBCPC3<- lmer(bc ~ Environment2+ Accentuation+ OrderRescale+PrePause
                            + LocSpeech + PrecSegDur +PCDec1+PCDec3+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC3)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.384e-01  8.770e-03  4.905e+02  72.798  < 2e-16 ***
#   Environment2s#V-stressed   -3.215e-02  4.827e-03  3.970e+01  -6.660 5.79e-08 ***
# Environment2s#V-unstressed -3.689e-02  4.212e-03  4.140e+01  -8.759 5.72e-11 ***
# Accentuationunaccented     -2.348e-03  1.635e-03  7.925e+02  -1.436   0.1515    
# OrderRescale               -4.097e-05  6.970e-05  7.589e+02  -0.588   0.5568    
# PrePausepause               3.214e-03  4.046e-03  7.635e+02   0.794   0.4273    
# LocSpeech                  -4.086e-03  4.399e-04  7.435e+02  -9.287  < 2e-16 ***
#   PrecSegDur                 -4.158e-02  6.274e-02  7.695e+02  -0.663   0.5077    
# PCDec1                      2.051e-03  1.045e-03  4.600e+01   1.963   0.0557 .  
# PCDec3                      1.027e-03  1.497e-03  2.625e+02   0.686   0.4936   



anova(disComplex.lmerBCPC2,disComplex.lmerBCPC3)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -4010.9 -3949.8 2018.4  -4036.9                         
# object 14 -4008.9 -3943.1 2018.5  -4036.9 0.0239      1     0.8772

# next  Order



disComplex.lmerBCPC4<- lmer(bc ~ Environment2+ Accentuation+ PrePause
                            + LocSpeech + PrecSegDur +PCDec1+PCDec3+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC4)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.379e-01  8.724e-03  4.826e+02  73.124  < 2e-16 ***
#   Environment2s#V-stressed   -3.214e-02  4.829e-03  3.970e+01  -6.654 5.92e-08 ***
# Environment2s#V-unstressed -3.690e-02  4.214e-03  4.130e+01  -8.756 5.81e-11 ***
# Accentuationunaccented     -2.331e-03  1.634e-03  7.934e+02  -1.427   0.1541    
# PrePausepause               3.296e-03  4.042e-03  7.646e+02   0.816   0.4150    
# LocSpeech                  -4.105e-03  4.386e-04  7.463e+02  -9.359  < 2e-16 ***
#   PrecSegDur                 -4.215e-02  6.271e-02  7.704e+02  -0.672   0.5017    
# PCDec1                      2.048e-03  1.045e-03  4.600e+01   1.960   0.0561 .  
# PCDec3                      9.873e-04  1.496e-03  2.619e+02   0.660   0.5098  
# 

anova(disComplex.lmerBCPC4,disComplex.lmerBCPC3)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 12 -4012.5 -3956.1 2018.3  -4036.5                         
# ..1    13 -4010.9 -3949.8 2018.4  -4036.9 0.3542      1     0.5517


# next PC 3

disComplex.lmerBCPC5<- lmer(bc ~ Environment2+ Accentuation+ PrePause
                            + LocSpeech + PrecSegDur +PCDec1+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC5)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.371e-01  8.640e-03  5.047e+02  73.739  < 2e-16 ***
#   Environment2s#V-stressed   -3.178e-02  4.790e-03  4.170e+01  -6.635 5.01e-08 ***
#   Environment2s#V-unstressed -3.631e-02  4.111e-03  4.650e+01  -8.833 1.63e-11 ***
#   Accentuationunaccented     -2.379e-03  1.632e-03  7.942e+02  -1.457   0.1454    
#   PrePausepause               3.415e-03  4.037e-03  7.650e+02   0.846   0.3978    
#   LocSpeech                  -4.079e-03  4.369e-04  7.426e+02  -9.336  < 2e-16 ***
#   PrecSegDur                 -4.182e-02  6.269e-02  7.712e+02  -0.667   0.5049    
#   PCDec1                      1.834e-03  9.913e-04  6.820e+01   1.850   0.0687 .  

anova(disComplex.lmerBCPC4,disComplex.lmerBCPC5)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    11 -4014.1 -3962.3 2018.0  -4036.1                         
# object 12 -4012.5 -3956.1 2018.3  -4036.5 0.4584      1     0.4984


# next PrecSegDur
disComplex.lmerBCPC6<- lmer(bc ~ Environment2+ Accentuation+ PrePause
                            + LocSpeech + PCDec1+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC6)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.340e-01  7.315e-03  3.914e+02  86.673  < 2e-16 ***
#   Environment2s#V-stressed   -3.192e-02  4.765e-03  4.170e+01  -6.699 4.05e-08 ***
# Environment2s#V-unstressed -3.665e-02  4.065e-03  4.550e+01  -9.014 1.09e-11 ***
# Accentuationunaccented     -2.263e-03  1.621e-03  7.954e+02  -1.396   0.1631    
# PrePausepause               3.198e-03  4.023e-03  7.662e+02   0.795   0.4269    
# LocSpeech                  -3.985e-03  4.151e-04  7.551e+02  -9.600  < 2e-16 ***
#   PCDec1                      1.813e-03  9.875e-04  6.800e+01   1.836   0.0707 .

anova(disComplex.lmerBCPC6,disComplex.lmerBCPC5)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object 10 -4015.6 -3968.6 2017.8  -4035.6                         
# ..1    11 -4014.1 -3962.3 2018.0  -4036.1 0.4285      1     0.5127


# next PrePause
disComplex.lmerBCPC7<- lmer(bc ~ Environment2+ Accentuation
                            + LocSpeech + PCDec1+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC7)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.372e-01  6.091e-03  2.235e+02 104.624  < 2e-16 ***
#   Environment2s#V-stressed   -3.195e-02  4.762e-03  4.170e+01  -6.710 3.92e-08 ***
# Environment2s#V-unstressed -3.659e-02  4.062e-03  4.550e+01  -9.007 1.12e-11 ***
# Accentuationunaccented     -2.359e-03  1.616e-03  7.963e+02  -1.459   0.1448    
# LocSpeech                  -3.993e-03  4.149e-04  7.555e+02  -9.625  < 2e-16 ***
#   PCDec1                      1.778e-03  9.859e-04  6.800e+01   1.804   0.0757 .  


anova(disComplex.lmerBCPC6,disComplex.lmerBCPC7)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1     9 -4017.0 -3974.7 2017.5  -4035.0                         
# object 10 -4015.6 -3968.6 2017.8  -4035.6 0.6261      1     0.4288


# next Acc
disComplex.lmerBCPC8<- lmer(bc ~ Environment2
                            + LocSpeech + PCDec1+
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC8)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.403e-01  5.755e-03  2.037e+02 111.255  < 2e-16 ***
#   Environment2s#V-stressed   -3.176e-02  4.842e-03  4.200e+01  -6.559 6.19e-08 ***
# Environment2s#V-unstressed -3.639e-02  4.127e-03  4.600e+01  -8.816 1.92e-11 ***
# LocSpeech                  -4.344e-03  3.438e-04  8.002e+02 -12.635  < 2e-16 ***
#   PCDec1                      1.763e-03  9.991e-04  6.940e+01   1.764   0.0821 .  

anova(disComplex.lmerBCPC8,disComplex.lmerBCPC7)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# object  8 -4016.8 -3979.2 2016.4  -4032.8                         
# ..1     9 -4017.0 -3974.7 2017.5  -4035.0 2.1823      1     0.1396

# next Dec 1

disComplex.lmerBCPC9<- lmer(bc ~ Environment2
                            + LocSpeech + 
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                 6.380e-01  5.632e-03  2.100e+02 113.288  < 2e-16 ***
#   Environment2s#V-stressed   -2.875e-02  4.664e-03  4.090e+01  -6.163 2.57e-07 ***
# Environment2s#V-unstressed -3.250e-02  3.594e-03  4.200e+01  -9.042 2.09e-11 ***
# LocSpeech                  -4.379e-03  3.441e-04  8.039e+02 -12.724  < 2e-16 ***


anova(disComplex.lmerBCPC8,disComplex.lmerBCPC9)
# Data: disComplex2
# Models:
#   ..1: bc ~ Environment2 + LocSpeech + (1 | Item) + (1 | Participant)
# object: bc ~ Environment2 + LocSpeech + PCDec1 + (1 | Item) + (1 | Participant)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# ..1     7 -4015.6 -3982.7 2014.8  -4029.6                           
# object  8 -4016.8 -3979.2 2016.4  -4032.8 3.2251      1    0.07252 .

# that woud be the final model without interactions.

# But we need to test for interactions (between the Dec-variables
# and Environment - the others we have tested alreayd)

##############################
##Interactions with PCs



disComplex.lmerBCPC9Int1<- lmer(bc ~ Environment2*PCDec1
                            + LocSpeech + 
                              (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int1)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                        6.420e-01  5.797e-03  2.007e+02 110.756  < 2e-16 ***
#   Environment2s#V-stressed          -3.291e-02  4.821e-03  4.140e+01  -6.827 2.76e-08 ***
# Environment2s#V-unstressed        -3.131e-02  4.519e-03  6.900e+01  -6.928 1.80e-09 ***
# PCDec1                             2.655e-03  1.076e-03  5.850e+01   2.467  0.01658 *  
#   LocSpeech                         -4.385e-03  3.431e-04  7.996e+02 -12.779  < 2e-16 ***
#   Environment2s#V-stressed:PCDec1   -1.641e-03  3.445e-03  1.404e+02  -0.476  0.63454    
# Environment2s#V-unstressed:PCDec1 -1.020e-02  3.580e-03  4.590e+02  -2.851  0.00456 ** 


disComplex.lmerBCPC9Int2<- lmer(bc ~ Environment2*PCDec2
                                + LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int2)
# no

disComplex.lmerBCPC9Int3<- lmer(bc ~ Environment2*PCDec3
                                + LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int3)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                        6.379e-01  5.660e-03  2.080e+02 112.700  < 2e-16 ***
#   Environment2s#V-stressed          -2.871e-02  4.707e-03  4.020e+01  -6.100 3.35e-07 ***
# Environment2s#V-unstressed        -3.272e-02  3.628e-03  4.130e+01  -9.016 2.61e-11 ***
# PCDec3                            -1.538e-03  1.915e-03  6.598e+02  -0.803   0.4224    
# LocSpeech                         -4.387e-03  3.447e-04  8.028e+02 -12.729  < 2e-16 ***
#   Environment2s#V-stressed:PCDec3   -2.961e-04  3.342e-03  4.337e+02  -0.089   0.9294    
# Environment2s#V-unstressed:PCDec3  7.471e-03  3.207e-03  7.776e+02   2.330   0.0201 * 

disComplex.lmerBCPC9Int4<- lmer(bc ~ Environment2*PCDec4
                                + LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int4)
# no


# so there are interactions with Pc 1 and Pc3, let's see whether they are better
# than our final model

anova(disComplex.lmerBCPC9Int3,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -4015.9 -3968.9 2017.9  -4035.9                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 14.634      0  < 2.2e-16 ***

anova(disComplex.lmerBCPC9Int1,final_dis_complex_model.lmer)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 10 -4021.1 -3974.1 2020.5  -4041.1                             
# ..1    10 -4030.5 -3983.5 2025.2  -4050.5 9.4391      0  < 2.2e-16 ***

# no, but let's see whether they remain sign. with the other interaction

disComplex.lmerBCPC9Int3b<- lmer(bc ~ Environment2*Accentuation+Environment2*PCDec3
                                + LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int3b)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.403e-01  6.125e-03  2.495e+02 104.539  < 2e-16 ***
#   Environment2s#V-stressed                          -3.167e-02  4.948e-03  5.380e+01  -6.400 3.93e-08 ***
# Environment2s#V-unstressed                        -3.911e-02  3.836e-03  5.650e+01 -10.194 2.07e-14 ***
# Accentuationunaccented                            -1.002e-02  2.571e-03  7.623e+02  -3.897 0.000106 ***
#   PCDec3                                            -1.526e-03  1.893e-03  6.498e+02  -0.806 0.420539    
# LocSpeech                                         -4.171e-03  4.145e-04  7.678e+02 -10.062  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    5.615e-03  3.900e-03  7.449e+02   1.440 0.150280    
# Environment2s#V-unstressed:Accentuationunaccented  1.293e-02  3.076e-03  7.468e+02   4.202 2.97e-05 ***
# Environment2s#V-stressed:PCDec3                   -6.089e-04  3.299e-03  4.191e+02  -0.185 0.853678    
# Environment2s#V-unstressed:PCDec3                  7.096e-03  3.175e-03  7.709e+02   2.235 0.025708 *  



disComplex.lmerBCPC9Int1b<- lmer(bc ~ Environment2*Accentuation+Environment2*PCDec1
                                + LocSpeech + 
                                  (1|Item) + (1|Participant), data = disComplex2)

summary(disComplex.lmerBCPC9Int1b)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.443e-01  6.234e-03  2.384e+02 103.362  < 2e-16 ***
#   Environment2s#V-stressed                          -3.571e-02  5.054e-03  5.420e+01  -7.066 3.17e-09 ***
# Environment2s#V-unstressed                        -3.780e-02  4.687e-03  8.430e+01  -8.063 4.46e-12 ***
# Accentuationunaccented                            -9.887e-03  2.568e-03  7.635e+02  -3.850 0.000128 ***
#   PCDec1                                             2.541e-03  1.055e-03  5.780e+01   2.408 0.019270 *  
#   LocSpeech                                         -4.184e-03  4.121e-04  7.559e+02 -10.155  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    5.640e-03  3.893e-03  7.483e+02   1.449 0.147808    
# Environment2s#V-unstressed:Accentuationunaccented  1.285e-02  3.074e-03  7.490e+02   4.182 3.23e-05 ***
# Environment2s#V-stressed:PCDec1                   -1.311e-03  3.395e-03  1.360e+02  -0.386 0.700068    
# Environment2s#V-unstressed:PCDec1                 -9.659e-03  3.536e-03  4.483e+02  -2.732 0.006553 ** 


# let's have a look at the effect

visreg(disComplex.lmerBCPC9Int1b, "PCDec1", by="Environment2")
visreg(disComplex.lmerBCPC9Int3b, "PCDec3", by="Environment2")

# okay, I will discard these interactions. The distributions are ridiculous

# so, the PC results in exactly the same model as the other analysis!

library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the disportance of the
# different factors.

options(na.action = "na.fail") 

disComplex.lm<- lmer(bc ~ Environment2*Accentuation+ OrderRescale+PrePause*Accentuation+PostPause+
                     + LocSpeech + GlobalSpeechRate+ logWordFormFreq+
                       PrecSegDur +Environment2*PCDec1+Environment2*PCDec2+Environment2*PCDec3+
                       (1|Item) + (1|Participant), data = disComplex2)


model_ranking <- dredge(disComplex.lm)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Relative variable importance: 
#   LocSpeech Environment2 PrecSegDur GlobalSpeechRate PrePause Accentuation PCDec1 PostPause PCDec2
# Importance:              1         1         0.06       0.01             0.01    <0.01        <0.01  <0.01     <0.01 
# N containing models:  8192     13824         8192       8192            10176    11904        10496   8192     10496 
# PCDec3 logWordFormFreq Accentuation:Environment2 OrderRescale Environment2:PCDec1
# Importance:          <0.01  <0.01           <0.01                     <0.01        <0.01              
# N containing models: 10496   8192            5184                      8192         4608              
# Environment2:PCDec3 Accentuation:PrePause Environment2:PCDec2
# Importance:          <0.01               <0.01                 <0.01              
# N containing models:  4608                3968                  4608  


###################################################################################
# Find out at which levels visreg draws lines
###################################################################################

summary(final_dis_complex_model.lmer)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                        6.407e-01  6.089e-03  2.518e+02 105.233  < 2e-16 ***
#   Environment2s#V-stressed                          -3.181e-02  4.914e-03  5.500e+01  -6.472 2.75e-08 ***
# Environment2s#V-unstressed                        -3.904e-02  3.809e-03  5.770e+01 -10.250 1.29e-14 ***
# Accentuationunaccented                            -1.004e-02  2.576e-03  7.652e+02  -3.898 0.000105 ***
#   LocSpeech                                         -4.196e-03  4.132e-04  7.670e+02 -10.153  < 2e-16 ***
#   Environment2s#V-stressed:Accentuationunaccented    5.908e-03  3.903e-03  7.480e+02   1.514 0.130542    
# Environment2s#V-unstressed:Accentuationunaccented  1.323e-02  3.082e-03  7.495e+02   4.294 1.98e-05 ***
# ---

visreg(final_dis_complex_model.lmer)


# Conditions used in construction of plot
intercept =  6.407e-01 

LocCondition= 12.20618
estSpeech= -4.196e-03

# Environment2: s#V-unstressed
EstEnvironment2disV.unstr= -3.904e-02
EstEnvironment2disV.str= -3.181e-02


# Accentuation: Accented
EstAccent.unaccented= -1.004e-02

EstInterdisVunstr.unac=1.323e-02
EstInterdisVstr.unac= 5.908e-03
  

visreg(final_dis_complex_model.lmer, "Accentuation", by="Environment2",overlay=T, 
       trans= function(x) x^(1/lambda)*1000,ylim=ylim)


#Accented levels

#level disV-unstr 

((intercept+(LocCondition*estSpeech)+EstEnvironment2disV.unstr)^(1/lambda))*1000
#[1] 102.9703

#level disV-str 

((intercept+(LocCondition*estSpeech)+EstEnvironment2disV.str)^(1/lambda))*1000
#[1] 108.2159

# diss-str
((intercept+(LocCondition*estSpeech))^(1/lambda))*1000
#[1] 133.6672

disV.unstr.accented= 102.9703
disV.str.accented= 108.2159
diss.str.accented= 133.6672

#Unaccented levels

#level disV-unstr 

((intercept+(LocCondition*estSpeech)+EstEnvironment2disV.unstr+EstInterdisVunstr.unac+
    EstAccent.unaccented)^(1/lambda))*1000
#[1] 105.2611

#level disV-str 

((intercept+(LocCondition*estSpeech)+EstEnvironment2disV.str+EstInterdisVstr.unac+
    EstAccent.unaccented)^(1/lambda))*1000
#[1] 105.1945


# diss-str
((intercept+(LocCondition*estSpeech+EstAccent.unaccented))^(1/lambda))*1000
#[1] 125.2038

disV.unstr.unaccented= 105.2611
disV.str.unaccented= 105.1945
diss.str.unaccented= 125.2038



# Unterschiede:

# Double single accented

diss.str.accented- disV.str.accented
#25.4513

diss.str.accented- disV.unstr.accented
#30.6969

# Double single unaccented

diss.str.unaccented- disV.str.unaccented
#20.0093

diss.str.unaccented- disV.unstr.unaccented
#19.9427