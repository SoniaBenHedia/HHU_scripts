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


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/Analyses/Analyses un and in/")



UnInComplete <- read.csv("un_in_experimental_data_complete.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

# I only want complex in and un

levels(UnInComplete$Environment)
# [1] "imBase" "imm"    "imp"    "inBase" "inn"    "inT"    "inV"    "n#n"    "n#t"    "unBase" "unn"   
# [12] "unV" 

UnInComplex<-UnInComplete[UnInComplete$Environment=="inn"|
                                 UnInComplete$Environment=="inT"|
                                 UnInComplete$Environment=="inV"|
                                 UnInComplete$Environment=="n#n"|
                                 UnInComplete$Environment=="n#t"|
                                 UnInComplete$Environment=="unn"|
                                 UnInComplete$Environment=="unV", ]


UnInComplex$Environment<-droplevels(UnInComplex$Environment)

levels(UnInComplex$Environment)
#[1] "inn" "inT" "inV" "n#n" "n#t" "unn" "unV"


levels(UnInComplex$Environment)<- c("n#nV", "n#C", "n#V", "n#nV", "n#C", "n#nV", "n#V")

 levels(UnInComplex$Environment)
# [1] "n#n" "n#C" "n#V"

 table(UnInComplex$Affix,UnInComplex$Environment)

#  n#nV n#C n#V
#  Loc   19 186  70
#  Neg   70 256 563
#  un   970 427 676
 
str(UnInComplex)

# 'data.frame':	3237 obs. of  77 variables:
#   $ X                          : int  1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 ...
# $ Item                       : Factor w/ 206 levels "immaculate","immaterial",..: 48 48 48 48 48 48 48 48 48 48 ...
# $ Participant                : Factor w/ 51 levels "Experiment_1_participant_10",..: 20 24 22 4 27 10 25 12 21 18 ...
# $ ID                         : int  3201 4144 4340 651 5011 1773 4506 2030 3556 2989 ...
# $ Filename                   : Factor w/ 5504 levels "participant_1_A_10.TextGrid",..: 3355 4086 3715 811 4869 1813 4249 2306 3684 3159 ...
# $ DeletionMorph              : Factor w/ 3 levels "L","N","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ DeviantPronun              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ Accentuation               : Factor w/ 3 levels "Accented","Unaccented",..: 2 1 2 2 1 1 1 1 1 1 ...
# $ Annotator                  : Factor w/ 6 levels "Lara","Mandy",..: 1 2 6 6 5 1 5 3 3 2 ...
# $ Order                      : int  10 40 60 157 258 165 34 284 303 286 ...
# $ WordDur                    : num  0.418 0.763 0.329 0.418 0.617 ...
# $ SyllNum                    : int  2 2 2 2 2 2 2 2 2 2 ...
# $ SegNum                     : int  6 5 5 5 5 5 5 5 5 5 ...
# $ ConsonantDur               : num  0.0517 0.0656 0.0568 0.0439 0.0615 ...
# $ PrecSeg                    : Factor w/ 15 levels "?","@","{","2",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ PrecSegVC                  : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur                 : num  0.0561 0.0663 0.0449 0.0378 0.1358 ...
# $ FollSeg                    : Factor w/ 85 levels "?","@","@e","@i",..: 8 8 8 8 14 8 8 8 8 8 ...
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
# $ Environment                : Factor w/ 3 levels "n#n","n#C","n#V": 3 3 3 3 3 3 3 3 3 3 ...
# $ Affix                      : Factor w/ 3 levels "Loc","Neg","un": 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFrequencyBNC       : int  2 2 2 2 2 2 2 2 2 2 ...
# $ WordFormFrequencyAllCOCA   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ WordFormFrequencySpokenCOCA: int  0 0 0 0 0 0 0 0 0 0 ...
# $ Base                       : Factor w/ 156 levels "able","acquainted",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ WordLemmaFrequencyBNC      : int  2 2 2 2 2 2 2 2 2 2 ...
# $ BaseLemmaFrequencyBNC      : int  38099 38099 38099 38099 38099 38099 38099 38099 38099 38099 ...
# $ SyllPhon                   : int  2 2 2 2 2 2 2 2 2 2 ...
# $ AffixStress                : Factor w/ 5 levels "debatable","primary",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ BaseInitialStress          : Factor w/ 3 levels "primary","Problem",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ SemanticTransparency       : Factor w/ 2 levels "opaque","transparent": 2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfRoot                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...
# $ Rating                     : int  2 2 4 4 1 1 4 4 1 2 ...
# $ TimeRating                 : num  758 610 1315 1011 692 ...
# $ TotalTime                  : num  650 528 1129 826 605 ...
# $ Age                        : int  19 18 19 35 32 19 29 24 61 19 ...
# $ Sex                        : Factor w/ 6 levels "female","Female",..: 2 2 4 4 5 4 6 1 2 5 ...
# $ L1                         : Factor w/ 10 levels "british","British",..: 5 5 5 2 5 2 3 6 2 5 ...
# $ Bilingual                  : Factor w/ 6 levels "I only know British English",..: 4 4 3 3 4 6 2 3 3 4 ...
# $ Grow_Up_Region             : Factor w/ 42 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 19 30 28 39 16 5 36 42 39 7 ...
# $ Languages                  : Factor w/ 30 levels "Basic French",..: 5 1 16 24 30 6 3 22 10 25 ...
# $ Latin                      : Factor w/ 19 levels "2 years secondary school",..: 7 13 6 14 7 4 5 6 6 7 ...
# $ Profession_Studies         : Factor w/ 50 levels "2nd Year Meida Studies",..: 48 41 17 30 5 25 7 33 44 11 ...
# $ University                 : Factor w/ 18 levels "Aberdeen University",..: 18 12 7 10 3 3 4 3 15 7 ...
# $ Knowledge_English_Ling     : Factor w/ 22 levels "2 years","Currently in my 2nd year of the course at university",..: 6 6 20 5 6 7 7 6 11 6 ...
# $ Phonetics                  : Factor w/ 16 levels "A couple of lectures",..: 8 8 13 8 8 8 9 8 13 8 ...
# $ Phonology                  : Factor w/ 14 levels "A couple of lectures",..: 7 7 12 7 7 7 8 7 12 6 ...
# $ Morphology                 : Factor w/ 12 levels "currently studying",..: 5 5 10 5 5 5 6 5 10 5 ...
# $ Semantics                  : Factor w/ 12 levels "currently studying",..: 5 5 10 5 5 5 6 5 10 5 ...
# $ AccentuationCondition      : Factor w/ 2 levels "accented","unaccented": 2 1 2 2 1 1 1 1 2 1 ...
# $ Experiment                 : Factor w/ 2 levels "Experiment_1",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ logWordFormFreq            : num  0.693 0.693 0.693 0.693 0.693 ...
# $ logBaseLemmaFreq           : num  10.5 10.5 10.5 10.5 10.5 ...
# $ logWordLemmaFreq           : num  0.693 0.693 0.693 0.693 0.693 ...
# $ RelFreq                    : num  0.0000525 0.0000525 0.0000525 0.0000525 0.0000525 ...
# $ logRelFreq                 : num  -9.85 -9.85 -9.85 -9.85 -9.85 ...
# $ Root                       : Factor w/ 43 levels "knit","known",..: NA NA NA NA NA NA NA NA NA NA ...
# $ BaseFinalStress            : logi  NA NA NA NA NA NA ...
# $ SuffixAdjSuffix            : logi  NA NA NA NA NA NA ...
# $ LastSyllDur                : num  NA NA NA NA NA NA NA NA NA NA ...
# $ InCorpus                   : Factor w/ 2 levels "no","yes": NA NA NA NA NA NA NA NA NA NA ...
# $ Consonant                  : Factor w/ 5 levels "b","l","n","O",..: NA NA NA NA NA NA NA NA NA NA ...
# $ Orthography                : Factor w/ 6 levels "kn","m","mm",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ median                     : int  2 2 2 2 2 2 2 2 2 2 ...
# $ TypeOfBase                 : Factor w/ 2 levels "bound","word": 2 2 2 2 2 2 2 2 2 2 ...

UnInComplex$X.1<-NULL
UnInComplex$X<-NULL

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
# - Affix

######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Let's see how much of the variability can solely be explained by speaker and item

SpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Participant), data = UnInComplex)
cor(UnInComplex$ConsonantDur, fitted(SpeakerComplex.lmer))^2
#[1] 0.07794004


ItemComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item), data = UnInComplex)
cor(UnInComplex$ConsonantDur, fitted(ItemComplex.lmer))^2
#[1] 0.7139729

ItemSpeakerComplex.lmer <- lmer(ConsonantDur ~ 1 + (1|Item) + (1|Participant), data = UnInComplex)
cor(UnInComplex$ConsonantDur, fitted(ItemSpeakerComplex.lmer))^2
#0.7688462

# so around 77 percent of the variability can be explained by this! 

# before we do an initial model, let's set the reference level right

UnInComplex$Environment <- relevel (UnInComplex$Environment, ref= "n#nV")

# okay, somehow I seem to not have coded for global speech rate

# I will do so here


GlobalSpeech<-c()

j=1
for (e in UnInComplex$AccentuationCondition){
  if (e=="accented") {GlobalSpeech= append(GlobalSpeech,as.numeric (4/UnInComplex$SentenceDur[j]))}
  else { GlobalSpeech= append(GlobalSpeech,as.numeric(9/UnInComplex$SentenceDur[j]))}
  j=j+1
}


UnInComplex$GlobalSpeechRate=GlobalSpeech


# and I need to code for PrePause and Post Pause


# we create an empty vector
Pauselist<-c()

# for each observation which has a duration other than zero, we add Pause
# to the strIn2g, for each which has a dration of 0 we ass no Pause

for (i in UnInComplex$PrePauseDur){
  if(i!=0) {Pauselist= append(Pauselist,"Pause")}
  else { Pauselist=append(Pauselist,"No Pause")}
}

# now let's create a new variable out of this vector

UnInComplex$PrePause<-as.factor(Pauselist)



# we create an empty vector
Pauselist<-c()

# for each observation which has a duration other than zero, we add Pause
# to the strIn2g, for each which has a dration of 0 we ass no Pause

for (i in UnInComplex$PostPauseDur){
  if(i!=0) {Pauselist= append(Pauselist,"Pause")}
  else { Pauselist=append(Pauselist,"No Pause")}
}

# now let's create a new variable out of this vector

UnInComplex$PostPause<-as.factor(Pauselist)

UnInComplex[is.na(UnInComplex$logWordFormFreq),"Item"]
#unneighbourly

# need to add that

UnInComplex[UnInComplex$Item=="unneighbourly","logWordFormFreq"]<-1.94591

##########################################################
##              Do an initial model:
UnInComplex$OrderRescale<-UnInComplex$Order*0.1

UnInComplex.lmer1 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                           BaseInitialStress + LocSpeech + GlobalSpeechRate +
                           PrePause+ PostPause +Affix+ 
                          (1|Item) + (1|Participant), data = UnInComplex)


summary(UnInComplex.lmer1)    

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.17109989    0.00600079  197.00000000  28.513 < 0.0000000000000002 ***
#   Environmentn#C                    -0.03562636    0.00334608  112.00000000 -10.647 < 0.0000000000000002 ***
# Environmentn#V                    -0.07608021    0.00350453  111.00000000 -21.709 < 0.0000000000000002 ***
# AccentuationConditionunaccented   -0.00226209    0.00100407 3163.00000000  -2.253               0.0243 *  
#   OrderRescale                      -0.00004345    0.00004220 3117.00000000  -1.030               0.3032    
# logWordFormFreq                   -0.00041465    0.00048750  112.00000000  -0.851               0.3968    
# BaseInitialStressunstressed       -0.00331442    0.00310769  111.00000000  -1.067               0.2885    
# LocSpeech                         -0.00413086    0.00025486 3132.00000000 -16.208 < 0.0000000000000002 ***
#   GlobalSpeechRate                   0.00010046    0.00013048 3096.00000000   0.770               0.4414    
# PrePausePause                      0.00527454    0.00094840 3136.00000000   5.561          0.000000029 ***
#   PostPausePause                     0.00149571    0.00108020 3162.00000000   1.385               0.1663    
# AffixNeg                           0.00229108    0.00446601  117.00000000   0.513               0.6089    
# Affixun                            0.00668910    0.00440444  120.00000000   1.519               0.1315  

#######################################################################################
# Dealing with collinearity                                                           #
######################################################################################

# Before slInming down the model we should deal with possible collinearity problems


# I will do so, by looking at what happens if both varables stay in a model, what happens
# if one is thrown out and also which effect each one has on its own




# 1.  Loc Speech  and/or Global Speech


cor.test(UnInComplex$LocSpeech,UnInComplex$GlobalSpeechRate)

# Pearson's product-moment correlation
# 
# data:  UnInComplex$LocSpeech and UnInComplex$GlobalSpeechRate
# t = 5.3854, df = 3235, p-value = 0.00000007744
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.0600073 0.1282985
# sample estimates:
# cor 
# 0.09426377 

UnInComplex.lmerSpeechRates<- lmer(ConsonantDur ~ LocSpeech+ GlobalSpeechRate + (1|Item)+(1|Participant), data = UnInComplex)

summary(UnInComplex.lmerSpeechRates)

# Fixed effects:
#   Estimate    Std. Error            df t value            Pr(>|t|)    
# (Intercept)         0.14121434    0.00403970  331.60000000  34.957 <0.0000000000000002 ***
#   LocSpeech          -0.00511266    0.00020878 3144.10000000 -24.488 <0.0000000000000002 ***
#   GlobalSpeechRate    0.00006332    0.00013133 3089.40000000   0.482                0.63   

cor(UnInComplex$ConsonantDur, fitted(UnInComplex.lmerSpeechRates))^2
#[1]  0.8027324



UnInComplex.lmerLocSpeech<- lmer(ConsonantDur ~ LocSpeech + (1|Item)+(1|Participant), data = UnInComplex)

summary(UnInComplex.lmerLocSpeech)

# Fixed effects:
# Estimate   Std. Error           df t value            Pr(>|t|)    
# (Intercept)    0.1412435    0.0040397  331.2000000   34.96 <0.0000000000000002 ***
#   LocSpeech     -0.0051046    0.0002081 3142.2000000  -24.53 <0.0000000000000002 ***

cor(UnInComplex$ConsonantDur, fitted(UnInComplex.lmerLocSpeech))^2
#[1] 0.8027227



UnInComplex.lmerGlobalSpeech<- lmer(ConsonantDur ~ GlobalSpeechRate + (1|Item)+(1|Participant), data = UnInComplex)

print(summary(UnInComplex.lmerGlobalSpeech),digits=3)

# Fixed effects:
#   Estimate  Std. Error          df t value            Pr(>|t|)    
# (Intercept)         0.082745    0.003798  156.000000    21.8 <0.0000000000000002 ***
#   GlobalSpeechRate   -0.000185    0.000142 3087.000000    -1.3                0.19  

cor(UnInComplex$ConsonantDur, fitted(UnInComplex.lmerGlobalSpeech))^2
#[1] 0.7689674


#####################################
# Summary Coll. Speech Rates:
# - There is a supression effect, Glbal Speech Rate changes its effect direction
# when LocSpeech is in - since LocSpeech is the better predictor anyways we will 
# just take that
#################################################


##########################################################
##              refit the model


UnInComplex.lmer2 <- lmer(ConsonantDur ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + 
                            PrePause+ PostPause + Affix+
                            (1|Item) + (1|Participant), data = UnInComplex)


summary(UnInComplex.lmer2)    

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.17123303    0.00599867  197.00000000  28.545 < 0.0000000000000002 ***
#   Environmentn#C                    -0.03564379    0.00334633  112.00000000 -10.652 < 0.0000000000000002 ***
# Environmentn#V                    -0.07609398    0.00350482  111.00000000 -21.711 < 0.0000000000000002 ***
# AccentuationConditionunaccented   -0.00221054    0.00100176 3165.00000000  -2.207               0.0274 *  
#   OrderRescale                      -0.00004302    0.00004219 3117.00000000  -1.020               0.3079    
# logWordFormFreq                   -0.00041413    0.00048755  112.00000000  -0.849               0.3975    
# BaseInitialStressunstressed       -0.00332887    0.00310793  111.00000000  -1.071               0.2864    
# LocSpeech                         -0.00412615    0.00025477 3132.00000000 -16.195 < 0.0000000000000002 ***
#   PrePausePause                      0.00527051    0.00094833 3137.00000000   5.558         0.0000000296 ***
#   PostPausePause                     0.00149558    0.00108013 3163.00000000   1.385               0.1663    
# AffixNeg                           0.00226874    0.00446632  117.00000000   0.508               0.6124    
# Affixun                            0.00668582    0.00440484  120.00000000   1.518               0.1317   


###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (UnInComplex.lmer2))
qqline (residuals (UnInComplex.lmer2))

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


UnInComplex.lm<-lm(ConsonantDur ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                    BaseInitialStress + LocSpeech + Affix +
                    PrePause + PostPause, data = UnInComplex)

summary(UnInComplex.lm)
# 
# Coefficients:
#   Estimate  Std. Error t value             Pr(>|t|)    
# (Intercept)                      0.18411220  0.00325284  56.600 < 0.0000000000000002 ***
#   Environmentn#C                  -0.03915894  0.00129991 -30.124 < 0.0000000000000002 ***
# Environmentn#V                  -0.07723045  0.00133932 -57.664 < 0.0000000000000002 ***
# AccentuationConditionunaccented -0.00196167  0.00109726  -1.788              0.07390 .  
# OrderRescale                    -0.00005887  0.00004849  -1.214              0.22483    
# logWordFormFreq                 -0.00044531  0.00018815  -2.367              0.01800 *  
#   BaseInitialStressunstressed     -0.00326580  0.00124912  -2.614              0.00898 ** 
#   LocSpeech                       -0.00465268  0.00022308 -20.857 < 0.0000000000000002 ***
#   AffixNeg                         0.00133756  0.00191194   0.700              0.48424    
# Affixun                          0.00345212  0.00183984   1.876              0.06070 .  
# PrePausePause                    0.00421328  0.00098858   4.262            0.0000208 ***
#   PostPausePause                   0.00020259  0.00115776   0.175              0.86110  


bc<-boxcox(UnInComplex.lm)

lambda <- bc$x[which.max(bc$y)]
lambda
#[1]  0.06060606

UnInComplex$bc <- UnInComplex$ConsonantDur^lambda

UnInComplex.lmerBC <- lmer(bc ~ Environment+ AccentuationCondition+OrderRescale +logWordFormFreq+
                            BaseInitialStress + LocSpeech + Affix +
                            PrePause + PostPause +(1|Item) + (1|Participant), data = UnInComplex)

summary(UnInComplex.lmerBC)
# 
# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.90417946    0.00319450  189.60000000 283.042 < 0.0000000000000002 ***
#   Environmentn#C                    -0.01543057    0.00179906  111.60000000  -8.577   0.0000000000000635 ***
# Environmentn#V                    -0.04478301    0.00188452  110.90000000 -23.764 < 0.0000000000000002 ***
# AccentuationConditionunaccented   -0.00098229    0.00052004 3161.60000000  -1.889               0.0590 .  
# OrderRescale                      -0.00003356    0.00002190 3117.20000000  -1.532               0.1256    
# logWordFormFreq                   -0.00016676    0.00026214  111.30000000  -0.636               0.5260    
# BaseInitialStressunstressed       -0.00333157    0.00167127  110.50000000  -1.993               0.0487 *  
#   LocSpeech                         -0.00217173    0.00013242 3137.40000000 -16.400 < 0.0000000000000002 ***
#   AffixNeg                          -0.00039530    0.00239956  116.10000000  -0.165               0.8694    
# Affixun                            0.00176238    0.00236547  118.60000000   0.745               0.4577    
# PrePausePause                      0.00246439    0.00049218 3133.20000000   5.007   0.0000005831877645 ***
#   PostPausePause                    -0.00039699    0.00056071 3159.90000000  -0.708               0.4790  

#let's check the assumptions

qqnorm (residuals (UnInComplex.lmerBC))
qqline (residuals (UnInComplex.lmerBC))

# it is better but not that good. Let's remove outliers and see how it looks afterwards

outliers<-romr.fnc(UnInComplex.lmerBC, UnInComplex, trim = 2.5)
# n.removed = 67 
# percent.removed = 2.069818 

UnInComplex2<-outliers$data

dim(UnInComplex2)
#[1] 3170   82

dim(UnInComplex)
#3237   81


# okay it seemes to have worked

UnInComplex.lmerBC2 <- lmer(bc ~ Environment+ AccentuationCondition+ OrderRescale +logWordFormFreq+
                             BaseInitialStress + LocSpeech + Affix +
                             PrePause + PostPause + 
                             (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC2)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
# (Intercept)                        0.90319405    0.00314408  177.50000000 287.268 < 0.0000000000000002 ***
#   Environmentn#C                    -0.01527467    0.00180212  111.00000000  -8.476    0.000000000000111 ***
# Environmentn#V                    -0.04485975    0.00188981  110.80000000 -23.738 < 0.0000000000000002 ***
# AccentuationConditionunaccented   -0.00133792    0.00046954 3086.40000000  -2.849              0.00441 ** 
#   OrderRescale                      -0.00003453    0.00001957 3039.10000000  -1.765              0.07773 .  
# logWordFormFreq                   -0.00018807    0.00026246  111.70000000  -0.717              0.47514    
# BaseInitialStressunstressed       -0.00270790    0.00167562  110.40000000  -1.616              0.10894    
# LocSpeech                         -0.00210957    0.00012057 3133.90000000 -17.497 < 0.0000000000000002 ***
#   AffixNeg                          -0.00048019    0.00239900  114.70000000  -0.200              0.84171    
# Affixun                            0.00250342    0.00236304  116.90000000   1.059              0.29160    
# PrePausePause                      0.00213254    0.00044054 3069.30000000   4.841    0.000001357759081 ***
#   PostPausePause                    -0.00023214    0.00050455 3082.40000000  -0.460              0.64548  

qqnorm (residuals (UnInComplex.lmerBC2))
qqline (residuals (UnInComplex.lmerBC2))

# this looks actually pretty good.


# We will work with this model! --> UnInComplex.lmerBC2



#########################################################################################
#                                                                                       #
#                      SInplification of the model                                      #
#########################################################################################

summary(UnInComplex.lmerBC2)

# Fixed effects:
#                                       Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.90319405    0.00314408  177.50000000 287.268 < 0.0000000000000002 ***
#   Environmentn#C                    -0.01527467    0.00180212  111.00000000  -8.476    0.000000000000111 ***
#   Environmentn#V                    -0.04485975    0.00188981  110.80000000 -23.738 < 0.0000000000000002 ***
#   AccentuationConditionunaccented   -0.00133792    0.00046954 3086.40000000  -2.849              0.00441 ** 
#   OrderRescale                      -0.00003453    0.00001957 3039.10000000  -1.765              0.07773 .  
#   logWordFormFreq                   -0.00018807    0.00026246  111.70000000  -0.717              0.47514    
#   BaseInitialStressunstressed       -0.00270790    0.00167562  110.40000000  -1.616              0.10894    
#   LocSpeech                         -0.00210957    0.00012057 3133.90000000 -17.497 < 0.0000000000000002 ***
#   AffixNeg                          -0.00048019    0.00239900  114.70000000  -0.200              0.84171    
#   Affixun                            0.00250342    0.00236304  116.90000000   1.059              0.29160    
#   PrePausePause                      0.00213254    0.00044054 3069.30000000   4.841    0.000001357759081 ***
#   PostPausePause                    -0.00023214    0.00050455 3082.40000000  -0.460              0.64548 

# let's throw out PostPause

UnInComplex.lmerBC3 <- lmer(bc ~ Environment+  OrderRescale +AccentuationCondition+logWordFormFreq+
                             BaseInitialStress + LocSpeech + Affix +
                             PrePause +
                             (1|Item) + (1|Participant), data = UnInComplex2)
summary(UnInComplex.lmerBC3)

# Fixed effects:
#                                         Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.90289909    0.00307881  164.00000000 293.263 < 0.0000000000000002 ***
#   Environmentn#C                    -0.01529040    0.00180166  111.00000000  -8.487    0.000000000000105 ***
#   Environmentn#V                    -0.04488171    0.00188909  110.70000000 -23.758 < 0.0000000000000002 ***
#   OrderRescale                      -0.00003447    0.00001957 3039.90000000  -1.762              0.07823 .  
#   AccentuationConditionunaccented   -0.00125590    0.00043409 3105.80000000  -2.893              0.00384 ** 
#   logWordFormFreq                   -0.00018678    0.00026243  111.60000000  -0.712              0.47810    
#   BaseInitialStressunstressed       -0.00270954    0.00167549  110.40000000  -1.617              0.10870    
#   LocSpeech                         -0.00209329    0.00011534 3132.30000000 -18.148 < 0.0000000000000002 ***
#   AffixNeg                          -0.00050443    0.00239825  114.60000000  -0.210              0.83378    
#   Affixun                            0.00251392    0.00236277  116.80000000   1.064              0.28954    
#   PrePausePause                      0.00210980    0.00043767 3072.00000000   4.821    0.000001500961660 ***

anova(UnInComplex.lmerBC2,UnInComplex.lmerBC3)

# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    14 -19808 -19723 9917.7   -19836                         
# object 15 -19806 -19715 9917.8   -19836 0.2128      1     0.6446

# model did not become worse


# let's throw out Frequency

UnInComplex.lmerBC4  <- lmer(bc ~ Environment+  OrderRescale +AccentuationCondition+
                               BaseInitialStress + LocSpeech + Affix +
                               PrePause +
                               (1|Item) + (1|Participant), data = UnInComplex2)
summary(UnInComplex.lmerBC4)

# Fixed effects:
#   Estimate    Std. Error            df t value             Pr(>|t|)    
#   (Intercept)                        0.90218538    0.00290387  174.30000000 310.683 < 0.0000000000000002 ***
#   Environmentn#C                    -0.01533125    0.00179579  111.90000000  -8.537   0.0000000000000768 ***
#   Environmentn#V                    -0.04495558    0.00188109  111.70000000 -23.899 < 0.0000000000000002 ***
#   OrderRescale                      -0.00003465    0.00001957 3040.70000000  -1.771              0.07667 .  
#   AccentuationConditionunaccented   -0.00125392    0.00043407 3106.20000000  -2.889              0.00389 ** 
#   BaseInitialStressunstressed       -0.00250960    0.00164720  111.40000000  -1.524              0.13046    
#   LocSpeech                         -0.00209377    0.00011533 3133.30000000 -18.155 < 0.0000000000000002 ***
#   AffixNeg                          -0.00030875    0.00237578  115.50000000  -0.130              0.89683    
#   Affixun                            0.00277260    0.00232816  117.90000000   1.191              0.23609    
#   PrePausePause                      0.00211006    0.00043767 3072.10000000   4.821   0.0000014968436610 ***

anova(UnInComplex.lmerBC3,UnInComplex.lmerBC4)

# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# ..1    13 -19809 -19730 9917.5   -19835                         
# object 14 -19808 -19723 9917.7   -19836 0.5274      1     0.4677

# nothing has changed

# now I will check for interactions, then I will simplify more


###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. I will only look at interactions which
# make sense from a theoretucal point if view


# Affix and  Environment and accentuation and stress and pause



# Let's see

# Environment and stress


UnInComplex.lmerBC4IntEnvStr <- lmer(bc ~ Environment*BaseInitialStress+  OrderRescale +AccentuationCondition+
                                        LocSpeech + Affix +
                                       PrePause +
                                       (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntEnvStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                 9.029e-01  2.814e-03  1.736e+02 320.926  < 2e-16 ***
#   Environmentn#C                             -1.823e-02  1.654e-03  1.102e+02 -11.020  < 2e-16 ***
# Environmentn#V                             -4.638e-02  1.858e-03  1.095e+02 -24.964  < 2e-16 ***
# BaseInitialStressunstressed                -2.396e-02  4.544e-03  1.061e+02  -5.272 7.16e-07 ***
#   OrderRescale                               -3.466e-05  1.955e-05  3.047e+03  -1.772  0.07642 .  
# AccentuationConditionunaccented            -1.319e-03  4.334e-04  3.116e+03  -3.042  0.00237 ** 
#   LocSpeech                                  -2.060e-03  1.148e-04  3.118e+03 -17.942  < 2e-16 ***
#   AffixNeg                                   -1.063e-04  2.267e-03  1.123e+02  -0.047  0.96267    
# Affixun                                     3.401e-03  2.217e-03  1.139e+02   1.534  0.12780    
# PrePausePause                               2.154e-03  4.374e-04  3.078e+03   4.925 8.88e-07 ***
#   Environmentn#C:BaseInitialStressunstressed  3.061e-02  5.203e-03  1.091e+02   5.884 4.48e-08 ***
# Environmentn#V:BaseInitialStressunstressed  2.128e-02  4.895e-03  1.064e+02   4.347 3.17e-05 ***

anova(UnInComplex.lmerBC4IntEnvStr,UnInComplex.lmerBC4)

# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# ..1    13 -19809 -19730 9917.5   -19835                            
# object 15 -19837 -19746 9933.6   -19867 32.27      2   9.83e-08 ***

#YES

# Environment and Acc

UnInComplex.lmerBC4IntEnvAcc <- lmer(bc ~ Environment*AccentuationCondition+BaseInitialStress+  OrderRescale +
                                       LocSpeech + Affix +
                                       PrePause +
                                       (1|Item) + (1|Participant), data = UnInComplex2)


summary(UnInComplex.lmerBC4IntEnvAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     9.049e-01  2.907e-03  1.763e+02 311.287  < 2e-16 ***
#   Environmentn#C                                 -1.774e-02  1.846e-03  1.257e+02  -9.613  < 2e-16 ***
# Environmentn#V                                 -4.926e-02  1.923e-03  1.227e+02 -25.620  < 2e-16 ***
# AccentuationConditionunaccented                -6.043e-03  6.527e-04  3.046e+03  -9.258  < 2e-16 ***
#   BaseInitialStressunstressed                    -2.485e-03  1.645e-03  1.115e+02  -1.511   0.1337    
# OrderRescale                                   -3.962e-05  1.924e-05  3.037e+03  -2.060   0.0395 *  
#   LocSpeech                                      -2.124e-03  1.135e-04  3.137e+03 -18.715  < 2e-16 ***
#   AffixNeg                                       -2.049e-04  2.372e-03  1.156e+02  -0.086   0.9313    
# Affixun                                         2.909e-03  2.324e-03  1.179e+02   1.251   0.2132    
# PrePausePause                                   2.008e-03  4.305e-04  3.070e+03   4.664 3.23e-06 ***
#   Environmentn#C:AccentuationConditionunaccented  5.022e-03  9.013e-04  2.998e+03   5.572 2.75e-08 ***
# Environmentn#V:AccentuationConditionunaccented  8.617e-03  8.209e-04  3.002e+03  10.497  < 2e-16 ***


visreg(UnInComplex.lmerBC4IntEnvAcc, "Environment", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the other one with an interaction


anova(UnInComplex.lmerBC4IntEnvStr,UnInComplex.lmerBC4IntEnvAcc)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -19837 -19746 9933.6   -19867                             
# ..1    15 -19914 -19823 9971.8   -19944 76.308      0  < 2.2e-16 ***

#  it is better


# Environment and PrePause
UnInComplex.lmerBC4IntEnvPause <- lmer(bc ~ Environment*PrePause +
                                         AccentuationCondition+BaseInitialStress+  OrderRescale +
                                         LocSpeech + Affix +
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntEnvPause)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.996e-01  2.927e-03  1.778e+02 307.350  < 2e-16 ***
#   Environmentn#C                  -1.398e-02  1.887e-03  1.348e+02  -7.407 1.27e-11 ***
# Environmentn#V                  -4.095e-02  1.957e-03  1.292e+02 -20.926  < 2e-16 ***
# PrePausePause                    5.328e-03  6.730e-04  3.047e+03   7.916 3.55e-15 ***
#   AccentuationConditionunaccented -1.297e-03  4.300e-04  3.103e+03  -3.016  0.00259 ** 
#   BaseInitialStressunstressed     -2.455e-03  1.653e-03  1.114e+02  -1.485  0.14029    
# OrderRescale                    -2.864e-05  1.939e-05  3.038e+03  -1.477  0.13987    
# LocSpeech                       -2.052e-03  1.146e-04  3.133e+03 -17.907  < 2e-16 ***
#   AffixNeg                        -2.301e-04  2.383e-03  1.155e+02  -0.097  0.92324    
# Affixun                          2.944e-03  2.335e-03  1.178e+02   1.261  0.20989    
# Environmentn#C:PrePausePause    -2.264e-03  9.464e-04  3.023e+03  -2.392  0.01680 *  
# Environmentn#V:PrePausePause    -6.702e-03  8.684e-04  3.020e+03  -7.718 1.60e-14 ***


visreg(UnInComplex.lmerBC4IntEnvPause, "Environment", by="PrePause",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# mk, let's see whether this model is better than the the otehr ones


anova(UnInComplex.lmerBC4IntEnvStr,UnInComplex.lmerBC4IntEnvPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -19837 -19746 9933.6   -19867                             
# ..1    15 -19866 -19775 9948.0   -19896 28.821      0  < 2.2e-16 ***

#yes

# Accentuation and PrePause
UnInComplex.lmerBC4IntAccPause <- lmer(bc ~ Environment+
                                         PrePause*AccentuationCondition+BaseInitialStress+  OrderRescale +
                                         LocSpeech + Affix +
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAccPause)

# no

# Stress and PrePause
UnInComplex.lmerBC4IntPauseStr <- lmer(bc ~ Environment+
                                         PrePause*BaseInitialStress+AccentuationCondition+  OrderRescale +
                                         LocSpeech + Affix +
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntPauseStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                9.009e-01  2.896e-03  1.767e+02 311.088  < 2e-16 ***
#   Environmentn#C                            -1.543e-02  1.785e-03  1.119e+02  -8.646 4.31e-14 ***
# Environmentn#V                            -4.509e-02  1.870e-03  1.117e+02 -24.119  < 2e-16 ***
# PrePausePause                              3.491e-03  4.984e-04  3.071e+03   7.005 3.02e-12 ***
#   BaseInitialStressunstressed                4.539e-04  1.718e-03  1.350e+02   0.264  0.79198    
# AccentuationConditionunaccented           -1.348e-03  4.322e-04  3.105e+03  -3.119  0.00183 ** 
#   OrderRescale                              -3.359e-05  1.947e-05  3.039e+03  -1.725  0.08455 .  
# LocSpeech                                 -2.040e-03  1.151e-04  3.134e+03 -17.713  < 2e-16 ***
#   AffixNeg                                  -4.774e-04  2.361e-03  1.156e+02  -0.202  0.84014    
# Affixun                                    2.762e-03  2.314e-03  1.179e+02   1.194  0.23494    
# PrePausePause:BaseInitialStressunstressed -4.632e-03  8.131e-04  3.024e+03  -5.697 1.34e-08 ***

# yes, lket's see whether it is better or worse than the other models

anova(UnInComplex.lmerBC4IntPauseStr,UnInComplex.lmerBC4IntEnvStr)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 14 -19839 -19755 9933.7   -19867                        
# ..1    15 -19837 -19746 9933.6   -19867     0      1          1

# same

anova(UnInComplex.lmerBC4IntPauseStr,UnInComplex.lmerBC4IntEnvPause)
# object 14 -19839 -19755 9933.7   -19867                             
# ..1    15 -19866 -19775 9948.0   -19896 28.688      1  8.504e-08 ***


# it worse than the Env Pause one


anova(UnInComplex.lmerBC4IntPauseStr,UnInComplex.lmerBC4IntEnvAcc)
# object 14 -19839 -19755 9933.7   -19867                             
# ..1    15 -19914 -19823 9971.8   -19944 76.174      1  < 2.2e-16 ***

# it is worse than the Env Acc one

# Stress and Acc
UnInComplex.lmerBC4IntAccStr <- lmer(bc ~ Environment+
                                       PrePause+BaseInitialStress*AccentuationCondition+  OrderRescale +
                                       LocSpeech + Affix +
                                       (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAccStr)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                  9.031e-01  2.895e-03  1.748e+02 311.984  < 2e-16 ***
#   Environmentn#C                                              -1.537e-02  1.789e-03  1.120e+02  -8.591 5.77e-14 ***
# Environmentn#V                                              -4.495e-02  1.874e-03  1.118e+02 -23.979  < 2e-16 ***
# PrePausePause                                                2.020e-03  4.339e-04  3.071e+03   4.655 3.38e-06 ***
#   BaseInitialStressunstressed                                 -5.404e-03  1.685e-03  1.239e+02  -3.206  0.00171 ** 
#   AccentuationConditionunaccented                             -3.098e-03  4.945e-04  3.088e+03  -6.264 4.26e-10 ***
#   OrderRescale                                                -3.832e-05  1.939e-05  3.039e+03  -1.976  0.04829 *  
#   LocSpeech                                                   -2.089e-03  1.143e-04  3.135e+03 -18.270  < 2e-16 ***
#   AffixNeg                                                    -3.013e-04  2.367e-03  1.156e+02  -0.127  0.89894    
# Affixun                                                      2.813e-03  2.320e-03  1.179e+02   1.213  0.22764    
# BaseInitialStressunstressed:AccentuationConditionunaccented  5.733e-03  7.588e-04  3.006e+03   7.555 5.51e-14 *

visreg(UnInComplex.lmerBC4IntAccStr, "BaseInitialStress", by="AccentuationCondition",
       overlay=T,trans= function(x) (x^(1/lambda)*1000))

# let's see whether its is better or worse than the other models

anova(UnInComplex.lmerBC4IntAccStr,UnInComplex.lmerBC4IntEnvPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 14 -19864 -19779 9945.8   -19892                           
# ..1    15 -19866 -19775 9948.0   -19896 4.4063      1    0.03581 *

# the other is better


anova(UnInComplex.lmerBC4IntAccStr,UnInComplex.lmerBC4IntEnvAcc)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 14 -19864 -19779 9945.8   -19892                             
# ..1    15 -19914 -19823 9971.8   -19944 51.893      1  5.861e-13 ***

# the other is better

anova(UnInComplex.lmerBC4IntAccStr,UnInComplex.lmerBC4IntEnvStr)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 14 -19864 -19779 9945.8   -19892                        
# ..1    15 -19837 -19746 9933.6   -19867     0      1          1

# same


anova(UnInComplex.lmerBC4IntAccStr,UnInComplex.lmerBC4IntAccStr)
# object 14 -19864 -19779 9945.8   -19892                        
# ..1    14 -19864 -19779 9945.8   -19892     0      0          1

# same

# Affix and Env
UnInComplex.lmerBC4IntAffixEnv <- lmer(bc ~ Affix*Environment+
                                       BaseInitialStress+PrePause+AccentuationCondition+  OrderRescale +
                                       LocSpeech + 
                                       (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixEnv)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.619e-01  4.794e-03  1.500e+02 179.795  < 2e-16 ***
#   AffixNeg                         2.043e-02  5.222e-03  1.241e+02   3.912 0.000150 ***
#   Affixun                          4.643e-02  4.669e-03  1.270e+02   9.946  < 2e-16 ***
#   Environmentn#C                   2.633e-02  4.749e-03  1.269e+02   5.544 1.64e-07 ***
# Environmentn#V                   4.018e-04  5.156e-03  1.242e+02   0.078 0.938022    
# BaseInitialStressunstressed     -1.714e-03  1.079e-03  1.084e+02  -1.588 0.115098    
# PrePausePause                    2.171e-03  4.362e-04  3.104e+03   4.978 6.76e-07 ***
#   AccentuationConditionunaccented -1.384e-03  4.302e-04  3.147e+03  -3.217 0.001311 ** 
#   OrderRescale                    -3.345e-05  1.951e-05  3.070e+03  -1.715 0.086505 .  
# LocSpeech                       -2.022e-03  1.120e-04  2.841e+03 -18.047  < 2e-16 ***
#   AffixNeg:Environmentn#C         -2.093e-02  5.537e-03  1.225e+02  -3.779 0.000244 ***
# Affixun:Environmentn#C          -4.788e-02  4.926e-03  1.262e+02  -9.720  < 2e-16 ***
# AffixNeg:Environmentn#V         -2.603e-02  5.955e-03  1.205e+02  -4.371 2.64e-05 ***
# Affixun:Environmentn#V          -5.189e-02  5.389e-03  1.224e+02  -9.630  < 2e-16 ***



# let's see whether its is better or worse than the other models

anova(UnInComplex.lmerBC4IntAffixEnv,UnInComplex.lmerBC4IntEnvPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    15 -19866 -19775 9948.0   -19896                             
# object 17 -19917 -19814 9975.7   -19951 55.319      2   9.72e-13 ***

# this one is better


# Affix and Stress
UnInComplex.lmerBC4IntAffixStr <- lmer(bc ~ Affix*BaseInitialStress+Environment+
                                         PrePause+AccentuationCondition+  OrderRescale +
                                         LocSpeech + 
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixStr)
#no

# Affix and Acc
UnInComplex.lmerBC4IntAffixAcc <- lmer(bc ~ Affix*AccentuationCondition+BaseInitialStress+Environment+
                                         PrePause+  OrderRescale +
                                         LocSpeech + 
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixAcc)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                               9.022e-01  2.957e-03  1.879e+02 305.071  < 2e-16 ***
#   AffixNeg                                 -1.724e-03  2.480e-03  1.373e+02  -0.695   0.4880    
#   Affixun                                   3.180e-03  2.418e-03  1.374e+02   1.315   0.1907    
#   AccentuationConditionunaccented          -1.533e-03  1.246e-03  3.022e+03  -1.231   0.2185    
#   BaseInitialStressunstressed              -2.526e-03  1.646e-03  1.114e+02  -1.534   0.1278    
#   Environmentn#C                           -1.533e-02  1.795e-03  1.119e+02  -8.543 7.42e-14 ***
#   Environmentn#V                           -4.500e-02  1.880e-03  1.117e+02 -23.935  < 2e-16 ***
#   PrePausePause                             2.029e-03  4.368e-04  3.070e+03   4.646 3.53e-06 ***
#   OrderRescale                             -3.383e-05  1.951e-05  3.038e+03  -1.734   0.0830 .  
#   LocSpeech                                -2.076e-03  1.151e-04  3.133e+03 -18.033  < 2e-16 ***
#   AffixNeg:AccentuationConditionunaccented  2.759e-03  1.395e-03  3.005e+03   1.978   0.0481 *  
#   Affixun:AccentuationConditionunaccented  -8.395e-04  1.294e-03  3.003e+03  -0.649   0.5165    


anova(UnInComplex.lmerBC4IntAffixEnv,UnInComplex.lmerBC4IntAffixAcc)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    15 -19825 -19734 9927.3   -19855                             
# object 17 -19917 -19814 9975.7   -19951 96.642      2  < 2.2e-16 ***

# (but other one is better)


anova(UnInComplex.lmerBC4IntAffixAcc,UnInComplex.lmerBC4IntEnvPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -19825 -19734 9927.3   -19855                             
# ..1    15 -19866 -19775 9948.0   -19896 41.324      0  < 2.2e-16 ***

# (but other one is better)

anova(UnInComplex.lmerBC4IntAffixAcc,UnInComplex.lmerBC4IntEnvAcc)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# object 15 -19825 -19734 9927.3   -19855                            
# ..1    15 -19914 -19823 9971.8   -19944 88.81      0  < 2.2e-16 ***

# (but other one is better)

anova(UnInComplex.lmerBC4IntAffixAcc,UnInComplex.lmerBC4IntEnvStr)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 -19825 -19734 9927.3   -19855                             
# ..1    15 -19837 -19746 9933.6   -19867 12.503      0  < 2.2e-16 ***

# (but other one is better)

anova(UnInComplex.lmerBC4IntAccStr,UnInComplex.lmerBC4IntAffixAcc)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# object 14 -19864 -19779 9945.8   -19892                        
# ..1    15 -19825 -19734 9927.3   -19855     0      1          1


# they are the same!
# Affix and Pause
UnInComplex.lmerBC4IntAffixPause<- lmer(bc ~ Affix*PrePause+AccentuationCondition+BaseInitialStress+Environment+
                                           OrderRescale +
                                         LocSpeech + 
                                         (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixPause)
# no

############
# Summary 2 way interactions:

# so, we have a number of possible interactiosn

# 1. Affix and Environment
# 2. Environment and Pause
# 3. Environment and Acc
# 4. Env* Stress, Stress*Acc, Stress*Pause, Affix*Pause

# Let's try out combinations and 3 way interactions

###########################
# 3 way interactions

# 1. Affix, Env and Pause
UnInComplex.lmerBC4IntAffixEnvPause<- lmer(bc ~ Affix*Environment*PrePause+AccentuationCondition+BaseInitialStress+
                                          OrderRescale +
                                          LocSpeech + 
                                          (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixEnvPause)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                            8.737e-01  7.022e-03  6.497e+02 124.423  < 2e-16 ***
#   AffixNeg                               9.082e-03  7.440e-03  4.954e+02   1.221 0.222765    
# Affixun                                3.168e-02  6.952e-03  5.931e+02   4.557 6.32e-06 ***
#   Environmentn#C                         1.344e-02  7.069e-03  5.920e+02   1.900 0.057856 .  
# Environmentn#V                        -1.259e-02  7.453e-03  5.221e+02  -1.689 0.091814 .  
# PrePausePause                         -1.315e-02  6.267e-03  2.993e+03  -2.099 0.035936 *  
#   AccentuationConditionunaccented       -1.434e-03  4.257e-04  3.139e+03  -3.368 0.000767 ***
#   BaseInitialStressunstressed           -1.647e-03  1.075e-03  1.087e+02  -1.532 0.128330    
# OrderRescale                          -2.760e-05  1.930e-05  3.062e+03  -1.430 0.152869    
# LocSpeech                             -1.966e-03  1.114e-04  2.843e+03 -17.645  < 2e-16 ***
#   AffixNeg:Environmentn#C               -1.025e-02  7.772e-03  4.636e+02  -1.319 0.187662    
# Affixun:Environmentn#C                -3.316e-02  7.222e-03  5.575e+02  -4.591 5.45e-06 ***
# AffixNeg:Environmentn#V               -1.173e-02  8.156e-03  4.167e+02  -1.438 0.151226    
# Affixun:Environmentn#V                -3.465e-02  7.642e-03  4.809e+02  -4.534 7.32e-06 ***
# AffixNeg:PrePausePause                 1.298e-02  6.711e-03  2.993e+03   1.934 0.053252 .  
# Affixun:PrePausePause                  1.922e-02  6.299e-03  2.993e+03   3.051 0.002302 ** 
#   Environmentn#C:PrePausePause           1.578e-02  6.455e-03  2.997e+03   2.445 0.014551 *  
# Environmentn#V:PrePausePause           1.589e-02  6.748e-03  2.996e+03   2.355 0.018588 *  
# AffixNeg:Environmentn#C:PrePausePause -1.200e-02  7.005e-03  2.998e+03  -1.713 0.086800 .  
# Affixun:Environmentn#C:PrePausePause  -1.881e-02  6.565e-03  3.000e+03  -2.866 0.004187 ** 
# AffixNeg:Environmentn#V:PrePausePause -1.773e-02  7.224e-03  2.997e+03  -2.455 0.014164 *  
# Affixun:Environmentn#V:PrePausePause  -2.323e-02  6.830e-03  2.997e+03  -3.402 0.000679 ***

anova(UnInComplex.lmerBC4IntAffixEnv,UnInComplex.lmerBC4IntAffixEnvPause)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 17 -19917 -19814  9975.7   -19951                             
# ..1    25 -19983 -19831 10016.4   -20033 81.436      8  2.512e-14 ***

# okay this one is way better

# 2. Affix, Env and Acc
UnInComplex.lmerBC4IntEnvAffixAcc<- lmer(bc ~ Affix*Environment*AccentuationCondition+PrePause+BaseInitialStress+
                                          OrderRescale +
                                          LocSpeech + 
                                          (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntEnvAffixAcc)
# no

# 3. Affix, Env and Stress
UnInComplex.lmerBC4IntEnvAffixStress<- lmer(bc ~ Affix*Environment*BaseInitialStress+PrePause+AccentuationCondition+
                                          OrderRescale +
                                          LocSpeech + 
                                          (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntEnvAffixStress)

# no


# 4. Pause, Acc and Stress
UnInComplex.lmerBC4IntPauseAccStress<- lmer(bc ~ Affix+Environment+BaseInitialStress*PrePause*AccentuationCondition+
                                              OrderRescale +
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntPauseAccStress)

# no
###################
# Summary 3-way: ony Affix*Env*Pause is significant

# the next 2 plots are not that helpful...

visreg(UnInComplex.lmerBC4IntAffixEnvPause,"Environment", by="Affix", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T, cond=list(PrePause="No Pause")
       #,ylim=c(30,120)
       )

# these plots are helpful

visreg(UnInComplex.lmerBC4IntAffixEnvPause,"Environment", by="Affix", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T, cond=list(PrePause="Pause")
       #,ylim=c(30,120)
)


visreg(UnInComplex.lmerBC4IntAffixEnvPause,"Affix", by="Environment", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T, cond=list(PrePause="No Pause")
       #,ylim=c(30,120)
)


visreg(UnInComplex.lmerBC4IntAffixEnvPause,"Affix", by="Environment", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T, cond=list(PrePause="Pause")
       #,ylim=c(30,120)
)


#################
# Trying out combinations of 2 way interactions

# 1. Env*Affix and Env*Pause
UnInComplex.lmerBC4IntEnvAffixEnvPause<- lmer(bc ~ Affix*Environment+BaseInitialStress+Environment*PrePause+AccentuationCondition+
                                              OrderRescale +
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntEnvAffixEnvPause)


# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      8.585e-01  4.794e-03  1.527e+02 179.089  < 2e-16 ***
#   AffixNeg                         2.134e-02  5.202e-03  1.243e+02   4.102 7.35e-05 ***
#   Affixun                          4.736e-02  4.651e-03  1.273e+02  10.183  < 2e-16 ***
#   Environmentn#C                   2.844e-02  4.779e-03  1.324e+02   5.951 2.25e-08 ***
# Environmentn#V                   5.382e-03  5.175e-03  1.282e+02   1.040 0.300333    
# BaseInitialStressunstressed     -1.616e-03  1.075e-03  1.087e+02  -1.502 0.135898    
# PrePausePause                    5.445e-03  6.721e-04  3.064e+03   8.102 8.88e-16 ***
#   AccentuationConditionunaccented -1.425e-03  4.262e-04  3.145e+03  -3.343 0.000839 ***
#   OrderRescale                    -2.748e-05  1.934e-05  3.068e+03  -1.421 0.155301    
# LocSpeech                       -1.981e-03  1.113e-04  2.852e+03 -17.797  < 2e-16 ***
#   AffixNeg:Environmentn#C         -2.185e-02  5.516e-03  1.228e+02  -3.962 0.000125 ***
# Affixun:Environmentn#C          -4.856e-02  4.907e-03  1.265e+02  -9.896  < 2e-16 ***
# AffixNeg:Environmentn#V         -2.704e-02  5.933e-03  1.207e+02  -4.558 1.25e-05 ***
# Affixun:Environmentn#V          -5.291e-02  5.368e-03  1.227e+02  -9.857  < 2e-16 ***
# Environmentn#C:PrePausePause    -2.340e-03  9.440e-04  3.048e+03  -2.479 0.013225 *  
# Environmentn#V:PrePausePause    -6.791e-03  8.669e-04  3.038e+03  -7.833 6.66e-15 ***

anova(UnInComplex.lmerBC4IntEnvAffixEnvPause,UnInComplex.lmerBC4IntAffixEnvPause)


# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# object 19 -19976 -19861  10007   -20014                            
# ..1    25 -19983 -19831  10016   -20033 18.522      6   0.005052 **

# 3 way is better - dann werden die anderen auch schlechter sein

# jetzt ist die Frage, ob eine zusätzliche Interkation das Modell noch besser
# machen würde

###############
# 3 way +

# 3 way + Env*Stress
UnInComplex.lmerBC4IntAffixEnvPause2<- lmer(bc ~ Affix*Environment*PrePause+AccentuationCondition+ Environment*BaseInitialStress+
                                             OrderRescale +
                                             LocSpeech + 
                                             (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixEnvPause2)

# Fixed effects:
#   Estimate     Std. Error             df t value
# (Intercept)                                   0.882243513    0.007657671  363.700000000 115.210
# AffixNeg                                      0.000006363    0.007993699  322.900000000   0.001
# Affixun                                       0.023020365    0.007529041  358.300000000   3.058
# Environmentn#C                                0.000503034    0.007812917  331.500000000   0.064
# Environmentn#V                               -0.021190121    0.008054036  329.400000000  -2.631
# PrePausePause                                -0.013095074    0.006269166 2988.800000000  -2.089
# AccentuationConditionunaccented              -0.001535999    0.000424914 3139.800000000  -3.615
# BaseInitialStressunstressed                  -0.010729163    0.003722348   79.900000000  -2.882
# OrderRescale                                 -0.000027144    0.000019293 3068.700000000  -1.407
# LocSpeech                                    -0.001914046    0.000110671 2684.800000000 -17.295
# AffixNeg:Environmentn#C                       0.001110139    0.008318746  311.600000000   0.133
# Affixun:Environmentn#C                       -0.020668205    0.007860906  339.200000000  -2.629
# AffixNeg:Environmentn#V                      -0.001934301    0.008534755  309.300000000  -0.227
# Affixun:Environmentn#V                       -0.025728239    0.008075500  337.300000000  -3.186
# AffixNeg:PrePausePause                        0.012911774    0.006713177 2989.100000000   1.923
# Affixun:PrePausePause                         0.019240276    0.006301609 2988.800000000   3.053
# Environmentn#C:PrePausePause                  0.015861088    0.006456374 2993.900000000   2.457
# Environmentn#V:PrePausePause                  0.015918666    0.006750170 2993.600000000   2.358
# Environmentn#C:BaseInitialStressunstressed    0.016891155    0.004255087   85.300000000   3.970
# Environmentn#V:BaseInitialStressunstressed    0.007430966    0.003898324   81.500000000   1.906
# AffixNeg:Environmentn#C:PrePausePause        -0.011958143    0.007006901 2995.000000000  -1.707
# Affixun:Environmentn#C:PrePausePause         -0.018948086    0.006566069 2997.400000000  -2.886
# AffixNeg:Environmentn#V:PrePausePause        -0.017751949    0.007226002 2994.700000000  -2.457
# Affixun:Environmentn#V:PrePausePause         -0.023264518    0.006831857 2994.900000000  -3.405
# Pr(>|t|)    
# (Intercept)                                < 0.0000000000000002 ***
#   AffixNeg                                               0.999365    
# Affixun                                                0.002399 ** 
#   Environmentn#C                                         0.948703    
# Environmentn#V                                         0.008914 ** 
# PrePausePause                                          0.036810 *  
#   AccentuationConditionunaccented                        0.000305 ***
#   BaseInitialStressunstressed                            0.005069 ** 
#   OrderRescale                                           0.159540    
# LocSpeech                                  < 0.0000000000000002 ***
#   AffixNeg:Environmentn#C                                0.893923    
# Affixun:Environmentn#C                                 0.008947 ** 
# AffixNeg:Environmentn#V                                0.820855    
# Affixun:Environmentn#V                                 0.001577 ** 
# AffixNeg:PrePausePause                                 0.054531 .  
# Affixun:PrePausePause                                  0.002284 ** 
#   Environmentn#C:PrePausePause                           0.014080 *  
# Environmentn#V:PrePausePause                           0.018425 *  
# Environmentn#C:BaseInitialStressunstressed             0.000150 ***
# Environmentn#V:BaseInitialStressunstressed             0.060151 .  
# AffixNeg:Environmentn#C:PrePausePause                  0.087996 .  
# Affixun:Environmentn#C:PrePausePause                   0.003933 ** 
# AffixNeg:Environmentn#V:PrePausePause                  0.014079 *  
# Affixun:Environmentn#V:PrePausePause                   0.000670 ***
# ---


anova(UnInComplex.lmerBC4IntAffixEnvPause2,UnInComplex.lmerBC4IntAffixEnvPause)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# ..1    25 -19983 -19831  10016   -20033                             
# object 27 -20001 -19837  10028   -20055 22.144      2 0.00001554 ***



# it is better


# 3 way + Env*Acc
UnInComplex.lmerBC4IntAffixEnvPause3<- lmer(bc ~ Affix*Environment*PrePause+AccentuationCondition+ Environment*AccentuationCondition+BaseInitialStress+
                                              OrderRescale +
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixEnvPause3)


anova(UnInComplex.lmerBC4IntAffixEnvPause2,UnInComplex.lmerBC4IntAffixEnvPause3)
# this is better!

# we can throw out stress and order

UnInComplex.lmerBC4IntAffixEnvPause4<- lmer(bc ~ Affix*Environment*PrePause+AccentuationCondition+ Environment*AccentuationCondition+
                                              LocSpeech + 
                                              (1|Item) + (1|Participant), data = UnInComplex2)

summary(UnInComplex.lmerBC4IntAffixEnvPause4)

# Fixed effects:
#   Estimate   Std. Error           df t value             Pr(>|t|)    
# (Intercept)                                       0.8732353    0.0068979  677.7000000 126.594 < 0.0000000000000002 ***
#   AffixNeg                                          0.0119828    0.0073260  508.4000000   1.636              0.10253    
# Affixun                                           0.0346729    0.0068391  613.8000000   5.070          0.000000528 ***
#   Environmentn#C                                    0.0124690    0.0070275  592.4000000   1.774              0.07652 .  
# Environmentn#V                                   -0.0147502    0.0073897  524.2000000  -1.996              0.04645 *  
# PrePausePause                                    -0.0124150    0.0061913 2991.7000000  -2.005              0.04503 *  
#   AccentuationConditionunaccented                  -0.0056127    0.0006605 3064.1000000  -8.498 < 0.0000000000000002 ***
#   LocSpeech                                        -0.0019975    0.0001103 2863.2000000 -18.116 < 0.0000000000000002 ***
#   AffixNeg:Environmentn#C                          -0.0124851    0.0077019  462.9000000  -1.621              0.10569    
# Affixun:Environmentn#C                           -0.0351495    0.0071684  551.8000000  -4.903          0.000001241 ***
# AffixNeg:Environmentn#V                          -0.0155051    0.0079688  439.0000000  -1.946              0.05233 .  
# Affixun:Environmentn#V                           -0.0380623    0.0075115  497.7000000  -5.067          0.000000570 ***
# AffixNeg:PrePausePause                            0.0113589    0.0066324 2991.8000000   1.713              0.08688 .  
# Affixun:PrePausePause                             0.0173428    0.0062269 2991.8000000   2.785              0.00538 ** 
#   Environmentn#C:PrePausePause                      0.0152638    0.0063778 2995.8000000   2.393              0.01676 *  
# Environmentn#V:PrePausePause                      0.0156629    0.0066667 2995.4000000   2.349              0.01887 *  
# Environmentn#C:AccentuationConditionunaccented    0.0046958    0.0009324 3003.2000000   5.037          0.000000502 ***
# Environmentn#V:AccentuationConditionunaccented    0.0073478    0.0008411 3006.2000000   8.736 < 0.0000000000000002 ***
# AffixNeg:Environmentn#C:PrePausePause            -0.0104484    0.0069223 2996.5000000  -1.509              0.13131    
# Affixun:Environmentn#C:PrePausePause             -0.0170030    0.0064897 2998.6000000  -2.620              0.00884 ** 
# AffixNeg:Environmentn#V:PrePausePause            -0.0158949    0.0071395 2996.3000000  -2.226              0.02607 *  
# Affixun:Environmentn#V:PrePausePause             -0.0210989    0.0067517 2996.5000000  -3.125              0.00180 ** 


######################################################
# Summary: 
# There are a lot of  significant interactions, also the combinations...but
# the best model is the one with a 3 way interaction (Affix*Env*Pause) and a two-way interaction
# Env*Acc

visreg(UnInComplex.lmerBC4IntAffixEnvPause4,"Affix", by="Environment", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,band=F,#cond = list(BaseInitialStress="primary"),
       cond = list(PrePause="Pause"),
       ylim=c(30,180))



visreg(UnInComplex.lmerBC4IntAffixEnvPause4,"Affix", by="Environment", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,band=F,#cond = list(BaseInitialStress="primary"),
       cond = list(PrePause="No Pause"),
       ylim=c(30,180))

# note für neg nnV and nC übereinander

# so, für Loc weniger gemination....

# let's see the relation to stress

unique(UnInComplex2[UnInComplex2$Affix=="Neg" & UnInComplex2$Environment=="n#nV",
                    c("Item","BaseInitialStress")])

#           Item BaseInitialStress
# 580   innocuous           primary
# 601  innominate           primary
# 625 innumerable           primary

unique(UnInComplex2[UnInComplex2$Affix=="Loc" & UnInComplex2$Environment=="n#nV",
                    c("Item","BaseInitialStress")])

#561 innervate        unstressed


#okey makes sense

visreg(UnInComplex.lmerBC4IntAffixEnvPause4,"AccentuationCondition", by="Environment", 
       trans= function(x) (x^(1/lambda)*1000), rug=F,overlay=T,band=F,
       ylim=c(30,180))

# okay doubles "react" more to gemination

#############################################################
# The final model:

summary(UnInComplex.lmerBC4IntAffixEnvPause4)


# Fixed effects:
#   Estimate   Std. Error           df t value
# (Intercept)                                       0.8732353    0.0068979  677.7000000 126.594
# AffixNeg                                          0.0119828    0.0073260  508.4000000   1.636
# Affixun                                           0.0346729    0.0068391  613.8000000   5.070
# Environmentn#C                                    0.0124690    0.0070275  592.4000000   1.774
# Environmentn#V                                   -0.0147502    0.0073897  524.2000000  -1.996
# PrePausePause                                    -0.0124150    0.0061913 2991.7000000  -2.005
# AccentuationConditionunaccented                  -0.0056127    0.0006605 3064.1000000  -8.498
# LocSpeech                                        -0.0019975    0.0001103 2863.2000000 -18.116
# AffixNeg:Environmentn#C                          -0.0124851    0.0077019  462.9000000  -1.621
# Affixun:Environmentn#C                           -0.0351495    0.0071684  551.8000000  -4.903
# AffixNeg:Environmentn#V                          -0.0155051    0.0079688  439.0000000  -1.946
# Affixun:Environmentn#V                           -0.0380623    0.0075115  497.7000000  -5.067
# AffixNeg:PrePausePause                            0.0113589    0.0066324 2991.8000000   1.713
# Affixun:PrePausePause                             0.0173428    0.0062269 2991.8000000   2.785
# Environmentn#C:PrePausePause                      0.0152638    0.0063778 2995.8000000   2.393
# Environmentn#V:PrePausePause                      0.0156629    0.0066667 2995.4000000   2.349
# Environmentn#C:AccentuationConditionunaccented    0.0046958    0.0009324 3003.2000000   5.037
# Environmentn#V:AccentuationConditionunaccented    0.0073478    0.0008411 3006.2000000   8.736
# AffixNeg:Environmentn#C:PrePausePause            -0.0104484    0.0069223 2996.5000000  -1.509
# Affixun:Environmentn#C:PrePausePause             -0.0170030    0.0064897 2998.6000000  -2.620
# AffixNeg:Environmentn#V:PrePausePause            -0.0158949    0.0071395 2996.3000000  -2.226
# Affixun:Environmentn#V:PrePausePause             -0.0210989    0.0067517 2996.5000000  -3.125
# Pr(>|t|)    
# (Intercept)                                    < 0.0000000000000002 ***
#   AffixNeg                                                    0.10253    
# Affixun                                                 0.000000528 ***
#   Environmentn#C                                              0.07652 .  
# Environmentn#V                                              0.04645 *  
# PrePausePause                                               0.04503 *  
#   AccentuationConditionunaccented                < 0.0000000000000002 ***
#   LocSpeech                                      < 0.0000000000000002 ***
#   AffixNeg:Environmentn#C                                     0.10569    
# Affixun:Environmentn#C                                  0.000001241 ***
# AffixNeg:Environmentn#V                                     0.05233 .  
# Affixun:Environmentn#V                                  0.000000570 ***
# AffixNeg:PrePausePause                                      0.08688 .  
# Affixun:PrePausePause                                       0.00538 ** 
#   Environmentn#C:PrePausePause                                0.01676 *  
# Environmentn#V:PrePausePause                                0.01887 *  
# Environmentn#C:AccentuationConditionunaccented          0.000000502 ***
# Environmentn#V:AccentuationConditionunaccented < 0.0000000000000002 ***
# AffixNeg:Environmentn#C:PrePausePause                       0.13131    
# Affixun:Environmentn#C:PrePausePause                        0.00884 ** 
# AffixNeg:Environmentn#V:PrePausePause                       0.02607 *  
# Affixun:Environmentn#V:PrePausePause                        0.00180 ** 


  lambda
#[1] 0.06060606


# I need to rename some variabels for the plot...


UnInComplex2<-rename(UnInComplex2,AccentuationAnnotator=Accentuation)

UnInComplex2<-rename(UnInComplex2,Accentuation=AccentuationCondition)

# need to rename the pause levels

levels(UnInComplex2$PrePause)
#[1] [1] "No Pause" "Pause"   



levels(UnInComplex2$PrePause)<-c("no pause"   , "pause")

levels(UnInComplex2$PrePause)
#[1] "no pause" "pause"   


levels(UnInComplex2$Affix)
#[1] "Loc" "Neg" "un"

levels(UnInComplex2$Affix)<-c("inLoc", "inNeg", "un")



final_UnIn_complete_model.lmer<-lmer(bc ~  Environment*Affix*PrePause+
                                     Environment*Accentuation+ LocSpeech                                             
                                    +(1|Participant)+ (1|Item) , data = UnInComplex2)                                 


summary(final_UnIn_complete_model.lmer)

#############
# Let's get the  model for the dissertation


table_final_models<-as.data.frame(coef(summary(final_UnIn_complete_model.lmer)))

xtable(table_final_models,digits = 3)



#change directory for plots

# need to create the variable ConsonantDurMS

UnInComplex$ConsonantDurMS<-UnInComplex$ConsonantDur*1000

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/Images/Experiment")


##############################
# We should  plot the main effect (not covariates)
###############################

png("UnInInterEnvAffixPause.png", units="cm", height=12, width=20, res=300)

ylim=c(30,180)
par(mfrow=c(1,2))

par(oma = c(4, 1, 1, 1))

visreg(final_UnIn_complete_model.lmer,"Affix", by="Environment", 
       cond=list(PrePause="pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,band=F,
       ylim=ylim, overlay=TRUE,xlab="environment", ylab="duration in milliseconds",
       line.par = list(col = c('cornflowerblue','mediumaquamarine','darkblue')),
       legend=FALSE, cex=0.8)
title(main = list("pause", cex = 1))

visreg(final_UnIn_complete_model.lmer,"Affix", by="Environment", 
       cond=list(PrePause="no pause"),trans= function(x) (x^(1/lambda)*1000), rug=F,band=F,
       ylim=ylim, overlay=TRUE,xlab="environment",ylab="duration in milliseconds",
       line.par = list(col = c('cornflowerblue','mediumaquamarine','darkblue')),legend=FALSE, cex=0.8)
title(main = list("no pause", cex = 1))

# legend (2,8,-1,c("stressed","unstressed"), 
#        fill = c('cornflowerblue','darkblue'))


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 0, 0, 0), 
    new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("n#nV","n#C","n#V"), xpd = TRUE, horiz = TRUE, inset = c(0, 
                                                                                 0),
       bty = "n", lwd=3,col =c('cornflowerblue','mediumaquamarine','darkblue'), cex =1)

dev.off()




png("UnInInterEnvAcc.png", units="cm", height=12, width=14, res=300, pointsize=15)


visreg(final_UnIn_complete_model.lmer, "Environment",by="Accentuation",
       ylab="duration in milliseconds", trans= function(x) (x^(1/lambda))*1000, 
       rug=F, xlab="environment",ylim=ylim,band=F,
       overlay=TRUE ,line.par = list(col = c('cornflowerblue','darkblue')),cex=0.8)


dev.off()




library (MuMIn)
# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the Inportance of the
# different factors.

options(na.action = "na.fail") 


UnInComplex.lm1<- lm(ConsonantDur ~ Environment*BaseInitialStress*Accentuation + 
                       PrePause*BaseInitialStress*Accentuation+ Affix*Environment*PrePause+
                       Affix*Environment*BaseInitialStress+Affix*Environment*Accentuation+
                       OrderRescale + logWordFormFreq +  + LocSpeech + PrecSegDur+PostPause, 
   data = UnInComplex)

model_ranking <- dredge(UnInComplex.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Relative variable importance: 
#   Affix BaseInitialStress Environment LocSpeech Affix:Environment
# Importance:           1.00  1.00              1.00        1.00      1.00            
# N containing models: 76704 76704             77056       40480     48800            
# BaseInitialStress:Environment PrePause Accentuation Accentuation:Environment
# Importance:           1.00                          1.00     1.00         1.00                   
# N containing models: 45888                         76256    76704        45888                   
# Environment:PrePause Affix:BaseInitialStress Accentuation:BaseInitialStress
# Importance:           1.00                 1.00                    0.98                         
# N containing models: 41120                41696                   45088                         
# Accentuation:BaseInitialStress:Environment logWordFormFreq PostPause
# Importance:           0.97                                       0.76            0.62    
# N containing models: 10528                                      40480           40480    
# Affix:BaseInitialStress:Environment Affix:PrePause PrecSegDur
# Importance:           0.61                                0.58           0.50     
# N containing models: 10368                               40960          40480     
# Affix:Environment:PrePause BaseInitialStress:PrePause OrderRescale
# Importance:           0.50                       0.40                       0.35       
# N containing models:  9312                      40608                      40480       
# Accentuation:PrePause Accentuation:Affix Accentuation:BaseInitialStress:PrePause
# Importance:           0.24                  0.17               0.04                                  
# N containing models: 40608                 41696               8608                                  
# Accentuation:Affix:Environment
# Importance:           0.01                         
# N containing models: 10368  