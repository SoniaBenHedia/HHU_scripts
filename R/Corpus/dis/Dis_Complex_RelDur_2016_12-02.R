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
#library(influence.ME)#
library(MuMIn)

# set the directory, so R knows where to find a file

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/dis/csv/durations")

#setwd("C:/Users/Plag/Documents/Forschung/Sonia/2015-08-18")

# loading the data, naming it d, telling R that t is the seperator, and that na, NA and   are NA strings

d <- read.csv("dis_2016_02_04.csv", sep=",",header = T,  na.string=c("na", "", "NA"))
dat<-read.csv("dis_durations_etc_2016_02_04.csv", sep=",",header = T,  na.string=c("na", "", "NA"))


str(d)
# 'data.frame':  290 obs. of  27 variables:
# $ ItemID                  : int  163 19 229 290 270 112 46 76 199 115 ...
# $ item                    : Factor w/ 99 levels "disabilities",..: 30 30 8 58 56 2 30 95 79 77 ...
# $ PartofSpeechWord        : Factor w/ 3 levels "j","n","v": 2 2 3 2 2 2 2 2 3 2 ...
# $ Base                    : Factor w/ 63 levels "abilities","ability",..: NA NA 8 NA 33 2 NA NA 51 49 ...
# $ PartofSpeechBase        : Factor w/ 3 levels "j","n","v": NA NA 3 NA 2 2 NA NA 3 2 ...
# $ WordFormFreqSpokenCOCA  : int  1501 1501 4515 1542 27 554 1501 5480 0 234 ...
# $ BaseFormFreqSpokenCOCA  : int  NA NA 19564 NA 0 6141 NA NA 0 12159 ...
# $ WordLemmaFreqSpokenCOCA : int  1589 1589 5593 1542 27 1117 1589 6604 0 233 ...
# $ BaseLemmaFreqSpokenCOCA : int  NA NA 27507 NA 0 6582 NA NA 0 18549 ...
# $ BaseFormFreqAllCoca     : int  NA NA 40882 NA 0 49022 NA NA 0 92250 ...
# $ WordFormFreqAllCoca     : int  12340 12340 9376 9184 253 6766 12340 44027 2 730 ...
# $ WordLemmaFreqAllCoca    : int  16884 16884 13428 9184 253 17388 16884 53953 0 721 ...
# $ BaseLemmaFreqAllCoca    : int  NA NA 83001 NA 0 56323 NA NA 0 160876 ...
# $ Speaker                 : Factor w/ 243 levels "sw2013+AC0-ms98+AC0-a",..: 65 116 175 159 205 92 179 128 64 229 ...
# $ Pos                     : Factor w/ 3 levels "E","M","P": 2 3 3 2 2 3 2 2 2 2 ...
# $ Orthography             : Factor w/ 4 levels "diS","disC","diss",..: 2 2 4 2 2 4 2 2 3 3 ...
# $ SematicTransparency     : Factor w/ 4 levels "?","n","o","y": 2 2 4 2 3 4 2 2 3 4 ...
# $ MorphGeminate           : Factor w/ 3 levels "maybe","no","yes": 1 1 2 2 2 2 1 2 3 3 ...
# $ DeletionMorph           : Factor w/ 3 levels "C","N","V": 2 2 2 2 2 2 2 2 3 2 ...
# $ DeviantPronun           : Factor w/ 4 levels "C","CC","N","Y": 3 3 3 4 3 3 3 3 3 3 ...
# $ PauseMorph              : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ AffixStressLongman      : Factor w/ 4 levels "d","p","s","u": 2 2 3 2 4 3 2 2 1 4 ...
# $ AdjSyllableStressLongman: Factor w/ 2 levels "p","u": 2 2 2 2 1 2 2 2 1 1 ...
# $ AffixStress             : Factor w/ 4 levels "d","p","s","u": 2 2 3 2 1 3 2 2 1 1 ...
# $ AdjSyllableStress       : Factor w/ 2 levels "p","u": 2 2 2 2 1 2 2 2 1 1 ...
# $ Compound.AF8.yes.AF8.no : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
# $ Compound                : Factor w/ 27 levels "attention deficit disorders",..: 1 2 3 4 5 6 6 6 7 7 ...

# looks good

dim(d)
#[1] 290 27

 head(d)
# ItemID          item PartofSpeechWord       Base PartofSpeechBase WordFormFreqSpokenCOCA BaseFormFreqSpokenCOCA
# 1    163    discipline                n       <NA>             <NA>                   1501                     NA
# 2     19    discipline                n       <NA>             <NA>                   1501                     NA
# 3    229      disagree                v      agree                v                   4515                  19564
# 4    290        Disney                n       <NA>             <NA>                   1542                     NA
# 5    270 dismemberment                n memberment                n                     27                      0
# 6    112    disability                n    ability                n                    554                   6141
# WordLemmaFreqSpokenCOCA BaseLemmaFreqSpokenCOCA BaseFormFreqAllCoca WordFormFreqAllCoca WordLemmaFreqAllCoca
# 1                    1589                      NA                  NA               12340                16884
# 2                    1589                      NA                  NA               12340                16884
# 3                    5593                   27507               40882                9376                13428
# 4                    1542                      NA                  NA                9184                 9184
# 5                      27                       0                   0                 253                  253
# 6                    1117                    6582               49022                6766                17388
# BaseLemmaFreqAllCoca               Speaker Pos Orthography SematicTransparency MorphGeminate DeletionMorph DeviantPronun
# 1                   NA sw2589+AC0-ms98+AC0-a   M        disC                   n         maybe             N             N
# 2                   NA sw2988+AC0-ms98+AC0-a   P        disC                   n         maybe             N             N
# 3                83001 sw3800+AC0-ms98+AC0-a   P        disV                   y            no             N             N
# 4                   NA sw3614+AC0-ms98+AC0-a   M        disC                   n            no             N             Y
# 5                    0 sw4159+AC0-ms98+AC0-a   M        disC                   o            no             N             N
# 6                56323 sw2799+AC0-ms98+AC0-a   P        disV                   y            no             N             N
# PauseMorph AffixStressLongman AdjSyllableStressLongman AffixStress AdjSyllableStress Compound.AF8.yes.AF8.no
# 1          N                  p                        u           p                 u                     yes
# 2          N                  p                        u           p                 u                     yes
# 3          N                  s                        u           s                 u                     yes
# 4          N                  p                        u           p                 u                     yes
# 5          N                  u                        p           d                 p                     yes
# 6          N                  s                        u           s                 u                     yes
# Compound
# 1 attention deficit disorders
# 2           beer distributors
# 3     coronary artery disease
# 4               disaster work
# 5              disasters area
# 6         discipline problems    




str(dat)

# 'data.frame':  290 obs. of  14 variables:
#   $ ItemID          : int  239 70 214 281 288 1 6 12 25 44 ...
# $ item            : Factor w/ 99 levels "disabilities",..: 76 23 76 77 24 14 21 21 22 73 ...
# $ AbsDur          : num  0.0966 0.1815 0.0805 0.2054 0.1488 ...
# $ WordDur         : num  0.584 0.698 0.67 0.678 0.561 ...
# $ PrecSeg         : Factor w/ 3 levels "@","d","I": 3 3 3 3 3 3 3 3 3 3 ...
# $ PrecSegDur      : num  0.0371 0.0612 0.0615 0.0453 0.0529 ...
# $ FollSeg         : Factor w/ 33 levels "?","@","@?","{",..: 5 6 6 9 9 1 4 4 4 4 ...
# $ FollSegVC       : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ FollSegDur      : num  0.0534 0.1487 0.0656 0.11 0.1006 ...
# $ LocSpeech       : num  13.71 8.59 8.95 11.8 12.48 ...
# $ PreSuf          : Factor w/ 2 levels "initialNoMorph",..: 1 2 1 2 2 2 1 1 1 2 ...
# $ SyllAct         : int  4 2 3 3 3 2 3 3 3 4 ...
# $ NumberOfSegments: int  8 6 6 8 7 7 8 9 8 8 ...
# $ PrefixDur       : num  0.182 0.303 0.188 0.32 0.245 ...

 
dim(dat)
#[1] 290  14

# looks good


# now let's merge them

dis<- merge(d, dat, by.x="ItemID", by.y="ItemID")

str(dis)
# 'data.frame':  290 obs. of  40 variables:
#   $ ItemID                  : int  1 2 3 4 5 6 7 8 9 10 ...
# $ item.x                  : Factor w/ 99 levels "disabilities",..: 14 29 36 38 30 21 30 48 39 30 ...
# $ PartofSpeechWord        : Factor w/ 3 levels "j","n","v": 3 1 1 2 2 2 2 2 3 2 ...
# $ Base                    : Factor w/ 63 levels "abilities","ability",..: 14 NA 20 21 NA NA NA 26 22 NA ...
# $ PartofSpeechBase        : Factor w/ 3 levels "j","n","v": 3 NA 1 2 NA NA NA 2 3 NA ...
# $ WordFormFreqSpokenCOCA  : int  247 187 197 1150 1501 603 1501 8319 4250 1501 ...
# $ BaseFormFreqSpokenCOCA  : int  4442 NA 0 7843 NA NA NA 1090 4419 NA ...
# $ WordLemmaFreqSpokenCOCA : int  0 187 121 0 1589 7 1589 10131 0 1589 ...
# $ BaseLemmaFreqSpokenCOCA : int  0 NA 0 3800 NA NA NA 435 0 NA ...
# $ BaseFormFreqAllCoca     : int  32500 NA 1 48648 NA NA NA 11683 31474 NA ...
# $ WordFormFreqAllCoca     : int  2927 2455 1266 11082 12340 3053 12340 46058 27702 12340 ...
# $ WordLemmaFreqAllCoca    : int  0 2455 726 1 16884 14 16884 57357 0 16884 ...
# $ BaseLemmaFreqAllCoca    : int  0 NA 0 24706 NA NA NA 6686 0 NA ...
# $ Speaker                 : Factor w/ 243 levels "sw2013+AC0-ms98+AC0-a",..: 199 229 133 56 90 4 34 231 91 84 ...
# $ Pos                     : Factor w/ 3 levels "E","M","P": 1 2 2 2 2 2 2 2 2 2 ...
# $ Orthography             : Factor w/ 4 levels "diS","disC","diss",..: 4 2 2 2 2 4 2 4 2 2 ...
# $ SematicTransparency     : Factor w/ 4 levels "?","n","o","y": 4 2 3 1 2 2 2 3 4 2 ...
# $ MorphGeminate           : Factor w/ 3 levels "maybe","no","yes": 2 1 2 2 1 2 1 2 2 1 ...
# $ DeletionMorph           : Factor w/ 3 levels "C","N","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ DeviantPronun           : Factor w/ 4 levels "C","CC","N","Y": 3 3 3 3 3 3 3 3 3 3 ...
# $ PauseMorph              : Factor w/ 2 levels "N","Y": 2 1 1 1 1 1 1 1 1 1 ...
# $ AffixStressLongman      : Factor w/ 4 levels "d","p","s","u": 3 2 4 4 2 4 2 4 4 2 ...
# $ AdjSyllableStressLongman: Factor w/ 2 levels "p","u": 2 2 1 1 2 1 2 1 1 2 ...
# $ AffixStress             : Factor w/ 4 levels "d","p","s","u": 3 2 1 1 2 4 2 1 1 2 ...
# $ AdjSyllableStress       : Factor w/ 2 levels "p","u": 2 2 1 1 2 1 2 1 1 2 ...
# $ Compound.AF8.yes.AF8.no : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ Compound                : Factor w/ 27 levels "attention deficit disorders",..: NA NA NA NA NA NA NA NA NA NA ...
# $ item.y                  : Factor w/ 99 levels "disabilities",..: 14 29 36 38 30 21 30 48 39 30 ...
# $ AbsDur                  : num  0.0848 0.0902 0.193 0.1298 0.0856 ...
# $ WordDur                 : num  0.759 0.719 1.06 0.576 0.64 ...
# $ PrecSeg                 : Factor w/ 3 levels "@","d","I": 3 3 3 3 3 3 3 3 3 3 ...
# $ PrecSegDur              : num  0.0514 0.0928 0.0649 0.0592 0.0891 ...
# $ FollSeg                 : Factor w/ 33 levels "?","@","@?","{",..: 1 2 22 22 2 4 2 20 22 2 ...
# $ FollSegVC               : Factor w/ 2 levels "C","V": 2 2 1 1 2 2 2 2 1 2 ...
# $ FollSegDur              : num  0.0202 0.0328 0.1038 0.0785 0.0239 ...
# $ LocSpeech               : num  9.22 15.29 8.49 12.16 10.94 ...
# $ PreSuf                  : Factor w/ 2 levels "initialNoMorph",..: 2 1 2 2 1 1 1 2 2 1 ...
# $ SyllAct                 : int  2 5 3 3 3 3 3 2 3 3 ...
# $ NumberOfSegments        : int  7 11 9 7 7 8 8 5 9 8 ...
# $ PrefixDur               : num  0.207 0.238 0.328 0.24 0.205 ...

# # yeah that's good!

# Transforming freq. in log frequencies (adding of 1 to elimate 0 frequencies):

dis$WordFormFreqSpokenCOCA <- dis$WordFormFreqSpokenCOCA+1
dis$BaseFormFreqSpokenCOCA <- dis$BaseFormFreqSpokenCOCA+1
dis$WordLemmaFreqSpokenCOCA <- dis$WordLemmaFreqSpokenCOCA+1
dis$BaseLemmaFreqSpokenCOCA <- dis$BaseLemmaFreqSpokenCOCA+1
dis$WordFormFreqAllCoca <- dis$WordFormFreqAllCoca+1
dis$BaseFormFreqAllCoca <- dis$BaseFormFreqAllCoca +1
dis$WordLemmaFreqAllCoca <- dis$WordLemmaFreqAllCoca+1
dis$BaseLemmaFreqAllCoca <- dis$BaseLemmaFreqAllCoca+1


dis$logWordFormFreqSpokenCOCA <- log(dis$WordFormFreqSpokenCOCA)
dis$logBaseFormFreqSpokenCOCA <- log(dis$BaseFormFreqSpokenCOCA)
dis$logWordLemmaFreqSpokenCOCA <- log(dis$WordLemmaFreqSpokenCOCA)
dis$logBaseLemmaFreqSpokenCOCA <- log(dis$BaseLemmaFreqSpokenCOCA)
dis$logWordFormFreqAllCoca <- log(dis$WordFormFreqAllCoca)
dis$logBaseFormFreqAllCoca <- log(dis$BaseFormFreqAllCoca)
dis$logWordLemmaFreqAllCoca <- log(dis$WordLemmaFreqAllCoca)
dis$logBaseLemmaFreqAllCoca <- log(dis$BaseLemmaFreqAllCoca)


dim(dis)
#[1] 290  48

# correcting a typo
dis$SemanticTransparency  <-dis$SematicTransparency
levels(dis$SemanticTransparency)
#[1] "?" "n" "o" "y"

# Which are the ?
dis[dis$SemanticTransparency=="?","item.x"]

# These are the Discover items. They refer to the credit card
# We should disregard these items since we cannot validly assign their
# morphological status

dis_1 <- dis[dis$SemanticTransparency!="?",]

dim(dis_1)
#[1] 281  49

# we lost 9 observations

levels(dis_1$SemanticTransparency)
dis_1$SemanticTransparency<-droplevels(dis_1$SemanticTransparency)
levels(dis_1$SemanticTransparency)
#[1] "n" "o" "y"

# let's rename them

levels(dis_1$SemanticTransparency) = c("no", "opaque", "transparent")

levels(dis_1$SemanticTransparency) 
#[1] "no"     "opaque" "transparent"   


names(dis_1)

# [1] "ItemID"                     "item.x"                     "PartofSpeechWord"           "Base"                      
# [5] "PartofSpeechBase"           "WordFormFreqSpokenCOCA"     "BaseFormFreqSpokenCOCA"     "WordLemmaFreqSpokenCOCA"   
# [9] "BaseLemmaFreqSpokenCOCA"    "BaseFormFreqAllCoca"        "WordFormFreqAllCoca"        "WordLemmaFreqAllCoca"      
# [13] "BaseLemmaFreqAllCoca"       "Speaker"                    "Pos"                        "Orthography"               
# [17] "SematicTransparency"        "MorphGeminate"              "DeletionMorph"              "DeviantPronun"             
# [21] "PauseMorph"                 "AffixStressLongman"         "AdjSyllableStressLongman"   "AffixStress"               
# [25] "AdjSyllableStress"          "Compound.AF8.yes.AF8.no"    "Compound"                   "item.y"                    
# [29] "AbsDur"                     "WordDur"                    "PrecSeg"                    "PrecSegDur"                
# [33] "FollSeg"                    "FollSegVC"                  "FollSegDur"                 "LocSpeech"                 
# [37] "PreSuf"                     "SyllAct"                    "NumberOfSegments"           "PrefixDur"                 
# [41] "logWordFormFreqSpokenCOCA"  "logBaseFormFreqSpokenCOCA"  "logWordLemmaFreqSpokenCOCA" "logBaseLemmaFreqSpokenCOCA"
# [45] "logWordFormFreqAllCoca"     "logBaseFormFreqAllCoca"     "logWordLemmaFreqAllCoca"    "logBaseLemmaFreqAllCoca"   
# [49] "SemanticTransparency"     


# We create the variable RelFreq by dividing the word lemma freq by the base lemma freq
dis_1$RelFreq <- dis_1$WordLemmaFreqAllCoca / dis_1$BaseLemmaFreqAllCoca
dis_1$logRelFreq <- log(dis_1$WordLemmaFreqAllCoca / dis_1$BaseLemmaFreqAllCoca)


dim(dis_1)
#[1] 281  51

# now let's just look at the complex items
dis_complex<- dis_1[dis_1$SemanticTransparency=="transparent"|dis_1$SemanticTransparency=="opaque",]

dim(dis_complex)
#[1] 128  51

# now we should not have any items with no semantic transparency in the the df

levels(dis_complex$SemanticTransparency)
#[1] "no"          "opaque"      "transparent"

dis_complex$SemanticTransparency<-droplevels(dis_complex$SemanticTransparency)

levels(dis_complex$SemanticTransparency)
#[1] "opaque"      "transparent"

# okay that looks good. We only have complex words in here now

# let's create the variabe RelDur

dis_complex$RelDur<-dis_complex$AbsDur/dis_complex$PrecSegDur



# Let's take a look at the distribution
plot(dis_complex$RelDur)
plot(density(dis_complex$RelDur))

# looks like a pretty normal distribution


#We will plot the distributions of double s, single s and maybe double s

plot( dis_complex$RelDur)

dis_complexSingle<- dis_complex[dis_complex$MorphGeminate=="no",]
dis_complexDouble<- dis_complex[dis_complex$MorphGeminate=="yes",]

plot(dis_complexSingle$RelDur)
densityplot(dis_complexSingle$RelDur)


plot(dis_complexDouble$RelDur)
densityplot(dis_complexDouble$RelDur)
plot(dis_complexDouble$RelDur)


# all in all everything looks okay



##############################################################################
#######   Now let's look at each variable in isolation     ###################
##############################################################################

# Problem is, that there aren't too many observations in dis. That 
# means that we have to be super-careful with regard to the number of 
# predictors, otherwise we end up with serious overfitting issues.

# We will now look at each preditor variable in order to decide which one to include
# Variables are in included if there is a theoretical reason to include them, it is
# statistically unproblematic to include them (levels are not too small).

# Covariates are included if an effect is observable. 
#Variables of iterest are all included.


#         1. Part of Speech

plot(dis_complex$PartofSpeechWord, dis_complex$RelDur)
table (dis_complex$PartofSpeechWord)
#j   n   v 
# 25 58 45

# It looks like adjectives are a little longer but since this effect looks
# only marginal and was not present in the other analysis I don't think
# we need to include it. Also there is no theoretical reason to believe
# that this factor has an effect


#         3. Frequencies

# We have a bunch if different frequency variables. We have frequencues from the 
# spoken part of COCA and the whole COCA corpus (AllCoca)

# Form Frequencies for base and word:

  #logWordFormFreqSpokenCOCA  logBaseFormFreqSpokenCOCA logWordFormFreqAllCoca  logBaseFormFreqAllCoca

# Lemma Frequencies for base and word:

  # WordLemmaFreqAllCoca  BaseLemmaFreqAllCoca logWordLemmaFreqSpokenCOCA  logBaseLemmaFreqSpokenCOCA


# and relative frequencies
			
# We will now take a look at the correlations between the different variables to 
# to decide which ones to include

pairscor.fnc(dis_complex [, c("logWordFormFreqSpokenCOCA", "logBaseFormFreqSpokenCOCA", "logWordLemmaFreqSpokenCOCA", "logBaseLemmaFreqSpokenCOCA", "logRelFreq","logWordFormFreqAllCoca", "logBaseFormFreqAllCoca", "logWordLemmaFreqAllCoca", "logBaseLemmaFreqAllCoca")])


# the pairscor plot shows 
#          - that the word form and lemma frequencies are 
#         almost the same (which is due to the lemmatizer used in COCA). So, it
#          doesn't make any sense to include both of them. We will include Form 
#         - that it does not seem to make a difference whether we use frequencies taken from
#           the spoken part of COCA or the whole corpus. They highly correlate.
#          I decide to use the word form frequencies from the whole corpus due to 
#           size of of corpus (bigger), so we have less zero-frequencies 
#           (in this case 1 cause we added 1 to the frequencies to get rid of 0)
#         - that base and word frequency correlate 
#         - that logRelFreq is also kind of related to two of these frequencies 
#          (logWordFormFreqAllCoca and logBaseFormFreqAllCoca), which isn't surprising because
#           these two variables were used to calculate RelFreq in the first place.
#           So, these three variables need some attention...

plot (dis_complex$logRelFreq, dis_complex$RelDur)
plot (dis_complex$logWordFormFreqAllCoca, dis_complex$RelDur)
plot (dis_complex$logBaseFormFreqAllCoca, dis_complex$RelDur)

# None of these three plots indicate an extremely strong effect on 
# the absolute duration. This probably means that any decision that we
# make won't affect the model to a large degree...

# We decide to use two variables: logWordFormFreqAllCoca, because we assume that
# in a very frequent word form, the segments are shorter, and logRelFreqAllCoca, 
# because we assume that in a more decomposable word, the absolute duration
# should be longer (and a word with a low RelFreq should be more 
# decomposable --> also one of the variables we are interested in).


#     4. Position

table (dis_complex$Pos)
#  E   M   P 
# 14 103  11 
 

plot (dis_complex$RelDur ~ dis_complex$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.



#     5. Word Duration

plot (density(dis_complex$WordDur))
plot (dis_complex$RelDur ~ dis_complex$WordDur)


# longer words seem to have a longer s

#     6. Preceding Segment duration

plot (density(dis_complex$PrecSegDur))
plot(dis_complex$PrecSegDur)
plot (dis_complex$RelDur ~ dis_complex$PrecSegDur)

#Of course there is a relation since RelDur partly consosts of PrecSegDur...

#     7. Following segment

table (dis_complex$FollSegVC)
#C   V 
#46  82

# Problem: After double s, only vowels may occur:

dis_complex$MorphGeminate<- droplevels(dis_complex$MorphGeminate)

table (dis_complex$FollSegVC, dis_complex$MorphGeminate)
#  no yes
#C 45   1
#V 59  23

# okay, this seems wrong! Lt's have a look

dis_complex[dis_complex$MorphGeminate=="yes"&dis_complex$FollSegVC=="C", ]
# ahhh okay, it is dissolution and the /o/ is deleted - the speaker says dislution

# So this is all fine but we should keep it in mind

# let's have a look at this effect

plot(dis_complex$RelDur~dis_complex$FollSegVC)
# does not seem to make a dfference but in the un-analysis it had a significant difference

# Thus we should include this factor, however because of the uneven distribution it makes
# sense to combine this factor with Morph-Geminate


dis_complex$TransitionType <- factor(paste (dis_complex$MorphGeminate,dis_complex$FollSegVC, sep="-"))
levels(dis_complex$TransitionType)

#[1] "no-C"  "no-V"  "yes-C" "yes-V"

# We should rename the levels
# We will now put all the doubles in one category keeping in mind that one
# has a following consonant

levels(dis_complex$TransitionType)<-c("s#C","s#V", "s#s","s#s")


levels(dis_complex$TransitionType)
#[1] "s#C" "s#V" "s#s"

table(dis_complex$TransitionType)

#s#C s#V s#s 
#45  59  24 


# Now let's have a look at the durational differences
plot(dis_complex$RelDur~dis_complex$TransitionType)

# Doubles seem to be longer!

# 8. Following segment duration

plot(dis_complex$RelDur~dis_complex$FollSegDur)


# We ignore FollSegDur, because that's sort of included in FollSegVC
# and in WordDur anyways, and there is nothing the literature that suggests
# that this is an extremely important predictor on theoretical grounds.



#  9. Word Length

# Next, we have a bunch of variables that are all closely related to each
# other and are related to word length
#, which means that we certainly do not want to include all of them:
#
# LocSpeech, SyllAct, NumberOfSegments

#    9.1. Local speech rate

plot(density(dis_complex$LocSpeech))
     
plot(dis_complex$RelDur~dis_complex$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables. But it might have an effct (see un and im), especially because WorDur is
# included in this factor and it has been shown to have an effect


# 9.2. Number of syllables

table(dis_complex$SyllAct)
# 1  2  3  4  5 
#1  27 59 32  9

plot (dis_complex$RelDur ~ dis_complex$SyllAct)
# hmmm

#     9.3. Word Duration

plot (dis_complex$RelDur ~ dis_complex$WordDur)

# we looked at this already

#     9.4. Number of Segments

plot(density(dis_complex$NumberOfSegments))
plot (dis_complex$RelDur ~ dis_complex$NumberOfSegments)

# nothing obvious here...
# but there might be an effect


# Let's now take a look at the relations between the different variables

# Number of Segments and SyllNumber
cor.test(dis_complex$NumberOfSegments, dis_complex$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  dis_complex$NumberOfSegments and dis_complex$SyllAct
# S = 52627.36, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.8494227 

# # There is a correlation between the number of segments and the syll-number 

# Number of Segments and word duration:

plot(dis_complex$NumberOfSegments, dis_complex$WordDur)
cor.test (dis_complex$NumberOfSegments, dis_complex$WordDur)

# Pearson's product-moment correlation
# 
# data:  dis_complex$NumberOfSegments and dis_complex$WordDur
# t = 5.7973, df = 126, p-value = 5.106e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3100363 0.5857652
# sample estimates:
#       cor 
# 0.4588788 


# So, there is, as expected, a correlation between the 
# number of segments and the duration of the word. This means that
# the model might show some unwanted effects of collinearity.

# The obvious solution is to take LocSpeech as a variable, since
# it combines Number of Segments and word duration

# Number of Syllables does not have to be incorporated since it
# has a high correlation with Number of Segments


#    10. Stress

table (dis_complex$AffixStress)
#d  p  s  u 
#86  1 39  2

plot (dis_complex$RelDur ~ dis_complex$AffixStress)

# there does not seem to be a connection between stress and duration in this case
# also some of the levels are too small to say anything AND the coding is
# somehow unreliable (Longman)

table (dis_complex$AdjSyllableStress)
#p   u 
#88 40 

plot (dis_complex$RelDur ~ dis_complex$AdjSyllableStress)

# no obvious effect is visible, 

table(dis_complex$AffixStress,dis_complex$AdjSyllableStress)
#   p  u
#d 86  0
#p  0  1
#s  0 39
#u  2  0


table (dis_complex$AdjSyllableStress,dis_complex$SemanticTransparency)
#   opaque transparent
#p     50          38
#u      4          36

table (dis_complex$AdjSyllableStress,dis_complex$MorphGeminate)

#    no yes
#p   67  21
#u   37   3


#for adjacent syllable stress :there is no sign that there
# is an effect whatsoever.....we will include this factor so noone
# complains, but it probably won't show any effect


#11. PrefixDur

plot(density(dis_complex$PrefixDur))
plot(dis_complex$PrefixDur)
# looks ok

plot(dis_complex$RelDur~dis_complex$PrefixDur)
# there seems to be a strong correlation, let's check

cor.test (dis_complex$PrefixDur, dis_complex$RelDur)
# Pearson's product-moment correlation
# 
# data:  dis_complex$PrefixDur and dis_complex$RelDur
# t = 1.9462, df = 126, p-value = 0.05386
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.002781601  0.334447372
# sample estimates:
#       cor 
# 0.1708314 


#12. Semantic Transparency

table(dis_complex$SemanticTransparency)
#    opaque transparent 
#        54          74  

# evenly distributed, 

table(dis_complex$SemanticTransparency, dis_complex$TransitionType)
#             s#C s#V s#s
#opaque       25  20   9
#transparent  20  39  15

# what if we only look at types

dis_complex_Types<-dis_complex[!duplicated(dis_complex$item.x),]

table(dis_complex_Types$SemanticTransparency)
# opaque transparent 
# 28          34 

# looks good

table(dis_complex_Types$SemanticTransparency, dis_complex_Types$TransitionType)
#               s#C s#V s#s
# opaque       20   2   6
# transparent  13  18   3


# ok - nothing strikes me as a possible problem here

# Now let's take a look at Semantic Transparency


plot (dis_complex$RelDur ~ dis_complex$SemanticTransparency)
# does not seem to make a dofference, however transparent seem to be a bit longer


# I would like to take a quick look at the stress distribution:

table(dis_complex_Types$TransitionType,dis_complex_Types$AdjSyllableStress)
#      p  u
# s#C 29  4
# s#V  9 11
# s#s  8  1

table(dis_complex_Types$SemanticTransparency,dis_complex_Types$AdjSyllableStress)
#              p  u
# opaque      26  2
# transparent 20 14


# I would like to see the items

dis_complex_Types[dis_complex_Types$SemanticTransparency=="transparent",c("item.x","AdjSyllableStress")]

# hm all seems normal

## We are going to include the following predictors:
# - TransitionType
# - SemanticTransparency
# - logWordFormFreqAllCoca
# - logRelFreq
# - LocSpeech
# - AdjSyllableStress


###########################################################################
# Note: I tested different measurments of decomposability and came to 
# the concludion that ST is the best measurmet. Its validity was confirmed
# by a decomopsability rating. The median of the rating corr. with
# this measurment. However, analyses (see other script) have shown that the
# binary factor ST is a better predictor for consonant duration than the rating
###########################################################################



# In this datset it might be worth to include random effect, however I am not sure!

tmp.lmer <- lmer(RelDur ~ 1 + (1|item.x) + (1|Speaker), data = dis_complex)
cor(dis_complex$RelDur, fitted(tmp.lmer))^2
#[1] 0.9279338

# does not make sense

tmp.lmer <- lmer(RelDur ~ 1 +(1|Speaker), data = dis_complex)
cor(dis_complex$RelDur, fitted(tmp.lmer))^2
#[1] 0.9519288

#nope

tmp.lmer <- lmer(RelDur ~ 1 + (1|item.x) , data = dis_complex)
cor(dis_complex$RelDur, fitted(tmp.lmer))^2
#[1] 0.4524465

# could make sense - so keep it in mind BUT
# The problem I see is that in certain levels we have more type variation than in
# others


length(unique(dis_complex[dis_complex$TransitionType=="s#s","item.x"]))
#[1] 9 types with morph Geminates

length(unique(dis_complex[dis_complex$TransitionType=="s#V","item.x"]))
#[1] 21 types with singletons followed by a vowel

length(unique(dis_complex[dis_complex$TransitionType=="s#C","item.x"]))
#[1] 34 types with singletons followed by a consonant


# Thus, ....thus we will not conduct LMER

#################################################################################
# Before doing an initial model, let's look at the number of types and tokens   #
# in each category                                                              #
#################################################################################


#1. TransitionType

table(dis_complex$TransitionType)

#s#C s#V s#s 
#45  59  24 


length(unique(dis_complex[dis_complex$TransitionType=="s#C","item.x"]))
#[1] 34  types

length(unique(dis_complex[dis_complex$TransitionType=="s#V","item.x"]))
# 21 types

length(unique(dis_complex[dis_complex$TransitionType=="s#s","item.x"]))
# 9 types

#2. SemanticTransparency

table(dis_complex$SemanticTransparency)

#     opaque transparent 
#       54          74 

# Number of types for SemanticTransparency

length(unique(dis_complex[dis_complex$SemanticTransparency=="transparent","item.x"]))
#[1] 34 transparent types

# example items

unique(dis_complex[dis_complex$SemanticTransparency=="transparent" & dis_complex$TransitionType=="s#s","item.x"])
#[1] dissatisfied dissimilar   disservice  

unique(dis_complex[dis_complex$SemanticTransparency=="transparent" & dis_complex$TransitionType=="s#C","item.x"])
#[1] discovered  disagree    discolor    discovers   disrepair   disregard   discharged 
#[8] dislike     discover    disgraceful disqualify  disappeared dislikes    distrust 


unique(dis_complex[dis_complex$SemanticTransparency=="transparent" & dis_complex$TransitionType=="s#V","item.x"])
#[1] disappears     disagree       disorders      disadvantages  disadvantage   disagreements 
#[7] disappear      disarm         disintegrating disagreement   disorient      disability    
#[13] disappeared    dishonest      disables       disappearing   disabilities   disabled      
#[19] disadvantaged

length(unique(dis_complex[dis_complex$SemanticTransparency=="opaque","item.x"]))
#[1] 28 opaque types


# example items

unique(dis_complex[dis_complex$SemanticTransparency=="opaque" & dis_complex$TransitionType=="s#s","item.x"])
#[1] dissolve     dissolved    dissolution  dissenting   dissimilated dissolving  


unique(dis_complex[dis_complex$SemanticTransparency=="opaque" & dis_complex$TransitionType=="s#C","item.x"])
#[1] discouraging  disposable    dispelled     distracting   disruptive    distribute   
#[7] dispose       distiller     disposables   distractions  distributors  disposal     
#[13] discoveries   distributing  discount      disposals     disposer      distributed  
#[19] dismemberment discouraged  



unique(dis_complex[dis_complex$SemanticTransparency=="opaque" & dis_complex$TransitionType=="s#V","item.x"])
#[1] disease  diseases




#3. RelDur
summary (dis_complex$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6654  1.3850  1.8190  1.9700  2.4230  5.2760 
   

sd (dis_complex$RelDur)
#[1] 0.8713318


# we should also repport the meann, median and sd for each level

# first doubles

summary (dis_complex[dis_complex$TransitionType=="s#s",]$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8164  1.7990  2.4250  2.6370  3.3030  5.2760 

sd (dis_complex[dis_complex$TransitionType=="s#s",]$RelDur)
#[1] 1.207216


#disV

summary (dis_complex[dis_complex$TransitionType=="s#V",]$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.6654  1.1250  1.5690  1.6340  2.0580  3.3180 


sd (dis_complex[dis_complex$TransitionType=="s#V",]$RelDur)
#[1] 0.6539561

# disC

summary (dis_complex[dis_complex$TransitionType=="s#C",]$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.114   1.494   2.043   2.055   2.509   3.931 


sd (dis_complex[dis_complex$TransitionType=="s#C",]$RelDur)
#[1] 0.6813256



#now let's conduct an anova to see whwther the doff are sign

anova.dis<-aov(RelDur~TransitionType,data = dis_complex)

summary(anova.dis)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# TransitionType   2  17.67   8.836   14.03 3.2e-06 ***
#   Residuals      125  78.75   0.630                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# now let's see whether all the differneces are sign

TukeyHSD(anova.dis)
# $TransitionType
# diff        lwr         upr     p adj
# s#V-s#C -0.4217382 -0.7943514 -0.04912503 0.0223050
# s#s-s#C  0.5813598  0.1054918  1.05722773 0.0122627
# s#s-s#V  1.0030980  0.5472907  1.45890520 0.0000022

library(multcomp)
Tuk.dis2<-glht(anova.dis,linfct=mcp(TransitionType="Tukey"))

summary(Tuk.dis2)

# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
# s#V - s#C == 0  -0.4217     0.1571  -2.685    0.022 *  
# s#s - s#C == 0   0.5814     0.2006   2.898    0.012 *  
# s#s - s#V == 0   1.0031     0.1922   5.220   <0.001 ***
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- single-step method)

# all of the differences are significant: before voiced consonants
# the vowel is longer, hence in RelDur, the voicing is out balanced..

#4. WordFormFreq

summary (dis_complex$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.099   7.136   7.751   7.875   9.146  10.740 

sd (dis_complex$logWordFormFreqAllCoca)
#[1] 1.735773


#5. RelFreq

summary (dis_complex$logRelFreq)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.      
#-8.72400 -1.82100 -0.58780  0.04353  2.14900  9.36000

sd (dis_complex$logRelFreq)
#[1] 2.938364


#6. LocSpeech

summary (dis_complex$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.40   10.78   12.91   13.02   15.55   22.44

sd(dis_complex$LocSpeech)
#[1] 3.163679


#7. PrecSegDur

summary (dis_complex$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02697 0.04571 0.05314 0.05782 0.06753 0.13430

sd(dis_complex$PrecSegDur)
#[1] 0.01942431




#8. AdjSyllableStress

summary (dis_complex$AdjSyllableStress)
#p   u 
#88 40  

#9. Voicing

# so we need to recode this
list_voiced=list("disease", "diseases", "dissolve", "dissolved", "dissolving")
voicing=list()

for (i in dis_complex$item.x){
  if (i %in% list_voiced)
  {voicing= append(voicing, "voiced")}
  else
  {voicing= append(voicing, "voiceless")}
  
}
dis_complex$Voicing=as.factor(as.character(voicing))

# nowe we have the variable voicing. Let's have a look at it

table(dis_complex$Voicing)
# voiced voiceless 
# 24       104 

# we need to include this in our model, as in the other model (AbsDur)

#########################################################################################
# Now let's do an initial model:                                                        #
#########################################################################################
rownames(dis_complex)<-1:nrow(dis_complex)

dis.lm <- lm (RelDur ~ TransitionType+Voicing+SemanticTransparency+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_complex)

summary (dis.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3126 -0.5084 -0.1505  0.4573  2.3696 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.81348    0.61427   1.324 0.187940    
#   TransitionTypes#V                0.15027    0.22354   0.672 0.502753    
#   TransitionTypes#s                0.92309    0.21305   4.333 3.09e-05 ***
#   Voicingvoiceless                 1.32255    0.35859   3.688 0.000342 ***
#   SemanticTransparencytransparent -0.31643    0.23834  -1.328 0.186837    
#   logRelFreq                       0.03248    0.03206   1.013 0.313098    
#   logWordFormFreqAllCoca           0.03709    0.05360   0.692 0.490256    
#   LocSpeech                       -0.01226    0.02396  -0.512 0.609905    
#   AdjSyllableStressu              -0.35602    0.20152  -1.767 0.079846 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7509 on 119 degrees of freedom
# Multiple R-squared:  0.3041,	Adjusted R-squared:  0.2573 
# F-statistic:   6.5 on 8 and 119 DF,  p-value: 5.244e-07


#Before we go on fitting the model, we should check the assumptions od the model:

qqnorm (residuals (dis.lm))
qqline (residuals (dis.lm))
plot(dis.lm)


# let' see what happens if we transform the dep variable 

bc <- boxcox(dis.lm)

lambda <- bc$x[which.max(bc$y)]

dis_complex$bc <- dis_complex$RelDur^lambda

dis.lmBC <- lm (bc ~ TransitionType+Voicing+SemanticTransparency+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_complex)

summary (dis.lmBC)


# Call:
#   lm(formula = bc ~ TransitionType + Voicing + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.094621 -0.028548 -0.001544  0.027650  0.084487 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.9868994  0.0328679  30.026  < 2e-16 ***
#   TransitionTypes#V                0.0046872  0.0119610   0.392 0.695852    
# TransitionTypes#s                0.0411381  0.0113998   3.609 0.000451 ***
# Voicingvoiceless                 0.0807125  0.0191872   4.207 5.05e-05 ***
#   SemanticTransparencytransparent -0.0178561  0.0127528  -1.400 0.164064    
# logRelFreq                       0.0014160  0.0017156   0.825 0.410818    
# logWordFormFreqAllCoca           0.0033392  0.0028680   1.164 0.246639    
# LocSpeech                       -0.0008032  0.0012821  -0.626 0.532219    
# AdjSyllableStressu              -0.0193241  0.0107826  -1.792 0.075649 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04018 on 119 degrees of freedom
# Multiple R-squared:  0.3098,	Adjusted R-squared:  0.2634 
# F-statistic: 6.678 on 8 and 119 DF,  p-value: 3.373e-07

# became better


qqnorm (residuals (dis.lmBC))
qqline (residuals (dis.lmBC))
plot(dis.lmBC)

# does not look better

# let's try to remove the outliers from model one
qqnorm (residuals (dis.lm))
qqline (residuals (dis.lm))
plot(dis.lm)

#77,123,84


# So, let's go on without them: We create the new dataset dis2

dis_2 <- dis_complex [-c(77,123,84), ]

# now, let's do another model

dis_2.lm <- lm (RelDur ~ TransitionType+Voicing+SemanticTransparency+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_2)

summary (dis_2.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2102 -0.5135 -0.1487  0.4705  1.7562 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.45202    0.55958   0.808   0.4209    
# TransitionTypes#V                0.03250    0.20059   0.162   0.8716    
# TransitionTypes#s                0.62834    0.19664   3.195   0.0018 ** 
# Voicingvoiceless                 1.31280    0.32061   4.095 7.85e-05 ***
#   SemanticTransparencytransparent -0.28903    0.21334  -1.355   0.1781    
# logRelFreq                       0.03163    0.02886   1.096   0.2753    
# logWordFormFreqAllCoca           0.09505    0.05063   1.877   0.0630 .  
# LocSpeech                       -0.01726    0.02148  -0.803   0.4234    
# AdjSyllableStressu              -0.30945    0.18101  -1.710   0.0900 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6692 on 116 degrees of freedom
# Multiple R-squared:  0.2709,	Adjusted R-squared:  0.2206 
# F-statistic: 5.388 on 8 and 116 DF,  p-value: 9.043e-06

qqnorm (residuals (dis_2.lm))
qqline (residuals (dis_2.lm))

# I think that looks even worse!
# We should try log!


dis.lm1<- lm (log(RelDur) ~ TransitionType+Voicing+SemanticTransparency+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_complex)

summary (dis.lm1)

# Call:
#   lm(formula = log(RelDur) ~ TransitionType + Voicing + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.90870 -0.27153 -0.00992  0.25917  0.77712 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     -0.123481   0.306975  -0.402 0.688222    
# TransitionTypes#V                0.039816   0.111712   0.356 0.722161    
# TransitionTypes#s                0.373523   0.106470   3.508 0.000637 ***
# Voicingvoiceless                 0.759981   0.179202   4.241 4.42e-05 ***
#   SemanticTransparencytransparent -0.166522   0.119107  -1.398 0.164686    
# logRelFreq                       0.012764   0.016023   0.797 0.427252    
# logWordFormFreqAllCoca           0.032050   0.026786   1.196 0.233883    
# LocSpeech                       -0.007539   0.011974  -0.630 0.530184    
# AdjSyllableStressu              -0.179120   0.100706  -1.779 0.077851 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3753 on 119 degrees of freedom
# Multiple R-squared:  0.3097,	Adjusted R-squared:  0.2633 
# F-statistic: 6.674 on 8 and 119 DF,  p-value: 3.409e-07

qqnorm (residuals (dis.lm1))
qqline (residuals (dis.lm1))

# even worse!!!!!

# I think the best onw is the first one

plot(dis.lm)

# we will go with tha model


#####################################################################
# Let's now look at interactions
#####################################################################

# There might be an interactions.
# We will check them systematically:

#1.Semantic Transparency and TransitionType

dis_int_SemT_Tr.lm <- lm (RelDur ~ TransitionType*SemanticTransparency, data = dis_complex)

summary (dis_int_SemT_Tr.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType * SemanticTransparency, 
#      data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4300 -0.5222 -0.1252  0.5226  3.0300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        2.27204    0.15387  14.766  < 2e-16 ***
#   TransitionTypes#V                                 -0.87551    0.23081  -3.793 0.000233 ***
# TransitionTypes#s                                 -0.02557    0.29907  -0.085 0.932012    
# SemanticTransparencytransparent                   -0.48725    0.23081  -2.111 0.036810 *  
#   TransitionTypes#V:SemanticTransparencytransparent  0.84612    0.31312   2.702 0.007870 ** 
# TransitionTypes#s:SemanticTransparencytransparent  1.11184    0.39812   2.793 0.006070 ** 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7694 on 122 degrees of freedom
# Multiple R-squared:  0.2511,	Adjusted R-squared:  0.2204 
# F-statistic: 8.179 on 5 and 122 DF,  p-value: 1.095e-06

# YES!!!!

#2.Frequencies and TransitionType?

dis_int_WordFr_Tr.lm <- lm (RelDur ~ TransitionType*logWordFormFreqAllCoca, data = dis_complex)

summary (dis_int_WordFr_Tr.lm)


# Call:
#   lm(formula = RelDur ~ TransitionType * logWordFormFreqAllCoca, 
#      data = dis_complex)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.80344 -0.59651 -0.05781  0.46715  2.17304 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.55318    0.49875   3.114  0.00230 ** 
#   TransitionTypes#V                         0.77253    0.77316   0.999  0.31968    
# TransitionTypes#s                         3.97646    0.98232   4.048 9.11e-05 ***
# logWordFormFreqAllCoca                    0.06905    0.06677   1.034  0.30313    
# TransitionTypes#V:logWordFormFreqAllCoca -0.14823    0.09434  -1.571  0.11873    
# TransitionTypes#s:logWordFormFreqAllCoca -0.48963    0.13817  -3.544  0.00056 ***
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7594 on 122 degrees of freedom
# Multiple R-squared:  0.2704,	Adjusted R-squared:  0.2405 
# F-statistic: 9.044 on 5 and 122 DF,  p-value: 2.462e-07

# interesting...

# 3. rel frequencies and TransitionType?

dis_int_RelFr_Tr.lm <- lm (RelDur ~ TransitionType*logRelFreq, data = dis_complex)

summary (dis_int_RelFr_Tr.lm)


# Call:
#   lm(formula = RelDur ~ TransitionType * logRelFreq, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.8140 -0.5499 -0.1139  0.4836  2.6595 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   2.02055    0.11750  17.196   <2e-16 ***
#   TransitionTypes#V            -0.35925    0.15681  -2.291   0.0237 *  
# TransitionTypes#s             0.59641    0.23026   2.590   0.0108 *  
# logRelFreq                    0.07255    0.03192   2.273   0.0248 *  
#   TransitionTypes#V:logRelFreq -0.13581    0.05739  -2.366   0.0195 *  
# TransitionTypes#s:logRelFreq -0.08397    0.07456  -1.126   0.2623    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7815 on 122 degrees of freedom
# Multiple R-squared:  0.2273,	Adjusted R-squared:  0.1957 
# F-statistic: 7.179 on 5 and 122 DF,  p-value: 6.389e-06

#4. rel frequencies and ST?

# not valid

#5. speechRate and ST?

dis_int_SpeechRate_Sem.lm <- lm (RelDur ~ SemanticTransparency*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_Sem.lm)
# Call:
#   lm(formula = RelDur ~ SemanticTransparency * LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3963 -0.5978 -0.1946  0.4940  3.0341 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                0.92820    0.46460   1.998   0.0479 *
#   SemanticTransparencytransparent            1.14907    0.66179   1.736   0.0850 .
# LocSpeech                                  0.08344    0.03694   2.259   0.0256 *
#   SemanticTransparencytransparent:LocSpeech -0.08988    0.05003  -1.797   0.0748 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8638 on 124 degrees of freedom
# Multiple R-squared:  0.04045,	Adjusted R-squared:  0.01723 
# F-statistic: 1.742 on 3 and 124 DF,  p-value: 0.1618


# marginally sign.

#6. speechRate and RelFreq?

dis_int_SpeechRate_RelF.lm <- lm (RelDur ~ logRelFreq*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_RelF.lm)

#no

#6. speechRate and Trans?

dis_int_SpeechRate_TT.lm <- lm (RelDur ~ TransitionType*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_TT.lm)

#no


#6. Stress and Trans?

dis_int_TT_Stress.lm <- lm (RelDur ~ AdjSyllableStress*TransitionType, data = dis_complex)

summary (dis_int_TT_Stress.lm)

#YES! there might be something

#########################################################################################
#   summary interactions: 
#     - there is an interaction between Freq and TransitionType 
#     - there is an interaction between SemTran and TransitionType 
#     - there is no interaction between LocSpeech and RelFreq
#     - there is no interaction between LocSpeech and ST 
#     - there is an interaction between LocSpeech and TT
#     - there might be an interaction between Stress and TransitionType                      ##
#     - there is an interaction between RelFreq and TransitionType                      #
#########################################################################################


# We will simplify the model first and the look at the interactions again

summary(dis.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3126 -0.5084 -0.1505  0.4573  2.3696 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.81348    0.61427   1.324 0.187940    
# TransitionTypes#V                0.15027    0.22354   0.672 0.502753    
# TransitionTypes#s                0.92309    0.21305   4.333 3.09e-05 ***
# Voicingvoiceless                 1.32255    0.35859   3.688 0.000342 ***
#   SemanticTransparencytransparent -0.31643    0.23834  -1.328 0.186837    
# logRelFreq                       0.03248    0.03206   1.013 0.313098    
# logWordFormFreqAllCoca           0.03709    0.05360   0.692 0.490256    
# LocSpeech                       -0.01226    0.02396  -0.512 0.609905    
# AdjSyllableStressu              -0.35602    0.20152  -1.767 0.079846 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7509 on 119 degrees of freedom
# Multiple R-squared:  0.3041,	Adjusted R-squared:  0.2573 
# F-statistic:   6.5 on 8 and 119 DF,  p-value: 5.244e-07




# okay now we have to get rid of Rel freq

# The two frequenvy varoables are fairly similar. Which is a better predictor?
# let's first check how RelFreq and WordFreq relate to each other

dis_int_RelFr.lm <- lm (RelDur ~ logRelFreq, data = dis_complex)

summary (dis_int_RelFr.lm)

# Call:
#   lm(formula = RelDur ~ logRelFreq, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2742 -0.5912 -0.1778  0.4354  3.3060 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.970478   0.077295  25.493   <2e-16 ***
#   logRelFreq  -0.008828   0.026406  -0.334    0.739    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8744 on 126 degrees of freedom
# Multiple R-squared:  0.0008864,	Adjusted R-squared:  -0.007043 
# F-statistic: 0.1118 on 1 and 126 DF,  p-value: 0.7387

# not significant!!!!

dis_int_WordFr.lm <- lm (RelDur ~ logWordFormFreqAllCoca, data = dis_complex)

summary (dis_int_WordFr.lm)

# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3264 -0.6495 -0.1490  0.5111  2.5796 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.06758    0.34640   8.856 6.51e-15 ***
#   logWordFormFreqAllCoca -0.13936    0.04296  -3.244  0.00151 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8404 on 126 degrees of freedom
# Multiple R-squared:  0.07707,	Adjusted R-squared:  0.06975 
# F-statistic: 10.52 on 1 and 126 DF,  p-value: 0.001511

#word freq is significant

dis_int_WordFr_RelFreq.lm <- lm (RelDur ~ logWordFormFreqAllCoca+logRelFreq, data = dis_complex)

summary (dis_int_WordFr_RelFreq.lm)


# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca + logRelFreq, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3174 -0.6545 -0.1313  0.4794  2.5935 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.11704    0.35952   8.670  1.9e-14 ***
#   logWordFormFreqAllCoca -0.14572    0.04470  -3.260  0.00144 ** 
#   logRelFreq              0.01410    0.02641   0.534  0.59427    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8428 on 125 degrees of freedom
# Multiple R-squared:  0.07917,	Adjusted R-squared:  0.06444 
# F-statistic: 5.374 on 2 and 125 DF,  p-value: 0.00577

# WordFormFreq has a better effect.

# so we throw out RelFreq


dis_complex.lm2<-update(dis.lm, ~ . - logRelFreq)

summary(dis_complex.lm2)


# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3476 -0.5139 -0.1267  0.4791  2.4129 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.68341    0.60077   1.138   0.2576    
#   TransitionTypes#V                0.20708    0.21641   0.957   0.3406    
#   TransitionTypes#s                0.89400    0.21113   4.234 4.51e-05 ***
#   Voicingvoiceless                 1.41109    0.34782   4.057 8.88e-05 ***
#   SemanticTransparencytransparent -0.44591    0.20119  -2.216   0.0286 *  
#   logWordFormFreqAllCoca           0.05305    0.05124   1.035   0.3026    
#   LocSpeech                       -0.01163    0.02396  -0.486   0.6282    
#   AdjSyllableStressu              -0.42034    0.19127  -2.198   0.0299 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.751 on 120 degrees of freedom
# Multiple R-squared:  0.2981,	Adjusted R-squared:  0.2571 
# F-statistic:  7.28 on 7 and 120 DF,  p-value: 2.886e-07

#Now LocSpeech


dis_complex.lm3<-update(dis_complex.lm2, ~ . - LocSpeech)

summary(dis_complex.lm3)

# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        logWordFormFreqAllCoca + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2888 -0.5204 -0.1089  0.4919  2.3806 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.58975    0.56715   1.040   0.3005    
#   TransitionTypes#V                0.20416    0.21565   0.947   0.3457    
#   TransitionTypes#s                0.89469    0.21046   4.251 4.21e-05 ***
#   Voicingvoiceless                 1.35544    0.32736   4.141 6.44e-05 ***
#   SemanticTransparencytransparent -0.44188    0.20038  -2.205   0.0293 *  
#   logWordFormFreqAllCoca           0.05096    0.05090   1.001   0.3187    
#   AdjSyllableStressu              -0.41181    0.18986  -2.169   0.0320 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7486 on 121 degrees of freedom
# Multiple R-squared:  0.2967,	Adjusted R-squared:  0.2618 
# F-statistic: 8.508 on 6 and 121 DF,  p-value: 1.032e-07


# now word freq

dis_complex.lm4<-update(dis_complex.lm3, ~ . - logWordFormFreqAllCoca)

summary(dis_complex.lm4)
# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3176 -0.5401 -0.1181  0.4884  2.1517 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       1.0991     0.2508   4.383 2.50e-05 ***
#   TransitionTypes#V                 0.1958     0.2155   0.909   0.3653    
#   TransitionTypes#s                 0.8383     0.2028   4.134 6.58e-05 ***
#   Voicingvoiceless                  1.1873     0.2810   4.225 4.63e-05 ***
#   SemanticTransparencytransparent  -0.3899     0.1936  -2.015   0.0461 *  
#   AdjSyllableStressu               -0.3702     0.1852  -1.998   0.0479 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7486 on 122 degrees of freedom
# Multiple R-squared:  0.2909,	Adjusted R-squared:  0.2618 
# F-statistic: 10.01 on 5 and 122 DF,  p-value: 4.815e-08


# So this would be the final model without any interactions

# What I find out so far:

# - Doubles are longer than singles 
# - after unstressed: longer
# - transparent are shorter?


# We need to change the reference levels of TransitionType to doubles
# to see whether they are longer than both singletons

dis_complex$TransitionType <- relevel (dis_complex$TransitionType, ref= "s#s")

# now let's redo the model

dis_complex.lm7<- lm(RelDur ~ TransitionType + Voicing + SemanticTransparency + 
                              AdjSyllableStress, data=dis_complex)

summary(dis_complex.lm7)


# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3176 -0.5401 -0.1181  0.4884  2.1517 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       1.9374     0.2311   8.384 1.04e-13 ***
#   TransitionTypes#C                -0.8383     0.2028  -4.134 6.58e-05 ***
#   TransitionTypes#V                -0.6425     0.2080  -3.089  0.00249 ** 
#   Voicingvoiceless                  1.1873     0.2810   4.225 4.63e-05 ***
#   SemanticTransparencytransparent  -0.3899     0.1936  -2.015  0.04614 *  
#   AdjSyllableStressu               -0.3702     0.1852  -1.998  0.04792 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7486 on 122 degrees of freedom
# Multiple R-squared:  0.2909,	Adjusted R-squared:  0.2618 
# F-statistic: 10.01 on 5 and 122 DF,  p-value: 4.815e-08

# okay so doubles are longer! dis geminates
########################################################
# now let's take a look at the interactions again
###########################################################

# first RelFreq and TrType

dis_complex.lm6InterRel<- lm(RelDur ~ TransitionType*logRelFreq+Voicing + SemanticTransparency + AdjSyllableStress, data = dis_complex)

summary(dis_complex.lm6InterRel)

#no interaction. So we can forget about RelFreq but 



# What if we let TrType amd ST interact

dis_complex.lm6InterSem<- lm(RelDur ~ TransitionType*SemanticTransparency+Voicing   + AdjSyllableStress,data = dis_complex)


summary(dis_complex.lm6InterSem)

# Call:
#   lm(formula = RelDur ~ TransitionType * SemanticTransparency + 
#        Voicing + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3893 -0.5406 -0.1323  0.5017  2.2252 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        1.42926    0.37239   3.838 0.000199 ***
#   TransitionTypes#C                                 -0.76914    0.38239  -2.011 0.046525 *  
#   TransitionTypes#V                                 -0.03274    0.40793  -0.080 0.936165    
#   SemanticTransparencytransparent                   -0.18018    0.40368  -0.446 0.656155    
#   Voicingvoiceless                                   1.62198    0.51444   3.153 0.002042 ** 
#   AdjSyllableStressu                                -0.25168    0.20435  -1.232 0.220500    
#   TransitionTypes#C:SemanticTransparencytransparent -0.24163    0.47826  -0.505 0.614320    
#   TransitionTypes#V:SemanticTransparencytransparent -0.88933    0.49226  -1.807 0.073329 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7448 on 120 degrees of freedom
# Multiple R-squared:  0.3097,	Adjusted R-squared:  0.2694 
# F-statistic: 7.689 on 7 and 120 DF,  p-value: 1.165e-07

# marginally

# what about an interaction with stress pattern


dis.lm6InterStressTT<- lm(RelDur ~ TransitionType*AdjSyllableStress+ SemanticTransparency+Voicing, data = dis_complex)


summary(dis.lm6InterStressTT)

# no!


# and Freq and TT?
dis_complex.lm6InterFreq<- lm(RelDur ~ TransitionType*logWordFormFreqAllCoca+SemanticTransparency+Voicing+AdjSyllableStress,data = dis_complex)

summary(dis_complex.lm6InterFreq)
# Call:
#   lm(formula = RelDur ~ TransitionType * logWordFormFreqAllCoca + 
#        SemanticTransparency + Voicing + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2247 -0.5432 -0.1041  0.4445  2.0041 
# 
# Coefficients:
#                                              Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                3.9532     0.9006   4.390 2.47e-05 ***
#   TransitionTypes#C                         -3.8122     0.9389  -4.060 8.81e-05 ***
#   TransitionTypes#V                         -3.3720     1.0429  -3.233 0.001584 ** 
#   logWordFormFreqAllCoca                    -0.3035     0.1196  -2.538 0.012435 *  
#   SemanticTransparencytransparent           -0.3810     0.1994  -1.911 0.058403 .  
#   Voicingvoiceless                           1.2705     0.3253   3.906 0.000156 ***
#   AdjSyllableStressu                        -0.3965     0.1844  -2.150 0.033576 *  
#   TransitionTypes#C:logWordFormFreqAllCoca   0.4238     0.1327   3.192 0.001806 ** 
#   TransitionTypes#V:logWordFormFreqAllCoca   0.3797     0.1379   2.754 0.006806 ** 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7231 on 119 degrees of freedom
# Multiple R-squared:  0.3547,	Adjusted R-squared:  0.3113 
# F-statistic: 8.175 on 8 and 119 DF,  p-value: 9.115e-09

# yes

# and LocSpeech and TT

dis_complex.lm6InterLoc<- lm(RelDur ~ TransitionType*LocSpeech+SemanticTransparency + Voicing + AdjSyllableStress, data = dis_complex)

summary(dis_complex.lm6InterLoc)
# no


# and finally stress and ST
dis_complex.lm6InterStressST<- lm(RelDur ~ TransitionType+ Voicing +SemanticTransparency * AdjSyllableStress, data = dis_complex)

summary(dis_complex.lm6InterStressST)

#no

# Thus we have several possible final models: 


summary(dis_complex.lm7)


# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3176 -0.5401 -0.1181  0.4884  2.1517 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       1.9374     0.2311   8.384 1.04e-13 ***
#   TransitionTypes#C                -0.8383     0.2028  -4.134 6.58e-05 ***
#   TransitionTypes#V                -0.6425     0.2080  -3.089  0.00249 ** 
#   Voicingvoiceless                  1.1873     0.2810   4.225 4.63e-05 ***
#   SemanticTransparencytransparent  -0.3899     0.1936  -2.015  0.04614 *  
#   AdjSyllableStressu               -0.3702     0.1852  -1.998  0.04792 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7486 on 122 degrees of freedom
# Multiple R-squared:  0.2909,	Adjusted R-squared:  0.2618 
# F-statistic: 10.01 on 5 and 122 DF,  p-value: 4.815e-08

visreg(dis_complex.lm7)


#OR

summary(dis_complex.lm6InterSem)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                        1.42926    0.37239   3.838 0.000199 ***
#   TransitionTypes#C                                 -0.76914    0.38239  -2.011 0.046525 *  
#   TransitionTypes#V                                 -0.03274    0.40793  -0.080 0.936165    
#   SemanticTransparencytransparent                   -0.18018    0.40368  -0.446 0.656155    
#   Voicingvoiceless                                   1.62198    0.51444   3.153 0.002042 ** 
#   AdjSyllableStressu                                -0.25168    0.20435  -1.232 0.220500    
#   TransitionTypes#C:SemanticTransparencytransparent -0.24163    0.47826  -0.505 0.614320    
#   TransitionTypes#V:SemanticTransparencytransparent -0.88933    0.49226  -1.807 0.073329 . 

#Multiple R-squared:  0.3097,	Adjusted R-squared:  0.2694 

visreg(dis_complex.lm6InterSem, "TransitionType", by="SemanticTransparency")
visreg(dis_complex.lm6InterSem, "SemanticTransparency", by="TransitionType")

#and

summary(dis_complex.lm6InterFreq)

# Call:
#   lm(formula = RelDur ~ TransitionType * logWordFormFreqAllCoca + 
#        SemanticTransparency + Voicing + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2247 -0.5432 -0.1041  0.4445  2.0041 
# 
# Coefficients:
#                                              Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                3.9532     0.9006   4.390 2.47e-05 ***
#   TransitionTypes#C                         -3.8122     0.9389  -4.060 8.81e-05 ***
#   TransitionTypes#V                         -3.3720     1.0429  -3.233 0.001584 ** 
#   logWordFormFreqAllCoca                    -0.3035     0.1196  -2.538 0.012435 *  
#   SemanticTransparencytransparent           -0.3810     0.1994  -1.911 0.058403 .  
#   Voicingvoiceless                           1.2705     0.3253   3.906 0.000156 ***
#   AdjSyllableStressu                        -0.3965     0.1844  -2.150 0.033576 *  
#   TransitionTypes#C:logWordFormFreqAllCoca   0.4238     0.1327   3.192 0.001806 ** 
#   TransitionTypes#V:logWordFormFreqAllCoca   0.3797     0.1379   2.754 0.006806 ** 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7231 on 119 degrees of freedom
# Multiple R-squared:  0.3547,	Adjusted R-squared:  0.3113 
# F-statistic: 8.175 on 8 and 119 DF,  p-value: 9.115e-09

visreg(dis_complex.lm6InterFreq, "TransitionType", by="logWordFormFreqAllCoca")
visreg(dis_complex.lm6InterFreq, "logWordFormFreqAllCoca", by="TransitionType")

####################################################################################
#                 summary
#
# - An interaction between Semantic transparency and TransitionType
#       - barely significant. The difference which is visible is that s#V opaque
#         items are longer than transparent items. This is however a fallacs.
#     All opaque s#V are voiced. Voiced consonants lead to a lenghtening of
#     the preceding vowel. Hence, the difference in relative duration is even
#     longer than in absolute duration. Since opacity and voicing go to
#     together in the case of disV, opaque items are estimated to be longer than
#     transparent items. Also, since there is a 1:1 distribution of Semantic
#     Transparency and Voicing, an intercation between the two cannot be implemented.
#     We hence disregard this interaction.
#
# - An interaction between Semantic transparency and WordFormFrequency:
#       - for s#s: shorter when more frequent --> more reduced than the vowel
#       - for s#C/s#V: longer when more frequent --> vowel is more reduced
#   BUT:one can't use this model because of the freq distribution_ There
#     are not items with a double and high frequency - hence

#   WE HENCE TAKE THE MODEL WITHOUT INTERACTIONS AS OUR FINAL MODEL:

  summary(dis_complex.lm7)

# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + SemanticTransparency + 
#        AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3176 -0.5401 -0.1181  0.4884  2.1517 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       1.9374     0.2311   8.384 1.04e-13 ***
#   TransitionTypes#C                -0.8383     0.2028  -4.134 6.58e-05 ***
#   TransitionTypes#V                -0.6425     0.2080  -3.089  0.00249 ** 
#   Voicingvoiceless                  1.1873     0.2810   4.225 4.63e-05 ***
#   SemanticTransparencytransparent  -0.3899     0.1936  -2.015  0.04614 *  
#   AdjSyllableStressu               -0.3702     0.1852  -1.998  0.04792 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7486 on 122 degrees of freedom
# Multiple R-squared:  0.2909,	Adjusted R-squared:  0.2618 
# F-statistic: 10.01 on 5 and 122 DF,  p-value: 4.815e-08

#   Otherwise expected effects of voicing, stress - no effect of speech rate:
#   probably just the same reduction for vowels and consonsts. Marginally
#   significant effect for SemanticTransparency - transparent items are longer:
#   however this might be due to the distribution of voicing and opacity. Let's
#   implement a model with only voiceless items and see what happens.
##############################################################################

# we need to rename the levels

levels(dis_complex$TransitionType)
#[1] "s#s" "s#C" "s#V"

levels(dis_complex$TransitionType)<-c ( "s#sV", "s#C", "s#V")

levels(dis_complex$TransitionType)
#[1] "s#sV" "s#C"  "s#V" 

dis_complex.lm7<-lm(RelDur ~ TransitionType + Voicing + SemanticTransparency + AdjSyllableStress, data = dis_complex)

##############################################################################
# first we need to save the intercation plot.

png("dis- model Relative Dur TransitionType.png", units="cm", height=15, width=15, res=300, pointsize=15)

visreg(dis_complex.lm7, "TransitionType",partial=FALSE,rug=F, ylab="relative duration", xlab="environment",  cex.axis=0.8)

dev.off()

#####################################################################################################


# However,let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 

model_ranking <- dredge(dis.lm)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = RelDur ~ <128 unique rhs>, data = dis_complex)
# 
# Component models: 
#   df  logLik   AICc delta weight
# 1367     7 -141.37 297.68  0.00   0.11
# 367      6 -142.61 297.92  0.24   0.10
# 1567     7 -141.49 297.92  0.24   0.10
# 13567    8 -140.66 298.54  0.85   0.07
# 3567     7 -142.03 299.00  1.32   0.06
# 14567    8 -140.97 299.14  1.46   0.05
# 12367    8 -141.26 299.72  2.04   0.04
# 567      6 -143.56 299.80  2.12   0.04
# 167      6 -143.59 299.87  2.19   0.04
# 13467    8 -141.36 299.92  2.24   0.04
# 2367     7 -142.54 300.01  2.33   0.04
# 12567    8 -141.41 300.03  2.35   0.03
# 3467     7 -142.59 300.11  2.43   0.03
# 134567   9 -140.43 300.39  2.71   0.03
# 123567   9 -140.55 300.62  2.94   0.03
# 23567    8 -141.96 301.14  3.46   0.02
# 124567   9 -140.84 301.21  3.53   0.02
# 34567    8 -142.02 301.25  3.57   0.02
# 4567     7 -143.41 301.75  4.07   0.01
# 1467     7 -143.49 301.91  4.23   0.01
# 1267     7 -143.52 301.98  4.30   0.01
# 2567     7 -143.52 301.98  4.30   0.01
# 123467   9 -141.23 301.99  4.31   0.01
# 67       5 -145.87 302.23  4.55   0.01
# 23467    8 -142.52 302.25  4.57   0.01
# 1234567 10 -140.29 302.46  4.78   0.01
# 234567   9 -141.95 303.42  5.74   0.01
# 24567    8 -143.37 303.94  6.26   0.00
# 12467    8 -143.41 304.03  6.34   0.00
# 267      6 -145.85 304.39  6.71   0.00
# 467      6 -145.87 304.43  6.75   0.00
# 2467     7 -145.85 306.63  8.95   0.00
# 6        4 -150.53 309.40 11.71   0.00
# 46       5 -149.65 309.80 12.12   0.00
# 3456     7 -147.44 309.81 12.13   0.00
# 346      6 -148.75 310.19 12.51   0.00
# 356      6 -148.76 310.22 12.53   0.00
# 26       5 -149.91 310.32 12.64   0.00
# 36       5 -150.03 310.55 12.86   0.00
# 246      6 -149.20 311.10 13.42   0.00
# 56       5 -150.32 311.12 13.44   0.00
# 236      6 -149.25 311.20 13.52   0.00
# 2346     7 -148.17 311.26 13.58   0.00
# 17       4 -151.52 311.37 13.69   0.00
# 16       5 -150.53 311.55 13.87   0.00
# 23456    8 -147.22 311.65 13.96   0.00
# 2356     7 -148.40 311.73 14.05   0.00
# 456      6 -149.53 311.75 14.07   0.00
# 146      6 -149.64 311.98 14.30   0.00
# 157      5 -150.75 311.99 14.31   0.00
# 13456    8 -147.44 312.09 14.41   0.00
# 1346     7 -148.65 312.24 14.55   0.00
# 256      6 -149.85 312.39 14.71   0.00
# 126      6 -149.88 312.45 14.76   0.00
# 1356     7 -148.76 312.45 14.77   0.00
# 136      6 -149.96 312.61 14.93   0.00
# 156      6 -150.23 313.16 15.48   0.00
# 147      5 -151.37 313.22 15.54   0.00
# 1246     7 -149.15 313.24 15.56   0.00
# 127      5 -151.38 313.24 15.56   0.00
# 2456     7 -149.17 313.28 15.60   0.00
# 137      5 -151.44 313.37 15.69   0.00
# 1236     7 -149.23 313.40 15.72   0.00
# 12346    8 -148.12 313.46 15.78   0.00
# 1456     7 -149.45 313.83 16.15   0.00
# 1257     6 -150.59 313.88 16.20   0.00
# 123456   9 -147.22 313.96 16.28   0.00
# 12356    8 -148.40 314.00 16.32   0.00
# 1457     6 -150.71 314.11 16.43   0.00
# 1357     6 -150.74 314.16 16.48   0.00
# 1256     7 -149.74 314.42 16.74   0.00
# 1347     6 -151.23 315.15 17.47   0.00
# 1247     6 -151.24 315.18 17.50   0.00
# 1237     6 -151.28 315.26 17.57   0.00
# 12456    8 -149.07 315.34 17.66   0.00
# 12457    7 -150.57 316.06 18.38   0.00
# 12357    7 -150.58 316.10 18.42   0.00
# 13457    7 -150.71 316.35 18.67   0.00
# 12347    7 -151.09 317.12 19.44   0.00
# 57       4 -154.43 317.18 19.50   0.00
# 457      5 -153.89 318.28 20.60   0.00
# 123457   8 -150.56 318.34 20.66   0.00
# 257      5 -154.36 319.20 21.52   0.00
# 47       4 -155.44 319.20 21.52   0.00
# 357      5 -154.43 319.34 21.66   0.00
# 347      5 -154.63 319.75 22.07   0.00
# 7        3 -156.83 319.85 22.17   0.00
# 3457     6 -153.82 320.34 22.66   0.00
# 2457     6 -153.85 320.39 22.70   0.00
# 37       4 -156.25 320.83 23.15   0.00
# 247      5 -155.42 321.33 23.65   0.00
# 2357     6 -154.35 321.40 23.72   0.00
# 2347     6 -154.58 321.86 24.18   0.00
# 27       4 -156.79 321.90 24.22   0.00
# 23457    7 -153.77 322.47 24.79   0.00
# 237      5 -156.18 322.85 25.17   0.00
# 4        3 -158.36 322.91 25.23   0.00
# 14       4 -157.44 323.20 25.52   0.00
# 124      5 -156.90 324.29 26.60   0.00
# 24       4 -157.99 324.30 26.62   0.00
# 145      5 -157.07 324.64 26.96   0.00
# 34       4 -158.21 324.75 27.07   0.00
# 45       4 -158.35 325.02 27.34   0.00
# 134      5 -157.44 325.36 27.68   0.00
# 1245     6 -156.68 326.05 28.37   0.00
# 234      5 -157.81 326.12 28.44   0.00
# 245      5 -157.99 326.46 28.78   0.00
# 1234     6 -156.90 326.48 28.80   0.00
# 1345     6 -156.92 326.53 28.85   0.00
# 345      5 -158.05 326.60 28.92   0.00
# 12345    7 -156.57 328.06 30.38   0.00
# 2345     6 -157.75 328.19 30.51   0.00
# 12       4 -160.73 329.79 32.11   0.00
# 1        3 -162.01 330.20 32.52   0.00
# 15       4 -161.30 330.93 33.25   0.00
# (Null)   2 -163.49 331.08 33.40   0.00
# 2        3 -162.49 331.18 33.50   0.00
# 125      5 -160.36 331.20 33.52   0.00
# 123      5 -160.40 331.29 33.61   0.00
# 13       4 -161.58 331.48 33.80   0.00
# 135      5 -161.23 332.96 35.28   0.00
# 3        3 -163.44 333.06 35.38   0.00
# 5        3 -163.45 333.09 35.41   0.00
# 1235     6 -160.26 333.22 35.54   0.00
# 23       4 -162.48 333.28 35.59   0.00
# 25       4 -162.49 333.31 35.63   0.00
# 35       4 -163.43 335.18 37.50   0.00
# 235      5 -162.46 335.41 37.73   0.00
# 
# Term codes: 
#   AdjSyllableStress              LocSpeech             logRelFreq logWordFormFreqAllCoca 
# 1                      2                      3                      4 
# SemanticTransparency         TransitionType                Voicing 
# 5                      6                      7 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                      1.980542   0.388970    0.392044   5.052 4.00e-07 ***
#   AdjSyllableStressu              -0.213994   0.227828    0.228831   0.935 0.349706    
# logRelFreq                       0.033589   0.035157    0.035299   0.952 0.341329    
# TransitionTypes#C               -0.849520   0.213975    0.215965   3.934 8.37e-05 ***
# TransitionTypes#V               -0.826220   0.243469    0.245281   3.368 0.000756 ***
# Voicingvoiceless                 1.009720   0.328422    0.330565   3.055 0.002254 ** 
#   SemanticTransparencytransparent -0.177780   0.236214    0.237208   0.749 0.453575    
# logWordFormFreqAllCoca           0.006258   0.030992    0.031225   0.200 0.841157    
# LocSpeech                       -0.002491   0.013003    0.013119   0.190 0.849408    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                      1.980542   0.388970    0.392044   5.052 4.00e-07 ***
#   AdjSyllableStressu              -0.344549   0.196444    0.198312   1.737 0.082314 .  
# logRelFreq                       0.053507   0.030053    0.030318   1.765 0.077587 .  
# TransitionTypes#C               -0.850003   0.213074    0.215074   3.952 7.75e-05 ***
# TransitionTypes#V               -0.826690   0.242740    0.244558   3.380 0.000724 ***
# Voicingvoiceless                 1.013272   0.323482    0.325666   3.111 0.001862 ** 
#   SemanticTransparencytransparent -0.339048   0.227453    0.229416   1.478 0.139442    
# logWordFormFreqAllCoca           0.022500   0.055570    0.056037   0.402 0.688037    
# LocSpeech                       -0.009593   0.024145    0.024386   0.393 0.694039    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                     TransitionType Voicing logRelFreq AdjSyllableStress SemanticTransparency
# Importance:          1.00           1.00    0.63       0.62              0.52                
# N containing models:   64             64      64         64                64                
# logWordFormFreqAllCoca LocSpeech
# Importance:          0.28                   0.26     
# N containing models:   64                     64    

# All in all 9 items were removed from the dataset

# I threw out 9 observations because their morph. status could not be
# identified (Disciver (card))


anova(dis_complex.lm7)


# Analysis of Variance Table
# 
# Response: RelDur
# Df Sum Sq Mean Sq F value    Pr(>F)    
# TransitionType         2 17.672  8.8361 15.7661 8.124e-07 ***
#   Voicing                1  5.538  5.5381  9.8815  0.002096 ** 
#   SemanticTransparency   1  2.598  2.5984  4.6363  0.033270 *  
#   AdjSyllableStress      1  2.238  2.2377  3.9928  0.047920 *  
#   Residuals            122 68.375  0.5604                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# we need to calcuate the different rations for transparent and opaque items


summary(dis_complex.lm7)


# (Intercept)                       1.9374     0.2311   8.384 1.04e-13 ***
#   TransitionTypes#C                -0.8383     0.2028  -4.134 6.58e-05 ***
#   TransitionTypes#V                -0.6425     0.2080  -3.089  0.00249 ** 
#   Voicingvoiceless                  1.1873     0.2810   4.225 4.63e-05 ***
#   SemanticTransparencytransparent  -0.3899     0.1936  -2.015  0.04614 *  
#   AdjSyllableStressu               -0.3702     0.1852  -1.998  0.04792 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

visreg(dis_complex.lm7, "SemanticTransparency")

intercept= 1.9374
estimateSingleV= -0.6425
estimateSingleC= -0.8383 
estimateVoiceless= 1.1873 
estimateTransparent= -0.3899
estimateUnstressed= -0.3702

# I deided to not calculate the differneces, since itz is hard to interpret anyways and I do
# not need this right now - but if I need it later on, I can of course do it...

# now, we need a model with only voiceless items since we anticipate a change
# in the semantic transparency effect:

# we need the voiceless dataset

dis_complex_voiceless<-dis_complex[dis_complex$Voicing=="voiceless",]

dis_voiceless.lm <- lm (RelDur ~ TransitionType+SemanticTransparency+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_complex_voiceless)

summary (dis_voiceless.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4007 -0.5350 -0.1429  0.3988  2.1451 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      3.184438   0.504435   6.313 8.47e-09 ***
#   TransitionTypes#C             -0.990061   0.221011  -4.480 2.06e-05 ***
# TransitionTypes#V               -0.975180   0.265500  -3.673 0.000394 ***
# SemanticTransparencytransparent -0.228887   0.249644  -0.917 0.361517    
# logRelFreq                       0.038879   0.033417   1.163 0.247537    
# logWordFormFreqAllCoca           0.004332   0.058920   0.074 0.941538    
# LocSpeech                       -0.003674   0.025644  -0.143 0.886392    
# AdjSyllableStressu              -0.234300   0.217139  -1.079 0.283276    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7623 on 96 degrees of freedom
# Multiple R-squared:  0.2746,	Adjusted R-squared:  0.2217 
# F-statistic: 5.191 on 7 and 96 DF,  p-value: 4.83e-05



#Before we go on fitting the model, we should check the assumptions od the model:

qqnorm (residuals (dis_voiceless.lm))
qqline (residuals (dis_voiceless.lm))

# actually looks okay

#####################################################################
# Let's now look at interactions
#####################################################################

# There might be an interactions.
# We will check them systematically:


#1.Frequencies and TransitionType?

dis_int_WordFr_Tr.lm2 <- lm (RelDur ~ TransitionType*logWordFormFreqAllCoca, data = dis_complex_voiceless)

summary (dis_int_WordFr_Tr.lm2)


# Call:
#   lm(formula = RelDur ~ TransitionType * logWordFormFreqAllCoca, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2580 -0.5613 -0.1077  0.4262  1.9585 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                5.3489     0.8576   6.237 1.13e-08 ***
#   TransitionTypes#C                         -3.7957     0.9879  -3.842 0.000216 ***
# TransitionTypes#V                         -3.4050     1.1458  -2.972 0.003725 ** 
# logWordFormFreqAllCoca                    -0.3665     0.1248  -2.937 0.004132 ** 
#   TransitionTypes#C:logWordFormFreqAllCoca   0.4355     0.1410   3.089 0.002614 ** 
# TransitionTypes#V:logWordFormFreqAllCoca   0.3431     0.1558   2.202 0.030022 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7465 on 98 degrees of freedom
# Multiple R-squared:  0.2899,	Adjusted R-squared:  0.2537 
# F-statistic: 8.002 on 5 and 98 DF,  p-value: 2.33e-06

# yes but must be discarded

# 2. rel frequencies and TransitionType?

dis_int_RelFr_Tr.lm2 <- lm (RelDur ~ TransitionType*logRelFreq, data = dis_complex_voiceless)

summary (dis_int_RelFr_Tr.lm2)

#no

#3. speechRate and RelFreq?

dis_int_SpeechRate_RelF.lm2 <- lm (RelDur ~ logRelFreq*LocSpeech, data = dis_complex_voiceless)

summary (dis_int_SpeechRate_RelF.lm2)

#no

# 4. speechRate and Trans?

dis_int_SpeechRate_TT.lm2 <- lm (RelDur ~ TransitionType*LocSpeech, data = dis_complex_voiceless)

summary (dis_int_SpeechRate_TT.lm2)

#no


#5. Stress and Trans?

dis_int_TT_Stress.lm2 <- lm (RelDur ~ AdjSyllableStress*TransitionType, data = dis_complex_voiceless)

summary (dis_int_TT_Stress.lm2)

#no

#6. Stress and ST?

dis_int_ST_Stress.lm2 <- lm (RelDur ~ AdjSyllableStress*SemanticTransparency, data = dis_complex_voiceless)

summary (dis_int_ST_Stress.lm2)

# no

#########################################################################################
#   summary interactions: 
# - there might be an interaction between TT and word form freq     
# # there might be an intercation between ST and Stresss
#########################################################################################


# We will simplify the model first and the look at the interactions again

summary(dis_voiceless.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4007 -0.5350 -0.1429  0.3988  2.1451 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      3.184438   0.504435   6.313 8.47e-09 ***
#   TransitionTypes#C               -0.990061   0.221011  -4.480 2.06e-05 ***
#   TransitionTypes#V               -0.975180   0.265500  -3.673 0.000394 ***
#   SemanticTransparencytransparent -0.228887   0.249644  -0.917 0.361517    
#   logRelFreq                       0.038879   0.033417   1.163 0.247537    
#   logWordFormFreqAllCoca           0.004332   0.058920   0.074 0.941538    
#   LocSpeech                       -0.003674   0.025644  -0.143 0.886392    
#   AdjSyllableStressu              -0.234300   0.217139  -1.079 0.283276    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7623 on 96 degrees of freedom
# Multiple R-squared:  0.2746,	Adjusted R-squared:  0.2217 
# F-statistic: 5.191 on 7 and 96 DF,  p-value: 4.83e-05




# okay now we have to get WordFormFreq

# The two frequenvy varoables are fairly similar. Which is a better predictor?
# let's first check how RelFreq and WordFreq relate to each other

dis_int_RelFr.lm2 <- lm (RelDur ~ logRelFreq, data = dis_complex_voiceless)

summary (dis_int_RelFr.lm2)

# not significant!!!!

dis_int_WordFr.lm2 <- lm (RelDur ~ logWordFormFreqAllCoca, data = dis_complex_voiceless)

summary (dis_int_WordFr.lm2)

# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3768 -0.6349 -0.1626  0.4801  2.5663 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.81537    0.40871   6.889 4.75e-10 ***
#   logWordFormFreqAllCoca -0.09573    0.05362  -1.785   0.0772 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8551 on 102 degrees of freedom
# Multiple R-squared:  0.0303,	Adjusted R-squared:  0.0208 
# F-statistic: 3.188 on 1 and 102 DF,  p-value: 0.07717

#word freq is marg. significant

dis_int_WordFr_RelFreq.lm2 <- lm (RelDur ~ logWordFormFreqAllCoca+logRelFreq, data = dis_complex_voiceless)

summary (dis_int_WordFr_RelFreq.lm2)


# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca + logRelFreq, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3603 -0.6124 -0.2171  0.4563  2.5377 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.87234    0.41029   7.001 2.87e-10 ***
#   logWordFormFreqAllCoca -0.10119    0.05367  -1.886   0.0622 .  
# logRelFreq              0.03491    0.02834   1.232   0.2209    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8529 on 101 degrees of freedom
# Multiple R-squared:  0.04466,	Adjusted R-squared:  0.02574 
# F-statistic:  2.36 on 2 and 101 DF,  p-value: 0.09956

# WordFormFreq has a better effect.

# so we throw out RelFreq


dis_complex_voiceless.lm2<-update(dis_voiceless.lm, ~ . - logRelFreq)

summary(dis_complex_voiceless.lm2)


# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency + 
#        logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4131 -0.5224 -0.1509  0.4125  2.2238 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      3.08350    0.49782   6.194 1.42e-08 ***
#   TransitionTypes#C               -0.95392    0.21921  -4.352 3.35e-05 ***
#   TransitionTypes#V               -0.86222    0.24756  -3.483 0.000746 ***
#   SemanticTransparencytransparent -0.39073    0.20767  -1.881 0.062906 .  
#   logWordFormFreqAllCoca           0.02800    0.05540   0.505 0.614364    
#   LocSpeech                       -0.00391    0.02569  -0.152 0.879357    
#   AdjSyllableStressu              -0.32094    0.20434  -1.571 0.119525    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7637 on 97 degrees of freedom
# Multiple R-squared:  0.2644,	Adjusted R-squared:  0.2189 
# F-statistic:  5.81 on 6 and 97 DF,  p-value: 3.334e-05

#Now LocSpeech


dis_complex_voiceless.lm3<-update(dis_complex_voiceless.lm2, ~ . - LocSpeech)

summary(dis_complex_voiceless.lm3)

# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency + 
#        logWordFormFreqAllCoca + AdjSyllableStress, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4080 -0.5286 -0.1405  0.4063  2.2106 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      3.03635    0.38771   7.831 5.80e-12 ***
#   TransitionTypes#C               -0.95425    0.21811  -4.375 3.03e-05 ***
#   TransitionTypes#V               -0.86433    0.24593  -3.514 0.000669 ***
#   SemanticTransparencytransparent -0.38875    0.20623  -1.885 0.062387 .  
#   logWordFormFreqAllCoca           0.02687    0.05462   0.492 0.623832    
#   AdjSyllableStressu              -0.31723    0.20186  -1.572 0.119289    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7599 on 98 degrees of freedom
# Multiple R-squared:  0.2642,	Adjusted R-squared:  0.2267 
# F-statistic: 7.037 on 5 and 98 DF,  p-value: 1.17e-05


# now word freq

dis_complex_voiceless.lm4<-update(dis_complex_voiceless.lm3, ~ . - logWordFormFreqAllCoca)

summary(dis_complex_voiceless.lm4)
# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency + 
#        AdjSyllableStress, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4006 -0.5237 -0.1583  0.4362  2.0852 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       3.1912     0.2255  14.151  < 2e-16 ***
#   TransitionTypes#C                -0.9310     0.2121  -4.389 2.85e-05 ***
#   TransitionTypes#V                -0.8538     0.2441  -3.498 0.000703 ***
#   SemanticTransparencytransparent  -0.3592     0.1965  -1.828 0.070608 .  
#   AdjSyllableStressu               -0.2896     0.1932  -1.499 0.136962    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.757 on 99 degrees of freedom
# Multiple R-squared:  0.2624,	Adjusted R-squared:  0.2326 
# F-statistic: 8.804 on 4 and 99 DF,  p-value: 4.014e-06

# now stress


dis_complex_voiceless.lm5<-update(dis_complex_voiceless.lm4, ~ . - AdjSyllableStress)

summary(dis_complex_voiceless.lm5)

# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6561 -0.5250 -0.1230  0.4416  2.1193 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       3.1571     0.2258  13.984  < 2e-16 ***
#   TransitionTypes#C                -0.9364     0.2134  -4.388 2.84e-05 ***
#   TransitionTypes#V                -1.0300     0.2152  -4.786 5.89e-06 ***
#   SemanticTransparencytransparent  -0.3717     0.1976  -1.881   0.0629 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7617 on 100 degrees of freedom
# Multiple R-squared:  0.2456,	Adjusted R-squared:  0.223 
# F-statistic: 10.85 on 3 and 100 DF,  p-value: 3.106e-06


# what fi we throw out ST instead


dis_complex_voiceless.lm6<-update(dis_complex_voiceless.lm4, ~ . - SemanticTransparency)

summary(dis_complex_voiceless.lm6)

# nahhhh

# So this would be the final model without any interactions

summary(dis_complex_voiceless.lm5)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       3.1571     0.2258  13.984  < 2e-16 ***
#   TransitionTypes#C                -0.9364     0.2134  -4.388 2.84e-05 ***
#   TransitionTypes#V                -1.0300     0.2152  -4.786 5.89e-06 ***
#   SemanticTransparencytransparent  -0.3717     0.1976  -1.881   0.0629 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7617 on 100 degrees of freedom
# Multiple R-squared:  0.2456,	Adjusted R-squared:  0.223 
# F-statistic: 10.85 on 3 and 100 DF,  p-value: 3.106e-06


# What I find out so far:

# - Doubles are longer than singles 
# - transparent are shorter?


# We need to change the reference levels of TransitionType to doubles
# to see whether they are longer than both singletons

dis_complex_voiceless$TransitionType <- relevel (dis_complex_voiceless$TransitionType, ref= "s#sV")

# now let's redo the model

dis_complex_voiceless.lm7<- lm(RelDur ~ TransitionType + SemanticTransparency, data=dis_complex_voiceless)

summary(dis_complex_voiceless.lm7)


# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6561 -0.5250 -0.1230  0.4416  2.1193 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       3.1571     0.2258  13.984  < 2e-16 ***
#   TransitionTypes#C                -0.9364     0.2134  -4.388 2.84e-05 ***
#   TransitionTypes#V                -1.0300     0.2152  -4.786 5.89e-06 ***
#    SemanticTransparencytransparent  -0.3717     0.1976  -1.881   0.0629 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7617 on 100 degrees of freedom
# Multiple R-squared:  0.2456,	Adjusted R-squared:  0.223 
# F-statistic: 10.85 on 3 and 100 DF,  p-value: 3.106e-06

# okay so doubles are longer! dis geminates
########################################################
# now let's take a look at the interactions again
###########################################################


summary(dis_complex_voiceless.lm7)


# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6561 -0.5250 -0.1230  0.4416  2.1193 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       3.1571     0.2258  13.984  < 2e-16 ***
#   TransitionTypes#C                -0.9364     0.2134  -4.388 2.84e-05 ***
# TransitionTypes#V                -1.0300     0.2152  -4.786 5.89e-06 ***
# SemanticTransparencytransparent  -0.3717     0.1976  -1.881   0.0629 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7617 on 100 degrees of freedom
# Multiple R-squared:  0.2456,	Adjusted R-squared:  0.223 
# F-statistic: 10.85 on 3 and 100 DF,  p-value: 3.106e-06

visreg(dis_complex_voiceless.lm7)


#OR

summary(dis_complex_voiceless.lm6InterFreq)

# Call:
#   lm(formula = RelDur ~ TransitionType * logWordFormFreqAllCoca + 
#        SemanticTransparency, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.5457 -0.5224 -0.1150  0.4092  2.0238 
# 
# Coefficients:
#                                             Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                5.3177     0.8484   6.268 1.01e-08 ***
#   TransitionTypes#C                         -3.8598     0.9777  -3.948 0.000149 ***
#   TransitionTypes#V                         -3.0099     1.1547  -2.607 0.010586 *  
#   logWordFormFreqAllCoca                    -0.3214     0.1260  -2.551 0.012314 *  
#   SemanticTransparencytransparent           -0.3639     0.2041  -1.783 0.077648 .  
#   TransitionTypes#C:logWordFormFreqAllCoca   0.4258     0.1396   3.051 0.002946 ** 
#   TransitionTypes#V:logWordFormFreqAllCoca   0.2979     0.1562   1.908 0.059370 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7383 on 97 degrees of freedom
# Multiple R-squared:  0.3125,	Adjusted R-squared:  0.2699 
# F-statistic: 7.347 on 6 and 97 DF,  p-value: 1.715e-06

visreg(dis_complex_voiceless.lm6InterFreq, "TransitionType", by="logWordFormFreqAllCoca")
visreg(dis_complex_voiceless.lm6InterFreq, "logWordFormFreqAllCoca", by="TransitionType")
visreg(dis_complex_voiceless.lm6InterFreq, "SemanticTransparency")

# so basically it tells us for low and mid freq ss are longer, not for high 
# --> because there are none

# the difference between opaque and transparent is arginally sign.


# the third possible model is this:


dis_complex_voiceless.lm6InterSTStress<-(lm(formula = RelDur ~ TransitionType + AdjSyllableStress*SemanticTransparency, data = dis_complex_voiceless))

                                            
summary(dis_complex_voiceless.lm6InterSTStress) 
# Call:
#   lm(formula = RelDur ~ TransitionType + AdjSyllableStress * SemanticTransparency, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4200 -0.4711 -0.1319  0.4408  1.8194 
# 
# Coefficients:
#                                                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                          3.4571     0.2567  13.468  < 2e-16 ***
#   TransitionTypes#C                                   -1.1234     0.2287  -4.913 3.59e-06 ***
#   TransitionTypes#V                                   -1.1040     0.2691  -4.103 8.45e-05 ***
#   AdjSyllableStressu                                  -1.0813     0.4286  -2.523   0.0133 *  
#   SemanticTransparencytransparent                     -0.5554     0.2155  -2.577   0.0115 *  
#   AdjSyllableStressu:SemanticTransparencytransparent   1.0263     0.4980   2.061   0.0420 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7449 on 98 degrees of freedom
# Multiple R-squared:  0.293,	Adjusted R-squared:  0.2569 
# F-statistic: 8.123 on 5 and 98 DF,  p-value: 1.908e-06

visreg(dis_complex_voiceless.lm6InterSTStress, "SemanticTransparency", by="AdjSyllableStress")
visreg(dis_complex_voiceless.lm6InterFreq, "logWordFormFreqAllCoca", by="TransitionType")
visreg(dis_complex_voiceless.lm6InterFreq, "SemanticTransparency")

#this is really not valid, we will hence go with the "single model"

summary(dis_complex_voiceless.lm7)

# Call:
#   lm(formula = RelDur ~ TransitionType + SemanticTransparency, 
#      data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6561 -0.5250 -0.1230  0.4416  2.1193 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       3.1571     0.2258  13.984  < 2e-16 ***
#   TransitionTypes#C                -0.9364     0.2134  -4.388 2.84e-05 ***
#   TransitionTypes#V                -1.0300     0.2152  -4.786 5.89e-06 ***
#   SemanticTransparencytransparent  -0.3717     0.1976  -1.881   0.0629 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7617 on 100 degrees of freedom
# Multiple R-squared:  0.2456,	Adjusted R-squared:  0.223 
# F-statistic: 10.85 on 3 and 100 DF,  p-value: 3.106e-06

visreg(dis_complex_voiceless.lm7)


# let's throw out the marginal significant effect, it is NOT significant


final.voiceless.lm<-lm(RelDur ~ TransitionType ,     data = dis_complex_voiceless)

summary(final.voiceless.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType, data = dis_complex_voiceless)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3966 -0.5687 -0.1009  0.4551  2.3981 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         2.8784     0.1724  16.692  < 2e-16 ***
#   TransitionTypes#C  -0.8229     0.2073  -3.970 0.000135 ***
#   TransitionTypes#V  -1.1230     0.2121  -5.295 6.99e-07 ***
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7712 on 101 degrees of freedom
# Multiple R-squared:  0.2189,	Adjusted R-squared:  0.2035 
# F-statistic: 14.16 on 2 and 101 DF,  p-value: 3.808e-06



####################################################################################
#                 summary
#
# - ss>CV/ SC
# Marginally
#   significant effect for SemanticTransparency - transparent items are longer:
##############################################################################



##############################################################################
# first we need to save the plot of the main effect

png("dis- model Relative Dur voiceless TransitionType.png", units="cm", height=15, width=15, res=300, pointsize=15)

visreg(dis_complex_voiceless.lm7, "TransitionType", partial=FALSE,rug=F, ylab="relative duration", xlab="environment",  cex.axis=0.8)

dev.off()

#####################################################################################################



anova(dis_complex_voiceless.lm6InterFreq)

# 
# Analysis of Variance Table
# 
# Response: RelDur
# Df Sum Sq Mean Sq F value    Pr(>F)    
# TransitionType                         2 16.837  8.4186 15.4437 1.504e-06 ***
#   logWordFormFreqAllCoca                 1  0.141  0.1410  0.2586   0.61221    
# SemanticTransparency                   1  1.914  1.9137  3.5106   0.06399 .  
# TransitionType:logWordFormFreqAllCoca  2  5.137  2.5686  4.7120   0.01114 *  
#   Residuals                             97 52.876  0.5451     



