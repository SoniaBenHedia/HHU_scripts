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




# Let's take a look at the distribution
plot(dis_complex$AbsDur)
plot(density(dis_complex$AbsDur))

# looks like a pretty normal distribution


#We will plot the distributions of double s, single s and maybe double s

plot( dis_complex$AbsDur)

dis_complexSingle<- dis_complex[dis_complex$MorphGeminate=="no",]
dis_complexDouble<- dis_complex[dis_complex$MorphGeminate=="yes",]

plot(dis_complexSingle$AbsDur)
densityplot(dis_complexSingle$AbsDur)
# there is one item which is really llong....

dis_complexSingle[dis_complexSingle$AbsDur>0.18,]
# ID= 3
# nothing weird here - so we will ignore this observation for now
#(looked in audiofiles as well)

plot(dis_complexDouble$AbsDur)
densityplot(dis_complexDouble$AbsDur)
plot(dis_complexDouble$AbsDur)

# there are two points which look "weird", they are shorter than all others
#lrt's identify them

dis_complexDouble[dis_complexDouble$AbsDur>0.18,]
#ID= 246, 281


# hmm nothing strange about them (looked in audiofiles as well)

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

plot(dis_complex$PartofSpeechWord, dis_complex$AbsDur)
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

plot (dis_complex$logRelFreq, dis_complex$AbsDur)
plot (dis_complex$logWordFormFreqAllCoca, dis_complex$AbsDur)
plot (dis_complex$logBaseFormFreqAllCoca, dis_complex$AbsDur)

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


plot (dis_complex$AbsDur ~ dis_complex$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.



#     5. Word Duration

plot (density(dis_complex$WordDur))
plot (dis_complex$AbsDur ~ dis_complex$WordDur)


# longer words seem to have a longer s

#     6. Preceding Segment duration

plot (density(dis_complex$PrecSegDur))
plot(dis_complex$PrecSegDur)
plot (dis_complex$AbsDur ~ dis_complex$PrecSegDur)

# Both plots don't suggest anything remarkable,maybe a slight correlation
#we include the duration
# of the preceding segments, because of other studies (such as Oh &
# Redford) have emphasized the interplay between the preceding vowel 
# duration and the geminate duration.

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

plot(dis_complex$AbsDur~dis_complex$FollSegVC)
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
plot(dis_complex$AbsDur~dis_complex$TransitionType)

# Doubles seem to be longer!

# 8. Following segment duration

plot(dis_complex$AbsDur~dis_complex$FollSegDur)


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

plot(dis_complex$AbsDur~dis_complex$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables. But it might have an effct (see un and im), especially because WorDur is
# included in this factor and it has been shown to have an effect


# 9.2. Number of syllables

table(dis_complex$SyllAct)
#1  2  3  4  5 
#1 27 59 32  9

plot (dis_complex$AbsDur ~ dis_complex$SyllAct)
# hmmm

#     9.3. Word Duration

plot (dis_complex$AbsDur ~ dis_complex$WordDur)

# we looked at this already

#     9.4. Number of Segments

plot(density(dis_complex$NumberOfSegments))
plot (dis_complex$AbsDur ~ dis_complex$NumberOfSegments)

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

plot (dis_complex$AbsDur ~ dis_complex$AffixStress)

# there does not seem to be a connection between stress and duration in this case
# also some of the levels are too small to say anything AND the coding is
# somehow unreliable (Longman)

table (dis_complex$AdjSyllableStress)
#p   u 
#88 40 

plot (dis_complex$AbsDur ~ dis_complex$AdjSyllableStress)

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

plot(dis_complex$AbsDur~dis_complex$PrefixDur)
# there seems to be a strong correlation, let's check

cor.test (dis_complex$PrefixDur, dis_complex$AbsDur)
# Pearson's product-moment correlation
# 
# data:  dis_complex$PrefixDur and dis_complex$AbsDur
# t = 13.2746, df = 126, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6801972 0.8274800
# sample estimates:
#       cor 
# 0.7635966 

# so we need to be careful interpreting the results and might
# later on think about a model in which we take the relative
# duration of the s in the prefix as the dep variable

dis_complex$RelDur<-dis_complex$PrefixDur/dis_complex$AbsDur
plot(dis_complex$RelDur)
# YEAH, so keep this in mind!

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


plot (dis_complex$AbsDur ~ dis_complex$SemanticTransparency)
# does not seem to make a dofference, however transparent seem to be a bit longer

dis_complex$item.x
# the following items are voiced:

#disease, diseases, dissolve, dissolved, dissolving

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
#24       104 


plot(dis_2$AbsDur~dis_2$Voicing)
# yes the voiced ones are shorter

# let's include it in out model

## We are going to include the following predictors:
# - TransitionType
# - SemanticTransparency
# - logWordFormFreqAllCoca
# - logRelFreq
# - LocSpeech
# - PrecSegDur
# - AdjSyllableStress

#############################################################################################
#  NOW the dis- dataset is in a form in which we can go on working...(the same as the        #
# one we did the LM in...)                                                                  #
#       Next, we will merge the rating and the LSA score, so that we can wor with one df    #
#############################################################################################

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/dis/csv/Decomposability")
LSA <- read.csv("LSA_scores_dis_24_03_2016.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(LSA)
#[1] 62   2

str(LSA)
#data.frame':	62 obs. of  2 variables:
# $ item: Factor w/ 62 levels "disabilities",..: 8 46 35 57 60 56 52 9 45 27 ...
#$ LSA : Factor w/ 31 levels "-0.00","-0.01",..: 28 13 31 31 31 31 6 22 31 8 ..

# LSA needs to be an integer

LSA$LSA<-as.numeric(as.character(LSA$LSA))

# let's rename LSA to LSAScore

LSA$LSAScore<-LSA$LSA

str(LSA)
#'data.frame':	62 obs. of  3 variables:
#  $ item    : Factor w/ 62 levels "disabilities",..: 8 46 35 57 60 56 52 9 45 27 ...
#$ LSA     : int  28 13 31 31 31 31 6 22 31 8 ...
#$ LSAScore: int  28 13 31 31 31 31 6 22 31 8 ...

dis_complex_2<- merge(dis_complex, LSA, by.x="item.x", by.y="item")


dim(dis_complex_2)
#[1] 128  55

# looks good, now we can have a look at the LSA scores

# 11. LSA

#Let's have a look at the distribution of RelFreq

plot(unique(dis_complex_2$LSAScore),dis_complex_2$ID)
text(unique(dis_complex_2$LSAScore),dis_complex_2$ID, dis_complex_2$item.x)
densityplot(dis_complex_2$LSAScore)

bwplot(~LSAScore|item.x,dis_complex_2)
# looking at the items this does not make too much sense
bwplot(~LSAScore|SemanticTransparency,dis_complex_2)
# this makes sense however, transparent items have higher scores

#########################################################################
# Let's try out another LSA-measurment (M. Marelli and M. Baroni. 2015) #
#########################################################################

LSA_Marelli <- read.csv("derived_words_ST.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(LSA_Marelli)
#[1] 900 4

str(LSA_Marelli)
#'data.frame':	900 obs. of  4 variables:
#  $ affix       : Factor w/ 18 levels "-able","-al",..: 6 3 6 8 6 7 1 6 12 12 ...
#$ stem        : Factor w/ 869 levels "abrade","abstain",..: 1 2 2 3 4 7 8 9 11 12 ...
#$ derived_word: Factor w/ 900 levels "abrasion","abstainer",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ ST          : num  4.71 6.43 4.43 6.14 5.86 3.29 6.29 6.43 6.29 6.29 ...

# ok

dis_complex_3<- merge(dis_complex_2, LSA_Marelli, by.x="item.x", by.y="derived_word")


dim(dis_complex_3)
#[1] 1 59

str(dis_complex_3)
# looks like only one of our types is in the dataset, so it is not
# interesting for us

# Let's check again

LSA_Marelli[LSA_Marelli$affix=="dis-",]

# yes, they did look at dis!

# So we will ignore this factor

############################################################################################
#                     MERGE  with the rating                                               #
############################################################################################

# We will first load all the df we need

rating<- read.csv("dis_items_AE_median_without_zeros_simple.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(rating)
#[1] 97 2


str(rating)
#'data.frame':	97 obs. of  2 variables:
#  $ Item  : Factor w/ 96 levels "disabilities",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ median: int  1 1 1 1 1 1 1 1 1 1 ...

# merge 
dis_complex_3<-merge(dis_complex_2, rating, by.x="item.x", by.y="Item")


dim(dis_complex_3)
#[1] 128 56

str(dis_complex_3)

# looks good!

bwplot(~median|item.x,dis_complex_3)

# this looks like opaque items were rated m´less decomposable than transparent items

#########################################################################
# Let's try out another measurment of semantic space (Rosella)
#########################################################################

Rosella_Scores <- read.csv("scores_all.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(Rosella_Scores)
#[1] 312 2

str(Rosella_Scores)
# 'data.frame':	312 obs. of  2 variables:
#   $ item : Factor w/ 239 levels "actively","actually",..: 184 233 196 220 171 189 188 185 232 228 ...
# $ score: num  0.2953 0.3302 0.0832 0.3287 0.508 ..
# # ok

dis_complex_4<- merge(dis_complex_3, Rosella_Scores, by.x="item.x", by.y="item")


dim(dis_complex_4)
#[1] 63 58

str(dis_complex_4)
# there are only 63 item for which a score could be computed
# let's have a look at them

#Let's have a look at the distribution of RelFreq

plot(unique(dis_complex_4$score),dis_complex_4$ID)
text(unique(dis_complex_4$score),dis_complex_4$ID, dis_complex_4$item.x)
densityplot(dis_complex_4$score)

bwplot(~score|item.x,dis_complex_4)
# looking at the items this does not make too much sense
bwplot(~score|SemanticTransparency,dis_complex_4)
# this makes sense however, transparent items have higher scores. however we
# do not have a lot of opaque items in this dataset

#let's have a looko at the items

dis_complex_4$item.x<-droplevels(dis_complex_4$item.x)
unique(dis_complex_4$item.x)

length(levels(dis_complex_4$item.x))
# 23
length(levels(dis_complex_3$item.x))
# 62

# so, there are only 23 types left from 62...

# let's just a short comparison before we work on withour these scores 
# (since we do  not have scores for the majority of our items we 
# can't use them)

# compare score with median

plot(dis_complex_4$median,dis_complex_4$score)

# the ones rated easier to decompose, more similar

# compare score with Semantic Trans

dis_complex_4$SematicTransparency<-droplevels(dis_complex_4$SematicTransparency)

plot(dis_complex_4$SematicTransparency,dis_complex_4$score)

# transparent more similar

# compare score withRelFre


plot(dis_complex_4$logRelFreq,dis_complex_4$score)
# I do not really see a connection...


# compare score with LSA


plot(dis_complex_4$LSAScore,dis_complex_4$score)

# positive correlation...

# now, let's get an overview

pairscor.fnc(dis_complex_4 [, c("logRelFreq", "median", "LSAScore", "score", "SemanticTransparency")])

# so tu summaroze:

# these scores seem comparable to LSA scores and seem to also
# mirror Semantic Trans and the rating. RelFreq does not seem to have a relation
# HOWEVER, as mentioned before we can't use them since they are not privided for every item


####################################################################################
# Now we can compare the (other) measurements                                              #
####################################################################################


# Let's first look at the distributions


table(dis_complex_3$median)
#1  2  3  4 
#84  5 35  4 

table(dis_complex_3$median,dis_complex_3$SemanticTransparency)

#    opaque transparent
# 1     11          73
# 2      4           1
# 3     35           0
# 4      4           0

# transparent always 1! (Except for 1)

# wor probieren ctre...
library(partykit)
dec_tree<-ctree (median~logRelFreq+SemanticTransparency+LSAScore, data=dis_complex_3)
plot(dec_tree)

# Das bedeutet also,dass es einen klaren Unterschied zwischen opaquen und transparenten
# gibt. RelFreq ist da vollkommen egal, genau wie LSA

dec_tree_2<-ctree (median~logRelFreq+LSAScore, data=dis_complex_3)
plot(dec_tree_2)

# Wir können uns jetzt auch nochmal die Korrelationen anschauen:

pairscor.fnc(dis_complex_3 [, c("SemanticTransparency","logRelFreq", "median", "LSAScore")])

# So, LSA does not correlate with any other measurment
# The other correlate in the expected way, however, beacuse of
# the opaque items the correlation between logRelFreq and Sem. Trans is inherent
# and since the rating is basically binary, the one between median and logRel is also


# Lt's take a closer at semantic transparency and teh results

bwplot(~median|SemanticTransparency,dis_complex_3)

bwplot(~logRelFreq|SemanticTransparency,dis_complex_3)

bwplot(~LSAScore|SemanticTransparency,dis_complex_3)

# shows us the same...


# we need to load tyope of base

type_base<- read.csv("type_of_base_dis.csv")

dis_complex_3<-merge(dis_complex_3, type_base, by.x="item.x", by.y="item")


class(dis_complex_3$type_of_base)


# we need the types

dis_complex_Types<-dis_complex_3[!duplicated(dis_complex_3$item.x),]


####################################################################################
# Let's systematically check the influence on consonant duration                                   #
####################################################################################


# let's get the summary of the LSA Score first

summary(dis_complex_3$LSAScore)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.0400  0.0900  0.2300  0.2707  0.4350  0.6900      26 



sd(dis_complex_3[!is.na(dis_complex_3$LSAScore),]$LSAScore)
#0.201535


# and  let's get the summary of median

summary(dis_complex_3$median)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    1.00    1.68    3.00    4.00 



sd(dis_complex_3$median)
#0.9795837


# and the summary for type of base

summary(dis_complex_3$type_of_base)
# bound_root       word 
# 18        110 

# we need our final model - let's create it 
dis.lm <- lm (AbsDur ~ TransitionType+SemanticTransparency+Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_complex_3)


bc <- boxcox(dis.lm)

lambda <- bc$x[which.max(bc$y)]

dis_complex_3$bc <- dis_complex_3$AbsDur^lambda

final_dis.lm<- lm (bc ~  TransitionType*AdjSyllableStress+ Voicing+ LocSpeech , data = dis_complex_3)

summary(final_dis.lm)

# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.5994637  0.0156375  38.335  < 2e-16 ***
#   TransitionTypes#V                     0.0192855  0.0103976   1.855   0.0661 .  
# TransitionTypes#s                     0.0449500  0.0086844   5.176 9.23e-07 ***
# AdjSyllableStressu                    0.0121116  0.0129309   0.937   0.3508    
# Voicingvoiceless                      0.0571720  0.0106532   5.367 3.97e-07 ***
#   LocSpeech                            -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   TransitionTypes#V:AdjSyllableStressu -0.0310425  0.0168448  -1.843   0.0678 .  
# TransitionTypes#s:AdjSyllableStressu -0.0478538  0.0231416  -2.068   0.0408 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03107 on 120 degrees of freedom
# Multiple R-squared:  0.3699,	Adjusted R-squared:  0.3331 
# F-statistic: 10.06 on 7 and 120 DF,  p-value: 7.439e-10

# that's the right one

# Let's now test the influence of each variablem let's start with LSA

# alone

disComplex.lmLSA<- lm(bc~ LSAScore, data = dis_complex_3[!is.na(dis_complex_3$LSAScore),])
summary(disComplex.lmLSA)
# NO


# now RelFreq

disComplex.lmRel<- lm(bc~ logRelFreq, data = dis_complex_3)
summary(disComplex.lmRel)

# Call:
#   lm(formula = bc ~ logRelFreq, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.079483 -0.025106  0.004129  0.022409  0.111606 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.598920   0.003311 180.865   <2e-16 ***
#   logRelFreq  -0.002537   0.001131  -2.243   0.0267 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03746 on 126 degrees of freedom
# Multiple R-squared:  0.03838,	Adjusted R-squared:  0.03075 
# F-statistic: 5.029 on 1 and 126 DF,  p-value: 0.02667

# yes, but is it stable?


disComplex.lmRel1<- lm(bc~ TransitionType+ LocSpeech+  logRelFreq, data = dis_complex_3)
summary(disComplex.lmRel1)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + logRelFreq, data = dis_complex_3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.09487 -0.02191  0.00570  0.02348  0.09431 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.6322335  0.0148573  42.554  < 2e-16 ***
#   TransitionTypes#V -0.0088655  0.0070067  -1.265  0.20816    
# TransitionTypes#s  0.0274850  0.0091720   2.997  0.00330 ** 
# LocSpeech         -0.0026429  0.0009992  -2.645  0.00924 ** 
#   logRelFreq        -0.0015616  0.0011013  -1.418  0.15874    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03464 on 123 degrees of freedom
# Multiple R-squared:  0.1974,	Adjusted R-squared:  0.1713 
# F-statistic: 7.562 on 4 and 123 DF,  p-value: 1.762e-05

# no

# let's try the cat measurment

# Let'S recode: I wanna recode as base more often or derivative more often

list_RelFreqCat=list()
for (i in dis_complex_3$Item){
  if(dis_complex_3[dis_complex_3$Item==i,"logRelFreq"]<0){
    list_RelFreqCat=append(list_RelFreqCat,"more decomposable")}
  else{list_RelFreqCat=append(list_RelFreqCat,"less decomposable")}
}

dis_complex_3$RelFreqCat<-as.factor(as.character(list_RelFreqCat))

# now RelFreqCat

dis_complex_3.RelFreqCat<- lm (bc ~  TransitionType +LocSpeech+Voicing+RelFreqCat, data = dis_complex_3)
summary(dis_complex_3.RelFreqCat)
# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + Voicing + RelFreqCat, 
#      data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069725 -0.019449  0.002427  0.023886  0.073631 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  6.153e-01  1.373e-02  44.807  < 2e-16 ***
#   TransitionTypes#V            3.443e-03  6.912e-03   0.498    0.619    
# TransitionTypes#s            3.644e-02  8.579e-03   4.247 4.26e-05 ***
# LocSpeech                   -4.715e-03  1.007e-03  -4.681 7.47e-06 ***
#   Voicingvoiceless             4.491e-02  9.819e-03   4.574 1.16e-05 ***
#   RelFreqCatmore decomposable  3.071e-08  6.707e-03   0.000    1.000    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03171 on 122 degrees of freedom
# Multiple R-squared:  0.3328,	Adjusted R-squared:  0.3055 
# F-statistic: 12.17 on 5 and 122 DF,  p-value: 1.418e-09

# no

# now Type of Base

dis_complex_3.Base<- lm (bc ~  TransitionType+ type_of_base, data = dis_complex_3)
summary(dis_complex_3.Base)

# Call:
#   lm(formula = bc ~ TransitionType + type_of_base, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.085387 -0.028982  0.005754  0.021877  0.097158 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.592078   0.008394  70.538  < 2e-16 ***
#   TransitionTypes#V -0.006812   0.008274  -0.823  0.41192    
# TransitionTypes#s  0.031685   0.009991   3.171  0.00191 ** 
# type_of_baseword   0.004574   0.010836   0.422  0.67370    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03561 on 124 degrees of freedom
# Multiple R-squared:  0.1447,	Adjusted R-squared:  0.124 
# F-statistic: 6.995 on 3 and 124 DF,  p-value: 0.0002192

# no

# now median

dis_complex_3.rating<- lm (bc ~ TransitionType+ median, data = dis_complex_3)
summary(dis_complex_3.rating)

# Call:
#   lm(formula = bc ~ TransitionType + median, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.086199 -0.024664  0.003724  0.021462  0.092642 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.607656   0.008428  72.103  < 2e-16 ***
#   TransitionTypes#V -0.006928   0.007020  -0.987  0.32561    
# TransitionTypes#s  0.027980   0.009320   3.002  0.00324 ** 
# median            -0.006489   0.003340  -1.943  0.05431 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03511 on 124 degrees of freedom
# Multiple R-squared:  0.1688,	Adjusted R-squared:  0.1487 
# F-statistic: 8.394 on 3 and 124 DF,  p-value: 4.004e-05

#marginally sign - what if we include voicing


dis_complex_3.rating2<- lm (bc ~ TransitionType+Voicing+ median, data = dis_complex_3)
summary(dis_complex_3.rating2)

# Call:
#   lm(formula = bc ~ TransitionType + Voicing + median, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.062325 -0.027793  0.003282  0.020626  0.098996 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       5.671e-01  1.758e-02  32.255  < 2e-16 ***
#   TransitionTypes#V 4.401e-03  8.120e-03   0.542 0.588820    
# TransitionTypes#s 3.813e-02  9.905e-03   3.850 0.000189 ***
# Voicingvoiceless  2.767e-02  1.061e-02   2.609 0.010212 *  
#   median            8.741e-06  4.106e-03   0.002 0.998305    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03431 on 123 degrees of freedom
# Multiple R-squared:  0.2124,	Adjusted R-squared:  0.1868 
# F-statistic: 8.292 on 4 and 123 DF,  p-value: 5.905e-06

# not significant anymore


# now semantic transparency

dis_complex_3.SemTr<- lm (bc ~  TransitionType+ SemanticTransparency, data = dis_complex_3)
summary(dis_complex_3.SemTr)


# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.075002 -0.024616  0.001992  0.022161  0.106372 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.587437   0.005889  99.759  < 2e-16 ***
#   TransitionTypes#V               -0.008581   0.007001  -1.226 0.222643    
# TransitionTypes#s                0.030514   0.008842   3.451 0.000765 ***
# SemanticTransparencytransparent  0.016616   0.006338   2.622 0.009843 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03469 on 124 degrees of freedom
# Multiple R-squared:  0.1885,	Adjusted R-squared:  0.1689 
# F-statistic: 9.601 on 3 and 124 DF,  p-value: 9.534e-06

# there is an effect but is it stable?

dis_complex_3.SemTr2<- lm (bc ~  TransitionType+ LocSpeech+Voicing+SemanticTransparency, data = dis_complex_3)
summary(dis_complex_3.SemTr2)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + Voicing + SemanticTransparency, 
#      data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.070246 -0.020177  0.002836  0.023703  0.075118 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.616836   0.014253  43.278  < 2e-16 ***
#   TransitionTypes#V                0.001708   0.008105   0.211 0.833448    
# TransitionTypes#s                0.035347   0.008579   4.120 6.93e-05 ***
# LocSpeech                       -0.004705   0.001004  -4.684 7.38e-06 ***
#   Voicingvoiceless                 0.041812   0.011658   3.586 0.000483 ***
#   SemanticTransparencytransparent  0.003225   0.008178   0.394 0.694011    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03169 on 122 degrees of freedom
# Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3063 
# F-statistic: 12.22 on 5 and 122 DF,  p-value: 1.316e-09

# no (also problem with distribution --Y remember that)
# We should also test the effect of decomposability on RelDur

# Create the variable RelDur

dis_complex_3$RelDur<-dis_complex_3$AbsDur/dis_complex_3$PrecSegDur
# we will then test the fefct of the different decomposability mesaurments (median, logRelfreq, SemanticTransparency, LSA)

# let' see whether these measurements habve an influence on consonant duration

#Now. let'see about our decomposiabilty measurments

disComplexRelDur.lmLSA<- lm(RelDur~ LSAScore, data = dis_complex_3[!is.na(dis_complex_3$LSAScore),])
disComplexRelDur.lmRel<- lm(RelDur~ logRelFreq, data = dis_complex_3)
disComplexRelDur.lmST<- lm(RelDur~ SemanticTransparency, data = dis_complex_3)
disComplexRelDur.lmRat<- lm(RelDur~ median, data = dis_complex_3)



summary(disComplexRelDur.lmLSA)
# So no effect what so ever!

summary(disComplexRelDur.lmRel)

# no effect


summary(disComplexRelDur.lmST)
# no effect


summary(disComplexRelDur.lmRat)

#no effect


# so with RelDur we do not find any effect!!!!



# einzeln, keinen Einfluss - vielleicht aber zusammen genommen 

# PCP Anylse


library(pls)


# we need to recode all the variables, so that they are numeric

# # also they need to "point in the same direction" --> the higher
# the less decomposable

# RelFreq is fine
# median is fine


#Type pf base

levels(dis_complex_3$type_of_base)
#[1] "bound root" "word"      

dis_complex_3$type_of_base <- relevel (dis_complex_3$type_of_base, ref= "word"   )

dis_complex_3$NumType_of_base<-as.numeric(dis_complex_3$type_of_base)

table(dis_complex_3$type_of_base,dis_complex_3$NumType_of_base)
#               1   2
#word       110   0
#bound_root   0  18


#Smenatic Transparency

levels(dis_complex_3$SemanticTransparency)
#[1] "opaque"      "transparent"

dis_complex_3$SemanticTransparency <- relevel (dis_complex_3$SemanticTransparency, ref= "transparent")

dis_complex_3$NumSemanticTransparency<-as.numeric(dis_complex_3$SemanticTransparency)

table(dis_complex_3$SemanticTransparency,dis_complex_3$NumSemanticTransparency)
#                1   2
# transparent 74  0
# opaque       0 54


# let's also include voicing


levels(dis_complex_3$Voicing)
#[1] "voiced"    "voiceless"

dis_complex_3$Voicing <- relevel (dis_complex_3$Voicing, ref= "voiceless")

dis_complex_3$NumVoicing<-as.numeric(dis_complex_3$Voicing)

table(dis_complex_3$Voicing,dis_complex_3$NumVoicing)
#            1   2
# voiceless 104   0
# voiced      0  24

# one further problem is that the variables are on 
# different scales - so we need to change this

dis_complex_3$ScaledSemanticTransparency<-as.numeric(scale(dis_complex_3$NumSemanticTransparency))
summary(dis_complex_3$ScaledSemanticTransparency)


dis_complex_3$ScaledMedian<-as.numeric(scale(dis_complex_3$median))
summary(dis_complex_3$ScaledMedian)

dis_complex_3$ScaledTypeOfBase<-as.numeric(scale(dis_complex_3$NumType_of_base))
summary(dis_complex_3$ScaledTypeOfBase)


dis_complex_3$ScaledVoicing<-as.numeric(scale(dis_complex_3$NumVoicing))
summary(dis_complex_3$ScaledVoicing)

dis_complex_3$ScaledRelFreq<-as.numeric(scale(dis_complex_3$logRelFreq))
summary(dis_complex_3$ScaledRelFreq)

decomposability.pc <- prcomp(dis_complex_3[, c("ScaledVoicing","ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability.pc)
# Importance of components:
#                        PC1    PC2    PC3     PC4     PC5
# Standard deviation     1.7170 1.1005 0.7374 0.44982 0.30723
# Proportion of Variance 0.5897 0.2422 0.1088 0.04047 0.01888
# Cumulative Proportion  0.5897 0.8319 0.9407 0.98112 1.00000000

# So the first 3 are important

decomposability.pc$rotation
#                               PC1         PC2        PC3         PC4         PC5
# ScaledVoicing              0.3354263  0.69993627 -0.2499462  0.29354244 -0.49893705
# ScaledRelFreq              0.4354606  0.05617844  0.8958381  0.04773093 -0.04913243
# ScaledMedian               0.5431608 -0.09876806 -0.2486781  0.48091367  0.63411554
# ScaledTypeOfBase           0.3458256 -0.70166185 -0.1639510  0.14843175 -0.58237737
# ScaledSemanticTransparency 0.5322071  0.06963276 -0.2151287 -0.81132278  0.08591798

# PC1 is mostrly median and ST but also RelFreq and TypeOfBase
# Voicing also plays a role

# PC 2 is mostly voicing and type of base

# let's see whether they influence the model

# PC3 is mostly RelFreq
dis_complex_3$PCDec <- decomposability.pc$x[, 1]
dis_complex_3$PCDec2 <- decomposability.pc$x[, 2]
dis_complex_3$PCDec3 <- decomposability.pc$x[, 3]

# let's see whether this has an influence


final_imPC.lm<-lm(bc ~ TransitionType + LocSpeech + AdjSyllableStress + PCDec+ PCDec2+ PCDec3, 
                  data = dis_complex_3)


summary(final_imPC.lm)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + AdjSyllableStress + 
#        PCDec + PCDec2 + PCDec3, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.076256 -0.023981  0.003853  0.021749  0.072968 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         0.653407   0.014576  44.827  < 2e-16 ***
#   TransitionTypes#V   0.002917   0.008665   0.337 0.736992    
#   TransitionTypes#s   0.035311   0.009328   3.785 0.000241 ***
#   LocSpeech          -0.004615   0.001010  -4.570 1.19e-05 ***
#   AdjSyllableStressu -0.007853   0.008104  -0.969 0.334509    
#   PCDec              -0.006930   0.002078  -3.334 0.001137 ** 
#   PCDec2             -0.011553   0.003469  -3.330 0.001154 ** 
#   PCDec3              0.007387   0.003997   1.848 0.067061 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03203 on 120 degrees of freedom
# Multiple R-squared:  0.3305,	Adjusted R-squared:  0.2915 
# F-statistic: 8.464 on 7 and 120 DF,  p-value: 2.151e-08

# 1 and 2 influence the model, also with the known interaction?


final_imPC2.lm<-lm(bc ~ TransitionType+PCDec2 + LocSpeech +PCDec, 
                  data = dis_complex_3)


summary(final_imPC2.lm)


# Call:
#   lm(formula = bc ~ TransitionType + PCDec2 + LocSpeech + PCDec, 
#      data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.078803 -0.020473  0.004447  0.022910  0.089973 
# 
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        0.6479481  0.0144764  44.759  < 2e-16 ***
#   TransitionTypes#V  0.0007163  0.0079855   0.090 0.928671    
#   TransitionTypes#s  0.0338656  0.0093267   3.631 0.000414 ***
#   PCDec2            -0.0105644  0.0033907  -3.116 0.002288 ** 
#   LocSpeech         -0.0042856  0.0010064  -4.258 4.08e-05 ***
#   PCDec             -0.0058675  0.0017980  -3.263 0.001429 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03234 on 122 degrees of freedom
# Multiple R-squared:  0.306,	Adjusted R-squared:  0.2776 
# F-statistic: 10.76 on 5 and 122 DF,  p-value: 1.385e-08



final_imPC.lm2<-lm(bc ~ TransitionType*AdjSyllableStress + LocSpeech  + PCDec+ PCDec2, 
                  data = dis_complex_3)


summary(final_imPC.lm2)
# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + LocSpeech + 
#        PCDec + PCDec2, data = dis_complex_3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.07481 -0.01991  0.00115  0.02223  0.09689 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.640650   0.015492  41.353  < 2e-16 ***
#   TransitionTypes#V                     0.014626   0.011647   1.256 0.211669    
# TransitionTypes#s                     0.041985   0.010483   4.005 0.000108 ***
# AdjSyllableStressu                    0.013006   0.014372   0.905 0.367324    
# LocSpeech                            -0.004075   0.001017  -4.007 0.000108 ***
#   PCDec                                -0.006638   0.002157  -3.078 0.002587 ** 
#   PCDec2                               -0.013723   0.003846  -3.568 0.000520 ***
#   TransitionTypes#V:AdjSyllableStressu -0.027811   0.018041  -1.542 0.125849    
# TransitionTypes#s:AdjSyllableStressu -0.039547   0.024694  -1.601 0.111924    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03216 on 119 degrees of freedom
# Multiple R-squared:  0.3304,	Adjusted R-squared:  0.2854 
# F-statistic: 7.341 on 8 and 119 DF,  p-value: 6.675e-08


# which model is better, the final model with or wihout PC
# the one withou!

# yes. but the interaction is not significant anymore.
# Let's try out a PC without voicing

decomposability.pc2 <- prcomp(dis_complex_3[, c("ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability.pc2)
#Importance of components:
#                         PC1    PC2    PC3    PC4
# Standard deviation     1.6479 0.8341 0.6474 0.4118
# Proportion of Variance 0.6789 0.1739 0.1048 0.0424
# Cumulative Proportion  0.6789 0.8528 0.9576 1.0000

# So, PC 1,2 and 3 are very important

decomposability.pc2$rotation
#                             PC1         PC2        PC3         PC4
# ScaledRelFreq              0.4492317  0.65254950  0.6091427  0.03626463
# ScaledMedian               0.5632592 -0.09389414 -0.2686261 -0.77573382
# ScaledTypeOfBase           0.4414899 -0.73519038  0.4468176  0.25482505
# ScaledSemanticTransparency 0.5348052  0.15766467 -0.5976112  0.57618236

# 1 is mostly median + ST,but the others are up there too
#2 is type of base and RelFreq

# let's see whwther they have an influence in the model

dis_complex_3$PCDec4 <- decomposability.pc2$x[, 1]

dis_complex_3$PCDec5 <- decomposability.pc2$x[, 2]

dis_complex_3$PCDec6 <- decomposability.pc2$x[, 3]

# let's see whether this has an influence


final_imPC3.lm<-lm(bc ~ TransitionType*AdjSyllableStress+Voicing + LocSpeech  + PCDec4 + PCDec5+ PCDec6, 
                   data = dis_complex_3)


summary(final_imPC3.lm)

#Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech + PCDec4 + PCDec5 + PCDec6, data = dis_complex_3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.06397 -0.02202  0.00134  0.02213  0.06822 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.658805   0.015778  41.756  < 2e-16 ***
#   TransitionTypes#V                     0.019855   0.011593   1.713   0.0894 .  
#   TransitionTypes#s                     0.046460   0.010341   4.493 1.66e-05 ***
#   AdjSyllableStressu                    0.012769   0.014010   0.911   0.3640    
#   Voicingvoiced                        -0.067644   0.016618  -4.071 8.56e-05 ***
#   LocSpeech                            -0.004665   0.001013  -4.607 1.05e-05 ***
#   PCDec4                                0.001585   0.002611   0.607   0.5449    
#   PCDec5                                0.003758   0.004672   0.804   0.4228    
#   PCDec6                               -0.004166   0.006928  -0.601   0.5488    
#   TransitionTypes#V:AdjSyllableStressu -0.030296   0.017572  -1.724   0.0873 .  
#   TransitionTypes#s:AdjSyllableStressu -0.055562   0.026278  -2.114   0.0366 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03136 on 117 degrees of freedom
# Multiple R-squared:  0.3742,	Adjusted R-squared:  0.3207 
# F-statistic: 6.997 on 10 and 117 DF,  p-value: 1.564e-08


# NO!

# i need the final model without the PCs for the appendix..

final_disAllPC.lm<-lm(bc ~ TransitionType*AdjSyllableStress+Voicing + LocSpeech, 
                   data = dis_complex_3)


summary(final_disAllPC.lm)



table_final_model_dis_PC<-as.data.frame(coef(summary(final_disAllPC.lm)))

xtable(table_final_model_dis_PC,digits = 3)



###############
# I need to do the sam for RelDur

dis_complex_3$RelDur<-dis_complex_3$AbsDur/dis_complex_3$PrecSegDur


final_imRelPC3.lm<-lm(RelDur ~ TransitionType+AdjSyllableStress+Voicing + LocSpeech  + PCDec4 + PCDec5+ PCDec6, 
                   data = dis_complex_3)


summary(final_imRelPC3.lm)


# Call:
#   lm(formula = RelDur ~ TransitionType + AdjSyllableStress + Voicing + 
#        LocSpeech + PCDec4 + PCDec5 + PCDec6, data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3100 -0.5451 -0.1331  0.4645  2.0360 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.26822    0.36018   6.297 5.27e-09 ***
#   TransitionTypes#V   0.12171    0.21260   0.572   0.5681    
# TransitionTypes#s   0.88433    0.21838   4.049 9.18e-05 ***
# AdjSyllableStressu -0.31544    0.19486  -1.619   0.1081    
# Voicingvoiced      -1.41665    0.36915  -3.838   0.0002 ***
#   LocSpeech          -0.01197    0.02383  -0.502   0.6165    
# PCDec4              0.13951    0.05741   2.430   0.0166 *  
#   PCDec5              0.17473    0.10736   1.628   0.1063    
# PCDec6             -0.11560    0.14742  -0.784   0.4345    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7482 on 119 degrees of freedom
# Multiple R-squared:  0.3091,	Adjusted R-squared:  0.2627 
# F-statistic: 6.655 on 8 and 119 DF,  p-value: 3.569e-07

final_imRelPC4.lm<-lm(RelDur ~ TransitionType+AdjSyllableStress+Voicing + LocSpeech  + PCDec4 + PCDec5, 
                      data = dis_complex_3)


summary(final_imRelPC4.lm)

# Call:
#   lm(formula = RelDur ~ TransitionType + AdjSyllableStress + Voicing + 
#        LocSpeech + PCDec4 + PCDec5, data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3138 -0.5597 -0.1459  0.4298  2.1817 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.23592    0.35724   6.259 6.23e-09 ***
#   TransitionTypes#V   0.08303    0.20646   0.402    0.688    
# TransitionTypes#s   0.90509    0.21643   4.182 5.52e-05 ***
# AdjSyllableStressu -0.28779    0.19134  -1.504    0.135    
# Voicingvoiced      -1.22767    0.27917  -4.398 2.38e-05 ***
#   LocSpeech          -0.01180    0.02379  -0.496    0.621    
# PCDec4              0.12516    0.05433   2.304    0.023 *  
#   PCDec5              0.14005    0.09767   1.434    0.154    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.747 on 120 degrees of freedom
# Multiple R-squared:  0.3055,	Adjusted R-squared:  0.265 
# F-statistic: 7.542 on 7 and 120 DF,  p-value: 1.612e-07






final_imRelPC5.lm<-lm(RelDur ~ TransitionType+AdjSyllableStress+Voicing + PCDec4  +PCDec5, 
                      data = dis_complex_3)


summary(final_imRelPC5.lm)
# Call:
#   lm(formula = RelDur ~ TransitionType + AdjSyllableStress + Voicing + 
#        PCDec4 + PCDec5, data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3391 -0.5571 -0.1185  0.4545  2.1603 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.07062    0.12821  16.150  < 2e-16 ***
#   TransitionTypes#V   0.08182    0.20581   0.398   0.6917    
# TransitionTypes#s   0.90784    0.21568   4.209 4.95e-05 ***
# AdjSyllableStressu -0.28153    0.19033  -1.479   0.1417    
# Voicingvoiced      -1.17587    0.25810  -4.556 1.25e-05 ***
#   PCDec4              0.12429    0.05413   2.296   0.0234 *  
#   PCDec5              0.13711    0.09718   1.411   0.1609    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7447 on 121 degrees of freedom
# Multiple R-squared:  0.3041,	Adjusted R-squared:  0.2696 
# F-statistic: 8.813 on 6 and 121 DF,  p-value: 5.697e-08

final_imRelPC6.lm<-lm(RelDur ~ TransitionType+AdjSyllableStress+Voicing + PCDec4 , 
                      data = dis_complex_3)


summary(final_imRelPC6.lm)
# 
# Call:
#   lm(formula = RelDur ~ TransitionType + AdjSyllableStress + Voicing + 
#        PCDec4, data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3205 -0.5502 -0.1230  0.4693  2.2679 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.02128    0.12385  16.321  < 2e-16 ***
#   TransitionTypes#V   0.13677    0.20290   0.674   0.5015    
# TransitionTypes#s   0.91731    0.21645   4.238 4.41e-05 ***
# AdjSyllableStressu -0.29325    0.19091  -1.536   0.1271    
# Voicingvoiced      -1.03780    0.23979  -4.328 3.10e-05 ***
#   PCDec4              0.11231    0.05368   2.092   0.0385 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7477 on 122 degrees of freedom
# Multiple R-squared:  0.2927,	Adjusted R-squared:  0.2637 
# F-statistic:  10.1 on 5 and 122 DF,  p-value: 4.163e-08


# need to change the ref level

dis_complex_3$TransitionType <- relevel (dis_complex_3$TransitionType, ref= "s#s")
dis_complex_3$Voicing <- relevel (dis_complex_3$Voicing, ref= "voiced")


final_imRelPC7.lm<-lm(RelDur ~ TransitionType+Voicing + PCDec4 , 
                      data = dis_complex_3)


summary(final_imRelPC7.lm)
# Call:
#   lm(formula = RelDur ~ TransitionType + Voicing + PCDec4, data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3326 -0.4975 -0.1646  0.5227  2.2972 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        1.99671    0.22603   8.834 8.60e-15 ***
#   TransitionTypes#C -0.93541    0.21732  -4.304 3.38e-05 ***
# TransitionTypes#V -0.92971    0.18507  -5.024 1.74e-06 ***
# Voicingvoiceless   0.89942    0.22345   4.025 9.88e-05 ***
#   PCDec4             0.13335    0.05219   2.555   0.0118 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7518 on 123 degrees of freedom
# Multiple R-squared:  0.279,	Adjusted R-squared:  0.2555 
# F-statistic:  11.9 on 4 and 123 DF,  p-value: 3.331e-08



table_final_model_dis_rel_PC<-as.data.frame(coef(summary(final_imRelPC7.lm)))

xtable(table_final_model_dis_rel_PC,digits = 3)



#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Corpus")


png("disPCAbsPC4.png", units="cm", height=15, width=15, res=300, pointsize=15)

par(mfrow=c(1,1))
visreg (final_imRelPC7.lm, "PCDec4",main="", rug=F ,ylab="duration in milliseconds", xlab="PC 1", cex.axis=0.9, partial=F)

dev.off()


# I think i sjould do the model with voiceless /s/ to avoid these nasty voicing, SemT distribution

dis_voicelss<-dis_complex_3[dis_complex_3$Voicing=="voiceless",]

final_imRelPCVoiceless.lm<-lm(RelDur ~ TransitionType + PCDec4 , 
                      data = dis_voicelss)


summary(final_imRelPCVoiceless.lm)
# okay the effect remains

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Corpus")


png("disPCAbsPC4voiceless.png", units="cm", height=15, width=15, res=300, pointsize=15)

par(mfrow=c(1,1))
visreg (final_imRelPCVoiceless.lm, "PCDec4",main="", rug=F ,ylab="duration in milliseconds", xlab="PC 1", cex.axis=0.9, partial=T)

dev.off()

# what if stress is put in the pc-analysis?

dis_complex_3$NumAdjSyllableStress<-as.numeric(dis_complex_3$AdjSyllableStress)

table(dis_complex_3$AdjSyllableStress,dis_complex_3$NumAdjSyllableStress)
#    1  2
# p 88  0
# u  0 40

# one further problem is that the variables are on 
# different scales - so we need to change this

dis_complex_3$ScaledAdjSyllableStress<-as.numeric(scale(dis_complex_3$NumAdjSyllableStress))
summary(dis_complex_3$ScaledAdjSyllableStress)

# now the thirs PCA

decomposability3.pc <- prcomp(dis_complex_3[, c("ScaledAdjSyllableStress","ScaledVoicing","ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability3.pc)
# Importance of components:
#                          PC1    PC2    PC3     PC4     PC5    PC6
# Standard deviation     1.802 1.1017 0.8458 0.72696 0.44955 0.3069
# Proportion of Variance 0.541 0.2023 0.1192 0.08808 0.03368 0.0157
# Cumulative Proportion  0.541 0.7433 0.8625 0.95062 0.98430 1.0000

# So the first 4 are important

decomposability3.pc$rotation
#                               PC1         PC2         PC3        PC4         PC5
# ScaledAdjSyllableStress    -0.3423281 -0.06490834 0.897273267 -0.2697807  0.02092329
# ScaledVoicing               0.3224025  0.68971331 0.074072954 -0.2746241  0.29588214
# ScaledRelFreq               0.4034277  0.03804171 0.400827785  0.8188038  0.04738100
# ScaledMedian                0.5112453 -0.11387442 0.101175052 -0.2861679  0.48629471
# ScaledTypeOfBase            0.3223704 -0.70907631 0.005458017 -0.1725207  0.14928093
# ScaledSemanticTransparency  0.5008192  0.05365336 0.135958773 -0.2640166 -0.80684799

# PC1 is mostrly median and ST but also RelFreq . Less of the others

# PC 2 is mostly voicing and type of base

# PC3 is 'mostly Stress

# PC4 is RelFreq

# let's see whwther they influence the model

dis_complex_3$PCDec7 <- decomposability3.pc$x[, 1]
dis_complex_3$PCDec8 <- decomposability3.pc$x[, 2]
dis_complex_3$PCDec9 <- decomposability3.pc$x[, 3]
dis_complex_3$PCDec10 <- decomposability3.pc$x[, 4]

# let's see whether this has an influence


final_imPC4.lm<-lm(bc ~ TransitionType + LocSpeech + PCDec7+ PCDec8+ PCDec9+ PCDec10, 
                  data = dis_complex_3)


summary(final_imPC4.lm)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + PCDec7 + PCDec8 + 
#        PCDec9 + PCDec10, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.076379 -0.024017  0.003933  0.021735  0.073168 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.650955   0.014532  44.793  < 2e-16 ***
#   TransitionTypes#V  0.002770   0.008648   0.320 0.749332    
# TransitionTypes#s  0.035338   0.009330   3.788 0.000239 ***
# LocSpeech         -0.004610   0.001010  -4.565 1.22e-05 ***
#   PCDec7            -0.005388   0.001696  -3.177 0.001892 ** 
#   PCDec8            -0.011081   0.003404  -3.256 0.001471 ** 
#   PCDec9            -0.004164   0.004008  -1.039 0.300914    
# PCDec10            0.008845   0.003989   2.217 0.028476 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03204 on 120 degrees of freedom
# Multiple R-squared:  0.3302,	Adjusted R-squared:  0.2911 
# F-statistic: 8.451 on 7 and 120 DF,  p-value: 2.211e-08

# yo yeah...

# what if voicing is not in there

# now the 4th PCA

decomposability4.pc <- prcomp(dis_complex_3[, c("ScaledAdjSyllableStress","ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability4.pc)
# Importance of components:
#                         PC1    PC2    PC3     PC4     PC5
# Standard deviation     1.7313 0.8645 0.8260 0.63554 0.41123
# Proportion of Variance 0.5995 0.1495 0.1364 0.08078 0.03382
# Cumulative Proportion  0.5995 0.7490 0.8854 0.96618 1.00000

# So the first 4 are important

decomposability4.pc$rotation
#                               PC1           PC2         PC3        PC4         PC5
# ScaledAdjSyllableStress    -0.3517711 -0.8143570651 -0.41345384  0.2032269 -0.02888588
# ScaledRelFreq               0.4172217  0.0463012292 -0.71985444 -0.5515949  0.03653641
# ScaledMedian                0.5311241 -0.1501726859  0.03920631  0.2861509 -0.78226334
# ScaledTypeOfBase            0.4049987 -0.5586830086  0.53233129 -0.4183239  0.25588635
# ScaledSemanticTransparency  0.5060300  0.0004763965 -0.16109551  0.6305273  0.56605384

# PC1 is mostrly median and ST but also RelFreq and TypeOfbase . Less of the others

# PC 2 is stress

# PC3 is RelFreq


# let's see whwther they influence the model

dis_complex_3$PCDec11 <- decomposability4.pc$x[, 1]
dis_complex_3$PCDec12 <- decomposability4.pc$x[, 2]
dis_complex_3$PCDec13 <- decomposability4.pc$x[, 3]

# let's see whether this has an influence


final_imPC5.lm<-lm(bc ~ TransitionType + LocSpeech + PCDec11+ PCDec12+ PCDec13+Voicing, 
                   data = dis_complex_3)


summary(final_imPC5.lm)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + PCDec11 + PCDec12 + 
#        PCDec13 + Voicing, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.064988 -0.022175  0.002383  0.022387  0.063556 
# 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)        0.6624461  0.0149259  44.382  < 2e-16 ***
#   TransitionTypes#V  0.0072952  0.0088255   0.827 0.410099    
#   TransitionTypes#s  0.0352898  0.0091881   3.841 0.000197 ***
#   LocSpeech         -0.0048551  0.0010082  -4.815 4.33e-06 ***
#   PCDec11            0.0014292  0.0022518   0.635 0.526826    
#   PCDec12            0.0060128  0.0039379   1.527 0.129417    
#   PCDec13           -0.0007975  0.0040783  -0.196 0.845295    
#   Voicingvoiced     -0.0553574  0.0121727  -4.548 1.31e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03166 on 120 degrees of freedom
# Multiple R-squared:  0.346,	Adjusted R-squared:  0.3078 
# F-statistic: 9.069 on 7 and 120 DF,  p-value: 5.915e-09

# no! So inly with voicing they do something!

# Let's have a look at the voiceless dataset

dis_voicelss<-dis_complex_3[dis_complex_3$Voicing=="voiceless",]


decomposability5.pc <- prcomp(dis_voicelss[, c("ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledSemanticTransparency")])
summary(decomposability5.pc)
# Importance of components:
#                         PC1    PC2     PC3    PC4
# Standard deviation     1.6820 0.8348 0.52257 0.2509
# Proportion of Variance 0.7326 0.1804 0.07071 0.0163
# Cumulative Proportion  0.7326 0.9130 0.98370 1.0000

# So the first 2 are important

decomposability5.pc$rotation
#                             PC1        PC2        PC3          PC4
# ScaledRelFreq              -0.3983714  0.9046750  0.1511920 -0.002081624
# ScaledMedian               -0.5040035 -0.2448859  0.1260488 -0.818610447
# ScaledTypeOfBase           -0.6019518 -0.3426327  0.4716309  0.545729930
# ScaledSemanticTransparency -0.4742728 -0.0647833 -0.8595446  0.179029176

# PC1 is mostrly median and ST and TypeOfbase

# PC 2 is RelFreq


# let's see whwther they influence the model

dis_voicelss$PCDec1 <- decomposability5.pc$x[, 1]
dis_voicelss$PCDec2 <- decomposability5.pc$x[, 2]

# let's see whether this has an influence

final_voicelessPC.lm<-lm(bc ~ TransitionType*AdjSyllableStress + LocSpeech + PCDec1+ PCDec2, 
                   data = dis_voicelss)


summary(final_voicelessPC.lm)

# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + LocSpeech + 
#        PCDec1 + PCDec2, data = dis_voicelss)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.06040 -0.01823  0.00008  0.02090  0.07086 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.6585039  0.0154300  42.677  < 2e-16 ***
#   TransitionTypes#V                     0.0071515  0.0122910   0.582   0.5620    
# TransitionTypes#s                     0.0500312  0.0099497   5.028 2.33e-06 ***
# AdjSyllableStressu                    0.0115131  0.0133008   0.866   0.3889    
# LocSpeech                            -0.0046883  0.0010042  -4.669 9.94e-06 ***
#   PCDec1                                0.0002419  0.0022468   0.108   0.9145    
# PCDec2                                0.0019188  0.0036527   0.525   0.6006    
# TransitionTypes#V:AdjSyllableStressu -0.0193707  0.0175071  -1.106   0.2713    
# TransitionTypes#s:AdjSyllableStressu -0.0517054  0.0231941  -2.229   0.0282 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0297 on 95 degrees of freedom
# Multiple R-squared:  0.4042,	Adjusted R-squared:  0.354 
# F-statistic: 8.056 on 8 and 95 DF,  p-value: 3.037e-08

# no ionfluence!!! So only if one considers voicing, PC has an influence


# Let's check a Mumin with the PC

dis.lmBCPC <- lm (bc ~ TransitionType+ PCDec4 +PCDec5 +PCDec6+ Voicing+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_complex_3)

summary (dis.lmBCPC)

# Call:
#   lm(formula = bc ~ TransitionType + PCDec4 + PCDec5 + PCDec6 + 
#        Voicing + logWordFormFreqAllCoca + LocSpeech + PrecSegDur + 
#        AdjSyllableStress, data = dis_complex_3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.062766 -0.023384  0.002497  0.022786  0.066290 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.6524211  0.0258132  25.275  < 2e-16 ***
#   TransitionTypes#V       0.0078006  0.0090796   0.859 0.392026    
# TransitionTypes#s       0.0376123  0.0096487   3.898 0.000162 ***
# PCDec4                 -0.0002036  0.0025752  -0.079 0.937128    
# PCDec5                  0.0022630  0.0046483   0.487 0.627281    
# PCDec6                 -0.0015093  0.0064161  -0.235 0.814443    
# Voicingvoiced          -0.0610700  0.0176665  -3.457 0.000763 ***
#   logWordFormFreqAllCoca  0.0024471  0.0022503   1.087 0.279061    
# LocSpeech              -0.0050134  0.0010577  -4.740 6.07e-06 ***
#   PrecSegDur             -0.0479435  0.1700841  -0.282 0.778533    
# AdjSyllableStressu     -0.0128314  0.0085988  -1.492 0.138329    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03188 on 117 degrees of freedom
# Multiple R-squared:  0.3531,	Adjusted R-squared:  0.2978 
# F-statistic: 6.387 on 10 and 117 DF,  p-value: 8.725e-08


options(na.action = "na.fail") 

model_ranking <- dredge(dis.lmBCPC)

model_average_<-model.avg(model_ranking)

summary(model_average_)


# Relative variable importance: 
#                       LocSpeech TransitionType Voicing AdjSyllableStress
# Importance:          1.00      1.00           1.00    0.47             
# N containing models:  256       256            256     256             
#                      logWordFormFreqAllCoca PCDec5 PrecSegDur PCDec6 PCDec4
# Importance:          0.37                   0.29   0.26       0.25   0.25  
# N containing models:  256                    256    256        256    256     

# now the same with the voieless data set


dis.lmBCPCvoiceless <- lm (bc ~ TransitionType+ PCDec4 +PCDec5 +PCDec6+ logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_voicelss)

summary (dis.lmBCPCvoiceless)

# Call:
#   lm(formula = bc ~ TransitionType + PCDec4 + PCDec5 + PCDec6 + 
#        logWordFormFreqAllCoca + LocSpeech + PrecSegDur + AdjSyllableStress, 
#      data = dis_voicelss)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.055401 -0.021972  0.001672  0.018916  0.068657 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             6.510e-01  2.695e-02  24.154  < 2e-16 ***
#   TransitionTypes#V      -7.165e-06  9.588e-03  -0.001    0.999    
# TransitionTypes#s       4.176e-02  9.432e-03   4.427 2.57e-05 ***
# PCDec4                 -1.395e-03  2.640e-03  -0.528    0.599    
# PCDec5                  8.178e-04  4.545e-03   0.180    0.858    
# PCDec6                  3.192e-03  6.760e-03   0.472    0.638    
# logWordFormFreqAllCoca  1.186e-03  2.373e-03   0.500    0.618    
# LocSpeech              -4.741e-03  1.090e-03  -4.348 3.48e-05 ***
#   PrecSegDur              5.622e-02  1.969e-01   0.286    0.776    
# AdjSyllableStressu     -7.519e-03  8.870e-03  -0.848    0.399    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03054 on 94 degrees of freedom
# Multiple R-squared:  0.3767,	Adjusted R-squared:  0.317 
# F-statistic: 6.313 on 9 and 94 DF,  p-value: 5.989e-07

options(na.action = "na.fail") 

model_ranking <- dredge(dis.lmBCPCvoiceless)

model_average_<-model.avg(model_ranking)

summary(model_average_)

# Relative variable importance: 
#                       TransitionType LocSpeech AdjSyllableStress PCDec6
# Importance:          1.00           1.00      0.32              0.30  
# N containing models:  128            128       128               128  
#                   logWordFormFreqAllCoca PCDec4 PCDec5 PrecSegDur
# Importance:          0.28                   0.28   0.27   0.25      
# N containing models:  128                    128    128    128  




# Let's do the same with RelDur

dis_voicelss$RelDur<-dis_voicelss$AbsDur/dis_voicelss$PrecSegDur
dis_complex_3$RelDur<-dis_complex_3$AbsDur/dis_complex_3$PrecSegDur


dis.lmBCPCRel <- lm (RelDur ~ TransitionType+ PCDec4 +PCDec5 +PCDec6+ Voicing+logWordFormFreqAllCoca +LocSpeech+  AdjSyllableStress, data = dis_complex_3)

summary (dis.lmBCPCRel)

# Call:
#   lm(formula = RelDur ~ TransitionType + PCDec4 + PCDec5 + PCDec6 + 
#        Voicing + logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, 
#      data = dis_complex_3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2978 -0.5480 -0.1155  0.4602  2.1947 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.03831    0.51261   3.976 0.000121 ***
#   TransitionTypes#V       0.13037    0.21358   0.610 0.542750    
# TransitionTypes#s       0.91837    0.22547   4.073 8.43e-05 ***
# PCDec4                  0.14620    0.05852   2.498 0.013862 *  
#   PCDec5                  0.16328    0.10914   1.496 0.137309    
# PCDec6                 -0.13406    0.15065  -0.890 0.375337    
# Voicingvoiced          -1.52387    0.40713  -3.743 0.000282 ***
#   logWordFormFreqAllCoca  0.03339    0.05284   0.632 0.528677    
# LocSpeech              -0.01302    0.02395  -0.544 0.587630    
# AdjSyllableStressu     -0.34607    0.20128  -1.719 0.088174 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7501 on 118 degrees of freedom
# Multiple R-squared:  0.3114,	Adjusted R-squared:  0.2589 
# F-statistic:  5.93 on 9 and 118 DF,  p-value: 8.165e-07

options(na.action = "na.fail") 

model_ranking <- dredge(dis.lmBCPCRel)

model_average_<-model.avg(model_ranking)

summary(model_average_)


# Relative variable importance: 
#   TransitionType Voicing PCDec4 AdjSyllableStress PCDec5 logWordFormFreqAllCoca
# Importance:          1.00           0.99    0.85   0.56              0.47   0.27                  
# N containing models:  128            128     128    128               128    128                  
# PCDec6 LocSpeech
# Importance:          0.27   0.26     
# N containing models:  128    128    

# now the same with the voieless data set


dis.lmBCPCvoicelessRel <- lm (RelDur ~ TransitionType+ PCDec4 +PCDec5 +PCDec6+ logWordFormFreqAllCoca +LocSpeech+ AdjSyllableStress, data = dis_voicelss)

summary (dis.lmBCPCvoicelessRel)

# Call:
#   lm(formula = RelDur ~ TransitionType + PCDec4 + PCDec5 + PCDec6 + 
#        logWordFormFreqAllCoca + LocSpeech + AdjSyllableStress, data = dis_voicelss)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3888 -0.5521 -0.1513  0.4114  2.0738 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.122071   0.542167   3.914 0.000171 ***
#   TransitionTypes#V       0.006035   0.240510   0.025 0.980033    
# TransitionTypes#s       0.964878   0.234688   4.111 8.36e-05 ***
# PCDec4                  0.119473   0.063269   1.888 0.062034 .  
# PCDec5                  0.142190   0.113636   1.251 0.213905    
# PCDec6                 -0.047799   0.169164  -0.283 0.778127    
# logWordFormFreqAllCoca  0.002578   0.059449   0.043 0.965504    
# LocSpeech              -0.004487   0.025827  -0.174 0.862453    
# AdjSyllableStressu     -0.241252   0.221103  -1.091 0.277976    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7661 on 95 degrees of freedom
# Multiple R-squared:  0.275,	Adjusted R-squared:  0.214 
# F-statistic: 4.505 on 8 and 95 DF,  p-value: 0.0001151

options(na.action = "na.fail") 

model_ranking <- dredge(dis.lmBCPCvoicelessRel)

model_average_<-model.avg(model_ranking)

summary(model_average_)

# Relative variable importance: 
#   TransitionType PCDec4 AdjSyllableStress PCDec5 PCDec6 logWordFormFreqAllCoca
# Importance:          1.00           0.71   0.41              0.40   0.28   0.25                  
# N containing models:   64             64     64                64     64     64                  
# LocSpeech
# Importance:          0.24     
# N containing models:   64    



