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

# Let's look at the types

unique(dis_complex[dis_complex$AffixStress=="d"&dis_complex$AdjSyllableStress=="p","item.x"])
# [1] discouraging   disease        discovered     diseases       disorders      disposable    
# [7] dispelled      discolor       distracting    dissatisfied   dissolve       discovers     
# [13] disruptive     distribute     dispose        disarm         dissolved      disintegrating
# [19] discharged     disorient      dissimilar     disservice     disposables    dislike       
# [25] distractions   discover       dishonest      disables       distributors   disgraceful   
# [31] disqualify     disposal       discoveries    dissenting     dissimilated   distributing  
# [37] dislikes       disabled       disposals      disposer       distributed    dismemberment 
# [43] dissolving     distrust       discouraged   

unique(dis_complex[dis_complex$AffixStress=="p"&dis_complex$AdjSyllableStress=="u","item.x"])
#[1] discount

unique(dis_complex[dis_complex$AffixStress=="s"&dis_complex$AdjSyllableStress=="u","item.x"])
# [1] disappears    disagree      disadvantages disadvantage  disagreements disrepair    
# [7] disappear     disregard     disagreement  disability    disappeared   disappearing 
# [13] dissolution   disabilities  disadvantaged

unique(dis_complex[dis_complex$AffixStress=="u"&dis_complex$AdjSyllableStress=="p","item.x"])
#[1] distiller


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


###########################################################################
# Note: I tested different measurments of decomposability and came to 
# the concludion that ST is the best measurmet. Its validity was confirmed
# by a decomopsability rating. The median of the rating corr. with
# this measurment. However, analyses (see other script) have shown that the
# binary factor ST is a better predictor for consonant duration than the rating
###########################################################################


# AND we should do a model with RelDur later on!

# In this datset it might be worth to include random effect, however I am not sure!

tmp.lmer <- lmer(AbsDur ~ 1 + (1|item.x) + (1|Speaker), data = dis_complex)
cor(dis_complex$AbsDur, fitted(tmp.lmer))^2
#[1] 0.959417

# does not make sense

tmp.lmer <- lmer(AbsDur ~ 1 +(1|Speaker), data = dis_complex)
cor(dis_complex$AbsDur, fitted(tmp.lmer))^2
#[1] 0.9645672

#nope

tmp.lmer <- lmer(AbsDur ~ 1 + (1|item.x) , data = dis_complex)
cor(dis_complex$AbsDur, fitted(tmp.lmer))^2
#[1] 0.5291423

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


#0. Speaker: did any speaker provide more than one token of the same type?

dis_complex<-droplevels(dis_complex)


Speaker<- as.data.frame(table(dis_complex$Speaker,dis_complex$item.x))
Speaker<-Speaker[Speaker$Freq!=0 & Speaker$Freq!=1,]


# so 5 speakers provided more than 1 toke, i.e. 2

# 2 times doubles
# 3 times singletons

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




#3. AbsDur
summary (dis_complex$AbsDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.04856 0.07939 0.10150 0.10270 0.11870 0.20540   

sd (dis_complex$AbsDur)
#[1] 0.02969747


# we should also repport the meann, median and sd for each level

# first doubles

summary (dis_complex[dis_complex$TransitionType=="s#s",]$AbsDur*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 64.03  111.70  130.10  127.30  143.40  205.40 

sd (dis_complex[dis_complex$TransitionType=="s#s",]$AbsDur*1000)
#[1] 34.88147


#disV

summary (dis_complex[dis_complex$TransitionType=="s#V",]$AbsDur*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.56   82.46   97.14   94.96  107.50  166.00 

sd (dis_complex[dis_complex$TransitionType=="s#V",]$AbsDur*1000)
#[1] 21.94594

# disC

summary (dis_complex[dis_complex$TransitionType=="s#C",]$AbsDur*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 59.22   75.45   99.84   99.61  115.10  193.00 


sd (dis_complex[dis_complex$TransitionType=="s#C",]$AbsDur*1000)

#[1] 29.2165

# let's create a variable with dur in ms

dis_complex$AbsDurMS<-dis_complex$AbsDur*1000

#now let's conduct an anova to see whwther the doff are sign

anova.dis<-aov(AbsDurMS~TransitionType,data = dis_complex)

summary(anova.dis)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# TransitionType   2  18529    9265   12.39 1.23e-05 ***
#   Residuals      125  93477     748  
# they are significant

# now let's see whether all the differneces are sign

TukeyHSD(anova.dis)
# $TransitionType
# #            diff       lwr       upr     p adj
# # s#V-s#C -4.643801 -17.48157  8.193966 0.6676867
# # s#s-s#C 27.729606  11.33437 44.124844 0.0003021
# # s#s-s#V 32.373406  16.66933 48.077486 0.0000090

library(multcomp)
Tuk.dis2<-glht(anova.dis,linfct=mcp(TransitionType="Tukey"))

summary(Tuk.dis2)

# Linear Hypotheses:
#                  Estimate Std. Error t value Pr(>|t|)    
# s#V - s#C == 0   -4.644      5.412  -0.858 0.665543    
# s#s - s#C == 0   27.730      6.912   4.012 0.000333 ***
# s#s - s#V == 0   32.373      6.621   4.890  < 1e-04 ***

# only the one between doubles and signles
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


summary (dis_complex$PrecSegDur*1000)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  26.97   45.71   53.14   57.82   67.53  134.30 


sd(dis_complex$PrecSegDur*1000)
#[1] 19.42431


#8. AdjSyllableStress

summary (dis_complex$AdjSyllableStress)
#p   u 
#88 40  

#9. Type of Base


#########################################################################################
# Now let's do an initial model:                                                        #
#########################################################################################
rownames(dis_complex)<-1:nrow(dis_complex)

dis.lm <- lm (AbsDur ~ TransitionType+SemanticTransparency+Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_complex)

summary (dis.lm)

# Call:
#   lm(formula = AbsDur ~ TransitionType + SemanticTransparency + 
#        Voicing + logRelFreq + logWordFormFreqAllCoca + LocSpeech + 
#        PrecSegDur + AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.04800 -0.01945  0.00063  0.01633  0.07092 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.1026931  0.0236747   4.338 3.05e-05 ***
#   TransitionTypes#V                0.0048677  0.0073078   0.666 0.506644    
#   TransitionTypes#s                0.0314330  0.0070003   4.490 1.67e-05 ***
#   SemanticTransparencytransparent  0.0027429  0.0079163   0.346 0.729591    
#   Voicingvoiceless                 0.0414927  0.0120240   3.451 0.000776 ***
#   logRelFreq                       0.0002880  0.0010502   0.274 0.784391    
#   logWordFormFreqAllCoca           0.0016157  0.0017558   0.920 0.359354    
#   LocSpeech                       -0.0039142  0.0008138  -4.810 4.51e-06 ***
#   PrecSegDur                      -0.0353367  0.1306261  -0.271 0.787234    
#   AdjSyllableStressu              -0.0101550  0.0066383  -1.530 0.128755    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02455 on 118 degrees of freedom
# Multiple R-squared:  0.3651,	Adjusted R-squared:  0.3167 
# F-statistic: 7.541 on 9 and 118 DF,  p-value: 1.149e-08

#Before we go on fitting the model, we should check the assumptions od the model:

qqnorm (residuals (dis.lm))
qqline (residuals (dis.lm))
plot(dis.lm)

# there are a few observations which can be classified as outliers
# (115,2, 123, 115, 84)

# let's exclude them and see what happens

dis_complex_1<-dis_complex [-c(115,2, 123, 115, 84) ,]

dis.lm1 <- lm (AbsDur ~ TransitionType+SemanticTransparency+Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_complex_1)

summary (dis.lm1)
# Call:
#   lm(formula = AbsDur ~ TransitionType + SemanticTransparency + 
#        Voicing + logRelFreq + logWordFormFreqAllCoca + LocSpeech + 
#        PrecSegDur + AdjSyllableStress, data = dis_complex_1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041822 -0.016081 -0.000745  0.015447  0.053302 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.0873719  0.0217960   4.009 0.000109 ***
#   TransitionTypes#V                0.0082156  0.0068461   1.200 0.232611    
#   TransitionTypes#s                0.0295726  0.0064698   4.571 1.24e-05 ***
#   SemanticTransparencytransparent  0.0018852  0.0071709   0.263 0.793103    
#   Voicingvoiceless                 0.0408657  0.0109755   3.723 0.000307 ***
#   logRelFreq                      -0.0002516  0.0009718  -0.259 0.796220    
#   logWordFormFreqAllCoca           0.0028992  0.0016761   1.730 0.086395 .  
#   LocSpeech                       -0.0034188  0.0007446  -4.592 1.14e-05 ***
#   PrecSegDur                      -0.0832990  0.1207384  -0.690 0.491651    
#   AdjSyllableStressu              -0.0123279  0.0061111  -2.017 0.046012 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02205 on 114 degrees of freedom
# Multiple R-squared:  0.3635,	Adjusted R-squared:  0.3133 
# F-statistic: 7.234 on 9 and 114 DF,  p-value: 2.904e-08

qqnorm (residuals (dis.lm1))
qqline (residuals (dis.lm1))

# NO

# let' see what happens if we transform the dep variable 

bc <- boxcox(dis.lm)

lambda <- bc$x[which.max(bc$y)]

dis_complex$bc <- dis_complex$AbsDur^lambda

dis.lmBC <- lm (bc ~ TransitionType+SemanticTransparency+ Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_complex)

summary (dis.lmBC)


# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + Voicing + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + PrecSegDur + 
#        AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.064072 -0.024127  0.001886  0.022223  0.071057 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.5941512  0.0306467  19.387  < 2e-16 ***
#   TransitionTypes#V                0.0072740  0.0094598   0.769 0.443468    
#   TransitionTypes#s                0.0386882  0.0090618   4.269 3.98e-05 ***
#   SemanticTransparencytransparent  0.0029059  0.0102475   0.284 0.777234    
#   Voicingvoiceless                 0.0554678  0.0155650   3.564 0.000529 ***
#   logRelFreq                       0.0002464  0.0013594   0.181 0.856496    
#   logWordFormFreqAllCoca           0.0024194  0.0022729   1.064 0.289293    
#   LocSpeech                       -0.0049991  0.0010535  -4.745 5.89e-06 ***
#   PrecSegDur                      -0.0511517  0.1690943  -0.303 0.762800    
#   AdjSyllableStressu              -0.0118084  0.0085932  -1.374 0.171999    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03178 on 118 degrees of freedom
# Multiple R-squared:  0.352,	Adjusted R-squared:  0.3025 
# F-statistic: 7.121 on 9 and 118 DF,  p-value: 3.421e-08
# became worse...



qqnorm (residuals (dis.lmBC))
qqline (residuals (dis.lmBC))
plot(dis.lmBC)
# we still have the same outliers, and the model did not become better
# thus we will exclud ethe outliers (115,2, 31, 84)


#--> let us exclude the outliers from the bc model

dis_complex [c(115,2, 31, 84) ,c("ItemID","item.x", "TransitionType")]

#     ItemID       item.x TransitionType
# 115    268    disregard            s#C
# 2        3 discouraging            s#C
# 31      72    dissolved            s#s
# 84     199 dissimilated            s#s

# So, let's go on without them: We create the new dataset dis2

dis_2 <- dis_complex [-c(115,2, 31, 84), ]

# now, let's do another model

dis_2.BClm <- lm (bc ~ TransitionType+SemanticTransparency+Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_2)

summary (dis_2.BClm)

# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + Voicing + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + PrecSegDur + 
#        AdjSyllableStress, data = dis_2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.05848 -0.02088  0.00099  0.02234  0.05933 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.5879558  0.0298902  19.671  < 2e-16 ***
#   TransitionTypes#V                0.0094756  0.0093458   1.014  0.31278    
#   TransitionTypes#s                0.0407922  0.0087287   4.673 8.18e-06 ***
#   SemanticTransparencytransparent  0.0031529  0.0097350   0.324  0.74663    
#   Voicingvoiceless                 0.0507721  0.0151528   3.351  0.00109 ** 
#   logRelFreq                      -0.0005979  0.0013071  -0.457  0.64822    
#   logWordFormFreqAllCoca           0.0036897  0.0022793   1.619  0.10826    
#   LocSpeech                       -0.0047557  0.0010069  -4.723 6.67e-06 ***
#   PrecSegDur                      -0.1389379  0.1630475  -0.852  0.39593    
#   AdjSyllableStressu              -0.0141470  0.0083098  -1.702  0.09140 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02985 on 114 degrees of freedom
# Multiple R-squared:  0.3796,	Adjusted R-squared:  0.3306 
# F-statistic:  7.75 on 9 and 114 DF,  p-value: 7.806e-09
qqnorm (residuals (dis_2.BClm))
qqline (residuals (dis_2.BClm))

# I think that is okay, but not the best..

# We should try log!


dis_2.lm1<- lm (log(AbsDur) ~ TransitionType+SemanticTransparency+Voicing+logRelFreq+logWordFormFreqAllCoca +LocSpeech+ PrecSegDur + AdjSyllableStress, data = dis_2)

summary (dis_2.lm1)

# Call:
#   lm(formula = log(AbsDur) ~ TransitionType + SemanticTransparency + 
#        Voicing + logRelFreq + logWordFormFreqAllCoca + LocSpeech + 
#        PrecSegDur + AdjSyllableStress, data = dis_2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46192 -0.15842  0.01313  0.16804  0.44510 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                     -2.405443   0.227194 -10.588  < 2e-16 ***
#   TransitionTypes#V                0.071145   0.071037   1.002  0.31870    
#   TransitionTypes#s                0.301404   0.066347   4.543 1.39e-05 ***
#   SemanticTransparencytransparent  0.022833   0.073996   0.309  0.75821    
#   Voicingvoiceless                 0.386350   0.115176   3.354  0.00108 ** 
#   logRelFreq                      -0.004370   0.009935  -0.440  0.66089    
#   logWordFormFreqAllCoca           0.028297   0.017325   1.633  0.10517    
#   LocSpeech                       -0.035832   0.007653  -4.682 7.90e-06 ***
#   PrecSegDur                      -1.028130   1.239318  -0.830  0.40850    
#   AdjSyllableStressu              -0.102521   0.063163  -1.623  0.10732    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2269 on 114 degrees of freedom
# Multiple R-squared:  0.3712,	Adjusted R-squared:  0.3216 
# F-statistic: 7.478 on 9 and 114 DF,  p-value: 1.555e-08

qqnorm (residuals (dis_2.lm1))
qqline (residuals (dis_2.lm1))

# even worse!!!!!


# I think the best onw is the one with the BC

plot(dis.lmBC)

# we will go with tha model


#####################################################################
# Let's now look at interactions
#####################################################################

# There might be an interactions.
# We will check them systematically:

#1.Semantic Transparency and TransitionType

dis_int_SemT_Tr.lm <- lm (bc ~ TransitionType*SemanticTransparency, data = dis_complex)

summary (dis_int_SemT_Tr.lm)

# Call:
#   lm(formula = bc ~ TransitionType * SemanticTransparency, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.067061 -0.026294  0.003296  0.020018  0.099046 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.5947638  0.0068155  87.267   <2e-16 ***
#   TransitionTypes#V                                 -0.0171360  0.0102232  -1.676   0.0963 .  
# TransitionTypes#s                                  0.0055640  0.0132469   0.420   0.6752    
# SemanticTransparencytransparent                    0.0001315  0.0102232   0.013   0.9898    
# TransitionTypes#V:SemanticTransparencytransparent  0.0183424  0.0138692   1.323   0.1885    
# TransitionTypes#s:SemanticTransparencytransparent  0.0446820  0.0176341   2.534   0.0125 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03408 on 122 degrees of freedom
# Multiple R-squared:  0.2295,	Adjusted R-squared:  0.1979 
# F-statistic: 7.267 on 5 and 122 DF,  p-value: 5.459e-06

# yes

#2.Frequencies and TransitionType?

dis_int_WordFr_Tr.lm <- lm (bc ~ TransitionType*logWordFormFreqAllCoca, data = dis_complex)

summary (dis_int_WordFr_Tr.lm)


# NO interaction

# 3. rel frequencies and TransitionType?

dis_int_RelFr_Tr.lm <- lm (bc ~ TransitionType*logRelFreq, data = dis_complex)

summary (dis_int_RelFr_Tr.lm)


# might be


#4. speechRate and ST?

dis_int_SpeechRate_Sem.lm <- lm (bc ~ SemanticTransparency*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_Sem.lm)

#no

#6. speechRate and RelFreq?

dis_int_SpeechRate_RelF.lm <- lm (bc ~ logRelFreq*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_RelF.lm)

#no

#6. speechRate and Trans?

dis_int_SpeechRate_TT.lm <- lm (bc ~ TransitionType*LocSpeech, data = dis_complex)

summary (dis_int_SpeechRate_TT.lm)

#might be


#6. Stress and Trans?

dis_int_TT_Stress.lm <- lm (bc~ AdjSyllableStress*TransitionType, data = dis_complex)

summary (dis_int_TT_Stress.lm)

#YES! there might be something

#6. Stress and ST?

dis_int_ST_Stress.lm <- lm (bc~ AdjSyllableStress*SemanticTransparency, data = dis_complex)

summary (dis_int_ST_Stress.lm)

#no

#########################################################################################
#   summary interactions:                                                               #
#     - there might be interaction between SemTran and TransitionType 
#     - there is might be interaction between LocSpeech and TT
#     - there might be an interaction between Stress and TransitionType                      ##
#     - there might be interaction between RelFreq and TransitionTyp
#     - there is no interaction between Freq and TransitionTyp
#     - there is no interaction between LocSpeech and RelFreq
#     - there is no interaction between LocSpeech and ST 
#     - there is no interaction between Stress and ST
#     - there might be an inteaction between RelFreq and SemTrans which is problematic        #
#     because of the distributuion of the data
#     - because voiced items only occur with opaque s#V ans s#s items, we cannot test
#     interactions with voicing
#########################################################################################


# We will simplify the model first and the look at the interactions again

summary(dis.lmBC)
# the best model is the bc-model, we will use that one
# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + Voicing + 
#        logRelFreq + logWordFormFreqAllCoca + LocSpeech + PrecSegDur + 
#        AdjSyllableStress, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.064072 -0.024127  0.001886  0.022223  0.071057 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.5941512  0.0306467  19.387  < 2e-16 ***
#   TransitionTypes#V                0.0072740  0.0094598   0.769 0.443468    
#   TransitionTypes#s                0.0386882  0.0090618   4.269 3.98e-05 ***
#   SemanticTransparencytransparent  0.0029059  0.0102475   0.284 0.777234    
#   Voicingvoiceless                 0.0554678  0.0155650   3.564 0.000529 ***
#   logRelFreq                       0.0002464  0.0013594   0.181 0.856496    
#   logWordFormFreqAllCoca           0.0024194  0.0022729   1.064 0.289293    
#   LocSpeech                       -0.0049991  0.0010535  -4.745 5.89e-06 ***
#   PrecSegDur                      -0.0511517  0.1690943  -0.303 0.762800    
#   AdjSyllableStressu              -0.0118084  0.0085932  -1.374 0.171999    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03178 on 118 degrees of freedom
# Multiple R-squared:  0.352,	Adjusted R-squared:  0.3025 
# F-statistic: 7.121 on 9 and 118 DF,  p-value: 3.421e-08

# okay now we should throw out RelFeq, but maybe there is something going on with WordFreq

# The two frequenvy varoables are fairly similar. Which is a better predictor?

dis_RelFr.lm <- lm (bc ~ logRelFreq, data = dis_complex)

summary (dis_RelFr.lm)

# Call:
#   lm(formula = bc ~ logRelFreq, data = dis_complex)
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


dis_int_WordFr.lm <- lm (bc ~ logWordFormFreqSpokenCOCA, data = dis_complex)

summary (dis_int_WordFr.lm)

# Call:
#   lm(formula = bc ~ logWordFormFreqSpokenCOCA, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.084131 -0.024600  0.002821  0.022731  0.103556 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                0.613996   0.010506  58.442   <2e-16 ***
#   logWordFormFreqSpokenCOCA -0.002574   0.001688  -1.525     0.13    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03785 on 126 degrees of freedom
# Multiple R-squared:  0.01812,	Adjusted R-squared:  0.01033 
# F-statistic: 2.325 on 1 and 126 DF,  p-value: 0.1298

# It seems that RelFreq is a btter predictor. What if we have the two in one model

dis_int_WordFr_RelFreq.lm <- lm (bc ~ logWordFormFreqSpokenCOCA+logRelFreq, data = dis_complex)

summary (dis_int_WordFr_RelFreq.lm)


# Call:
#   lm(formula = bc ~ logWordFormFreqSpokenCOCA + logRelFreq, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.077467 -0.022395  0.005122  0.021648  0.108699 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                0.609815   0.010608  57.488   <2e-16 ***
#   logWordFormFreqSpokenCOCA -0.001849   0.001710  -1.081   0.2817    
# logRelFreq                -0.002266   0.001158  -1.956   0.0527 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03744 on 125 degrees of freedom
# Multiple R-squared:  0.04729,	Adjusted R-squared:  0.03205 
# F-statistic: 3.102 on 2 and 125 DF,  p-value: 0.04842

# shows us the same, if one has an effect then it should be RelFrw. Thus we will 
# exclude WordFormFreq

dis.lmBC2<-update(dis.lmBC, ~ . - logWordFormFreqAllCoca)

summary(dis.lmBC2)

# NOTE: in the output the factors are orded in a different sequence (can be ignored!)

# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + logRelFreq + 
#        LocSpeech + PrecSegDur + AdjSyllableStress + Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.066652 -0.022301  0.003442  0.021685  0.070897 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.6180983  0.0208230  29.683  < 2e-16 ***
#   TransitionTypes#V                0.0061556  0.0094065   0.654 0.514119    
#   TransitionTypes#s                0.0365937  0.0088506   4.135 6.65e-05 ***
#   SemanticTransparencytransparent  0.0069745  0.0095133   0.733 0.464920    
#   logRelFreq                       0.0006641  0.0013023   0.510 0.611011    
#   LocSpeech                       -0.0049404  0.0010526  -4.693 7.25e-06 ***
#   PrecSegDur                      -0.0625642  0.1688484  -0.371 0.711643    
#   AdjSyllableStressu              -0.0090485  0.0081973  -1.104 0.271892    
#   Voicingvoiceless                 0.0464543  0.0130675   3.555 0.000543 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0318 on 119 degrees of freedom
# Multiple R-squared:  0.3457,	Adjusted R-squared:  0.3017 
# F-statistic:  7.86 on 8 and 119 DF,  p-value: 1.921e-08


# Now PrecSegDur


dis.lmBC3<-update(dis.lmBC2, ~ . - PrecSegDur)

summary(dis.lmBC3)

# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + logRelFreq + 
#        LocSpeech + AdjSyllableStress + Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.066484 -0.023036  0.003202  0.021947  0.070073 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.6125980  0.0145504  42.102  < 2e-16 ***
#   TransitionTypes#V                0.0061224  0.0093722   0.653 0.514845    
#   TransitionTypes#s                0.0368856  0.0087837   4.199 5.17e-05 ***
#   SemanticTransparencytransparent  0.0063914  0.0093485   0.684 0.495494    
#   logRelFreq                       0.0007046  0.0012930   0.545 0.586803    
#   LocSpeech                       -0.0048329  0.0010082  -4.793 4.75e-06 ***
#    AdjSyllableStressu              -0.0093806  0.0081189  -1.155 0.250220    
#   Voicingvoiceless                 0.0475412  0.0126882   3.747 0.000277 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03168 on 120 degrees of freedom
# Multiple R-squared:  0.345,	Adjusted R-squared:  0.3068 
# F-statistic: 9.028 on 7 and 120 DF,  p-value: 6.44e-09

# now RelFreq

dis.lmBC4<-update(dis.lmBC3, ~ . - logRelFreq)

summary(dis.lmBC4)

# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency + LocSpeech + 
#        AdjSyllableStress + Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.066202 -0.022436  0.001786  0.021636  0.073260 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.613101   0.014479  42.345  < 2e-16 ***
#   TransitionTypes#V                0.007295   0.009095   0.802   0.4241    
#   TransitionTypes#s                0.035875   0.008561   4.191 5.32e-05 ***
#   SemanticTransparencytransparent  0.003937   0.008169   0.482   0.6307    
#   LocSpeech                       -0.004806   0.001004  -4.786 4.85e-06 ***
#   AdjSyllableStressu              -0.010485   0.007839  -1.338   0.1835    
#   Voicingvoiceless                 0.048263   0.012582   3.836   0.0002 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03159 on 121 degrees of freedom
# Multiple R-squared:  0.3434,	Adjusted R-squared:  0.3108 
# F-statistic: 10.54 on 6 and 121 DF,  p-value: 2.142e-09

# now ST
dis.lmBC5<-update(dis.lmBC4, ~ . - SemanticTransparency)

summary(dis.lmBC5)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + AdjSyllableStress + 
#        Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.067378 -0.021168  0.001868  0.022249  0.071496 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         0.611332   0.013962  43.787  < 2e-16 ***
#   TransitionTypes#V   0.009273   0.008092   1.146    0.254    
#   TransitionTypes#s   0.037187   0.008091   4.596 1.06e-05 ***
#   LocSpeech          -0.004816   0.001001  -4.812 4.32e-06 ***
#   AdjSyllableStressu -0.010239   0.007797  -1.313    0.192    
#   Voicingvoiceless    0.051879   0.010069   5.152 1.00e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03149 on 122 degrees of freedom
# Multiple R-squared:  0.3421,	Adjusted R-squared:  0.3151 
# F-statistic: 12.69 on 5 and 122 DF,  p-value: 6.264e-10

# now stress


dis.lmBC6<-update(dis.lmBC5, ~ . - AdjSyllableStress)

summary(dis.lmBC6)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech + Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069725 -0.019449  0.002427  0.023886  0.073631 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.615309   0.013669  45.014  < 2e-16 ***
#   TransitionTypes#V  0.003443   0.006785   0.507    0.613    
# TransitionTypes#s  0.036436   0.008095   4.501 1.55e-05 ***
# LocSpeech         -0.004715   0.001001  -4.711 6.53e-06 ***
#   Voicingvoiceless   0.044911   0.008582   5.233 6.97e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03158 on 123 degrees of freedom
# Multiple R-squared:  0.3328,	Adjusted R-squared:  0.3111 
# F-statistic: 15.34 on 4 and 123 DF,  p-value: 3.34e-10


# So this would be the final model without any interactions

# What I find out so far:

# - Doubles are longer than singles 
# - the higher the speech rate, the shorter s
# - voiced sounds are shorter

# We need to change the reference levels of TransitionType to doubles
# to see whether they are longer than both singletons

dis_complex$TransitionType <- relevel (dis_complex$TransitionType, ref= "s#s")

# now let's redo the model

dis.lmBC7<- lm(bc ~ TransitionType + Voicing + LocSpeech, data = dis_complex)

summary(dis.lmBC7)


# Call:
#   lm(formula = bc ~ TransitionType + Voicing + LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069725 -0.019449  0.002427  0.023886  0.073631 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        0.651745   0.013440  48.492  < 2e-16 ***
#   TransitionTypes#C -0.036436   0.008095  -4.501 1.55e-05 ***
#   TransitionTypes#V -0.032993   0.007764  -4.250 4.19e-05 ***
#   Voicingvoiceless   0.044911   0.008582   5.233 6.97e-07 ***
#   LocSpeech         -0.004715   0.001001  -4.711 6.53e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03158 on 123 degrees of freedom
# Multiple R-squared:  0.3328,	Adjusted R-squared:  0.3111 
# F-statistic: 15.34 on 4 and 123 DF,  p-value: 3.34e-10

# okay so doubles are longer! dis geminates

#############################################
# now let's take a look at the interactions again
###################################################


# first RelFreq and TrType

dis.lmBC6InterRel<- lm(bc ~ TransitionType*logRelFreq+Voicing + LocSpeech, data = dis_complex)

summary(dis.lmBC6InterRel)

#no interaction. So we can forget about RelFreq  


dis.lmBC6InterSRTT<- lm(bc ~ TransitionType*LocSpeech+Voicing, data = dis_complex)

summary(dis.lmBC6InterSRTT)

# no


# What if we let TrType amd ST interact

dis.lmBC6InterSem<-lm(bc ~ TransitionType*SemanticTransparency +Voicing+ LocSpeech, data = dis_complex)


summary(dis.lmBC6InterSem)

# Call:
#   lm(formula = bc ~ TransitionType * SemanticTransparency + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.065841 -0.019742  0.000671  0.020556  0.073769 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                        0.6214331  0.0183716  33.826  < 2e-16 ***
#   TransitionTypes#C                                 -0.0254465  0.0152006  -1.674  0.09673 .  
#   TransitionTypes#V                                  0.0025962  0.0169945   0.153  0.87884    
#   SemanticTransparencytransparent                    0.0223092  0.0160292   1.392  0.16656    
#   Voicingvoiceless                                   0.0635394  0.0211523   3.004  0.00325 ** 
#   LocSpeech                                         -0.0046492  0.0009837  -4.726 6.28e-06 ***
#   TransitionTypes#C:SemanticTransparencytransparent -0.0227917  0.0185331  -1.230  0.22118    
#   TransitionTypes#V:SemanticTransparencytransparent -0.0501532  0.0194368  -2.580  0.01108 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03103 on 120 degrees of freedom
# Multiple R-squared:  0.3717,	Adjusted R-squared:  0.3351 
# F-statistic: 10.14 on 7 and 120 DF,  p-value: 6.317e-10

# hmmmmm what if we have a 3-way interaction

dis.lm6InterSemRel<- lm(bc ~ TransitionType*SemanticTransparency*logRelFreq + Voicing+LocSpeech, data = dis_complex)


summary(dis.lm6InterSemRel)

# THAT does not have any effect, so we can forget about this

# what about an interaction with stress pattern


dis.lm6InterStressTT<- lm(bc ~ TransitionType*AdjSyllableStress +Voicing + LocSpeech, data = dis_complex)


summary(dis.lm6InterStressTT)

# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.6444137  0.0147602  43.659  < 2e-16 ***
#   TransitionTypes#C                    -0.0449500  0.0086844  -5.176 9.23e-07 ***
#   TransitionTypes#V                    -0.0256645  0.0102393  -2.506   0.0135 *  
#   AdjSyllableStressu                   -0.0357422  0.0193093  -1.851   0.0666 .  
#   Voicingvoiceless                      0.0571720  0.0106532   5.367 3.97e-07 ***
#   LocSpeech                            -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0478538  0.0231416   2.068   0.0408 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0168113  0.0214858   0.782   0.4355    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03107 on 120 degrees of freedom
# Multiple R-squared:  0.3699,	Adjusted R-squared:  0.3331 
# F-statistic: 10.06 on 7 and 120 DF,  p-value: 7.439e-10

# What about Stress and ST
dis.lm6InterStressST<- lm(bc ~ TransitionType+SemanticTransparency*AdjSyllableStress +Voicing + LocSpeech, data = dis_complex)


summary(dis.lm6InterStressST)
# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency * AdjSyllableStress + 
#        Voicing + LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.072134 -0.019756  0.004525  0.021555  0.069399 
# 
# Coefficients:
#                                                       Estimate Std. Error t value Pr(>|t|)    
#  ( Intercept)                                         0.6561700  0.0140951  46.553  < 2e-16 ***
#   TransitionTypes#C                                  -0.0419162  0.0090679  -4.622 9.63e-06 ***
#   TransitionTypes#V                                  -0.0353891  0.0094312  -3.752 0.000271 ***
#   SemanticTransparencytransparent                    -0.0037006  0.0090576  -0.409 0.683587    
#   AdjSyllableStressu                                 -0.0408103  0.0179689  -2.271 0.024920 *  
#   Voicingvoiceless                                    0.0522644  0.0126362   4.136 6.58e-05 ***
#   LocSpeech                                          -0.0049580  0.0009972  -4.972 2.23e-06 ***
#   SemanticTransparencytransparent:AdjSyllableStressu  0.0380165  0.0203182   1.871 0.063773 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03127 on 120 degrees of freedom
# Multiple R-squared:  0.362,	Adjusted R-squared:  0.3247 
# F-statistic: 9.725 on 7 and 120 DF,  p-value: 1.492e-09


# Thus we have 3 possible final models: with an interaction and without

summary(dis.lmBC6InterSem)

# Call:
#   lm(formula = bc ~ TransitionType * SemanticTransparency + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.065841 -0.019742  0.000671  0.020556  0.073769 
# 
# Coefficients:
#                                                      Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                        0.6214331  0.0183716  33.826  < 2e-16 ***
#   TransitionTypes#C                                 -0.0254465  0.0152006  -1.674  0.09673 .  
#   TransitionTypes#V                                  0.0025962  0.0169945   0.153  0.87884    
#   SemanticTransparencytransparent                    0.0223092  0.0160292   1.392  0.16656    
#   Voicingvoiceless                                   0.0635394  0.0211523   3.004  0.00325 ** 
#   LocSpeech                                         -0.0046492  0.0009837  -4.726 6.28e-06 ***
#   TransitionTypes#C:SemanticTransparencytransparent -0.0227917  0.0185331  -1.230  0.22118    
#   TransitionTypes#V:SemanticTransparencytransparent -0.0501532  0.0194368  -2.580  0.01108 *  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03103 on 120 degrees of freedom
# Multiple R-squared:  0.3717,	Adjusted R-squared:  0.3351 
# F-statistic: 10.14 on 7 and 120 DF,  p-value: 6.317e-10

visreg(dis.lmBC6InterSem, "LocSpeech", trans=function(x) x^(1/lambda)*1000)

visreg(dis.lmBC6InterSem, "TransitionType", by="SemanticTransparency", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lmBC6InterSem, "SemanticTransparency", by="TransitionType", trans=function(x) x^(1/lambda)*1000) 

# one problem with this is the distribution of voiced items in the dataset

table(dis_complex$Voicing,dis_complex$TransitionType)
#           s#s s#C s#V
# voiced      4   0  20
# voiceless  20  45  39

table(dis_complex$Voicing,dis_complex$SemanticTransparency)

#             opaque transparent
# voiced        24           0
# voiceless     30          74

table(dis_complex$TransitionType,dis_complex$SemanticTransparency)
#       opaque transparent
# s#s      9          15
# s#C     25          20
# s#V     20          39

# are all opaques with a doubles also voiced?

dis_complex[dis_complex$TransitionType=="s#s" & dis_complex$SemanticTransparency=="opaque",c("item.x","Voicing")]

# no.but only 5 voiceless 

#          item.x   Voicing
# 17      dissolve    voiced
# 31     dissolved    voiced
# 71   dissolution voiceless
# 79    dissenting voiceless
# 84  dissimilated voiceless
# 89   dissolution voiceless
# 94   dissolution voiceless
# 102    dissolved    voiced
# 122   dissolving    voiced

# so basically it is difficult to test the effect of Semantic Transparanecy in this dataset
# since most of the opaque items for s#s and s#V have a vocied /s/ which is shorter. 

# so for s#V something is predicted which does not exists. Hence, we must be carful with
# unterpreting this difference. For s#V there is no difference, the question is hence:
# is there a difference between s#s opaque and transparent?
# Let's have a look at the opaque ones

dis_complex[dis_complex$SemanticTransparency=="opaque" & dis_complex$TransitionType=="s#s","item.x"]
# 5 are opaque and 5 are transparent: What needs to be done is basically have a 

#3 way interaction but I don't think that's possible with this dataset since we only have
# voiced ones in 2 out of 6 categories. Hence, let's just have a look at the effect
# when excluding the voiced doubles


dis_complex_no_voice<- dis_complex[dis_complex$Voicing!="voiced",]



# so let' look at a model now



disNoVoice.lm<- lm(bc ~ TransitionType+SemanticTransparency+AdjSyllableStress +LocSpeech, data = dis_complex_no_voice)

summary(disNoVoice.lm)

# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.700084   0.016425  42.622  < 2e-16 ***
#   TransitionTypes#C               -0.040627   0.008438  -4.815 5.36e-06 ***
#   TransitionTypes#V               -0.039408   0.009723  -4.053 0.000101 ***
#   SemanticTransparencytransparent  0.005516   0.007814   0.706 0.481913    
#   AdjSyllableStressu              -0.006342   0.007706  -0.823 0.412517    
#   LocSpeech                       -0.004765   0.001003  -4.751 6.93e-06 ***
  
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03009 on 98 degrees of freedom
# Multiple R-squared:  0.3692,	Adjusted R-squared:  0.337 
# F-statistic: 11.47 on 5 and 98 DF,  p-value: 9.898e-09
---

# indicually only LocSpeech and TransitionType. Semantic Transparency does not have an effect

# let's have a look at only doubles ( we canb't test an interaction with TT ans ST when we exclude voiced items)


dis_complex_double<-dis_complex_no_voice[dis_complex_no_voice$TransitionType=="s#s",]
dis_complex_double<-dis_complex_no_voice[dis_complex_no_voice$TransitionType=="s#s",]

dis_double.lm<-lm(AbsDur~SemanticTransparency + AdjSyllableStress + LocSpeech, data= dis_complex_double)

summary(dis_double.lm)

# Call:
#   lm(formula = AbsDur ~ SemanticTransparency + AdjSyllableStress + 
#        LocSpeech, data = dis_complex_double)
# 
# # Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.033605 -0.017405  0.003694  0.012528  0.048140 
# 
# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                      0.296761   0.058251   5.095 0.000108 ***
#   SemanticTransparencytransparent -0.018823   0.019677  -0.957 0.352985    
#   AdjSyllableStressu              -0.061668   0.024477  -2.519 0.022768 *  
#   LocSpeech                       -0.010226   0.003513  -2.911 0.010212 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0233 on 16 degrees of freedom
# Multiple R-squared:  0.4606,	Adjusted R-squared:  0.3594 
# F-statistic: 4.553 on 3 and 16 DF,  p-value: 0.01725

# when only havinf voiceless items, there is no effect of semantic transparency. Hence,
# one should not investigate the interaction any further


# However, there was a second interesting interactoin

summary(dis.lm6InterStressTT)
# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.6444137  0.0147602  43.659  < 2e-16 ***
#   TransitionTypes#C                    -0.0449500  0.0086844  -5.176 9.23e-07 ***
#   TransitionTypes#V                    -0.0256645  0.0102393  -2.506   0.0135 *  
#   AdjSyllableStressu                   -0.0357422  0.0193093  -1.851   0.0666 .  
#   Voicingvoiceless                      0.0571720  0.0106532   5.367 3.97e-07 ***
#   LocSpeech                            -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0478538  0.0231416   2.068   0.0408 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0168113  0.0214858   0.782   0.4355   

visreg(dis.lm6InterStressTT, "LocSpeech", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressTT, "TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressTT, "AdjSyllableStress", by="TransitionType", trans=function(x) x^(1/lambda)*1000 )

# It seems that if a vowel follows there is an effct of stress, - not if a consonant follows. But
# let's take a look at the relation of stress and voicing again

table(dis_complex$AdjSyllableStress, dis_complex$Voicing)
#    voiced voiceless
# p     24        64
# u      0        40

# so basically every time somehing is voiced, it also precedes a stressed syllable.


table(dis_complex$AdjSyllableStress, dis_complex$TransitionType)


#   s#s s#C s#V
# p  21  38  29
# u   3   7  30

# so for s#s and s#C it is hardly interpretable....only s#V has enough
# items, however, the types diesease and diseases are stressed. Let's have
# a look at how many of thestressed s#V items are those types


dis_complex[dis_complex$TransitionType=="s#V"& dis_complex$AdjSyllableStress=="p","item.x"]
# [1] disease        diseases       disorders      diseases       disease        diseases      
# [7] disarm         disease        disease        diseases       disintegrating disease       
# [13] disorient      disease        disease        dishonest      disables       disease       
# [19] disease        diseases       disease        diseases       disorders      disabled      
# [25] disintegrating diseases       diseases       disease        diseases 

# so the majority.... I predict, this interaction would vanish if we only look at voiceless
# items. Let's see

dis.lm6InterStressTTNoVoice<-lm(formula = bc ~ TransitionType * AdjSyllableStress +
  LocSpeech, data = dis_complex_no_voice)

summary(dis.lm6InterStressTTNoVoice)

# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + LocSpeech, 
#      data = dis_complex_no_voice)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063771 -0.017149 -0.000013  0.021723  0.075797 
# 
# Coefficients:
#                                        Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.7080344  0.0153395  46.157  < 2e-16 ***
#   TransitionTypes#C                    -0.0504575  0.0086037  -5.865 6.23e-08 ***
#   TransitionTypes#V                    -0.0417055  0.0121433  -3.434 0.000875 ***
#   AdjSyllableStressu                   -0.0413639  0.0184716  -2.239 0.027419 *  
#   LocSpeech                            -0.0046585  0.0009936  -4.688 9.00e-06 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0533449  0.0220658   2.418 0.017494 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0329438  0.0216409   1.522 0.131188    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02943 on 97 degrees of freedom
# Multiple R-squared:  0.4025,	Adjusted R-squared:  0.3655 
# F-statistic: 10.89 on 6 and 97 DF,  p-value: 3.065e-09


# it still is significant

visreg(dis.lm6InterStressTTNoVoice, "LocSpeech", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressTTNoVoice, "TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressTTNoVoice, "AdjSyllableStress", by="TransitionType", trans=function(x) x^(1/lambda)*1000 )

# so, for the item, which are unstressed and have a double, we do not find gemination. However,
# these are only 3items: let's have a look at them


dis_complex_no_voice[dis_complex_no_voice$TransitionType=="s#s"&dis_complex_no_voice$AdjSyllableStress=="u",c("item.x","SemanticTransparency")]
#[1] dissolution dissolution dissolution --> the opaque ones

dis_complex_no_voice[dis_complex_no_voice$TransitionType=="s#s"& dis_complex_no_voice$SemanticTransparency=="opaque","item.x"]
#[1] dissolution  dissenting   dissimilated dissolution  dissolution 

# so basically the ones which on a ponological level show "resyllabification" but only 3 items:
# one can thus say, that NOT all double s-items geminate but we only have a few types
# which do not: they have an unstressed syllable following, aere opaque AND/OR are voiced

# let's have a look at our full dataset again:

visreg(dis.lm6InterStressTT, "AdjSyllableStress", by="TransitionType", trans=function(x) x^(1/lambda)*1000 )
visreg(dis.lm6InterStressTT ,"TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )
visreg(dis.lm6InterStressTTNoVoice ,"TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )

# So we should take the model with the interaction with stress since the interaction with
# semantic transparency vanishes when voiced sounds are excluded.Voiced ones are all opaque however, as well
# as the unstressed ones.


# let's quicklx check the third possible interaction

visreg(dis.lm6InterStressST, "LocSpeech", trans=function(x) x^(1/lambda)*1000)

visreg(dis.lm6InterStressST, "TransitionType", by="SemanticTransparency", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressST, "SemanticTransparency", by="TransitionType", trans=function(x) x^(1/lambda)*1000) 

summary(dis.lm6InterStressST)
#Multiple R-squared:  0.362,	Adjusted R-squared:  0.3247 

# so this model is worse and it does not add anything to the model. We discard it

# So, this is our final model:

summary(dis.lm6InterStressTT)

#
# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.6444137  0.0147602  43.659  < 2e-16 ***
#   TransitionTypes#C                    -0.0449500  0.0086844  -5.176 9.23e-07 ***
#   TransitionTypes#V                    -0.0256645  0.0102393  -2.506   0.0135 *  
#   AdjSyllableStressu                   -0.0357422  0.0193093  -1.851   0.0666 .  
#   Voicingvoiceless                      0.0571720  0.0106532   5.367 3.97e-07 ***
#   LocSpeech                            -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0478538  0.0231416   2.068   0.0408 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0168113  0.0214858   0.782   0.4355    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03107 on 120 degrees of freedom
# Multiple R-squared:  0.3699,	Adjusted R-squared:  0.3331 
# F-statistic: 10.06 on 7 and 120 DF,  p-value: 7.439e-10

# As explained before, the effect of s#V for primary stressed items is however difficult
# to interpret, since the majority of those items is voiced, when  excluding those items, the 
# effect looks different (no difference between primary and unstressed ones for disV).




####################################################################################
#                 summary
#
# - LocSpeech has the expected effect: The faster someone speaks, the shorter the s
# - Voiced sounds are shorter
# - An interaction between Stress and TransitionType
#       - For words with primary stress: s#s longer than s#C and s#V BUT no difference between s#C and s#V
#       - For words with no stress: s#s and s#C land s#V are of the same duration
#         BUT s#s only has 3 observations! We need to be careful with the interpretation
# We could look at it from another perspective:
#         - For s#s: primary longer than unstresses
#         - For s#V: No differece between primary and unstressed.
#         - For s#C: No differece between primary and unstressed.
#
# Hierarchy of durations: s#s-transparent< everything else
#####################################################################################################


# However,let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 

model_ranking <- dredge(dis.lmBC)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = bc ~ <256 unique rhs>, data = dis_complex)
# 
# Component models: 
#   df logLik    AICc delta weight
# 278       6 263.19 -513.69  0.00   0.14
# 1278      7 264.09 -513.24  0.44   0.11
# 12478     8 264.97 -512.73  0.95   0.09
# 2478      7 263.66 -512.38  1.30   0.07
# 2378      7 263.36 -511.78  1.91   0.05
# 2578      7 263.31 -511.69  2.00   0.05
# 2678      7 263.27 -511.61  2.08   0.05
# 12678     8 264.21 -511.21  2.47   0.04
# 12578     8 264.13 -511.06  2.63   0.04
# 12378     8 264.12 -511.03  2.66   0.04
# 124578    9 265.01 -510.49  3.19   0.03
# 124678    9 264.98 -510.44  3.24   0.03
# 123478    9 264.98 -510.42  3.26   0.03
# 23478     8 263.80 -510.38  3.30   0.03
# 24578     8 263.79 -510.36  3.32   0.03
# 24678     8 263.67 -510.13  3.56   0.02
# 23678     8 263.66 -510.11  3.57   0.02
# 25678     8 263.45 -509.70  3.99   0.02
# 23578     8 263.43 -509.65  4.04   0.02
# 123678    9 264.37 -509.21  4.47   0.01
# 125678    9 264.30 -509.08  4.60   0.01
# 123578    9 264.15 -508.78  4.90   0.01
# 234678    9 263.92 -508.31  5.38   0.01
# 234578    9 263.88 -508.23  5.45   0.01
# 1245678  10 265.04 -508.19  5.49   0.01
# 1234578  10 265.01 -508.14  5.54   0.01
# 1234678  10 265.00 -508.13  5.56   0.01
# 245678    9 263.83 -508.13  5.56   0.01
# 235678    9 263.79 -508.06  5.63   0.01
# 1235678  10 264.44 -507.00  6.68   0.00
# 2345678  10 264.04 -506.20  7.49   0.00
# 12345678 11 265.05 -505.83  7.85   0.00
# 267       6 256.86 -501.02 12.67   0.00
# 2567      7 257.65 -500.36 13.32   0.00
# 2467      7 257.26 -499.58 14.10   0.00
# 2367      7 257.23 -499.52 14.16   0.00
# 1267      7 256.87 -498.80 14.89   0.00
# 24567     8 258.00 -498.79 14.89   0.00
# 23567     8 257.91 -498.62 15.07   0.00
# 23467     8 257.82 -498.42 15.26   0.00
# 12567     8 257.67 -498.13 15.56   0.00
# 12367     8 257.29 -497.37 16.32   0.00
# 12467     8 257.27 -497.32 16.36   0.00
# 234567    9 258.42 -497.32 16.36   0.00
# 124567    9 258.02 -496.52 17.17   0.00
# 128       5 253.50 -496.51 17.18   0.00
# 123567    9 257.98 -496.44 17.24   0.00
# 123467    9 257.90 -496.27 17.42   0.00
# 1268      6 254.47 -496.25 17.44   0.00
# 1238      6 254.44 -496.18 17.50   0.00
# 28        4 252.00 -495.67 18.02   0.00
# 1234567  10 258.51 -495.14 18.54   0.00
# 12368     7 254.82 -494.72 18.97   0.00
# 78        5 252.57 -494.65 19.03   0.00
# 12568     7 254.74 -494.54 19.14   0.00
# 1258      6 253.57 -494.44 19.25   0.00
# 238       5 252.45 -494.40 19.29   0.00
# 1248      6 253.52 -494.35 19.34   0.00
# 12358     7 254.61 -494.28 19.40   0.00
# 12348     7 254.56 -494.19 19.49   0.00
# 258       5 252.30 -494.11 19.57   0.00
# 268       5 252.26 -494.04 19.65   0.00
# 12468     7 254.48 -494.02 19.67   0.00
# 248       5 252.09 -493.69 19.99   0.00
# 178       6 252.97 -493.24 20.45   0.00
# 2358      6 252.94 -493.19 20.50   0.00
# 578       6 252.91 -493.13 20.56   0.00
# 123568    8 255.13 -493.05 20.64   0.00
# 2568      6 252.83 -492.96 20.72   0.00
# 478       6 252.78 -492.86 20.83   0.00
# 678       6 252.69 -492.68 21.00   0.00
# 378       6 252.60 -492.50 21.19   0.00
# 123468    8 254.84 -492.47 21.21   0.00
# 2468      6 252.52 -492.35 21.33   0.00
# 2348      6 252.51 -492.32 21.37   0.00
# 2368      6 252.50 -492.30 21.39   0.00
# 124568    8 254.75 -492.29 21.39   0.00
# 123458    8 254.74 -492.28 21.41   0.00
# 12458     7 253.59 -492.24 21.44   0.00
# 2458      6 252.37 -492.05 21.64   0.00
# 1578      7 253.45 -491.96 21.73   0.00
# 1478      7 253.34 -491.75 21.93   0.00
# 4578      7 253.12 -491.30 22.38   0.00
# 1678      7 253.11 -491.29 22.39   0.00
# 23568     7 253.11 -491.28 22.40   0.00
# 24568     7 253.09 -491.25 22.43   0.00
# 3578      7 252.99 -491.06 22.63   0.00
# 23458     7 252.97 -491.00 22.68   0.00
# 1378      7 252.97 -491.00 22.69   0.00
# 5678      7 252.95 -490.98 22.71   0.00
# 67        5 250.66 -490.83 22.86   0.00
# 1234568   9 255.14 -490.75 22.93   0.00
# 4678      7 252.83 -490.73 22.96   0.00
# 3678      7 252.82 -490.71 22.98   0.00
# 3478      7 252.79 -490.65 23.03   0.00
# 14578     8 253.86 -490.52 23.17   0.00
# 23468     7 252.64 -490.34 23.34   0.00
# 127       6 251.49 -490.28 23.40   0.00
# 27        5 250.33 -490.16 23.53   0.00
# 237       6 251.36 -490.03 23.65   0.00
# 15678     8 253.50 -489.79 23.89   0.00
# 13578     8 253.46 -489.72 23.97   0.00
# 14678     8 253.40 -489.59 24.10   0.00
# 13478     8 253.35 -489.49 24.19   0.00
# 247       6 251.03 -489.36 24.33   0.00
# 1247      7 252.14 -489.35 24.34   0.00
# 234568    8 253.25 -489.28 24.40   0.00
# 34578     8 253.19 -489.16 24.52   0.00
# 13678     8 253.16 -489.10 24.58   0.00
# 35678     8 253.14 -489.08 24.61   0.00
# 45678     8 253.13 -489.04 24.64   0.00
# 367       6 250.82 -488.95 24.74   0.00
# 1237      7 251.90 -488.87 24.82   0.00
# 467       6 250.78 -488.86 24.82   0.00
# 1257      7 251.84 -488.75 24.94   0.00
# 2357      7 251.82 -488.70 24.98   0.00
# 567       6 250.66 -488.63 25.05   0.00
# 167       6 250.66 -488.63 25.06   0.00
# 2347      7 251.78 -488.62 25.07   0.00
# 34678     8 252.91 -488.61 25.07   0.00
# 257       6 250.56 -488.42 25.26   0.00
# 145678    9 253.87 -488.21 25.48   0.00
# 134578    9 253.87 -488.21 25.48   0.00
# 12457     8 252.46 -487.71 25.97   0.00
# 135678    9 253.57 -487.62 26.06   0.00
# 12357     8 252.39 -487.57 26.12   0.00
# 2457      7 251.23 -487.54 26.15   0.00
# 12347     8 252.37 -487.54 26.15   0.00
# 134678    9 253.40 -487.28 26.41   0.00
# 23457     8 252.17 -487.14 26.55   0.00
# 3467      7 251.00 -487.07 26.61   0.00
# 345678    9 253.25 -486.98 26.71   0.00
# 3567      7 250.84 -486.74 26.95   0.00
# 1367      7 250.82 -486.71 26.98   0.00
# 4567      7 250.79 -486.64 27.04   0.00
# 2456      6 249.66 -486.63 27.05   0.00
# 1467      7 250.78 -486.63 27.05   0.00
# 1567      7 250.67 -486.40 27.28   0.00
# 7         4 247.21 -486.09 27.60   0.00
# 123457    9 252.80 -486.08 27.61   0.00
# 246       5 248.24 -485.98 27.70   0.00
# 1345678  10 253.88 -485.87 27.81   0.00
# 12456     7 250.08 -485.22 28.47   0.00
# 37        5 247.82 -485.15 28.53   0.00
# 17        5 247.74 -484.99 28.70   0.00
# 1246      6 248.81 -484.92 28.76   0.00
# 34567     8 251.03 -484.86 28.83   0.00
# 13467     8 251.01 -484.80 28.88   0.00
# 47        5 247.53 -484.57 29.11   0.00
# 23456     7 249.71 -484.48 29.20   0.00
# 13567     8 250.84 -484.46 29.22   0.00
# 14567     8 250.79 -484.37 29.31   0.00
# 57        5 247.22 -483.96 29.73   0.00
# 2346      6 248.25 -483.80 29.88   0.00
# 147       6 248.02 -483.35 30.33   0.00
# 137       6 248.02 -483.35 30.34   0.00
# 347       6 247.99 -483.29 30.40   0.00
# 123456    8 250.16 -483.12 30.57   0.00
# 357       6 247.82 -482.95 30.73   0.00
# 157       6 247.75 -482.81 30.88   0.00
# 12346     7 248.85 -482.78 30.91   0.00
# 256       5 246.62 -482.74 30.95   0.00
# 134567    9 251.04 -482.55 31.14   0.00
# 457       6 247.56 -482.43 31.25   0.00
# 1256      6 247.42 -482.15 31.53   0.00
# 2356      6 247.16 -481.62 32.06   0.00
# 1347      7 248.20 -481.48 32.21   0.00
# 12356     7 248.08 -481.22 32.46   0.00
# 1457      7 248.05 -481.16 32.53   0.00
# 1357      7 248.02 -481.11 32.57   0.00
# 3457      7 248.00 -481.06 32.63   0.00
# 126       5 245.69 -480.89 32.80   0.00
# 26        4 244.57 -480.81 32.87   0.00
# 1236      6 246.27 -479.85 33.83   0.00
# 234       5 245.09 -479.69 34.00   0.00
# 236       5 245.02 -479.55 34.14   0.00
# 2345      6 246.05 -479.41 34.28   0.00
# 13457     8 248.21 -479.22 34.47   0.00
# 8         3 242.29 -478.39 35.29   0.00
# 235       5 244.39 -478.30 35.39   0.00
# 138       5 244.35 -478.21 35.48   0.00
# 24        4 243.16 -477.99 35.70   0.00
# 18        4 243.13 -477.94 35.74   0.00
# 38        4 243.04 -477.76 35.92   0.00
# 168       5 244.05 -477.61 36.07   0.00
# 1234      6 245.13 -477.56 36.12   0.00
# 23        4 242.90 -477.47 36.22   0.00
# 12345     7 246.06 -477.18 36.51   0.00
# 68        4 242.65 -476.97 36.71   0.00
# 245       5 243.64 -476.79 36.89   0.00
# 48        4 242.50 -476.67 37.01   0.00
# 1368      6 244.63 -476.56 37.13   0.00
# 158       5 243.50 -476.52 37.17   0.00
# 58        4 242.41 -476.50 37.19   0.00
# 1358      6 244.54 -476.39 37.30   0.00
# 1235      6 244.53 -476.37 37.32   0.00
# 46        4 242.33 -476.33 37.35   0.00
# 1348      6 244.36 -476.02 37.66   0.00
# 124       5 243.26 -476.02 37.66   0.00
# 123       5 243.21 -475.92 37.76   0.00
# [ reached getOption("max.print") -- omitted 56 rows ]
# 
# Term codes: 
#   AdjSyllableStress              LocSpeech             logRelFreq logWordFormFreqAllCoca 
# 1                      2                      3                      4 
# PrecSegDur   SemanticTransparency         TransitionType                Voicing 
# 5                      6                      7                      8 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                      0.6447352  0.0212400   0.0214030  30.124  < 2e-16 ***
#   LocSpeech                       -0.0048244  0.0010200   0.0010302   4.683 2.80e-06 ***
#   TransitionTypes#C               -0.0374215  0.0085232   0.0086069   4.348 1.37e-05 ***
# TransitionTypes#V               -0.0320300  0.0090056   0.0090870   3.525 0.000424 ***
# Voicingvoiceless                 0.0494254  0.0120987   0.0121952   4.053 5.06e-05 ***
#   AdjSyllableStressu              -0.0051466  0.0078132   0.0078517   0.655 0.512160    
# logWordFormFreqAllCoca           0.0008461  0.0016977   0.0017074   0.496 0.620187    
# logRelFreq                       0.0001382  0.0006624   0.0006679   0.207 0.836091    
# PrecSegDur                      -0.0170832  0.0888520   0.0896471   0.191 0.848870    
# SemanticTransparencytransparent  0.0010378  0.0049581   0.0049992   0.208 0.835546    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                      0.6447352  0.0212400   0.0214030  30.124  < 2e-16 ***
#   LocSpeech                       -0.0048248  0.0010193   0.0010295   4.687 2.80e-06 ***
#   TransitionTypes#C               -0.0374299  0.0085056   0.0085894   4.358 1.31e-05 ***
# TransitionTypes#V               -0.0320373  0.0089937   0.0090752   3.530 0.000415 ***
# Voicingvoiceless                 0.0494862  0.0119812   0.0120788   4.097 4.19e-05 ***
#   AdjSyllableStressu              -0.0110855  0.0081026   0.0081825   1.355 0.175489    
# logWordFormFreqAllCoca           0.0022664  0.0021216   0.0021422   1.058 0.290075    
# logRelFreq                       0.0005211  0.0012062   0.0012177   0.428 0.668682    
# PrecSegDur                      -0.0661235  0.1652725   0.1669260   0.396 0.692013    
# SemanticTransparencytransparent  0.0039259  0.0090363   0.0091216   0.430 0.666909    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                      LocSpeech TransitionType Voicing AdjSyllableStress logWordFormFreqAllCoca logRelFreq
# Importance:          1.00      1.00           1.00    0.46              0.37                   0.27      
# N containing models:  128       128            128     128               128                    128      
# SemanticTransparency PrecSegDur
# Importance:          0.26                 0.26      
# N containing models:  128                  128  

# yeah, we see the same 3 factors are the most important ones!

##################################################################################
#We can summarize:                                                                       #
#  - For items with primary stress on the base intial syllable, dis geminates
# - however, only 3 items which do nt have primary stress, when they have a double:
#   these items are dissolution. The are also opaque and might have undergone
#   resyllabification.
#  - Voicing has the expected effect
#  - SpeechRate has the expected effect                                                  #
##########################################################################################


# All in all 9 items were removed from the dataset

# I threw out 9 observations because their morph. status could not be
# identified (Disciver (card))


qqnorm (residuals (dis.lm6InterStressTT))
qqline (residuals (dis.lm6InterStressTT))



#1. TransitionType

table(dis_complex$TransitionType)

#s#s s#C s#V 
#22  44  59 

length(unique(dis_complex[dis_complex$TransitionType=="s#s","item.x"]))
#[1] 9 types

length(unique(dis_complex[dis_complex$TransitionType=="s#V","item.x"]))
#[1] 21 types

length(unique(dis_complex[dis_complex$TransitionType=="s#C","item.x"]))
#[1] 34 types


#2. SemanticTransparency

table(dis_complex$SemanticTransparency)

#     opaque transparent 
#       52          73 

# Number of types for SemanticTransparency

length(unique(dis_complex[dis_complex$SemanticTransparency=="transparent","item.x"]))
#[1] 34 transparent types

length(unique(dis_complex[dis_complex$SemanticTransparency=="opaque","item.x"]))
#[1] 28 opaque types

# 3. SemanticTransparency and TransitionType

table(dis_complex$SemanticTransparency,dis_complex$TransitionType)


#              s#s s#C s#V
# opaque        8  24  20
# transparent  14  20  39

# let's look at the distrubtion per type

dis_complexTypes<-dis_complex[!duplicated(dis_complex$item.x),]

table(dis_complexTypes$SemanticTransparency,dis_complexTypes$TransitionType)
#              s#s s#C s#V
# opaque        6  20   2
# transparent   3  13  18

length(unique(dis_complex[dis_complex$TransitionType=="s#s"&dis_complex$SemanticTransparency=="transparent" ,"item.x"]))
#[1] 3 types of transparent s#s

(unique(dis_complex[dis_complex$TransitionType=="s#s"&dis_complex$SemanticTransparency=="transparent" ,"item.x"]))
#[1] dissatisfied dissimilar   disservice  


length(unique(dis_complex[dis_complex$TransitionType=="s#V"& dis_complex$SemanticTransparency=="transparent","item.x"]))
#[1] 19 types of transparent s#V

(unique(dis_complex[dis_complex$TransitionType=="s#V"& dis_complex$SemanticTransparency=="transparent","item.x"]))
# [1]  disappears     disagree       disorders      disadvantages  disadvantage   disagreements 
# [7]  disappear      disarm         disintegrating disagreement   disorient      disability    
# [13] disappeared    dishonest      disables       disappearing   disabilities   disabled      
# [19] disadvantaged 

length(unique(dis_complex[dis_complex$TransitionType=="s#C"& dis_complex$SemanticTransparency=="transparent","item.x"]))
#[1] 14 types of transparent s#C

(unique(dis_complex[dis_complex$TransitionType=="s#C"& dis_complex$SemanticTransparency=="transparent","item.x"]))
# [1] discovered  disagree    discolor    discovers   disrepair   disregard   discharged 
# [8] dislike     discover    disgraceful disqualify  disappeared dislikes    distrust  


length(unique(dis_complex[dis_complex$TransitionType=="s#s"&dis_complex$SemanticTransparency=="opaque" ,"item.x"]))
#[1] 6 types of opaque s#s

(unique(dis_complex[dis_complex$TransitionType=="s#s"&dis_complex$SemanticTransparency=="opaque" ,"item.x"]))
#[1] dissolve     dissolution  dissenting   dissimilated dissolved    dissolving  


length(unique(dis_complex[dis_complex$TransitionType=="s#V"& dis_complex$SemanticTransparency=="opaque","item.x"]))
#[1] 2 types of opaque s#V

(unique(dis_complex[dis_complex$TransitionType=="s#V"& dis_complex$SemanticTransparency=="opaque","item.x"]))
#[1] disease  diseases


length(unique(dis_complex[dis_complex$TransitionType=="s#C"& dis_complex$SemanticTransparency=="opaque","item.x"]))
#[1] 20 types of opaque s#C

(unique(dis_complex[dis_complex$TransitionType=="s#C"& dis_complex$SemanticTransparency=="opaque","item.x"]))
# [1] disposable    dispelled     distracting   disruptive    distribute    dispose      
# [7] distiller     disposables   distractions  distributors  disposal      discoveries  
# [13] distributing  discount      discouraging  disposals     disposer      distributed  
# [19] dismemberment discouraged

#4. bc

summary (dis_complex$bc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5106  0.5695  0.6014  0.5988  0.6228  0.7035 
 


sd (dis_complex$bc)
#[1] 0.03804987


lambda
#[1] 0.2222222


#5. LocSpeech

summary (dis_complex$LocSpeech)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.40   10.81   12.93   13.11   15.66   22.44 


sd (dis_complex$LocSpeech)
#[1] 3.144058


summary (dis_complex$Voicing)
# voiced voiceless 
# 24       104 

# Now let's calculate the estimated values in our final model

final.lm<-dis.lm6InterStressTT

summary(final.lm)


# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + Voicing + 
#        LocSpeech, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.6444137  0.0147602  43.659  < 2e-16 ***
#   TransitionTypes#C                    -0.0449500  0.0086844  -5.176 9.23e-07 ***
#   TransitionTypes#V                    -0.0256645  0.0102393  -2.506   0.0135 *  
#   AdjSyllableStressu                   -0.0357422  0.0193093  -1.851   0.0666 .  
#   Voicingvoiceless                      0.0571720  0.0106532   5.367 3.97e-07 ***
#   LocSpeech                            -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0478538  0.0231416   2.068   0.0408 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0168113  0.0214858   0.782   0.4355    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03107 on 120 degrees of freedom
# Multiple R-squared:  0.3699,	Adjusted R-squared:  0.3331 
# F-statistic: 10.06 on 7 and 120 DF,  p-value: 7.439e-10

# Find out at which levels visreg draws lines

intercept=0.6444137
medianSpeech=median(dis_complex$LocSpeech)
estSpeech=-0.0045921
estsC=-0.0449500
estsV=-0.0256645
estStress=-0.0357422
estVoice= 0.0571720
estIntsC=0.0478538
estIntsV=0.0168113 


visreg(dis.lm6InterStressTT, "AdjSyllableStress", by="TransitionType", trans=function(x) x^(1/lambda)*1000 )
visreg(dis.lm6InterStressTT ,"TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )
#visreg(dis.lm6InterStressTTNoVoice ,"TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000 )

visreg(dis.lm6InterStressTT,"LocSpeech", trans=function(x) x^(1/lambda)*1000 )
visreg(dis.lm6InterStressTT ,"Voicing", trans=function(x) x^(1/lambda)*1000 )

#level stressed,voiceless s#s
((intercept+(medianSpeech*estSpeech)+(estVoice))^(1/lambda))*1000
#[1] 136.3857

EstimatedValueDoubleStress= 136.3857

#level stressed,voiceless s#V
((intercept+(medianSpeech*estSpeech)+(estsV)+(estVoice))^(1/lambda))*1000
#[1] 113.5205

EstimatedValueSingleVStress= 113.5205


#level stressed,voiceless s#C
((intercept+(medianSpeech*estSpeech)+(estsC)+(estVoice))^(1/lambda))*1000
#[1] 98.39527

EstimatedValueSingleCStress=98.39527


#level unstressed,voiceless s#s
((intercept+(medianSpeech*estSpeech)+(estVoice)+ estStress)^(1/lambda))*1000
#[1] 105.4071

EstimatedValueDoubleUnstressed= 105.4071

#level unstressed,voiceless s#V
((intercept+(medianSpeech*estSpeech)+(estsV)+(estVoice)+estStress+ estIntsV)^(1/lambda))*1000
#[1] 98.65839

EstimatedValueSingleVUnstressed= 98.65839


#level stressed,voiceless s#V
((intercept+(medianSpeech*estSpeech)+(estsC)+estStress+estIntsCTrans+(estVoice))^(1/lambda))*1000
#[1] 107.6971

EstimatedValueSingleCUnstressed=107.6971

#level stressed,voiced s#s
((intercept+(medianSpeech*estSpeech))^(1/lambda))*1000
#[1] 89.6545

EstimatedValueDoubleStressVoiced= 89.6545

#level stressed,voiced s#C
((intercept+(medianSpeech*estSpeech)+(estsC))^(1/lambda))*1000
#[1] 62.56831

EstimatedValueSingleCStressVoiced=62.56831


################################################################
#Unterschiede ausrechnen:
#################################################################

#####################################################
# With regard to gemination
##################################################

EstimatedValueDoubleStress-EstimatedValueSingleCStress
#[1] 37.99043
# Stressed items:  ss is 37.99043 sec longer than s#C


EstimatedValueDoubleStress-EstimatedValueSingleVStress
#[1] 22.8652

# Stressed items:  ss is 22.8652 sec longer than s#V

EstimatedValueSingleVStress-EstimatedValueSingleCStress
#[1] 15.12523


# BUT here the voied sounds might have had a huge impact



EstimatedValueDoubleUnstressed-EstimatedValueSingleCUnstressed
#[1] -2.29

# Unstressed items:  ss is 2.29 sec shorter than s#C --> no difference!


EstimatedValueDoubleUnstressed-EstimatedValueSingleVUnstressed
#[1] 6.74871

# Unstressed items:  ss is 6.74871sec longer than s#V --> no difference

#####################################################
# With regard to voicing
#####################

# doubeles
EstimatedValueDoubleStress-EstimatedValueDoubleStressVoiced
#[1] 46.7312

# Stressed items:  voiced items are 47 ms shorter

#s#C
EstimatedValueSingleCStress-EstimatedValueSingleCStressVoiced
#[1] 35.82696

# Stressed items:  voiced items are 36 ms shorter


#Also, we should conduct an ANOVA for the paper

anova(final.lm)


# Analysis of Variance Table
# 
# Response: bc
#                                     Df   Sum Sq   Mean Sq F value    Pr(>F)    
#   TransitionType                     2 0.026387 0.0131933 13.6644 4.504e-06 ***
#   AdjSyllableStress                  1 0.001306 0.0013064  1.3530 0.2470574    
#   Voicing                            1 0.012247 0.0122472 12.6845 0.0005297 ***
#   LocSpeech                          1 0.022960 0.0229605 23.7804 3.347e-06 ***
#   TransitionType:AdjSyllableStress   2 0.005107 0.0025533  2.6444 0.0751831 .  
#   Residuals                        120 0.115863 0.0009655                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Let's do some plots

levels(dis_complex$TransitionType)
#[1] "s#s" "s#C" "s#V"

levels(dis_complex$TransitionType)<-c("s#sV", "s#C" ,"s#V")

levels(dis_complex$TransitionType)
#[1] "s#sV" "s#C"  "s#V" 

png("boxDis.png", units="cm", height=10, width=7, res=300, pointsize=8)
bwplot (AbsDurMS ~ TransitionType, dis_complex, ylab="duration in milliseconds", main="dis-", ylim=c(0,180), cex.axis=0.5)

dev.off()





# we need to relabel the stress level

levels(dis_complex$AdjSyllableStress)
#[1] "p" "u"

levels(dis_complex$AdjSyllableStress)<-c("stressed", "unstressed")

levels(dis_complex$AdjSyllableStress)
#[1] "stressed"   "unstressed"





# redo the final model

final.lm<-lm(bc~TransitionType*AdjSyllableStress+LocSpeech+Voicing, data = dis_complex)

summary(final.lm)


png("dis- modelcov.png", units="cm", height=8, width=14, res=300, pointsize=8)
par(mfrow=c(1,2))


visreg(final.lm,"LocSpeech", trans=function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="local speech rate",ylim=c(20,160), cex.axis=1)
visreg(final.lm,"Voicing", trans=function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="voicing",ylim=c(20,160), cex.axis=1)

dev.off()

par(mfrow=c(1,1))

png("dis- model TransitionType by Stress.png", units="cm", height=15, width=20, res=300, pointsize=16)

par <- trellis.par.get()
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

visreg(final.lm, "TransitionType", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000,rug=T, ylab="duration in milliseconds", xlab=" ",  ylim=c(20,160), cex.axis=0.9,par.settings=par)

dev.off()






png("dis- model stress by TransType.png", units="cm", height=15, width=20, res=300, pointsize = 16)


visreg(final.lm, "AdjSyllableStress", by ="TransitionType", trans=function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="stress pattern by environment",ylim=c(20,160), cex.axis=1)

dev.off()


# let's quickly check whether the difference between s#C and s#V is significant in the final
# model with voiced items

dis_complex$TransitionType <- relevel (dis_complex$TransitionType, ref= "s#C"   )


final2.lm<-lm(bc~TransitionType*AdjSyllableStress+LocSpeech+Voicing, data = dis_complex)


summary(final2.lm)
# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + LocSpeech + 
#        Voicing, data = dis_complex)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063687 -0.018477  0.001405  0.022595  0.076175 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                     0.5994637  0.0156375  38.335  < 2e-16 ***
#   TransitionTypes#sV                              0.0449500  0.0086844   5.176 9.23e-07 ***
#   TransitionTypes#V                               0.0192855  0.0103976   1.855   0.0661 .  
#   AdjSyllableStressunstressed                     0.0121116  0.0129309   0.937   0.3508    
#   LocSpeech                                      -0.0045921  0.0009994  -4.595 1.08e-05 ***
#   Voicingvoiceless                                0.0571720  0.0106532   5.367 3.97e-07 ***
#   TransitionTypes#sV:AdjSyllableStressunstressed -0.0478538  0.0231416  -2.068   0.0408 *  
#   TransitionTypes#V:AdjSyllableStressunstressed  -0.0310425  0.0168448  -1.843   0.0678 .  

# so it is marginally sign.

##################################################################################################
#


###########################################


# let's also include the model without the voiced ones


summary(dis.lm6InterStressTTNoVoice)
# Call:
#   lm(formula = bc ~ TransitionType * AdjSyllableStress + LocSpeech, 
#      data = dis_complex_no_voice)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.063771 -0.017149 -0.000013  0.021723  0.075797 
# 
# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                           0.7080344  0.0153395  46.157  < 2e-16 ***
#   TransitionTypes#C                    -0.0504575  0.0086037  -5.865 6.23e-08 ***
#   TransitionTypes#V                    -0.0417055  0.0121433  -3.434 0.000875 ***
#   AdjSyllableStressu                   -0.0413639  0.0184716  -2.239 0.027419 *  
#   LocSpeech                            -0.0046585  0.0009936  -4.688 9.00e-06 ***
#   TransitionTypes#C:AdjSyllableStressu  0.0533449  0.0220658   2.418 0.017494 *  
#   TransitionTypes#V:AdjSyllableStressu  0.0329438  0.0216409   1.522 0.131188    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02943 on 97 degrees of freedom
# Multiple R-squared:  0.4025,	Adjusted R-squared:  0.3655 
# F-statistic: 10.89 on 6 and 97 DF,  p-value: 3.065e-09

# I would like to check one last thing: Is there an interaction between stress and SemTrans
# when there are only voiceless items

dis.lm6InterStressSTNoVoice<-(lm(bc ~ TransitionType +SemanticTransparency* AdjSyllableStress + LocSpeech,  data = dis_complex_no_voice))
summary(dis.lm6InterStressSTNoVoice)


# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency * AdjSyllableStress + 
#        LocSpeech, data = dis_complex_no_voice)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.068219 -0.019061  0.000338  0.018396  0.069974 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                         0.7163195  0.0170846  41.928  < 2e-16 ***
#   TransitionTypes#C                                  -0.0502745  0.0089659  -5.607 1.93e-07 ***
#   TransitionTypes#V                                  -0.0519090  0.0105549  -4.918 3.57e-06 ***
#   SemanticTransparencytransparent                    -0.0044293  0.0084656  -0.523  0.60201    
#   AdjSyllableStressu                                 -0.0464589  0.0169071  -2.748  0.00715 ** 
#   LocSpeech                                          -0.0049699  0.0009767  -5.088 1.77e-06 ***
#   SemanticTransparencytransparent:AdjSyllableStressu  0.0518311  0.0195897   2.646  0.00951 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02921 on 97 degrees of freedom
# Multiple R-squared:  0.4117,	Adjusted R-squared:  0.3753 
# F-statistic: 11.31 on 6 and 97 DF,  p-value: 1.506e-09

visreg(dis.lm6InterStressSTNoVoice, "SemanticTransparency", by="AdjSyllableStress")
visreg(dis.lm6InterStressSTNoVoice, "AdjSyllableStress", by="SemanticTransparency")
visreg(dis.lm6InterStressSTNoVoice, "TransitionType")

# so the opaque ones with a following unstressed syllable are shorter than all the others

# which items are those (only 4)
#[1] dissolution dissolution discount    dissolution

dis_complex_no_voice[dis_complex_no_voice$SemanticTransparency=="opaque" & dis_complex_no_voice$AdjSyllableStress=="u","item.x"]
#[1] dissolution dissolution discount    dissolution


# so again, our ss items which are short

# what if we put both interactions in the model

dis.lm6InterStressSTTTNoVoice<-(lm(bc ~ TransitionType*SemanticTransparency +SemanticTransparency* AdjSyllableStress + LocSpeech,  data = dis_complex_no_voice))
summary(dis.lm6InterStressSTTTNoVoice)
# not possible

# SO, our final model is:

finalNoVoice.lm<-dis.lm6InterStressSTNoVoice

summary(finalNoVoice.lm)

# Call:
#   lm(formula = bc ~ TransitionType + SemanticTransparency * AdjSyllableStress + 
#        LocSpeech, data = dis_complex_no_voice)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.068219 -0.019061  0.000338  0.018396  0.069974 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                         0.7163195  0.0170846  41.928  < 2e-16 ***
#   TransitionTypes#C                                  -0.0502745  0.0089659  -5.607 1.93e-07 ***
#   TransitionTypes#V                                  -0.0519090  0.0105549  -4.918 3.57e-06 ***
#   SemanticTransparencytransparent                    -0.0044293  0.0084656  -0.523  0.60201    
#   AdjSyllableStressu                                 -0.0464589  0.0169071  -2.748  0.00715 ** 
#   LocSpeech                                          -0.0049699  0.0009767  -5.088 1.77e-06 ***
#   SemanticTransparencytransparent:AdjSyllableStressu  0.0518311  0.0195897   2.646  0.00951 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02921 on 97 degrees of freedom
# Multiple R-squared:  0.4117,	Adjusted R-squared:  0.3753 
# F-statistic: 11.31 on 6 and 97 DF,  p-value: 1.506e-09

# Find out at which levels visreg draws lines

intercept=0.7163195
medianSpeech=median(dis_complex$LocSpeech)
estSpeech=-0.0049699
estsC=-0.0502745
estsV=-0.0519090
estSemT=-0.0044293
estStress=-0.0464589
estInt=0.0518311



#level unstressed,s#s opaque
((intercept+(medianSpeech*estSpeech)+estStress)^(1/lambda))*1000
#[1] 104.7348

EstimatedValueDoubleOpaqueUnstessed= 104.7348


#level unstressed,s#s opaque
((intercept+(medianSpeech*estSpeech)+estStress+estInt+estSemT)^(1/lambda))*1000
#[1] 147.0114

EstimatedValueDoubleTransUnstessed= 147.0114


####################################

#level double
((intercept+(medianSpeech*estSpeech)+estSemT)^(1/lambda))*1000
#[1] 141.6473

#level single c
((intercept+(medianSpeech*estSpeech)+estSemT+estsC)^(1/lambda))*1000
#[1] 98.4702

#level single V
((intercept+(medianSpeech*estSpeech)+estSemT+estsV)^(1/lambda))*1000
#[1] 97.26369




################################################################
#Unterschiede ausrechnen:
#################################################################

EstimatedValueDoubleTransUnstessed -EstimatedValueDoubleOpaqueUnstessed
#[1] 42.2766

# Opaques are [1] 42.2766 shorter when before and unstressed syllable

# now let's do the plots 

levels(dis_complex_no_voice$TransitionType)
#[1] "s#s" "s#C" "s#V"

levels(dis_complex_no_voice$TransitionType)<-c("s#sV", "s#C" ,"s#V")

levels(dis_complex_no_voice$TransitionType)
#[1] "s#sV" "s#C"  "s#V" 


# we need to rename stress

levels(dis_complex_no_voice$AdjSyllableStress)
#[1] "p" "u"


levels(dis_complex_no_voice$AdjSyllableStress)<-c( "stressed" ,"unstressed")

levels(dis_complex_no_voice$AdjSyllableStress)
#[1] "stressed"   "unstressed"


# redo the final model

finalNoVoice.lm<-lm(bc~TransitionType+SemanticTransparency*AdjSyllableStress+LocSpeech, data = dis_complex_no_voice)

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Corpus")


png("dis- model without voiced sounds SemanticTransType by Stress.png", units="cm", height=15, width=20, res=300, pointsize=15)
par <- trellis.par.get()
par$fontsize <- list(text=15) # das ist glaub ich logisch :)
par$strip.background$col <- "lightgrey" # das macht das pinke/orangefarbene weg 
par$panel.background$col <- "white" # das macht das weiß im plot weg

visreg(finalNoVoice.lm, "SemanticTransparency", by="AdjSyllableStress", trans=function(x) x^(1/lambda)*1000,rug=T, ylab="duration in milliseconds", xlab="",  cond=list(TransitionType="s#sV"), ylim=c(20,180),ylab=" ",cex.axis=0.9,par.settings=par)

dev.off()









png("dis- model without voiced sounds environment.png", units="cm", height=15, width=15, res=300, pointsize=15)

visreg(finalNoVoice.lm, "TransitionType", trans=function(x) x^(1/0.2222222)*1000,rug=F, ylab="duration in milliseconds", xlab="environment",  ylim=c(20,160), cex.axis=0.9)

dev.off()
# SemanticTransparency: transparent
# AdjSyllableStress: p
# LocSpeech: 13.77522

# unterschied ausrehnen
#doubles
((intercept+ (-0.0049699 *13.77522))^(1/0.2222222))*1000
#[1] 141.7945

#single C
((intercept+ (-0.0049699 *13.77522)-0.0502745 )^(1/0.2222222))*1000
#[1] 98.58111

#single C
((intercept+ (-0.0049699 *13.77522)-0.0519090 )^(1/0.2222222))*1000
#[1] 97.37354
