# We need to chcke whether the same results (regarding the gemination
# in English affixation) are yielded when one uses RelativeDuration (vowel to Nasal)
# instead of absolute duraton


unComplex$RelDur<-unComplex$AbsDurCon/unComplex$PrecSegDur



#Loading libraries

#library(ascii)
library(xtable)
library(languageR)
library(lme4)
library (MASS)
library(lattice)
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


# set the directory, so R knows where to find a file


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/duration")


# loading the data, naming it d, telling R that t is the seperator, and that na, NA and   are NA strings

d <- read.table("GemDataR_2015_09_10.csv", sep="\t",header = T,  na.string=c("na", "", "NA"))



str(d)
# 'data.frame':	906 obs. of  42 variables:
#   $ ItemID              : int  20 33 49 51 64 68 71 86 107 124 ...
# $ item                : Factor w/ 590 levels "actively","actually",..: 171 107 158 144 160 178 169 168 174 148 ...
# $ PartofSpeech        : Factor w/ 10 levels "a","adv","c",..: 5 5 10 10 10 5 10 10 1 10 ...
# $ Base                : Factor w/ 447 levels "ability","able",..: 337 272 322 308 324 345 335 334 340 315 ...
# $ PartofSpeechBase    : Factor w/ 6 levels "a","adv","n",..: 3 3 6 6 6 3 6 6 1 6 ...
# $ WordFormFreq        : int  2282 7396 961 260 435 267 1358 278 1097 447 ...
# $ BaseFormFreq        : int  0 373 804 4 493 0 736 16457 0 1478 ...
# $ WordLemmaFreq       : int  2676 7523 2547 1040 2547 271 1451 1451 1096 577 ...
# $ BaseLemmaFreq       : int  0 394 2506 54 2506 0 3014 3014 0 9 ...
# $ Speaker             : int  79 32 70 57 218 79 220 224 10 102 ...
# $ Pos                 : Factor w/ 3 levels "E","M","P": 2 2 2 2 2 2 2 2 2 2 ...
# $ Affix               : Factor w/ 6 levels "inLoc","inNeg",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ AffixOrth           : Factor w/ 5 levels "im","in","lely",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ MorphBound          : Factor w/ 3 levels "n","o","y": 2 2 2 2 2 3 2 2 2 2 ...
# $ Consonant           : Factor w/ 3 levels "l","m","n": 2 2 2 2 2 2 2 2 2 2 ...
# $ NoCons              : int  1 1 1 1 1 1 1 1 1 1 ...
# $ AbsDurCon           : num  0.0548 0.0718 0.0943 0.0754 0.0666 ...
# $ WordDur             : num  0.41 0.537 0.907 0.4 0.669 ...
# $ PrecSeg             : Factor w/ 36 levels "@","@U","{","a",..: 18 18 18 1 18 18 18 18 18 18 ...
# $ PrecSegVC           : Factor w/ 5 levels "C","v","V","VV",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ PrecSegDur          : num  0.0273 0.1129 0.0508 0.0394 0.0543 ...
# $ FollSeg             : Factor w/ 42 levels "@","@U","{","3",..: 31 31 31 31 31 31 31 31 31 31 ...
# $ FollSegVC           : Factor w/ 4 levels "C","pause","v",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ FollSegDur          : num  0.0939 0.0572 0.1008 0.0758 0.1809 ...
# $ LocSpeech           : num  17.07 9.31 7.72 17.5 11.97 ...
# $ PreSuf              : Factor w/ 4 levels "finalNoMorph",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ SyllPhon            : int  3 2 2 2 3 4 2 2 3 2 ...
# $ SyllAct             : int  3 2 2 2 3 4 2 2 3 2 ...
# $ NoSegWord           : int  7 5 7 7 8 10 6 6 8 5 ...
# $ DeletionMorph       : Factor w/ 5 levels "C","CV","M","N",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ DeviantPronun       : Factor w/ 3 levels "C","N","Y": 2 2 2 2 2 2 2 2 2 2 ...
# $ PauseMorph          : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFormFreqAllCoca : int  11 2786 6019 121 2270 1 12162 66079 4 12676 ...
# $ WordFormFreqAllCoca : int  10916 44362 7420 4951 3756 1577 8330 2455 9918 4038 ...
# $ AffixStressLongman  : Factor w/ 4 levels "d","p","s","u": 4 2 4 4 4 4 4 4 4 4 ...
# $ AdjSyllStressLongman: Factor w/ 3 levels "p","s","u": 1 3 1 1 1 1 1 1 1 1 ...
# $ AffixStress         : Factor w/ 4 levels "d","p","s","u": 1 2 1 1 1 1 1 1 1 1 ...
# $ AdjSyllStress       : Factor w/ 3 levels "p","s","u": 1 3 1 1 1 1 1 1 1 1 ...
# $ WordLemmaFreqAllCoca: int  13483 49070 0 0 0 1613 0 10669 9918 4472 ...
# $ BaseLemmaFreqAllCoca: int  11 3125 0 0 0 1 0 27728 4 102 ...
# $ al                  : Factor w/ 2 levels "m","y": NA NA NA NA NA NA NA NA NA NA ...
# $ SearchName          : Factor w/ 11 levels "imbp","imm","inC",..: 1 1 1 1 1 1 1 1 1 1 ...

dim(d)
#[1] 906  42

head(d)
#  ItemID         item PartofSpeech       Base PartofSpeechBase WordFormFreq BaseFormFreq
#  1     20   impression            n   pression                n         2282            0
#  2     33       impact            n       pact                n         7396          373
#  3     49      imposed            v      posed                v          961          804
#  4     51      implies            v      plies                v          260            4
#  5     64     imposing            v     posing                v          435          493
#  6     68 imprisonment            n prisonment                n          267            0
#  WordLemmaFreq BaseLemmaFreq Speaker Pos Affix AffixOrth MorphBound Consonant NoCons
#  1          2676             0      79   M inLoc        im          o         m      1
#  2          7523           394      32   M inLoc        im          o         m      1
#  3          2547          2506      70   M inLoc        im          o         m      1
#  4          1040            54      57   M inLoc        im          o         m      1
#  5          2547          2506     218   M inLoc        im          o         m      1
#  6           271             0      79   M inLoc        im          y         m      1
#  AbsDurCon   WordDur PrecSeg PrecSegVC PrecSegDur FollSeg FollSegVC FollSegDur LocSpeech
#  1 0.05482255 0.4100000       I         V 0.02731973       p         C 0.09386882 17.073171
#  2 0.07180221 0.5371603       I         V 0.11288529       p         C 0.05724486  9.308209
#  3 0.09429401 0.9068865       I         V 0.05084481       p         C 0.10076517  7.718717
#  4 0.07541828 0.4000000       @         V 0.03935137       p         C 0.07580307 17.500000
#  5 0.06661941 0.6685819       I         V 0.05426809       p         C 0.18092417 11.965624
#  6 0.03722816 0.4500000       I         V 0.04037432       p         C 0.08138248 22.222222
#  PreSuf SyllPhon SyllAct NoSegWord DeletionMorph DeviantPronun PauseMorph BaseFormFreqAllCoca
#  1 prefix        3       3         7             N             N          N                  11
#  2 prefix        2       2         5             N             N          N                2786
#  3 prefix        2       2         7             N             N          N                6019
#  4 prefix        2       2         7             N             N          N                 121
#  5 prefix        3       3         8             N             N          N                2270
#  6 prefix        4       4        10             N             N          N                   1
#  WordFormFreqAllCoca AffixStressLongman AdjSyllStressLongman AffixStress AdjSyllStress
#  1               10916                  u                    p           d             p
#  2               44362                  p                    u           p             u
#  3                7420                  u                    p           d             p
#  4                4951                  u                    p           d             p
#  5                3756                  u                    p           d             p
#  6                1577                  u                    p           d             p
#  WordLemmaFreqAllCoca BaseLemmaFreqAllCoca   al SearchName
#  1                13483                   11 <NA>       imbp
#  2                49070                 3125 <NA>       imbp
#  3                    0                    0 <NA>       imbp
#  4                    0                    0 <NA>       imbp
#  5                    0                    0 <NA>       imbp
#  6                 1613                    1 <NA>       imbp

table(d$Consonant)

# l   m   n 
# 203 199 504 


pairscor.fnc(d[ , c(6:9,12,14:15, 17,18)])



# Transforming freq. in log frequencies (adding of 1 to elimate 0 frequencies):

d$WordFormFreq <- d$WordFormFreq+1
d$BaseFormFreq <- d$BaseFormFreq+1
d$WordLemmaFreq <- d$WordLemmaFreq+1
d$BaseLemmaFreq <- d$BaseLemmaFreq+1
d$WordFormFreqAllCoca <- d$WordFormFreqAllCoca+1
d$BaseFormFreqAllCoca <- d$BaseFormFreqAllCoca +1
d$WordLemmaFreqAllCoca <- d$WordLemmaFreqAllCoca+1
d$BaseLemmaFreqAllCoca <- d$BaseLemmaFreqAllCoca+1


d$logWordFormFreq <- log(d$WordFormFreq)
d$logBaseFormFreq <- log(d$BaseFormFreq)
d$logWordLemmaFreq <- log(d$WordLemmaFreq)
d$logBaseLemmaFreq <- log(d$BaseLemmaFreq)
d$logWordFormFreqAllCoca <- log(d$WordFormFreqAllCoca)
d$logBaseFormFreqAllCoca <- log(d$BaseFormFreqAllCoca)
d$logWordLemmaFreqAllCoca <- log(d$WordLemmaFreqAllCoca)
d$logBaseLemmaFreqAllCoca <- log(d$BaseLemmaFreqAllCoca)


levels(d$MorphBound)
# [1] "n" "o" "y"

#renaming these levels

levels(d$MorphBound) = c("no", "opaque", "transparent")

levels(d$MorphBound) 
#[1] "no"     "opaque" "transparent"   


names(d)

# [1] "ItemID"                  "item"                    "PartofSpeech"           
# [4] "Base"                    "PartofSpeechBase"        "WordFormFreq"           
# [7] "BaseFormFreq"            "WordLemmaFreq"           "BaseLemmaFreq"          
# [10] "Speaker"                 "Pos"                     "Affix"                  
# [13] "AffixOrth"               "MorphBound"              "Consonant"              
# [16] "NoCons"                  "AbsDurCon"               "WordDur"                
# [19] "PrecSeg"                 "PrecSegVC"               "PrecSegDur"             
# [22] "FollSeg"                 "FollSegVC"               "FollSegDur"             
# [25] "LocSpeech"               "PreSuf"                  "SyllPhon"               
# [28] "SyllAct"                 "NoSegWord"               "DeletionMorph"          
# [31] "DeviantPronun"           "PauseMorph"              "BaseFormFreqAllCoca"    
# [34] "WordFormFreqAllCoca"     "AffixStressLongman"      "AdjSyllStressLongman"   
# [37] "AffixStress"             "AdjSyllStress"           "WordLemmaFreqAllCoca"   
# [40] "BaseLemmaFreqAllCoca"    "al"                      "SearchName"             
# [43] "logWordFormFreq"         "logBaseFormFreq"         "logWordLemmaFreq"       
# [46] "logBaseLemmaFreq"        "logWordFormFreqAllCoca"  "logBaseFormFreqAllCoca" 
# [49] "logWordLemmaFreqAllCoca" "logBaseLemmaFreqAllCoca"

class(d$Pos)
#[1] "factor"

levels(d$Pos)
#[1] "E" "M" "P"

# Umbenennen der Level in Position
levels(d$Pos) <- c("end", "mid", "pause")

levels(d$Pos)
#[1] "end"   "mid"   "pause"

class(d$NoCons)
#[1] "integer"

#changing class of variables NoCons
d$NoCons <- as.factor(d$NoCons)

class(d$NoCons)
# [1] "factor"

levels(d$NoCons)
# [1] "1" "2"

#renaming levels of NoCons
levels(d$NoCons) = c("single", "double")

levels (d$NoCons)
# [1] "single" "double"

class(d$FollSegVC)
#[1] "factor"

# v and V in FollSeg zusammenführen --> man benennt die zwei Variablen, die eine werden sollen, gleich : So werde
# sie zusammengeführt

levels(d$FollSegVC)
#[1] "C"     "pause" "v"     "V"    
levels(d$FollSegVC) <- c("C", "pause", "V", "V")

levels(d$FollSegVC)
#[1] "C"     "pause" "V" 

# Now all the variables are the type of variable they are supposed to be!

# Remove items with missing frequencies: (frequencies are missing because of clitics)

d <-d [!is.na(d$WordFormFreq), ]


# we have coded accentedness for some of the un- and im-items. Let's merge
# files before we go on creating subsets

accent<- read.delim("UnInComplexAccent.csv", sep="\t",header = T,  na.string=c("na", "", "NA"))

# merge the two files

d2<-merge( accent,d, by.x="ItemID", by.y="ItemID")

# d2 is now the file in which all complex ins and uns are coded for accented and everythings 
# else


# let's see whether we have coded similarly (even though we know
# already we have not..)

table(d2$AccentIngo, d2$PhrasalAccentSonia)

#          no unclear yes
# no       26      22  14
# unclear   0       5   8
# yes      10      51 180


# so the yeses seem pretty clear, and I was much more insecure than 
# Ingo

# Let's see my second strategy rating

table(d2$AccentIngo, d2$AnyAccentSonia)
#           no unclear yes
# no       16      11  35
# unclear   0       4   9
# yes      11      49 181


# looks pretty similar

# let' see whether there is a difference between in- and un

un <- d2[d2$AffixOrth== "un", ]
im <- d2[d2$AffixOrth=="im",]



table(un$AccentIngo, un$PhrasalAccentSonia)

#         no unclear yes
# no      11      10   5
# unclear  0       1   4
# yes      5      25  97


table(un$AccentIngo, un$AnyAccentSonia)

#          no unclear yes
# no       5       5  16
# unclear  0       2   3
# yes      5      23  99


table(im$AccentIngo, im$PhrasalAccentSonia)

#          no unclear yes
# no      15      12   9
# unclear  0       4   4
# yes      5      26  83


table(im$AccentIngo, im$AnyAccentSonia)

#          no unclear yes
# no      11       6  19
# unclear  0       2   6
# yes      6      26  82


# Es scheint nichts mit dem Affix zu tun haben, sondern wirklich
# mit der Kodierung,,,


# I am not convinced that this is really a valid coding but later on we 
# will look whether it has an influence. If it does not have an effect
# we must however consider, that the coding was not valid AND that according
# to our coding, most items are accented and hence the distribution of the data 
# does not allow for a valid testing of this effect

# 3 Datensätze erstellen, für jedes Morphem einen
In <- d[d$AffixOrth=="in" | d$AffixOrth=="im", ]
un <- d[d$AffixOrth== "un", ]
ly <- d[d$AffixOrth== "ly" | d$AffixOrth== "lely",]

# Den Datensatz erstellen, in dem alle komplexen un-items sind

un <- d2[d2$AffixOrth== "un", ]
unComplex <- un[un$MorphBound=="transparent", ]

unComplex$RelDur<-unComplex$AbsDurCon/unComplex$PrecSegDur

dim(unComplex)
# NOTE : now we have 158 --> one outlier was excluded in the scipt, so it
# was not coded for accentedness and is out already

# [1] 158  57


# We create the variable RelFreq by dividing the word lemma freq by the base lemma freq
unComplex$RelFreq <- unComplex$WordLemmaFreqAllCoca / unComplex$BaseLemmaFreqAllCoca
unComplex$logRelFreq <- log(unComplex$WordLemmaFreqAllCoca / unComplex$BaseLemmaFreqAllCoca)

densityplot(unComplex$RelDur)
plot(unComplex$RelDur)

#######################################################################
##### In the "original" un-dataset, there was one outlier:
# use identify() to find out the line numbers of selected points:
#outliers <- identify(unComplex$RelDur)
#outliers
#[1] 13

# mit dieser Funktion wird eine Linie in die Grpahik eingefügt: Hilfreich um Grenzen festzulegen
#abline(0.15,0)

# we remove outliers
#unComplex <- unComplex [-outliers, ]
unComplex1<- unComplex[-13, ]



dim(unComplex1)
# [1] 157  59


# Let's plot the distribution of the data

densityplot(unComplex1$RelDur)
plot(unComplex1$RelDur)


#we will create two new datasets, one for singles, one for doubles (maybe we need them later)

unComplex1Double<- unComplex1[unComplex1$NoCons=="double",]

unComplex1Single<- unComplex1[unComplex1$NoCons=="single",]

##############################################################################
#######   Back to the analysis of the un-complex-dataset   ###################
##############################################################################

# Problem is, that there aren't too many observations in unComplex. That 
# means that we have to be super-careful with regard to the number of 
# predictors, otherwise we end up with serious overfitting issues.

# We will now look at each preditor variable in order to decide which one to include
# Variables are in included if there is a theoretical reason to include them, it is
# statistically unproblematic to include them (levels are not too small).

# Covariates are included if an effect is observable. 
#Variables of iterest are all included.


#         1. Part of Speech

unComplex1$PartofSpeech<-droplevels(unComplex1$PartofSpeech)
plot(unComplex1$PartofSpeech, unComplex1$RelDur)
table (unComplex1$PartofSpeech)
#     a adv   n  pn   v 
#    139  12  11   1   4 

# There is no point in including this as a predictor in a model.
# (a) some of the levels are too 'small'
# (b) there is no good theoretical reason why this should matter in the
# first place.

#       2. Number of consonants (NoCons)

table(unComplex1$NoCons)
# single double 
#    135     22


# there has been a mistake when coding the data (doubles were identified based on
# orthographics- thus un+kn was not considered to be a double, we need to change this)

unComplex1$item<-droplevels(unComplex1$item)
levels(unComplex1$item)

# thus we have to recode "unknown"

unComplex1[unComplex1$item=="unknown",c( "NoCons")]

unComplex1[unComplex1$item=="unknown",c( "NoCons")]<-"double"

unComplex1[unComplex1$item=="unknown",c( "NoCons")]

table(unComplex1$NoCons)
#single double 
#134     23

plot (unComplex1$RelDur ~ unComplex1$NoCons)

# We definetly need to include this factor 
#(variable of interest and it shows a great difference 
#between the two levels)


#         3. Frequencies

# We have a bunch if different frequency variables. We have frequencues from the 
# spoken part of COCA and the whole COCA corpus (AllCoca)

# Form Frequencies for base and word:

#logWordFormFreq  logBaseFormFreq logWordFormFreqAllCoca  logBaseFormFreqAllCoca

# Lemma Frequencies for base and word:

# WordLemmaFreqAllCoca  BaseLemmaFreqAllCoca logWordLemmaFreq  logBaseLemmaFreq


# and relative frequencies

# We will now take a look at the correlations between the different variables to 
# to decide which ones to include

pairscor.fnc(unComplex1 [, c("logWordFormFreq", "logBaseFormFreq", "logWordLemmaFreq", "logBaseLemmaFreq", "logRelFreq","logWordFormFreqAllCoca", "logBaseFormFreqAllCoca", "logWordLemmaFreqAllCoca", "logBaseLemmaFreqAllCoca")])

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

plot (unComplex1$logRelFreq, unComplex1$RelDur)
plot (unComplex1$logWordFormFreqAllCoca, unComplex1$RelDur)
plot (unComplex1$logBaseFormFreqAllCoca, unComplex1$RelDur)

# None of these three plots indicate an extremely strong effect on 
# the absolute duration. This probably means that any decision that we
# make won't affect the model to a large degree...

# We decide to use two variables: logWordFormFreqAllCoca, because we assume that
# in a very frequent word form, the segments are shorter, and logRelFreqAllCoca, 
# because we assume that in a more decomposable word, the absolute duration
# should be longer (and a word with a low RelFreq should be more 
# decomposable --> also one of the variables we are interested in).


#     4. Position

table (unComplex1$Pos)
#   end   mid pause 
#    14   120    23 

plot (unComplex1$RelDur ~ unComplex1$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.



#     5. Word Duration

plot (density(unComplex1$WordDur))
plot (unComplex1$RelDur ~ unComplex1$WordDur)


#There seems to be a relation
# between the overall duration of a word (perhaps due to speech rate?, 
# or due to phrase-final lengthening?) and the duration of the consonants.
# The longer a word is, the longer are its segments pronounced.

#     6. Preceding Segment duration

plot (density(unComplex1$PrecSegDur))
plot(unComplex1$PrecSegDur)
plot (unComplex1$RelDur ~ unComplex1$PrecSegDur)

# Of Course there is a relation, since RelDur consists partly of PrecSegDur. 
# Hence we cannot use this factor

#     7. Following segment

unComplex1$FollSegVC<-droplevels(unComplex1$FollSegVC)
table (unComplex1$FollSegVC)
#C  V 
#67 90

# Problem: After double Ns, only vowels may occur:

table (unComplex1$FollSegVC, unComplex1$NoCons)
#     single double
#C     67      0
#V     67     23

# What we do is construct a new factor that combines the two:
unComplex1$TransitionType <- factor(paste (unComplex1$NoCons, unComplex1$FollSegVC, sep="-"))
levels(unComplex1$TransitionType)
# [1] "double-V" "single-C" "single-V"



# 8. Following segment duration

# We ignore FollSegDur, because that's sort of included in our new factor 
# and in WordDur anyways, and there is nothing the literature that suggests
# that this is an extremely important predictor on theoretical grounds.



#  9. Word Length

# Next, we have a bunch of variables that are all closely related to each
# other and are related to word length
#, which means that we certainly do not want to include all of them:
#
# LocSpeech, SyllPhon, SyllAct, NoSegWord

# I think Locspeech is a factor which we do not necessariliy include in the 
# model, since it is calclated by WordDur/NoSegWord. If we decide to include
# both of these variables + check their interactions we sort of
# included LocSpeech. Nevertheless, let's take a look at this factor


#    9.1. Local speech rate

plot(density(unComplex1$LocSpeech))

plot(unComplex1$RelDur~unComplex1$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables- However, this could be due to the fact, that the two variables of
# which LocSpeech is made of (WordDur and NoSeg) have different effect:
# longer word dur - longer consonant duration, more segmentents (in longer words)-
# shorter consonant duration

# we might include this factor (depending on how NoSegWords looks like..)


# 9.2. Number of syllables

mosaicplot (unComplex1$SyllPhon ~ unComplex1$SyllAct)
table(unComplex1$SyllPhon, unComplex1$SyllAct)
#      2  3  4  5  6
#   2 11  0  0  0  0
#   3  2 47  6  0  0
#   4  0  8 35  1  0
#   5  1  1 10 25  0
#   6  0  0  0  6  3
#   7  0  0  0  0  1

cor.test(unComplex1$SyllPhon, unComplex1$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  unComplex1$SyllPhon and unComplex1$SyllAct
# S = 86882, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8652898 

# Both are fairly similar, but SyllAct (the actual number of syllables) 
# seems to be more plausible.

plot (unComplex1$RelDur ~ unComplex1$SyllAct)


#     9.3. Word Duration

plot (unComplex1$RelDur ~ unComplex1$WordDur)

# there seems to be an effect!

# But there is also  a relation between SyllAct and NoSegWords


cor.test(unComplex1$NoSegWord, unComplex1$SyllAct, method="spearman")
# Spearman's rank correlation rho
# 
# data:  unComplex1$NoSegWord and unComplex1$SyllAct
# S = 109540, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8301561

# There is also a correlation between the number of segments in the word and 
# word duration:

plot(unComplex1$NoSegWord, unComplex1$WordDur)
cor.test (unComplex1$NoSegWord, unComplex1$WordDur)
# 
# Pearson's product-moment correlation
# 
# data:  unComplex1$NoSegWord and unComplex1$WordDur
# t = 8.4469, df = 155, p-value = 2.023e-14
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.4438406 0.6600376
# sample estimates:
# cor 
# 0.5614457 


# So, there is, as expected, a rather strong correlation between the 
# number of segments and the duration of the word. This means that
# the model might show some unwanted effects of collinearity.

# Possible solutions:
# - Residualize/orthogonalize?
# - Principal component analysis?
# - Ignore?
# - Use only one of the two?
#

# We include NoSegWord since it is the most fine graines measure of segments
# in the word. In order to avoic coll. effects with word dur, we can
# combine the two in LocSpeech. This way we also "save" one variable.


#    10. Stress

unComplex1$AffixStress<- droplevels(unComplex1$AffixStress)

table (unComplex1$AffixStress)
#d   s 
#101  56

plot (unComplex1$RelDur ~ unComplex1$AffixStress)

# ite seems like,when the secondary stress is on the affix, the nasal is pronounced
# shorter tham when stress is debatable

table (unComplex1$AdjSyllStress)
#p   s   u 
#100   1  56 

unique(unComplex1[unComplex1$AdjSyllStress=="u","item"])
# [1] uncontrolled    unpredictable   unemployment    unemployed      undecided      
# [6] unconventional  undefeated      unacceptable    unaccustomed    uncontrollable 
# [11] unofficially    unpopularity    unbelievably    unimpressive    uninformed     
# [16] unelectable     uninspiring     unfamiliar      unattended      unaccepted     
# [21] unintentionally unreliable      unexpected      uninsured       unavailable    
# [26] unexpectedly    uninsurable     unannounced     unintentional   unaffordable   
# [31] uninhabitable   unproductive    undevoted       unincorporated  undeserving    
# [36] unimportant     unimaginable

unique(unComplex1[unComplex1$AdjSyllStress=="p","item"])
# [1] unair-conditioned unusually         unfit             unable            unheard          
# [6] undisciplined     untouchable       unfortunate       unfairness        unduly           
# [11] unhealthy         uninfluenced      unwind            unfreeze          unread           
# [16] uneven            unoccupied        uneducated        unbiased          unnecessarily    
# [21] uneasy            unusual           unnecessary       uninteresting     unevenness       
# [26] unpopular         unauthorized      Untouchables      unnerved          unknown          
# [31] uncourteous       undone            unnerving         unstitched        unlicensed       
# [36] unsatisfactory    unmarried         unloaded          unopened          untrained        
# [41] unnatural         unplugged         unorthodox        unlikely          unused           
# [46] unfortunately     unevenly          unanswered        unusable          unthinkable      
# [51] unarmed           untalented        unethical         unorganized       unequal          
# [56] unscrewing        unkind            unquestionably    unsuitable        unbolted         
# [61] unregulated       unwarranted       unfriendly       

plot (unComplex1$RelDur ~ unComplex1$AdjSyllStress)

# when primary stress auf zweiter Silbe, länger!!! Könnte im Zusammenhang mit
# der double liegen --> hier /n/ Teil der zweiten Silbe

table (unComplex1$AdjSyllStress,unComplex1$NoCons)
#  single double
#p     77     23
#s      1      0
#u     56      0


#alle mit doubles sind auch stressed....


table (unComplex1$AdjSyllStress,unComplex1$TransitionType)
#     double-V single-C single-V
# p       23       54       23
# s        0        0        1
# u        0       13       43

# Außerdem:Zusammenhang zwischen Affix-stress und AdjSyllStress

table (unComplex1$AdjSyllStress,unComplex1$AffixStress)

#   d   s
#p 100   0
#s   1   0
#u   0  56

# --> primary stress auf der adjacent syllable,debatable stress auf dem Affix
# --> unstressed adjacent syllable, secondary stress auf affix (lange Wörter), 

# deshalb erstellen einer neuen Variable - stress pattern kombinert AsjSyllStress und AffixStress


unComplex1$StressPattern <- factor(paste (unComplex1$AffixStress, unComplex1$AdjSyllStress, sep="-"))
levels(unComplex1$StressPattern)
#[1] "d-p" "d-s" "s-u"

table (unComplex1$StressPattern)
#d-p d-s s-u 
#100   1  56 


# since there is only one case in which the adjacent syllable is secondary stressed we will
# incorpprate it in d-p ans name this variable d-st (debatable-stressed)


levels(unComplex1$StressPattern)
#[1] "d-p" "d-s" "s-u"

levels(unComplex1$StressPattern)= c("d-st","d-st", "s-u")

levels(unComplex1$StressPattern)
#[1] "d-st" "s-u" 

plot (unComplex1$RelDur ~ unComplex1$StressPattern)
#when the adjacent syllable is stressed n is longer than when only the affix is stressed

# one further problem could be that certain StressPatterns correlate with NoSegWords

table (unComplex1$StressPattern,unComplex1$NoSegWord)
#      4  5  6  7  8  9 10 11 12 13
# d-st  3  8 13 26 19 16  8  6  2  0
# s-u   0  0  0  3 10  9  9 13  7  5


plot(table (unComplex1$StressPattern,unComplex1$NoSegWord))

# kurze Wörter: debatabale - stressed 
# --> könnte Kollinearitätsprobleme verursachen

plot(unComplex1$StressPattern,unComplex1$WordDur)

# same relation with WordDur...
# we will ignore this problem for now and see how these variables behave in the model


## We are going to include the following predictors:
# - TransitionType
# - logWordFormFreqAllCoca
# - logRelFreq
# - Stress Pattern
# - LocSpeech
# - Accent

# Due to the unnestedness of the data, there is no point in including
# either Speaker or Item as a random effect. This would lead to a seriously
# overpowered model in which almost everything can be predicted on the 
# basis of the random effect structure alone:

tmp.lmer <- lmer(RelDur ~ 1 + (1|item) + (1|Speaker), data = unComplex1)
cor(unComplex1$RelDur, fitted(tmp.lmer))^2
# [1] 0.8510241

###############################################################################
# Also: Note: I tested the influence of different decomposability measurments##
# and NONE has an influence on duration (see other R-file for this!)          #
###############################################################################

# Before doing an initial model, let's look at the number of types and tokens 
# in each category

table(unComplex1$TransitionType)

#double-V single-C single-V 
#23       67       67 

# Number of types for Nocons= double and NoCons= single 

unComplex1Double<- unComplex1[unComplex1$NoCons=="double",]

unComplex1Single<- unComplex1[unComplex1$NoCons=="single",]

unComplex1SingleC<- unComplex1Single[unComplex1Single$FollSegVC=="C",]

unComplex1SingleV<- unComplex1Single[unComplex1Single$FollSegVC=="V",]

unComplex1Double$item<- factor(unComplex1Double$item)

unComplex1Single$item<- factor(unComplex1Single$item)

unComplex1SingleC$item<- factor(unComplex1SingleC$item)

unComplex1SingleV$item<- factor(unComplex1SingleV$item)


levels(unComplex1Double$item)
#[1] "unnatural"     "unnecessarily" "unnecessary"   "unnerved"      "unnerving"   

str(unComplex1Double$item)
#Factor w/ 6 levels "unnatural","unnecessarily",..: 2 3 4 5 5 3 3 5 5 3 ...

# So there are 6 different types with a double consonant

str(unComplex1Single$item)
#Factor w/ 95 levels "unable","unacademically",..: 18 7 94 42 76 1 48 32 31 21 ...

str(unComplex1SingleC$item)
#Factor w/ 53 levels "unbiased","unbolted",..: 4 51 17 33 23 7 5 11 50 45 ...


# So there are 53 different types with a  single nasal followed by a consonant

str(unComplex1SingleV$item)
# Factor w/ 42 levels "unable","unacademically",..: 7 1 18 17 3 30 18 5 21 39 ...

# So there are 42 different types with a  single nasal followed by a vowel


# Let's create the variable logRelDur - just for the sake of being able
# to get a summary of that variable
unComplex1$logRelDur <- log(unComplex1$RelDur)



#  Now - let's get the summaries of the variables which are in our initial
# model 

summary (unComplex1$logRelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.09100 -0.82180 -0.37500 -0.42900 -0.02516  0.88310 

sd (unComplex1$logRelDur)
#[1] 0.5625816


summary (unComplex1$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1236  0.4396  0.6873  0.7585  0.9752  2.4180 

sd (unComplex1$RelDur)
#[1] 0.4286809

summary (unComplex1SingleC$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2364  0.5194  0.7577  0.7982  0.9635  1.8830 

sd (unComplex1SingleC$RelDur)
#[1] 0.3621193

summary (unComplex1SingleV$RelDur)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1236  0.3274  0.4609  0.5379  0.6477  1.7360 

sd (unComplex1SingleV$RelDur)
#[1] 0.2940353

summary (unComplex1Double$RelDur)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6636  0.8921  1.2630  1.2860  1.5460  2.4180 
  
sd (unComplex1Double$RelDur)
#[1] 0.4547348

# Let's conduct an anova, to see whwther the differences between the environments are significant



anova.un<-aov(RelDur~TransitionType,data=unComplex1)

summary(anova.un)


# Df Sum Sq Mean Sq F value   Pr(>F)    
# TransitionType   2  9.758   4.879   39.73 1.22e-14 ***
#   Residuals      154 18.910   0.123                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


TukeyHSD(anova.un)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = RelDur ~ TransitionType, data = unComplex1)
# 
# $TransitionType
# diff        lwr        upr    p adj
# single-C-double-V -0.4874624 -0.6878769 -0.2870480 1.00e-07
# single-V-double-V -0.7477586 -0.9481730 -0.5473441 0.00e+00
# single-V-single-C -0.2602961 -0.4035765 -0.1170158 8.94e-05

Tuk.un2<-glht(anova.un,linfct=mcp(TransitionType="Tukey"))

summary(Tuk.un2)

# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: aov(formula = RelDur ~ TransitionType, data = unComplex1)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
# single-C - double-V == 0 -0.48746    0.08468  -5.756  < 1e-05 ***
#   single-V - double-V == 0 -0.74776    0.08468  -8.830  < 1e-05 ***
#   single-V - single-C == 0 -0.26030    0.06054  -4.299 8.98e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- single-step method)

summary (unComplex1$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   6.252   7.098   7.166   8.429   9.838 

sd (unComplex1$logWordFormFreqAllCoca)
#[1] 1.947607


summary (unComplex1$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.4950 -2.2580 -1.1290 -0.8035  0.2124  7.0980 

sd (unComplex1$logRelFreq)
#[1] 2.525329

summary (unComplex1$WordDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3314  0.5406  0.6348  0.6691  0.7950  1.2650 

sd(unComplex1$WordDur)
#[1] 0.183301

summary (unComplex1$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.136  11.300  13.130  13.170  15.150  20.570 

sd(unComplex1$LocSpeech)
#[1] 2.9911

summary (unComplex1$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02526 0.06961 0.08472 0.08736 0.10420 0.16650 

sd(unComplex1$PrecSegDur)
#[1] 0.02849342

summary (unComplex1$NoSegWord)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.000   7.000   8.000   8.548  10.000  13.000 

sd (unComplex1$NoSegWord)
#[1] 2.098369

summary (unComplex1$TransitionType)
#double-V single-C single-V 
#23       67       67 

summary (unComplex1$StressPattern)
#d-st  s-u 
#101   56 

# I would also like to check the distribution of the doubles and singles


summary(unComplex1Single$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1236  0.4185  0.5785  0.6680  0.8586  1.8830 

sd(unComplex1Single$RelDur)
#[1] 0.353612

summary(unComplex1Single$LocSpeech)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.136  11.520  13.420  13.470  15.250  20.570 

sd(unComplex1Single$LocSpeech)
#[1] 3.004381


# now doubles

summary(unComplex1Double$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6636  0.8921  1.2630  1.2860  1.5460  2.4180 

sd(unComplex1Double$RelDur)
#[1] 0.4547348


summary(unComplex1Double$LocSpeech)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.276   9.733  11.880  11.410  13.180  14.290 

sd(unComplex1Double$LocSpeech)
#[1] 2.254765

## Do an initial model:

unComplex.lm1 <- lm (RelDur ~ StressPattern +TransitionType + logWordFormFreqAllCoca + logRelFreq + LocSpeech , data = unComplex1)

summary (unComplex.lm1)

  
# Call:
#   lm(formula = RelDur ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech, data = unComplex1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.62628 -0.21010 -0.04953  0.14407  1.16080 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.1430935  0.1725368   6.625 5.84e-10 ***
#   StressPatterns-u       -0.0754988  0.0711037  -1.062    0.290    
#   TransitionTypesingle-C -0.4603480  0.0882576  -5.216 5.98e-07 ***
#   TransitionTypesingle-V -0.6853579  0.0976612  -7.018 7.25e-11 ***
#   logWordFormFreqAllCoca  0.0183105  0.0154090   1.188    0.237    
#   logRelFreq              0.0042657  0.0116393   0.366    0.715    
#   LocSpeech               0.0002635  0.0101444   0.026    0.979    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3514 on 150 degrees of freedom
# Multiple R-squared:  0.354,	Adjusted R-squared:  0.3282 
# F-statistic:  13.7 on 6 and 150 DF,  p-value: 2.24e-12

# Before we go on fitting the model, we should check the assumptions od the model:

qqnorm (residuals (unComplex.lm1))
qqline (residuals (unComplex.lm1))

# The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors


# We will try what happens whem we transform the dep. variable

# 1.boxcox transformation:

bc <- boxcox(unComplex.lm1)

lambda <- bc$x[which.max(bc$y)]

unComplex1$bc <- unComplex1$RelDur^lambda

unComplexBC.lm1 <- lm (bc~ StressPattern +TransitionType + logWordFormFreqAllCoca + logRelFreq + LocSpeech , data = unComplex1)
summary(unComplexBC.lm1)

# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech, data = unComplex1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.16966 -0.04426  0.00142  0.03516  0.16905 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.9980759  0.0301143  33.143  < 2e-16 ***
#   StressPatterns-u       -0.0154928  0.0124103  -1.248    0.214    
#   TransitionTypesingle-C -0.0651459  0.0154043  -4.229 4.06e-05 ***
#   TransitionTypesingle-V -0.1123278  0.0170456  -6.590 7.02e-10 ***
#   logWordFormFreqAllCoca  0.0043820  0.0026895   1.629    0.105    
#   logRelFreq             -0.0006843  0.0020315  -0.337    0.737    
#   LocSpeech              -0.0003546  0.0017706  -0.200    0.842    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06133 on 150 degrees of freedom
# Multiple R-squared:  0.3548,	Adjusted R-squared:  0.329 
# F-statistic: 13.75 on 6 and 150 DF,  p-value: 2.063e-12

#let's check the assumptions again

par(mfrow=c(2,2))
plot(unComplexBC.lm1)

par(mfrow=c(1,1))

qqnorm (residuals (unComplexBC.lm1))
qqline (residuals (unComplexBC.lm1))

#looks very good

 
# Let's take a look at influential points

plot(cooks.distance(unComplexBC.lm1))

# This is kind of okay. The residuals are not too far away from the qq line,
# and there are only a few observations left with high residuals.

# Let's now fit our model


######################################
# We should check our variable accent first since we suspect some
# problems and we have 3 different accent variables...
# What we will do is to fit 3 models: one for each accet variable and
# we will check whether any of teh variables has any influence
#########################################

#
unComplexBC.lmAccent1 <- lm (bc~ PhrasalAccentSonia+StressPattern, data = unComplex1)
summary(unComplexBC.lmAccent1)

# no


unComplexBC.lmAccent2 <- lm (bc~ AccentIngo+StressPattern, data = unComplex1)
summary(unComplexBC.lmAccent2)


# no

# for the third one, I would like to recode the variable, sonia's unclears
# into no (since she did not hear any accent but just was not sure whether this
# can really be true). If we recode the unsures into nos, we get the clearly
# accented ones on the one hand and all the others on the other


levels(unComplex1$AnyAccentSonia)
#[1] "no"      "unclear" "yes"    

levels(unComplex1$AnyAccentSonia)<- c("no", "no", "yes")

levels(unComplex1$AnyAccentSonia)
#[1] "no"  "yes"

unComplexBC.lmAccent3 <- lm (bc~ AnyAccentSonia+StressPattern, data = unComplex1)
summary(unComplexBC.lmAccent3)


# no

# what about an interaction

unComplexBC.lmAccent4 <- lm (bc~ AnyAccentSonia*StressPattern, data = unComplex1)
summary(unComplexBC.lmAccent4)

# no

#So accent will not play any part in the models but as mentioned before:
# one has to be careful with the interpretation


# now let's update or model:

# The update() function takes a model as the first parameter, and a 
# formula that specifies the changes made to that model as the second.
# In that formula, the part "~ ." means "everything in the preceding
# model". If you want to add additional predictors, use the "+ Pred" 
# notation, and if you want to remove a predictor, use "- Pred".

# So what do we exclude first 

summary (unComplexBC.lm1)

# LocSpeech

# So, the following line gives the summary of an updated version of your
# initial model, excluding the predictor WordFormFreq:

unComplexBC.lm2 <- update(unComplexBC.lm1, ~ . - LocSpeech)
summary(unComplexBC.lm2)

# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq, data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.170967 -0.044919  0.001244  0.034723  0.169455 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.9946804  0.0248090  40.093  < 2e-16 ***
#   StressPatterns-u       -0.0160742  0.0120277  -1.336    0.183    
#   TransitionTypesingle-C -0.0657159  0.0150909  -4.355 2.45e-05 ***
#   TransitionTypesingle-V -0.1128611  0.0167827  -6.725 3.40e-10 ***
#   logWordFormFreqAllCoca  0.0042989  0.0026488   1.623    0.107    
#   logRelFreq             -0.0006816  0.0020250  -0.337    0.737    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06113 on 151 degrees of freedom
# Multiple R-squared:  0.3546,	Adjusted R-squared:  0.3332 
# F-statistic: 16.59 on 5 and 151 DF,  p-value: 4.844e-13

# without WordFormFreq the model becomes slightly better (R2) and the
#other variables do not display any different behavior.

# Next we should exclude RelFreq

unComplexBC.lm3<-update(unComplexBC.lm2, ~ . - logRelFreq)

summary(unComplexBC.lm3)


# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logWordFormFreqAllCoca, 
#      data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.169792 -0.044506  0.000009  0.034731  0.169884 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.997264   0.023523  42.396  < 2e-16 ***
#   StressPatterns-u       -0.016248   0.011981  -1.356    0.177    
#   TransitionTypesingle-C -0.065972   0.015028  -4.390 2.11e-05 ***
#   TransitionTypesingle-V -0.112950   0.016732  -6.751 2.92e-10 ***
#   logWordFormFreqAllCoca  0.004044   0.002531   1.598    0.112    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06095 on 152 degrees of freedom
# Multiple R-squared:  0.3541,	Adjusted R-squared:  0.3371 
# F-statistic: 20.84 on 4 and 152 DF,  p-value: 1.039e-13

# the model became slightly better

# Next we should exclude Stress

unComplexBC.lm4<-update(unComplexBC.lm3, ~ . - StressPattern)

summary(unComplexBC.lm4)

# Call:
#   lm(formula = bc ~ TransitionType + logWordFormFreqAllCoca, data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.159371 -0.044104  0.001444  0.034655  0.180320 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.997082   0.023587  42.273  < 2e-16 ***
#   TransitionTypesingle-C -0.069107   0.014890  -4.641 7.39e-06 ***
#   TransitionTypesingle-V -0.123360   0.014908  -8.275 5.88e-14 ***
#   logWordFormFreqAllCoca  0.004067   0.002538   1.603    0.111    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06112 on 153 degrees of freedom
# Multiple R-squared:  0.3463,	Adjusted R-squared:  0.3335 
# F-statistic: 27.02 on 3 and 153 DF,  p-value: 4.434e-14

# Next we should exclude WordFormFreq

unComplexBC.lm5<-update(unComplexBC.lm4, ~ . - logWordFormFreqAllCoca)

summary(unComplexBC.lm5)

# Call:
#   lm(formula = bc ~ TransitionType, data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.158254 -0.046982  0.002493  0.039230  0.178798 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.02889    0.01281  80.326  < 2e-16 ***
#   TransitionTypesingle-C -0.07212    0.01485  -4.858 2.89e-06 ***
#   TransitionTypesingle-V -0.12659    0.01485  -8.527 1.30e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06143 on 154 degrees of freedom
# Multiple R-squared:  0.3353,	Adjusted R-squared:  0.3267 
# F-statistic: 38.85 on 2 and 154 DF,  p-value: 2.188e-14

# Let's take a look at this:

visreg(unComplexBC.lm5)

# ich würde gerne die duration zurück transformieren. Dafür müsste ich die lamdate-Wurzel von bc berechenen

visreg(unComplex.reduced,trans=function(x) x^(1/lambda))

# which value is lambda?

lambda
#[1] 0.1414141


######################################################################################
# Summary:
# - This model is way worse than the one with the absolute duration but
#   it also shows that doubles are longer than singletons - even in relative dur
# - LocSpeech does not have an effect, probably because the relation (reg. dur.)
# between the nasal and the vowel does not change depending on speech rate. Makes sense!
######################################################################################

# let's fit a mumin

library(MuMIn)

options(na.action = "na.fail") 

model_ranking <- dredge(unComplexBC.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = bc ~ <32 unique rhs>, data = unComplex1)
# 
# Component models: 
#   df logLik    AICc delta weight
# 35      5 218.06 -425.72  0.00   0.16
# 345     6 219.00 -425.44  0.27   0.14
# 5       4 216.75 -425.24  0.48   0.13
# 45      5 217.69 -424.99  0.73   0.11
# 135     6 218.19 -423.82  1.90   0.06
# 235     6 218.14 -423.71  2.00   0.06
# 2345    7 219.06 -423.37  2.35   0.05
# 1345    7 219.02 -423.29  2.43   0.05
# 15      5 216.78 -423.17  2.55   0.05
# 25      5 216.75 -423.11  2.61   0.04
# 245     6 217.70 -422.85  2.87   0.04
# 145     6 217.70 -422.83  2.89   0.04
# 1235    7 218.27 -421.79  3.93   0.02
# 12345   8 219.08 -421.19  4.53   0.02
# 125     6 216.79 -421.02  4.70   0.02
# 1245    7 217.70 -420.66  5.06   0.01
# 34      4 198.18 -388.11 37.61   0.00
# 134     5 198.72 -387.05 38.67   0.00
# 234     5 198.25 -386.10 39.62   0.00
# 4       3 196.00 -385.84 39.88   0.00
# 1234    6 198.79 -385.01 40.70   0.00
# 14      4 196.27 -384.27 41.44   0.00
# 24      4 196.03 -383.80 41.92   0.00
# 124     5 196.31 -382.22 43.50   0.00
# 13      4 189.89 -371.51 54.20   0.00
# 123     5 190.01 -369.62 56.09   0.00
# 3       3 187.10 -368.04 57.68   0.00
# 1       3 186.82 -367.47 58.24   0.00
# 23      4 187.23 -366.20 59.52   0.00
# 12      4 186.84 -365.42 60.30   0.00
# (Null)  2 184.68 -365.29 60.43   0.00
# 2       3 184.69 -363.23 62.49   0.00
# 
# Term codes: 
#   LocSpeech             logRelFreq logWordFormFreqAllCoca          StressPattern 
# 1                      2                      3                      4 
# TransitionType 
# 5 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             1.012e+00  2.704e-02   2.718e-02  37.219  < 2e-16 ***
#   logWordFormFreqAllCoca  2.354e-03  2.835e-03   2.845e-03   0.827    0.408    
# TransitionTypesingle-C -6.871e-02  1.520e-02   1.532e-02   4.485  7.3e-06 ***
#   TransitionTypesingle-V -1.197e-01  1.675e-02   1.687e-02   7.093  < 2e-16 ***
#   StressPatterns-u       -7.456e-03  1.153e-02   1.157e-02   0.644    0.519    
# LocSpeech              -1.195e-04  9.298e-04   9.367e-04   0.128    0.898    
# logRelFreq             -8.861e-05  1.058e-03   1.066e-03   0.083    0.934    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             1.0117675  0.0270417   0.0271845  37.219  < 2e-16 ***
#   logWordFormFreqAllCoca  0.0041702  0.0025806   0.0026013   1.603    0.109    
# TransitionTypesingle-C -0.0687062  0.0152009   0.0153200   4.485  7.3e-06 ***
#   TransitionTypesingle-V -0.1196535  0.0167482   0.0168688   7.093  < 2e-16 ***
#   StressPatterns-u       -0.0162231  0.0121200   0.0122173   1.328    0.184    
# LocSpeech              -0.0004545  0.0017706   0.0017843   0.255    0.799    
# logRelFreq             -0.0003401  0.0020527   0.0020683   0.164    0.869    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                        TransitionType logWordFormFreqAllCoca StressPattern LocSpeech logRelFreq
# Importance:          1.00           0.56                   0.46          0.26      0.26      
# N containing models:   16             16                     16            16        16      
