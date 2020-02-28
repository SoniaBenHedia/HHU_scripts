# I can use the checkpoint function to save the packages I used when running
#this script:

# library(checkpoint)
# checkpoint("2016-11-15")

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
In <- d2[d2$AffixOrth=="in" | d2$AffixOrth=="im", ]
un <- d2[d2$AffixOrth== "un", ]
ly <- d2[d2$AffixOrth== "ly" | d2$AffixOrth== "lely",]

# write data dets in csv

write.csv(In,file = "In_complete_SWB.csv")
write.csv(un,file = "un_complete_SWB.csv")
write.csv(ly,file = "ly_complete_SWB.csv")

# Den Datensatz erstellen, in dem alle komplexen un-items sind

un <- d2[d2$AffixOrth== "un", ]
unComplex <- un[un$MorphBound=="transparent", ]


dim(unComplex)
# NOTE : now we have 158 --> one outlier was excluded in the scipt, so it
# was not coded for accentedness and is out already

# [1] 158  58


# We create the variable RelFreq by dividing the word lemma freq by the base lemma freq
unComplex$RelFreq <- unComplex$WordLemmaFreqAllCoca / unComplex$BaseLemmaFreqAllCoca
unComplex$logRelFreq <- log(unComplex$WordLemmaFreqAllCoca / unComplex$BaseLemmaFreqAllCoca)

densityplot(unComplex$AbsDurCon)
plot(unComplex$AbsDurCon)

#######################################################################
##### In the "original" un-dataset, there was one outlier:
# use identify() to find out the line numbers of selected points:
#outliers <- identify(unComplex$AbsDurCon)
#outliers
#[1] 76

# mit dieser Funktion wird eine Linie in die Grpahik eingefügt: Hilfreich um Grenzen festzulegen
#abline(0.15,0)

# we remove outliers
#unComplex <- unComplex [-outliers, ]
#unComplex1<- unComplex[-76, ]

#########################################
# So, originally we excluded one item because of its very long duration. Since this item
# was not coded for accentuation, and the accentuation file was used to merge, in
# this unComplex dataset, the outlier is already removed. Hence, we will just
# rename it into unComplex 1
#
unComplex1<- unComplex

dim(unComplex1)
# [1] 158  58


# Let's plot the distribution of the data

densityplot(unComplex1$AbsDurCon)
plot(unComplex1$AbsDurCon)


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
plot(unComplex1$PartofSpeech, unComplex1$AbsDurCon)
table (unComplex1$PartofSpeech)
#     a adv   n  pn   v 
#    130  12  11   1   4 

# There is no point in including this as a predictor in a model.
# (a) some of the levels are too 'small'
# (b) there is no good theoretical reason why this should matter in the
# first place.

#       2. Number of consonants (NoCons)

table(unComplex1$NoCons)
# single double 
#    136     22


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
#135     23

plot (unComplex1$AbsDurCon ~ unComplex1$NoCons)

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

plot (unComplex1$logRelFreq, unComplex1$AbsDurCon)
plot (unComplex1$logWordFormFreqAllCoca, unComplex1$AbsDurCon)
plot (unComplex1$logBaseFormFreqAllCoca, unComplex1$AbsDurCon)

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
#    14   121    23 

plot (unComplex1$AbsDurCon ~ unComplex1$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.



#     5. Word Duration

plot (density(unComplex1$WordDur))
plot (unComplex1$AbsDurCon ~ unComplex1$WordDur)


#There seems to be a relation
# between the overall duration of a word (perhaps due to speech rate?, 
# or due to phrase-final lengthening?) and the duration of the consonants.
# The longer a word is, the longer are its segments pronounced.

#     6. Preceding Segment duration

plot (density(unComplex1$PrecSegDur))
plot(unComplex1$PrecSegDur)
plot (unComplex1$AbsDurCon ~ unComplex1$PrecSegDur)

# Both plots don't suggest anything remarkable, but we include the duration
# of the preceding segments anyway, because of other studies (such as Oh &
# Redford) have emphasized the interplay between the preceding vowel 
# duration and the geminate duration.
# Plus, if there is indeed no effect whatsover of PrecSegDur on AbsDurCon,
# there may be no need for a model of RelDurCon.

#     7. Following segment

unComplex1$FollSegVC<-droplevels(unComplex1$FollSegVC)
table (unComplex1$FollSegVC)
#C  V 
#68 90

# Problem: After double Ns, only vowels may occur:

table (unComplex1$FollSegVC, unComplex1$NoCons)
#          single double
#C     68      0
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
     
plot(unComplex1$AbsDurCon~unComplex1$LocSpeech) 

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
#   3  2 48  6  0  0
#   4  0  8 35  1  0
#   5  1  1 10 25  0
#   6  0  0  0  6  3
#   7  0  0  0  0  1

cor.test(unComplex1$SyllPhon, unComplex1$SyllAct, method="spearman")
#         Spearman's rank correlation rho
# 
# data:  unComplex$SyllPhon and unComplex$SyllAct
# S = 88182.05, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.865854

# Both are fairly similar, but SyllAct (the actual number of syllables) 
# seems to be more plausible.

plot (unComplex1$AbsDurCon ~ unComplex1$SyllAct)


#     9.3. Word Duration

plot (unComplex1$AbsDurCon ~ unComplex1$WordDur)

# there seems to be an effect!

# But there is also  a relation between SyllAct and NoSegWords


cor.test(unComplex1$NoSegWord, unComplex1$SyllAct, method="spearman")
#         Spearman's rank correlation rho
# 
# data:  unComplex$NoSegWord and unComplex$SyllAct
# S = 111847.2, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.8298538 

# There is also a correlation between the number of segments in the word and 
# word duration:

plot(unComplex1$NoSegWord, unComplex1$WordDur)
cor.test (unComplex1$NoSegWord, unComplex1$WordDur)
# 
# Pearson's product-moment correlation
# 
# data:  unComplex$NoSegWord and unComplex$WordDur
# t = 8.4718, df = 156, p-value = 1.688e-14
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.4441243 0.6596609
# sample estimates:
# cor 
# 0.5613382 


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
#102  56

plot (unComplex1$AbsDurCon ~ unComplex1$AffixStress)

# ite seems like,when the secondary stress is on the affix, the nasal is pronounced
# shorter tham when stress is debatable

table (unComplex1$AdjSyllStress)
#p   s   u 
#101   1  56 

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
# [6] undisciplined     unusual           untouchable       unfortunate       unfairness       
# [11] unduly            unhealthy         uninfluenced      unread            uneven           
# [16] unoccupied        uneducated        unbiased          unnecessarily     uneasy           
# [21] unnecessary       uninteresting     unevenness        unpopular         unauthorized     
# [26] Untouchables      unnerved          unknown           uncourteous       undone           
# [31] unnerving         unstitched        unlicensed        unsatisfactory    unmarried        
# [36] unloaded          unopened          untrained         unnatural         unplugged        
# [41] unorthodox        unlikely          unused            unfortunately     unevenly         
# [46] unanswered        unusable          unthinkable       unarmed           untalented       
# [51] unethical         unorganized       unequal           unscrewing        unkind           
# [56] unquestionably    unsuitable        unbolted          unregulated       unwarranted      
# [61] unfriendly        unwind            unfreeze      



plot (unComplex1$AbsDurCon ~ unComplex1$AdjSyllStress)

# when primary stress auf zweiter Silbe, länger!!! Könnte im Zusammenhang mit
# der double liegen --> hier /n/ Teil der zweiten Silbe

table (unComplex1$AdjSyllStress,unComplex1$NoCons)
#  single double
#p     79     22
#s      1      0
#u     56      0


#alle mit doubles sind auch stressed....


table (unComplex1$AdjSyllStress,unComplex1$TransitionType)
#     double-V single-C single-V
# p       23       55       23
# s        0        0        1
# u        0       13       43

# Außerdem:Zusammenhang zwischen Affix-stress und AdjSyllStress

table (unComplex1$AdjSyllStress,unComplex1$AffixStress)

#   d   s
#p 101   0
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
#101   1  56 


# since there is only one case in which the adjacent syllable is secondary stressed we will
# incorpprate it in d-p ans name this variable d-st (debatable-stressed)


levels(unComplex1$StressPattern)
#[1] "d-p" "d-s" "s-u"

levels(unComplex1$StressPattern)= c("d-st","d-st", "s-u")

levels(unComplex1$StressPattern)
#[1] "d-st" "s-u" 

table(unComplex1$StressPattern)
# d-st  s-u 
# 102   56 

unique(unComplex1[unComplex1$StressPattern=="d-st","item"])
# [1] unair-conditioned unusually         unfit             unable           
# [5] unheard           undisciplined     unusual           untouchable      
# [9] unfortunate       unfairness        unduly            unhealthy        
# [13] uninfluenced      unwind            unfreeze          unread           
# [17] uneven            unoccupied        uneducated        unacademically   
# [21] unbiased          unnecessarily     uneasy            unnecessary      
# [25] uninteresting     unevenness        unpopular         unauthorized     
# [29] Untouchables      unnerved          unknown           uncourteous      
# [33] undone            unnerving         unstitched        unlicensed       
# [37] unsatisfactory    unmarried         unloaded          unopened         
# [41] untrained         unnatural         unplugged         unorthodox       
# [45] unlikely          unused            unfortunately     unevenly         
# [49] unanswered        unusable          unthinkable       unarmed          
# [53] untalented        unethical         unorganized       unequal          
# [57] unscrewing        unkind            unquestionably    unsuitable       
# [61] unbolted          unregulated       unwarranted       unfriendly  

unique(unComplex1[unComplex1$StressPattern=="s-u","item"])
# [1] uncontrolled    unpredictable   unemployment    unemployed     
# [5] undecided       unconventional  undefeated      unacceptable   
# [9] unaccustomed    uncontrollable  unofficially    unpopularity   
# [13] unbelievably    unimpressive    uninformed      unelectable    
# [17] uninspiring     unfamiliar      unattended      unaccepted     
# [21] unintentionally unreliable      unexpected      uninsured      
# [25] unavailable     unexpectedly    uninsurable     unannounced    
# [29] unintentional   unaffordable    uninhabitable   unproductive   
# [33] undevoted       unincorporated  undeserving     unimportant    
# [37] unimaginable   

plot (unComplex1$AbsDurCon ~ unComplex1$StressPattern)
#when the adjacent syllable is stressed n is longer than when only the affix is stressed

# one further problem could be that certain StressPatterns correlate with NoSegWords

table (unComplex1$StressPattern,unComplex1$NoSegWord)
#      4  5  6  7  8  9 10 11 12 13
#d-st  3  8 13 26 20 16  8  6  2  0
#s-u   0  0  0  3 10  9  9 13  7  5


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
# - PrecSegDur
# - Stress Pattern
# - LocSpeech
# - Accent

# Due to the unnestedness of the data, there is no point in including
# either Speaker or Item as a random effect. This would lead to a seriously
# overpowered model in which almost everything can be predicted on the 
# basis of the random effect structure alone:

tmp.lmer <- lmer(AbsDurCon ~ 1 + (1|item) + (1|Speaker), data = unComplex1)
cor(unComplex1$AbsDurCon, fitted(tmp.lmer))^2
# [1] 0.9772724

###############################################################################
# Also: Note: I tested the influence of different decomposability measurments##
# and NONE has an influence on duration (see other R-file for this!)          #
###############################################################################

# Before doing an initial model, let's look at the number of types and tokens 
# in each category

table(unComplex1$TransitionType)

#double-V single-C single-V 
#23       68       67 

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


# Let's create the variable logAbsDurCon - just for the sake of being able
# to get a summary of that variable
unComplex1$logAbsDurCon <- log(unComplex1$AbsDurCon)



#  Now - let's get the summaries of the variables which are in our initial
# model 

summary (unComplex1$logAbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.152  -3.247  -2.912  -2.919  -2.554  -1.988 

sd (unComplex1$logAbsDurCon)
#[1] 0.4838156


summary (unComplex1$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01573 0.03889 0.05435 0.06040 0.07775 0.13700  


sd (unComplex1$AbsDurCon)
#[1] 0.02847142


summary (unComplex1SingleC$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02143 0.04504 0.06027 0.06381 0.07658 0.13340   


sd (unComplex1SingleC$AbsDurCon)
#[1] 0.02439148

summary (unComplex1SingleV$AbsDurCon)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01573 0.03139 0.04001 0.04347 0.05168 0.09184 


sd (unComplex1SingleV$AbsDurCon)
#[1] 0.01840824

summary (unComplex1Double$AbsDurCon)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.06216 0.08324 0.10240 0.09965 0.11500 0.13700   


sd (unComplex1Double$AbsDurCon)
#[1] 0.02090666



# Let's conduct an anova, to see whwther the differences between the environments are significant

unComplex1$AbsDurConMS<-unComplex1$AbsDurCon*1000

# we will do this in ms, just so it becomes easier to write it down in ms

anova.un<-aov(AbsDurConMS~TransitionType,data=unComplex1)

summary(anova.un)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# TransitionType   2  55425   27713   59.79 <2e-16 ***
#   Residuals      155  71842     463                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


TukeyHSD(anova.un)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = AbsDurConMS ~ TransitionType, data = unComplex1)
# 
# $TransitionType
#                     diff       lwr       upr p adj
# single-C-double-V -35.84577 -48.13494 -23.55659 0e+00
# single-V-double-V -56.18024 -68.49258 -43.86791 0e+00
# single-V-single-C -20.33447 -29.10439 -11.56456 5e-07

Tuk.un2<-glht(anova.un,linfct=mcp(TransitionType="Tukey"))

summary(Tuk.un2)

# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: aov(formula = AbsDurConMS ~ TransitionType, data = unComplex1)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
# single-C - double-V == 0  -35.846      5.193  -6.903  < 1e-07 ***
#   single-V - double-V == 0  -56.180      5.203 -10.798  < 1e-07 ***
#   single-V - single-C == 0  -20.334      3.706  -5.487 4.95e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- single-step method)




summary (unComplex1$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   6.256   7.170   7.182   8.429   9.838 

sd (unComplex1$logWordFormFreqAllCoca)
#[1] 1.952759


summary (unComplex1$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.4950 -2.2580 -1.1250 -0.7974  0.1991  7.0980 

sd (unComplex1$logRelFreq)
#[1] 2.518439

summary (unComplex1$WordDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3314  0.5401  0.6337  0.6680  0.7947  1.2650 

sd(unComplex1$WordDur)
#[1] 0.1832108

summary (unComplex1$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.136  11.320  13.130  13.190  15.160  20.570

sd(unComplex1$LocSpeech)
#[1] 2.99003

summary (unComplex1$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01969 0.06811 0.08467 0.08694 0.10400 0.16650 

sd(unComplex1$PrecSegDur)
#[1] 0.02890834

summary (unComplex1$NoSegWord)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.000   7.000   8.000   8.544  10.000  13.000

sd (unComplex1$NoSegWord)
#[1] 2.092129

summary (unComplex1$TransitionType)
#double-V single-C single-V 
#23       68       67 

summary (unComplex1$StressPattern)
#d-st  s-u 
#102   56 


# I would also like to check the distribution of the doubles and singles


summary(unComplex1Single$AbsDurCon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01573 0.03668 0.05023 0.05372 0.06678 0.13340 

sd(unComplex1Single$AbsDurCon)
#[1] 0.02384352

summary(unComplex1Single$LocSpeech)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.136  11.540  13.430  13.490  15.330  20.570

sd(unComplex1Single$LocSpeech)
#[1] 3.001029


# now doubles

summary(unComplex1Double$AbsDurCon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.06216 0.08324 0.10240 0.09965 0.11500 0.13700

sd(unComplex1Double$AbsDurCon)
#[1] 0.02090666


summary(unComplex1Double$LocSpeech)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.276   9.733  11.880  11.410  13.180  14.290 

sd(unComplex1Double$LocSpeech)
#[1] 2.254765

## Do an initial model:

unComplex.lm1 <- lm (AbsDurCon ~ StressPattern +TransitionType + logWordFormFreqAllCoca + logRelFreq + LocSpeech + PrecSegDur, data = unComplex1)

summary (unComplex.lm1)

# Call:
#   lm(formula = AbsDurCon ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech + PrecSegDur, data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.054167 -0.014122 -0.001541  0.010149  0.054900 
# 
#   Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.1402263  0.0132387  10.592  < 2e-16 ***
#   StressPatterns-u        0.0012852  0.0039574   0.325    0.746    
#   TransitionTypesingle-C -0.0288964  0.0049242  -5.868 2.72e-08 ***
#   TransitionTypesingle-V -0.0476901  0.0054615  -8.732 4.54e-15 ***
#   logWordFormFreqAllCoca  0.0006384  0.0008610   0.741    0.460    
#   logRelFreq             -0.0006265  0.0006502  -0.964    0.337    
#   LocSpeech              -0.0036051  0.0006249  -5.769 4.41e-08 ***
#   PrecSegDur             -0.0592359  0.0619439  -0.956    0.340    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01952 on 150 degrees of freedom
# Multiple R-squared:  0.5508,	Adjusted R-squared:  0.5298 
# F-statistic: 26.28 on 7 and 150 DF,  p-value: < 2.2e-16

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

unComplex1$bc <- unComplex1$AbsDurCon^lambda

unComplexBC.lm1 <- lm (bc~ StressPattern +TransitionType + logWordFormFreqAllCoca + logRelFreq + LocSpeech + PrecSegDur, data = unComplex1)
summary(unComplexBC.lm1)

# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech + PrecSegDur, data = unComplex1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.132971 -0.026927 -0.000663  0.024283  0.110102 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.577006   0.028485  20.257  < 2e-16 ***
#   StressPatterns-u       -0.002856   0.008515  -0.335   0.7377    
# TransitionTypesingle-C -0.050835   0.010595  -4.798 3.83e-06 ***
#   TransitionTypesingle-V -0.092927   0.011751  -7.908 5.27e-13 ***
#   logWordFormFreqAllCoca  0.001714   0.001853   0.925   0.3562    
# logRelFreq             -0.002316   0.001399  -1.656   0.0999 .  
# LocSpeech              -0.007644   0.001345  -5.685 6.62e-08 ***
#   PrecSegDur             -0.123350   0.133281  -0.925   0.3562    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.042 on 150 degrees of freedom
# Multiple R-squared:  0.5407,	Adjusted R-squared:  0.5193 
# F-statistic: 25.23 on 7 and 150 DF,  p-value: < 2.2e-16

#let's check the assumptions again

par(mfrow=c(2,2))
plot(unComplexBC.lm1)

par(mfrow=c(1,1))

qqnorm (residuals (unComplexBC.lm1))
qqline (residuals (unComplexBC.lm1))

#there are two outliers



# reset rownames
rownames(unComplex1) <- seq(length=nrow(unComplex1))

#outliers <- identify(qqnorm (residuals (unComplexBC.lm1)))


#unComplex1 [outliers, c("item", "TransitionType")]

#          item TransitionType
#25     unread       single-C
#31 uneducated       single-C



# Since no double consosnant items are in the outlier dataset, we can exclude them (we have much more
# single consonant data)

# So, let's go on without them: We create the new dataset unComplex2

unComplex2 <- unComplex1 [-c(25, 31), ]

# We make a LM with the new dataset

unComplexBC.lm2 <- lm (bc~ StressPattern +TransitionType + logWordFormFreqAllCoca + logRelFreq + LocSpeech + PrecSegDur, data = unComplex2)
summary(unComplexBC.lm2)


# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech + PrecSegDur, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.083243 -0.026914 -0.002399  0.023331  0.111844 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.591222   0.027408  21.571  < 2e-16 ***
#   StressPatterns-u       -0.004403   0.008070  -0.546    0.586    
# TransitionTypesingle-C -0.046821   0.010069  -4.650 7.29e-06 ***
#   TransitionTypesingle-V -0.091627   0.011127  -8.234 8.72e-14 ***
#   logWordFormFreqAllCoca  0.000794   0.001769   0.449    0.654    
# logRelFreq             -0.001360   0.001357  -1.002    0.318    
# LocSpeech              -0.008046   0.001288  -6.246 4.23e-09 ***
#   PrecSegDur             -0.142550   0.126480  -1.127    0.262    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03975 on 148 degrees of freedom
# Multiple R-squared:  0.5784,	Adjusted R-squared:  0.5584 
# F-statistic:    29 on 7 and 148 DF,  p-value: < 2.2e-16

#We check the assumptions of the new model
par(mfrow=c(2,2))
plot(unComplexBC.lm2)

par(mfrow=c(1,1))

qqnorm (residuals (unComplexBC.lm2))
qqline (residuals (unComplexBC.lm2))

# Let's take a look at influential points

plot(cooks.distance(unComplexBC.lm2))

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
unComplexBC.lmAccent1 <- lm (bc~ PhrasalAccentSonia+StressPattern, data = unComplex2)
summary(unComplexBC.lmAccent1)

# no


unComplexBC.lmAccent2 <- lm (bc~ AccentIngo+StressPattern, data = unComplex2)
summary(unComplexBC.lmAccent2)


# no

# for the third one, I would like to recode the variable, sonia's unclears
# into no (since she did not hear any accent but just was not sure whether this
# can really be true). If we recode the unsures into nos, we get the clearly
# accented ones on the one hand and all the others on the other


levels(unComplex2$AnyAccentSonia)
#[1] "no"      "unclear" "yes"    

levels(unComplex2$AnyAccentSonia)<- c("no", "no", "yes")

levels(unComplex2$AnyAccentSonia)
#[1] "no"  "yes"

unComplexBC.lmAccent3 <- lm (bc~ AnyAccentSonia+StressPattern, data = unComplex2)
summary(unComplexBC.lmAccent3)


# no

# what about an interaction

unComplexBC.lmAccent4 <- lm (bc~ AnyAccentSonia*StressPattern, data = unComplex2)
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

summary (unComplexBC.lm2)

# WordFormFrq

# So, the following line gives the summary of an updated version of your
# initial model, excluding the predictor WordFormFreq:

unComplexBC.lm3 <- update(unComplexBC.lm2, ~ . - logWordFormFreqAllCoca)
summary(unComplexBC.lm3)

# Call:
#   lm(formula = bc ~ StressPattern + TransitionType + logRelFreq + 
#        LocSpeech + PrecSegDur, data = unComplex2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.08392 -0.02603 -0.00134  0.02377  0.11254 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.597866   0.023004  25.989  < 2e-16 ***
#   StressPatterns-u       -0.004585   0.008039  -0.570    0.569    
# TransitionTypesingle-C -0.047368   0.009968  -4.752 4.70e-06 ***
#   TransitionTypesingle-V -0.092191   0.011027  -8.361 4.08e-14 ***
#   logRelFreq             -0.001186   0.001297  -0.915    0.362    
# LocSpeech              -0.008009   0.001282  -6.247 4.15e-09 ***
#   PrecSegDur             -0.150972   0.124745  -1.210    0.228    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03965 on 149 degrees of freedom
# Multiple R-squared:  0.5778,	Adjusted R-squared:  0.5608 
# F-statistic: 33.98 on 6 and 149 DF,  p-value: < 2.2e-16



# without WordFormFreq the model becomes slightly better (R2) and the
#other variables do not display any different behavior.

# Next we should exclude Stress

unComplexBC.lm4<-update(unComplexBC.lm3, ~ . - StressPattern)

summary(unComplexBC.lm4)


# Call:
#   lm(formula = bc ~ TransitionType + logRelFreq + LocSpeech + PrecSegDur, 
#      data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.086057 -0.026575 -0.000409  0.023652  0.110960 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.600337   0.022542  26.632  < 2e-16 ***
#   TransitionTypesingle-C -0.047930   0.009897  -4.843 3.16e-06 ***
#   TransitionTypesingle-V -0.094695   0.010092  -9.383  < 2e-16 ***
#   logRelFreq             -0.001215   0.001293  -0.940    0.349    
# LocSpeech              -0.008182   0.001243  -6.585 7.22e-10 ***
#   PrecSegDur             -0.157157   0.123993  -1.267    0.207    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03956 on 150 degrees of freedom
# Multiple R-squared:  0.5769,	Adjusted R-squared:  0.5628 
# F-statistic:  40.9 on 5 and 150 DF,  p-value: < 2.2e-16

# the model became slightly better

# Let's use the step() function to simplify this model and call the new model unComplex.reduced:

unComplex.reduced <- stepAIC(unComplexBC.lm4)

# Start:  AIC=-1001.87
# bc ~ TransitionType + logRelFreq + LocSpeech + PrecSegDur
# 
# Df Sum of Sq     RSS      AIC
# - logRelFreq      1  0.001382 0.23611 -1002.95
# - PrecSegDur      1  0.002514 0.23725 -1002.21
# <none>                        0.23473 -1001.87
# - LocSpeech       1  0.067847 0.30258  -964.26
# - TransitionType  2  0.157577 0.39231  -925.75
# 
# Step:  AIC=-1002.95
# bc ~ TransitionType + LocSpeech + PrecSegDur
# 
# Df Sum of Sq     RSS      AIC
# - PrecSegDur      1  0.002081 0.23819 -1003.59
# <none>                        0.23611 -1002.95
# - LocSpeech       1  0.068043 0.30416  -965.45
# - TransitionType  2  0.157512 0.39363  -927.22
# 
# Step:  AIC=-1003.59
# bc ~ TransitionType + LocSpeech
# 
# Df Sum of Sq     RSS      AIC
# <none>                        0.23819 -1003.59
# - LocSpeech       1  0.072766 0.31096  -964.00
# - TransitionType  2  0.169155 0.40735  -923.88

summary(unComplex.reduced)


# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.084297 -0.025824  0.000047  0.025345  0.114253 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.580904   0.015088  38.502  < 2e-16 ***
#   TransitionTypesingle-C -0.049706   0.009800  -5.072 1.13e-06 ***
#   TransitionTypesingle-V -0.096917   0.009920  -9.770  < 2e-16 ***
#   LocSpeech              -0.007540   0.001106  -6.814 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03959 on 152 degrees of freedom
# Multiple R-squared:  0.5706,	Adjusted R-squared:  0.5621 
# F-statistic: 67.33 on 3 and 152 DF,  p-value: < 2.2e-16

# Let's take a look at this:

visreg(unComplex.reduced)

# ich würde gerne die duration zurück transformieren. Dafür müsste ich die lamdate-Wurzel von bc berechenen

visreg(unComplex.reduced,trans=function(x) x^(1/lambda))



##################################################################################
#########                 Summary of the data        ##############################
###################################################################################

# For documenting the results, it is necessary to know the data distribution. That is why
#I'll summarize the data distribution here.

str(unComplex2)


# 'data.frame':	156 obs. of  63 variables:
#   $ ItemID                 : int  14 35 36 59 62 75 80 96 97 102 ...
# $ Sound.file             : Factor w/ 279 levels "sw02001","sw02012",..: 2 5 6 9 10 13 14 16 16 17 ...
# $ item.x                 : Factor w/ 185 levels "imbalance","immaculate",..: 102 91 183 126 165 85 132 116 115 105 ...
# $ PhrasalAccentSonia     : Factor w/ 3 levels "no","unclear",..: 3 3 2 3 3 2 3 2 3 3 ...
# $ AnyAccentSonia         : Factor w/ 2 levels "no","yes": 2 2 1 2 2 2 2 1 2 2 ...
# $ AccentIngo             : Factor w/ 3 levels "no","unclear",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Text                   : Factor w/ 314 levels "a a a such an unreliable animal that the American Seeing",..: 259 123 130 89 203 193 55 262 61 269 ...
# $ item                   : Factor w/ 101 levels "unable","unacademically",..: 18 7 99 42 81 1 48 32 31 21 ...
# $ PartofSpeech           : Factor w/ 5 levels "a","adv","n",..: 1 1 2 1 1 1 1 3 1 1 ...
# $ Base                   : Factor w/ 447 levels "ability","able",..: 61 16 433 146 332 2 169 114 113 73 ...
# $ PartofSpeechBase       : Factor w/ 6 levels "a","adv","n",..: 1 1 2 1 1 1 1 3 1 1 ...
# $ WordFormFreq           : num  108 1 312 185 433 ...
# $ BaseFormFreq           : num  1845 42 7435 3993 426 ...
# $ WordLemmaFreq          : num  108 1 312 174 432 ...
# $ BaseLemmaFreq          : num  380 41 7429 1137 426 ...
# $ Speaker                : int  57 49 50 25 55 221 72 18 43 229 ...
# $ Pos                    : Factor w/ 3 levels "end","mid","pause": 2 2 2 1 2 2 2 2 3 2 ...
# $ Affix                  : Factor w/ 6 levels "inLoc","inNeg",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ AffixOrth              : Factor w/ 5 levels "im","in","lely",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ MorphBound             : Factor w/ 3 levels "no","opaque",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Consonant              : Factor w/ 3 levels "l","m","n": 3 3 3 3 3 3 3 3 3 3 ...
# $ NoCons                 : Factor w/ 2 levels "single","double": 1 1 1 1 1 1 1 1 1 1 ...
# $ AbsDurCon              : num  0.1131 0.0484 0.0458 0.0685 0.1082 ...
# $ WordDur                : num  0.651 0.838 0.65 0.652 1.265 ...
# $ PrecSeg                : Factor w/ 36 levels "@","@U","{","a",..: 5 5 5 5 5 5 4 4 4 35 ...
# $ PrecSegVC              : Factor w/ 5 levels "C","v","V","VV",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ PrecSegDur             : num  0.106 0.1575 0.0632 0.1562 0.0574 ...
# $ FollSeg                : Factor w/ 42 levels "@","@U","{","3",..: 24 12 23 18 31 14 19 1 21 9 ...
# $ FollSegVC              : Factor w/ 2 levels "C","V": 1 2 1 1 1 2 1 2 2 1 ...
# $ FollSegDur             : num  0.0977 0.0984 0.0744 0.1267 0.0902 ...
# $ LocSpeech              : num  12.29 13.12 12.31 6.14 10.27 ...
# $ PreSuf                 : Factor w/ 4 levels "finalNoMorph",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ SyllPhon               : int  4 6 4 2 5 3 2 4 3 4 ...
# $ SyllAct                : int  3 5 4 2 5 3 2 4 3 4 ...
# $ NoSegWord              : int  8 11 8 4 13 5 5 8 7 9 ...
# $ DeletionMorph          : Factor w/ 5 levels "C","CV","M","N",..: 4 4 4 4 4 4 4 1 1 4 ...
# $ DeviantPronun          : Factor w/ 3 levels "C","N","Y": 2 2 2 2 2 2 2 2 2 2 ...
# $ PauseMorph             : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFormFreqAllCoca    : num  13506 703 56986 32221 4105 ...
# $ WordFormFreqAllCoca    : num  923 5 3488 960 3211 ...
# $ AffixStressLongman     : Factor w/ 4 levels "d","p","s","u": NA NA 4 1 3 4 1 3 3 3 ...
# $ AdjSyllStressLongman   : Factor w/ 3 levels "p","s","u": NA NA 1 1 3 1 1 3 3 3 ...
# $ AffixStress            : Factor w/ 2 levels "d","s": 2 1 1 1 2 1 1 2 2 2 ...
# $ AdjSyllStress          : Factor w/ 3 levels "p","s","u": 3 1 1 1 3 1 1 3 3 3 ...
# $ WordLemmaFreqAllCoca   : num  923 5 3488 902 3211 ...
# $ BaseLemmaFreqAllCoca   : num  2854 678 56986 9772 4105 ...
# $ al                     : Factor w/ 2 levels "m","y": NA NA NA NA NA NA NA NA NA NA ...
# $ SearchName             : Factor w/ 11 levels "imbp","imm","inC",..: 9 11 11 9 9 11 9 11 11 9 ...
# $ logWordFormFreq        : num  4.68 0 5.74 5.22 6.07 ...
# $ logBaseFormFreq        : num  7.52 3.74 8.91 8.29 6.05 ...
# $ logWordLemmaFreq       : num  4.68 0 5.74 5.16 6.07 ...
# $ logBaseLemmaFreq       : num  5.94 3.71 8.91 7.04 6.05 ...
# $ logWordFormFreqAllCoca : num  6.83 1.61 8.16 6.87 8.07 ...
# $ logBaseFormFreqAllCoca : num  9.51 6.56 10.95 10.38 8.32 ...
# $ logWordLemmaFreqAllCoca: num  6.83 1.61 8.16 6.8 8.07 ...
# $ logBaseLemmaFreqAllCoca: num  7.96 6.52 10.95 9.19 8.32 ...
# $ RelFreq                : num  0.32341 0.00737 0.06121 0.0923 0.78222 ...
# $ logRelFreq             : num  -1.129 -4.91 -2.793 -2.383 -0.246 ...
# $ TransitionType         : Factor w/ 3 levels "double-V","single-C",..: 2 3 2 2 2 3 2 3 3 2 ...
# $ StressPattern          : Factor w/ 2 levels "d-st","s-u": 2 1 1 1 2 1 1 2 2 2 ...
# $ logAbsDurCon           : num  -2.18 -3.03 -3.08 -2.68 -2.22 ...
# $ AbsDurConMS            : num  113.1 48.4 45.8 68.5 108.2 ...
# $ bc                     : num  0.517 0.399 0.393 0.444 0.51 ...



table(unComplex2$TransitionType)

#double-V single-C single-V 
#      23       66       67 


# Number of types for Nocons= double and NoCons= single 

unComplex2Double<- unComplex2[unComplex2$NoCons=="double",]

unComplex2Single<- unComplex2[unComplex2$NoCons=="single",]

unComplex2SingleC<- unComplex2Single[unComplex2Single$FollSegVC=="C",]

unComplex2SingleV<- unComplex2Single[unComplex2Single$FollSegVC=="V",]

unComplex2Double$item<- factor(unComplex2Double$item)

unComplex2Single$item<- factor(unComplex2Single$item)

unComplex2SingleC$item<- factor(unComplex2SingleC$item)

unComplex2SingleV$item<- factor(unComplex2SingleV$item)


levels(unComplex2Double$item)
#[1] "unknown"       "unnatural"     "unnecessarily" "unnecessary"   "unnerved"     
#[6] "unnerving"    

str(unComplex2Double$item)
#Factor w/ 6 levels "unnatural","unnecessarily",..: 2 3 4 5 5 3 3 5 5 3 ...

# 6 types for n#n

str(unComplex2Single$item)
#Factor w/ 93 levels "unable","unacademically",..: 18 7 91 41 74 1 47 31 30 21 ...

str(unComplex2SingleC$item)
# Factor w/ 51 levels "unbiased","unbolted",..: 4 49 16 32 22 7 5 11 48 43 ...

# 51 typey for n#C

str(unComplex2SingleV$item)
#Factor w/ 42 levels "unable","unacademically",..: 7 1 18 17 3 30 18 5 21 38 ...

# 42 types for n#V

#renaming levels of TransitionType (so that other people understand what this variable is about)

levels (unComplex2$TransitionType)
#[1] "double-V" "single-C" "single-V"

levels (unComplex2$TransitionType) = c("n#nV", "n#C", "n#V")

levels (unComplex2$TransitionType)
#[1] "n#nV" "n#C"  "n#V" 


#  Now - let's get the summaries of the variables which are in the final dataset


summary (unComplex2$bc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2841  0.3763  0.4146  0.4186  0.4622  0.5475 

sd (unComplex2$bc)
#[1] 0.05982435


summary (unComplex2$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   6.265   7.245   7.199   8.429   9.838 

sd (unComplex2$logWordFormFreqAllCoca)
#[1] 1.958023


summary (unComplex2$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.4950 -2.2590 -1.1250 -0.8297  0.1727  7.0980 

sd (unComplex2$logRelFreq)
#[1] 2.487161

summary (unComplex2$WordDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3314  0.5412  0.6374  0.6713  0.7954  1.2650  

sd(unComplex2$WordDur)
#[1] 0.1818422

summary (unComplex2$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01969 0.06759 0.08415 0.08692 0.10430 0.16650 


sd(unComplex2$PrecSegDur)
#[1]0.02909231

summary (unComplex2$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.136  11.370  13.130  13.230  15.170  20.570 

sd(unComplex2$LocSpeech)
#[1] 2.97994


summary (unComplex2$NoSegWord)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.000   7.000   8.000   8.596  10.000  13.000 

sd (unComplex2$NoSegWord)
#[1] 2.053412


summary (unComplex2$TransitionType)
#   n#nV     n#C     n#V 
#   23       66       67


lambda
#[1] 0.3030303


# For the ppt-presentation and the article, we need some nice graphs:

# Since we relevled one variable, we need to fit the model uncomplex.reduced again---

unComplex.reduced1<- lm (bc ~  TransitionType+ LocSpeech, data = unComplex2)
summary(unComplex.reduced1)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.084297 -0.025824  0.000047  0.025345  0.114253 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.580904   0.015088  38.502  < 2e-16 ***
#   TransitionTypen#C -0.049706   0.009800  -5.072 1.13e-06 ***
# TransitionTypen#V -0.096917   0.009920  -9.770  < 2e-16 ***
# LocSpeech         -0.007540   0.001106  -6.814 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03959 on 152 degrees of freedom
# Multiple R-squared:  0.5706,	Adjusted R-squared:  0.5621 
# F-statistic: 67.33 on 3 and 152 DF,  p-value: < 2.2e-16

# Let's create them and save them!

# png("un Loc Speech.png", units="cm", height=10, width=10, res=300, pointsize=12)
# 
 visreg(unComplex.reduced1, "LocSpeech", type= "conditional", trans=function(x) x^(1/lambda),  xlab="Word Duration in seconds", ylab="Duration of Nasal in seconds", ylim=c(0.005, 0.15),main="un-")
# 
# dev.off()
# 
# 
# png("Model 1 TransitionType.png", units="cm", height=10, width=18, res=300, pointsize=12)
# 
visreg(unComplex.reduced1, "TransitionType", type= "conditional", trans=function(x) x^(1/lambda), xlab="Number of Nasals in the Word", ylab="Duration of Nasal in seconds", ylim=c(0.005, 0.15),main="un-")
# 
# dev.off()

# I would like to know whether the difference between single-C and single-V
# is also significant - For this reason, I need to change the reference level
# for TransitionType and thus for the LM

unComplex2$TransitionType <- relevel (unComplex2$TransitionType, ref= "n#C")

# Let's fit an new LM

UnComplex.reduced2 <- lm (bc ~ TransitionType + LocSpeech, data = unComplex2)

summary(UnComplex.reduced2)

# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.084297 -0.025824  0.000047  0.025345  0.114253 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.531198   0.015457  34.366  < 2e-16 ***
#   TransitionTypen#nV  0.049706   0.009800   5.072 1.13e-06 ***
# TransitionTypen#V  -0.047212   0.006890  -6.852 1.70e-10 ***
# LocSpeech          -0.007540   0.001106  -6.814 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03959 on 152 degrees of freedom
# Multiple R-squared:  0.5706,	Adjusted R-squared:  0.5621 
# F-statistic: 67.33 on 3 and 152 DF,  p-value: < 2.2e-16


visreg(UnComplex.reduced2,trans= function(x) x^(1/lambda))



# so we can see that there is also a significant difference between 
# n#C and n#V: Before vowels consonants are shorter.


##########################################################################################
#We can summarize:    Un geminates when double consonant!!!!                             #
#                                                                                        #  
# Transition Type has an effect: nn < nC < nV                                            #
# The higher the speech rate, the shorter the nasal (WordDur has an effect: longer words #
#- longer consonant duration and NoSegWord has an effect: the more segments, the shorter #
# OR the more segments are spoken in less time, the shorter the dur                      #
# No frequency effects                                                                   #
# stress has no direct effect --> number of segments in the word better predictor        #
##########################################################################################



#################################
# Let's quickly check for interactions
######################################

# TransitionType and Speech Rate 

UnComplex.reduced2Int <- lm (bc ~ TransitionType* LocSpeech, data = unComplex2)

summary(UnComplex.reduced2Int)

# no: So there is no interaction between TransitionType and SpeechRate


# Stress and LocSpeech

UnComplex.reduced2IntStressSpeech <- lm (bc ~ TransitionType+ StressPattern* LocSpeech, data = unComplex2)


summary(UnComplex.reduced2IntStressSpeech)
# no interaction


# Speech Rate and Relative Frequency

UnComplex.reduced2IntSpeechRelF <- lm (bc ~ TransitionType+ logRelFreq* LocSpeech, data = unComplex2)


summary(UnComplex.reduced2IntSpeechRelF)

# no

###########################################
# So, we do not see an interaction: RelFreq cannot be tested with TransType,
# or should not be tested, since we do only have 5 values for doubles
########################################


# We need to change the order of levels:

levels(unComplex2$TransitionType)
#[1] "n#C"  "n#nV" "n#V" 

unComplex2$TransitionType <- relevel (unComplex2$TransitionType, ref= "n#nV")

levels(unComplex2$TransitionType)
#[1] "n#nV" "n#C"  "n#V" 



summary(unComplex.reduced)
# Call:
#   lm(formula = bc ~ TransitionType + LocSpeech, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.084297 -0.025824  0.000047  0.025345  0.114253 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.580904   0.015088  38.502  < 2e-16 ***
#   TransitionTypesingle-C -0.049706   0.009800  -5.072 1.13e-06 ***
#   TransitionTypesingle-V -0.096917   0.009920  -9.770  < 2e-16 ***
#   LocSpeech              -0.007540   0.001106  -6.814 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03959 on 152 degrees of freedom
# Multiple R-squared:  0.5706,	Adjusted R-squared:  0.5621 
# F-statistic: 67.33 on 3 and 152 DF,  p-value: < 2.2e-16


# Find out at which levels visreg draws lines

intercept=0.580904 
medianSpeech=median(unComplex2$LocSpeech)
estSpeech=-0.007540
estNC=-0.049706 
estNV=-0.096917  

#levels double
(intercept+(medianSpeech*estSpeech))^(1/lambda)
#[1] 0.0898975

EstimatedValueDouble=0.0898975

#level single C
(intercept+(medianSpeech*estSpeech)+(estNC))^(1/lambda)
#[1]0.06276657
EstimatedValueSingleC=0.06276657


#level single V
(intercept+(medianSpeech*estSpeech)+(estNV))^(1/lambda)
#[1] 0.04284936

EstimatedValueSingleV=0.04284936


#Unterschiede ausrechnen:

EstimatedValueDouble-EstimatedValueSingleC
#[1] 0.02713093

EstimatedValueDouble-EstimatedValueSingleV
#[1] 0.04704814

#Also, we should conduct an ANOVA for the paper

anova(unComplex.reduced)
# Analysis of Variance Table
# 
#                 Response: bc
#                  Df   Sum Sq  Mean Sq F value    Pr(>F)    
# TransitionType   2 0.243777 0.121888  77.781 < 2.2e-16 ***
#   LocSpeech      1 0.072766 0.072766  46.435  2.08e-10 ***
#   Residuals      152 0.238194 0.001567                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# anova

# model with only LocSpeech

LocSpeech.lm<-lm(AbsDurCon~LocSpeech, data = unComplex2)
summary(LocSpeech.lm)


# Call:
#   lm(formula = AbsDurCon ~ LocSpeech, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.048659 -0.016504 -0.003192  0.015444  0.061139 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.1247762  0.0089261  13.979  < 2e-16 ***
#   LocSpeech   -0.0048350  0.0006591  -7.335 1.18e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02445 on 154 degrees of freedom
# Multiple R-squared:  0.2589,	Adjusted R-squared:  0.2541 
# F-statistic: 53.81 on 1 and 154 DF,  p-value: 1.182e-11


# model with only TransitionTyope

TransitionType.lm<-lm(AbsDurCon~TransitionType, data = unComplex2)
summary(TransitionType.lm)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType, data = unComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039976 -0.015614 -0.003141  0.011562  0.068368 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.099653   0.004403  22.631  < 2e-16 ***
#   TransitionTypen#C -0.034577   0.005113  -6.762  2.7e-10 ***
# TransitionTypen#V -0.056180   0.005103 -11.008  < 2e-16 ***
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02112 on 153 degrees of freedom
# Multiple R-squared:  0.451,	Adjusted R-squared:  0.4438 
# F-statistic: 62.83 on 2 and 153 DF,  p-value: < 2.2e-16




# Do some plots for our paper

levels(unComplex1$TransitionType)
#[1] "double-V" "single-C" "single-V"

levels(unComplex1$TransitionType) = c("n#nV", "n#C", "n#V")

levels(unComplex1$TransitionType)
#[1] "n#nV" "n#C"  "n#V" 

# redo the model with new names

UnComplex.reduced <- lm (bc ~ TransitionType + LocSpeech, data = unComplex2)

summary(UnComplex.reduced2)


unComplex1$AbsDurConMS<-unComplex1$AbsDurCon*1000

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/duration")


png("boxUn.png", units="cm", height=10, width=7, res=300, pointsize=8)
bwplot (AbsDurConMS ~ TransitionType, unComplex1, ylab="duration in milliseconds", main="un-", ylim=c(0,180), cex.axis=5)

dev.off()



png("un- model.png", units="cm", height=8, width=14, res=300, pointsize=8)

par(mfrow=c(1,2))
visreg (UnComplex.reduced, "LocSpeech", trans= function(x) x^(1/lambda)*1000, rug=F, main="", ylab="duration in milliseconds", xlab="speech rate", cex.axis=0.8, partial=TRUE)
visreg (UnComplex.reduced, "TransitionType", trans= function(x) x^(1/lambda)*1000, rug=F, main="", ylab="duration in milliseconds", xlab="environment", cex.axis=0.8)

dev.off()


png("un- model SpeechRate.png", units="cm", height=15, width=15, res=300, pointsize=15)

par(mfrow=c(1,1))
visreg (UnComplex.reduced, "LocSpeech", trans= function(x) x^(1/lambda)*1000,main="", rug=F ,ylab="duration in milliseconds", xlab="local speech rate", ylim=c(20,130), cex.axis=0.8)

dev.off()

png("un- model NumNasal.png", units="cm", height=15, width=15, res=300, pointsize=15)
par(mfrow=c(1,1))
visreg (UnComplex.reduced, "TransitionType", trans= function(x) x^(1/lambda)*1000,rug=F, main="", ylab="duration in milliseconds", xlab="environment", ylim=c(20,130), cex.axis=0.8)
dev.off()


png("unCorpusMainEffect.png", units="cm", height=16, width=8, res=300, pointsize=11)
par(mfrow=c(1,1))

visreg (UnComplex.reduced, "TransitionType", trans= function(x) x^(1/lambda)*1000,rug=F, main="", ylab="duration in milliseconds", xlab="environment", ylim=c(0,180), cex.axis=0.9)
dev.off()

# Lastly, let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.
library(MuMIn)

options(na.action = "na.fail") 

model_ranking <- dredge(unComplexBC.lm2)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = bc ~ <64 unique rhs>, data = unComplex2)
# 
# Component models: 
#         df logLik    AICc  delta weight
# 16      5 284.44 -558.48   0.00   0.21
# 146     6 285.12 -557.68   0.80   0.14
# 126     6 284.75 -556.94   1.54   0.10
# 156     6 284.69 -556.82   1.66   0.09
# 136     6 284.52 -556.48   2.00   0.08
# 1246    7 285.58 -556.40   2.07   0.07
# 1456    7 285.31 -555.87   2.61   0.06
# 1346    7 285.14 -555.53   2.95   0.05
# 1256    7 284.99 -555.22   3.26   0.04
# 1236    7 284.98 -555.20   3.27   0.04
# 1356    7 284.76 -554.76   3.71   0.03
# 12456   8 285.75 -554.52   3.95   0.03
# 12346   8 285.70 -554.42   4.06   0.03
# 13456   8 285.33 -553.68   4.80   0.02
# 12356   8 285.19 -553.40   5.08   0.02
# 123456  9 285.86 -552.48   6.00   0.01
# 456     6 267.26 -521.95  36.52   0.00
# 56      5 265.60 -520.81  37.67   0.00
# 2456    7 267.60 -520.45  38.02   0.00
# 46      5 265.37 -520.34  38.14   0.00
# 256     6 266.20 -519.83  38.64   0.00
# 3456    7 267.28 -519.80  38.68   0.00
# 6       4 263.65 -519.03  39.45   0.00
# 356     6 265.79 -519.01  39.47   0.00
# 246     6 265.78 -518.99  39.49   0.00
# 26      5 264.32 -518.25  40.23   0.00
# 23456   8 267.61 -518.23  40.24   0.00
# 346     6 265.38 -518.20  40.28   0.00
# 2356    7 266.24 -517.72  40.75   0.00
# 36      5 263.81 -517.21  41.26   0.00
# 2346    7 265.78 -516.81  41.67   0.00
# 236     6 264.35 -516.13  42.34   0.00
# 145     5 254.06 -497.73  60.75   0.00
# 15      4 252.42 -496.58  61.90   0.00
# 1345    6 254.42 -496.27  62.21   0.00
# 1245    6 254.26 -495.96  62.51   0.00
# 135     5 253.11 -495.83  62.65   0.00
# 12345   7 254.83 -494.91  63.57   0.00
# 125     5 252.49 -494.58  63.90   0.00
# 1235    6 253.41 -494.25  64.23   0.00
# 14      4 245.26 -482.25  76.23   0.00
# 134     5 245.88 -481.35  77.12   0.00
# 124     5 245.52 -480.64  77.84   0.00
# 1234    6 246.46 -480.36  78.12   0.00
# 13      4 243.80 -479.33  79.15   0.00
# 1       3 242.58 -479.01  79.47   0.00
# 123     5 244.21 -478.02  80.46   0.00
# 12      4 242.66 -477.06  81.41   0.00
# 5       3 234.86 -463.57  94.91   0.00
# 45      4 235.42 -462.58  95.90   0.00
# 25      4 235.12 -461.98  96.50   0.00
# 35      4 234.90 -461.54  96.94   0.00
# 245     5 235.58 -460.77  97.71   0.00
# 345     5 235.57 -460.74  97.74   0.00
# 235     5 235.26 -460.12  98.36   0.00
# 2345    6 235.85 -459.14  99.34   0.00
# (Null)  2 218.50 -432.91 125.56   0.00
# 4       3 218.96 -431.77 126.71   0.00
# 2       3 218.84 -431.52 126.95   0.00
# 3       3 218.68 -431.21 127.26   0.00
# 34      4 219.34 -430.42 128.06   0.00
# 23      4 219.26 -430.25 128.23   0.00
# 24      4 219.20 -430.14 128.34   0.00
# 234     5 219.82 -429.23 129.24   0.00
# 
# Term codes: 
#   LocSpeech             logRelFreq logWordFormFreqAllCoca             PrecSegDur 
# 1                      2                      3                      4 
# StressPattern         TransitionType 
# 5                      6 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             0.5863399  0.0222484   0.0223904  26.187  < 2e-16 ***
#   LocSpeech              -0.0077503  0.0012234   0.0012326   6.288  < 2e-16 ***
#   TransitionTypen#C      -0.0486814  0.0099313   0.0100112   4.863  1.2e-06 ***
# TransitionTypen#V      -0.0950321  0.0104579   0.0105405   9.016  < 2e-16 ***
# PrecSegDur             -0.0576807  0.1056545   0.1061302   0.543    0.587    
# logRelFreq             -0.0003816  0.0009311   0.0009361   0.408    0.684    
# StressPatterns-u       -0.0015279  0.0049581   0.0049892   0.306    0.759    
# logWordFormFreqAllCoca  0.0001872  0.0009471   0.0009538   0.196    0.844    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             0.586340   0.022248    0.022390  26.187  < 2e-16 ***
#   LocSpeech              -0.007750   0.001223    0.001233   6.288  < 2e-16 ***
#   TransitionTypen#C      -0.048681   0.009931    0.010011   4.863  1.2e-06 ***
# TransitionTypen#V      -0.095032   0.010458    0.010540   9.016  < 2e-16 ***
# PrecSegDur             -0.143782   0.124283    0.125289   1.148    0.251    
# logRelFreq             -0.001147   0.001314    0.001325   0.866    0.387    
# StressPatterns-u       -0.005204   0.008037    0.008102   0.642    0.521    
# logWordFormFreqAllCoca  0.000696   0.001727    0.001740   0.400    0.689    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                  TransitionType LocSpeech PrecSegDur logRelFreq StressPattern
# Importance:          1.00           1.00      0.40       0.33       0.29         
# N containing models:   32             32        32         32         32         
# logWordFormFreqAllCoca
# Importance:          0.27                  
# N containing models:   32     

