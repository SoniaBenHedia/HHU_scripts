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

# origuinal file contained apostrophes, Ingo eliminated them and saved the revised file as 

# set the directory, so R knows where to find a file


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/duration")
#setwd("C:/Users/sbenhedia/Documents/Geminates/Corpus studies/Data un_in_ly/R-data")

#setwd("C:/Users/Plag/Documents/Forschung/Sonia/2015-08-18")

# loading the data, naming it d, telling R that t is the seperator, and that na, NA and   are NA strings

d <- read.table("GemDataR_2016_04_25.csv", sep=",",header = T,  na.string=c("na", "", "NA"))



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



# 3 Datensätze erstellen, für jedes Morphem einen
In <- d[d$AffixOrth=="in" | d$AffixOrth=="im", ]
un <- d[d$AffixOrth== "un", ]
ly <- d[d$AffixOrth== "ly" | d$AffixOrth== "lely",]



# Let's make a datset with only complex lys + we have the problem that we have some items
# which have y as a suffix and not ly. We will also exclude those

lyComplex <- ly[ly$MorphBound=="transparent", ]
rownames(lyComplex) <- 1:nrow(lyComplex)

lyComplex <- lyComplex[lyComplex$Affix=="ly", ]
rownames(lyComplex) <- 1:nrow(lyComplex)


dim(lyComplex)
#[1] 159  50

str(lyComplex)
# 'data.frame':	159 obs. of  50 variables:
#   $ ItemID                 : int  169 365 436 449 759 4 5 6 7 9 ...
# $ item                   : Factor w/ 590 levels "actively","actually",..: 407 407 407 407 407 51 376 424 357 377 ...
# $ PartofSpeech           : Factor w/ 10 levels "a","adv","c",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Base                   : Factor w/ 447 levels "ability","able",..: 387 387 387 387 387 126 327 414 246 331 ...
# $ PartofSpeechBase       : Factor w/ 6 levels "a","adv","n",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFreq           : num  491 491 491 491 491 ...
# $ BaseFormFreq           : num  581 581 581 581 581 ...
# $ WordLemmaFreq          : num  491 491 491 491 491 ...
# $ BaseLemmaFreq          : num  562 562 562 562 562 ...
# $ Speaker                : int  259 123 27 60 168 31 17 17 26 26 ...
# $ Pos                    : Factor w/ 3 levels "end","mid","pause": 2 2 2 2 2 2 1 3 2 3 ...
# $ Affix                  : Factor w/ 6 levels "inLoc","inNeg",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ AffixOrth              : Factor w/ 5 levels "im","in","lely",..: 3 3 3 3 3 4 4 4 4 4 ...
# $ MorphBound             : Factor w/ 3 levels "no","opaque",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Consonant              : Factor w/ 3 levels "l","m","n": 1 1 1 1 1 1 1 1 1 1 ...
# $ NoCons                 : Factor w/ 2 levels "single","double": 2 2 2 2 2 2 1 2 1 1 ...
# $ AbsDurCon              : num  0.0288 0.0775 0.0733 0.0569 0.0728 ...
# $ WordDur                : num  0.289 0.623 0.383 0.376 0.366 ...
# $ PrecSeg                : Factor w/ 36 levels "@","@U","{","a",..: 2 13 13 2 23 28 8 23 27 27 ...
# $ PrecSegVC              : Factor w/ 5 levels "C","v","V","VV",..: 3 4 4 3 3 1 1 3 1 1 ...
# $ PrecSegDur             : num  0.1205 0.0872 0.1787 0.1375 0.0789 ...
# $ FollSeg                : Factor w/ 42 levels "@","@U","{","3",..: 21 20 20 20 21 20 21 20 20 20 ...
# $ FollSegVC              : Factor w/ 3 levels "C","pause","V": 3 3 3 3 3 3 3 3 3 3 ...
# $ FollSegDur             : num  0.0595 0.3537 0.1145 0.0623 0.1018 ...
# $ LocSpeech              : num  13.86 6.42 10.45 10.64 10.93 ...
# $ PreSuf                 : Factor w/ 4 levels "finalNoMorph",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ SyllPhon               : int  2 2 2 2 2 4 3 3 2 3 ...
# $ SyllAct                : int  2 2 2 2 2 3 3 2 2 3 ...
# $ NoSegWord              : int  4 4 4 4 4 7 7 4 5 8 ...
# $ DeletionMorph          : Factor w/ 5 levels "C","CV","M","N",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ DeviantPronun          : Factor w/ 3 levels "C","N","Y": 2 2 2 2 2 2 2 2 2 2 ...
# $ PauseMorph             : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFormFreqAllCoca    : num  6601 6601 6601 6601 6601 ...
# $ WordFormFreqAllCoca    : num  5845 5845 5845 5845 5845 ...
# $ AffixStressLongman     : Factor w/ 4 levels "d","p","s","u": 4 4 4 4 4 4 4 4 4 4 ...
# $ AdjSyllStressLongman   : Factor w/ 3 levels "p","s","u": 1 1 1 1 1 3 3 3 1 1 ...
# $ AffixStress            : Factor w/ 4 levels "d","p","s","u": 4 4 4 4 4 4 4 4 4 4 ...
# $ AdjSyllStress          : Factor w/ 3 levels "p","s","u": 1 1 1 1 1 3 3 3 1 1 ...
# $ WordLemmaFreqAllCoca   : num  5845 5845 5845 5845 5845 ...
# $ BaseLemmaFreqAllCoca   : num  5974 5974 5974 5974 5974 ...
# $ al                     : Factor w/ 2 levels "m","y": 1 1 1 1 1 2 1 2 1 1 ...
# $ SearchName             : Factor w/ 11 levels "imbp","imm","inC",..: 6 6 6 6 6 7 8 7 8 8 ...
# $ logWordFormFreq        : num  6.2 6.2 6.2 6.2 6.2 ...
# $ logBaseFormFreq        : num  6.36 6.36 6.36 6.36 6.36 ...
# $ logWordLemmaFreq       : num  6.2 6.2 6.2 6.2 6.2 ...
# $ logBaseLemmaFreq       : num  6.33 6.33 6.33 6.33 6.33 ...
# $ logWordFormFreqAllCoca : num  8.67 8.67 8.67 8.67 8.67 ...
# $ logBaseFormFreqAllCoca : num  8.79 8.79 8.79 8.79 8.79 ...
# $ logWordLemmaFreqAllCoca: num  8.67 8.67 8.67 8.67 8.67 ...
# $ logBaseLemmaFreqAllCoca: num  8.7 8.7 8.7 8.7 8.7 ...

# We create the variable RelFreq

lyComplex$RelFreq <- lyComplex$WordLemmaFreqAllCoca / lyComplex$BaseLemmaFreqAllCoca
lyComplex$logRelFreq <- log(lyComplex$WordLemmaFreqAllCoca / lyComplex$BaseLemmaFreqAllCoca)

# now we have 51 variables. Let's take a look at the data distribution (duration)

densityplot(lyComplex$AbsDurCon)
densityplot(log(lyComplex$AbsDurCon))
plot(sort(lyComplex$AbsDurCon))
plot(sort(log(lyComplex$AbsDurCon)))
plot(lyComplex$AbsDurCon)

# There are a few items which have a really long or a really short l-duration. Which are these
# items?

outliers1 <- lyComplex [lyComplex$AbsDurCon > 0.1|lyComplex$AbsDurCon < 0.005 ,]

outliers1
# 6   10     occasionally          adv     occasional                a         1067          449
# 79   355  internationally          adv  international                a          542        13942
# 86   374         secondly          adv         second                a         2074        25458
# 107   479 indiscriminately          adv indiscriminate                a           73           85

# We can see that for the first outlier, the whole suffix is deleted. 
#For the second the follwing vowel is deleted which might have led to a longer duration
#there is nothing which explains the long duiration for secondly
# in the third case the praat textgriod showed that the boundary was set in the wrong place,
# thus we need to remeasure!
# the consonant duration for l is 0.045712500000000045
# the preceding segment duration is 0.08752083333333349
# let's change that in the dataset



lyComplex[c(107), c( "AbsDurCon")]
#[1] 0.103275


lyComplex[c(107), c( "AbsDurCon")]<- 0.045712500000000045

lyComplex[c(107), c( "AbsDurCon")]
#[1] 0.0457125


lyComplex[c(107), c( "PrecSegDur")]
#[1]  0.02995833

lyComplex[c(107), c( "PrecSegDur")]<-  0.08752083333333349

lyComplex[c(107), c( "PrecSegDur")]
#[1] 0.08752083

# Thus, we should exclude two items (6, 79)
lyComplex1 <-lyComplex [-c (6, 79),]

dim(lyComplex1)
#[1] 157  52



densityplot(lyComplex1$AbsDurCon)
densityplot(log(lyComplex1$AbsDurCon))
plot(sort(lyComplex1$AbsDurCon))
plot(sort(log(lyComplex1$AbsDurCon)))

# It looks much better now - there is this one long duration l. Maybe we have to exclude it
# later, but for now I do not see any reason why it has such a long duration - thus we leave it
# in the dataset...

# Furthermore we can see that we have almost a binary distribution. We should keep that in mind!

# There are a few items in which Parts of the affix are deleted. 
#Let's take a look at them:

lyComplex1 [lyComplex1$DeletionMorph=="C"| lyComplex1$DeletionMorph=="CV" | lyComplex1$DeletionMorph=="V", ]

# We need to leave this data out! If there is no l in the affix, we cannot measure its duration!
# These are three items (+ 2 we deleted before)

lyComplex2 <- lyComplex1[lyComplex1$DeletionMorph=="N",]

dim(lyComplex2)
#[1] 154  52

# We lost 3 items

# Let's create a datset which only includes doubles and one which only includes single

lyComplex2Double <- lyComplex2[lyComplex2$NoCons=="double", ]
lyComplex2Single <- lyComplex2[lyComplex2$NoCons=="single", ]

# We do look at the distributions of these items

densityplot(lyComplex2Double$AbsDurCon)
# there seem to be two different types of data: binary distribution

densityplot(lyComplex2Single$AbsDurCon)
# does not look quiete as much like a binary dstribtion

# If we look at the duartions however, it is intesrting to see that the "long"
#doubles are not longer than the singletons

# Another way to look at the density plots for the two categories is by
# using the formula notation that is used in many functions from the
# lattice package :
densityplot(~AbsDurCon|NoCons, data = lyComplex2)



#############################################################################
#                                                                           #
########  Let's look at our variables                     ###################
#                                                                           #
#############################################################################


# Problem is, that there aren't too many observations in lyComplex. That 
# means that we have to be super-careful with regard to the number of 
# predictors, otherwise we end up with serious overfitting issues.

# We will now look at each preditor variable in order to decide which one to include.
# Variables are in included if there is a theoretical reason to include them, it is
# statistically unproblematic to include them (levels are not too small).

# Covariates are included if an effect is observable. 
#Variables of iterest are all included.



# 1.                       Partofspeech


lyComplex2$PartofSpeech<-droplevels(lyComplex2$PartofSpeech)
plot(lyComplex2$AbsDurCon~lyComplex2$PartofSpeech)

table (lyComplex2$PartofSpeech)

#a adv 
#7 147 

# There is no point in including this as a predictor in a model.
# (a)the levels are too 'small' 
# (b) there is no good theoretical reason why this should matter in the
# first place.
# (c) we do not see a difference between the levels


# 2.                 NoCons (number of consonants)

table(lyComplex2$NoCons)
#single double 
#73     81 

plot (lyComplex2$AbsDurCon ~ lyComplex2$NoCons)

# there does not seem to be any difference between single and double
# consonants. However we are interested in exactly this aspect, so we
# must include this factor.


#3.                  Frequency effects

# We have a bunch if different frequency variables. We have frequencues from the 
# spoken part of COCA and the whole COCA corpus (AllCoca)

# Form Frequencies for base and word:

#logWordFormFreq  logBaseFormFreq logWordFormFreqAllCoca  logBaseFormFreqAllCoca

# Lemma Frequencies for base and word:

# WordLemmaFreqAllCoca  BaseLemmaFreqAllCoca logWordLemmaFreq  logBaseLemmaFreq


# and relative frequencies

# We will now take a look at the correlations between the different variables to 
# to decide which ones to include

pairscor.fnc(lyComplex2 [, c("logWordFormFreq", "logBaseFormFreq", "logWordLemmaFreq", "logBaseLemmaFreq", "logRelFreq","logWordFormFreqAllCoca", "logBaseFormFreqAllCoca", "logWordLemmaFreqAllCoca", "logBaseLemmaFreqAllCoca")])

# the pairscor plot shows 
#          - that the word form and lemma frequencies are 
#         almost the same (which is due to the lemmatizer used in COCA). So, it
#          doesn't make any sense to include both of them. We will include Form 
#         - that it does not seem to make a difference whether we use frequencies taken from
#           the spoken part of COCA or the whole corpus. They highly correlate.
#          I decide to use the word form frequencies from the whole corpus due to 
#           size of of corpus (bigger), so we have less zero-frequencies 
#           (in this case 1 cause we added 1 to the frequencies to get rid of 0)
#         - that logRelFreq is related to word and base frequencies 
#          (logWordFormFreqAllCoca and logBaseFormFreqAllCoca), which isn't surprising because
#           these two variables were used to calculate RelFreq in the first place.
#           So, these three variables need some attention...


#Let's take a closer look at the base form and the word form frequency: Which should
# we include?

plot (lyComplex2$logBaseFormFreq ~lyComplex2$logWordFormFreq)

# There is a high correlation and therefore we should just include one frequency variable.


plot (lyComplex2$logWordFormFreq, lyComplex2$AbsDurCon)
plot (lyComplex2$logBaseFormFreq, lyComplex2$AbsDurCon)

# None of these plots indicate an extremely strong effect on 
# the absolute duration. This probably means that any decision that we
# make won't affect the model to a large degree.

# So: We decide to use logWordFormFreq, because we assume that
# in a very frequent word form, the segments are shorter!


# Let's also look at logRelFreq since we are interested in this variable
# on theoretical grounds

plot (lyComplex2$logRelFreq, lyComplex2$AbsDurCon)

# There does not seem to be a high correlation. However I would like
# to include this factor this factor anyways since I am intersted on
# it on a theoretical base.

#4.                  Pos

table (lyComplex2$Pos)
#   end   mid pause 
#  6   124    24 


plot (lyComplex2$AbsDurCon ~ lyComplex2$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. 

#5.               WordDur

plot (density(lyComplex2$WordDur))
plot (lyComplex2$AbsDurCon ~ lyComplex2$WordDur)


#There seems to be a relation
# between the overall duration of a word (perhaps due to speech rate?, 
# or due to phrase-final lengthening?) and the duration of the consonants.
# The longer a word is, the longer are its segments pronounced.
# We might include this factor, however due to collinearity problems with trhe number
# of units (segments or syllables) in word, we might use the factor speech rate instead

#7.         PrecSegDur

plot (density(lyComplex2$PrecSegDur))
plot(lyComplex2$PrecSegDur)


plot (lyComplex2$AbsDurCon ~ lyComplex2$PrecSegDur)

# The plot doesn't suggest anything remarkable, but we include the duration
# of the preceding segments anyway, because of other studies (such as Oh &
# Redford) have emphasized the interplay between the preceding vowel 
# duration and the geminate duration. 

# PrecSegDur and WordDur could however show a collinearity effect. We should
#check for this.

plot (lyComplex2$WordDur ~ lyComplex2$PrecSegDur)
# I am not quite sure whether there is indeed some correlation. Let's check
# with a test.

cor.test (lyComplex2$PrecSegDur, lyComplex2$WordDur)

# Pearson's product-moment correlation
# 
# data:  lyComplex2$PrecSegDur and lyComplex2$WordDur
# t = 1.743, df = 152, p-value = 0.08336
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.01859133  0.29168387
# sample estimates:
# cor 
# 0.1399809 

# The correlation is really small so we do not have to worry about it.


#8.                      PrecSegVC 

lyComplex2$PrecSegVC<-droplevels(lyComplex2$PrecSegVC)

table (lyComplex2$PrecSegVC)
#  C   v   V  VV 
#109   3  38   4 


levels (lyComplex2$PrecSegVC)
#[1] "C"  "v"  "V"  "VV" "z" 

levels(lyComplex2$PrecSegVC) = c("C", "V", "V", "V")

levels (lyComplex2$PrecSegVC)
#[1] "C"  "V" 


# Let's look at the distribution

table (lyComplex2$PrecSegVC)
#C   V 
#109  45

plot (lyComplex2$AbsDurCon~lyComplex2$PrecSegVC)

# We include this factor since it seems like it might have an influence on
# the consonant duration in the suffix

# However, there should be a relatiob between PrecSegDur and PrecSegVC

plot (lyComplex2$PrecSegDur~lyComplex2$PrecSegVC)
# it looks like consonants are pronounced with a longer duration. This might be
# due to the fact that we often find reduced vowels in the items (schwa)

# We should keep this in mind

# 9. FollSegDur

plot(lyComplex2$FollSegDur)
plot(lyComplex2$FollSegDur~lyComplex2$AbsDurCon)
plot(lyComplex2$FollSegDur)

# There could be a connection to the factor WordDur. Let's check

plot (lyComplex2$WordDur ~ lyComplex2$FollSegDur)

#There seems to be a positive correlation. Let's check.

cor.test (lyComplex2$FollSegDur, lyComplex2$WordDur)

# Pearson's product-moment correlation
# 
# data:  lyComplex2$FollSegDur and lyComplex2$WordDur
# t = 6.7696, df = 152, p-value = 2.638e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3497709 0.5942314
# sample estimates:
# cor 
# 0.4813058 


# We ignore FollSegDur, because that's sort of included in WordDur anyways, 
# and we can't see any connection to AbsDurCon.


##  10        . Word Length

# Next, we have a bunch of variables that are all closely related to each
# other and are related to word length
#, which means that we certainly do not want to include all of them:
#


#    10.1. Local speech rate

# I think Locspeech is a factor which we do not necessariliy include in the 
# model, since it is calclated by WordDur/NoSegWord. If we decide to include
# both of these variables + check their interactions we sort of
# included LocSpeech. Nevertheless, let's take a look at this factor

plot(density(lyComplex2$LocSpeech))

plot(lyComplex2$AbsDurCon~lyComplex2$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables. However, we saw in our other models that this factor might be of 
# great importance. Also, we might not be able to see teh effect because NoSegWord
# and WorDur have contrary effects.

# Thus, we should keep in mind the option to include this variable instead
# of WordDur and NoSegWord!

#      10.2. SyllPhon and SyllAct

mosaicplot (lyComplex2$SyllPhon ~ lyComplex2$SyllAct)
table(lyComplex2$SyllPhon, lyComplex2$SyllAct)

#   2  3  4  5  6
#2 24  2  0  0  0
#3 11 30  3  0  0
#4  3 11 18  0  0
#5  0  3 22 11  0
#6  0  0  2 11  2
#7  0  0  0  0  1

cor.test(lyComplex2$SyllPhon, lyComplex2$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  lyComplex2$SyllPhon and lyComplex2$SyllAct
# S = 85404.07, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8596909 


# Both are fairly similar, but SyllAct (the actual number of syllables) 
# seems to be more plausible.

# But there is a relation between SyllAct and NoSegWords

#          10.3. NoSegWord

# How is it related to SyllAct?

cor.test(lyComplex2$NoSegWord, lyComplex2$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  lyComplex2$NoSegWord and lyComplex2$SyllAct
# S = 61286.77, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8993128 


# Strong correlation! We should only include one!
# I'm in favor of NoSegWord since it is the more 
# fine-grained variable


# How is it related to WordDur?

plot(lyComplex2$NoSegWord, lyComplex2$WordDur)
cor.test (lyComplex2$NoSegWord, lyComplex2$WordDur)
# 
# Pearson's product-moment correlation
# 
# data:  lyComplex2$NoSegWord and lyComplex2$WordDur
# t = 13.9822, df = 152, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.6715706 0.8119064
# sample estimates:
# cor 
# 0.7500626


# So, there is, as expected, a rather strong correlation between the 
# number of segments and the duration of the word. This means that
# the model might show some unwanted effects of collinearity.

# Possible solutions:
# - Residualize/orthogonalize?
# - Principal component analyComplexsis?
# - Ignore?
# - Use onlyComplex one of the two?

#We just use the variable LocSpeech which combines the two measurments


#             12. Stress

lyComplex2$AffixStress<-droplevels(lyComplex2$AffixStress)

table (lyComplex2$AffixStress)
#u 
#154

# ly ist immer unstressed!

lyComplex2$AdjSyllStress<-droplevels(lyComplex2$AdjSyllStress)

table (lyComplex2$AdjSyllStress)
#p   u 
#35 119

# Let's have a look at the items

unique(lyComplex2[lyComplex2$AdjSyllStress=="p","item"])
# [1] nicely       precisely    carefully    calmly       lovely       monthly      greatly     
# [8] nightly      earthly      badly        dearly       oddly        roughly      solely      
# [15] facedly      heartedly    actively     bluntly      incorrectly  frightfully  swiftly     
# [22] jointly      subtly       manly        briskly      distinctly   logistically dryly       
# [29] unfriendly   really       actually 

unique(lyComplex2[lyComplex2$AdjSyllStress=="u","item"])
# [1] eventually       possibly         totally          typically        economically    
# [6] financially      particularly     hopefully        originally       frequently      
# [11] inordinately     vociferously     constitutionally traditionally    illegally       
# [16] historically     socially         luckily          naturally        previously      
# [21] secretly         dramatically     mentally         definitely       faithfully      
# [26] excessively      agriculturally   recreationally   publicly         amazingly       
# [31] spiritually      ridiculously     partially        threateningly    formerly        
# [36] blatantly        emotionally      musically        electronically   inevitably      
# [41] mathematically   gainfully        successfully     electrically     ironically      
# [46] aerobically      unofficially     unacademically   globally         unbelievably    
# [51] tastefully       temporarily      uproariously     progressively    emphatically    
# [56] confidently      happily          grudgingly       cohesively       unconditionally 
# [61] secondly         theoretically    logically        annually         rightfully      
# [66] selfishly        evenly           feverishly       retroactively    truthfully      
# [71] fundamentally    structurally     alternately      physiologically  prominently     
# [76] fantastically    stereotypically  indiscriminately psychologically  tediously       
# [81] contractually    securely         allegedly        willfully        temporally      
# [86] commercially     anonymously      unconsciously    strategically    proportionally  
# [91] earnestly        generically      wishfully        statistically    horrifyingly    
# [96] conscientiously  aesthetically    horizontally     therapeutically  ravenously      
# [101] educationally    provisionally    nutritionally    optimistically   painlessly      
# [106] painfully        unquestionably   conceivably      ecologically     vigorously      
# [111] philosophically  responsibly      regionally       sequentially     normally        
# [116] professionally   elderly          finally          especially      

plot (lyComplex2$AbsDurCon ~ lyComplex2$AdjSyllStress)
# there does not seem to be a difference

# We do not see any difference. However, let's just include this factor
# since we saw that in the prefixed words adjacent syllable stress had
# an effect on consonant duration

############################################################################
#           The binary distribution                                        #
############################################################################

# As stated above, it seems like that we have a binary distribution. Let's
# see which lys are the "long" ones by creating a dataset in which only those
# are

# Let's look at the distribution again

densityplot(lyComplex2$AbsDurCon)
plot(sort(lyComplex2$AbsDurCon))
abline(h=c(0.07), lty="dotted")

# The long lys are longer than 0.07

lylong <- lyComplex2[lyComplex2$AbsDurCon>0.07,]

# What do all of these have in common?

# One factor we did not account for up untill now could be that in some 
#cases -ly is added to the suffix al, e.g. econnomically : maybe this has
# an influence. We should code for this and see whether this is the case


#  13. al

levels(lyComplex2$al)
#[1] "m" "y"


# We shouls rename m into no and y in yes

levels(lyComplex2$al)= c("no", "yes")

levels(lyComplex2$al)
#[1] "no"  "yes"

table (lyComplex2$al)

#no yes 
#91  63 



plot(lyComplex2$AbsDurCon~lyComplex2$al)

# There is no indication that the presence of al has an effect on the duration of ly

# However, the string al is interstung because some of the words in which ly is preceded
# by al might be realized with a syllabic /l/. This is the case if the preceding vowel
# is deleted and we have a consonant instead.

# Let's get those words:

lyComplex2[lyComplex2$al=="yes" & lyComplex2$PrecSegVC=="C", "item" ]

# We can create a new factor syllabicity

lyComplex2$syllabicity<-factor(paste(lyComplex2$al,lyComplex2$PrecSegVC, sep="-"))

levels(lyComplex2$syllabicity)
#[1] "no-C"  "no-V"  "yes-C" "yes-V"

# the yes-C words contain a syllabic l


levels(lyComplex2$syllabicity)=c("no", "no", "yes", "no")

levels(lyComplex2$syllabicity)
#[1] "no"  "yes"

# let's have a closer look at this factor

table(lyComplex2$syllabicity)
#no yes 
#106  48 

plot(lyComplex2$syllabicity, lyComplex2$AbsDurCon)
# there does not seem to be a difference, but other factors might intervene

# Also, maybe there are some relations to other variables. We need to check


# Let's take a look at the distribution between Single and double
table(lyComplex2$syllabicity,lyComplex2$NoCons)

#      single double
#no      73     33
#yes      0     48

# of course singles never have al! We should thus think about paste this factot

lyComplex2$TransitionType<-factor(paste(lyComplex2$syllabicity,lyComplex2$NoCons, sep="-"))

levels(lyComplex2$TransitionType)
#[1] "no-double"  "no-single"  "yes-double"

levels(lyComplex2$TransitionType)=c("double", "single","syllabic-double")

levels(lyComplex2$TransitionType)
#[1] "double"          "single"          "syllabic-double"

# does that have an influence?

plot(lyComplex2$AbsDurCon~lyComplex2$TransitionType)

# maybe doubles are longer than syllabic doubles

# but How does this relate to preceding Segment?

table(lyComplex2$TransitionType,lyComplex2$PrecSegVC)
#                 C  V
#double           6 27
#single          55 18
#syllabic-double 48  0


# Of course all syllabic doubles are preceded by a consonant! We need to keep this in mind
# when interpreting the results

# We of course include transitionType in our analyses

# Let's try out one other thing. What if we have a factor which combines syllabicty with PrecSegVC

lyComplex2$Environment<-factor(paste(lyComplex2$syllabicity,lyComplex2$PrecSegVC, sep="-"))

levels(lyComplex2$Environment)
#[1] "no-C"  "no-V"  "yes-C"

levels(lyComplex2$Environment)=c("PrecC", "PrecV","syllabic")

levels(lyComplex2$Environment)
#[1] "PrecC"    "PrecV"    "syllabic"

table(lyComplex2$Environment)
#PrecC    PrecV syllabic 
#61       45       48 

plot(lyComplex2$Environment,lyComplex2$AbsDurCon)
# there seems to be a difference which is due to the preceding segment. We need to keep
# this in mind!!!!!! We thus keep the variable transitionType and PrecSegVC and remember
# that the levels of TransitionType are related to PrecSegVC and one factor might surpress the other

###############################################################
#   Summary: variables to include                            ##
###############################################################

# So all in all, we came to the following conclusion:

## We are going to include the following predictors:
# - TransitionType
# - RelFreq
# - logWordFormFreqAllCoca
# - PrecSegDur
# - PrecSegVC
# - LocSpeech
# - AdjSyllStress

# All in all that are 11 levels which is okay considering the datasize of
# 154 observations

# Summary of variables in initial dataset

table(lyComplex2$TransitionType)

#double          single syllabic-double 
#33              73              48 


# Number of types 

length(unique(lyComplex2[lyComplex2$TransitionType=="double","item"]))
#[1] 29 types for double


length(unique(lyComplex2[lyComplex2$TransitionType=="syllabic-double","item"]))
#[1] 48 types for syllabic-double

length(unique(lyComplex2[lyComplex2$TransitionType=="single","item"]))
#[1] 73 types for single





summary (lyComplex2$AbsDurCon*1000)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#  5.423  27.040  41.200  43.160  55.820     111.400 



sd (lyComplex2$AbsDurCon*1000)
#[1]  21.78215


# let's get the means and stuf for eah environment

# doubles
summary(lyComplex2[lyComplex2$TransitionType=="double","AbsDurCon"]*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.312  36.700  49.710  50.470  72.620  88.560 

sd(lyComplex2[lyComplex2$TransitionType=="double","AbsDurCon"]*1000)
#[1] 22.89765


# doubles syllabic
summary(lyComplex2[lyComplex2$TransitionType=="syllabic-double","AbsDurCon"]*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.17   26.45   37.28   40.41   48.35   91.00 


sd(lyComplex2[lyComplex2$TransitionType=="syllabic-double","AbsDurCon"]*1000)
#[1] 20.83753


# doubles singles
summary(lyComplex2[lyComplex2$TransitionType=="single","AbsDurCon"]*1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  5.423  24.630  39.270  41.670  54.320 111.400 

sd(lyComplex2[lyComplex2$TransitionType=="single","AbsDurCon"]*1000)
#[1] 21.4475

summary (lyComplex2$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6931  6.1390  7.2450  7.2770  8.6730 12.5800 

sd (lyComplex2$logWordFormFreqAllCoca)
#[1] 1.94291


summary (lyComplex2$logRelFreq)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.29400 -2.15900 -1.33000 -0.64290 -0.02183  8.65600 


sd (lyComplex2$logRelFreq)
#[1]  2.721003

summary (lyComplex2$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.418  10.970  12.850  13.540  15.190  26.010 


sd(lyComplex2$LocSpeech)
#[1] 3.476689


summary (lyComplex2$PrecSegDur*1000)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  13.38   55.92   82.95   84.07  103.00  212.40 



sd(lyComplex2$PrecSegDur*1000)
#[1] 37.04758

summary (lyComplex2$PrecSegVC)
#C   V 
#109  45 


length(unique(lyComplex2[lyComplex2$PrecSegVC=="C","item"]))
#[1] 109 types for C

length(unique(lyComplex2[lyComplex2$PrecSegVC=="V","item"]))
#[1] 41 types for V



summary (lyComplex2$AdjSyllStress)
#p   u 
#35 119 

length(unique(lyComplex2[lyComplex2$AdjSyllStress=="p","item"]))
#[1] 31 types for p

length(unique(lyComplex2[lyComplex2$AdjSyllStress=="u","item"]))
#[1] 119 types for u



###############################################################
#  Note: I analyzed different decomposability measurments for ly (see other script)
# and came to the conclusion that LSA does not mirror the decomposability of ly. Thus
# it will not be included in the models! We should also be careful with RelFreq since
# it does not have an effect on the duration of ly by itself. We include it because 
# an interaction might be significant##
###############################################################

# Due to the unnestedness of the data, there is no point in including
# either Speaker or Item as a random effect. This would lead to a seriouslyComplex
# overpowered model in which almost everything can be predicted on the 
# basis of the random effect structure alone:

tmp.lmer <- lmer(AbsDurCon ~ 1 + (1|item) + (1|Speaker), data = lyComplex2)
cor(lyComplex2$AbsDurCon, fitted(tmp.lmer))^2
#[1] 0.9445149


tmp.lmer2 <- lmer(AbsDurCon ~ 1 + (1|item), data = lyComplex2)
cor(lyComplex2$AbsDurCon, fitted(tmp.lmer2))^2
#[1] 0.9445149

# Let's start modeling now!

## Do an initial model:

rownames(lyComplex2) <- 1:nrow(lyComplex2)




lyComplex.lm1 <- lm (AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + PrecSegVC + LocSpeech + PrecSegDur + AdjSyllStress, data = lyComplex2)

summary (lyComplex.lm1)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + LocSpeech + PrecSegDur + AdjSyllStress, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039141 -0.013100 -0.000998  0.011895  0.064806 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0890488  0.0118841   7.493 6.09e-12 ***
#   TransitionTypesingle          -0.0001504  0.0049432  -0.030   0.9758    
# TransitionTypesyllabic-double  0.0020500  0.0060499   0.339   0.7352    
# logRelFreq                    -0.0004151  0.0006147  -0.675   0.5005    
# logWordFormFreqAllCoca        -0.0011432  0.0008691  -1.315   0.1904    
# PrecSegVCV                     0.0095438  0.0046773   2.040   0.0431 *  
#   LocSpeech                     -0.0026282  0.0005037  -5.218 6.14e-07 ***
#   PrecSegDur                    -0.0719485  0.0470057  -1.531   0.1280    
# AdjSyllStressu                 0.0005558  0.0040899   0.136   0.8921    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01965 on 145 degrees of freedom
# Multiple R-squared:  0.2289,	Adjusted R-squared:  0.1864 
# F-statistic: 5.381 on 8 and 145 DF,  p-value: 6.198e-06



# The model does not explain much of the variation found and only two 
#factors are significant - not our variable of interest!
#instead of fitting the model now, let's first deal with
#coll. issues

#############################################################################
#           Collinearity issues                                             # 
#############################################################################

# We stated that we might encounter some coll. problems. Let's deal with them now


# 1. LogRelFreq an WordFormFreqAllCocs


lyComplex.lmRelFreq<-lm (AbsDurCon ~ logRelFreq , data = lyComplex2)

summary (lyComplex.lmRelFreq)

# Call:
#   lm(formula = AbsDurCon ~ logRelFreq, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.037152 -0.016363 -0.001842  0.011841  0.074432 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0427061  0.0018028  23.689   <2e-16 ***
#   logRelFreq  -0.0007057  0.0006468  -1.091    0.277    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02177 on 152 degrees of freedom
# Multiple R-squared:  0.00777,	Adjusted R-squared:  0.001243 
# F-statistic:  1.19 on 1 and 152 DF,  p-value: 0.277


lyComplex.lmWordFreq<-lm (AbsDurCon ~logWordFormFreqAllCoca , data = lyComplex2)

summary (lyComplex.lmWordFreq)

# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041276 -0.016133 -0.001459  0.012789  0.069695 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0561601  0.0067598   8.308 5.01e-14 ***
#   logWordFormFreqAllCoca -0.0017866  0.0008977  -1.990   0.0484 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02157 on 152 degrees of freedom
# Multiple R-squared:  0.02539,	Adjusted R-squared:  0.01898 
# F-statistic: 3.961 on 1 and 152 DF,  p-value: 0.04837


lyComplex.lmFreq<-lm (AbsDurCon ~ logRelFreq + logWordFormFreqAllCoca, data=lyComplex2)
summary (lyComplex.lmFreq)

# Call:
#   lm(formula = AbsDurCon ~ logRelFreq + logWordFormFreqAllCoca, 
#      data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041540 -0.015833 -0.001217  0.011805  0.073554 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0548823  0.0070177   7.821 8.41e-13 ***
#   logRelFreq             -0.0004555  0.0006571  -0.693   0.4892    
# logWordFormFreqAllCoca -0.0016512  0.0009202  -1.794   0.0747 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02161 on 151 degrees of freedom
# Multiple R-squared:  0.02849,	Adjusted R-squared:  0.01562 
# F-statistic: 2.214 on 2 and 151 DF,  p-value: 0.1128




#The two variables do not supress each other, so both can remain in the model untill
# they are removed in the process of slimming down the model




# 3. PrecSegVC and PrecSegDur

# Testing the effect of PrecSegDur

lyComplex.lmPrecSegDur<-lm (AbsDurCon ~ PrecSegDur , data = lyComplex2)

summary (lyComplex.lmPrecSegDur)

# lm(formula = AbsDurCon ~ PrecSegDur, data = lyComplex2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03800 -0.01614 -0.00217  0.01266  0.06814 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.043586   0.004379   9.954   <2e-16 ***
#   PrecSegDur  -0.005070   0.047687  -0.106    0.915    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02185 on 152 degrees of freedom
# Multiple R-squared:  7.437e-05,	Adjusted R-squared:  -0.006504 
# F-statistic: 0.0113 on 1 and 152 DF,  p-value: 0.9155


# Testing the effect of PrecSegVC

lyComplex.lmPrecSegVC<-lm (AbsDurCon ~ PrecSegVC , data = lyComplex2)

summary (lyComplex.lmPrecSegVC)

# Call:
#   lm(formula = AbsDurCon ~ PrecSegVC, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.044983 -0.015219 -0.002744  0.011647  0.071279 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.040168   0.002045  19.647  < 2e-16 ***
#   PrecSegVCV  0.010237   0.003782   2.707  0.00757 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02135 on 152 degrees of freedom
# Multiple R-squared:  0.04598,	Adjusted R-squared:  0.0397 
# F-statistic: 7.326 on 1 and 152 DF,  p-value: 0.007574




# Testing the effect of PrecSegVC and PrecSegDur

lyComplex.lmPrecSeg<-lm (AbsDurCon ~ PrecSegDur + PrecSegVC , data = lyComplex2)

summary (lyComplex.lmPrecSeg)

# Call:
#   lm(formula = AbsDurCon ~ PrecSegDur + PrecSegVC, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.044291 -0.015259 -0.002756  0.011533  0.071820 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.038699   0.004651   8.321  4.8e-14 ***
#   PrecSegDur  0.016688   0.047395   0.352   0.7253    
# PrecSegVCV  0.010466   0.003848   2.720   0.0073 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02141 on 151 degrees of freedom
# Multiple R-squared:  0.04676,	Adjusted R-squared:  0.03414 
# F-statistic: 3.704 on 2 and 151 DF,  p-value: 0.02689


# the effect od PrecSegDur changes effect direction when both variables are in the model
# It should thus be excluded (it is never significant)


# TransitionType and PrecSeg VC

# Testing the effect of PrecSegVC

lyComplex.lmPrecSegVC<-lm (AbsDurCon ~ PrecSegVC , data = lyComplex2)

summary (lyComplex.lmPrecSegVC)

# Call:
#   lm(formula = AbsDurCon ~ PrecSegVC, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.044983 -0.015219 -0.002744  0.011647  0.071279 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.040168   0.002045  19.647  < 2e-16 ***
#   PrecSegVCV  0.010237   0.003782   2.707  0.00757 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02135 on 152 degrees of freedom
# Multiple R-squared:  0.04598,	Adjusted R-squared:  0.0397 
# F-statistic: 7.326 on 1 and 152 DF,  p-value: 0.007574


# Testing the effect of TransitionType

lyComplex.lmTT<-lm (AbsDurCon ~ TransitionType , data = lyComplex2)

summary (lyComplex.lmTT)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.043154 -0.015536 -0.002667  0.011655  0.069783 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.050466   0.003756  13.436   <2e-16 ***
#   TransitionTypesingle          -0.008801   0.004526  -1.944   0.0537 .  
# TransitionTypesyllabic-double -0.010057   0.004879  -2.061   0.0410 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02158 on 151 degrees of freedom
# Multiple R-squared:  0.03152,	Adjusted R-squared:  0.01869 
# F-statistic: 2.457 on 2 and 151 DF,  p-value: 0.08912

# singles are shorter than doubles, as well as syllabic doubles


# Testing the effect of TransitionType and PrecSeg

lyComplex.lmTTPrecSeg<-lm (AbsDurCon ~ TransitionType + PrecSegVC, data = lyComplex2)

summary (lyComplex.lmTTPrecSeg)

# 
# Call:
#   lm(formula = AbsDurCon ~ TransitionType + PrecSegVC, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.042593 -0.015218 -0.002985  0.011976  0.071861 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.043570   0.005532   7.875 6.33e-13 ***
#   TransitionTypesingle          -0.003983   0.005327  -0.748   0.4558    
# TransitionTypesyllabic-double -0.003160   0.006340  -0.499   0.6188    
# PrecSegVCV                     0.008429   0.004990   1.689   0.0933 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02145 on 150 degrees of freedom
# Multiple R-squared:  0.04959,	Adjusted R-squared:  0.03058 
# F-statistic: 2.609 on 3 and 150 DF,  p-value: 0.05371


# both effects vanish but their effect direction does not change. Thus we can keep both
# in the model right now. Right now it seems PrecSegVC explains more variance. Let's see
# what happens ion the model

#########################################################################
#          Summary Coll                                                 #
#########################################################################

# We will include both Freq Variables, which will probably both be excluded soon
# due to their minimal effect
# We will only include PrecSegVC since PrecSegDur is never significant
# TT and PrecSegVC affect each other in that they take away the other's effect size
# They do however not change the effect direction and can thus both stay in the model for now


#########################################################################
#          Fitting a model                                              #
#########################################################################


# Now let's fit our model

lyComplex.lm2 <- lm (AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + PrecSegVC +LocSpeech+AdjSyllStress, data = lyComplex2)

summary(lyComplex.lm2)




# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + LocSpeech + AdjSyllStress, data = lyComplex2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03875 -0.01359 -0.00236  0.01263  0.06837 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0793583  0.0101035   7.855 7.98e-13 ***
#   TransitionTypesingle          -0.0001655  0.0049658  -0.033   0.9735    
# TransitionTypesyllabic-double  0.0018402  0.0060761   0.303   0.7624    
# logRelFreq                    -0.0004440  0.0006172  -0.719   0.4731    
# logWordFormFreqAllCoca        -0.0012370  0.0008709  -1.420   0.1576    
# PrecSegVCV                     0.0105769  0.0046495   2.275   0.0244 *  
#   LocSpeech                     -0.0023428  0.0004700  -4.984 1.74e-06 ***
#   AdjSyllStressu                 0.0008314  0.0041047   0.203   0.8398    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01974 on 146 degrees of freedom
# Multiple R-squared:  0.2165,	Adjusted R-squared:  0.1789 
# F-statistic: 5.763 on 7 and 146 DF,  p-value: 6.611e-06



####################################################################
#               checking assumptions                               #
####################################################################

plot(lyComplex.lm2)




qqnorm (scale(residuals (lyComplex.lm2)))
qqline (scale(residuals (lyComplex.lm2)))

#identify(qqnorm (scale(residuals (lyComplex.lm2))))
#[1]83 17


# That does not look too bad, however there are 3 outliers with high residuals, 2 of them
# also shows a high leverage

# let's exclude them
lyComplex3 <- lyComplex2[-c(83,17,59), ]

dim(lyComplex3)
#[1]  151  55

#let's refit the model

lyComplex.lm3 <- lm (AbsDurCon~ TransitionType + logRelFreq + logWordFormFreqAllCoca + PrecSegVC +AdjSyllStress+ LocSpeech, data = lyComplex3)
summary(lyComplex.lm3)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + AdjSyllStress + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041234 -0.013194 -0.000543  0.011598  0.043083 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0748083  0.0092904   8.052 2.88e-13 ***
#   TransitionTypesingle           0.0046359  0.0047352   0.979 0.329220    
# TransitionTypesyllabic-double  0.0103718  0.0058318   1.778 0.077449 .  
# logRelFreq                    -0.0011445  0.0005882  -1.946 0.053652 .  
# logWordFormFreqAllCoca        -0.0014074  0.0008038  -1.751 0.082115 .  
# PrecSegVCV                     0.0170283  0.0044583   3.819 0.000199 ***
#   AdjSyllStressu                -0.0023592  0.0038137  -0.619 0.537154    
# LocSpeech                     -0.0023143  0.0004350  -5.320 3.91e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01807 on 143 degrees of freedom
# Multiple R-squared:  0.2835,	Adjusted R-squared:  0.2484 
# F-statistic: 8.082 on 7 and 143 DF,  p-value: 2.879e-08

# model became better
# now let's check the assumptions

plot(lyComplex.lm3)

qqnorm (scale(residuals (lyComplex.lm3)))
qqline (scale(residuals (lyComplex.lm3)))

# all in all this seems okay! However, let's see whether a bc transformation would
# help us to even better meet the assumptions of an lm


#########BOx-COX

bc <- boxcox(lyComplex.lm3)
lambda <- bc$x[which.max(bc$y)]

lyComplex3$bc <- lyComplex3$AbsDurCon^lambda

lyComplexBC.lm3 <- lm (bc~ TransitionType + logRelFreq + logWordFormFreqAllCoca + PrecSegVC +AdjSyllStress+ LocSpeech, data = lyComplex3)
summary(lyComplexBC.lm3)

# Call:
#   lm(formula = bc ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + AdjSyllStress + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.114047 -0.031001  0.001608  0.031350  0.092568 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.280727   0.022992  12.210  < 2e-16 ***
#   TransitionTypesingle           0.012532   0.011719   1.069 0.286712    
# TransitionTypesyllabic-double  0.025131   0.014433   1.741 0.083792 .  
# logRelFreq                    -0.002607   0.001456  -1.791 0.075461 .  
# logWordFormFreqAllCoca        -0.003925   0.001989  -1.973 0.050442 .  
# PrecSegVCV                     0.040750   0.011034   3.693 0.000314 ***
#   AdjSyllStressu                -0.002533   0.009438  -0.268 0.788810    
# LocSpeech                     -0.005990   0.001077  -5.564 1.26e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04472 on 143 degrees of freedom
# Multiple R-squared:  0.2908,	Adjusted R-squared:  0.2561 
# F-statistic: 8.378 on 7 and 143 DF,  p-value: 1.462e-08

# model bacme a bit better (r-squared) but factors remain the same

plot(lyComplexBC.lm3)

qqnorm (scale(residuals (lyComplexBC.lm3)))
qqline (scale(residuals (lyComplexBC.lm3)))


# let's compare with the previous model


qqnorm (scale(residuals (lyComplex.lm3)))
qqline (scale(residuals (lyComplex.lm3)))

# both are okay, but I think the first one looks better. Thus we will go with the AbsDurCon

# Before going on to slim down the model we need to look for interactions

############################################################################
#          Looking for interactions                                        #
############################################################################

############################################################################################
#               Testing interactions                                                       #
############################################################################################


# But now, let's try some interactions

# Possible interactions are

# 1. TransitionType and Frequencies
#   a) Word Form
#   b) RelFreq

# 1a  TransitionType and WordFormFreq

lyComplex.lmintFormFreq <- lm (AbsDurCon~ TransitionType*logWordFormFreqAllCoca, data = lyComplex3)
summary(lyComplex.lmintFormFreq)


# no interaction!

# 1b  TransitionType and RelFreq

lyComplex.lmintRelFreq <- lm (AbsDurCon~ TransitionType*logRelFreq, data = lyComplex3)
summary(lyComplex.lmintRelFreq)


# no interaction!

# 1c  TransitionType and LocSpeech

lyComplex.lmintSpeech <- lm (AbsDurCon~ TransitionType*LocSpeech, data = lyComplex3)
summary(lyComplex.lmintSpeech)

# no interaction!

# 1d  TransitionType and StressPattern

lyComplex.lmintStress <- lm (AbsDurCon~ TransitionType* AdjSyllStress, data = lyComplex3)
summary(lyComplex.lmintStress)

# no interaction!

#######################################################################
#         Summary interactions
# we do not have any!
# tested interactions:
# TT and Speech Rate
# TT and both Frequencies
# TT and Stress
#######################################################################

# now we can go on slimming down the model

############################################################################
#           Slim down the model                                            #
############################################################################



summary(lyComplex.lm3)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + AdjSyllStress + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041234 -0.013194 -0.000543  0.011598  0.043083 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0748083  0.0092904   8.052 2.88e-13 ***
#   TransitionTypesingle           0.0046359  0.0047352   0.979 0.329220    
# TransitionTypesyllabic-double  0.0103718  0.0058318   1.778 0.077449 .  
# logRelFreq                    -0.0011445  0.0005882  -1.946 0.053652 .  
# logWordFormFreqAllCoca        -0.0014074  0.0008038  -1.751 0.082115 .  
# PrecSegVCV                     0.0170283  0.0044583   3.819 0.000199 ***
#   AdjSyllStressu                -0.0023592  0.0038137  -0.619 0.537154    
# LocSpeech                     -0.0023143  0.0004350  -5.320 3.91e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01807 on 143 degrees of freedom
# Multiple R-squared:  0.2835,	Adjusted R-squared:  0.2484 
# F-statistic: 8.082 on 7 and 143 DF,  p-value: 2.879e-08


# the first factor we can throw out is AdjSyllStree


lyComplex.lm4 <- update(lyComplex.lm3, ~ . - AdjSyllStress)


summary(lyComplex.lm4)
# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + logWordFormFreqAllCoca + 
#        PrecSegVC + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.040710 -0.012639 -0.001403  0.011685  0.044532 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0727538  0.0086579   8.403 3.81e-14 ***
#   TransitionTypesingle           0.0046561  0.0047249   0.985  0.32606    
# TransitionTypesyllabic-double  0.0096284  0.0056944   1.691  0.09303 .  
# logRelFreq                    -0.0011721  0.0005853  -2.003  0.04709 *  
#   logWordFormFreqAllCoca        -0.0013070  0.0007856  -1.664  0.09835 .  
# PrecSegVCV                     0.0166655  0.0044101   3.779  0.00023 ***
#   LocSpeech                     -0.0023281  0.0004335  -5.371 3.07e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01803 on 144 degrees of freedom
# Multiple R-squared:  0.2815,	Adjusted R-squared:  0.2516 
# F-statistic: 9.405 on 6 and 144 DF,  p-value: 1.051e-08

# We should throw out one of the Freq variables. RelFreq is sign. WordFormFreq is not. However
# we saw before that they affect each other. Let's test throwing out both .

# First RelFreq

lyComplex.lm5 <- update(lyComplex.lm4, ~ . - logRelFreq)


summary(lyComplex.lm5)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logWordFormFreqAllCoca + 
#        PrecSegVC + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039228 -0.013135 -0.000586  0.012467  0.044022 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0773230  0.0084382   9.163 4.45e-16 ***
#   TransitionTypesingle           0.0039653  0.0047610   0.833 0.406284    
# TransitionTypesyllabic-double  0.0073865  0.0056410   1.309 0.192458    
# logWordFormFreqAllCoca        -0.0016310  0.0007767  -2.100 0.037461 *  
#   PrecSegVCV                     0.0156936  0.0044286   3.544 0.000531 ***
#   LocSpeech                     -0.0023351  0.0004379  -5.332 3.65e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01822 on 145 degrees of freedom
# Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2361 
# F-statistic: 10.27 on 5 and 145 DF,  p-value: 1.918e-08

# now WordFormFreq


lyComplex.lm6 <- update(lyComplex.lm4, ~ . - logWordFormFreqAllCoca)


summary(lyComplex.lm6)



# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logRelFreq + PrecSegVC + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.040417 -0.012908 -0.001661  0.012376  0.046882 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0646994  0.0072215   8.959 1.47e-15 ***
#   TransitionTypesingle           0.0046865  0.0047536   0.986 0.325830    
# TransitionTypesyllabic-double  0.0096542  0.0057290   1.685 0.094112 .  
# logRelFreq                    -0.0013727  0.0005762  -2.382 0.018505 *  
#   PrecSegVCV                     0.0162007  0.0044280   3.659 0.000354 ***
#   LocSpeech                     -0.0024341  0.0004314  -5.643 8.50e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01814 on 145 degrees of freedom
# Multiple R-squared:  0.2677,	Adjusted R-squared:  0.2425 
# F-statistic:  10.6 on 5 and 145 DF,  p-value: 1.076e-08

# it seem RelFreq is the better predictor. But if it is in a model on its own, it is
# not significant

summary(lyComplex.lmRelFreq)

# Call:
#   lm(formula = AbsDurCon ~ logRelFreq, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.037152 -0.016363 -0.001842  0.011841  0.074432 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0427061  0.0018028  23.689   <2e-16 ***
#   logRelFreq  -0.0007057  0.0006468  -1.091    0.277    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02177 on 152 degrees of freedom
# Multiple R-squared:  0.00777,	Adjusted R-squared:  0.001243 
# F-statistic:  1.19 on 1 and 152 DF,  p-value: 0.27

#whereas logWordFormFreq is!

summary(lyComplex.lmWordFreq)

# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca, data = lyComplex2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041276 -0.016133 -0.001459  0.012789  0.069695 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0561601  0.0067598   8.308 5.01e-14 ***
#   logWordFormFreqAllCoca -0.0017866  0.0008977  -1.990   0.0484 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02157 on 152 degrees of freedom
# Multiple R-squared:  0.02539,	Adjusted R-squared:  0.01898 
# F-statistic: 3.961 on 1 and 152 DF,  p-value: 0.04837

# which is why I will keep WordFormFreq and NOT RelFreq.
# Thus this is our current model


summary(lyComplex.lm5)


# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logWordFormFreqAllCoca + 
#        PrecSegVC + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039228 -0.013135 -0.000586  0.012467  0.044022 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0773230  0.0084382   9.163 4.45e-16 ***
#   TransitionTypesingle           0.0039653  0.0047610   0.833 0.406284    
# TransitionTypesyllabic-double  0.0073865  0.0056410   1.309 0.192458    
# logWordFormFreqAllCoca        -0.0016310  0.0007767  -2.100 0.037461 *  
#   PrecSegVCV                     0.0156936  0.0044286   3.544 0.000531 ***
#   LocSpeech                     -0.0023351  0.0004379  -5.332 3.65e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01822 on 145 degrees of freedom
# Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2361 
# F-statistic: 10.27 on 5 and 145 DF,  p-value: 1.918e-08


# Thus now we are left with 4 factors: LocSpecch, WordFormFreq  and TransitionType and PrecSegVC. Since we said the latter two take away
# each others effects, let's see what happens if we throw out each



lyComplex.lm6a <- update(lyComplex.lm5, ~ . - TransitionType)


summary(lyComplex.lm6a)


# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca + PrecSegVC + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039948 -0.013224 -0.001209  0.012831  0.045430 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0813094  0.0076418  10.640  < 2e-16 ***
#   logWordFormFreqAllCoca -0.0015840  0.0007752  -2.043 0.042798 *  
#   PrecSegVCV              0.0119181  0.0032755   3.639 0.000379 ***
#   LocSpeech              -0.0022625  0.0004322  -5.235 5.61e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0182 on 147 degrees of freedom
# Multiple R-squared:  0.2524,	Adjusted R-squared:  0.2371 
# F-statistic: 16.54 on 3 and 147 DF,  p-value: 2.59e-09



visreg(lyComplex.lm6a)



lyComplex.lm6b <- update(lyComplex.lm5, ~ . - PrecSegVC)


summary(lyComplex.lm6b)
# Call:
#   lm(formula = AbsDurCon ~ TransitionType + logWordFormFreqAllCoca + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041026 -0.013500 -0.003138  0.013310  0.049628 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0876195  0.0082297  10.647  < 2e-16 ***
#   TransitionTypesingle          -0.0056044  0.0040732  -1.376   0.1710    
# TransitionTypesyllabic-double -0.0059695  0.0043600  -1.369   0.1731    
# logWordFormFreqAllCoca        -0.0013876  0.0008037  -1.727   0.0864 .  
# LocSpeech                     -0.0022424  0.0004541  -4.938 2.13e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01892 on 146 degrees of freedom
# Multiple R-squared:  0.1976,	Adjusted R-squared:  0.1756 
# F-statistic: 8.988 on 4 and 146 DF,  p-value: 1.619e-06


visreg(lyComplex.lm6b)

# None of the effects of TransitionType is sign!
#doubles are longer when they are not syllabic THUS here we have most doubles preceded
# by a vowel, whereas all the syllabic doubles are preceded by a consonant. The singles are in 
# between.
#Here we have vocalic and consonantal precseding segments. Thus, the "effect"trend" we see here
# might really be an effect of the preceding segment, especially considering that it changes when
# we include PrecSegVC. 
#We have a smaller R-square than
#when we use Preceding Segment as a variable. 

# However,let' ss see what happens if we combine the two


lyComplex3$Transition <- factor(paste (lyComplex3$PrecSegVC,lyComplex3$TransitionType, sep="-"))
levels(lyComplex3$Transition)

#[1] "C-double"          "C-single"          "C-syllabic-double" "V-double"          "V-single"         

# let's try out what happens if we include this in our model


lyComplex.lm7 <- lm(AbsDurCon~ Transition + logWordFormFreqAllCoca +LocSpeech, data = lyComplex3)


summary(lyComplex.lm7)

# Call:
#   lm(formula = AbsDurCon ~ Transition + logWordFormFreqAllCoca + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.04042 -0.01314 -0.00050  0.01277  0.04438 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  0.0812977  0.0106150   7.659 2.51e-12 ***
#   TransitionC-single          -0.0004406  0.0085646  -0.051   0.9590    
# TransitionC-syllabic-double  0.0033513  0.0086249   0.389   0.6982    
# TransitionV-double           0.0109028  0.0089167   1.223   0.2234    
# TransitionV-single           0.0168077  0.0093630   1.795   0.0747 .  
# logWordFormFreqAllCoca      -0.0016133  0.0007789  -2.071   0.0401 *  
#   LocSpeech                   -0.0023400  0.0004389  -5.331 3.69e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01826 on 144 degrees of freedom
# Multiple R-squared:  0.2635,	Adjusted R-squared:  0.2328 
# F-statistic: 8.587 on 6 and 144 DF,  p-value: 5.53e-08

# no sign. differences!

visreg(lyComplex.lm7)

# the plot shows that precedingV and precedingV pattern together.

# Thus our final model is


final.lm<-lyComplex.lm6a

summary(final.lm)


# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca + PrecSegVC + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039948 -0.013224 -0.001209  0.012831  0.045430 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0813094  0.0076418  10.640  < 2e-16 ***
#   logWordFormFreqAllCoca -0.0015840  0.0007752  -2.043 0.042798 *  
#   PrecSegVCV              0.0119181  0.0032755   3.639 0.000379 ***
#   LocSpeech              -0.0022625  0.0004322  -5.235 5.61e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0182 on 147 degrees of freedom
# Multiple R-squared:  0.2524,	Adjusted R-squared:  0.2371 
# F-statistic: 16.54 on 3 and 147 DF,  p-value: 2.59e-09

#Summary:
# this is the final model, wiht no effect of no. of consonants

# The model suggests that 
#  - LocSpeech has the expected effect: The faster someone speakes, the shorter the l
#  - after vowels the /l/ is longer
# - frequency effect is significant: the more frequent, teh shorter BUT really small effect size!


par(mfrow=c(1,3))

visreg(final.lm)

# let's try out multi-model inferencing to see whether the same effects are shown


library(MuMIn)

options(na.action = "na.fail") 

model_ranking <- dredge(lyComplex.lm3)

model_average_<-model.avg(model_ranking)


summary(model_average_)


# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = AbsDurCon ~ <64 unique rhs>, data = lyComplex3)
# 
# Component models: 
#   df logLik    AICc delta weight
# 2345    6 394.07 -775.56  0.00   0.19
# 245     5 392.69 -774.96  0.60   0.14
# 235     5 392.66 -774.91  0.66   0.13
# 23456   8 395.69 -774.37  1.19   0.10
# 2356    7 394.26 -773.73  1.84   0.07
# 12345   7 394.08 -773.38  2.19   0.06
# 1245    6 392.75 -772.92  2.64   0.05
# 25      4 390.57 -772.87  2.69   0.05
# 1235    6 392.68 -772.78  2.78   0.05
# 123456  9 395.89 -772.51  3.05   0.04
# 2456    7 393.62 -772.45  3.11   0.04
# 12356   8 394.29 -771.57  3.99   0.03
# 12456   8 393.92 -770.83  4.73   0.02
# 125     5 390.57 -770.73  4.83   0.02
# 256     6 391.36 -770.13  5.43   0.01
# 1256    7 391.42 -768.05  7.51   0.00
# 23      4 386.70 -765.12 10.44   0.00
# 234     5 387.55 -764.68 10.88   0.00
# 24      4 386.18 -764.08 11.49   0.00
# 2       3 384.77 -763.39 12.18   0.00
# 123     5 386.72 -763.02 12.55   0.00
# 236     6 387.59 -762.59 12.97   0.00
# 1234    6 387.55 -762.52 13.05   0.00
# 2346    7 388.55 -762.33 13.24   0.00
# 246     6 387.35 -762.11 13.45   0.00
# 124     5 386.22 -762.03 13.54   0.00
# 12      4 384.77 -761.28 14.29   0.00
# 26      5 385.82 -761.23 14.33   0.00
# 1236    7 387.60 -760.42 15.15   0.00
# 12346   8 388.56 -760.11 15.46   0.00
# 1246    7 387.38 -759.98 15.59   0.00
# 126     6 385.82 -759.06 16.50   0.00
# 345     5 381.16 -751.91 23.65   0.00
# 45      4 379.78 -751.29 24.27   0.00
# 1345    6 381.24 -749.90 25.67   0.00
# 145     5 379.98 -749.55 26.02   0.00
# 3456    7 381.91 -749.03 26.53   0.00
# 35      4 378.57 -748.86 26.71   0.00
# 456     6 380.10 -747.62 27.95   0.00
# 13456   8 382.26 -747.50 28.06   0.00
# 135     5 378.57 -746.72 28.84   0.00
# 1456    7 380.57 -746.36 29.21   0.00
# 5       3 376.21 -746.27 29.30   0.00
# 356     6 379.27 -745.95 29.61   0.00
# 15      4 376.23 -744.19 31.37   0.00
# 1356    7 379.33 -743.88 31.69   0.00
# 56      5 376.42 -742.42 33.14   0.00
# 34      4 374.75 -741.22 34.35   0.00
# 346     6 376.82 -741.06 34.51   0.00
# 46      5 375.69 -740.96 34.60   0.00
# 4       3 373.37 -740.58 34.99   0.00
# 156     6 376.51 -740.44 35.12   0.00
# 3       3 372.92 -739.68 35.89   0.00
# 36      5 374.81 -739.21 36.36   0.00
# 134     5 374.80 -739.19 36.37   0.00
# 146     6 375.82 -739.05 36.51   0.00
# 1346    7 376.89 -739.00 36.56   0.00
# 14      4 373.53 -738.79 36.77   0.00
# 13      4 372.92 -737.57 38.00   0.00
# 6       4 372.88 -737.49 38.07   0.00
# (Null)  2 370.73 -737.38 38.19   0.00
# 136     6 374.81 -737.04 38.53   0.00
# 16      5 372.89 -735.36 40.20   0.00
# 1       3 370.75 -735.33 40.23   0.00
# 
# Term codes: 
#   AdjSyllStress              LocSpeech             logRelFreq logWordFormFreqAllCoca 
#         1                      2                      3                      4 
# PrecSegVC         TransitionType 
# 5                      6 
# 
# Model-averaged coefficients:  
#   (full average) 
#                                Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                    0.0748978  0.0093471   0.0094000   7.968  < 2e-16 ***
#   LocSpeech                     -0.0023154  0.0004370   0.0004406   5.256    1e-07 ***
#   logRelFreq                    -0.0007441  0.0007109   0.0007135   1.043  0.29702    
# logWordFormFreqAllCoca        -0.0009120  0.0009397   0.0009433   0.967  0.33360    
# PrecSegVCV                     0.0130885  0.0043745   0.0044003   2.974  0.00294 ** 
#   TransitionTypesingle           0.0014111  0.0034025   0.0034203   0.413  0.67992    
# TransitionTypesyllabic-double  0.0029177  0.0054177   0.0054339   0.537  0.59131    
# AdjSyllStressu                -0.0002355  0.0020053   0.0020203   0.117  0.90720    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)                    0.0748978  0.0093471   0.0094000   7.968  < 2e-16 ***
#   LocSpeech                     -0.0023154  0.0004370   0.0004405   5.256    1e-07 ***
#   logRelFreq                    -0.0011053  0.0005929   0.0005975   1.850  0.06434 .  
# logWordFormFreqAllCoca        -0.0014325  0.0008009   0.0008074   1.774  0.07603 .  
# PrecSegVCV                     0.0131544  0.0042855   0.0043120   3.051  0.00228 ** 
#   TransitionTypesingle           0.0044580  0.0047949   0.0048346   0.922  0.35648    
# TransitionTypesyllabic-double  0.0092176  0.0058870   0.0059342   1.553  0.12035    
# AdjSyllStressu                -0.0008948  0.0038327   0.0038624   0.232  0.81679    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                     LocSpeech PrecSegVC logRelFreq logWordFormFreqAllCoca TransitionType AdjSyllStress
# Importance:          1.00      0.99      0.67       0.64                   0.32           0.26         
# N containing models:   32        32        32         32                     32             32 



# LocSpeech and PrecSegVC are the most important factors, followed vy the Frequencies
# the comparison of the different models also shows that the models containing these variables
# are the best models. Because of the unreliability of teh effect of RelFreq the decision to 
# include FormFrq instead of RelFreq is justified

##################################################################################
#########                 Summary of the data        ##############################
###################################################################################

# For documenting the results, it is necessary to know the data distribution. That is why
#I'll summarize the data distribution here.

str(lyComplex3)

# 'data.frame':	151 obs. of  57 variables:
#   $ ItemID                 : int  4 5 6 7 9 13 15 17 18 19 ...
# $ item                   : Factor w/ 590 levels "actively","actually",..: 51 376 424 357 377 427 20 41 59 370 ...
# $ PartofSpeech           : Factor w/ 2 levels "a","adv": 2 2 2 2 2 2 2 2 2 2 ...
# $ Base                   : Factor w/ 447 levels "ability","able",..: 126 327 414 246 331 420 46 98 145 285 ...
# $ PartofSpeechBase       : Factor w/ 6 levels "a","adv","n",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ WordFormFreq           : num  5509 6203 7364 548 1785 ...
# $ BaseFormFreq           : num  228 16190 5375 17802 740 ...
# $ WordLemmaFreq          : num  5507 6195 7362 549 1784 ...
# $ BaseLemmaFreq          : num  228 16192 3811 17757 739 ...
# $ Speaker                : int  31 17 17 26 26 61 57 214 215 58 ...
# $ Pos                    : Factor w/ 3 levels "end","mid","pause": 2 1 3 2 3 2 2 2 2 2 ...
# $ Affix                  : Factor w/ 6 levels "inLoc","inNeg",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ AffixOrth              : Factor w/ 5 levels "im","in","lely",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ MorphBound             : Factor w/ 3 levels "no","opaque",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Consonant              : Factor w/ 3 levels "l","m","n": 1 1 1 1 1 1 1 1 1 1 ...
# $ NoCons                 : Factor w/ 2 levels "single","double": 2 1 2 1 1 2 2 2 2 1 ...
# $ AbsDurCon              : num  0.0307 0.0271 0.0241 0.0158 0.0316 ...
# $ WordDur                : num  0.621 0.489 0.396 0.5 0.627 ...
# $ PrecSeg                : Factor w/ 36 levels "@","@U","{","a",..: 28 8 23 27 27 19 1 19 28 32 ...
# $ PrecSegVC              : Factor w/ 2 levels "C","V": 1 1 2 1 1 1 2 1 1 2 ...
# $ PrecSegDur             : num  0.1328 0.0739 0.1478 0.1692 0.1004 ...
# $ FollSeg                : Factor w/ 42 levels "@","@U","{","3",..: 20 21 20 20 20 20 20 1 21 20 ...
# $ FollSegVC              : Factor w/ 3 levels "C","pause","V": 3 3 3 3 3 3 3 3 3 3 ...
# $ FollSegDur             : num  0.0953 0.1197 0.1472 0.0816 0.1196 ...
# $ LocSpeech              : num  11.3 14.3 10.1 10 12.8 ...
# $ PreSuf                 : Factor w/ 4 levels "finalNoMorph",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ SyllPhon               : int  4 3 3 2 3 4 2 6 4 5 ...
# $ SyllAct                : int  3 3 2 2 3 3 3 5 3 4 ...
# $ NoSegWord              : int  7 7 4 5 8 7 7 9 9 8 ...
# $ DeletionMorph          : Factor w/ 5 levels "C","CV","M","N",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ DeviantPronun          : Factor w/ 3 levels "C","N","Y": 2 2 2 2 2 2 2 2 2 2 ...
# $ PauseMorph             : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFormFreqAllCoca    : num  2817 91754 59326 48734 7829 ...
# $ WordFormFreqAllCoca    : num  37507 22763 20221 3816 13572 ...
# $ AffixStressLongman     : Factor w/ 4 levels "d","p","s","u": 4 4 4 4 4 4 4 NA 4 4 ...
# $ AdjSyllStressLongman   : Factor w/ 3 levels "p","s","u": 3 3 3 1 1 3 1 NA 3 3 ...
# $ AffixStress            : Factor w/ 1 level "u": 1 1 1 1 1 1 1 1 1 1 ...
# $ AdjSyllStress          : Factor w/ 2 levels "p","u": 2 2 2 1 1 2 1 2 2 2 ...
# $ WordLemmaFreqAllCoca   : num  37507 22763 20219 3816 13572 ...
# $ BaseLemmaFreqAllCoca   : num  2817 91787 43203 49058 7829 ...
# $ al                     : Factor w/ 2 levels "no","yes": 2 1 2 1 1 2 1 2 2 1 ...
# $ SearchName             : Factor w/ 11 levels "imbp","imm","inC",..: 7 8 7 8 8 7 7 7 7 8 ...
# $ logWordFormFreq        : num  8.61 8.73 8.9 6.31 7.49 ...
# $ logBaseFormFreq        : num  5.43 9.69 8.59 9.79 6.61 ...
# $ logWordLemmaFreq       : num  8.61 8.73 8.9 6.31 7.49 ...
# $ logBaseLemmaFreq       : num  5.43 9.69 8.25 9.78 6.61 ...
# $ logWordFormFreqAllCoca : num  10.53 10.03 9.91 8.25 9.52 ...
# $ logBaseFormFreqAllCoca : num  7.94 11.43 10.99 10.79 8.97 ...
# $ logWordLemmaFreqAllCoca: num  10.53 10.03 9.91 8.25 9.52 ...
# $ logBaseLemmaFreqAllCoca: num  7.94 11.43 10.67 10.8 8.97 ...
# $ RelFreq                : num  13.3145 0.248 0.468 0.0778 1.7336 ...
# $ logRelFreq             : num  2.589 -1.394 -0.759 -2.554 0.55 ...
# $ syllabicity            : Factor w/ 2 levels "no","yes": 2 1 1 1 1 2 1 2 2 1 ...
# $ TransitionType         : Factor w/ 3 levels "double","single",..: 3 2 1 2 2 3 1 3 3 2 ...
# $ Environment            : Factor w/ 3 levels "PrecC","PrecV",..: 3 1 2 1 1 3 2 3 3 2 ...
# $ bc                     : num  0.172 0.162 0.152 0.123 0.175 ...
# $ Transition             : Factor w/ 5 levels "C-double","C-single",..: 3 2 4 2 2 3 4 3 3 5 ...


# Summary of variables in final dataset

table(lyComplex3$TransitionType)

#double          single syllabic-double 
#32              71              48 


# Number of types 

length(unique(lyComplex3[lyComplex3$TransitionType=="double","item"]))
#[1] 28 types for double


length(unique(lyComplex3[lyComplex3$TransitionType=="syllabic-double","item"]))
#[1] 48 types for syllabic-double

length(unique(lyComplex3[lyComplex3$TransitionType=="single","item"]))
#[1] 71 types for single



summary (lyComplex3$AbsDurCon)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.005423 0.027050 0.041020 0.042580 0.054860 0.091000 


sd (lyComplex3$AbsDurCon)
#[1] 0.02084229


summary (lyComplex3$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6931  6.1070  7.2030  7.2460  8.6670 12.5800 

sd (lyComplex3$logWordFormFreqAllCoca)
#[1] 1.94789


summary (lyComplex3$logRelFreq)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.29400 -2.18300 -1.33900 -0.68750 -0.02183  8.65600 


sd (lyComplex3$logRelFreq)
#[1] 2.651418

summary (lyComplex3$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  6.418  11.100  12.940  13.580  15.300  26.010 


sd(lyComplex3$LocSpeech)
#[1]  3.485593


summary (lyComplex3$PrecSegVC)
#C   V 
#107  44 


length(unique(lyComplex3[lyComplex3$PrecSegVC=="C","item"]))
#[1] 107 types for C

length(unique(lyComplex3[lyComplex3$PrecSegVC=="V","item"]))
#[1] 40 types for V



summary (lyComplex3$AdjSyllStress)
#p   u 
#34 117 

length(unique(lyComplex3[lyComplex3$AdjSyllStress=="p","item"]))
#[1] 30 types for p

length(unique(lyComplex3[lyComplex3$AdjSyllStress=="u","item"]))
#[1] 117 types for u



# also, maybe we want to document the non-significant difference between 
# the double and single levels. Therefore, we need a model with TransitionType

final_TransitionType.lm<-lm (AbsDurCon  ~ TransitionType+PrecSegVC+ logWordFormFreqAllCoca +LocSpeech, data= lyComplex3)

summary(final_TransitionType.lm)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + PrecSegVC + logWordFormFreqAllCoca + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039228 -0.013135 -0.000586  0.012467  0.044022 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.0773230  0.0084382   9.163 4.45e-16 ***
#   TransitionTypesingle           0.0039653  0.0047610   0.833 0.406284    
#  TransitionTypesyllabic-double  0.0073865  0.0056410   1.309 0.192458    
#  PrecSegVCV                     0.0156936  0.0044286   3.544 0.000531 ***
#   logWordFormFreqAllCoca        -0.0016310  0.0007767  -2.100 0.037461 *  
#   LocSpeech                     -0.0023351  0.0004379  -5.332 3.65e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01822 on 145 degrees of freedom
# Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2361 
# F-statistic: 10.27 on 5 and 145 DF,  p-value: 1.918e-08

par(mfrow=c(2,2))

visreg(final_TransitionType.lm)

# Find out at which levels visreg draws lines

intercept=0.0773230 
estSingle=0.0039653 
estSyllabic= 0.0073865 
estPrecV= 0.0156936
medianSpeech=median(lyComplex3$LocSpeech)
estSpeech= -0.0023351
estFreq=-0.0016310 
medianFreq=median(lyComplex3$logWordFormFreqAllCoca)

EstimatedValueDouble= intercept+(medianFreq*estFreq)+(medianSpeech*estSpeech)

EstimatedValueDouble
#[1] 0.0353543



EstimatedValueSingle= intercept+(medianFreq*estFreq)+(medianSpeech*estSpeech)+ estSingle

EstimatedValueSingle
#[1] 0.0393196




EstimatedValueSyllabic= intercept+(medianFreq*estFreq)+(medianSpeech*estSpeech)+ estSyllabic

EstimatedValueSyllabic
#[1] 0.0427408




EstimatedValuePrecC= intercept+(medianFreq*estFreq)+(medianSpeech*estSpeech)+ estSingle

EstimatedValuePrecC
#[1] 0.0393196


EstimatedValuePrecV= intercept+(medianFreq*estFreq)+(medianSpeech*estSpeech)+ estSingle+ estPrecV

EstimatedValuePrecV
#[1] 0.0550132



# Calculate differences

# Preceding C and V

EstimatedValuePrecV - EstimatedValuePrecC

#[1] 0.0156936

# If a vowel precesed, l is [1] 0.0156936 sec longer

# singles, doubles and syllabics

EstimatedValueSyllabic- EstimatedValueDouble
#0.0073865

EstimatedValueSyllabic -  EstimatedValueSingle
#[1] 0.0034212


EstimatedValueSingle -EstimatedValueDouble
#[1] 0.0039653


# Syllabics are 0.0073865 longer than doubles and 0.0034212
# longer than singles. Singles are 0.0039653 longer than doubles.

# Maybe orthography plays an important role? We need to think about this.

#Also, we should conduct an ANOVA for the paper

anova(final_TransitionType.lm)


# Analysis of Variance Table
# 
# Response: AbsDurCon
# Df   Sum Sq   Mean Sq F value    Pr(>F)    
# TransitionType           2 0.001833 0.0009165  2.7619  0.066496 .  
# PrecSegVC                1 0.002897 0.0028970  8.7298  0.003654 ** 
#   logWordFormFreqAllCoca   1 0.002877 0.0028772  8.6703  0.003769 ** 
#   LocSpeech                1 0.009435 0.0094346 28.4302 3.646e-07 ***
#   Residuals              145 0.048118 0.0003319                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# We need to conduct a list with all types in teh corpus 

types_all_ly<-unique(ly$item)
types_all_ly
#write.csv(types_all_ly, "all_types_ly.csv")

levels(ly$MorphBound)
#[1] "no"          "opaque"      "transparent"
# and we need a list with only the complex dis_items

types_complex<-unique(ly[ly$MorphBound=="transparent"|ly$MorphBound=="opaque","item" ])
#write.csv(types_complex, "complex_types_ly.csv")

# and we need a list with only the transparent ly items

types_transparent<-unique(ly[ly$MorphBound=="transparent","item" ])
#write.csv(types_transparent, "transparent_types_ly.csv")


lyComplex3$AbsDurConMS<-lyComplex3$AbsDurCon*1000

levels(lyComplex3$NoCons)

levels(lyComplex3$NoCons)<-c("C#l","l#l")
lyComplex3$NoCons<- relevel (lyComplex3$NoCons, ref= "l#l")

levels(lyComplex3$NoCons)
#[1] "l#l" "C#l"

# for the plots we need to put the levels in a different order
lyComplex2$TransitionType <- relevel (lyComplex2$TransitionType, ref= "syllabic-double")
lyComplex2$TransitionType <- relevel (lyComplex2$TransitionType, ref= "double")

# we need to rename the levels

levels(lyComplex2$TransitionType)
#[1] "double"          "syllabic-double" "single"         

levels(lyComplex2$TransitionType)<-c("l#l", "syllabic l#l", "#l")        


lyComplex3$TransitionType <- relevel (lyComplex3$TransitionType, ref= "syllabic-double")
lyComplex3$TransitionType <- relevel (lyComplex3$TransitionType, ref= "double")

levels(lyComplex3$TransitionType)<-c("l#l", "syllabic l#l", "#l")        


png("boxLy.png", units="cm", height=10, width=7, res=300, pointsize=8)
bwplot (AbsDurCon*1000 ~ TransitionType, lyComplex2, ylab="duration in milliseconds", main="-ly", ylim=c(0,180), cex.axis=1 )
dev.off()

# we need to redo the model:

final_TransitionType.lm<-lm (AbsDurCon  ~ TransitionType+PrecSegVC+ logWordFormFreqAllCoca +LocSpeech, data= lyComplex3)

summary(final_TransitionType.lm)

# Call:
#   lm(formula = AbsDurCon ~ TransitionType + PrecSegVC + logWordFormFreqAllCoca + 
#        LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039228 -0.013135 -0.000586  0.012467  0.044022 
# 
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 0.0773230  0.0084382   9.163 4.45e-16 ***
#   TransitionTypesyllabic l#l  0.0073865  0.0056410   1.309 0.192458    
#   TransitionType#l            0.0039653  0.0047610   0.833 0.406284    
#   PrecSegVCV                  0.0156936  0.0044286   3.544 0.000531 ***
#   logWordFormFreqAllCoca     -0.0016310  0.0007767  -2.100 0.037461 *  
#   LocSpeech                  -0.0023351  0.0004379  -5.332 3.65e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01822 on 145 degrees of freedom
# Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2361 
# F-statistic: 10.27 on 5 and 145 DF,  p-value: 1.918e-08


png("ly- modelcov.png", units="cm", height=8, width=14, res=300, pointsize=8)


par(mfrow=c(1,2))

visreg(final_TransitionType.lm, "LocSpeech", trans= function(x)x*1000, rug=F, ylab="duration in milliseconds", xlab="local speech rate",ylim=c(0,100), cex.axis=0.8)
visreg(final_TransitionType.lm, "PrecSegVC", trans= function(x)x*1000, rug=F, ylab="duration in milliseconds", xlab="preceding segment",ylim=c(0,100), cex.axis=0.8)


dev.off()



png("ly- model TransitionType and freq.png", units="cm",height=8, width=14, res=300, pointsize=8)
par(mfrow=c(1,2))


visreg(final_TransitionType.lm, "logWordFormFreqAllCoca", trans= function(x)x*1000, rug=F, ylab="duration in milliseconds", xlab="word form frequency",ylim=c(0,100), cex.axis=0.8)
visreg(final_TransitionType.lm, "TransitionType", trans= function(x)x*1000, rug=F, ylab="duration in milliseconds", xlab="environment",ylim=c(0,100), cex.axis=0.8)


dev.off()

# to report the model, let's redo it with ms (easier to copy and paste)

final_TransitionType.lm<-lm (AbsDurCon*1000  ~ TransitionType+PrecSegVC+ logWordFormFreqAllCoca +LocSpeech, data= lyComplex3)

summary(final_TransitionType.lm)

# Call:
#   lm(formula = AbsDurCon * 1000 ~ TransitionType + PrecSegVC + 
#        logWordFormFreqAllCoca + LocSpeech, data = lyComplex3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.228 -13.135  -0.586  12.467  44.022 
# 
#   Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 77.3230     8.4382   9.163 
#***
#   TransitionTypesyllabic l#l   7.3865     5.6410   1.309 0.192458    
#   TransitionType#l             3.9653     4.7610   0.833 0.406284    
#   PrecSegVCV                  15.6936     4.4286   3.544 0.000531 ***
#   logWordFormFreqAllCoca      -1.6310     0.7767  -2.100 0.037461 *  
#   LocSpeech                   -2.3351     0.4379  -5.332 3.65e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 18.22 on 145 degrees of freedom
# Multiple R-squared:  0.2615,	Adjusted R-squared:  0.2361 
# F-statistic: 10.27 on 5 and 145 DF,  p-value: 1.918e-08