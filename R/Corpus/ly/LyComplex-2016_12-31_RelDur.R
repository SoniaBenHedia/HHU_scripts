# After doind the analysis on the absolute duration of ly we should take a look
# at the relative duration.


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

# since we want to look at the relative duration of ly, we only need those items
# for which it makes sense to calculate this, i.e. the items with a precedong
# vowel

# we need to recode prec Seg Vc (mistakes in the coding..)

levels(lyComplex$PrecSegVC)
#[1] "C"  "v"  "V"  "VV" "z" 


levels(lyComplex$PrecSegVC)<- c("C" , "V",  "V",  "V" ,"V" )

levels(lyComplex$PrecSegVC)
#[1] "C" "V"

# now let's create the dataset
lyComplexPrecV <- lyComplex[lyComplex$PrecSegVC=="V", ]
rownames(lyComplexPrecV ) <- 1:nrow(lyComplexPrecV )



dim(lyComplexPrecV)
#[1]  48 50

# let's see how many doubles and singles

table(lyComplexPrecV$NoCons)
# single double 
# 19     29 


# only 48 left!!! dataset is barely big enough



str(lyComplexPrecV)
# 'data.frame':	48 obs. of  50 variables:
#   $ ItemID                 : int  6 10 15 19 27 38 48 67 79 108 ...
# $ item                   : Factor w/ 590 levels "actively","actually",..: 424 361 20 370 366 284 346 82 406 31 ...
# $ PartofSpeech           : Factor w/ 10 levels "a","adv","c",..: 2 2 2 2 2 2 1 2 2 2 ...
# $ Base                   : Factor w/ 447 levels "ability","able",..: 414 258 46 285 270 189 213 178 386 76 ...
# $ PartofSpeechBase       : Factor w/ 6 levels "a","adv","n",..: 1 1 1 1 1 1 3 1 1 1 ...
# $ WordFormFreq           : num  7364 1067 2780 10571 2122 ...
# $ BaseFormFreq           : num  5375 449 4195 11848 4164 ...
# $ WordLemmaFreq          : num  7362 1065 2775 10550 2120 ...
# $ BaseLemmaFreq          : num  3811 449 4195 9092 4109 ...
# $ Speaker                : int  17 18 57 58 93 50 70 87 58 231 ...
# $ Pos                    : Factor w/ 3 levels "end","mid","pause": 3 2 2 2 2 2 2 1 2 2 ...
# $ Affix                  : Factor w/ 6 levels "inLoc","inNeg",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ AffixOrth              : Factor w/ 5 levels "im","in","lely",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ MorphBound             : Factor w/ 3 levels "no","opaque",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ Consonant              : Factor w/ 3 levels "l","m","n": 1 1 1 1 1 1 1 1 1 1 ...
# $ NoCons                 : Factor w/ 2 levels "single","double": 2 2 2 1 2 1 1 2 2 1 ...
# $ AbsDurCon              : num  0.02413 0 0.02241 0.00542 0.05332 ...
# $ WordDur                : num  0.396 0.46 0.403 0.308 0.5 ...
# $ PrecSeg                : Factor w/ 36 levels "@","@U","{","a",..: 23 1 1 32 1 18 34 1 1 1 ...
# $ PrecSegVC              : Factor w/ 2 levels "C","V": 2 2 2 2 2 2 2 2 2 2 ...
# $ PrecSegDur             : num  0.1478 0.0354 0.0276 0.0329 0.0654 ...
# $ FollSeg                : Factor w/ 42 levels "@","@U","{","3",..: 20 NA 20 20 20 29 21 20 20 20 ...
# $ FollSegVC              : Factor w/ 3 levels "C","pause","V": 3 NA 3 3 3 3 3 3 3 3 ...
# $ FollSegDur             : num  0.1472 NA 0.0684 0.0392 0.0757 ...
# $ LocSpeech              : num  10.1 10.9 17.4 26 18 ...
# $ PreSuf                 : Factor w/ 4 levels "finalNoMorph",..: 4 4 4 4 4 3 4 4 4 4 ...
# $ SyllPhon               : int  3 5 2 5 5 5 2 4 3 4 ...
# $ SyllAct                : int  2 3 3 4 4 5 2 4 3 4 ...
# $ NoSegWord              : int  4 5 7 8 9 11 5 7 6 8 ...
# $ DeletionMorph          : Factor w/ 5 levels "C","CV","M","N",..: 4 2 4 4 4 4 4 4 4 4 ...
# $ DeviantPronun          : Factor w/ 3 levels "C","N","Y": 2 2 2 2 2 2 2 2 2 2 ...
# $ PauseMorph             : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ BaseFormFreqAllCoca    : num  59326 8865 19449 59417 37906 ...
# $ WordFormFreqAllCoca    : num  20221 14535 24397 54031 11835 ...
# $ AffixStressLongman     : Factor w/ 4 levels "d","p","s","u": 4 4 4 4 4 4 4 4 4 4 ...
# $ AdjSyllStressLongman   : Factor w/ 3 levels "p","s","u": 3 3 1 3 3 3 1 3 3 3 ...
# $ AffixStress            : Factor w/ 4 levels "d","p","s","u": 4 4 4 4 4 4 4 4 4 4 ...
# $ AdjSyllStress          : Factor w/ 3 levels "p","s","u": 3 3 1 3 3 3 1 3 3 3 ...
# $ WordLemmaFreqAllCoca   : num  20219 14535 24397 54031 11835 ...
# $ BaseLemmaFreqAllCoca   : num  43203 8865 19456 44866 37307 ...
# $ al                     : Factor w/ 2 levels "m","y": 2 2 1 1 2 1 1 2 2 1 ...
# $ SearchName             : Factor w/ 11 levels "imbp","imm","inC",..: 7 7 7 8 7 8 8 7 7 8 ...
# $ logWordFormFreq        : num  8.9 6.97 7.93 9.27 7.66 ...
# $ logBaseFormFreq        : num  8.59 6.11 8.34 9.38 8.33 ...
# $ logWordLemmaFreq       : num  8.9 6.97 7.93 9.26 7.66 ...
# $ logBaseLemmaFreq       : num  8.25 6.11 8.34 9.12 8.32 ...
# $ logWordFormFreqAllCoca : num  9.91 9.58 10.1 10.9 9.38 ...
# $ logBaseFormFreqAllCoca : num  10.99 9.09 9.88 10.99 10.54 ...
# $ logWordLemmaFreqAllCoca: num  9.91 9.58 10.1 10.9 9.38 ...
# $ logBaseLemmaFreqAllCoca: num  10.67 9.09 9.88 10.71 10.53 ...

# We create the variable RelFreq

lyComplexPrecV$RelFreq <- lyComplexPrecV$WordLemmaFreqAllCoca / lyComplexPrecV$BaseLemmaFreqAllCoca
lyComplexPrecV$logRelFreq <- log(lyComplexPrecV$WordLemmaFreqAllCoca / lyComplexPrecV$BaseLemmaFreqAllCoca)

# now we have 51 variables. Let's take a look at the data distribution (duration)

densityplot(lyComplexPrecV$AbsDurCon)
densityplot(log(lyComplexPrecV$AbsDurCon))
plot(sort(lyComplexPrecV$AbsDurCon))
plot(sort(log(lyComplexPrecV$AbsDurCon)))
plot(lyComplexPrecV$AbsDurCon)


# let's create the variable RelDur

lyComplexPrecV$RelDur<-lyComplexPrecV$AbsDur/lyComplexPrecV$PrecSegDur


densityplot(lyComplexPrecV$RelDur)
densityplot(log(lyComplexPrecV$RelDur))
plot(sort(lyComplexPrecV$RelDur))
plot(sort(log(lyComplexPrecV$RelDur)))
plot(lyComplexPrecV$RelDur)


#############################################################################
#                                                                           #
########  Let's look at our variables                     ###################
#                                                                           #
#############################################################################


# Problem is, that there aren't too many observations in lyComplexPrecV. That 
# means that we have to be super-careful with regard to the number of 
# predictors, otherwise we end up with serious overfitting issues.

# We will now look at each preditor variable in order to decide which one to include.
# Variables are in included if there is a theoretical reason to include them, it is
# statistically unproblematic to include them (levels are not too small).

# Covariates are included if an effect is observable. 
#Variables of iterest are all included.



# 1.                       Partofspeech


lyComplexPrecV$PartofSpeech<-droplevels(lyComplexPrecV$PartofSpeech)
plot(lyComplexPrecV$RelDur~lyComplexPrecV$PartofSpeech)

table (lyComplexPrecV$PartofSpeech)

#a adv 
#2  46  

# There is no point in including this as a predictor in a model.
# (a)the levels are too 'small' 
# (b) there is no good theoretical reason why this should matter in the
# first place.
# (c) we do not see a difference between the levels


# 2.                 NoCons (number of consonants)

table(lyComplexPrecV$NoCons)
#single double 
# 19     29 

plot (lyComplexPrecV$RelDur ~ lyComplexPrecV$NoCons)

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

pairscor.fnc(lyComplexPrecV [, c("logWordFormFreq", "logBaseFormFreq", "logWordLemmaFreq", "logBaseLemmaFreq", "logRelFreq","logWordFormFreqAllCoca", "logBaseFormFreqAllCoca", "logWordLemmaFreqAllCoca", "logBaseLemmaFreqAllCoca")])

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

plot (lyComplexPrecV$logBaseFormFreq ~lyComplexPrecV$logWordFormFreq)

# There is a high correlation and therefore we should just include one frequency variable.


plot (lyComplexPrecV$logWordFormFreq, lyComplexPrecV$RelDur)
plot (lyComplexPrecV$logBaseFormFreq, lyComplexPrecV$RelDur)

# None of these plots indicate an extremely strong effect on 
# the absolute duration. This probably means that any decision that we
# make won't affect the model to a large degree.

# So: We decide to use logWordFormFreq, because we assume that
# in a very frequent word form, the segments are shorter!


# Let's also look at logRelFreq since we are interested in this variable
# on theoretical grounds

plot (lyComplexPrecV$logRelFreq, lyComplexPrecV$RelDur)

# There does not seem to be a high correlation. However I would like
# to include this factor this factor anyways since I am intersted on
# it on a theoretical base.

#4.                  Pos

table (lyComplexPrecV$Pos)
#   end   mid pause 
#   1    38     9 


plot (lyComplexPrecV$RelDur ~ lyComplexPrecV$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. 

#5.               WordDur

plot (density(lyComplexPrecV$WordDur))
plot (lyComplexPrecV$RelDur ~ lyComplexPrecV$WordDur)


#There seems to be a relation
# between the overall duration of a word (perhaps due to speech rate?, 
# or due to phrase-final lengthening?) and the duration of the consonants.
# The longer a word is, the longer are its segments pronounced.
# We might include this factor, however due to collinearity problems with trhe number
# of units (segments or syllables) in word, we might use the factor speech rate instead

#7.         PrecSegDur

plot (density(lyComplexPrecV$PrecSegDur))
plot(lyComplexPrecV$PrecSegDur)


plot (lyComplexPrecV$RelDur ~ lyComplexPrecV$PrecSegDur)

# The plot doesn't suggest anything remarkable, but we include the duration
# of the preceding segments anyway, because of other studies (such as Oh &
# Redford) have emphasized the interplay between the preceding vowel 
# duration and the geminate duration. 

# PrecSegDur and WordDur could however show a collinearity effect. We should
#check for this.

plot (lyComplexPrecV$WordDur ~ lyComplexPrecV$PrecSegDur)
# I am not quite sure whether there is indeed some correlation. Let's check
# with a test.

cor.test (lyComplexPrecV$PrecSegDur, lyComplexPrecV$WordDur)

# Pearson's product-moment correlation
# 
# data:  lyComplexPrecV$PrecSegDur and lyComplexPrecV$WordDur
# t = -0.61678, df = 46, p-value = 0.5404
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.3652998  0.1986820
# sample estimates:
# cor 
# -0.09056531 

# no corr



# 8. FollSegDur

plot(lyComplexPrecV$FollSegDur)
plot(lyComplexPrecV$FollSegDur~lyComplexPrecV$RelDur)
plot(lyComplexPrecV$FollSegDur)

# There could be a connection to the factor WordDur. Let's check

plot (lyComplexPrecV$WordDur ~ lyComplexPrecV$FollSegDur)

#There seems to be a positive correlation. Let's check.

cor.test (lyComplexPrecV$FollSegDur, lyComplexPrecV$WordDur)

# Pearson's product-moment correlation
# 
# data:  lyComplexPrecV$FollSegDur and lyComplexPrecV$WordDur
# t = 4.2832, df = 45, p-value = 9.556e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.2968709 0.7148515
# sample estimates:
# cor 
# 0.5381589


# We ignore FollSegDur, because that's sort of included in WordDur anyways, 
# and we can't see any connection to RelDur.


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

plot(density(lyComplexPrecV$LocSpeech))

plot(lyComplexPrecV$RelDur~lyComplexPrecV$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables. However, we saw in our other models that this factor might be of 
# great importance. Also, we might not be able to see teh effect because NoSegWord
# and WorDur have contrary effects.

# Thus, we should keep in mind the option to include this variable instead
# of WordDur and NoSegWord!

#      10.2. SyllPhon and SyllAct

mosaicplot (lyComplexPrecV$SyllPhon ~ lyComplexPrecV$SyllAct)
table(lyComplexPrecV$SyllPhon, lyComplexPrecV$SyllAct)


#     2  3  4  5
# 2  8  2  0  0
# 3  2 12  1  0
# 4  0  0  9  0
# 5  0  2  5  5
# 6  0  0  0  2

cor.test(lyComplexPrecV$SyllPhon, lyComplexPrecV$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  lyComplexPrecV$SyllPhon and lyComplexPrecV$SyllAct
# S = 2523.4, p-value = 3.062e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8630379 


# Both are fairly similar, but SyllAct (the actual number of syllables) 
# seems to be more plausible.

# But there is a relation between SyllAct and NoSegWords

#          10.3. NoSegWord

# How is it related to SyllAct?

cor.test(lyComplexPrecV$NoSegWord, lyComplexPrecV$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  lyComplexPrecV$NoSegWord and lyComplexPrecV$SyllAct
# S = 1355.2, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.9264416 

# Strong correlation! We should only include one!
# I'm in favor of NoSegWord since it is the more 
# fine-grained variable


# How is it related to WordDur?

plot(lyComplexPrecV$NoSegWord, lyComplexPrecV$WordDur)
cor.test (lyComplexPrecV$NoSegWord, lyComplexPrecV$WordDur)
# 
# Pearson's product-moment correlation
# 
# data:  lyComplexPrecV$NoSegWord and lyComplexPrecV$WordDur
# t = 7.552, df = 46, p-value = 1.363e-09
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.5831409 0.8487198
# sample estimates:
# cor 
# 0.7440016 



# So, there is, as expected, a rather strong correlation between the 
# number of segments and the duration of the word. This means that
# the model might show some unwanted effects of collinearity.

# Possible solutions:
# - Residualize/orthogonalize?
# - Principal component analyComplexPrecVsis?
# - Ignore?
# - Use onlyComplexPrecV one of the two?

#We just use the variable LocSpeech which combines the two measurments


#             12. Stress

lyComplexPrecV$AffixStress<-droplevels(lyComplexPrecV$AffixStress)

table (lyComplexPrecV$AffixStress)
#u 
#48

# ly ist immer unstressed!

lyComplexPrecV$AdjSyllStress<-droplevels(lyComplexPrecV$AdjSyllStress)

table (lyComplexPrecV$AdjSyllStress)
#p   u 
#11 37 


plot (lyComplexPrecV$RelDur ~ lyComplexPrecV$AdjSyllStress)
# there does not seem to be a difference

# We do not see any difference. However, let's just include this factor
# since we saw that in the prefixed words adjacent syllable stress had
# an effect on consonant duration

###############################################################
#   Summary: variables to include                            ##
###############################################################

# Since we only have so few items, we will just include those
# variables which were significant in the other analysis

## We are going to include the following predictors:
# - TransitionType
# - logWordFormFreqAllCoca
# - LocSpeech



###############################################################
#  Note: I analyzed different decomposability measurments for ly (see other script)
# and came to the conclusion that LSA does not mirror the decomposability of ly. Thus
# it will not be included in the models! We should also be careful with RelFreq since
# it does not have an effect on the duration of ly by itself. We include it because 
# an interaction might be significant##
###############################################################

## Do an initial model:

rownames(lyComplexPrecV) <- 1:nrow(lyComplexPrecV)




lyComplexPrecV.lm1 <- lm (RelDur ~ NoCons + logWordFormFreqAllCoca + LocSpeech , data = lyComplexPrecV)

summary (lyComplexPrecV.lm1)

# Call:
#   lm(formula = RelDur ~ NoCons + logWordFormFreqAllCoca + LocSpeech, 
#      data = lyComplexPrecV)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.05286 -0.34327 -0.00366  0.25111  1.34993 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.69334    0.38515   4.397 6.86e-05 ***
#   NoConsdouble           -0.02249    0.15508  -0.145 0.885359    
# logWordFormFreqAllCoca -0.13284    0.03715  -3.575 0.000864 ***
#   LocSpeech               0.01347    0.01932   0.697 0.489464    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5137 on 44 degrees of freedom
# Multiple R-squared:  0.2276,	Adjusted R-squared:  0.175 
# F-statistic: 4.323 on 3 and 44 DF,  p-value: 0.009349



# The model does not explain much of the variation found and only one 
#factor is significant - not our variable of interest!
#instead of fitting the model now, let's first deal with
#coll. issues



####################################################################
#               checking assumptions                               #
####################################################################

plot(lyComplexPrecV.lm1)

qqnorm (scale(residuals (lyComplexPrecV.lm1)))
qqline (scale(residuals (lyComplexPrecV.lm1)))

#looks okay

############################################################################
#           Slim down the model                                            #
############################################################################



summary(lyComplexPrecV.lm1)

# Call:
#   lm(formula = RelDur ~ NoCons + logWordFormFreqAllCoca + LocSpeech, 
#      data = lyComplexPrecV)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.05286 -0.34327 -0.00366  0.25111  1.34993 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.69334    0.38515   4.397 6.86e-05 ***
#   NoConsdouble           -0.02249    0.15508  -0.145 0.885359    
#   logWordFormFreqAllCoca -0.13284    0.03715  -3.575 0.000864 ***
#   LocSpeech               0.01347    0.01932   0.697 0.489464    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5137 on 44 degrees of freedom
# Multiple R-squared:  0.2276,	Adjusted R-squared:  0.175 
# F-statistic: 4.323 on 3 and 44 DF,  p-value: 0.009349

# the first factor we can throw out is LocSpeech


lyComplexPrecV.lm2<- update(lyComplexPrecV.lm1, ~ . - LocSpeech)


summary(lyComplexPrecV.lm2)
# Call:
#   lm(formula = RelDur ~ NoCons + logWordFormFreqAllCoca, data = lyComplexPrecV)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.01613 -0.33890  0.00218  0.23330  1.37205 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.85991    0.30031   6.193 1.61e-07 ***
#   NoConsdouble           -0.04506    0.15080  -0.299 0.766471    
#   logWordFormFreqAllCoca -0.12928    0.03659  -3.533 0.000963 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5108 on 45 degrees of freedom
# Multiple R-squared:  0.2191,	Adjusted R-squared:  0.1844 
# F-statistic: 6.313 on 2 and 45 DF,  p-value: 0.003831


# This is the final mode: only word form freq has an influence