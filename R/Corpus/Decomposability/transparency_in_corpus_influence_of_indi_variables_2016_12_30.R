
# let's write a function which gives us the number of types in a certain df

get_number_types <- function(df,variable,level) {
  # this function gives you the number of types (Item) of a specific level of a df
  number_types<- length(unique(df[df[variable]==level,"item"]))
  return(number_types)
}

get_types <- function(df, variable,level) {
  # this function gives you the number of types (Item) of a specific level of a df
  types<-unique(df[df[variable]==level,"item"])
  return(types)
}


get_type_combination <- function(df, variable1,level1, variable2, level2) {
  # this function gives you the number of types (Item) of a specific level of a df
  types<-unique(df[df[variable1]==level1 & df[variable2]==level2  ,"item"])
  return(types)
}

get_number_type_combination <- function(df, variable1,level1, variable2, level2) {
  # this function gives you the number of types (Item) of a specific level of a df
  types<-length(unique(df[df[variable1]==level1 & df[variable2]==level2  ,"item"]))
  return(types)
}

# Loading libraries

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

# origuinal file contained apostrophes, Ingo eliminated them and saved the revised file as 

# set the directory, so R knows where to find a file


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/duration")
#setwd("C:/Users/sbenhedia/Documents/Geminates/Corpus studies/Data un_in_ly/R-data")

#setwd("C:/Users/Plag/Documents/Forschung/Sonia/2015-08-18")

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

# Let's start with im

# Den Datensatz erstellen, in dem alle komplexen im-items sind
# previous analyses have shown that we cannot analyze im and in in
#the same dataset. This is due to the different phonetic characteristica
# of /m/ and /n/

# Zuerst Datensatz erstellen, in dem alle ims sind

im <- d[d$AffixOrth=="im",]

# Damit wir alle infos haben, d.h. auch accent, entnehmen wir die komplexen
# ims aus d2

imComplex <- d2[d2$AffixOrth=="im",]


dim(imComplex)
#[1] 158  56


# We create the variable RelFreq

imComplex$RelFreq <- imComplex$WordLemmaFreqAllCoca / imComplex$BaseLemmaFreqAllCoca
imComplex$logRelFreq <- log(imComplex$WordLemmaFreqAllCoca / imComplex$BaseLemmaFreqAllCoca)


# Let's check the number of our observations for doubles and singles, since we are interested in
# this variable primarily

table(imComplex$NoCons)
# single double 
# 67      91

# this looks satisfying
# We look at the distribution of the nasal duration

densityplot(imComplex$AbsDurCon)
plot(imComplex$AbsDurCon)

# therer is one item which displays a rather long nasal duration. Let's see which one it is

#use identify() to find out the line numbers of selected points:
#outliers <- identify(imComplex$AbsDurCon)
#outliers
#[1] 97

outliers <- imComplex[c(97), ]

#Check which one it is_

imComplex[c(97), c("ItemID","item", "AbsDurCon")]

# ItemID        item AbsDurCon
# 191    590 immediately 0.1908750

imComplex[c(97), c( "AbsDurCon")]


# Looking at the praat script, it revealed that the segmentation was false, let's
# change the value to the right one

imComplex[c(97), c( "AbsDurCon")] <-0.1483750000000006

imComplex[c(97), c( "AbsDurCon")]

densityplot(imComplex$AbsDurCon)
plot(imComplex$AbsDurCon)

densityplot(imComplex$AbsDurCon)
plot(imComplex$AbsDurCon)


# looks much better


# Let's plot the distribution of the data (single vs. double) & save it
# We might use these plots in a ppt/ paper


# In order to plot the distribution, we need to make two new datasets

imComplexDouble<-imComplex[imComplex$NoCons=="double",]

imComplexSingle<- imComplex[imComplex$NoCons=="single",]


#############################################################################
#                                                                           #
########  Back to the analysis of the im-complex-dataset  ###################
#                                                                           #
#############################################################################


# Problem is, that there aren't too many observations in unComplex. That 
# means that we have to be super-careful with regard to the number of 
# predictors, otherwise we end up with serious overfitting issues.

# We will now look at each preditor variable in order to decide which one to include.
# Variables are in included if there is a theoretical reason to include them, it is
# statistically unproblematic to include them (levels are not too small).

# Covariates are included if an effect is observable. 
#Variables of iterest are all included.

#         1. Part of Speech

imComplex$PartofSpeech<-droplevels(imComplex$PartofSpeech)
plot(imComplex$PartofSpeech, imComplex$AbsDurCon)
table (imComplex$PartofSpeech)
#a adv   n   v 
#50  37  42  29

# Even though it looks like a and adv have a longer m than n and v, we will not
# include this variable. This is becausethere is no good theoretical reason to include this factor
# AND we have to # chose carefuuly which variables to include (this would take up a lot of space:4)

# Sooo. We will not include this factor in the analysis

#       2. Number of consonants (NoCons)

table(imComplex$NoCons)

# single double 
#    67    91

plot (imComplex$AbsDurCon ~ imComplex$NoCons)

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

pairscor.fnc(imComplex [, c("logWordFormFreq", "logBaseFormFreq", "logWordLemmaFreq", "logBaseLemmaFreq", "logRelFreq","logWordFormFreqAllCoca", "logBaseFormFreqAllCoca", "logWordLemmaFreqAllCoca", "logBaseLemmaFreqAllCoca")])

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



plot (imComplex$logRelFreq, imComplex$AbsDurCon)
plot (imComplex$logWordFormFreqAllCoca, imComplex$AbsDurCon)
plot (imComplex$logBaseFormFreq, imComplex$AbsDurCon)

# None of these three plots indicate an extremely strong effect on 
# the absolute duration- it will not affect our model to a great degree.

# It's especially striking that
# we often have a base frequency of zero which is probably
# due to all the opaque item, also there is no theoretic backgorund to
# justify including this variable --> initially it was coded to calculate RelFreq
# So, I would prefer to NOT include BaseFormFreq.


# So we are left with two variables: logWordFormFreqAllCoca, because we assume that
# in a very frequent word form, the segments are shorter, and logRelFreq, 
# because we assume that in a more decomposable word, the absolute duration
# should be longer (and a word with a low RelFreq should be more 
# decomposable).

# We need however keep in mind that those two variables are correlated!

cor.test(imComplex$logWordFormFreqAllCoca, imComplex$logRelFreq, method="spearman")

# data:  imComplex$logWordFormFreqAllCoca and imComplex$logRelFreq
# S = 226901.6, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.6548285 

#     4. Position

table (imComplex$Pos)
#   end   mid pause 
# 9   134    15 

plot (imComplex$AbsDurCon ~ imComplex$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.


#     5. Word Duration

plot (density(imComplex$WordDur))
plot (imComplex$AbsDurCon ~ imComplex$WordDur)


#There does not seem to be a big correlation
# between the overall duration of a word  and the duration of the consonants.
# However, as we have seen in the un-analysis and in previous studies,
# there might be an effect of word length:
#The longer a word is, the longer are its segments pronounced.
# therefore we should include this factor


#     6. Preceding Segment duration

plot (density(imComplex$PrecSegDur))
plot(imComplex$PrecSegDur)

# there is one item which has an extremely long prec. segment duration. Let's take
# a look at this item

#outliers2 <- identify(imComplex$PrecSegDur)
#outliers2
#1] 85

# Let's see which item it is...

imComplex[85,]

# ItemID Sound.file    item.x PhrasalAccentSonia AnyAccentSonia AccentIngo
# 167    537    sw03023 immediate            unclear            yes        yes
# Text      item PartofSpeech
# 167 fight you know the the immediate need to show some sort immediate            a
# Base PartofSpeechBase WordFormFreq BaseFormFreq WordLemmaFreq BaseLemmaFreq
# 167 mediate                a         2689          102          2688             1
# Speaker Pos Affix AffixOrth MorphBound Consonant NoCons AbsDurCon   WordDur PrecSeg
# 167     373 mid inNeg        im     opaque         m double  0.109478 0.5441875       E
# PrecSegVC PrecSegDur FollSeg FollSegVC FollSegDur LocSpeech PreSuf SyllPhon SyllAct
# 167         V  0.1769595       i         V 0.07914562  12.86321 prefix        3       3
# NoSegWord DeletionMorph DeviantPronun PauseMorph BaseFormFreqAllCoca
# 167         7             N             N          N                1140
# WordFormFreqAllCoca AffixStressLongman AdjSyllStressLongman AffixStress
# 167               16858                  u                    p           d
# AdjSyllStress WordLemmaFreqAllCoca BaseLemmaFreqAllCoca   al SearchName
# 167             p                16858                    1 <NA>        imm
# logWordFormFreq logBaseFormFreq logWordLemmaFreq logBaseLemmaFreq
# 167        7.896925        4.624973         7.896553                0
# logWordFormFreqAllCoca logBaseFormFreqAllCoca logWordLemmaFreqAllCoca
# 167               9.732581               7.038784                9.732581
# logBaseLemmaFreqAllCoca RelFreq logRelFreq
# 167                       0   16858   9.732581

# I can't detect why this observation behaves differently than all the others. However
# I think we should exclude it from the dataset

imComplex2 <- imComplex [-c(85), ]
dim(imComplex2)
#[1] 157  58

# Let's look at the distribution now:

plot (density(imComplex2$PrecSegDur))
plot(imComplex2$PrecSegDur)


plot (imComplex2$AbsDurCon ~ imComplex2$PrecSegDur)

# The plot doesn't suggest anything remarkable, but there could be
# a weak correlation. we include the duration
# of the preceding segments a, because of other studies (such as Oh &
# Redford) have emphasized the interplay between the preceding vowel 
# duration and the geminate duration.
# Plus, if there is indeed no effect whatsover of PrecSegDur on AbsDurCon,
# there may be no need for a model of RelDurCon.

#     7. Following segment

table (imComplex2$FollSegVC)
#     C pause     V 
#    67    0    90 

# Problem: After double ms, only vowels may occur

table (imComplex2$FollSegVC, imComplex2$NoCons)
#      single double
#C         67      0
#pause      0      0
#V          0     90

# Since FollSegVC
# and NoSeg describe each the same items  we should not include
# both variables. We will use NoCons and keep in mind that after all singles
# a Consonant follows and after almost all doubles a vowel.


# Howeversomeone could argue that lengthening effects could be due to the 
#following segment which is  a Vowel after all doubled consonants and not the the double consonant.
# However, in general the phonetic literature suggests that a following vowel
# has a shortening effect on the preceding consonant and the same was observed
# for our un- data. Thus, the combinations we find do work against our hypothesis
# that doubles are longer than singles and thus we can be certain, that if we
#find a length difference in the predicted direction it is a gemination effect.


# 8. Following segment duration

plot (density(imComplex2$FollSegDur))
plot(imComplex2$FollSegDur)

# there is one item which has a verx long following segment duration. Let's look at which 
# item that is:

#outliers3 <- identify(imComplex2$FollSegDur)
#outliers3
#1] 112

# Which one is that?
imComplex2[112,c(1,2,3)]

#   ItemID Sound.file item.x
#237    728    sw03888 impede

# there seems to be nothing significant about this item. We will leave the item
# in the dataset for now


# Let's see whether FollSegDur has an effect on nasal dur.

plot (imComplex2$AbsDurCon ~ imComplex2$FollSegDur)

# it seems like there is a correlation. However this could also be
# due to the following segVC. Let's see how they behave

plot (imComplex2$FollSegDur ~ imComplex2$WordDur)
# One can see FollSegDur and WordDur seems correlated positively
# let's check

cor.test(imComplex2$FollSegDur, imComplex2$WordDur, method="spearman")

#Spearman's rank correlation rho

# data:  imComplex2$FollSegDur and imComplex2$WordDur
# S = 465238.8, p-value = 0.0004096
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.2786504 


# We ignore FollSegDur, because that's sort of included in WordDur anyways,
# and there is nothing the literature that suggests
# that this is an extremely important predictor on theoretical grounds.

#  9. Word Length

# Next, we have a bunch of variables that are all closely related to each
# other and are related to word length
#, which means that we certainly do not want to include all of them:
#
# LocSpeech, SyllPhon, SyllAct, NoSegWord


#    9.1. Local speech rate

# I think Locspeech is a factor which we do not necessariliy include in the 
# model, since it is calclated by WordDur/NoSegWord. If we decide to include
# both of these variables + check their interactions we sort of
# included LocSpeech. Nevertheless, let's take a look at this factor

plot(density(imComplex2$LocSpeech))
plot(imComplex2$AbsDurCon~imComplex2$LocSpeech) 

# The plot does not suggest that there is a strong correlation between the 
# two variables. However since it is made of two  variables which pull oppositional
# effects, there might still be an effect! We might include this factor (then we won't)
# include word dur



# 9.2. Number of syllables

mosaicplot (imComplex2$SyllPhon ~ imComplex2$SyllAct)
table(imComplex2$SyllPhon, imComplex2$SyllAct)

#   2  3  4  5  6
# 2 18  0  0  0  0
# 3  1 55 11  0  0
# 4  0  5 44 17  1
# 5  0  0  2  2  0
# 6  0  0  0  1  0

cor.test(imComplex2$SyllPhon, imComplex2$SyllAct, method="spearman")

# Spearman's rank correlation rho
# 
# data:  imComplex2$SyllPhon and imComplex2$SyllAct
# S = 97665.03, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.848571 

# Both are fairly similar, but SyllAct (the actual number of syllables) 
# seems to be more plausible.


plot (imComplex2$AbsDurCon ~ imComplex2$SyllAct)

#     9.3. Word Duration

plot (imComplex2$AbsDurCon ~ imComplex2$WordDur)

# there smight be an effect!

# But there is a relation between SyllAct and NoSegWords

plot(imComplex2$NoSegWord~imComplex2$SyllAct)

cor.test(imComplex2$NoSegWord, imComplex2$SyllAct, method="spearman")

# data:  imComplex2$NoSegWord and imComplex2$SyllAct
# S = 181117.4, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.7191787 


# Is there also a duration between NoSegWord and WordDur?

plot(imComplex2$NoSegWord, imComplex2$WordDur)
cor.test (imComplex2$NoSegWord, imComplex2$WordDur)

# Pearson's product-moment correlation
# 
# data:  imComplex2$NoSegWord and imComplex2$WordDur
# t = 5.7078, df = 155, p-value = 5.661e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.2782770 0.5382513
# sample estimates:
# cor 
# 0.4167496 

# So, there is, as expected, a correlation between the 
# number of segments and the duration of the word. This means that
# the model might show some imwanted effects of collinearity.

# Possible solutions:
# - Residualize/orthogonalize?
# - Principal component analysis?
# - Ignore?
# - Use only one of the two?

# We include NoSegWord since it is the most fine graines measure of segments
# in the word. In order to avoic coll. effects with word dur, we can
# combine the two in LocSpeech. This way we also "save" one variable.

#     10. MorphBound

imComplex2$MorphBound<-droplevels(imComplex2$MorphBound)

table(imComplex2$MorphBound)
# opaque transparent 
#106          51


plot (imComplex2$AbsDurCon ~ imComplex2$MorphBound)
# We need to include this factor! It looks like the semantic opaque
# words have a longer duration

#Let's check the relation between MorphBound and Nocons
table(imComplex2$MorphBound, imComplex2$NoCons)

#              single double
#opaque          46     60
#transparent     21     30

# The distribution looks okay.

#     11. Affix

# We should include affix  because

# we are interested in how differet affixes of the sam form behave.

get_types(imComplex2, "Affix","inLoc")
# need to change two codings (mistake)--> immaculate was coded as being Loc

imComplex2[imComplex2$item=="immaculate","Affix"] 
# in Loc

imComplex2[imComplex2$item=="immaculate","Affix"] <-"inNeg"
imComplex2[imComplex2$item=="immaculate","Affix"]
# [1] inNeg

# and impersonation was coded as inNeg

imComplex2[imComplex2$item=="impersonation","Affix"] 
# inNeg

imComplex2[imComplex2$item=="impersonation","Affix"] <-"inLoc"
imComplex2[imComplex2$item=="impersonation","Affix"]
#[1] inLoc

# Let's look at the distribution etc.

imComplex2$Affix<-droplevels(imComplex2$Affix)

table(imComplex2$Affix)
# inLoc inNeg 
#  71    86 

# that looks all right. But how does this relate to NoCons and MorphBound

table(imComplex2$Affix, imComplex2$NoCons)

#        single double
#inLoc     49     22
#inNeg     18     68


# That looks all right.

table(imComplex2$Affix, imComplex2$MorphBound)

#       opaque transparent
#inLoc     43          28
#inNeg     63          23


# That looks all right.

# Let's look at the relation between Affix and AbsDurCon

plot (imComplex2$AbsDurCon ~ imComplex2$Affix)
# inNeg seems to be longer.


#             12. Stress


table (imComplex2$AffixStress)
#d   p   s   u 
#116  22  18   1  

plot (imComplex2$AbsDurCon ~ imComplex2$AffixStress)
# debatable am längsten

table (imComplex2$AdjSyllStress)
# p  s  u 
# 116  1 40 

plot (imComplex2$AbsDurCon ~ imComplex2$AdjSyllStress)
# primary am längsten

# when primary stress auf zweiter Silbe, länger!!! Könnte im Zusammenhang mit
# der double liegen --> hier /m/ Teil der zweiten Silbe

table (imComplex2$AdjSyllStress,imComplex2$NoCons)

#   single double
#p     53     63
#s      1      0
#u     13     27


# scheint nicht der Fall zu sein..

# Außerdem:Zusammenhang zwischen Affix-stress und AdjSyllStress

table (imComplex2$AdjSyllStress,imComplex2$AffixStress)

#    d   p   s   u
#p 116   0   0   0
#s   0   0   0   1
#u   0  22  18   0


# --> primary stress auf der adjacent syllable,debatable stress auf dem Affix
# --> unstressed adjacent syllable, secondary or primary stress auf affix (lange Wörter)


# deshalb erstellen einer neuen Variable - stress pattern kombinert AsjSyllStress und AffixStress


imComplex2$StressPattern <- factor(paste (imComplex2$AffixStress, imComplex2$AdjSyllStress, sep="-"))
levels(imComplex2$StressPattern)
#[1] "d-p"  "p-u" "s-u" "u-s"

table (imComplex2$StressPattern)
#d-p p-u s-u u-s 
#116  22  18   1

plot (imComplex2$AbsDurCon ~ imComplex2$StressPattern)
#es zeigt sich, wenn die adjacent syllable stressed ist, ist /m/ länger (aber für u-s nur ein item),
# und wenn sie unstressed ist. ist das /m/ kürzer

# wir können also je zwei Level zusammenlegen (d-p + u-s =d/u-str UND p-u + s-u= str-unstr")

levels(imComplex2$StressPattern)
#[1] "d-p" "p-u" "s-u" "u-s"

levels(imComplex2$StressPattern)= c("d/u-str", "str-unstr", "str-unstr", "d/u-str")

levels(imComplex2$StressPattern)
#[1] "d/u-str"   "str-unstr"

# Let's take a look at it now

plot (imComplex2$AbsDurCon ~ imComplex2$StressPattern)

#when the adjacent syllable is stressed n is longer than when only the affix is stressed



# one further problem could be that certain StressPatterns correlate with NoSegWords

table (imComplex2$StressPattern,imComplex2$NoSegWord)

#           4  5  6  7  8  9 10 11 13
#d/u-str    3  9 17 24 30 13 17  4  0
#str-unstr  0  2  6  5  5 15  4  2  1

# kurze Wörter: d/u-str , aber nicht so extrem wie bei un!!!
# --> könnte Kollinearitätsprobleme verursachen

plot(imComplex2$StressPattern,imComplex2$WordDur)
# es sieht nicht so aus, als ob es hier einen Zusammenhang gibt

# we will include this factor in our analysis and will keep an eye on possible
# coll. problems

# Let's check the stress pattern of the two different affixes

table(imComplex2$StressPattern,imComplex2$Affix)
#              inLoc inNeg
# d/u-str      39    78
# str-unstr    32     8

# the majotity of negative ins have a stressed first syll. of base!!!!!



###############################################################
#   Summary: variables to include                            ##
###############################################################

## We are going to include the following predictors:
# - NoCons
# - logWordFormFreqAllCoca
# - logRelFreq
# - LocSpeech
# - PrecSegDur
# - Affix
# - MorphBound
# - StressPattern
# - Median
# - Accent




#############################################################################################
#  NOW the im- dataset os in a form in which we can go on working...(the same as the        #
# one we did the LM in...)                                                                  #
#       Next, we will merge the rating and the LSA score, so that we can wor with one df    #
#############################################################################################


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/Decomposability")


LSA <- read.csv("LSA_scores_und_in_18_01_2016.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(LSA)
#[1] 226   2

str(LSA)
#'data.frame':  226 obs. of  2 variables:
#  $ item: Factor w/ 226 levels "imbalance","imbibe",..: 115 85 95 217 181 25 183 182 33 176 ...
#$ LSAScore : Factor w/ 64 levels "-0.01","-0.04",..: 64 64 64 29 64 13 3 64 64 52 ...

# LSA needs to be an integer

LSA$LSAScore<-as.numeric((as.character(LSA$LSAScore)))

str(LSA)
# 'data.frame':	226 obs. of  2 variables:
#   $ item    : Factor w/ 226 levels "imbalance","imbibe",..: 115 85 95 217 181 25 183 182 33 176 ...
# $ LSAScore: num  NA NA NA 0.24 NA 0.08 -0.06 NA NA 0.49 ...


imComplex3<- merge(imComplex2, LSA, by.x="item", by.y="item")


dim(imComplex3)
#[1] 157 60

# looks good, now we can have a look at the LSA scores

# 11. LSA

#Let's have a look at the distribution of RelFreq

plot(unique(imComplex3$LSAScore),imComplex3$ID)
text(unique(imComplex3$LSAScore),imComplex3$ID, imComplex3$item)
densityplot(imComplex3$LSAScore)

bwplot(~LSAScore|item,imComplex3)
bwplot(~LSAScore|Affix,imComplex3)
bwplot(~LSAScore|MorphBound,imComplex3)

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

imComplex4<- merge(imComplex3, LSA_Marelli, by.x="item", by.y="derived_word")


dim(imComplex4)
#[1] 1 63

str(imComplex4)
# looks like only one of our types is in the dataset, so it is not
# interesting for us

# Let's check again

LSA_Marelli[LSA_Marelli$affix=="in-",]

# yes, they did not include any words with the allomorph im-! The only word which
# is included which is also in our dataset is impossibility but here the 
# similarity between impossible and ity is investigated.

# So we will ignore this factor

############################################################################################
#                     MERGE  with the rating                                               #
############################################################################################

# We will first load all the df we need

rating_1<- read.csv("rating_1_items_AE_median_simple.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

dim(rating_1)
#[1] 113 3


str(rating_1)
# 'data.frame':  113 obs. of  3 variables:
#   $ X     : int  25 44 62 94 128 146 157 202 213 238 ...
# $ Item  : Factor w/ 113 levels "imbalance","imbibe",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ median: int  1 4 4 1 4 4 2 2 2 1 ..

rating_2<- read.csv("rating_2_items_AE_median_simple.csv", sep="\t",header = T,  na.string=c("na", "", "NA"))

dim(rating_2)
#[1] 111 3

str(rating_2)
# 'data.frame':  111 obs. of  3 variables:
#   $ X     : int  2 58 78 107 145 164 214 252 260 313 ...
# $ Item  : Factor w/ 111 levels "immaterial","immediacy",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ median: int  1 3 3 3 1 2 2 1 3 2 ...


# bind the two rating

rating<-rbind(rating_1,rating_2)
dim(rating)
#[1] 224   3


str(rating)
# 'data.frame':  224 obs. of  3 variables:
#   $ X     : int  25 44 62 94 128 146 157 202 213 238 ...
# $ Item  : Factor w/ 224 levels "imbalance","imbibe",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ median: int  1 4 4 1 4 4 2 2 2 1 ...

# merge 
imComplex4<-merge(rating,imComplex3,  by.x="Item", by.y="item",all= FALSE)

dim(imComplex4)
#[1] 156 62

imComplexTypes<- imComplex4[!duplicated(imComplex4$Item),]

dim(imComplexTypes)
#[1] 83  62

# now we have a table with all types of in in the datasset with all of their measures!
bwplot(~median|Item,imComplexTypes)


# Let's read the table in which the roots are coded

#r <- read.table("type_of_root.csv", sep=",",header = T,  na.string=c("na", "", "NA"))
r <- read.table("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/Einzelne_Variablen/Roots/type_of_root.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

imComplex5<- merge(imComplex4, r, by.x="Item", by.y="types")
imComplexTypes<- merge(imComplexTypes, r, by.x="Item", by.y="types")

dim(imComplex5)
#[1] 56 63

dim(imComplexTypes)
# 83 63

# Let's look at the distribution of inNeg and inLoc

imComplex5$Affix<-droplevels(imComplex5$Affix)

# Let's look at the distribution of type of root per Affix

# for each type

table(imComplexTypes$type_of_base,imComplexTypes$Affix)
#             inLoc inNeg
#bound root    44    7
#word          10    22

# inLoc has a less words than roots, inNeg almost only words


# for each token

table(imComplex5$type_of_base,imComplex5$Affix)
#             inLoc inNeg
# bound root    60    63
# word          10    23


# but one can see that due to the token distribution inNeg has a lot of bound roots
# in the dataset!


# Let's look at the distribution of Semantic Transp per Affix

imComplex5$MorphBound<-droplevels(imComplex5$MorphBound)

table(imComplexTypes$type_of_base,imComplexTypes$MorphBound)

#              opaque transparent
# bound root    46          5
# word            3          29


# per token
table(imComplex5$type_of_base,imComplex5$MorphBound)

#           opaque transparent
# bound root    102          21
# word            3          30


# which are the opaque words with a word as aroot?

imComplexTypes[imComplexTypes$MorphBound=="opaque" & imComplexTypes$type_of_base=="word","Item"]

#[1] impersonation impulse       impulsive  


# which are the transparent words with a bound root?

imComplexTypes[imComplexTypes$MorphBound=="transparent" & imComplexTypes$type_of_base=="bound root","Item"]

#[1] all the immigr words..


#Let's have a look at the Semantic Transparency per affix

# first tokens
table(imComplex5$Affix,imComplex5$MorphBound)

#           opaque transparent
# inLoc     42          28
# inNeg     63          23

# now types

table (imComplexTypes$Affix, imComplexTypes$MorphBound)
#       opaque transparent
#inLoc     42          12
#inNeg     7           22


# let's have a alook at the rating

# tokens
 table(imComplex5$median)
#1  2  3  4 
#23 30 85 18 
 
# types
 table(imComplexTypes$median)
 # 1  2  3  4 
 # 22 14 29 18

 
 # now per affix
 
 # tokens
 table(imComplex5$median,imComplex5$Affix)
#   inLoc inNeg
 # 1     2    21
 # 2    29     1
 # 3    25    60
 # 4    14     4
 
 
# types
 
 table(imComplexTypes$median,imComplexTypes$Affix)
 #   inLoc inNeg
 # 1     2    20
 # 2    13     1
 # 3    25     4
 # 4    14     4

# jetzt relFreq
# for tokens
 
Loc<-imComplex5[imComplex5$Affix=="inLoc",]
LocTypes<-imComplexTypes[imComplexTypes$Affix=="inLoc",]

Neg<-imComplex5[imComplex5$Affix=="inNeg",]
NegTypes<-imComplexTypes[imComplexTypes$Affix=="inNeg",]

summary(Loc$logRelFreq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4.2150 -0.8604  0.8064  1.2420  2.5920  9.7900 

summary(LocTypes$logRelFreq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4.2150 -0.8604  0.0000  1.5550  3.6950  9.7900 


summary(Neg$logRelFreq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -9.6700  0.6636  7.7140  5.4020  9.2280 10.4900


summary(NegTypes$logRelFreq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -9.6700 -2.8530 -1.6270 -0.2054  1.6090  9.7330 

# now stress pattern

# first tokens
table(imComplex5$Affix,imComplex5$StressPattern)
#          d/u-str str-unstr
# inLoc      39        31
# inNeg      78         8

# now types
table(imComplexTypes$Affix,imComplexTypes$StressPattern)
#           d/u-str str-unstr
# inLoc      39        15
# inNeg      22         7


####################################################################################
# Now we can compare the measurements                                              #
####################################################################################


# Let's first look at the distributions

# hmmmm irgendwie muss ich mir über Verteilung klar werden....

# wor probieren ctre...

library(party)

# with tokens
 
dec_tree<-ctree (median~logRelFreq+MorphBound+Affix+LSAScore, data=imComplex5)
plot(dec_tree)

# with types
dec_tree<-ctree (median~logRelFreq+MorphBound+Affix+LSAScore, data=imComplexTypes)
plot(dec_tree)

# Das bedeutet also, dass fast alle opaquen als mit 3 oder 4 bewertet wurden, für transparente wurden
# lokative mit 3 bewertet, während negatiuve mit 1 oder 2 bewertet wurden. LSA hatte keinen Einfluss und
# RelFreq auch nicht wirklich


dec_tree_2<-ctree (median~MorphBound+Affix+LSAScore, data=imComplexTypes)
plot(dec_tree_2)

# Wenn wir RelFreq rausnehmen geschieht folgendes:
# fast alle opaquen werden mit 3 bewertet wurden
# Für transpararente:lokative mit 2 bewertet, während negatiuve mit 1 bewertet wurden.

# Insgesamt können wir also festhalten, dass es für das rating quasi folgende Hierarchie gibt
# opaque< transparen/Loc< transparent/neg

# Note: almost all opaques are in Loc

# das ist wie erwartet!


# Lt's look at semantic transparency and teh results

bwplot(~median|MorphBound,imComplexTypes)

bwplot(~logRelFreq|MorphBound,imComplexTypes)

bwplot(~LSAScore|MorphBound,imComplexTypes)

# only rating seems to be influenced by this


# Let's look at affix and teh results

bwplot(~median|Affix,imComplexTypes)

# Neg als more decomposable bewertet

bwplot(~logRelFreq|Affix,imComplexTypes)

bwplot(~LSAScore|Affix,imComplexTypes)


# we don't see any difference for LSA and RelFreq, however the ctree has given us some nterstung insights!

#################################################################################
# Summary inNeg vs in Loc                                              #
# - inLoc has fewer words as roots and is more often opaque than inNeg
# - inLoc was also rated less transparent than in inNeg
#############################################################################################




####################################################################################
# Let's systematically check the influence on consonant duration                                   #
####################################################################################


# let's get the summary of the LSA Score first

summary(imComplex5$LSAScore)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -0.0400  0.1500  0.2300  0.2549  0.3700  0.6800      63 



sd(imComplex5[!is.na(imComplex5$LSAScore),]$LSAScore)
#[1] 0.1630592

# Let's now test the influence of each variablem let's start with LSA

imComplex5.LSA<- lm (AbsDurCon ~  NoCons+ LSAScore+ LocSpeech +StressPattern, data = imComplex5[!is.na(imComplex5$LSAScore),])
summary(imComplex5.LSA)

# Call:
#   lm(formula = AbsDurCon ~ NoCons + LSAScore + LocSpeech + StressPattern, 
#      data = imComplex5[!is.na(imComplex5$LSAScore), ])
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.040662 -0.015323 -0.002092  0.011179  0.057529 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0961245  0.0083070  11.572  < 2e-16 ***
#   NoConsdouble            0.0340501  0.0051945   6.555 3.66e-09 ***
#   LSAScore               -0.0059614  0.0156050  -0.382 0.703368    
# LocSpeech              -0.0021807  0.0005746  -3.795 0.000271 ***
#   StressPatternstr-unstr -0.0258102  0.0050149  -5.147 1.60e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02001 on 88 degrees of freedom
# Multiple R-squared:  0.435,	Adjusted R-squared:  0.4093 
# F-statistic: 16.94 on 4 and 88 DF,  p-value: 2.483e-10


# no influence of LSA

# now RelFreq

imComplex5.RelFreq<- lm (AbsDurCon ~  NoCons+ logRelFreq+ LocSpeech +StressPattern, data = imComplex5)
summary(imComplex5.RelFreq)


# Call:
#   lm(formula = AbsDurCon ~ NoCons + logRelFreq + LocSpeech + StressPattern, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039462 -0.014907 -0.002357  0.010890  0.059543 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0928761  0.0064026  14.506  < 2e-16 ***
#   NoConsdouble            0.0282017  0.0038892   7.251 2.00e-11 ***
#   logRelFreq              0.0004780  0.0004359   1.097    0.275    
# LocSpeech              -0.0020459  0.0004471  -4.576 9.84e-06 ***
#   StressPatternstr-unstr -0.0219227  0.0042315  -5.181 6.97e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01956 on 151 degrees of freedom
# Multiple R-squared:  0.4911,	Adjusted R-squared:  0.4776 
# F-statistic: 36.43 on 4 and 151 DF,  p-value: < 2.2e-16
# no

# let's try the cat measurment

# Let'S recode: I wanna recode as base more often or derivative more often

list_RelFreqCat=list()
for (i in imComplex5$item){
  if(imComplex5[imComplex5$item==i,"logRelFreq"]<0){
    list_RelFreqCat=append(list_RelFreqCat,"more decomposable")}
  else{list_RelFreqCat=append(list_RelFreqCat,"less decomposable")}
}

imComplex5$RelFreqCat<-as.factor(as.character(list_RelFreqCat))

# now RelFreqCat

imComplex5.RelFreqCat<- lm (AbsDurCon ~  NoCons +LocSpeech+StressPattern+RelFreqCat, data = imComplex5)
summary(imComplex5.RelFreqCat)
# 
# Call:
#   lm(formula = AbsDurCon ~ NoCons + LocSpeech + StressPattern + 
#        RelFreqCat, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.040064 -0.014565 -0.002325  0.011845  0.061445 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  0.0925016  0.0067228  13.759  < 2e-16 ***
#   NoConsdouble                 0.0307160  0.0034333   8.947 1.23e-15 ***
#   LocSpeech                   -0.0019680  0.0004448  -4.424 1.84e-05 ***
#   StressPatternstr-unstr      -0.0242166  0.0037911  -6.388 1.97e-09 ***
#   RelFreqCatmore decomposable  0.0003457  0.0038601   0.090    0.929    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01963 on 151 degrees of freedom
# Multiple R-squared:  0.4871,	Adjusted R-squared:  0.4735 
# F-statistic: 35.84 on 4 and 151 DF,  p-value: < 2.2e-16

# no

# now Type of Base

imComplex5.Base<- lm (AbsDurCon ~  NoCons +LocSpeech+StressPattern+ type_of_base, data = imComplex5)
summary(imComplex5.Base)


# Call:
#   lm(formula = AbsDurCon ~ NoCons + LocSpeech + StressPattern + 
#        type_of_base, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039425 -0.014012 -0.003467  0.011475  0.061735 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0913963  0.0065953  13.858  < 2e-16 ***
#   NoConsdouble            0.0314840  0.0033842   9.303  < 2e-16 ***
#   LocSpeech              -0.0019620  0.0004428  -4.431 1.79e-05 ***
#   StressPatternstr-unstr -0.0243674  0.0037258  -6.540 8.96e-10 ***
#   type_of_baseword        0.0033564  0.0040517   0.828    0.409    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01959 on 151 degrees of freedom
# Multiple R-squared:  0.4893,	Adjusted R-squared:  0.4758 
# F-statistic: 36.17 on 4 and 151 DF,  p-value: < 2.2e-16

# now median

imComplex5.rating<- lm (AbsDurCon ~  NoCons +LocSpeech+StressPattern+ median, data = imComplex5)
summary(imComplex5.rating)



# Call:
#   lm(formula = AbsDurCon ~ NoCons + LocSpeech + StressPattern + 
#        median, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039931 -0.013997 -0.003543  0.011076  0.061768 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0959346  0.0081266  11.805  < 2e-16 ***
#   NoConsdouble            0.0305754  0.0032187   9.499  < 2e-16 ***
#   LocSpeech              -0.0019573  0.0004435  -4.413 1.93e-05 ***
#   StressPatternstr-unstr -0.0249111  0.0038968  -6.393 1.92e-09 ***
#   median                 -0.0012334  0.0018892  -0.653    0.515    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01961 on 151 degrees of freedom
# Multiple R-squared:  0.4885,	Adjusted R-squared:  0.4749 
# F-statistic: 36.05 on 4 and 151 DF,  p-value: < 2.2e-16

#no

# now semantic transparency

imComplex5.SemTr<- lm (AbsDurCon ~  NoCons +LocSpeech+StressPattern+ MorphBound, data = imComplex5)
summary(imComplex5.SemTr)


# Call:
#   lm(formula = AbsDurCon ~ NoCons + LocSpeech + StressPattern + 
#        MorphBound, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.041060 -0.014557 -0.002438  0.012224  0.060704 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.093465   0.006442  14.508  < 2e-16 ***
#   NoConsdouble            0.030474   0.003213   9.485  < 2e-16 ***
#   LocSpeech              -0.001965   0.000442  -4.447 1.68e-05 ***
#   StressPatternstr-unstr -0.021815   0.004299  -5.075 1.12e-06 ***
#   MorphBoundtransparent  -0.004195   0.003892  -1.078    0.283    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01956 on 151 degrees of freedom
# Multiple R-squared:  0.4909,	Adjusted R-squared:  0.4775 
# F-statistic: 36.41 on 4 and 151 DF,  p-value: < 2.2e-16


# einzeln, keinen Einfluss - vielleicht aber zusammen genommen 

# PCP Anylse

# to make such an analysis we need our final model - let's create it 
# here

imComplex.lm2 <- lm (AbsDurCon ~ NoCons+ logRelFreq + LocSpeech+ PrecSegDur + median+ StressPattern+ MorphBound + Affix, data = imComplex5)


bc <- boxcox(imComplex.lm2)

lambda <- bc$x[which.max(bc$y)]

imComplex5$bc <- imComplex5$AbsDurCon^lambda


final_im.lm<-lm(bc ~ NoCons + LocSpeech + StressPattern + Affix, 
               data = imComplex5)


summary(final_im.lm)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
#   NoConsdouble            0.0481981  0.0072235   6.672 4.48e-10 ***
#   LocSpeech              -0.0034761  0.0008019  -4.335 2.65e-05 ***
#   StressPatternstr-unstr -0.0375435  0.0077786  -4.826 3.37e-06 ***
#   AffixinNeg              0.0164092  0.0077370   2.121   0.0356 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03527 on 151 degrees of freedom
# Multiple R-squared:  0.5171,	Adjusted R-squared:  0.5043 
# F-statistic: 40.42 on 4 and 151 DF,  p-value: < 2.2e-16


library(pls)


# we need to recode all the variables, so that they are numeric

# # also they need to "point in the same direction" --> the higher
# the less decomposable

# RelFreq is fine
# median is fine

#Affix

levels(imComplex5$Affix)
#[1] "inLoc" "inNeg"

imComplex5$Affix <- relevel (imComplex5$Affix, ref= "inNeg")

imComplex5$NumAffix<-as.numeric(imComplex5$Affix)

table(imComplex5$Affix,imComplex5$NumAffix)

#      1  2
# inNeg 86  0
# inLoc  0 70

# good


#Type pf base

levels(imComplex5$type_of_base)
#[1] "bound root" "word"      

imComplex5$type_of_base <- relevel (imComplex5$type_of_base, ref= "word"   )

imComplex5$NumType_of_base<-as.numeric(imComplex5$type_of_base)

table(imComplex5$type_of_base,imComplex5$NumType_of_base)
#               1   2
# word        33   0
# bound root   0 123


#Smenatic Transparency

levels(imComplex5$MorphBound)
#[1] "opaque"      "transparent"

imComplex5$MorphBound <- relevel (imComplex5$MorphBound, ref= "transparent"   )

imComplex5$NumMorphBound<-as.numeric(imComplex5$MorphBound)

table(imComplex5$MorphBound,imComplex5$NumMorphBound)
#                1   2
# transparent  51   0
# opaque        0 105

# one further problem is that the variables are on 
# different scales - so we need to change this

imComplex5$ScaledMorphBound<-as.numeric(scale(imComplex5$NumMorphBound))
summary(imComplex5$ScaledMorphBound)

imComplex5$ScaledMedian<-as.numeric(scale(imComplex5$median))
summary(imComplex5$ScaledMedian)

imComplex5$ScaledTypeOfBase<-as.numeric(scale(imComplex5$NumType_of_base))
summary(imComplex5$ScaledTypeOfBase)

imComplex5$ScaledAffix<-as.numeric(scale(imComplex5$NumAffix))
summary(imComplex5$ScaledAffix)

imComplex5$ScaledRelFreq<-as.numeric(scale(imComplex5$logRelFreq))
summary(imComplex5$ScaledRelFreq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.7850 -0.8179 -0.1031  0.0000  0.8811  1.4660

# so now they are scaled

decomposability.pc <- prcomp(imComplex5[, c("ScaledAffix", "ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledMorphBound")])
summary(decomposability.pc)
#Importance of components:
#                          PC1    PC2     PC3     PC4     PC5
# Standard deviation     1.6603 1.1497 0.65399 0.56575 0.41710
# Proportion of Variance 0.5513 0.2643 0.08554 0.06401 0.03479
# Cumulative Proportion  0.5513 0.8156 0.90119 0.96521 1.00000

# This clearly shows that the decomposability information can really be mostly
# captured in a single component, PC1 and PC2, PC3 is also of importance 

decomposability.pc$rotation
#                    PC1          PC2        PC3         PC4         PC5
# ScaledAffix       0.07784658 -0.816811579 -0.2439200  0.44946020  0.25543558
# ScaledRelFreq    -0.42833274  0.450254847 -0.5303133  0.57419391  0.05358029
# ScaledMedian     -0.52115039 -0.233483147  0.4503460  0.26916771 -0.63136766
# ScaledTypeOfBase -0.48662219 -0.274903229 -0.5290781 -0.62305276 -0.13967343
# ScaledMorphBound -0.54961086 -0.001803157  0.4201616 -0.08741216  0.71676218

# PC1: MOSTLY the decomposability measures, a little affix
# PC2: Mostly Affix

imComplex5$PCDec1 <- decomposability.pc$x[, 1]
imComplex5$PCDec2 <- decomposability.pc$x[, 2]

# let's see whether this has an influence


imPC.lm<-lm(bc ~ NoCons +PrecSegDur+logWordFormFreqAllCoca+ LocSpeech + StressPattern + PCDec1+ PCDec2, 
                data = imComplex5)


summary(imPC.lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.3250077  0.0263860  12.317  < 2e-16 ***
#   NoConsdouble            0.0441486  0.0086697   5.092 1.06e-06 ***
#   PrecSegDur             -0.0152493  0.1356267  -0.112 0.910630    
# logWordFormFreqAllCoca  0.0010458  0.0023945   0.437 0.662918    
# LocSpeech              -0.0036720  0.0009322  -3.939 0.000126 ***
#   StressPatternstr-unstr -0.0347652  0.0093549  -3.716 0.000286 ***
#   PCDec1                 -0.0011188  0.0024563  -0.455 0.649433    
# PCDec2                  0.0080172  0.0035062   2.287 0.023641 *


# let's throw out PrecSegDur

imPC.lm2<-lm(bc ~ NoCons +logWordFormFreqAllCoca+ LocSpeech + StressPattern + PCDec1+ PCDec2, 
            data = imComplex5)


summary(imPC.lm2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.3233411  0.0217569  14.862  < 2e-16 ***
#   NoConsdouble            0.0441832  0.0086355   5.116 9.45e-07 ***
#   logWordFormFreqAllCoca  0.0010485  0.0023864   0.439 0.661032    
# LocSpeech              -0.0036187  0.0007998  -4.524 1.23e-05 ***
#   StressPatternstr-unstr -0.0350258  0.0090331  -3.877 0.000158 ***
#   PCDec1                 -0.0011385  0.0024419  -0.466 0.641725    
# PCDec2                  0.0080390  0.0034892   2.304 0.022608 *  


# let's throw out Frq

imPC.lm3<-lm(bc ~ NoCons + LocSpeech + StressPattern + PCDec1+ PCDec2, 
             data = imComplex5)


summary(imPC.lm3)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.3314087  0.0116393  28.473  < 2e-16 ***
#   NoConsdouble            0.0457974  0.0077940   5.876 2.62e-08 ***
#   LocSpeech              -0.0036285  0.0007974  -4.551 1.10e-05 ***
#   StressPatternstr-unstr -0.0344243  0.0089047  -3.866 0.000164 ***
#   PCDec1                 -0.0016866  0.0020936  -0.806 0.421757    
# PCDec2                  0.0082381  0.0034504   2.388 0.018204 *

# let's throw out PCDec1

imPC.lm4<-lm(bc ~ NoCons + LocSpeech + StressPattern + PCDec2, 
             data = imComplex5)


summary(imPC.lm4)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.3304134  0.0115600  28.582  < 2e-16 ***
#   NoConsdouble            0.0486089  0.0069608   6.983 8.57e-11 ***
#   LocSpeech              -0.0036036  0.0007958  -4.528 1.20e-05 ***
#   StressPatternstr-unstr -0.0382870  0.0074948  -5.108 9.67e-07 ***
#   PCDec2                  0.0071446  0.0031684   2.255   0.0256 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0352 on 151 degrees of freedom
# Multiple R-squared:  0.5189,	Adjusted R-squared:  0.5061 
# F-statistic: 40.71 on 4 and 151 DF,  p-value: < 2.2e-16


# what about an interactions
final_imPCint.lm<-lm(bc ~ NoCons*PCDec2 + LocSpeech + StressPattern , 
                  data = imComplex5)


summary(final_imPCint.lm)

# no influence!!!! 

# so, this the final model looks basically the same

################
# I need to report the final PC -model

# I need the output in Latex

table_final_model_im_PC<-as.data.frame(coef(summary(imPC.lm4)))

xtable(table_final_model_im_PC,digits = 3)


#probably show a plot...



#change directory for plots

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Schriften/Diss/images/Corpus")


png("imPCAbsPC2.png", units="cm", height=15, width=15, res=300, pointsize=15)

par(mfrow=c(1,1))
visreg (imPC.lm4, "PCDec2",trans= function(x) x^(1/lambda)*1000,main="", rug=F ,ylab="duration in milliseconds", xlab="PC 2", ylim=c(20,130), cex.axis=0.9)

dev.off()

decomposability.pc2 <- prcomp(imComplex5[, c( "ScaledRelFreq","ScaledMedian","ScaledTypeOfBase","ScaledMorphBound")])
summary(decomposability.pc2)
# Importance of components:
#                         PC1    PC2     PC3     PC4
# Standard deviation     1.6572 0.8089 0.63204 0.44688
# Proportion of Variance 0.6866 0.1636 0.09987 0.04992
# Cumulative Proportion  0.6866 0.8502 0.95008 1.00000

# So, PC 1 and 2 are very important

decomposability.pc2$rotation
#                       PC1         PC2         PC3        PC4
# ScaledRelFreq    0.4152908 -0.88947303  0.08229384  0.1720437
# ScaledMedian     0.5289737  0.33268873 -0.43244797  0.6499952
# ScaledTypeOfBase 0.4958853  0.30338080  0.81347850 -0.0176225
# ScaledMorphBound 0.5493798  0.07820506 -0.38009105 -0.7399977

# 1 is mostly median + ST and type of base, 2 is RelFrq

imComplex5$PCDec2 <- decomposability.pc2$x[, 1]

# let's see whether this has an influence


final_imPC2.lm<-lm(bc ~ NoCons + LocSpeech + StressPattern + PCDec2, 
                  data = imComplex5)


summary(final_imPC2.lm)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + PCDec2, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.098600 -0.024963 -0.001681  0.023539  0.081584 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)              0.3280685  0.0117331  27.961  < 2e-16 ***
#   NoConsdouble            0.0577159  0.0060608   9.523  < 2e-16 ***
#   LocSpeech              -0.0036569  0.0008096  -4.517 1.26e-05 ***
#   StressPatternstr-unstr -0.0466424  0.0073833  -6.317 2.83e-09 ***
#   PCDec2                 -0.0004554  0.0019241  -0.237    0.813    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03578 on 151 degrees of freedom
# Multiple R-squared:  0.5029,	Adjusted R-squared:  0.4897 
# F-statistic: 38.19 on 4 and 151 DF,  p-value: < 2.2e-16

# NO!


# what if we take affix into account


final_imPC3.lm<-lm(bc ~ NoCons + LocSpeech + StressPattern + Affix+PCDec2, 
                   data = imComplex5)


summary(final_imPC3.lm)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix + 
#        PCDec2, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.090917 -0.023964 -0.001896  0.023636  0.080455 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3367307  0.0122850  27.410  < 2e-16 ***
#   NoConsdouble            0.0470886  0.0077856   6.048 1.12e-08 ***
#   LocSpeech              -0.0034826  0.0008043  -4.330 2.72e-05 ***
#   StressPatternstr-unstr -0.0359112  0.0088584  -4.054 8.06e-05 ***
#   AffixinLoc             -0.0173207  0.0081052  -2.137   0.0342 *  
#   PCDec2                  0.0007725  0.0019867   0.389   0.6980    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03537 on 150 degrees of freedom
# Multiple R-squared:  0.5176,	Adjusted R-squared:  0.5015 
# F-statistic: 32.18 on 5 and 150 DF,  p-value: < 2.2e-16

# NO, affix is still a better predictor


# I should do a Mumin


imComplexlmMuMin<- lm(AbsDurCon~ NoCons +logWordFormFreqAllCoca+ LocSpeech +PCDec+ PrecSegDur + 
                         StressPattern + Affix, data = imComplex5)




options(na.action = "na.fail") 

model_ranking <- dredge(imComplexlmMuMin)

model_average_<-model.avg(model_ranking)

summary(model_average_)

# Relative variable importance: 
#                   NoCons LocSpeech StressPattern Affix PCDec logWordFormFreqAllCoca PrecSegDur
# Importance:          1.00   1.00      1.00          0.79  0.29  0.28                   0.27      
# N containing models:   64     64        64            64    64    64    64  


# I need to do the same witrh RelDur

imComplex5$RelDur<-as.numeric(imComplex5$AbsDurCon/imComplex5$PrecSegDur)

imComplex6<- imComplex5[is.finite(imComplex5$RelDur),]


imPC.lmRel<-lm(RelDur ~ NoCons +logWordFormFreqAllCoca+ LocSpeech + StressPattern + PCDec1+ PCDec2, 
            data = imComplex6)


summary(imPC.lmRel)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.10807    0.45171   0.239 0.811251    
# NoConsdouble            0.51134    0.18145   2.818 0.005491 ** 
#   logWordFormFreqAllCoca  0.05423    0.04970   1.091 0.276959    
# LocSpeech               0.05615    0.01675   3.352 0.001019 ** 
#   StressPatternstr-unstr -0.66626    0.18883  -3.528 0.000557 ***
#   PCDec1                 -1.50595    0.99103  -1.520 0.130750    
# PCDec2                 -1.46275    0.97267  -1.504 0.134753    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.737 on 148 degrees of freedom
# Multiple R-squared:  0.3846,	Adjusted R-squared:  0.3596 
# F-statistic: 15.42 on 6 and 148 DF,  p-value: 1.1e-13

# let's throw out freq

imPC.lmRel2<-lm(RelDur ~ NoCons + LocSpeech + StressPattern + PCDec1+ PCDec2, 
               data = imComplex6)


summary(imPC.lmRel2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.52369    0.24298   2.155 0.032745 *  
#   NoConsdouble            0.59646    0.16393   3.639 0.000377 ***
#   LocSpeech               0.05573    0.01676   3.325 0.001112 ** 
#   StressPatternstr-unstr -0.63666    0.18699  -3.405 0.000851 ***
#   PCDec1                 -1.65693    0.98195  -1.687 0.093623 .  
# PCDec2                 -1.58596    0.96671  -1.641 0.102996 


# let's throw out Pc2

imPC.lmRel3<-lm(RelDur ~ NoCons + LocSpeech + StressPattern + PCDec1, 
                data = imComplex6)


summary(imPC.lmRel3)

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.49642    0.24377   2.036  0.04347 *  
#   NoConsdouble            0.76781    0.12706   6.043 1.15e-08 ***
#   LocSpeech               0.05382    0.01681   3.201  0.00167 ** 
#   StressPatternstr-unstr -0.81069    0.15486  -5.235 5.47e-07 ***
#   PCDec1                 -0.04732    0.04054  -1.167  0.24496    
# ---



# and Dec 1


imPC.lmRel4<-lm(RelDur ~ NoCons + LocSpeech + StressPattern, 
                data = imComplex6)


summary(imPC.lmRel4)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.47705    0.24350   1.959  0.05194 .  
# NoConsdouble            0.80861    0.12230   6.612 6.17e-10 ***
#   LocSpeech               0.05485    0.01681   3.263  0.00136 ** 
#   StressPatternstr-unstr -0.88585    0.14100  -6.282 3.38e-09 ***
#   ---
  

# so same model as before

table_final_model_im_PC<-as.data.frame(coef(summary(imPC.lmRel4)))

xtable(table_final_model_im_PC,digits = 3)



# now MuMin

imComplexlmMuMinRel<- lm(RelDur ~ NoCons*PCDec1 +NoCons*PCDec2 
                         +logWordFormFreqAllCoca+ LocSpeech + 
                       + NoCons*StressPattern
                       , data = imComplex6)



options(na.action = "na.fail") 

model_ranking <- dredge(imComplexlmMuMinRel)

model_average_<-model.avg(model_ranking)

summary(model_average_)

# Relative variable importance: 
#                    NoCons StressPattern LocSpeech PCDec1 PCDec2 NoCons:StressPattern
# Importance:          1.00   0.99          0.98      0.48   0.46   0.41                
# N containing models:  108     88            70        88     88     36                
# logWordFormFreqAllCoca NoCons:PCDec1 NoCons:PCDec2
# Importance:          0.39                   0.19          0.18         
# N containing models:   70                     36            36  
