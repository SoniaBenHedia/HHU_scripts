
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


###############################################################################
# Also: Note: I tested the influence of different decomposability measurments##
# It has been shown that Median, SemTrans and RelFreq are correlated. There
# is a strong relation between Affix, ST and the rating. However, Affix and ST
# seem to predict duration better than the median od the rating.LSA is not a valid
# measurment for decomposability AND it does not have aby effect on duration
# We weill include ST, median and RelFreq.
###############################################################################

############################################################################################
#                     MERGE  with the rating                                               #
############################################################################################

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/Decomposability")


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
imComplex3<-merge(imComplex2, rating, by.x="item", by.y="Item")

table(rating$Item)

imComplex3$item<-droplevels(imComplex3$item)
table(imComplex3$item)

# it seems the rating for imminant got lost, thus we lose one observation

dim(imComplex3)
#[1] 156  61

str(imComplex3)

###################################################################
#              Data distribution                                 ##
###################################################################

# Before doing an initial model, let's look at the number of types and tokens 
# in each category

table(imComplex3$NoCons)
#single double 
#67     89 

# Finding out the number of types for double and single 

imComplex3Double<- imComplex3[imComplex3$NoCons=="double",]

imComplex3Single<- imComplex3[imComplex3$NoCons=="single",]

imComplex3Double$item<- factor(imComplex3Double$item)

imComplex3Single$item<- factor(imComplex3Single$item)

levels(imComplex3Double$item)
#[1] "immaculate"  "immaterial"  "immature"    "immediacy"   "immediate"   "immediately"
#[7] "immemorial"  "immigrant"   "immigrants"  "immigrate"   "immigrated"  "immigration"
#[13] "immobilized" "immoral"     "immorality"  "immortal"  

str(imComplex3Double$item)

#Factor w/ 16 levels "immaculate","immaterial",..: 5 6 5 9 6 12 10 8 9 5 ...

# 16 types for m#mV

str(imComplex3Single$item)

#Factor w/ 67 levels "imbalance","impact",..: 56 2 48 43 32 45 63 54 53 59 ...

# 67 types for m#p/b

# Let's create the variable logAbsDurCon - just for the sake of being able
# to get a summary of that variable

imComplex3$logAbsDurCon <- log(imComplex3$AbsDurCon)

###########################################################################
#  Now - let's get the summaries of the variables which are in our initial#
# model                                                                   #
###########################################################################



summary (imComplex3$logAbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.902  -2.873  -2.604  -2.644  -2.399  -1.777


sd (imComplex3$logAbsDurCon)
#[1] 0.3784319



summary (imComplex3$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02020 0.05652 0.07395 0.07595 0.09079 0.16920 

sd(imComplex3$AbsDurCon)
#0.02705768

sd (imComplex3Single$AbsDurCon)
#[1] 0.01869767

summary (imComplex3Single$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02020 0.04840 0.06083 0.06107 0.07503 0.09901


sd (imComplex3Double$AbsDurCon)
#[1] 0.02705497

summary (imComplex3Double$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.03674 0.07025 0.08146 0.08714 0.10460 0.16920 


# t-test

t.test(AbsDurCon~NoCons, data=imComplex3)


# Welch Two Sample t-test
# 
# data:  AbsDurCon by NoCons
# t = 7.1122, df = 152.98, p-value = 4.106e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01883278 0.03331933
# sample estimates:
#   mean in group m#mV  mean in group m#C 
#         0.08714493         0.06106888 

summary (imComplex3$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.609   7.124   9.028   8.582   9.733  10.700 

sd(imComplex3$logWordFormFreqAllCoca)
#[1] 1.773862

summary (imComplex$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-9.6700 -0.2115  3.2570  3.5990  7.7140 10.4900 

sd (imComplex3$logRelFreq)
#[1]  4.742333

summary (imComplex3$WordDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2575  0.4639  0.5617  0.5739  0.6658  0.9748

sd (imComplex3$WordDur)
#[1] 0.1508541


summary (imComplex3$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.03923 0.05690 0.06078 0.07765 0.12680  

sd(imComplex3$PrecSegDur)
#[1] 0.02560936


summary (imComplex3$NoSegWord)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.00    7.00    8.00    7.84    9.00   13.00

sd (imComplex3$NoSegWord)
#[1]  1.705766



summary (imComplex3$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.279  12.020  14.070  14.290  16.590  24.320 

sd (imComplex3$LocSpeech)
#[1] 3.665657

summary (imComplex3$Affix)
#inLoc inNeg   
#70    86

summary(imComplex3Single$Affix)
# inLoc inNeg 
# 49    18 


get_number_types(imComplex3Single, "Affix", "inLoc")
#[1] 49

get_number_types(imComplex3Single, "Affix", "inNeg")
# 18

summary(imComplex3Double$Affix)
# inLoc inNeg 
# 21    68 

get_number_types(imComplex3Double, "Affix", "inLoc")
#5

get_number_types(imComplex3Double, "Affix", "inNeg")
# 11

summary (imComplex3$MorphBound)
#opaque transparent 
#105         51 

summary (imComplex3$NoCons)
#m     m#m 
#67     89 

summary(imComplex3$StressPattern)
#d/u-str str-unstr 
#117        39 

summary(imComplex3$median)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.628   3.000   4.000 




# Let's got some distributional data from Loc and Neg in


inNeg<-imComplex3[imComplex3$Affix=="inNeg",]
inLoc<-imComplex3[imComplex3$Affix=="inLoc",]

# First neg

summary (inNeg$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.03035 0.07358 0.08381 0.08930 0.10490 0.16920 


sd (inNeg$AbsDurCon)
#[1]0.02614086

summary (inNeg$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   7.772  11.580  13.540  13.970  16.490  23.950 

 

sd (inNeg$LocSpeech)
#[1]3.482212

summary (inNeg[inNeg$NoCons=="double",]$AbsDurCon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05614 0.07773 0.09084 0.09566 0.10950 0.16920 

sd(inNeg[inNeg$NoCons=="double",]$AbsDurCon)
#0.02435344

summary (inNeg[inNeg$NoCons=="single",]$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.03035 0.05022 0.06975 0.06528 0.07677 0.09669 


sd(inNeg[inNeg$NoCons=="single",]$AbsDurCon)
#[1] 0.01750924

# let's also see how many doubles and singles we have

table(inNeg$NoCons)
# single double 
# 18     68 

# THAT IS INTERESTING

# Let's have a look at the types:

get_types(inNeg,"NoCons","double")
# [1] immaculate  immaterial  immature    immediacy   immediate   immediately immemorial 
# [8] immobilized immoral     immorality  immortal

get_types(inNeg,"NoCons","single")
# [1] imbalance     impair        impaired      impairing     impairment    impartial    
# [7] impatient     impeccable    imperfect     imperfections impersonal    
# [13] impolite      impossibility impossible    impotence     impractical   improper     
# [19] improperly 


# typewise it looks okay!

# inLoc

summary (inLoc$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02020 0.04920 0.05800 0.05954 0.07131 0.09901 


sd (inLoc$AbsDurCon)
#[1]0.01754017

summary (inLoc$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  5.279  12.770  14.630  14.680  16.920  24.320 
 

sd (inLoc$LocSpeech)
#[1]3.869009

summary (inLoc[inLoc$NoCons=="double",]$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.03674 0.05069 0.05540 0.05959 0.07025 0.08687 


sd(inLoc[inLoc$NoCons=="double",]$AbsDurCon)
#0.01378758

summary (inLoc[inLoc$NoCons=="single",]$AbsDurCon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02020 0.04756 0.05934 0.05952 0.07180 0.09901 


sd(inLoc[inLoc$NoCons=="single",]$AbsDurCon)
#0.01905386

table(inLoc$NoCons)
# single double 
# 49     21 

get_types(inLoc,"NoCons","double")
#[1] immigrant   immigrants  immigrate   immigrated  immigration

get_types(inLoc,"NoCons","single")
# [1] impact         impacted       impaneled      impaneling     impart        
# [6] imparted       imparting      impede         impediment     impelled      
# [11] impersonation  implant        implement      implementation implemented   
# [16] implementing   implication    implications   implicit       implied       
# [21] implies        imply          implying       import         imported      
# [26] importer       importers      importing      imports        impose        
# [31] imposed        imposes        imposing       imposition     impoverished  
# [36] impregnated    impress        impressed      impresses      impression    
# [41] impressionable impressions    impressive     imprint        imprinted     
# [46] imprisoned     imprisonment   impulse        impulsive


######################################################################################
#                 fitting a model                                                    #
######################################################################################

# Due to the imnestedness of the data, there is no point in including
# either Speaker or Item as a random effect. This would lead to a seriously
# overpowered model in which almost everything can be predicted on the 
# basis of the random effect structure alone:

tmp.lmer <- lmer(AbsDurCon ~ 1 + (1|item) + (1|Speaker), data = imComplex3)
cor(imComplex3$AbsDurCon, fitted(tmp.lmer))^2
#[1] 0.9743843


tmp.lmer <- lmer(AbsDurCon ~ 1 + (1|item), data = imComplex3)
cor(imComplex3$AbsDurCon, fitted(tmp.lmer))^2
#[1] 0.5693692



##              Do an initial model:

imComplex.lm1 <- lm (AbsDurCon ~ NoCons+ logWordFormFreqAllCoca + logRelFreq + LocSpeech + PrecSegDur + median + Affix + MorphBound + StressPattern, data = imComplex3)

summary(imComplex.lm1)    

# Call:
#   lm(formula = AbsDurCon ~ NoCons + logWordFormFreqAllCoca + logRelFreq + 
#        LocSpeech + PrecSegDur + median + Affix + MorphBound + StressPattern, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.038597 -0.012721 -0.003152  0.011602  0.059270 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.082e-01  1.725e-02   6.273 3.80e-09 ***
#   NoConsdouble            2.474e-02  5.071e-03   4.879 2.76e-06 ***
#   logWordFormFreqAllCoca  1.687e-05  1.285e-03   0.013 0.989542    
#   logRelFreq              2.499e-04  5.322e-04   0.470 0.639287    
#   LocSpeech              -1.930e-03  5.230e-04  -3.690 0.000315 ***
#   PrecSegDur             -2.216e-02  7.416e-02  -0.299 0.765546    
#   median                 -5.472e-03  3.123e-03  -1.752 0.081806 .  
#   AffixinNeg              6.966e-03  4.578e-03   1.522 0.130253    
#   MorphBoundtransparent  -1.245e-02  6.633e-03  -1.877 0.062529 .  
#   StressPatternstr-unstr -1.537e-02  5.165e-03  -2.976 0.003420 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01933 on 146 degrees of freedom
# Multiple R-squared:  0.5194,	Adjusted R-squared:  0.4897 
# F-statistic: 17.53 on 9 and 146 DF,  p-value: < 2.2e-16


# Es scheint auf den ersten Blick so, als hätten NoCons, LocSpeech, Affix
# und stress pattern einen Einfluss,aber wir haben im decomposability script gesehen,
# dass  zwischen Affix, MorphBound und median komplexe Beziehungen bestehen.

# 1. Affix and MorphBound

# Ich möchte schauen, inwiefern MorphBound durch Affix "beeinflusst wird" 

# lets see what they do individually

imComplex1.lmAffix <- lm (AbsDurCon ~  Affix, data = imComplex3)
summary(imComplex1.lmAffix) 


# Call:
#   lm(formula = AbsDurCon ~ Affix, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.058946 -0.014219 -0.003305  0.012717  0.079915 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.059542   0.002712  21.951  < 2e-16 ***
#   AffixinNeg  0.029755   0.003653   8.145 1.21e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02269 on 154 degrees of freedom
# Multiple R-squared:  0.3011,	Adjusted R-squared:  0.2965 
# F-statistic: 66.34 on 1 and 154 DF,  p-value: 1.211e-13

# so negative ins are longer

imComplex1.lmMorphBound <- lm (AbsDurCon ~  MorphBound, data = imComplex3)
summary(imComplex1.lmMorphBound) 

# Call:
#   lm(formula = AbsDurCon ~ MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.060798 -0.017915 -0.002959  0.013980  0.088218 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)            0.080994   0.002552   31.74  < 2e-16 ***
#   MorphBoundtransparent -0.015442   0.004463   -3.46 0.000699 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02615 on 154 degrees of freedom
# Multiple R-squared:  0.07213,	Adjusted R-squared:  0.06611 
# F-statistic: 11.97 on 1 and 154 DF,  p-value: 0.0006988


# und opaque scheinen länger ABER Unterschied minimal und ich glaube hängt sehr von
# immediate ab - let's test it

# immediate and immediately make up 51!!!!! of the 105 opaque items,
#so we need to be careful with the interpretation! 


# What happens if we include both?

imComplex1.lmMorphBoundAffix <- lm (AbsDurCon ~  MorphBound + Affix, data = imComplex3)
summary(imComplex1.lmMorphBoundAffix) 


# Call:
#   lm(formula = AbsDurCon ~ MorphBound + Affix, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.050719 -0.014302 -0.003482  0.012449  0.076912 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.064034   0.003057  20.950  < 2e-16 ***
#   MorphBoundtransparent -0.011230   0.003819  -2.941  0.00378 ** 
#   AffixinNeg             0.028266   0.003601   7.849  6.8e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02215 on 153 degrees of freedom
# Multiple R-squared:  0.3385,	Adjusted R-squared:  0.3298 
# F-statistic: 39.14 on 2 and 153 DF,  p-value: 1.87e-14


# Both have a significant effect and MorphBound effect becomes smaller.
# Affix seems to be pretty robust BUT we need to be cautious because of
# the distribution

par(mfrow=c(1,2))
visreg(imComplex1.lmMorphBoundAffix)
par(mfrow=c(1,1))

### Der Effeckt for MorphBound ist nicht stabiul und könnte wirklich an immediate liegen!
# Außerdem ist ein zusätzliches Problem, dass die meisten InNegs doubles sind, sowie
# viele der opaques


# we cannot look at them without taking into account the numbers of consonants.
# It is a complex distribution which means that it does not make to much sense
# to look at factors in isolation.

# What happens if we let them interact?
# interaction:

imComplex1.lmInt1 <- lm (AbsDurCon ~  MorphBound * Affix, data = imComplex3)
summary(imComplex1.lmInt1) 

# Call:
#   lm(formula = AbsDurCon ~ MorphBound * Affix, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.049121 -0.013981 -0.002328  0.012589  0.073842 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       0.059429   0.003326  17.868  < 2e-16 ***
#   MorphBoundtransparent             0.000284   0.005259   0.054  0.95700    
#   AffixinNeg                        0.035942   0.004294   8.371 3.48e-14 ***
#   MorphBoundtransparent:AffixinNeg -0.022995   0.007432  -3.094  0.00235 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02155 on 152 degrees of freedom
# Multiple R-squared:  0.3777,	Adjusted R-squared:  0.3654 
# F-statistic: 30.75 on 3 and 152 DF,  p-value: 1.361e-15


# the interaction is significant:

# however we don't know how robust this interaction will be later. Let's keep it in 
# mind

# Median and SemanticTranspareny and Affix


imComplex.lmInt2 <- lm (AbsDurCon ~  median * Affix, data = imComplex3)
summary(imComplex.lmInt2) 


# Call:
#   lm(formula = AbsDurCon ~ median * Affix, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.056799 -0.013756 -0.001888  0.013370  0.075625 
# 
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        0.074160   0.009124   8.128 1.42e-13 ***
#   median            -0.005357   0.003206  -1.671 0.096759 .  
#   AffixinNeg        -0.008956   0.011470  -0.781 0.436156    
#   median:AffixinNeg  0.014818   0.004109   3.606 0.000421 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02171 on 152 degrees of freedom
# Multiple R-squared:  0.3689,	Adjusted R-squared:  0.3565 
# F-statistic: 29.62 on 3 and 152 DF,  p-value: 3.885e-15


visreg(imComplex.lmInt2, "median",by="Affix")

# that is actually quite interesting. It looks like for negIn it makes adifference
# whether it is perceived as easy or difficult to segment, while that is not the
# case for inLoc - however we do not have that many types for each caegory, so better be cautios



imComplex.lmInt3 <- lm (AbsDurCon ~  MorphBound * median, data = imComplex3)
summary(imComplex.lmInt3) 
# no interaction

# so there might be an interaction between Affix and median (keep that in mind)



# 2. The frequencies

#  The frequencies do not seem to have a huge effect. However
# as we stated above we might also have a collinearity effect.
# Thus we test the freqeuncies

cor.test(imComplex3$logWordFormFreqAllCoca, imComplex3$logRelFreq)

# Pearson's product-moment correlation
# 
# data:  imComplex3$logWordFormFreqAllCoca and imComplex3$logRelFreq
# t = 9.3006, df = 154, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.4886317 0.6916795
# sample estimates:
#   cor 
# 0.5997233 


imComplex3.lmFreq1 <- lm (AbsDurCon ~ logWordFormFreqAllCoca , data = imComplex3)
summary(imComplex3.lmFreq1) 

# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.059602 -0.016743 -0.001641  0.013002  0.086851 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.028102   0.010025   2.803  0.00571 ** 
#   logWordFormFreqAllCoca 0.005575   0.001144   4.873 2.71e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02527 on 154 degrees of freedom
# Multiple R-squared:  0.1336,	Adjusted R-squared:  0.128 
# F-statistic: 23.74 on 1 and 154 DF,  p-value: 2.711e-06

imComplex3.lmFreq2 <- lm (AbsDurCon ~ logRelFreq , data = imComplex3)
summary(imComplex3.lmFreq2) 


# Call:
#   lm(formula = AbsDurCon ~ logRelFreq, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069890 -0.015765 -0.002839  0.013630  0.083240 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.0672424  0.0024484  27.464  < 2e-16 ***
#   logRelFreq  0.0024617  0.0004148   5.935 1.87e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02449 on 154 degrees of freedom
# Multiple R-squared:  0.1862,	Adjusted R-squared:  0.1809 
# F-statistic: 35.22 on 1 and 154 DF,  p-value: 1.875e-08



imComplex3.lmFreq3 <- lm (AbsDurCon ~ logWordFormFreqAllCoca + logRelFreq , data = imComplex3)
summary(imComplex3.lmFreq3) 

# Call:
#   lm(formula = AbsDurCon ~ logWordFormFreqAllCoca + logRelFreq, 
#      data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.06831 -0.01524 -0.00357  0.01290  0.08263 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.0474390  0.0109814   4.320  2.8e-05 ***
#   logWordFormFreqAllCoca 0.0025426  0.0013750   1.849 0.066362 .  
# logRelFreq             0.0018913  0.0005143   3.677 0.000326 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0243 on 153 degrees of freedom
# Multiple R-squared:  0.2039,	Adjusted R-squared:  0.1935 
# F-statistic:  19.6 on 2 and 153 DF,  p-value: 2.644e-08



# we use only RelFreq in model because it has the bigger effect when being the
# only predctor and when both frequency predictors are in a model. They do not
# supress each other though!

# 4. StressPattern and LocSpeech

# stress Pattern
imComplex3.lmStressPattern<- lm (AbsDurCon ~ StressPattern , data = imComplex3)
summary(imComplex3.lmStressPattern)

# Call:
#   lm(formula = AbsDurCon ~ StressPattern, data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.06080 -0.01435 -0.00376  0.01379  0.08734 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.081871   0.002321  35.277  < 2e-16 ***
#   StressPatternstr-unstr -0.023702   0.004642  -5.106 9.57e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0251 on 154 degrees of freedom
# Multiple R-squared:  0.1448,	Adjusted R-squared:  0.1392 
# F-statistic: 26.08 on 1 and 154 DF,  p-value: 9.574e-07



# StressPattern: highly significant

# LocSpeech
imComplex3.lmLocSpeech<- lm (AbsDurCon ~ LocSpeech , data = imComplex3)
summary(imComplex3.lmLocSpeech) 

# Call:
#   lm(formula = AbsDurCon ~ LocSpeech, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.055132 -0.017053 -0.002616  0.013231  0.080706 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.1038285  0.0084592  12.274  < 2e-16 ***
#   LocSpeech   -0.0019517  0.0005736  -3.402 0.000852 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02618 on 154 degrees of freedom
# Multiple R-squared:  0.06991,	Adjusted R-squared:  0.06387 
# F-statistic: 11.58 on 1 and 154 DF,  p-value: 0.0008517

# LocSpeech: significant


# LocSpeech and StressPattern
imComplex3.lmLocSpeechStressPattern <- lm (AbsDurCon ~ LocSpeech +StressPattern , data = imComplex3)
summary(imComplex3.lmLocSpeechStressPattern)

# Call:
#   lm(formula = AbsDurCon ~ LocSpeech + StressPattern, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.055977 -0.016400 -0.002427  0.012499  0.078783 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.1016357  0.0079806  12.735  < 2e-16 ***
#   LocSpeech              -0.0014274  0.0005524  -2.584   0.0107 *  
#   StressPatternstr-unstr -0.0211868  0.0046610  -4.546 1.11e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02465 on 153 degrees of freedom
# Multiple R-squared:  0.1806,	Adjusted R-squared:  0.1699 
# F-statistic: 16.86 on 2 and 153 DF,  p-value: 2.419e-07



# Nothing changes

#thus we can keep both in without worrying about supression effects

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################

# Now we have dealt with all collinearity problems: 
#- threw out one frequency variable
# decided to look at the interaction of ST and Affix, as well as ST and median later
# leave Stress and LocSpeech in


# We throw out WordFormFreq
imComplex.lm2 <- lm (AbsDurCon ~ NoCons+ logRelFreq + LocSpeech+ PrecSegDur + median+ StressPattern+ MorphBound + Affix, data = imComplex3)
summary(imComplex.lm2)

# Call:
#   lm(formula = AbsDurCon ~ NoCons + logRelFreq + LocSpeech + PrecSegDur + 
#        median + StressPattern + MorphBound + Affix, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.038628 -0.012745 -0.003142  0.011583  0.059261 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.1083019  0.0143825   7.530 4.73e-12 ***
#   NoConsdouble            0.0247710  0.0045689   5.422 2.37e-07 ***
#   logRelFreq              0.0002516  0.0005152   0.488 0.625997    
#   LocSpeech              -0.0019305  0.0005206  -3.708 0.000295 ***
#   PrecSegDur             -0.0221727  0.0738945  -0.300 0.764556    
#   median                 -0.0054701  0.0031070  -1.761 0.080390 .  
#   StressPatternstr-unstr -0.0153600  0.0050941  -3.015 0.003026 ** 
#   MorphBoundtransparent  -0.0124634  0.0065269  -1.910 0.058140 .  
#   AffixinNeg              0.0069665  0.0045620   1.527 0.128897    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01926 on 147 degrees of freedom
# Multiple R-squared:  0.5194,	Adjusted R-squared:  0.4932 
# F-statistic: 19.86 on 8 and 147 DF,  p-value: < 2.2e-16

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (imComplex.lm2))
qqline (residuals (imComplex.lm2))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Use log(AbsDurCon):
imComplex3.lm3 <- lm (log(AbsDurCon) ~ NoCons+ logRelFreq + LocSpeech + PrecSegDur + median + StressPattern+MorphBound + Affix, data = imComplex3)
summary(imComplex3.lm3)

# Call:
#   lm(formula = log(AbsDurCon) ~ NoCons + logRelFreq + LocSpeech + 
#        PrecSegDur + median + StressPattern + MorphBound + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.81672 -0.16107 -0.00513  0.18969  0.59896 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)            -2.190e+00  1.999e-01 -10.954  < 2e-16 ***
#   NoConsdouble            3.783e-01  6.351e-02   5.957 1.82e-08 ***
#   logRelFreq             -3.468e-06  7.160e-03   0.000 0.999614    
#   LocSpeech              -2.407e-02  7.236e-03  -3.326 0.001113 ** 
#   PrecSegDur              3.149e-01  1.027e+00   0.307 0.759582    
#   median                 -9.810e-02  4.319e-02  -2.271 0.024570 *  
#   StressPatternstr-unstr -2.654e-01  7.081e-02  -3.748 0.000255 ***
#   MorphBoundtransparent  -1.940e-01  9.072e-02  -2.138 0.034138 *  
#   AffixinNeg              7.672e-02  6.341e-02   1.210 0.228245    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2677 on 147 degrees of freedom
# Multiple R-squared:  0.5253,	Adjusted R-squared:  0.4995 
# F-statistic: 20.33 on 8 and 147 DF,  p-value: < 2.2e-16


# effects remain the same in total - but median and ST become sign 

qqnorm (residuals (imComplex3.lm3))
qqline (residuals (imComplex3.lm3))

# it looks better on the top but still now good for the -1 till -0.5
# quantile, let's identify the outliers


#Check which ones they are

# use identify() to find out the line numbers of selected points:
#outliersRes <- identify(qqnorm (residuals (imComplex3.lm3)))
#outliersRes
#[1]  98 114 118 140 144

imComplex3 [c(98, 114, 118 ,140, 144), ]

# I can't detect why these should behave any different then the other, not very long
# WordDur or anything

# I'll exclude them from the dataset for now to meet the assumptions of the model



# we remove outliers
imComplex4 <- imComplex3 [-c(98, 114, 118 ,140, 144), ]
dim(imComplex4)
#[1] 151  62


#Let's make a new model with the new dataset and check the model's assumptions


imComplex.lm4 <- lm (log(AbsDurCon) ~ NoCons+ logRelFreq + LocSpeech + PrecSegDur + median + StressPattern+MorphBound + Affix, data = imComplex4)
summary(imComplex.lm4)

# Call:
#   lm(formula = log(AbsDurCon) ~ NoCons + logRelFreq + LocSpeech + 
#        PrecSegDur + median + StressPattern + MorphBound + Affix, 
#      data = imComplex4)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50663 -0.16645 -0.02661  0.14813  0.55676 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)            -2.149324   0.173659 -12.377  < 2e-16 ***
#   NoConsdouble            0.283422   0.056433   5.022 1.51e-06 ***
#   logRelFreq              0.002974   0.006326   0.470 0.638970    
#   LocSpeech              -0.024516   0.006260  -3.916 0.000139 ***
#   PrecSegDur             -0.687016   0.896143  -0.767 0.444570    
#   median                 -0.074893   0.038083  -1.967 0.051184 .  
#   StressPatternstr-unstr -0.210568   0.065048  -3.237 0.001503 ** 
#   MorphBoundtransparent  -0.147522   0.081069  -1.820 0.070911 .  
#   AffixinNeg              0.090035   0.054995   1.637 0.103813    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2288 on 142 degrees of freedom
# Multiple R-squared:  0.5326,	Adjusted R-squared:  0.5063 
# F-statistic: 20.23 on 8 and 142 DF,  p-value: < 2.2e-16

# But first: Let's check the model's assumptions

qqnorm (residuals (imComplex.lm4))

qqline (residuals (imComplex.lm4))

# yay- that's okay!


# However maybe a box-cox transformation will lead to an even more satisfactory
# distribuition of res. Let's try


bc <- boxcox(imComplex.lm2)

lambda <- bc$x[which.max(bc$y)]

imComplex3$bc <- imComplex3$AbsDurCon^lambda

imComplexBC.lm1 <- lm (bc~ NoCons + logRelFreq + LocSpeech + median + PrecSegDur +StressPattern + Affix +MorphBound, data = imComplex3)

summary(imComplexBC.lm1)

# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        PrecSegDur + StressPattern + Affix + MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.090715 -0.022908 -0.003274  0.023558  0.079682 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3574837  0.0262014  13.644  < 2e-16 ***
#   NoConsdouble            0.0484412  0.0083234   5.820 3.55e-08 ***
#   logRelFreq              0.0002347  0.0009385   0.250 0.802840    
#   LocSpeech              -0.0034020  0.0009485  -3.587 0.000455 ***
#   median                 -0.0116791  0.0056602  -2.063 0.040835 *  
#   PrecSegDur              0.0007508  0.1346180   0.006 0.995557    
#   StressPatternstr-unstr -0.0321933  0.0092802  -3.469 0.000686 ***
#   AffixinNeg              0.0118395  0.0083109   1.425 0.156402    
#   MorphBoundtransparent  -0.0247321  0.0118904  -2.080 0.039262 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03509 on 147 degrees of freedom
# Multiple R-squared:  0.5345,	Adjusted R-squared:  0.5092 
# F-statistic:  21.1 on 8 and 147 DF,  p-value: < 2.2e-16

#let's check the assumptions

qqnorm(residuals(imComplexBC.lm1))
qqline (residuals (imComplexBC.lm1))


# this looks actually pretty good.

# We will work with this model! --> imComplexBC.lm1


######################################
# We should check our variable accent befire we simplify since we suspect some
# problems and we have 3 different accent variables...
# What we will do is to fit 3 models: one for each accet variable and
# we will check whether any of teh variables has any influence
#########################################

#
imComplexBC.lmAccent1 <- lm (bc~ PhrasalAccentSonia+StressPattern, data = imComplex3)
summary(imComplexBC.lmAccent1)

# no


imComplexBC.lmAccent2 <- lm (bc~ AccentIngo+StressPattern, data = imComplex3)
summary(imComplexBC.lmAccent2)


# no

# for the third one, I would like to recode the variable, sonia's imclears
# into no (since she did not hear any accent but just was not sure whether this
# can really be true). If we recode the imsures into nos, we get the clearly
# accented ones on the one hand and all the others on the other


levels(imComplex3$AnyAccentSonia)
#[1] "no"      "imclear" "yes"    

levels(imComplex3$AnyAccentSonia)<- c("no", "no", "yes")

levels(imComplex3$AnyAccentSonia)
#[1] "no"  "yes"

imComplexBC.lmAccent3 <- lm (bc~ AnyAccentSonia+StressPattern, data = imComplex3)
summary(imComplexBC.lmAccent3)


# no

# what about an interaction

imComplexBC.lmAccent4 <- lm (bc~ AnyAccentSonia*StressPattern, data = imComplex3)
summary(imComplexBC.lmAccent4)

# no

#So accent will not play any part in the models but as mentioned before:
# one has to be careful with the interpretation

###########################################################################
#    Later of we also coded for type of root: We should check its influence!
################################################################################

#let's see what happens iof we put in bound root instead of MorphBound. Then we have to
# read in another table

r <- read.table("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/Einzelne_Variablen/Roots/type_of_root.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

imComplex4<- merge(imComplex3, r, by.x="item", by.y="types")

dim(imComplex3)
#[1] 156  63

dim(imComplex4)
#[1] 156  64


# now let#s do that model

imComplexBC.lmRoot<- lm(bc ~ NoCons + LocSpeech + StressPattern+ type_of_base, data = imComplex4)
summary(imComplexBC.lmRoot)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + type_of_base, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.102487 -0.024087 -0.003795  0.023535  0.081909 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.3847518  0.0123043  31.270  < 2e-16 ***
#   NoConsm#C                     -0.0591871  0.0061642  -9.602  < 2e-16 ***
# LocSpeech                     -0.0036484  0.0008065  -4.524 1.22e-05 ***
#   StressPatternbeforeUnstressed -0.0464020  0.0067865  -6.837 1.87e-10 ***
#   type_of_baseword               0.0070086  0.0073800   0.950    0.344    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03568 on 151 degrees of freedom
# Multiple R-squared:  0.5056,	Adjusted R-squared:  0.4925 
# F-statistic: 38.61 on 4 and 151 DF,  p-value: < 2.2e-16

# Type of base does not have an effect - what about a model, in which it
# is the only factor

imComplexBC.lmRoot2<- lm(bc ~ type_of_base, data = imComplex4)
summary(imComplexBC.lmRoot2)

# not even then -> so let's ignore it

#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(imComplexBC.lm1)


# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        PrecSegDur + StressPattern + Affix + MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.090715 -0.022908 -0.003274  0.023558  0.079682 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3574837  0.0262014  13.644  < 2e-16 ***
#   NoConsdouble            0.0484412  0.0083234   5.820 3.55e-08 ***
#   logRelFreq              0.0002347  0.0009385   0.250 0.802840    
#   LocSpeech              -0.0034020  0.0009485  -3.587 0.000455 ***
#   median                 -0.0116791  0.0056602  -2.063 0.040835 *  
#   PrecSegDur              0.0007508  0.1346180   0.006 0.995557    
#   StressPatternstr-unstr -0.0321933  0.0092802  -3.469 0.000686 ***
#   AffixinNeg              0.0118395  0.0083109   1.425 0.156402    
#   MorphBoundtransparent  -0.0247321  0.0118904  -2.080 0.039262 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03509 on 147 degrees of freedom
# Multiple R-squared:  0.5345,	Adjusted R-squared:  0.5092 
# F-statistic:  21.1 on 8 and 147 DF,  p-value: < 2.2e-16


# Let's trim the model

# Now let's throw out PrecSegDur


imComplexBC.lm2<-update(imComplexBC.lm1, ~ . - PrecSegDur)

summary(imComplexBC.lm2)

# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        StressPattern + Affix + MorphBound, data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.09072 -0.02291 -0.00328  0.02355  0.07966 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3575653  0.0216577  16.510  < 2e-16 ***
#   NoConsdouble            0.0484382  0.0082780   5.851 3.01e-08 ***
#   logRelFreq              0.0002349  0.0009350   0.251  0.80201    
#   LocSpeech              -0.0034047  0.0008126  -4.190 4.78e-05 ***
#   median                 -0.0116788  0.0056408  -2.070  0.04015 *  
#   StressPatternstr-unstr -0.0321813  0.0089967  -3.577  0.00047 ***
#   AffixinNeg              0.0118365  0.0082657   1.432  0.15425    
#   MorphBoundtransparent  -0.0247269  0.0118145  -2.093  0.03806 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03497 on 148 degrees of freedom
# Multiple R-squared:  0.5345,	Adjusted R-squared:  0.5125 
# F-statistic: 24.28 on 7 and 148 DF,  p-value: < 2.2e-16


# Model becomes better, otherwise no changes, so let's go on with
# the simplification by throwing out RelFreq

imComplexBC.lm3<-update(imComplexBC.lm2, ~ . - logRelFreq)

summary(imComplexBC.lm3)


# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix + MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.091197 -0.022775 -0.003241  0.023111  0.080371 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3571905  0.0215382  16.584  < 2e-16 ***
#   NoConsdouble            0.0494268  0.0072597   6.808 2.26e-10 ***
#   LocSpeech              -0.0033643  0.0007941  -4.236 3.96e-05 ***
#   median                 -0.0115634  0.0056043  -2.063 0.040819 *  
#   StressPatternstr-unstr -0.0325079  0.0088742  -3.663 0.000346 ***
#   AffixinNeg              0.0121338  0.0081548   1.488 0.138882    
#   MorphBoundtransparent  -0.0257067  0.0111169  -2.312 0.022125 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03486 on 149 degrees of freedom
# Multiple R-squared:  0.5343,	Adjusted R-squared:  0.5156 
# F-statistic:  28.5 on 6 and 149 DF,  p-value: < 2.2e-16

###########################################################################
#           Checking for interactions                                     #
###########################################################################

#This looks good already. Let's see however, whether interactions will
# add do the goodness of the model. There are several interactions which might
# be of interest:

# 1. Affix * MorphBound
# 2. Affix*NoCons
# 3. NoCons*MorphBound
# 4. Affix*median
# 5. Locspeech* NoCons
# 6. Affix*LocSpeech
# 7. StressPattern*Affix
# 8. NoCons*StressPatter



# 1.Affix * MorphBound

imComplexBC.lmIntAffixMorphBound <- lm (bc~ NoCons + LocSpeech + median + StressPattern + Affix*MorphBound, data = imComplex3)

summary(imComplexBC.lmIntAffixMorphBound)

# This interaction is not significant and does not help!

# what if we exclude median


imComplexBC.lmIntAffixMorphBound2<-update(imComplexBC.lmIntAffixMorphBound, ~ . - median)

summary(imComplexBC.lmIntAffixMorphBound2)

# still no interaction


# 2. Affix*NoCons

imComplexBC.lmIntAffixNoCons <- lm (bc~ LocSpeech+median + StressPattern + MorphBound+ Affix* NoCons , data = imComplex3)

summary(imComplexBC.lmIntAffixNoCons)

# This interaction is not significant

# 3. NoCons*MorphBound

imComplexBC.lmIntNoConsMorphBound <- lm (bc~ LocSpeech+median+ StressPattern + Affix + NoCons*MorphBound, data = imComplex3)
 

summary(imComplexBC.lmIntNoConsMorphBound )

# no

# 4.median*Affix

imComplexBC.lmIntAffixMedian <- lm (bc~ LocSpeech+MorphBound+ StressPattern + NoCons +Affix*median, data = imComplex3)

summary(imComplexBC.lmIntAffixMedian )
# This interaction is not significant



# 5. NoCons and LocSpeech 

# Let's see whether we can see an interaction woth LocSpeech
imComplexBC.lmIntNoConsSpeech <- lm (bc~  NoCons*LocSpeech, data = imComplex3)

summary(imComplexBC.lmIntNoConsSpeech)


# NO


# 6. LocSpeech*Affix

# Let's see whether we can see an interaction woth LocSpeech
imComplexBC.lmIntAffixSpeech <- lm (bc~  NoCons+ Affix*LocSpeech, data = imComplex3)

summary(imComplexBC.lmIntAffixSpeech)
# NO



# 7. StressPattern*Affix
imComplexBC.lmIntAffixStress <- lm (bc~ LocSpeech+MorphBound+  NoCons +Affix*StressPattern +median, data = imComplex3)

summary(imComplexBC.lmIntAffixStress )


# no

# 8. NoCons*StressPatter

imComplexBC.lmIntNoConsStress <- lm (bc~ LocSpeech+MorphBound+Affix+  NoCons *StressPattern +median, data = imComplex3)

summary(imComplexBC.lmIntNoConsStress )
# no

##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# We don't find interaction, so we simply simplify further without any interactions

summary(imComplexBC.lm3)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix + MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.091197 -0.022775 -0.003241  0.023111  0.080371 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3571905  0.0215382  16.584  < 2e-16 ***
#   NoConsdouble            0.0494268  0.0072597   6.808 2.26e-10 ***
#   LocSpeech              -0.0033643  0.0007941  -4.236 3.96e-05 ***
#   median                 -0.0115634  0.0056043  -2.063 0.040819 *  
#   StressPatternstr-unstr -0.0325079  0.0088742  -3.663 0.000346 ***
#   AffixinNeg              0.0121338  0.0081548   1.488 0.138882    
#   MorphBoundtransparent  -0.0257067  0.0111169  -2.312 0.022125 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03486 on 149 degrees of freedom
# Multiple R-squared:  0.5343,	Adjusted R-squared:  0.5156 
# F-statistic:  28.5 on 6 and 149 DF,  p-value: < 2.2e-16

# okay now we need to try out several things, because median, ST and Affix
# probably affect eachother a great deal and we need to think about how
# to deal with this. 


# first let's see what happens if we do an lmer (probably not the best idea
# but wee need to control this stupid immediate)

imComplexBC.lmer1 <- lmer (bc~ LocSpeech+MorphBound+Affix+  NoCons +StressPattern +median+ (1|item), data = imComplex3)

summary(imComplexBC.lmer1)

#Let's take out each at a time and see what happens


# 1. Without median

imComplexBC.lm4<-update(imComplexBC.lm3, ~ . - median)

summary(imComplexBC.lm4)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix + 
#        MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.088055 -0.024365 -0.002276  0.022549  0.079797 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3204609  0.0122554  26.148  < 2e-16 ***
#   NoConsdouble            0.0472933  0.0072632   6.511 1.06e-09 ***
#   LocSpeech              -0.0034523  0.0008016  -4.307 2.98e-05 ***
#   StressPatternstr-unstr -0.0325520  0.0089700  -3.629  0.00039 ***
#   AffixinNeg              0.0175707  0.0078007   2.252  0.02574 *  
#   MorphBoundtransparent  -0.0078864  0.0070750  -1.115  0.26677    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03524 on 150 degrees of freedom
# Multiple R-squared:  0.521,	Adjusted R-squared:  0.5051 
# F-statistic: 32.64 on 5 and 150 DF,  p-value: < 2.2e-16

# without median Affix becomes significant --> they express the same!

# 2. Without  Affix

imComplexBC.lm5<-update(imComplexBC.lm3, ~ . - Affix)

summary(imComplexBC.lm5)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        MorphBound, data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.095802 -0.024153 -0.003127  0.022866  0.080785 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)             0.371229   0.019440  19.096  < 2e-16 ***
#   NoConsdouble            0.056035   0.005766   9.719  < 2e-16 ***
#   LocSpeech              -0.003472   0.000794  -4.373 2.28e-05 ***
#   median                 -0.014258   0.005325  -2.678  0.00824 ** 
#   StressPatternstr-unstr -0.038802   0.007832  -4.954 1.94e-06 ***
#   MorphBoundtransparent  -0.028543   0.010996  -2.596  0.01038 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.035 on 150 degrees of freedom
# Multiple R-squared:  0.5274,	Adjusted R-squared:  0.5117 
# F-statistic: 33.48 on 5 and 150 DF,  p-value: < 2.2e-16

# now median becomes significant!


# 3. Without MorphBound

imComplexBC.lm6<-update(imComplexBC.lm3, ~ . - MorphBound)

summary(imComplexBC.lm6)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix, data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.09260 -0.02476 -0.00216  0.02386  0.08156 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3240913  0.0163253  19.852  < 2e-16 ***
#   NoConsdouble            0.0487383  0.0073579   6.624 5.87e-10 ***
#   LocSpeech              -0.0034717  0.0008042  -4.317 2.86e-05 ***
#   median                 -0.0014951  0.0035793  -0.418   0.6768    
#   StressPatternstr-unstr -0.0389961  0.0085401  -4.566 1.03e-05 ***
#   AffixinNeg              0.0153668  0.0081496   1.886   0.0613 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03536 on 150 degrees of freedom
# Multiple R-squared:  0.5176,	Adjusted R-squared:  0.5016 
# F-statistic: 32.19 on 5 and 150 DF,  p-value: < 2.2e-16

# Affix becomes significant!

# let' throw out median too

imComplexBC.lm7<-update(imComplexBC.lm6, ~ . - median)

summary(imComplexBC.lm7)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
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

table(imComplex3$Affix, imComplex3$MorphBound)

#         opaque transparent
# inLoc     42          28
# inNeg     63          23


table(imComplex3$Affix, imComplex3$median)
#        1  2  3  4
# inLoc  2 29 25 14
# inNeg 21  1 60  4

# There are A LOT of inNegs rated with 3 --> which items are those?

unique(inNeg[inNeg$median=="3","item"])
#[1] immediacy   immediate   immediately impeccable 

unique(inNeg[inNeg$median=="4","item"])
#[1] immaculate impair     impairing  impairment


table(imComplex3$MorphBound, imComplex3$median)
#             1  2  3  4
# opaque       0  6 82 17
# transparent 23 24  3  1

table(imComplex3$MorphBound, imComplex3$NoCons)
#               single double
# opaque          46     59
# transparent     21     30


table(imComplex3$median, imComplex3$NoCons)
#     single double
# 1     15      8
# 2      9     21
# 3     26     59
# 4     17      1

table(imComplex3$Affix, imComplex3$NoCons)
#        single double
# inLoc     49     21
# inNeg     18     68

# let's look at the distributions within inLoc and in Neg

table(inLoc$MorphBound, inLoc$NoCons)
#              single double
# opaque          42      0
# transparent      7     21

table(inNeg$MorphBound, inNeg$NoCons)

#                single double
# opaque           4     59
# transparent     14      9



# Okay, so we see that most tokens, which are semantically transparent, are
# inLoc in our dataset. This does however not mirror the type distribution
# of the affixes inLoc and inNeg, which shows that inLoc has more transpatrent
# items. This distribution is created by the token-choice - 

get_types(inLoc,"MorphBound","opaque")
# [1] impact         impacted       impart         imparted       imparting     
# [6] impede         impediment     impelled       impersonation  implement     
# [11] implementation implemented    implementing   implication    implications  
# [16] implicit       implied        implies        imply          implying      
# [21] import         imported       importer       importers      importing     
# [26] imports        impose         imposed        imposes        imposing      
# [31] imposition     impoverished   impregnated    impress        impressed     
# [36] impresses      impression     impressionable impressions    impressive    
# [41] impulse        impulsive   


get_types(inLoc,"MorphBound","transparent")
# [1] immigrant    immigrants   immigrate    immigrated   immigration  impaneled   
# [7] impaneling   implant      imprint      imprinted    imprisoned   imprisonment


get_types(inNeg,"MorphBound","transparent")
# [1] imbalance     immaculate    immaterial    immature      immemorial    immobilized  
# [7] immoral       immorality    immortal      impartial     impatient     impeccable   
# [13] imperfect     imperfections impersonal    impolite      impossibility impossible   
# [19] impotence     impractical   improper      improperly

get_types(inNeg,"MorphBound","opaque")
# [1] immediacy     immediate     immediately   impair        impaired      impairing    
# [7] impairment    


# okay it si pretty much established, that the item immediate has a huge influence
# on the results, let's just see what happens if we exclude it


imComplex4<-imComplex3[imComplex3$item!="immediately"&imComplex3$item!="immediate",]

imComplexBC.lmWithoutImmediately1 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + Affix + median, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately1)
# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        Affix + median, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.090016 -0.020964 -0.002579  0.023254  0.074222 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.350642   0.023286  15.058  < 2e-16 ***
#   LocSpeech              -0.002889   0.001054  -2.741  0.00736 ** 
#   NoConsdouble            0.044920   0.010877   4.130 7.99e-05 ***
#   StressPatternstr-unstr -0.031579   0.009586  -3.294  0.00140 ** 
#   MorphBoundtransparent  -0.023073   0.013210  -1.747  0.08404 .  
#   AffixinNeg              0.010564   0.009521   1.109  0.27011    
#   median                 -0.011762   0.005693  -2.066  0.04165 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03538 on 92 degrees of freedom
# Multiple R-squared:  0.3379,	Adjusted R-squared:  0.2948 
# F-statistic: 7.826 on 6 and 92 DF,  p-value: 8.083e-07


# so, we see a similar picture and again, the 3 variables median, Affix and
# ST are closely connected and we need to see at each of them individually

# 1. Without Affix

imComplexBC.lmWithoutImmediately2 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + median, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately2)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        median, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092437 -0.020771 -0.003762  0.023852  0.073489 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.361480   0.021164  17.080  < 2e-16 ***
#   LocSpeech              -0.003148   0.001029  -3.059 0.002900 ** 
#   NoConsdouble            0.045004   0.010891   4.132 7.85e-05 ***
#   StressPatternstr-unstr -0.033921   0.009363  -3.623 0.000475 ***
#   MorphBoundtransparent  -0.021174   0.013115  -1.614 0.109813    
#   median                 -0.013484   0.005484  -2.459 0.015787 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03543 on 93 degrees of freedom
# Multiple R-squared:  0.3291,	Adjusted R-squared:  0.293 
# F-statistic: 9.123 on 5 and 93 DF,  p-value: 4.352e-07


# 2. Without median

imComplexBC.lmWithoutImmediately3 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + Affix, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately3)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        Affix, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.089853 -0.021329 -0.000896  0.024093  0.076750 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.314063   0.015388  20.409  < 2e-16 ***
#   LocSpeech              -0.003038   0.001070  -2.839 0.005550 ** 
#   NoConsdouble            0.042656   0.011010   3.874 0.000199 ***
#   StressPatternstr-unstr -0.031503   0.009753  -3.230 0.001712 ** 
#   MorphBoundtransparent  -0.004784   0.009976  -0.480 0.632677    
#   AffixinNeg              0.015929   0.009320   1.709 0.090754 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.036 on 93 degrees of freedom
# Multiple R-squared:  0.3072,	Adjusted R-squared:   0.27 
# F-statistic: 8.248 on 5 and 93 DF,  p-value: 1.755e-06


# 3. Without MorphBound

imComplexBC.lmWithoutImmediately4 <- lm (bc~ LocSpeech+NoCons +StressPattern + Affix + median, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately4)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + Affix + 
#        median, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092964 -0.020579  0.000322  0.023447  0.078714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.331285   0.020705  16.001  < 2e-16 ***
#   LocSpeech              -0.003244   0.001046  -3.103 0.002539 ** 
#   NoConsdouble            0.037331   0.010081   3.703 0.000361 ***
#   StressPatternstr-unstr -0.032859   0.009663  -3.400 0.000993 ***
#   AffixinNeg              0.008409   0.009545   0.881 0.380569    
#   median                 -0.005098   0.004272  -1.193 0.235761    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03577 on 93 degrees of freedom
# Multiple R-squared:  0.316,	Adjusted R-squared:  0.2792 
# F-statistic: 8.592 on 5 and 93 DF,  p-value: 1.01e-06



# Let's also have a look at each variable withut the other 2


# 4. Only MorphBound

imComplexBC.lmWithoutImmediately5 <- lm (bc~ LocSpeech+NoCons +StressPattern +MorphBound, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately5)


# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.098252 -0.022201  0.001234  0.023507  0.076454 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.322991   0.014622  22.090  < 2e-16 ***
#   LocSpeech              -0.003495   0.001046  -3.340 0.001201 ** 
#   NoConsdouble            0.042251   0.011120   3.800 0.000257 ***
#   StressPatternstr-unstr -0.035299   0.009594  -3.679 0.000390 ***
#   MorphBoundtransparent   0.002674   0.009062   0.295 0.768551    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03636 on 94 degrees of freedom
# Multiple R-squared:  0.2855,	Adjusted R-squared:  0.2551 
# F-statistic: 9.388 on 4 and 94 DF,  p-value: 1.987e-06

# interestinglöy now the effect direction changed and it is no longer
# significant. This just shows, that the effect of SemTransparency cannot
# validly be checked with this dataset!


# 5. Only Affix

imComplexBC.lmWithoutImmediately6 <- lm (bc~ LocSpeech+NoCons +StressPattern +Affix, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately6)


# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + Affix, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.093373 -0.021419 -0.001244  0.023875  0.078158 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)             0.314578   0.015288  20.577  < 2e-16 ***
#   LocSpeech              -0.003140   0.001044  -3.007  0.00339 ** 
#   NoConsdouble            0.040282   0.009795   4.113 8.38e-05 ***
#   StressPatternstr-unstr -0.032001   0.009658  -3.313  0.00131 ** 
#   AffixinNeg              0.013974   0.008347   1.674  0.09740 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03585 on 94 degrees of freedom
# Multiple R-squared:  0.3055,	Adjusted R-squared:  0.276 
# F-statistic: 10.34 on 4 and 94 DF,  p-value: 5.556e-07


# only marginally significant but effect direction does not change. Affix
# seems to be reliable!


# 6. Only median

imComplexBC.lmWithoutImmediately7 <- lm (bc~ LocSpeech+NoCons +StressPattern +median, data = imComplex4)

summary(imComplexBC.lmWithoutImmediately7)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + median, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.094264 -0.020300 -0.002386  0.023686  0.077677 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.341350   0.017247  19.792  < 2e-16 ***
#   LocSpeech              -0.003430   0.001023  -3.354 0.001150 ** 
#   NoConsdouble            0.037904   0.010048   3.772 0.000283 ***
#   StressPatternstr-unstr -0.034669   0.009431  -3.676 0.000394 ***
#   median                 -0.006937   0.003723  -1.863 0.065539 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03573 on 94 degrees of freedom
# Multiple R-squared:  0.3103,	Adjusted R-squared:  0.2809 
# F-statistic: 10.57 on 4 and 94 DF,  p-value: 4.079e-07

# also only marginally significant but effect direction remains the same. Hence
# we can trust this factor.


# All in all our analysis of the distribution of the data has revealed the followong:

# The variable Semantic Transparency is immensely influenced by the types with
# the root "immed" - 59 of ALL opaque words have this root. When throwing these
# words out of the analysis, the effect direction of Semantic Transparency changes,
# but the factor does not reach significance (only 99 items remain in the dataset)

# One reason why the word immediate might change the effect direction of the variable
# Semantic Transparency is that it has some attributes which lenghten its nasal and whcih
# which hence lead to the fact that the opaque word immediate has a long nasal. These
# factors are a double consonant, the stressed syllable following the /im/ 
# and the fact that it has a negative in,
# which are LONGER than locative ins- (see all the models above). Thus in our dataset
# we have a lot of opaque words which have a long nasal but we cannot be sure that
# the length of the nasal can be attributed to the opaqueness or to the combination
# of other attributes, i.e. negative in, double nasal and the stress pattern.

# One must note however, that the word immediate is atypical for a inNeg word
# since it is semantically opaque and also rated as very difficult to segment 
# (which goes together as the decomposability analysis has shown.) 
# In-Neg has far more transparent types than in-Loc & is rated as far more
# separable than inLoc. Immediate is one of the few opaque ins which is rated as
# difficult to segment.  

# All in all negative in- is longer than loc in - (which can be seen in the models
# with and without immediate) - but the words immediate with its enormous amount
# of tokens and with the two attributes of being opaque and being rated as
# not segmentable at all - works against the majority of the other negative ins-
# which are transparent and rated as easy to segment. When leaving out the word 
# immmediate this becomes obvious since affix OR median are stable, i.e. they stay sign.
# in the expected direction.

# We can thus more or less trust these factors (of course we still have the
# problem that we do not have a huge amount of data). Also, what we see is that
# median is significant when Affix is not included in the model, and that affix
# is significant when median is not included in the model. Reason for this could
# be that both measure the same factor.

# Let's have a look at the model without morphbound


summary(imComplexBC.lm6)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix, data = imComplex3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.09260 -0.02476 -0.00216  0.02386  0.08156 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3240913  0.0163253  19.852  < 2e-16 ***
#   NoConsdouble            0.0487383  0.0073579   6.624 5.87e-10 ***
#   LocSpeech              -0.0034717  0.0008042  -4.317 2.86e-05 ***
#   median                 -0.0014951  0.0035793  -0.418   0.6768    
#   StressPatternstr-unstr -0.0389961  0.0085401  -4.566 1.03e-05 ***
#   AffixinNeg              0.0153668  0.0081496   1.886   0.0613 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03536 on 150 degrees of freedom
# Multiple R-squared:  0.5176,	Adjusted R-squared:  0.5016 
# F-statistic: 32.19 on 5 and 150 DF,  p-value: < 2.2e-16


# without median
imComplexBC.lm8<-update(imComplexBC.lm6, ~ . - median)

summary(imComplexBC.lm8)



# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
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

# withouT Affix

imComplexBC.lm9<-update(imComplexBC.lm6, ~ . - Affix)

summary(imComplexBC.lm9)
# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.097236 -0.024339 -0.004552  0.022672  0.082276 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3376447  0.0147812  22.843  < 2e-16 ***
#   NoConsdouble            0.0572621  0.0058544   9.781  < 2e-16 ***
#   LocSpeech              -0.0036276  0.0008067  -4.497 1.37e-05 ***
#   median                 -0.0035617  0.0034361  -1.037    0.302    
#   StressPatternstr-unstr -0.0481431  0.0070878  -6.792 2.38e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03566 on 151 degrees of freedom
# Multiple R-squared:  0.5062,	Adjusted R-squared:  0.4931 
# F-statistic:  38.7 on 4 and 151 DF,  p-value: < 2.2e-16


# so median does not become significant which could also be due to the 
# word immediate --> whe one looks at the ratings, one can see that out of the 64
# ratings which are 3 or 4 with in NEg, 59 are ratings for words with the root
# imm --> so here again one word influenced a whole variable in a significant way,
# i.e. the word immediate which has certain attributes which lengthen it,
# i.e. stress pattern, double consonat, negative in is part of the group
# of items whcih are not separable: thus, here we have "both" sides pulling at
# the length: on the one side:double, prosodic boundary, negative in , on the
# other: really opaque, i.e. rated as difficult to segment

# I assume, that in general immediate was pronounced pretty long --> longer
# than the other opaque items:

# Let's have a look --> let's create a dataset with only the immed. words

immed<-imComplex3[imComplex3$item=="immediate"|imComplex3$item=="immediacy"|imComplex3$item=="immediately",]
others<-imComplex3[imComplex3$item!="immediate"&imComplex3$item!="immediacy"&imComplex3$item!="immediately",]
othersOP<-others[others$MorphBound=="opaque",]
othersTr<-others[others$MorphBound=="transparent",]


mean(immed$LocSpeech)
#[1] 14.43963

mean(othersOP$LocSpeech)
#[1] 13.43633

mean(othersTr$LocSpeech)
#[1] 14.8766

# So, the speech rate was higher than for the other opaques and more
# like the transparent items

# now let's look at the AbsDurCon

mean(immed$AbsDurCon)
#[1] 0.09720239

table(othersOP$NoCons)
# single double 
# 46      0 

# Problem: immed. were the only doubles which were opaque

othersTrDouble<-othersTr[othersTr$NoCons=="double",]

mean(othersTrDouble$AbsDurCon)
#[1] 0.06736526


# really LOW

# I think this has to do with the word immigrant

immigr<-imComplex3[imComplex3$item=="immigrate"|imComplex3$item=="immigrant"|
                    imComplex3$item=="immigrants"|imComplex3$item=="immigrated"|
                    imComplex3$item=="immigration",]


mean(immigr$AbsDurCon)
#[1] 0.05958628


# now the other doubles

doubles<-imComplex3[imComplex3$item=="immacualte"|imComplex3$item=="immaterial"|
                     imComplex3$item=="immature"|imComplex3$item=="immemorial"|
                     imComplex3$item=="immobilized"|imComplex3$item=="immoral"
                   |imComplex3$item=="immorality"|imComplex3$item=="immortal",
                   ]


mean(doubles$AbsDurCon)
#[1] 0.08827324


# Interesting: immigr words are particularly short, immediate long -->
# might have to with the stress: we should test the interaction again....

# Immediate was indeed pronounced longer than the other opaques BUT it is
# the only one with a double consonant...Another reason why the variable MorphBound
# is senseless: All the doubles in MorphBound opaque are negatives and have the same
# root....

#########################################################################
#
# So, to summarize:
#
# This dataset is not perfect to test the effect of decomposability on
# the duration of the prefixal nasal but can only give us some hints in what
# direction to go. In the final model we do see an effect of Affix which could
# reflect the inherent difference in decomosability between inLoc and inNeg
# concerning their decomposability (which we already saw in the decomp. analysis)
# but we could not detect any "isolated" decomposability effects. 
#
# The reasons can be seen in the following:
# 1. The composition of the dataset, i.e. the distribution of types and tokens:
#   - items with the root immed. are by far the most frequent items in the dataset
#     this leads to the fact, that the majority of opaque items are negative ins,
#     with a double consonant and a stressed first syllable of the base --> all factors
#     which lenghten the nasal. Hence, the variable Semantic Transparency is highly
#     influenced by one type which "pulls" its effect in an opposite direction:
#     when taking theses words out, the effect direction changes.
#     However, since we do not have too much choice, we need these words in order to
#     make a statistical model which is pwerful enough to test any effects. Since
#     most of the other types only occur once or twice in the dataset, we cannot
#     include a random factor since almost all of the variance would be explained by
#     the factor item. Hence, it is almost impossible to make any assumptions
#     about the factor semantic transparency based on this dataset.
#     A similar problem arises with the factor median. The higher the median, the 
#     less segmentable, the longer the duration. This effect i on the edge of
#     being significant in some models, and when looking at the distribution we
#     can see that most löocative ins are rated to be difficult to segment, with the
#     exception of the immed. -words - again the same "problem" - the highe amount
#     of tokens of this one type have an immense influence.
#
#     What one could however assume (based on the type analsys and the tendencies)
#     that in most cases decomposability on different levels (semantic, prosodic, articulotory)
#     often comes in "bundles" - and that the combination of those factors together
#     is reflected in fine phonetic detail, such as duration. These bundles often
#     fall together, for example in this case in affixes, such as negative and locative in, which
#     is why we can see an effect of affix on duration. This might however just be
#     an abstraction, i.e. different segmentability factors influence duration and in
#     other cases they could have a specific indicidual effect --> not just together.
#
#     More data, with a distribution which actually allows for testing these factors
#     is needed --> experiment. also, one has to look at other affixes and see
#     whether one can see the same phenomena.



# But now let's have a look at the final model

summary(imComplexBC.lm8)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
#
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
#   NoConsdouble            0.0481981  0.0072235   6.672 4.48e-10 ***
#   LocSpeech              -0.0034761  0.0008019  -4.335 2.65e-05 ***
#   StressPatternstr-unstr -0.0375435  0.0077786  -4.826 3.37e-06 ***
#   AffixinNeg              0.0164092  0.0077370   2.121   0.0356 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.03527 on 151 degrees of freedom
# Multiple R-squared:  0.5171,	Adjusted R-squared:  0.5043 
# F-statistic: 40.42 on 4 and 151 DF,  p-value: < 2.2e-16



par(mfrow=c(2,2))
visreg(imComplexBC.lm8,trans= function(x) x^(1/lambda))


table(imComplex3$NoCons,imComplex3$StressPattern)
#              beforeStressed beforeUnstressed
# m#mV             63               26
# m#C              54               13


get_number_type_combination(imComplex3,"NoCons","m#mV","StressPattern","beforeUnstressed")
#[1] 9

get_type_combination(imComplex3,"NoCons","m#mV","StressPattern","beforeUnstressed")
#[1] immaterial  immature    immemorial  immigrant   immigrants  immigrate   immigrated  immigration immorality 

get_number_type_combination(imComplex3,"NoCons","m#mV","StressPattern","beforeStressed")
#[1] 7

get_type_combination(imComplex3,"NoCons","m#mV","StressPattern","beforeStressed")
#[1] immaculate  immediacy   immediate   immediately immobilized immoral     immortal   

table(imComplex3$StressPattern,imComplex3$Affix)
#                   inLoc inNeg
# beforeStressed      39    78
# beforeUnstressed    31     8


# let's check whether the same factors are derived when we use 
#  the MuMin packe to compare the importance of the
# different factors.

options(na.action = "na.fail") 

model_ranking <- dredge(imComplexBC.lm1)

model_average_<-model.avg(model_ranking)


summary(model_average_)

# Call:
#   model.avg(object = model_ranking)
# 
# Component model call: 
#   lm(formula = bc ~ <256 unique rhs>, data = imComplex3)
# 
# Component models: 
#   df logLik    AICc  delta weight
# 124568    8 305.82 -594.65   0.00   0.16
# 24568     7 304.67 -594.58   0.08   0.15
# 1268      6 302.98 -593.39   1.27   0.09
# 234568    8 304.78 -592.57   2.08   0.06
# 12568     7 303.62 -592.48   2.17   0.05
# 1234568   9 305.85 -592.47   2.19   0.05
# 1245678   9 305.82 -592.40   2.25   0.05
# 245678    8 304.67 -592.36   2.29   0.05
# 12368     7 303.21 -591.67   2.98   0.04
# 12468     7 303.07 -591.38   3.28   0.03
# 12678     7 302.98 -591.21   3.44   0.03
# 268       5 300.69 -590.97   3.68   0.03
# 2345678   9 304.78 -590.33   4.33   0.02
# 123568    8 303.62 -590.27   4.39   0.02
# 125678    8 303.62 -590.26   4.39   0.02
# 12345678 10 305.85 -590.18   4.47   0.02
# 123468    8 303.57 -590.17   4.48   0.02
# 2468      6 301.24 -589.91   4.74   0.02
# 23468     7 302.14 -589.53   5.12   0.01
# 2568      6 301.02 -589.49   5.17   0.01
# 123678    8 303.22 -589.46   5.19   0.01
# 2368      6 300.96 -589.37   5.29   0.01
# 124678    8 303.08 -589.18   5.48   0.01
# 2678      6 300.72 -588.87   5.79   0.01
# 1235678   9 303.62 -588.01   6.64   0.01
# 1234678   9 303.59 -587.94   6.71   0.01
# 24678     7 301.28 -587.81   6.84   0.01
# 23568     7 301.09 -587.42   7.23   0.00
# 234678    8 302.18 -587.39   7.26   0.00
# 25678     7 301.04 -587.32   7.33   0.00
# 23678     7 300.99 -587.22   7.43   0.00
# 235678    8 301.10 -585.23   9.42   0.00
# 12456     7 299.09 -583.42  11.23   0.00
# 124567    8 299.42 -581.87  12.78   0.00
# 123456    8 299.38 -581.78  12.87   0.00
# 1256      6 297.06 -581.55  13.11   0.00
# 145678    8 299.22 -581.47  13.19   0.00
# 45678     7 297.52 -580.27  14.38   0.00
# 1234567   9 299.71 -580.19  14.46   0.00
# 12567     7 297.40 -580.04  14.61   0.00
# 12356     7 297.23 -579.70  14.95   0.00
# 1345678   9 299.31 -579.38  15.28   0.00
# 1678      6 295.90 -579.23  15.42   0.00
# 14568     7 296.95 -579.13  15.52   0.00
# 15678     7 296.81 -578.87  15.78   0.00
# 123567    8 297.57 -578.16  16.49   0.00
# 345678    8 297.53 -578.08  16.57   0.00
# 4568      6 295.31 -578.05  16.60   0.00
# 1236      6 294.92 -577.27  17.38   0.00
# 168       5 293.83 -577.25  17.40   0.00
# 134568    8 297.11 -577.24  17.42   0.00
# 14678     7 295.95 -577.15  17.51   0.00
# 13678     7 295.92 -577.09  17.56   0.00
# 135678    8 296.99 -577.00  17.65   0.00
# 12367     7 295.63 -576.50  18.15   0.00
# 1568      6 294.52 -576.49  18.17   0.00
# 34568     7 295.37 -575.97  18.68   0.00
# 1468      6 293.93 -575.30  19.35   0.00
# 678       5 292.77 -575.13  19.52   0.00
# 12346     7 294.93 -575.10  19.56   0.00
# 134678    8 296.04 -575.10  19.56   0.00
# 1368      6 293.83 -575.09  19.56   0.00
# 13568     7 294.81 -574.87  19.79   0.00
# 123467    8 295.63 -574.28  20.38   0.00
# 4678      6 293.34 -574.11  20.54   0.00
# 5678      6 293.25 -573.94  20.71   0.00
# 1267      6 292.99 -573.42  21.23   0.00
# 23456     7 294.02 -573.28  21.37   0.00
# 1246      6 292.91 -573.26  21.39   0.00
# 68        4 290.73 -573.19  21.46   0.00
# 126       5 291.78 -573.17  21.48   0.00
# 13468     7 293.96 -573.16  21.49   0.00
# 2456      6 292.85 -573.13  21.52   0.00
# 3678      6 292.80 -573.03  21.62   0.00
# 24567     7 293.87 -572.99  21.67   0.00
# 234567    8 294.95 -572.91  21.74   0.00
# 12467     7 293.76 -572.76  21.89   0.00
# 34678     7 293.66 -572.56  22.10   0.00
# 468       5 291.44 -572.47  22.18   0.00
# 35678     7 293.30 -571.85  22.80   0.00
# 568       5 291.06 -571.73  22.92   0.00
# 368       5 290.73 -571.06  23.60   0.00
# 3468      6 291.62 -570.68  23.97   0.00
# 3568      6 291.18 -569.79  24.86   0.00
# 1456      6 289.73 -566.90  27.76   0.00
# 14567     7 290.72 -566.69  27.97   0.00
# 13456     7 289.73 -564.71  29.95   0.00
# 1238      6 288.56 -564.55  30.10   0.00
# 156       5 287.47 -564.53  30.12   0.00
# 123       5 287.46 -564.52  30.13   0.00
# 134567    8 290.72 -564.47  30.18   0.00
# 1567      6 288.48 -564.40  30.26   0.00
# 2567      6 288.43 -564.29  30.36   0.00
# 23567     7 289.45 -564.14  30.51   0.00
# 2356      6 288.17 -563.77  30.89   0.00
# 256       5 287.03 -563.67  30.98   0.00
# 12348     7 289.20 -563.64  31.02   0.00
# 2367      6 288.09 -563.62  31.04   0.00
# 1237      6 287.74 -562.91  31.74   0.00
# 23467     7 288.76 -562.76  31.89   0.00
# 1234      6 287.66 -562.76  31.89   0.00
# 12345     7 288.75 -562.73  31.92   0.00
# 1235      6 287.60 -562.64  32.01   0.00
# 12378     7 288.61 -562.47  32.19   0.00
# 1356      6 287.50 -562.44  32.22   0.00
# 12358     7 288.57 -562.38  32.28   0.00
# 123458    8 289.62 -562.25  32.40   0.00
# 13567     7 288.49 -562.22  32.44   0.00
# 236       5 286.28 -562.17  32.48   0.00
# 123478    8 289.28 -561.57  33.08   0.00
# 12347     7 288.02 -561.28  33.38   0.00
# 2346      6 286.81 -561.06  33.60   0.00
# 123457    8 288.94 -560.91  33.75   0.00
# 12357     7 287.81 -560.87  33.78   0.00
# 123578    8 288.63 -560.27  34.38   0.00
# 1234578   9 289.68 -560.12  34.53   0.00
# 136       5 283.54 -556.68  37.97   0.00
# 125       5 283.49 -556.59  38.06   0.00
# 267       5 283.25 -556.10  38.55   0.00
# 1367      6 284.20 -555.85  38.81   0.00
# 1258      6 284.20 -555.84  38.81   0.00
# 1245      6 284.05 -555.53  39.13   0.00
# 146       5 282.94 -555.49  39.17   0.00
# 128       5 282.82 -555.24  39.41   0.00
# 16        4 281.74 -555.21  39.44   0.00
# 1346      6 283.73 -554.89  39.76   0.00
# 1257      6 283.68 -554.80  39.85   0.00
# 12458     7 284.69 -554.62  40.03   0.00
# 1467      6 283.47 -554.37  40.29   0.00
# 2467      6 283.39 -554.22  40.43   0.00
# 13467     7 284.44 -554.12  40.53   0.00
# 1378      6 283.20 -553.84  40.82   0.00
# 12578     7 284.27 -553.77  40.88   0.00
# 12457     7 284.22 -553.69  40.96   0.00
# 138       5 282.04 -553.68  40.97   0.00
# 167       5 282.03 -553.66  40.99   0.00
# 1248      6 283.05 -553.54  41.11   0.00
# 456       5 281.88 -553.36  41.29   0.00
# 1278      6 282.96 -553.36  41.29   0.00
# 124578    8 284.75 -552.52  42.13   0.00
# 13478     7 283.53 -552.29  42.36   0.00
# 1348      6 282.38 -552.20  42.45   0.00
# 124       5 281.29 -552.18  42.47   0.00
# 12        4 280.21 -552.15  42.50   0.00
# 4567      6 282.34 -552.12  42.53   0.00
# 134578    8 284.42 -551.87  42.78   0.00
# 13458     7 283.29 -551.82  42.84   0.00
# 13578     7 283.27 -551.79  42.87   0.00
# 26        4 280.02 -551.78  42.87   0.00
# 3456      6 282.15 -551.74  42.91   0.00
# 1358      6 282.11 -551.66  43.00   0.00
# 12478     7 283.17 -551.57  43.08   0.00
# 127       5 280.93 -551.46  43.19   0.00
# 1345      6 281.75 -550.93  43.72   0.00
# 1247      6 281.74 -550.91  43.74   0.00
# 34567     7 282.71 -550.67  43.99   0.00
# 13        4 279.45 -550.64  44.02   0.00
# 246       5 280.44 -550.49  44.17   0.00
# 135       5 280.44 -550.47  44.18   0.00
# 13457     7 282.54 -550.31  44.34   0.00
# 1357      6 281.23 -549.89  44.76   0.00
# 137       5 280.11 -549.81  44.84   0.00
# 158       5 280.07 -549.73  44.92   0.00
# 15        4 278.75 -549.23  45.43   0.00
# 18        4 278.72 -549.18  45.48   0.00
# 1578      6 280.79 -549.02  45.64   0.00
# 1458      6 280.76 -548.96  45.69   0.00
# 145       5 279.57 -548.74  45.91   0.00
# 134       5 279.45 -548.50  46.15   0.00
# 178       5 279.26 -548.13  46.52   0.00
# 14578     7 281.44 -548.13  46.53   0.00
# 157       5 279.25 -548.09  46.56   0.00
# 2348      6 280.26 -547.95  46.70   0.00
# 1347      6 280.11 -547.65  47.00   0.00
# 1457      6 280.04 -547.52  47.14   0.00
# 148       5 278.87 -547.33  47.32   0.00
# 23458     7 280.74 -546.72  47.93   0.00
# 23478     7 280.59 -546.43  48.22   0.00
# 1478      6 279.47 -546.38  48.28   0.00
# 234578    8 281.03 -545.07  49.58   0.00
# 14        4 275.99 -543.71  50.95   0.00
# 1         3 274.83 -543.51  51.15   0.00
# 147       5 276.26 -542.13  52.53   0.00
# 56        4 274.98 -541.69  52.97   0.00
# 17        4 274.96 -541.65  53.00   0.00
# 2345      6 277.07 -541.58  53.07   0.00
# 23457     7 278.10 -541.45  53.20   0.00
# 2358      6 276.69 -540.81  53.84   0.00
# 238       5 275.37 -540.33  54.32   0.00
# 567       5 275.33 -540.26  54.40   0.00
# 2347      6 276.37 -540.17  54.48   0.00
# 356       5 275.20 -540.00  54.65   0.00
# 23578     7 277.05 -539.35  55.30   0.00
# 234       5 274.78 -539.16  55.49   0.00
# 2378      6 275.65 -538.73  55.92   0.00
# 3567      6 275.63 -538.70  55.96   0.00
# 237       5 272.81 -535.21  59.44   0.00
# 36        4 271.60 -534.94  59.71   0.00
# 23        4 271.57 -534.88  59.77   0.00
# 348       5 272.45 -534.51  60.14   0.00
# [ reached getOption("max.print") -- omitted 56 rows ]
# 
# Term codes: 
#   Affix     LocSpeech    logRelFreq        median    MorphBound        NoCons 
# 1             2             3             4             5             6 
# PrecSegDur StressPattern 
# 7             8 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             0.3470928  0.0284071   0.0285135  12.173  < 2e-16 ***
#   AffixinNeg              0.0087817  0.0095979   0.0096314   0.912 0.361886    
# LocSpeech              -0.0034861  0.0008512   0.0008580   4.063 4.84e-05 ***
#   median                 -0.0076433  0.0075516   0.0075716   1.009 0.312746    
# MorphBoundtransparent  -0.0160583  0.0153335   0.0153757   1.044 0.296302    
# NoConsdouble            0.0510628  0.0080721   0.0081230   6.286  < 2e-16 ***
#   StressPatternstr-unstr -0.0365243  0.0094276   0.0094888   3.849 0.000118 ***
#   logRelFreq              0.0001272  0.0005360   0.0005395   0.236 0.813614    
# PrecSegDur             -0.0024744  0.0680299   0.0685757   0.036 0.971216    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             0.3470928  0.0284071   0.0285135  12.173  < 2e-16 ***
#   AffixinNeg              0.0144062  0.0083721   0.0084350   1.708   0.0877 .  
# LocSpeech              -0.0034894  0.0008449   0.0008517   4.097 4.18e-05 ***
#   median                 -0.0114268  0.0064824   0.0065172   1.753   0.0795 .  
# MorphBoundtransparent  -0.0234079  0.0130647   0.0131367   1.782   0.0748 .  
# NoConsdouble            0.0510629  0.0080721   0.0081230   6.286  < 2e-16 ***
#   StressPatternstr-unstr -0.0365898  0.0093081   0.0093702   3.905 9.43e-05 ***
#   logRelFreq              0.0004544  0.0009369   0.0009440   0.481   0.6302    
# PrecSegDur             -0.0099499  0.1361461   0.1372428   0.072   0.9422    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#   NoCons LocSpeech StressPattern MorphBound median Affix logRelFreq
# Importance:          1.00   1.00      1.00          0.69       0.67   0.61  0.28      
# N containing models:  128    128       128           128        128    128   128      
# PrecSegDur
# Importance:          0.25      
# N containing models:  128    

# So here we see again, teh complication with MorphBound, median and Affix:
# We have already discussed that....


summary(imComplexBC.lm8)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
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



# So, this is our final model: The effects are
#   1. Doubles longer than singles --> in geminates
#   2. The longer the word, the longer the consonant and the more segments in the word,
#      the shorter the consonant (speech rate as a phonetic effect)
#   3. When Affix stresses and adacent syllable unstressed, consonant is shorter
#   4. Neg in longer than loc in (acoustic duration reflects morphological distinctions)


###################################################################################################
#              Some Stats on the variables of the final dataset                                   #
###################################################################################################


# Number of types for Nocons= double and NoCons= single 

imComplex3Double<- imComplex3[imComplex3$NoCons=="double",]

imComplex3Single<- imComplex3[imComplex3$NoCons=="single",]

imComplex3Double$item<- factor(imComplex3Double$item)

imComplex3Single$item<- factor(imComplex3Single$item)

levels(imComplex3Double$item)
# [1] "immaculate"  "immaterial"  "immature"    "immediacy"   "immediate"   "immediately"
# [7] "immemorial"  "immigrant"   "immigrants"  "immigrate"   "immigrated"  "immigration"
# [13] "immobilized" "immoral"     "immorality"  "immortal"

str(imComplex3Double$item)

#Factor w/ 16 levels "immaculate","immaterial",..: 5 6 5 9 6 12 10 8 9 5 ...

# But a huge amount is immediate and immigr...

# We have 16 types with double consonants

str(imComplex3Single$item)

#Factor w/ 67 levels "imbalance","impact",..: 1 2 3 4 5 6 7 8 9 10 ...

# We have 67 types with single m

#renaming levels of TransitionType (so that other people understand what this variable is about)

levels (imComplex3$NoCons)
#[1] "single" "double"

levels (imComplex3$NoCons) = c("m#", "m#m")

levels (imComplex3$NoCons)
#[1] "m#"  "m#m"



#  Now - let's get the summaries of the variables which are in our initial
# model with our dinal dataset

summary (imComplex3$bc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1631  0.2632  0.2982  0.2971  0.3280  0.4380 

sd (imComplex3$bc)
#[1] 0.05009002

summary (imComplex3$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.609   7.124   9.028   8.582   9.733  10.700

sd(imComplex3$logWordFormFreqAllCoca)
#[1] 1.773862


summary (imComplex3$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-9.6700 -0.3434  3.0470  3.5350  7.7140 10.4900 

sd (imComplex3$logRelFreq)
#[1]  4.742333


summary (imComplex3$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.279  12.020  14.070  14.290  16.590  24.320 

sd (imComplex3$LocSpeech)
#[1] 3.665657


summary (imComplex3$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.03923 0.05690 0.06078 0.07765 0.12680 

sd(imComplex3$PrecSegDur)
#[1] 0.02560936

summary (imComplex3$Affix)
#inLoc inNeg   
#70    86

summary (imComplex3$MorphBound)
#opaque transparent 
#105         51 

summary (imComplex3$NoCons)
#m# m#m 
# 67  89 

summary (imComplex3$StressPattern)
#d/u-str str-unstr 
#117        39 

summary (imComplex3$median)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.628   3.000   4.000 



# Information for article

lambda
#[1] 0.4646465

dim(imComplex)
#[1] 158  58

dim(imComplex2)
#[1] 157  59

dim(imComplex3)
#[1] 156  63

# Thus we removed 2 items
# one outlier (long preceding segm duration)
# one because we did not have a valid rating for imminent


#R-squared is 0.5043

summary(imComplexBC.lm8)

# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.3195951  0.0122406  26.109  < 2e-16 ***
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


# We conduct an anova

anova(imComplexBC.lm8)
# Analysis of Variance Table
# 
# Response: bc
# Df   Sum Sq  Mean Sq F value    Pr(>F)    
# NoCons          1 0.091129 0.091129 73.2677 1.199e-14 ***
#   LocSpeech       1 0.045698 0.045698 36.7413 1.033e-08 ***
#   StressPattern   1 0.058665 0.058665 47.1669 1.591e-10 ***
#   Affix           1 0.005595 0.005595  4.4981   0.03557 *  
#   Residuals     151 0.187810 0.001244                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# # Need to rename some levels for the plots

levels(imComplex3$StressPattern)
#[1] "d/u-str"   "str-unstr"

levels(imComplex3$StressPattern) = c("beforeStressed", "beforeUnstressed")

levels(imComplex3$StressPattern)
#[1] "beforeStressed"   "beforeUnstressed"


levels(imComplex3$NoCons)
#[1] "m#"  "m#m"

levels(imComplex3$NoCons) = c("m#C", "m#mV")
levels(imComplex3$NoCons)
#[1] "m#C"  "m#mV"

#redo the model
imComplexBC.lm8a<- lm(bc ~ NoCons + LocSpeech + StressPattern +  Affix, data = imComplex3)
summary(imComplexBC.lm8a)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                    0.3195951  0.0122406  26.109  < 2e-16 ***
#   NoConsm#mV                     0.0481981  0.0072235   6.672 4.48e-10 ***
#   LocSpeech                     -0.0034761  0.0008019  -4.335 2.65e-05 ***
#   StressPatternbeforeUnstressed -0.0375435  0.0077786  -4.826 3.37e-06 ***
#   AffixinNeg                     0.0164092  0.0077370   2.121   0.0356 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03527 on 151 degrees of freedom
# Multiple R-squared:  0.5171,	Adjusted R-squared:  0.5043 
# F-statistic: 40.42 on 4 and 151 DF,  p-value: < 2.2e-16


# Do the plots


# Find out at which levels visreg draws lines

intercept=0.3195951
medianSpeech=median(imComplex3$LocSpeech)
estSpeech=-0.0034761
estDouble=0.0481981
estUnstressed=-0.0375435
estInNeg=0.0164092


#levels single
(intercept+(medianSpeech*estSpeech)+estInNeg)^(1/lambda)
#[1] 0.06816798

EstimatedValueSingle=0.06816798



#level double
(intercept+(medianSpeech*estSpeech)+estInNeg+ estDouble)^(1/lambda)
#[1] 0.09519941

EstimatedValueDouble=0.09519941


#Unterschiede ausrechnen:

EstimatedValueDouble-EstimatedValueSingle
#[1]  0.02703143
# This if for Negative in



#levels affix neg
(intercept+estDouble+(medianSpeech*estSpeech)+estInNeg)^(1/lambda)
#[1]  0.09519941
EstimatedValueNeg= 0.09519941



#level double loc
(intercept+(medianSpeech*estSpeech)+estDouble)^(1/lambda)
#[1] 00.08545435

EstimatedValueLoc=0.08545435

#Difference
EstimatedValueNeg-EstimatedValueLoc
#[1]0.00974506


# For the difference between double loc and single loc,
#we need to calculate how long a single locative in is

(intercept+(medianSpeech*estSpeech))^(1/lambda)
#[1] 0.06005798

EstimatedValueSingleLoc=0.06005798


EstimatedValueLoc-EstimatedValueSingleLoc
#[1] 0.02539637



#levels affix stressed
(intercept+estDouble+(medianSpeech*estSpeech)+estInNeg)^(1/lambda)
#[1] 0.09519941

EstimatedValueStressed=0.09519941



#level double loc
(intercept+estDouble+(medianSpeech*estSpeech)+estInNeg+estUnstressed)^(1/lambda)
#[1] 0.07372922

EstimatedValueUnstressed=0.07372922

#Difference
EstimatedValueStressed-EstimatedValueUnstressed
#[1] 0.02147019



# Note: It makes more sense to have m#m als reference-level im model, da
# es sonst auch diese Reihenfolge gegeben hat. Deswegen einmal releveln,
# Zahlen in der Tabelle und im text austauschen und plot änder

# I want to relevel the levls also for the last model, so that
# it is comparable to the un-data in which the double is also the ref-level

imComplex3$NoCons <- relevel (imComplex3$NoCons, ref= "m#mV")
levels(imComplex3$NoCons)
#[1] "m#mV"   "m#C"
#

# I also need to rename the levels for stress

levels(imComplex3$StressPattern)
#[1] "beforeStressed"   "beforeUnstressed"

levels(imComplex3$StressPattern)<-c("stressed"  , "unstressed")
levels(imComplex3$StressPattern)
#[1] "stressed"   "unstressed"

#redo the model
imComplexBC.lm8b<- lm(bc ~ NoCons + LocSpeech + StressPattern +  Affix, data = imComplex3)
summary(imComplexBC.lm8b)


# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.092516 -0.024302 -0.001385  0.023954  0.081107 
# 
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                    0.3677932  0.0147524  24.931  < 2e-16 ***
#   NoConsm#C                     -0.0481981  0.0072235  -6.672 4.48e-10 ***
#   LocSpeech                     -0.0034761  0.0008019  -4.335 2.65e-05 ***
#   StressPatternunstressed       -0.0375435  0.0077786  -4.826 3.37e-06 ***
#   AffixinNeg                     0.0164092  0.0077370   2.121   0.0356 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.03527 on 151 degrees of freedom
# Multiple R-squared:  0.5171,	Adjusted R-squared:  0.5043 
# F-statistic: 40.42 on 4 and 151 DF,  p-value: < 2.2e-16


levels(imComplex$NoCons)
#[1] "single" "double"

levels(imComplex$NoCons) = c("m#C", "m#mV")

levels(imComplex$NoCons)
#[1] "m#C"   "m#mV"

imComplex$AbsDurConMS<-imComplex$AbsDurCon*1000

imComplex$NoCons <- relevel (imComplex$NoCons, ref= "m#mV")

levels(imComplex$NoCons)
#[1] "m#mV" "m#C"

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/duration")


png("boxIn.png", units="cm", height=10, width=7, res=300, pointsize=8)
bwplot (AbsDurConMS ~ NoCons, imComplex, ylab="duration in milliseconds", main="in-", ylim=c(0,180), cex.axis=0.5)

dev.off()


png("in- modelcov.png", units="cm", height=8, width=14, res=300, pointsize=8)

par(mfrow=c(1,2))

visreg(imComplexBC.lm8b, "LocSpeech", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="local speech rate", cex.axis=0.5, ylim=c(60,130))
visreg(imComplexBC.lm8b, "StressPattern", trans= function(x) x^(1/lambda)*1000, rug=F,  ylab="duration in milliseconds", xlab="stress pattern", cex.axis=0.5, ylim=c(60,130))

dev.off()



png("in- model.png", units="cm", height=8, width=14, res=300, pointsize=8)

par(mfrow=c(1,2))

visreg(imComplexBC.lm8b, "NoCons", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="environment", cex.axis=0.8, ylim=c(20,130))
visreg(imComplexBC.lm8b, "Affix", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="affix", cex.axis=0.8, ylim=c(20,130))

dev.off()






imComplexBC.lm8c<- lm()

png("in- modelcov.png", units="cm", height=8, width=14, res=300, pointsize=8)

par(mfrow=c(1,2))

visreg(imComplexBC.lm8b, "LocSpeech", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="local speech rate", cex.axis=0.8, ylim=c(20,130))
visreg(imComplexBC.lm8b, "StressPattern", trans= function(x) x^(1/lambda)*1000, rug=F,  ylab="duration in milliseconds", xlab="base-initial stress", cex.axis=0.8, ylim=c(20,130))

dev.off()

png("imCorpusMainEffect.png", units="cm", height=16, width=8, res=300, pointsize=11)
par(mfrow=c(1,1))

visreg(imComplexBC.lm8b, "NoCons", trans= function(x) x^(1/lambda)*1000, rug=F, ylab="duration in milliseconds", xlab="environment", cex.axis=0.5, ylim=c(0,180))
dev.off()

