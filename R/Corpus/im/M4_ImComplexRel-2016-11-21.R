
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

imComplex$RelDur<-imComplex$AbsDurCon/imComplex$PrecSegDur

dim(imComplex)
#[1] 158  57


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

densityplot(imComplex$RelDur)
plot(imComplex$RelDur)

# there is one item with an extremely long nasal dur (know this from
# the other analysis - this is the item)

imComplex[c(97), c("ItemID","item", "RelDur")]

# ItemID        item RelDur
# 191    590 immediately 0.1908750

imComplex[c(97), c( "AbsDurCon")]


# Looking at the praat script, it revealed that the segmentation was false, let's
# change the value to the right one

imComplex[c(97), c( "AbsDurCon")] <-0.1483750000000006

imComplex[c(97), c( "AbsDurCon")]
#0.148375

# now I need to create the variable RelDur again...

imComplex$RelDur<-imComplex$AbsDurCon/imComplex$PrecSegDur


densityplot(imComplex$RelDur)
plot(imComplex$RelDur)


# looks ok


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
plot(imComplex$PartofSpeech, imComplex$RelDur)
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

plot (imComplex$RelDur ~ imComplex$NoCons)

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



plot (imComplex$logRelFreq, imComplex$RelDur)
plot (imComplex$logWordFormFreqAllCoca, imComplex$RelDur)
plot (imComplex$logBaseFormFreq, imComplex$RelDur)

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

plot (imComplex$RelDur ~ imComplex$Pos)

# The plot suggests that there is not a big difference between any of the
# three positions. Why should we include this predictor, then? Well, it may
# be useful to account for phrase-final (or utterance-final) lengthening. 
# But we are (probably) going to include WordDur as another predictor, 
# and if there is such a lengthening effect, it would be better implemented
# by the continuous WordDur predictor. So, bye-bye, Pos.


#     5. Word Duration

plot (density(imComplex$WordDur))
plot (imComplex$RelDur ~ imComplex$WordDur)


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
# Speaker Pos Affix AffixOrth MorphBound Consonant NoCons RelDur   WordDur PrecSeg
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
#[1] 157  59

# Let's look at the distribution now:

plot (density(imComplex2$PrecSegDur))
plot(imComplex2$PrecSegDur)


plot (imComplex2$RelDur ~ imComplex2$PrecSegDur)

# Of Course there is a relation, since RelDur consists partly of PrecSegDur. 
# Hence we cannot use this factor


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

plot (imComplex2$RelDur ~ imComplex2$FollSegDur)

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
plot(imComplex2$RelDur~imComplex2$LocSpeech) 

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


plot (imComplex2$RelDur ~ imComplex2$SyllAct)

#     9.3. Word Duration

plot (imComplex2$RelDur ~ imComplex2$WordDur)

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


plot (imComplex2$RelDur ~ imComplex2$MorphBound)
# We need to include this factor! It looks like the semantic opaque
# words have a longer duration

#Let's check the relation between MorphBound and Nocons
table(imComplex2$MorphBound, imComplex2$NoCons)

#              single double
#opaque          46     60
#transparent     21     30

# The distribution looks okayish.

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

# ALL of this will cause problems...


# Let's look at the relation between Affix and RelDur

plot (imComplex2$RelDur ~ imComplex2$Affix)
# inNeg seems to be longer.


#             12. Stress


table (imComplex2$AffixStress)
#d   p   s   u 
#116  22  18   1  

plot (imComplex2$RelDur ~ imComplex2$AffixStress)
# debatable am längsten

table (imComplex2$AdjSyllStress)
# p  s  u 
# 116  1 40 

plot (imComplex2$RelDur ~ imComplex2$AdjSyllStress)
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

plot (imComplex2$RelDur ~ imComplex2$StressPattern)
#es zeigt sich, wenn die adjacent syllable stressed ist, ist /m/ länger (aber für u-s nur ein item),
# und wenn sie unstressed ist. ist das /m/ kürzer

# wir können also je zwei Level zusammenlegen (d-p + u-s =d/u-str UND p-u + s-u= str-unstr")

levels(imComplex2$StressPattern)
#[1] "d-p" "p-u" "s-u" "u-s"

# let's have a look at the types

unique(imComplex2[imComplex2$StressPattern=="d-p","item.x"])
# [1] immediate      impression     impossible     imposed        implies        imposing      
# [7] imprisonment   impressed      impress        impressive     immediately    import        
# [13] impartial      impacted       impressions    importers      imbalance      impossibility 
# [19] imply          immobilized    implant        impose         impractical    immaculate    
# [25] imported       impresses      immortal       implied        impatient      impulsive     
# [31] importing      imprint        impelled       imparting      imprisoned     impart        
# [37] impregnated    impersonal     imprinted      impressionable impeccable     impoverished  
# [43] imposes        immediacy      impede         improper       impairment     impaneling    
# [49] impaneled      impair         implying       immoral        importer       improperly    
# [55] impairing      imparted       impaired       impediment     implicit       imperfect 


unique(imComplex2[imComplex2$StressPattern=="p-u","item.x"])
# [1] impact         implemented    immigrants     immigrate      immigrant      implementing  
# [7] imports        implement      impulse        implementation imminent       immigrated    
# [13] impotence 

unique(imComplex2[imComplex2$StressPattern=="s-u","item.x"])
# [1] implications  immigration   imperfections impolite      immature      implication   immemorial   
# [8] immaterial    imposition    immorality   

unique(imComplex2[imComplex2$StressPattern=="u-s","item.x"])
#[1] impersonation


# now let's rename the levels (and only care fo the base inital syllablr)

levels(imComplex2$StressPattern)= c("d/u-str", "str-unstr", "str-unstr", "d/u-str")

levels(imComplex2$StressPattern)
#[1] "d/u-str"   "str-unstr"

# Let's take a look at it now

plot (imComplex2$RelDur ~ imComplex2$StressPattern)

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
#[1] 156  62

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

# Let's create the variable logRelDur - just for the sake of being able
# to get a summary of that variable

imComplex3$logRelDur <- log(imComplex3$RelDur)

###########################################################################
#  Now - let's get the summaries of the variables which are in our initial#
# model                                                                   #
###########################################################################



summary (imComplex3$logRelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.2640 -0.2016  0.2075     Inf  0.6915     Inf 


# we need to exclude all the ones which have a deleted I

imComplex4<-imComplex3[imComplex3$PrecSegDur>0,]

# we lost one item
dim(imComplex4)
#[1] 155  63

summary (imComplex4$logRelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.2640 -0.2016  0.2052  0.2346  0.6827  1.7680 


sd (imComplex4$logRelDur)
#[1] 0.5836334

# need to create the double and single df again

imComplex4Double<-imComplex4[imComplex4$NoCons=="double",]
imComplex4Single<-imComplex4[imComplex4$NoCons=="single",]

sd (imComplex4Single$RelDur)
#[1] 0.5174781

summary (imComplex4Single$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2826  0.6739  0.9221  1.0540  1.2470  2.7800 

sd (imComplex4Double$RelDur)
#[1] 1.01571

summary (imComplex4Double$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3687  1.0370  1.6250  1.8350  2.4030  5.8590 

# t-test

t.test(RelDur~NoCons, data=imComplex4)

# 
# data:  RelDur by NoCons
# t = -6.2292, df = 135.65, p-value = 5.502e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.0289701 -0.5330638
# sample estimates:
#   mean in group single mean in group double 
# 1.054405             1.835422 

summary (imComplex4$logWordFormFreqAllCoca)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.609   7.114   9.028   8.574   9.733  10.700 

sd(imComplex4$logWordFormFreqAllCoca)
#[1] 1.777179

summary (imComplex$logRelFreq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-9.6700 -0.2115  3.2570  3.5990  7.7140 10.4900 

sd (imComplex4$logRelFreq)
#[1]  4.731255

summary (imComplex4$WordDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2575  0.4664  0.5634  0.5753  0.6665  0.9748 

sd (imComplex4$WordDur)
#[1] 0.1502337


summary (imComplex4$PrecSegDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01296 0.03933 0.05723 0.06118 0.07776 0.12680 

sd(imComplex4$PrecSegDur)
#[1] 0.02521808

summary (imComplex4$NoSegWord)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.000   7.000   8.000   7.865   9.000  13.000 

sd (imComplex4$NoSegWord)
#[1] 1.682906

summary (imComplex4$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.279  12.040  14.100  14.300  16.620  24.320 

sd (imComplex4$LocSpeech)
#[1] 3.670669

summary (imComplex4$Affix)
#inLoc inNeg   
#70    85

summary (imComplex4$MorphBound)
#opaque transparent 
#104         51 

summary (imComplex4$NoCons)
#m     m#m 
#67     88 

summary(imComplex4$StressPattern)
#d/u-str str-unstr 
#116        39 

summary(imComplex4$median)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.626   3.000   4.000 


# Let's got some distributional data from Loc and Neg in


inNeg<-imComplex4[imComplex4$Affix=="inNeg",]
inLoc<-imComplex4[imComplex4$Affix=="inLoc",]

# First neg

summary (inNeg$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.4332  1.0930  1.6050  1.8480  2.3520  5.8590 

sd (inNeg$RelDur)
#[1]0.9835702

summary (inNeg$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.772  11.820  13.550  14.000  16.530  23.950 

sd (inNeg$LocSpeech)
#[1]3.492432

summary (inNeg[inNeg$NoCons=="double",]$RelDur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.7616  1.2670  1.9150  2.0520  2.4630  5.8590 

sd(inNeg[inNeg$NoCons=="double",]$RelDur)
#0.9907767

summary (inNeg[inNeg$NoCons=="single",]$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.4332  0.7969  1.0250  1.0860  1.2740  1.9260 

sd(inNeg[inNeg$NoCons=="single",]$RelDur)
#[1] 0.4356189

# let's also see how many doubles and singles we have

table(inNeg$NoCons)
# single double 
# 18     67 

# THAT IS INTERESTING

# Let's have a look at the types:

get_types(inNeg,"NoCons","double")
# [1] immaculate  immaterial  immature    immediacy   immediate   immediately immemorial 
# [8] immobilized immoral     immorality  immortal

get_types(inNeg,"NoCons","single")
# [1] imbalance     impair        impaired      impairing     impairment    impartial    
# [7] impatient     impeccable    imperfect     imperfections impersonal    impolite     
# [13] impossibility impossible    impotence     impractical   improper      improperly

# typewise it looks okay!

# inLoc

summary (inLoc$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2826  0.6486  0.8963  1.0730  1.2570  3.0310 


sd (inLoc$RelDur)
#[1]0.6175468

summary (inLoc$LocSpeech)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.279  12.770  14.630  14.680  16.920  24.320 


sd (inLoc$LocSpeech)
#[1]3.869009

summary (inLoc[inLoc$NoCons=="double",]$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3687  0.6476  0.8175  1.1430  1.4770  3.0310 


sd(inLoc[inLoc$NoCons=="double",]$RelDur)
#0.7662692

summary (inLoc[inLoc$NoCons=="single",]$RelDur)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2826  0.6516  0.9164  1.0430  1.2280  2.7800 


sd(inLoc[inLoc$NoCons=="single",]$RelDur)
#0.5481589

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

tmp.lmer <- lmer(RelDur ~ 1 + (1|item) + (1|Speaker), data = imComplex4)
cor(imComplex4$RelDur, fitted(tmp.lmer))^2
#[1] 0.9911142


tmp.lmer <- lmer(RelDur ~ 1 + (1|item), data = imComplex4)
cor(imComplex4$RelDur, fitted(tmp.lmer))^2
#[1] 0.3882351

#              Do an initial model:

imComplex.lm1 <- lm (RelDur ~ NoCons+ logWordFormFreqAllCoca + logRelFreq + LocSpeech +  median + Affix + MorphBound + StressPattern, data = imComplex4)

summary(imComplex.lm1)    

# Call:
#   lm(formula = RelDur ~ NoCons + logWordFormFreqAllCoca + logRelFreq + 
#        LocSpeech + median + Affix + MorphBound + StressPattern, 
#      data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3419 -0.4407 -0.1346  0.3215  3.2059 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.2856712  0.5800760   0.492 0.623126    
#   NoConsdouble            0.5679114  0.1937099   2.932 0.003914 ** 
#   logWordFormFreqAllCoca  0.0526464  0.0491395   1.071 0.285773    
#   logRelFreq              0.0006975  0.0203871   0.034 0.972756    
#   LocSpeech               0.0583533  0.0172353   3.386 0.000912 ***
#   median                 -0.0907136  0.1193876  -0.760 0.448585    
#   AffixinNeg              0.1924707  0.1746657   1.102 0.272302    
#   MorphBoundtransparent  -0.3230097  0.2529082  -1.277 0.203566    
#   StressPatternstr-unstr -0.6355614  0.1920751  -3.309 0.001180 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.739 on 146 degrees of freedom
# Multiple R-squared:  0.3897,	Adjusted R-squared:  0.3562 
# F-statistic: 11.65 on 8 and 146 DF,  p-value: 9.824e-13

# Es scheint auf den ersten Blick so, als hätten NoCons, LocSpeech,
# und stress pattern einen Einfluss,aber wir haben im decomposability script gesehen,
# dass  zwischen Affix, MorphBound und median komplexe Beziehungen bestehen.
#
# Zusätzlich gibt es das Problem, dass stress pattern, NoCons and Affix
# also in a complex relation stehen

# 1. Affix and MorphBound

# Ich möchte schauen, inwiefern MorphBound durch Affix "beeinflusst wird" 

# lets see what they do individually

imComplex1.lmAffix <- lm (RelDur ~  Affix, data = imComplex4)
summary(imComplex1.lmAffix) 

# 
# Call:
#   lm(formula = RelDur ~ Affix, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4147 -0.5667 -0.2027  0.3983  4.0108 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.0728     0.1002  10.705  < 2e-16 ***
#   AffixinNeg    0.7750     0.1353   5.726 5.27e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8385 on 153 degrees of freedom
# Multiple R-squared:  0.1765,	Adjusted R-squared:  0.1711 
# F-statistic: 32.79 on 1 and 153 DF,  p-value: 5.272e-08

# so negative ins are longer

imComplex1.lmMorphBound <- lm (RelDur ~  MorphBound, data = imComplex4)
summary(imComplex1.lmMorphBound) 
# 
# Call:
#   lm(formula = RelDur ~ MorphBound, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3871 -0.6334 -0.2581  0.4692  4.1888 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            1.66973    0.08731  19.125  < 2e-16 ***
#   MorphBoundtransparent -0.52248    0.15221  -3.433 0.000769 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8904 on 153 degrees of freedom
# Multiple R-squared:  0.07151,	Adjusted R-squared:  0.06544 
# F-statistic: 11.78 on 1 and 153 DF,  p-value: 0.0007688


# und opaque scheinen länger ABER Unterschied minimal und ich glaube hängt sehr von
# immediate ab - let's test it

# immediate and immediately make up 51!!!!! of the 105 opaque items,
#so we need to be careful with the interpretation! 


# What happens if we include both?

imComplex1.lmMorphBoundAffix <- lm (RelDur ~  MorphBound + Affix, data = imComplex4)
summary(imComplex1.lmMorphBoundAffix) 

# 
# Call:
#   lm(formula = RelDur ~ MorphBound + Affix, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.1993 -0.5821 -0.1751  0.3517  3.8977 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.2400     0.1129  10.982  < 2e-16 ***
#   MorphBoundtransparent  -0.4178     0.1412  -2.960  0.00357 ** 
#   AffixinNeg              0.7209     0.1333   5.409 2.42e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.818 on 152 degrees of freedom
# Multiple R-squared:  0.2214,	Adjusted R-squared:  0.2111 
# F-statistic: 21.61 on 2 and 152 DF,  p-value: 5.515e-09


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

imComplex1.lmInt1 <- lm (RelDur ~  MorphBound * Affix, data = imComplex4)
summary(imComplex1.lmInt1) 

# Call:
#   lm(formula = RelDur ~ MorphBound * Affix, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3438 -0.4978 -0.1193  0.3122  3.7532 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        1.0267     0.1205   8.517 1.54e-14 ***
#   MorphBoundtransparent              0.1154     0.1906   0.605 0.545878    
# AffixinNeg                         1.0787     0.1561   6.909 1.28e-10 ***
#   MorphBoundtransparent:AffixinNeg  -1.0671     0.2696  -3.958 0.000116 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7812 on 151 degrees of freedom
# Multiple R-squared:  0.2945,	Adjusted R-squared:  0.2805 
# F-statistic: 21.01 on 3 and 151 DF,  p-value: 1.971e-11



# the interaction is significant:

# however we don't know how robust this interaction will be later. Let's keep it in 
# mind

# Median and SemanticTranspareny and Affix


imComplex.lmInt2 <- lm (RelDur ~  median * Affix, data = imComplex4)
summary(imComplex.lmInt2) 


# Call:
#   lm(formula = RelDur ~ median * Affix, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4238 -0.5047 -0.1658  0.3699  3.8255 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         1.4600     0.3335   4.378 2.23e-05 ***
#   median             -0.1419     0.1172  -1.211 0.227861    
# AffixinNeg         -0.6384     0.4193  -1.523 0.129960    
# median:AffixinNeg   0.5457     0.1503   3.631 0.000386 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7934 on 151 degrees of freedom
# Multiple R-squared:  0.2723,	Adjusted R-squared:  0.2579 
# F-statistic: 18.84 on 3 and 151 DF,  p-value: 1.971e-10

visreg(imComplex.lmInt2, "median",by="Affix")

# that is actually quite interesting. It looks like for negIn it makes adifference
# whether it is perceived as easy or difficult to segment, while that is not the
# case for inLoc - 
#however we do not have that many types for each caegory, so better be cautios
# It basically tells us , the word immediate is pronounced long


imComplex.lmInt3 <- lm (RelDur ~  MorphBound * median, data = imComplex4)
summary(imComplex.lmInt3) 
# no interaction




# 2. The frequencies

#  The frequencies do not seem to have a huge effect. However
# as we stated above we might also have a collinearity effect.
# Thus we test the freqeuncies

cor.test(imComplex4$logWordFormFreqAllCoca, imComplex4$logRelFreq)

# Pearson's product-moment correlation
# 
# data:  imComplex4$logWordFormFreqAllCoca and imComplex4$logRelFreq
# t = 9.2374, df = 153, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.4866129 0.6908400
# sample estimates:
#   cor 
# 0.5983589 



imComplex4.lmFreq1 <- lm (RelDur ~ logWordFormFreqAllCoca , data = imComplex4)
summary(imComplex4.lmFreq1) 

# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3567 -0.5633 -0.1372  0.3936  4.1298 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -0.21169    0.33860  -0.625    0.533    
# logWordFormFreqAllCoca  0.19937    0.03867   5.155 7.72e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8529 on 153 degrees of freedom
# Multiple R-squared:  0.148,	Adjusted R-squared:  0.1424 
# F-statistic: 26.58 on 1 and 153 DF,  p-value: 7.716e-07

imComplex4.lmFreq2 <- lm (RelDur ~ logRelFreq , data = imComplex4)
summary(imComplex4.lmFreq2) 


# Call:
#   lm(formula = RelDur ~ logRelFreq, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.5480 -0.6199 -0.1443  0.4158  3.8214 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.19557    0.08277  14.445  < 2e-16 ***
#   logRelFreq   0.08647    0.01410   6.133 7.05e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8279 on 153 degrees of freedom
# Multiple R-squared:  0.1973,	Adjusted R-squared:  0.1921 
# F-statistic: 37.61 on 1 and 153 DF,  p-value: 7.055e-09



imComplex4.lmFreq3 <- lm (RelDur ~ logWordFormFreqAllCoca + logRelFreq , data = imComplex4)
summary(imComplex4.lmFreq3) 

# Call:
#   lm(formula = RelDur ~ logWordFormFreqAllCoca + logRelFreq, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4881 -0.5933 -0.0738  0.4063  3.8448 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.44782    0.37021   1.210 0.228296    
# logWordFormFreqAllCoca  0.09600    0.04635   2.071 0.040039 *  
#   logRelFreq              0.06489    0.01741   3.727 0.000273 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8191 on 152 degrees of freedom
# Multiple R-squared:  0.2193,	Adjusted R-squared:  0.2091 
# F-statistic: 21.35 on 2 and 152 DF,  p-value: 6.716e-09



# we use both - no supression

# 4. StressPattern and LocSpeech

# stress Pattern
imComplex4.lmStressPattern<- lm (RelDur ~ StressPattern , data = imComplex4)
summary(imComplex4.lmStressPattern)

# Call:
#   lm(formula = RelDur ~ StressPattern, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.1955 -0.6149 -0.2202  0.3817  4.1888 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.66978    0.08119  20.566  < 2e-16 ***
#   StressPatternstr-unstr -0.68344    0.16186  -4.222 4.13e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8745 on 153 degrees of freedom
# Multiple R-squared:  0.1044,	Adjusted R-squared:  0.09851 
# F-statistic: 17.83 on 1 and 153 DF,  p-value: 4.134e-05

# StressPattern: highly significant

# LocSpeech
imComplex4.lmLocSpeech<- lm (RelDur ~ LocSpeech , data = imComplex4)
summary(imComplex4.lmLocSpeech) 

# Call:
#   lm(formula = RelDur ~ LocSpeech, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.5517 -0.6368 -0.2279  0.4632  3.9112 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.78300    0.29351   2.668  0.00846 **
#   LocSpeech    0.04997    0.01988   2.514  0.01298 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9055 on 153 degrees of freedom
# Multiple R-squared:  0.03967,	Adjusted R-squared:  0.03339 
# F-statistic: 6.319 on 1 and 153 DF,  p-value: 0.01298

# LocSpeech: significant


# LocSpeech and StressPattern
imComplex4.lmLocSpeechStressPattern <- lm (RelDur ~ LocSpeech +StressPattern , data = imComplex4)
summary(imComplex4.lmLocSpeechStressPattern)

# Call:
#   lm(formula = RelDur ~ LocSpeech + StressPattern, data = imComplex4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4438 -0.5787 -0.1943  0.3956  3.5314 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.70346    0.27286   2.578 0.010884 *  
#   LocSpeech               0.06969    0.01886   3.696 0.000305 ***
#   StressPatternstr-unstr -0.80481    0.15899  -5.062 1.18e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8404 on 152 degrees of freedom
# Multiple R-squared:  0.1782,	Adjusted R-squared:  0.1674 
# F-statistic: 16.48 on 2 and 152 DF,  p-value: 3.325e-07



# Nothing changes

#thus we can keep both in without worrying about supression effects

##############################################################################################
#                                                                                 ############
#              summary coll.                                                      ############
##############################################################################################

# Now we have dealt with all collinearity problems: 
#- keep both frequency variable
# decided to look at the interaction of ST and Affix, as well as ST and median later
# leave Stress and LocSpeech in

###################################################################
#                                                                 #
# Let's now check the assumptions of our model:                   #
###################################################################


par(mfrow=c(1,1))


qqnorm (residuals (imComplex.lm1))
qqline (residuals (imComplex.lm1))

# That does not look that good.

## The qq plot shows that the residuals are not normally distributed --
# this means that the assumption of a linear relation between the dependent
# and the independent variable is violated.

# What to do?
# - transform the response variable
# - transform one or more of the predictors
# - add higher-order predictors

# Use log(RelDur):
imComplex.lm2 <- lm (log(RelDur) ~ NoCons+ logWordFormFreqAllCoca + logRelFreq + LocSpeech +  median + Affix + MorphBound + StressPattern, data = imComplex4)

summary(imComplex.lm2) 

# Call:
#   lm(formula = log(RelDur) ~ NoCons + logWordFormFreqAllCoca + 
#        logRelFreq + LocSpeech + median + Affix + MorphBound + StressPattern, 
#      data = imComplex4)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.04243 -0.29264 -0.02331  0.28646  1.13078 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)            -0.41742    0.34669  -1.204 0.230535    
#   NoConsdouble            0.41472    0.11577   3.582 0.000463 ***
#   logWordFormFreqAllCoca  0.02961    0.02937   1.008 0.315036    
#   logRelFreq             -0.00615    0.01218  -0.505 0.614528    
#   LocSpeech               0.03850    0.01030   3.737 0.000266 ***
#   median                 -0.08793    0.07135  -1.232 0.219799    
#   AffixinNeg              0.13659    0.10439   1.308 0.192775    
#   MorphBoundtransparent  -0.23950    0.15116  -1.584 0.115248    
#   StressPatternstr-unstr -0.52343    0.11480  -4.560 1.08e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4417 on 146 degrees of freedom
# Multiple R-squared:  0.4571,	Adjusted R-squared:  0.4273 
# F-statistic: 15.37 on 8 and 146 DF,  p-value: 3.039e-16


# effects remain the same in total - but median and ST become sign 

qqnorm (residuals (imComplex.lm2))
qqline (residuals (imComplex.lm2))

# it looks pretty good


# However maybe a box-cox transformation will lead to an even more satisfactory
# distribuition of res. Let's try


bc <- boxcox(imComplex.lm1)

lambda <- bc$x[which.max(bc$y)]

imComplex4$bc <- imComplex4$RelDur^lambda

imComplexBC.lm1 <- lm (bc~ NoCons + logRelFreq + LocSpeech + median + StressPattern + Affix +MorphBound, data = imComplex4)

summary(imComplexBC.lm1)
# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        StressPattern + Affix + MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112753 -0.029062  0.001138  0.028405  0.109445 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0197695  0.0269385  37.855  < 2e-16 ***
#   NoConsdouble           -0.0459508  0.0102918  -4.465 1.59e-05 ***
#   logRelFreq              0.0003881  0.0011648   0.333 0.739481    
#   LocSpeech              -0.0037300  0.0010128  -3.683 0.000324 ***
#   median                  0.0086075  0.0070119   1.228 0.221574    
#   StressPatternstr-unstr  0.0509763  0.0111836   4.558 1.08e-05 ***
#   AffixinNeg             -0.0136120  0.0102751  -1.325 0.187307    
#   MorphBoundtransparent   0.0261991  0.0146863   1.784 0.076501 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04347 on 147 degrees of freedom
# Multiple R-squared:  0.4549,	Adjusted R-squared:  0.429 
# F-statistic: 17.53 on 7 and 147 DF,  p-value: < 2.2e-16

#let's check the assumptions

qqnorm(residuals(imComplexBC.lm1))
qqline (residuals (imComplexBC.lm1))


# this looks actually pretty good.

# We will work with this model! --> imComplexBC.lm1

# let's check what lambda is

lambda
#[1] -0.1010101

######################################
# We should check our variable accent befire we simplify since we suspect some
# problems and we have 3 different accent variables...
# What we will do is to fit 3 models: one for each accet variable and
# we will check whether any of teh variables has any influence
#########################################

#
imComplexBC.lmAccent1 <- lm (bc~ PhrasalAccentSonia+StressPattern, data = imComplex4)
summary(imComplexBC.lmAccent1)

# yes...


imComplexBC.lmAccent2 <- lm (bc~ AccentIngo+StressPattern, data = imComplex4)
summary(imComplexBC.lmAccent2)


# yes...

# for the third one, I would like to recode the variable, sonia's imclears
# into no (since she did not hear any accent but just was not sure whether this
# can really be true). If we recode the imsures into nos, we get the clearly
# accented ones on the one hand and all the others on the other


levels(imComplex4$AnyAccentSonia)
#[1] "no"      "imclear" "yes"    

levels(imComplex4$AnyAccentSonia)<- c("no", "no", "yes")

levels(imComplex4$AnyAccentSonia)
#[1] "no"  "yes"

imComplexBC.lmAccent3 <- lm (bc~ AnyAccentSonia+StressPattern, data = imComplex4)
summary(imComplexBC.lmAccent3)


# no

# what about an interaction

imComplexBC.lmAccent4 <- lm (bc~ AnyAccentSonia*StressPattern, data = imComplex4)
summary(imComplexBC.lmAccent4)

# no

# Well, we should put one ine and see what happens

###########################################################################
#    Later of we also coded for type of root: We should check its influence!
################################################################################

#let's see what happens iof we put in bound root instead of MorphBound. Then we have to
# read in another table

r <- read.table("C:/Users/sbenhedia/Dropbox/Geminates/Corpus/un_im_ly/csv/Einzelne_Variablen/Roots/type_of_root.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

imComplex5<- merge(imComplex4, r, by.x="item", by.y="types")

dim(imComplex5)
#[1] 155  65

dim(imComplex4)
#[1] 155  64


# now let#s do that model

imComplexBC.lmRoot<- lm(bc ~ NoCons + LocSpeech + StressPattern+ type_of_base, data = imComplex5)
summary(imComplexBC.lmRoot)


# Type of base does not have an effect - what about a model, in which it
# is the only factor

imComplexBC.lmRoot2<- lm(bc ~ type_of_base, data = imComplex5)
summary(imComplexBC.lmRoot2)

# it does have an effect - I guess we should include it 

#########################################################################################
#                                                                                       #
#                      Simplification of the model                                      #
#########################################################################################

summary(imComplexBC.lm1)


# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        StressPattern + Affix + MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112753 -0.029062  0.001138  0.028405  0.109445 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.0197695  0.0269385  37.855  < 2e-16 ***
#   NoConsdouble           -0.0459508  0.0102918  -4.465 1.59e-05 ***
#   logRelFreq              0.0003881  0.0011648   0.333 0.739481    
# LocSpeech              -0.0037300  0.0010128  -3.683 0.000324 ***
#   median                  0.0086075  0.0070119   1.228 0.221574    
# StressPatternstr-unstr  0.0509763  0.0111836   4.558 1.08e-05 ***
#   AffixinNeg             -0.0136120  0.0102751  -1.325 0.187307    
# MorphBoundtransparent   0.0261991  0.0146863   1.784 0.076501 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04347 on 147 degrees of freedom
# Multiple R-squared:  0.4549,	Adjusted R-squared:  0.429 
# F-statistic: 17.53 on 7 and 147 DF,  p-value: < 2.2e-16

# Let's trim the model


# Before trimming the model, we need to add accent and root

imComplexBC.lm2<-lm(bc ~ NoCons + logRelFreq + LocSpeech + median + AccentIngo+ type_of_base+ 
  StressPattern + Affix + MorphBound, data = imComplex5)

summary(imComplexBC.lm2)
# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        AccentIngo + type_of_base + StressPattern + Affix + MorphBound, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.113868 -0.030412  0.001015  0.028320  0.109551 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0140267  0.0296460  34.205  < 2e-16 ***
#   NoConsdouble           -0.0486851  0.0134040  -3.632 0.000390 ***
#   logRelFreq              0.0005521  0.0011929   0.463 0.644166    
#   LocSpeech              -0.0036775  0.0010495  -3.504 0.000611 ***
#   median                  0.0088216  0.0071240   1.238 0.217617    
#   AccentIngounclear       0.0215957  0.0174818   1.235 0.218723    
#   AccentIngoyes           0.0033890  0.0092803   0.365 0.715512    
#   type_of_baseword       -0.0053018  0.0175096  -0.303 0.762485    
#   StressPatternstr-unstr  0.0512645  0.0114319   4.484 1.48e-05 ***
#   AffixinNeg             -0.0110403  0.0121901  -0.906 0.366618    
#   MorphBoundtransparent   0.0306650  0.0184979   1.658 0.099544 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04367 on 144 degrees of freedom
# Multiple R-squared:  0.4612,	Adjusted R-squared:  0.4238 
# F-statistic: 12.33 on 10 and 144 DF,  p-value: 2.85e-15

# Now let's throw out type of base


imComplexBC.lm3<-update(imComplexBC.lm2, ~ . - type_of_base)

summary(imComplexBC.lm3)

# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        AccentIngo + StressPattern + Affix + MorphBound, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112923 -0.030170  0.001087  0.028325  0.109095 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.0130970  0.0293940  34.466  < 2e-16 ***
#   NoConsdouble           -0.0461132  0.0103367  -4.461 1.62e-05 ***
#   logRelFreq              0.0005067  0.0011797   0.430 0.668180    
# LocSpeech              -0.0036846  0.0010459  -3.523 0.000571 ***
#   median                  0.0091190  0.0070338   1.296 0.196880    
# AccentIngounclear       0.0217925  0.0174149   1.251 0.212815    
# AccentIngoyes           0.0031862  0.0092270   0.345 0.730362    
# StressPatternstr-unstr  0.0518144  0.0112513   4.605 8.94e-06 ***
#   AffixinNeg             -0.0129935  0.0103110  -1.260 0.209639    
# MorphBoundtransparent   0.0272968  0.0147330   1.853 0.065950 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04353 on 145 degrees of freedom
# Multiple R-squared:  0.4609,	Adjusted R-squared:  0.4274 
# F-statistic: 13.77 on 9 and 145 DF,  p-value: 7.715e-16

# Model becomes better, otherwise no changes, so let's go on with
# the simplification by throwing out Accent - so that is model 1...

summary(imComplexBC.lm1)

# Call:
#   lm(formula = bc ~ NoCons + logRelFreq + LocSpeech + median + 
#        StressPattern + Affix + MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112753 -0.029062  0.001138  0.028405  0.109445 
# 
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0197695  0.0269385  37.855  < 2e-16 ***
#   NoConsdouble           -0.0459508  0.0102918  -4.465 1.59e-05 ***
#   logRelFreq              0.0003881  0.0011648   0.333 0.739481    
#   LocSpeech              -0.0037300  0.0010128  -3.683 0.000324 ***
#   median                  0.0086075  0.0070119   1.228 0.221574    
#  StressPatternstr-unstr  0.0509763  0.0111836   4.558 1.08e-05 ***
#   AffixinNeg             -0.0136120  0.0102751  -1.325 0.187307    
#   MorphBoundtransparent   0.0261991  0.0146863   1.784 0.076501 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04347 on 147 degrees of freedom
# Multiple R-squared:  0.4549,	Adjusted R-squared:  0.429 
# F-statistic: 17.53 on 7 and 147 DF,  p-value: < 2.2e-16

# now RelFreq
imComplexBC.lm4<-update(imComplexBC.lm1, ~ . - logRelFreq)

summary(imComplexBC.lm4)


# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix + MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112736 -0.029159  0.001141  0.028403  0.108431 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0191326  0.0267897  38.042  < 2e-16 ***
#   NoConsdouble           -0.0443282  0.0090392  -4.904 2.44e-06 ***
#   LocSpeech              -0.0036621  0.0009891  -3.702 0.000301 ***
#   median                  0.0087977  0.0069676   1.263 0.208696    
#   StressPatternstr-unstr  0.0504386  0.0110332   4.572 1.01e-05 ***
#   AffixinNeg             -0.0131243  0.0101397  -1.294 0.197560    
#   MorphBoundtransparent   0.0245862  0.0138238   1.779 0.077368 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04334 on 148 degrees of freedom
# Multiple R-squared:  0.4545,	Adjusted R-squared:  0.4324 
# F-statistic: 20.55 on 6 and 148 DF,  p-value: < 2.2e-16

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

imComplexBC.lmIntAffixMorphBound <- lm (bc~ NoCons + LocSpeech + median + StressPattern + Affix*MorphBound, data = imComplex4)

summary(imComplexBC.lmIntAffixMorphBound)

# This interaction is not significant and does not help!

# what if we exclude median


imComplexBC.lmIntAffixMorphBound2<-update(imComplexBC.lmIntAffixMorphBound, ~ . - median)

summary(imComplexBC.lmIntAffixMorphBound2)

# still no interaction


# 2. Affix*NoCons

imComplexBC.lmIntAffixNoCons <- lm (bc~ LocSpeech+median + StressPattern + MorphBound+ Affix* NoCons , data = imComplex4)

summary(imComplexBC.lmIntAffixNoCons)

# This interaction is not significant

# 3. NoCons*MorphBound

imComplexBC.lmIntNoConsMorphBound <- lm (bc~ LocSpeech+median+ StressPattern + Affix + NoCons*MorphBound, data = imComplex4)
 

summary(imComplexBC.lmIntNoConsMorphBound )

# no

# 4.median*Affix

imComplexBC.lmIntAffixMedian <- lm (bc~ LocSpeech+MorphBound+ StressPattern + NoCons +Affix*median, data = imComplex4)

summary(imComplexBC.lmIntAffixMedian )
# This interaction is not significant



# 5. NoCons and LocSpeech 

# Let's see whether we can see an interaction woth LocSpeech
imComplexBC.lmIntNoConsSpeech <- lm (bc~  NoCons*LocSpeech, data = imComplex4)

summary(imComplexBC.lmIntNoConsSpeech)


# NO


# 6. LocSpeech*Affix

# Let's see whether we can see an interaction woth LocSpeech
imComplexBC.lmIntAffixSpeech <- lm (bc~  NoCons+ Affix*LocSpeech, data = imComplex4)

summary(imComplexBC.lmIntAffixSpeech)
# NO



# 7. StressPattern*Affix
imComplexBC.lmIntAffixStress <- lm (bc~ LocSpeech+MorphBound+  NoCons +Affix*StressPattern +median, data = imComplex4)

summary(imComplexBC.lmIntAffixStress )


# no

# 8. NoCons*StressPatter

imComplexBC.lmIntNoConsStress <- lm (bc~ LocSpeech+MorphBound+Affix+  NoCons *StressPattern +median, data = imComplex4)

summary(imComplexBC.lmIntNoConsStress )
# no

##############################################################################################
#             Summary interactions   --> Simplification of our model                        ##
##############################################################################################

# We don't find interaction, so we simply simplify further without any interactions

summary(imComplexBC.lm4)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix + MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112736 -0.029159  0.001141  0.028403  0.108431 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0191326  0.0267897  38.042  < 2e-16 ***
#   NoConsdouble           -0.0443282  0.0090392  -4.904 2.44e-06 ***
#   LocSpeech              -0.0036621  0.0009891  -3.702 0.000301 ***
#   median                  0.0087977  0.0069676   1.263 0.208696    
#   StressPatternstr-unstr  0.0504386  0.0110332   4.572 1.01e-05 ***
#   AffixinNeg             -0.0131243  0.0101397  -1.294 0.197560    
#   MorphBoundtransparent   0.0245862  0.0138238   1.779 0.077368 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04334 on 148 degrees of freedom
# Multiple R-squared:  0.4545,	Adjusted R-squared:  0.4324 
# F-statistic: 20.55 on 6 and 148 DF,  p-value: < 2.2e-16

# okay now we need to try out several things, because median, ST and Affix
# probably affect eachother a great deal and we need to think about how
# to deal with this. 


# first let's see what happens if we do an lmer (probably not the best idea
# but wee need to control this stupid immediate)

imComplexBC.lmer1 <- lmer (bc~ LocSpeech+MorphBound+Affix+  NoCons +StressPattern +median+ (1|item), data = imComplex4)

summary(imComplexBC.lmer1)
# same thing..

#Let's take out each at a time and see what happens


# 1. Without median

imComplexBC.lm5<-update(imComplexBC.lm4, ~ . - median)

summary(imComplexBC.lm5)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix + 
#        MorphBound, data = imComplex4)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.11229 -0.02849  0.00288  0.02850  0.11423 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.0470735  0.0151303  69.204  < 2e-16 ***
#   NoConsdouble           -0.0427072  0.0089654  -4.764 4.47e-06 ***
#   LocSpeech              -0.0035949  0.0009897  -3.632 0.000386 ***
#   StressPatternstr-unstr  0.0504726  0.0110552   4.566 1.04e-05 ***
#   AffixinNeg             -0.0172616  0.0096148  -1.795 0.074631 .  
# MorphBoundtransparent   0.0110295  0.0087250   1.264 0.208157    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04343 on 149 degrees of freedom
# Multiple R-squared:  0.4486,	Adjusted R-squared:  0.4301 
# F-statistic: 24.25 on 5 and 149 DF,  p-value: < 2.2e-16

# without median Affix becomes marg. significant --> they express the same!

# 2. Without  Affix

imComplexBC.lm6<-update(imComplexBC.lm4, ~ . - Affix)

summary(imComplexBC.lm6)


# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        MorphBound, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.108037 -0.029978  0.001246  0.031903  0.102711 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0039680  0.0241466  41.578  < 2e-16 ***
#   NoConsdouble           -0.0514644  0.0071793  -7.168 3.28e-11 ***
#   LocSpeech              -0.0035469  0.0009874  -3.592 0.000444 ***
#   median                  0.0117120  0.0066087   1.772 0.078403 .  
#   StressPatternstr-unstr  0.0572433  0.0097222   5.888 2.49e-08 ***
#   MorphBoundtransparent   0.0276474  0.0136508   2.025 0.044619 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04344 on 149 degrees of freedom
# Multiple R-squared:  0.4483,	Adjusted R-squared:  0.4298 
# F-statistic: 24.22 on 5 and 149 DF,  p-value: < 2.2e-16


# now median becomes significant!


# 3. Without MorphBound

imComplexBC.lm7<-update(imComplexBC.lm4, ~ . - MorphBound)

summary(imComplexBC.lm7)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.109114 -0.028237  0.001687  0.029381  0.106943 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.0508055  0.0201586  52.127  < 2e-16 ***
#   NoConsdouble           -0.0436525  0.0090965  -4.799 3.84e-06 ***
#   LocSpeech              -0.0035615  0.0009947  -3.581 0.000463 ***
#   median                 -0.0008270  0.0044206  -0.187 0.851856    
# StressPatternstr-unstr  0.0566383  0.0105438   5.372 2.94e-07 ***
#   AffixinNeg             -0.0162096  0.0100625  -1.611 0.109318    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04366 on 149 degrees of freedom
# Multiple R-squared:  0.4428,	Adjusted R-squared:  0.4241 
# F-statistic: 23.69 on 5 and 149 DF,  p-value: < 2.2e-16

# nothing really

# let' throw out median too

imComplexBC.lm8<-update(imComplexBC.lm7, ~ . - median)

summary(imComplexBC.lm8)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.108746 -0.027912  0.001725  0.029591  0.105373 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0483234  0.0151280  69.297  < 2e-16 ***
#   NoConsdouble           -0.0439482  0.0089292  -4.922 2.23e-06 ***
#   LocSpeech              -0.0035643  0.0009913  -3.595 0.000439 ***
#   StressPatternstr-unstr  0.0574402  0.0096023   5.982 1.55e-08 ***
#   AffixinNeg             -0.0156325  0.0095470  -1.637 0.103640    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04352 on 150 degrees of freedom
# Multiple R-squared:  0.4427,	Adjusted R-squared:  0.4279 
# F-statistic: 29.79 on 4 and 150 DF,  p-value: < 2.2e-16

# the effect of affix is gone!! Does that mean the duration of
# neg in is longer as a whole, and not just the nasal? Makes sense...


# Let's have a look at the distribution

table(imComplex4$Affix, imComplex4$MorphBound)

#         opaque transparent
# inLoc     42          28
# inNeg     62          23

table(imComplex4$Affix, imComplex4$median)
#        1  2  3  4
# inLoc  2 29 25 14
# inNeg 21  1 59  4

# There are A LOT of inNegs rated with 3 --> which items are those?

unique(inNeg[inNeg$median=="3","item"])
#[1] immediacy   immediate   immediately impeccable 

unique(inNeg[inNeg$median=="4","item"])
#[1] immaculate impair     impairing  impairment


table(imComplex4$MorphBound, imComplex4$median)
#             1  2  3  4
# opaque       0  6 81 17
# transparent 23 24  3  1

table(imComplex4$MorphBound, imComplex4$NoCons)
#               single double
# opaque          46     58
# transparent     21     30


table(imComplex4$median, imComplex4$NoCons)
#     single double
# 1     15      8
# 2      9     21
# 3     26     59
# 4     17      1

table(imComplex4$Affix, imComplex4$NoCons)
#        single double
# inLoc     49     21
# inNeg     18     67

# let's look at the distributions within inLoc and in Neg

table(inLoc$MorphBound, inLoc$NoCons)
#              single double
# opaque          42      0
# transparent      7     21

table(inNeg$MorphBound, inNeg$NoCons)

#                single double
# opaque           4     58
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


imComplex5<-imComplex4[imComplex4$item!="immediately"&imComplex4$item!="immediate",]

imComplexBC.lmWithoutImmediately1 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + Affix + median, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately1)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        Affix + median, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.112679 -0.028575  0.004916  0.027599  0.112733 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.027604   0.029078  35.340  < 2e-16 ***
#   LocSpeech              -0.004324   0.001316  -3.286  0.00144 ** 
#   NoConsdouble           -0.042015   0.013583  -3.093  0.00262 ** 
#   StressPatternstr-unstr  0.050565   0.011971   4.224 5.64e-05 ***
#   MorphBoundtransparent   0.024069   0.016496   1.459  0.14795    
#   AffixinNeg             -0.013037   0.011889  -1.097  0.27570    
#   median                  0.009055   0.007109   1.274  0.20597    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04418 on 92 degrees of freedom
# Multiple R-squared:  0.2479,	Adjusted R-squared:  0.1988 
# F-statistic: 5.053 on 6 and 92 DF,  p-value: 0.0001607

# this is really puszzling!!!!!


# 1. Without Affix

imComplexBC.lmWithoutImmediately2 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + median, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately2)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        median, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.109293 -0.029708  0.003501  0.030431  0.107989 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.014228   0.026424  38.383  < 2e-16 ***
#   LocSpeech              -0.004004   0.001285  -3.117  0.00243 ** 
#   NoConsdouble           -0.042118   0.013597  -3.098  0.00258 ** 
#   StressPatternstr-unstr  0.053455   0.011690   4.573 1.48e-05 ***
#   MorphBoundtransparent   0.021725   0.016374   1.327  0.18784    
#   median                  0.011181   0.006847   1.633  0.10584    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04423 on 93 degrees of freedom
# Multiple R-squared:  0.238,	Adjusted R-squared:  0.1971 
# F-statistic: 5.811 on 5 and 93 DF,  p-value: 0.000103


# 2. Without median

imComplexBC.lmWithoutImmediately3 <- lm (bc~ LocSpeech+NoCons +StressPattern + MorphBound + Affix, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately3)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound + 
#        Affix, data = imComplex5)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.11223 -0.03159  0.00294  0.02694  0.11844 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)             1.055765   0.018950  55.715  < 2e-16 ***
#   LocSpeech              -0.004210   0.001317  -3.195  0.00191 ** 
#   NoConsdouble           -0.040271   0.013559  -2.970  0.00379 ** 
#   StressPatternstr-unstr  0.050506   0.012010   4.205    6e-05 ***
#   MorphBoundtransparent   0.009988   0.012285   0.813  0.41825    
#   AffixinNeg             -0.017168   0.011477  -1.496  0.13807    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04433 on 93 degrees of freedom
# Multiple R-squared:  0.2346,	Adjusted R-squared:  0.1935 
# F-statistic: 5.701 on 5 and 93 DF,  p-value: 0.0001244


# 3. Without MorphBound

imComplexBC.lmWithoutImmediately4 <- lm (bc~ LocSpeech+NoCons +StressPattern + Affix + median, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately4)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + Affix + 
#        median, data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.110287 -0.029335  0.002113  0.027711  0.111218 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.047796   0.025728  40.726  < 2e-16 ***
#   LocSpeech              -0.003954   0.001299  -3.043  0.00304 ** 
#   NoConsdouble           -0.034098   0.012527  -2.722  0.00775 ** 
#   StressPatternstr-unstr  0.051900   0.012008   4.322 3.87e-05 ***
#   AffixinNeg             -0.010790   0.011861  -0.910  0.36532    
# median                  0.002104   0.005308   0.396  0.69278    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04445 on 93 degrees of freedom
# Multiple R-squared:  0.2305,	Adjusted R-squared:  0.1891 
# F-statistic:  5.57 on 5 and 93 DF,  p-value: 0.000156



# Let's also have a look at each variable withut the other 2


# 4. Only MorphBound

imComplexBC.lmWithoutImmediately5 <- lm (bc~ LocSpeech+NoCons +StressPattern +MorphBound, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately5)

# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + MorphBound, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.107265 -0.029645  0.003757  0.030605  0.113598 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.046143   0.017941  58.309  < 2e-16 ***
#   LocSpeech              -0.003717   0.001284  -2.895  0.00472 ** 
#   NoConsdouble           -0.039836   0.013644  -2.920  0.00439 ** 
#   StressPatternstr-unstr  0.054598   0.011772   4.638 1.13e-05 ***
#   MorphBoundtransparent   0.001950   0.011119   0.175  0.86117    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04462 on 94 degrees of freedom
# Multiple R-squared:  0.2162,	Adjusted R-squared:  0.1828 
# F-statistic: 6.482 on 4 and 94 DF,  p-value: 0.000119

# interestinglöy now the effect direction changed and it is no longer
# significant. This just shows, that the effect of SemTransparency cannot
# validly be checked with this dataset! There are a lot of supression effects
# and it is really challenging, if not impossible to make valid statements


# 5. Only Affix

imComplexBC.lmWithoutImmediately6 <- lm (bc~ LocSpeech+NoCons +StressPattern +Affix, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately6)


# Call:
#   lm(formula = bc ~ LocSpeech + NoCons + StressPattern + Affix, 
#      data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.110688 -0.029835  0.002985  0.027579  0.113999 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.054691   0.018869  55.895  < 2e-16 ***
#   LocSpeech              -0.003997   0.001289  -3.101  0.00254 ** 
#   NoConsdouble           -0.035316   0.012090  -2.921  0.00437 ** 
#   StressPatternstr-unstr  0.051546   0.011921   4.324 3.81e-05 ***
#   AffixinNeg             -0.013086   0.010302  -1.270  0.20712    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04425 on 94 degrees of freedom
# Multiple R-squared:  0.2292,	Adjusted R-squared:  0.1964 
# F-statistic: 6.987 on 4 and 94 DF,  p-value: 5.727e-05


# only marginally significant and effect direction changes...


# 6. Only median

imComplexBC.lmWithoutImmediately7 <- lm (bc~ LocSpeech+NoCons +StressPattern +median, data = imComplex5)

summary(imComplexBC.lmWithoutImmediately7)

# #Call:
# lm(formula = bc ~ LocSpeech + NoCons + StressPattern + median, 
#    data = imComplex5)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.107632 -0.031349  0.003778  0.029725  0.107349 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.034882   0.021438  48.274  < 2e-16 ***
#   LocSpeech              -0.003715   0.001271  -2.922  0.00435 ** 
#   NoConsdouble           -0.034833   0.012489  -2.789  0.00640 ** 
#   StressPatternstr-unstr  0.054223   0.011722   4.626 1.19e-05 ***
#   median                  0.004463   0.004627   0.965  0.33726    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04441 on 94 degrees of freedom
# Multiple R-squared:  0.2236,	Adjusted R-squared:  0.1906 
# F-statistic: 6.769 on 4 and 94 DF,  p-value: 7.845e-05


# nit significant

# All in all our analysis of the distribution of the data has revealed the followong:

# The variable Semantic Transparency is immensely influenced by the types with
# the root "immed" - 59 of ALL opaque words have this root. When throwing these
# words out of the analysis, the effect direction of Semantic Transparency changes 
#(sometimes, depending what else is in the model),
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

# Let's have a look at the model without morphbound


summary(imComplexBC.lm7)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + median + StressPattern + 
#        Affix, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.109114 -0.028237  0.001687  0.029381  0.106943 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0508055  0.0201586  52.127  < 2e-16 ***
#   NoConsdouble           -0.0436525  0.0090965  -4.799 3.84e-06 ***
#   LocSpeech              -0.0035615  0.0009947  -3.581 0.000463 ***
#   median                 -0.0008270  0.0044206  -0.187 0.851856    
#   StressPatternstr-unstr  0.0566383  0.0105438   5.372 2.94e-07 ***
#   AffixinNeg             -0.0162096  0.0100625  -1.611 0.109318    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04366 on 149 degrees of freedom
# Multiple R-squared:  0.4428,	Adjusted R-squared:  0.4241 
# F-statistic: 23.69 on 5 and 149 DF,  p-value: < 2.2e-16

# without median
imComplexBC.lm9<-update(imComplexBC.lm7, ~ . - median)

summary(imComplexBC.lm9)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern + Affix, 
#      data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.108746 -0.027912  0.001725  0.029591  0.105373 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             1.0483234  0.0151280  69.297  < 2e-16 ***
#   NoConsdouble           -0.0439482  0.0089292  -4.922 2.23e-06 ***
#   LocSpeech              -0.0035643  0.0009913  -3.595 0.000439 ***
#   StressPatternstr-unstr  0.0574402  0.0096023   5.982 1.55e-08 ***
#   AffixinNeg             -0.0156325  0.0095470  -1.637 0.103640    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04352 on 150 degrees of freedom
# Multiple R-squared:  0.4427,	Adjusted R-squared:  0.4279 
# F-statistic: 29.79 on 4 and 150 DF,  p-value: < 2.2e-16

# Affix is not sign- let's throw it out

# without median
imComplexBC.lm10<-update(imComplexBC.lm9, ~ . - Affix)

summary(imComplexBC.lm10)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.102805 -0.030200  0.000105  0.032489  0.101786 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.0401013  0.0143496  72.483  < 2e-16 ***
#   NoConsdouble           -0.0526678  0.0072073  -7.308 1.47e-11 ***
#   LocSpeech              -0.0033836  0.0009906  -3.416 0.000818 ***
#   StressPatternstr-unstr  0.0654481  0.0083094   7.876 6.13e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04376 on 151 degrees of freedom
# Multiple R-squared:  0.4328,	Adjusted R-squared:  0.4215 
# F-statistic:  38.4 on 3 and 151 DF,  p-value: < 2.2e-16

#########################################################################
#
# So, to summarize:
#
# This dataset is not perfect to test the effect of decomposability on
# the duration of the prefixal nasal but can only give us some hints in what
# direction to go. In contrast to the absolute duration we do not even see 
# an effect of affxi which suggests that the difference in the Affixes is 
# not only mirrored in the duration of the nasal but in the duration of
# the previos vowel - it is lenghtened proportionally. However, as discussed
# the analysis of the absolute duration- this dataset does not allow to make
# very confident about the effect of decomposability/ affix-specific 
# influences on duration. what is clear however is that in does geminate.

# now let's have a look at the sign. predictors:

par(mfrow=c(2,2))
visreg(imComplexBC.lm10,trans= function(x) x^(1/lambda))


# 1. Doubles are longer
# 2. Before stressed nasals are longer
# 3. The faster the speech rate, the longer the nasal in proportion to the vowel, which
#   makes sense, since there is more "material" for reduction in the vowel and it is 
#   thus reduced more than the nasal


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
#   lm(formula = bc ~ <128 unique rhs>, data = imComplex4)
# 
# Component models: 
#   df logLik    AICc delta weight
# 1267     6 268.47 -524.38  0.00   0.15
# 12567    7 269.30 -523.84  0.54   0.12
# 267      5 267.10 -523.80  0.58   0.11
# 24567    7 269.26 -523.76  0.62   0.11
# 124567   8 270.13 -523.28  1.10   0.09
# 2567     6 267.64 -522.72  1.66   0.07
# 12367    7 268.53 -522.30  2.08   0.05
# 12467    7 268.49 -522.22  2.16   0.05
# 123567   8 269.40 -521.81  2.57   0.04
# 2367     6 267.17 -521.78  2.60   0.04
# 2467     6 267.15 -521.74  2.64   0.04
# 234567   8 269.27 -521.55  2.83   0.04
# 1234567  9 270.19 -521.14  3.24   0.03
# 23567    7 267.67 -520.57  3.81   0.02
# 123467   8 268.53 -520.07  4.31   0.02
# 23467    7 267.32 -519.88  4.50   0.02
# 67       4 261.33 -514.40  9.98   0.00
# 167      5 262.07 -513.73 10.65   0.00
# 567      5 261.81 -513.21 11.17   0.00
# 4567     6 262.82 -513.08 11.31   0.00
# 367      5 261.73 -513.05 11.33   0.00
# 1567     6 262.73 -512.88 11.50   0.00
# 1367     6 262.45 -512.33 12.05   0.00
# 467      5 261.35 -512.29 12.09   0.00
# 14567    7 263.27 -511.77 12.61   0.00
# 1467     6 262.10 -511.62 12.76   0.00
# 3467     6 261.91 -511.25 13.13   0.00
# 3567     6 261.90 -511.24 13.14   0.00
# 34567    7 262.96 -511.15 13.23   0.00
# 13567    7 262.77 -510.79 13.60   0.00
# 13467    7 262.47 -510.18 14.20   0.00
# 134567   8 263.35 -509.71 14.67   0.00
# 1237     6 259.89 -507.21 17.17   0.00
# 1256     6 259.15 -505.74 18.64   0.00
# 12357    7 260.00 -505.24 19.14   0.00
# 12347    7 259.92 -505.07 19.31   0.00
# 12456    7 259.89 -505.03 19.35   0.00
# 1257     6 258.32 -504.06 20.32   0.00
# 123457   8 260.33 -503.68 20.70   0.00
# 12356    7 259.18 -503.59 20.79   0.00
# 127      5 256.87 -503.34 21.04   0.00
# 123456   8 259.94 -502.90 21.48   0.00
# 12457    7 258.46 -502.16 22.22   0.00
# 1247     6 257.35 -502.14 22.24   0.00
# 1235     6 255.62 -498.68 25.70   0.00
# 156      5 254.42 -498.43 25.95   0.00
# 125      5 254.29 -498.18 26.20   0.00
# 1236     6 255.17 -497.77 26.61   0.00
# 12345    7 256.07 -497.37 27.01   0.00
# 1456     6 254.93 -497.29 27.09   0.00
# 12346    7 255.88 -497.00 27.39   0.00
# 1246     6 254.77 -496.98 27.40   0.00
# 123      5 253.68 -496.95 27.43   0.00
# 1356     6 254.73 -496.89 27.49   0.00
# 1245     6 254.53 -496.50 27.88   0.00
# 137      5 253.38 -496.36 28.02   0.00
# 13456    7 255.32 -495.88 28.50   0.00
# 1234     6 254.00 -495.44 28.94   0.00
# 1347     6 253.50 -494.44 29.94   0.00
# 1357     6 253.38 -494.20 30.18   0.00
# 2347     6 253.31 -494.05 30.33   0.00
# 136      5 252.13 -493.87 30.52   0.00
# 2456     6 253.05 -493.53 30.85   0.00
# 126      5 251.89 -493.38 31.00   0.00
# 23457    7 253.78 -492.80 31.58   0.00
# 23456    7 253.70 -492.64 31.75   0.00
# 1346     6 252.54 -492.52 31.86   0.00
# 13457    7 253.59 -492.41 31.97   0.00
# 237      5 251.38 -492.35 32.03   0.00
# 124      5 251.19 -491.98 32.40   0.00
# 13       4 249.80 -491.33 33.05   0.00
# 146      5 250.86 -491.31 33.07   0.00
# 2357     6 251.68 -490.80 33.58   0.00
# 135      5 250.54 -490.68 33.70   0.00
# 134      5 249.87 -489.35 35.03   0.00
# 3456     6 250.95 -489.33 35.06   0.00
# 456      5 249.84 -489.27 35.11   0.00
# 1345     6 250.83 -489.10 35.28   0.00
# 157      5 249.55 -488.70 35.68   0.00
# 256      5 249.50 -488.61 35.78   0.00
# 12       4 248.35 -488.44 35.95   0.00
# 16       4 248.30 -488.34 36.04   0.00
# 17       4 248.30 -488.33 36.05   0.00
# 2356     6 250.15 -487.73 36.65   0.00
# 147      5 248.92 -487.43 36.95   0.00
# 347      5 248.56 -486.72 37.66   0.00
# 1457     6 249.57 -486.58 37.81   0.00
# 15       4 247.39 -486.51 37.87   0.00
# 56       4 247.10 -485.94 38.44   0.00
# 356      5 248.14 -485.88 38.50   0.00
# 37       4 246.58 -484.89 39.49   0.00
# 3457     6 248.70 -484.83 39.55   0.00
# 145      5 247.44 -484.49 39.90   0.00
# 357      5 247.18 -483.97 40.42   0.00
# 236      5 246.81 -483.21 41.17   0.00
# 36       4 245.63 -482.99 41.39   0.00
# 14       4 245.40 -482.53 41.85   0.00
# 2346     6 246.81 -481.05 43.33   0.00
# 346      5 245.64 -480.87 43.51   0.00
# 2457     6 246.30 -480.02 44.36   0.00
# 1        3 242.96 -479.76 44.62   0.00
# 27       4 243.63 -479.00 45.38   0.00
# 257      5 244.27 -478.13 46.25   0.00
# 2345     6 245.31 -478.06 46.32   0.00
# 247      5 243.71 -477.02 47.36   0.00
# 345      5 242.53 -474.66 49.72   0.00
# 23       4 241.24 -474.22 50.16   0.00
# 246      5 241.94 -473.48 50.90   0.00
# 234      5 241.82 -473.24 51.14   0.00
# 235      5 241.72 -473.05 51.33   0.00
# 3        3 239.53 -472.90 51.48   0.00
# 46       4 240.53 -472.79 51.59   0.00
# 26       4 240.43 -472.59 51.79   0.00
# 34       4 240.25 -472.23 52.15   0.00
# 6        3 239.07 -471.99 52.39   0.00
# 35       4 239.69 -471.11 53.27   0.00
# 7        3 236.53 -466.89 57.49   0.00
# 457      5 238.40 -466.39 57.99   0.00
# 57       4 237.09 -465.91 58.47   0.00
# 47       4 236.55 -464.83 59.55   0.00
# 245      5 236.62 -462.84 61.54   0.00
# 25       4 232.99 -457.72 66.66   0.00
# 45       4 231.68 -455.09 69.29   0.00
# 5        3 229.05 -451.94 72.44   0.00
# 2        3 225.82 -445.47 78.91   0.00
# 24       4 226.73 -445.20 79.18   0.00
# (Null)   2 223.16 -442.24 82.14   0.00
# 4        3 223.99 -441.83 82.55   0.00
# 
# Term codes: 
#   Affix     LocSpeech    logRelFreq        median    MorphBound        NoCons 
# 1             2             3             4             5             6 
# StressPattern 
# 7 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             1.035e+00  2.483e-02   2.494e-02  41.495  < 2e-16 ***
#   AffixinNeg             -8.618e-03  1.071e-02   1.075e-02   0.802 0.422783    
# LocSpeech              -3.504e-03  1.036e-03   1.043e-03   3.358 0.000785 ***
#   NoConsdouble           -4.748e-02  9.637e-03   9.701e-03   4.894    1e-06 ***
#   StressPatternstr-unstr  5.729e-02  1.129e-02   1.136e-02   5.043    5e-07 ***
#   MorphBoundtransparent   9.716e-03  1.398e-02   1.403e-02   0.693 0.488483    
# median                  2.808e-03  6.025e-03   6.045e-03   0.464 0.642347    
# logRelFreq             -4.315e-06  5.895e-04   5.938e-04   0.007 0.994202    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)             1.035e+00  2.483e-02   2.494e-02  41.495  < 2e-16 ***
#   AffixinNeg             -1.566e-02  9.906e-03   9.985e-03   1.569 0.116717    
# LocSpeech              -3.526e-03  1.001e-03   1.010e-03   3.492 0.000479 ***
#   NoConsdouble           -4.748e-02  9.630e-03   9.693e-03   4.898    1e-06 ***
#   StressPatternstr-unstr  5.730e-02  1.128e-02   1.136e-02   5.045    5e-07 ***
#   MorphBoundtransparent   1.896e-02  1.436e-02   1.444e-02   1.313 0.189217    
# median                  7.168e-03  7.838e-03   7.877e-03   0.910 0.362820    
# logRelFreq             -1.654e-05  1.154e-03   1.162e-03   0.014 0.988650    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Relative variable importance: 
#                    StressPattern NoCons LocSpeech Affix MorphBound median logRelFreq
# Importance:          1.00          1.00   0.99      0.55  0.51       0.39   0.26      
# N containing models:   64            64     64        64    64         64     64    

# So here we see again, teh complication with MorphBound, median and Affix:
# We have already discussed that....

# so MuMin tells us the same

# I need to change the ref-level...

imComplex4$NoCons <- relevel (imComplex4$NoCons, ref= "double")
levels(imComplex4$NoCons)
#[1] "double" "single"


# redo the model


finalImRelDur.lm<-lm(bc ~ NoCons + LocSpeech + StressPattern, data = imComplex4)


summary(finalImRelDur.lm)

# Call:
#   lm(formula = bc ~ NoCons + LocSpeech + StressPattern, data = imComplex4)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.102805 -0.030200  0.000105  0.032489  0.101786 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.9874335  0.0151250  65.285  < 2e-16 ***
#   NoConssingle            0.0526678  0.0072073   7.308 1.47e-11 ***
#   LocSpeech              -0.0033836  0.0009906  -3.416 0.000818 ***
#   StressPatternstr-unstr  0.0654481  0.0083094   7.876 6.13e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04376 on 151 degrees of freedom
# Multiple R-squared:  0.4328,	Adjusted R-squared:  0.4215 
# F-statistic:  38.4 on 3 and 151 DF,  p-value: < 2.2e-16


