#Loading libraries

library(xtable)
library(languageR)

library(plotrix) 
library (lattice)
library (irr)
library(dplyr)
library(psych)
library(reshape2)
library(multilevel)



# this is a script in which I will invetsigate

# a) the ratings of the reading experiemtn of un- and in-prefixed words (Cambridge 15)


# In order to investigate the ratings, I need the df rating2

rating2<- read.csv("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/un_in/csv/ratings_all_participant_short.csv", sep=",",header = T,  na.string=c("na", "", "NA"))


str(rating2)
# 'data.frame':	5349 obs. of  19 variables:
#   $ Participant           : Factor w/ 31 levels "participant_10",..: 11 11 11 11 11 11 11 11 11 11 ...
# $ Age                   : int  28 28 28 28 28 28 28 28 28 28 ...
# $ Sex                   : Factor w/ 6 levels "female","Female",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ L1                    : Factor w/ 12 levels "british","British",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ Bilingual             : Factor w/ 8 levels "English and Indonesian",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Grow_Up_Region        : Factor w/ 28 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 13 13 13 13 13 13 13 13 13 13 ...
# $ Languages             : Factor w/ 21 levels "Basic French",..: 18 18 18 18 18 18 18 18 18 18 ...
# $ Latin                 : Factor w/ 15 levels "2 years secondary school",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ Profession_Studies    : Factor w/ 30 levels "2nd year Natural Sciences (Chemistry and materials)",..: 27 27 27 27 27 27 27 27 27 27 ...
# $ University            : Factor w/ 15 levels "anglia ruskin",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Knowledge_English_Ling: Factor w/ 14 levels "Currently in my 2nd year of the course at university",..: 13 13 13 13 13 13 13 13 13 13 ...
# $ Phonetics             : Factor w/ 13 levels "A couple of lectures",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ Phonology             : Factor w/ 12 levels "A couple of lectures",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ Morphology            : Factor w/ 11 levels "currently studying",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Semantics             : Factor w/ 11 levels "currently studying",..: 11 11 11 11 11 11 11 11 11 11 ...
# $ TimeRating            : num  583 583 583 583 583 ...
# $ TotalTime             : num  489 489 489 489 489 ...
# $ Item                  : Factor w/ 176 levels "imbibe","immaculate",..: 2 3 4 5 6 7 9 10 11 12 ...
# $ Rating                : int  4 1 1 1 2 1 4 2 4 1 ...

# I need to exclude the two participants whose L1 is not BE


table(rating2$Participant,rating2$L1)

# we exlcude 7 and 31


rating3<-rating2%>%filter(Participant!="participant_7")%>% filter(Participant!="participant_31")

rating3<-rating3[!duplicated(rating3),]

str(rating3)

# 'data.frame':	4947 obs. of  19 variables:
#   $ Participant           : Factor w/ 31 levels "participant_10",..: 11 11 11 11 11 11 11 11 11 11 ...
# $ Age                   : int  28 28 28 28 28 28 28 28 28 28 ...
# $ Sex                   : Factor w/ 6 levels "female","Female",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ L1                    : Factor w/ 12 levels "british","British",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ Bilingual             : Factor w/ 8 levels "English and Indonesian",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Grow_Up_Region        : Factor w/ 28 levels "3 years in Cambridge. 2 in Bristol. 3 in Felixstowe. 8 in Bradford. 2 in Abingdon",..: 13 13 13 13 13 13 13 13 13 13 ...
# $ Languages             : Factor w/ 21 levels "Basic French",..: 18 18 18 18 18 18 18 18 18 18 ...
# $ Latin                 : Factor w/ 15 levels "2 years secondary school",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ Profession_Studies    : Factor w/ 30 levels "2nd year Natural Sciences (Chemistry and materials)",..: 27 27 27 27 27 27 27 27 27 27 ...
# $ University            : Factor w/ 15 levels "anglia ruskin",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Knowledge_English_Ling: Factor w/ 14 levels "Currently in my 2nd year of the course at university",..: 13 13 13 13 13 13 13 13 13 13 ...
# $ Phonetics             : Factor w/ 13 levels "A couple of lectures",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ Phonology             : Factor w/ 12 levels "A couple of lectures",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ Morphology            : Factor w/ 11 levels "currently studying",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Semantics             : Factor w/ 11 levels "currently studying",..: 11 11 11 11 11 11 11 11 11 11 ...
# $ TimeRating            : num  583 583 583 583 583 ...
# $ TotalTime             : num  489 489 489 489 489 ...
# $ Item                  : Factor w/ 176 levels "imbibe","immaculate",..: 2 3 4 5 6 7 9 10 11 12 ...
# $ Rating                : int  4 1 1 1 2 1 4 2 4 1 ...


# let's have a look at the distribution



#Let's have a look at some plots

densityplot(rating3$Rating)

# thus it seems that a lot of items were classified as being very easy to segment, let's look
# at the items and their distribution

bwplot(~Rating|Item,rating3)


# a lot of items were rated to be
# very easily to decompose

# There are only a few which were rated as more difficult to decompose. The question is really whether
# the participants used the whole range and if not, why?

bwplot(~Rating|Participant,rating3)

# Participant 10, 11 and 29 did not use the full range, it seems like they did not understand the task correctly
# let's keep that in mind


# We need to code the data for whether they are simplex or complex words,
# so that we cann see whether the simplex words were rated as being simplex

# Therefore I will create a list with all types included in the experiment

setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Analyses/csv")
Experiment1 <- read.csv("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/un_in/csv/un_in_experimental_data_complete_df.csv", sep=",",header = T,  na.string=c("na", "", "NA"))


experimental_types<-unique(Experiment1$Item)

# now I make a list in which I save whether an item is simplex or complex

morph_status_list<-list()

for (i in rating3$Item) {
  if (i  %in% experimental_types) 
  {morph_status_list=append(morph_status_list, "complex")}
  else {morph_status_list = append(morph_status_list, "simplex")}
}


# Now I need a variable in which I code simplex or complex

rating3$MorphStatus<- as.factor(as.character(morph_status_list))

table(rating3$MorphStatus)
#complex simplex 
#      4076     871 




# How many simplex items? 

length(unique(rating3[rating3$MorphStatus=="simplex","Item"]))
# 34

(unique(rating3[rating3$MorphStatus=="simplex","Item"]))

# [1] ineligible   inexcusably  unexcellent  improve      important    improvise    immense     
# [8] imperial     impetus      Impala       impeachment  immune       imbibe       impunity    
# [15] imperative   impugned     union        united       university   under        uncle       
# [22] universe     until        unit         Unix         unto         unanimous    unless      
# [29] unique       unenjoyable  nexperienced intelligent  integral     integrity   

# there are a few problems, i.e. words which were
# misspelled in the rating: ineligible,unejoyable, inexcusably,nexperienced and unexcellent must be thrown out

rating5<-rating3[rating3$Item!="inexcusably"&rating3$Item!="unexcellent"&rating3$Item!="unenjoyable"
                 &rating3$Item!="ineligible"&rating3$Item!="nexperienced",]
rating5<-droplevels(rating5)

table(rating5$MorphStatus, rating5$Item)
# yay it worked!!!!!!


# imbibe immaculate immaterial immature immeasurable immediate immemorial immense immerse
# complex      0         29         29       29           29        29         29       0      29
# simplex     29          0          0        0            0         0          0      29       0
# 
# immigrant imminent immitigable immixture immobile immoderate immodest immoral immortal
# complex        29       29          29        29       29         29       29      29       29
# simplex         0        0           0         0        0          0        0       0        0
# 
# immotile immovable immune immutable Impala impalpable impanel imparity impartible impassible
# complex       29        29      0        29      0         29      29       29         29         29
# simplex        0         0     29         0     29          0       0        0          0          0
# 
# impassion impeachment imperative imperceivable imperfectible imperforable imperial
# complex        29           0          0            29            29           29        0
# simplex         0          29         29             0             0            0       29
# 
# imperishable impermissible imperturbable impetus implant implausible implicit implode
# complex           29            29            29       0      29          29       29      29
# simplex            0             0             0      29       0           0        0       0
# 
# impolitic imponderable import important impossible impotence impracticable impractical
# complex        29           29     29         0         29        29            29          29
# simplex         0            0      0        29          0         0             0           0
# 
# imprecise imprescriptible imprint imprison improve improvident improvise impugned impunity
# complex        29              29      29       29       0          29         0        0        0
# simplex         0               0       0        0      29           0        29       29       29
# 
# inact inappeasable inapplicable inapposite inappreciable inartistic inedible ineffaceable
# complex    28           28           28         28            28         28       28           28
# simplex     0            0            0          0             0          0        0            0
# 
# inefficacious inefficient inelastic inelegant ineliminable inelligble inessential inexact
# complex            28          28        28        29           28         28          28      28
# simplex             0           0         0         0            0          0           0       0
# 
# inexcusable inexistent inexpedient inexperienced inexplicit inexpressive inexpungible
# complex          28         29          28            26         29           29           28
# simplex           0          0           0             0          0            0            0
# 
# inextensible innervate innocuous innominate innumerable inobservable inoculate intake
# complex           28        29        29         29          29           28        28     28
# simplex            0         0         0          0           0            0         0      0
# 
# intangible integral integrity intelligent intemperate intemporal intenacity interior
# complex         28        0         0           0          28         28         28       28
# simplex          0       28        28          28           0          0          0        0
# 
# intestate into intolerant intoxicate intractable intransgressible intransient intransitive
# complex        28   28         28         28          28               28          28           28
# simplex         0    0          0          0           0                0           0            0
# 
# intrepid intrude intubate intumesce inturned inundate unable unacquainted unaided unaired
# complex       28      28       28        28       28       28     29           29      57      29
# simplex        0       0        0         0        0        0      0            0       0       0
# 
# unamplified unamused unanimous unarm unassumable unattested unauthentic unavailable uncle
# complex          29       29         0    29          29         29          29          29     0
# simplex           0        0        29     0           0          0           0           0    29
# 
# under unease uneaten unequal uneven unexampled unextended unindexed uninstall union unique
# complex     0     29      29      29     57         29         29        29        29     0      0
# simplex    29      0       0       0      0          0          0         0         0    29     29
# 
# unit united universe university Unix unless unnailed unnamed unnatural unnavigable
# complex    0      0        0          0    0      0       29      29        29          29
# simplex   29     29       29         29   29     29        0       0         0           0
# 
# unnecessary unneeded unnegotiable unneighbourly unnerve unnested unnetted unneutered
# complex          29       29           29            29      29       29       29         29
# simplex           0        0            0             0       0        0        0          0
# 
# unneutral unnoted unnoteworthy unnoticed unnourishing unnuanced unnumbered unnurtured
# complex        29      29           29        29           29        29         29         29
# simplex         0       0            0         0            0         0          0          0
# 
# unobserved unopened unoppressed unordered unoriginal unoxidized until unto
# complex         29       29          29        29         29         29     0    0
# simplex          0        0           0         0          0          0    29   29




table(rating5$Participant)

# participant_10 participant_11 participant_12 participant_13 participant_14 participant_15 participant_16 
# 171            171            171            171            171            171            171 
# participant_17 participant_18 participant_19  participant_2 participant_20 participant_21 participant_22 
# 171            171            172            126            171            171            171 
# participant_23 participant_24 participant_25 participant_26 participant_27 participant_28 participant_29 
# 171            171            171            171            171            172            171 
# participant_3 participant_30 participant_32  participant_4  participant_5  participant_6  participant_8 
# 170            171            171            170            171            171            171 
# participant_9 
# 171 




# someting is wring with 28 & 19 (they rated more items)


table(rating5[rating5$Participant=="participant_28","Item"])
# Now we can have a look at whether the simplex words were rated as being simplex

# He rated unaided twice -> we need to get rid of one

rating5[rating5$Participant=="participant_28"& rating5$"Item"=="unaided",]

# He gave two different rating, thus we will delete those

table(rating5[rating5$Participant=="participant_19","Item"])

rating5[rating5$Participant=="participant_19"& rating5$"Item"=="unaided",]

# same with 19


# so we need to throw iut these ratings

rating5$Id_info<-paste(rating5$Participant,"_",rating5$Item)

rating6<-rating5[rating5$Id_info!="participant_28 - unaided",]
rating6<-rating6[rating6$Id_info!="participant_19 - unaided",]


table(rating6$MorphStatus)
#complex simplex 
#4072     838 

table(rating6$Participant)
# participant_10 participant_11 participant_12 participant_13 participant_14 participant_15 participant_16 
# 171            171            171            171            171            171            171 
# participant_17 participant_18 participant_19  participant_2 participant_20 participant_21 participant_22 
# 171            171            170            126            171            171            171 
# participant_23 participant_24 participant_25 participant_26 participant_27 participant_28 participant_29 
# 171            171            171            171            171            170            171 
# participant_3 participant_30 participant_32  participant_4  participant_5  participant_6  participant_8 
# 170            171            171            170            171            171            171 
# participant_9 
# 171 




# How many simplex items? 

length(unique(rating5[rating5$MorphStatus=="simplex","Item"]))
# 29

(unique(rating5[rating5$MorphStatus=="simplex","Item"]))

# [1] improve     important   improvise   immense     imperial    impetus     Impala      impeachment
# [9] immune      imbibe      impunity    imperative  impugned    union       united      university 
# [17] under       uncle       universe    until       unit        Unix        unto        unanimous  
# [25] unless      unique      intelligent integral    integrity  



# Now we can have a look at whether the simplex words were rated as being simplex

# How many complex items? 

length(unique(rating5[rating5$MorphStatus=="complex","Item"]))
# 142

# 1 item weas not rated: 


# unejoyed (and unaided) my fault: typo)

(unique(rating5[rating5$MorphStatus=="simplex","Item"]))

# [1] improve     important   improvise   immense     imperial    impetus     Impala      impeachment
# [9] immune      imbibe      impunity    imperative  impugned    union       united      university 
# [17] under       uncle       universe    until       unit        Unix        unto        unanimous  
# [25] unless      unique      intelligent integral    integrity



table(rating5$MorphStatus, rating5$Item)

# imbibe immaculate immaterial immature immeasurable immediate immemorial immense immerse
# complex      0         29         29       29           29        29         29       0      29
# simplex     29          0          0        0            0         0          0      29       0
# 
# immigrant imminent immitigable immixture immobile immoderate immodest immoral immortal immotile
# complex        29       29          29        29       29         29       29      29       29       29
# simplex         0        0           0         0        0          0        0       0        0        0
# 
# immovable immune immutable Impala impalpable impanel imparity impartible impassible impassion
# complex        29      0        29      0         29      29       29         29         29        29
# simplex         0     29         0     29          0       0        0          0          0         0
# 
# impeachment imperative imperceivable imperfectible imperforable imperial imperishable
# complex           0          0            29            29           29        0           29
# simplex          29         29             0             0            0       29            0
# 
# impermissible imperturbable impetus implant implausible implicit implode impolitic imponderable
# complex            29            29       0      29          29       29      29        29           29
# simplex             0             0      29       0           0        0       0         0            0
# 
# import important impossible impotence impracticable impractical imprecise imprescriptible
# complex     29         0         29        29            29          29        29              29
# simplex      0        29          0         0             0           0         0               0
# 
# imprint imprison improve improvident improvise impugned impunity inact inappeasable
# complex      29       29       0          29         0        0        0    28           28
# simplex       0        0      29           0        29       29       29     0            0
# 
# inapplicable inapposite inappreciable inartistic inedible ineffaceable inefficacious
# complex           28         28            28         28       28           28            28
# simplex            0          0             0          0        0            0             0
# 
# inefficient inelastic inelegant ineliminable inelligble inessential inexact inexcusable
# complex          28        28        29           28         29          28      28          28
# simplex           0         0         0            0          0           0       0           0
# 
# inexistent inexpedient inexperienced inexplicit inexpressive inexpungible inextensible
# complex         29          28            28         29           29           28           28
# simplex          0           0             0          0            0            0            0
# 
# innervate innocuous innominate innumerable inobservable inoculate intake intangible integral
# complex        29        29         29          29           28        28     28         28        0
# simplex         0         0          0           0            0         0      0          0       28
# 
# integrity intelligent intemperate intemporal intenacity interior intestate into intolerant
# complex         0           0          28         28         28       28        28   28         28
# simplex        28          28           0          0          0        0         0    0          0
# 
# intoxicate intractable intransgressible intransient intransitive intrepid intrude intubate
# complex         28          28               28          28           28       28      28       28
# simplex          0           0                0           0            0        0       0        0
# 
# intumesce inturned inundate unable unacquainted unaided unaired unamplified unamused unanimous
# complex        28       28       28     29           29      27      29          29       29         0
# simplex         0        0        0      0            0       0       0           0        0        29
# 
# unarm unassumable unattested unauthentic unavailable uncle under unease uneaten unequal uneven
# complex    29          29         29          29          29     0     0     29      29      29     29
# simplex     0           0          0           0           0    29    29      0       0       0      0
# 
# unexampled unextended unindexed uninstall union unique unit united universe university Unix
# complex         29         29        29        29     0      0    0      0        0          0    0
# simplex          0          0         0         0    29     29   29     29       29         29   29
# 
# unless unnailed unnamed unnatural unnavigable unnecessary unneeded unnegotiable unneighbourly
# complex      0       29      29        29          29          29       29           29            29
# simplex     29        0       0         0           0           0        0            0             0
# 
# unnerve unnested unnetted unneutered unneutral unnoted unnoteworthy unnoticed unnourishing
# complex      29       29       29         29        29      29           29        29           29
# simplex       0        0        0          0         0       0            0         0            0
# 
# unnuanced unnumbered unnurtured unobserved unopened unoppressed unordered unoriginal unoxidized
# complex        29         29         29         29       29          29        29         29         29
# simplex         0          0          0          0        0           0         0          0          0
# 
# until unto
# complex     0    0
# simplex    29   29

table(rating5$Participant)

# participant_10 participant_11 participant_12 participant_13 participant_14 participant_15 participant_16 
# 171            171            171            171            171            171            171 
# participant_17 participant_18 participant_19  participant_2 participant_20 participant_21 participant_22 
# 171            171            172            126            171            171            171 
# participant_23 participant_24 participant_25 participant_26 participant_27 participant_28 participant_29 
# 171            171            171            171            171            172            171 
# participant_3 participant_30 participant_32  participant_4  participant_5  participant_6  participant_8 
# 170            171            171            170            171            171            171 
# participant_9 
# 171 


# now we have to exclude the items which were rated to be unknown

rating7<-rating6[!is.na(rating6$Rating),]
4910-4636

# Exclusion of 274 items

table(rating7$MorphStatus)
# complex simplex 
#  3857     779 

# now we can have a look at the data


bwplot(~Rating|MorphStatus,rating7)

# all in alml it seems that they made the right choice and understood the task. Let's
# however look at each participant

bwplot(Rating~MorphStatus|Participant,rating7)

# 1 participant did not make a distinction, participant 25

# Furthermore it looks like most participants made a categorical
# distinction between simplex and complex words, and thus did rated all words
# which they thought would be complex with only one value

# Let's have a look at the item-whole corr and the ICC

# in order to have a look at these measurement, 
#we need our table in another format

rating7<-droplevels(rating7)

list_items<-levels(rating7$Item)
length(list_items)
# 171


list_participants <- levels(rating7$Participant)
length(list_participants)
# 29

171*29

# 4959

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(rating7[rating7$Participant==participant & rating7$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(rating7[rating7$Participant==participant & rating7$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating <- matrix(tmp_list,ncol=29,byrow=TRUE)
colnames(new_rating) <- list_participants
rownames(new_rating) <- list_items

colnames(new_rating)
rownames(new_rating)


new_rating<-as.data.frame(new_rating)

dim(new_rating)
#[1] 171  29

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating, missing = FALSE)

# Call: ICC(x = new_rating, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type   ICC   F df1  df2 p lower bound upper bound
# Single_raters_absolute   ICC1 0.051 2.5 170 4788 0       0.036        0.07
# Single_random_raters     ICC2 0.050 2.5 170 4760 0       0.035        0.07
# Single_fixed_raters      ICC3 0.050 2.5 170 4760 0       0.035        0.07
# Average_raters_absolute ICC1k 0.607 2.5 170 4788 0       0.517        0.69
# Average_random_raters   ICC2k 0.606 2.5 170 4760 0       0.516        0.69
# Average_fixed_raters    ICC3k 0.605 2.5 170 4760 0       0.515        0.69
# 
# Number of subjects = 171     Number of Judges =  29

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating)

#     Variable   Item.Total Alpha.Without  N
# 1  participant_10 -0.007282809     0.6712309 66
# 2  participant_11  0.071220201     0.6658791 66
# 3  participant_12  0.183348167     0.6563326 66
# 4  participant_13  0.042557008     0.6688046 66
# 5  participant_14  0.113762741     0.6617960 66
# 6  participant_15  0.124213943     0.6608327 66
# 7  participant_16  0.255649802     0.6501414 66
# 8  participant_17  0.172579939     0.6573589 66
# 9  participant_18  0.054193576     0.6679790 66
# 10 participant_19  0.397568073     0.6368790 66
# 11  participant_2  0.040699183     0.6683815 66
# 12 participant_20  0.242878787     0.6509716 66
# 13 participant_21  0.267144850     0.6492668 66
# 14 participant_22  0.255341979     0.6503155 66
# 15 participant_23  0.392330353     0.6363772 66
# 16 participant_24  0.237985222     0.6514427 66
# 17 participant_25  0.180994031     0.6566973 66
# 18 participant_26  0.244518370     0.6508832 66
# 19 participant_27  0.259848582     0.6496352 66
# 20 participant_28  0.414289168     0.6358287 66
# 21 participant_29  0.157996465     0.6581183 66
# 22  participant_3  0.195131295     0.6554241 66
# 23 participant_30  0.189521727     0.6557372 66
# 24 participant_32  0.383049910     0.6394912 66
# 25  participant_4  0.103278866     0.6634659 66
# 26  participant_5  0.144080285     0.6593885 66
# 27  participant_6  0.126841076     0.6609535 66
# 28  participant_8  0.376483068     0.6397577 66
# 29  participant_9  0.317141736     0.6446784 66

# the numbers are pretty low, which could be due to the fact that
# a lot of raters basically only used 2 steps: simplex or complex.
# Which numbers were used as simplex and which were used as complex
# deviated however. I think this time, the item-total corr.
# might not be a valid measuremtn (due to the categorical distinction
# made)

# However, all in all, it seems like the rating can be seen as a valid
# categorization of complex vs. simplex words. It does however not seem
# to capture gradient differences in the decomposability of complex
# words.



my.item.total <- function (items, method="pearson") {
  items <- na.exclude(items)
  N <- ncol(items)
  ans <- matrix(0, N, 3)
  ans[, 1] <- labels(items)[[2]]
  for (i in 1:N) {
    ans[i, 2] <- cor(items[, i], apply(items[, -i], 1, mean),
                     method=method)
    ans[i, 3] <- cronbach(items[, -i])[[1]] 
    
  } 
  
  OUT <- data.frame(Variable = ans[, 1], Item.Total = as.numeric(ans[,
                                                                     2]), Alpha.Without = as.numeric(ans[, 3]), N = nrow(items)) 
  
  return(OUT) 
  
}

my.item.total(new_rating,method = "spearman")


# auch nicht besser...

# Let's exclude invalid ratings now



# We should consider those participants for which we see a sign. differencernece between simplex and 
# complex as valid


kruskal.test(Rating~MorphStatus,rating7)
# Kruskal-Wallis rank sum test

# data:  Rating by MorphStatus
# Kruskal-Wallis chi-squared = 1298.4, df = 1, p-value < 2.2e-16

# so in general there is a significant difference. Niw let's trake a look
# t each participant individually

Participant_list=levels(unique(rating7$Participant))

for (i in Participant_list){
  #print(as.factor(i))
  part_dataset=rating7[rating7$Participant==i,]
  print(i)
  print(kruskal.test(part_dataset$Rating~part_dataset$MorphStatus))
  
  #kruskal.test(Rating~MorphStatus,part_dataset)
} 
# 
# [1] "participant_10"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 67.649, df = 1, p-value < 2.2e-16
# 
# [1] "participant_11"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 45.53, df = 1, p-value = 1.503e-11
# 
# [1] "participant_12"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 52.251, df = 1, p-value = 4.884e-13
# 
# [1] "participant_13"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 60.35, df = 1, p-value = 7.94e-15
# 
# [1] "participant_14"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 45.571, df = 1, p-value = 1.472e-11
# 
# [1] "participant_15"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 77.718, df = 1, p-value < 2.2e-16
# 
# [1] "participant_16"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 65.83, df = 1, p-value = 4.914e-16
# 
# [1] "participant_17"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 62.588, df = 1, p-value = 2.548e-15
# 
# [1] "participant_18"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 33.884, df = 1, p-value = 5.851e-09
# 
# [1] "participant_19"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 3.5619, df = 1, p-value = 0.05912
# 
# [1] "participant_2"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 45.663, df = 1, p-value = 1.405e-11
# 
# [1] "participant_20"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 55.229, df = 1, p-value = 1.073e-13
# 
# [1] "participant_21"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 15.138, df = 1, p-value = 9.992e-05
# 
# [1] "participant_22"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 43.04, df = 1, p-value = 5.362e-11
# 
# [1] "participant_23"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 65.307, df = 1, p-value = 6.408e-16
# 
# [1] "participant_24"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 51.33, df = 1, p-value = 7.806e-13
# 
# [1] "participant_25"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 3.967, df = 1, p-value = 0.0464
# 
# [1] "participant_26"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 65.153, df = 1, p-value = 6.932e-16
# 
# [1] "participant_27"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 43.309, df = 1, p-value = 4.674e-11
# 
# [1] "participant_28"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 37.826, df = 1, p-value = 7.736e-10
# 
# [1] "participant_29"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 65.365, df = 1, p-value = 6.225e-16
# 
# [1] "participant_3"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 59.352, df = 1, p-value = 1.319e-14
# 
# [1] "participant_30"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 60.638, df = 1, p-value = 6.86e-15
# 
# [1] "participant_32"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 55.053, df = 1, p-value = 1.174e-13
# 
# [1] "participant_4"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 68.358, df = 1, p-value < 2.2e-16
# 
# [1] "participant_5"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 55.1, df = 1, p-value = 1.145e-13
# 
# [1] "participant_6"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 66.556, df = 1, p-value = 3.401e-16
# 
# [1] "participant_8"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 38.837, df = 1, p-value = 4.607e-10
# 
# [1] "participant_9"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 61.536, df = 1, p-value = 4.347e-15


# exclude 19 since he does not differentiate between simplex and complex

rating8<-rating7%>%filter(Participant!="participant_19")


# Let's have a look at the item-whole corr and the ICC again

# in order to have a look at these measurement, 
#we need our table in another format

rating8<-droplevels(rating8)

list_items<-levels(rating8$Item)
length(list_items)
# 171


list_participants <- levels(rating8$Participant)
length(list_participants)
# 28

171*28

# 4788

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(rating8[rating8$Participant==participant & rating8$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(rating8[rating8$Participant==participant & rating8$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating <- matrix(tmp_list,ncol=28,byrow=TRUE)
colnames(new_rating) <- list_participants
rownames(new_rating) <- list_items

colnames(new_rating)
rownames(new_rating)


new_rating<-as.data.frame(new_rating)

dim(new_rating)
#[1] 171  28

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating, missing = FALSE)

# Call: ICC(x = new_rating, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type   ICC   F df1  df2       p lower bound upper bound
# Single_raters_absolute   ICC1 0.042 2.2 170 4617 0.0e+00       0.028        0.06
# Single_random_raters     ICC2 0.042 2.2 170 4590 1.1e-16       0.028        0.06
# Single_fixed_raters      ICC3 0.042 2.2 170 4590 1.1e-16       0.028        0.06
# Average_raters_absolute ICC1k 0.550 2.2 170 4617 0.0e+00       0.447        0.64
# Average_random_raters   ICC2k 0.549 2.2 170 4590 1.1e-16       0.446        0.64
# Average_fixed_raters    ICC3k 0.549 2.2 170 4590 1.1e-16       0.445        0.64
# 
# Number of subjects = 171     Number of Judges =  28

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating)

#           Variable   Item.Total Alpha.Without  N
# 1  participant_10  0.118392051     0.5106761 66
# 2  participant_11 -0.005878161     0.5267813 66
# 3  participant_12  0.245227474     0.4933662 66
# 4  participant_13  0.092411934     0.5143298 66
# 5  participant_14  0.269478041     0.4880504 66
# 6  participant_15  0.164162444     0.5040120 66
# 7  participant_16  0.138265517     0.5078627 66
# 8  participant_17  0.179568818     0.5030043 66
# 9  participant_18  0.267728566     0.4882447 66
# 10  participant_2  0.103536279     0.5125696 66
# 11 participant_20  0.041523542     0.5215869 66
# 12 participant_21 -0.057064232     0.5359188 66
# 13 participant_22  0.186402553     0.5004983 66
# 14 participant_23  0.129174156     0.5090208 66
# 15 participant_24  0.146419064     0.5065071 66
# 16 participant_25  0.219139241     0.4946495 66
# 17 participant_26  0.268124905     0.4879864 66
# 18 participant_27  0.102223701     0.5129838 66
# 19 participant_28  0.306959860     0.4837637 66
# 20 participant_29  0.325133210     0.4797830 66
# 21  participant_3  0.258304947     0.4885906 66
# 22 participant_30  0.118163862     0.5105255 66
# 23 participant_32  0.216469272     0.4962806 66
# 24  participant_4  0.089880269     0.5145473 66
# 25  participant_5  0.047798471     0.5205620 66
# 26  participant_6 -0.030820868     0.5289866 66
# 27  participant_8 -0.005022906     0.5275758 66
# 28  participant_9 -0.089782171     0.5397844 66


ItemTotalDF<-as.data.frame(item.total(new_rating))

head(ItemTotalDF)

#          Variable   Item.Total Alpha.Without  N
# 1 participant_10  0.118392051     0.5106761 66
# 2 participant_11 -0.005878161     0.5267813 66
# 3 participant_12  0.245227474     0.4933662 66
# 4 participant_13  0.092411934     0.5143298 66
# 5 participant_14  0.269478041     0.4880504 66
# 6 participant_15  0.164162444     0.5040120 66


summary(ItemTotalDF$Item.Total)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.08978  0.07936  0.13370  0.13740  0.22570  0.32510 


alpha(new_rating)
# Call: alpha(x = new_rating)
# 
# raw_alpha std.alpha G6(smc) average_r S/N  ase mean   sd
# 0.57      0.57    0.72     0.045 1.3 0.05  1.8 0.34
# 
# lower alpha upper     95% confidence boundaries
# 0.47 0.57 0.67 

# the numbers are pretty low, which could be due to the fact that
# a lot of raters basically only used 2 steps: simplex or complex.
# Which numbers were used as simplex and which were used as complex
# deviated however. I think this time, the item-total corr.
# might not be a valid measuremtn (due to the categorical distinction
# made)

# However, all in all, it seems like the rating can be seen as a valid
# categorization of complex vs. simplex words. It does however not seem
# to capture gradient differences in the decomposability of complex
# words.

#############################
# maybe we need the median
#
# Let's create it

v1<-levels(rating8$Item)


median<-tapply(rating8$Rating,rating8$Item,median)

median<-as.data.frame(median)

class(median)
#[1] "data.frame"

str(median)
# 'data.frame':	171 obs. of  1 variable:
#   $ median: num [1:171(1d)] 4 3 1 1 1 3 1 4 4 2 ...
# ..- attr(*, "dimnames")=List of 1
# .. ..$ : chr  "imbibe" "immaculate" "immaterial" "immature" ...

rating_Exp1<-  merge(Experiment1, median, by.x="Item", by.y="row.names")

median$Item<-as.factor(row.names(median))


class(median$median)
#[1] "array"

median$median<-as.integer(median$median)
class(median$median)
#[1] "integer"

str(median)
bwplot(~median|Item,median)

rownames(median) <- c()


#yayy that looks good. Let's write that in a csv

write.csv(median, file="Experiment_1_median.csv", sep=",")

###################
# Let's look at the ratings for each affix, i.e. at the reliability for each affix

#############un

un<-rating_Exp1[rating_Exp1$Affix=="un",]

un<-droplevels(un)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(un$Item)
length(list_items)
# 45


list_participants <- levels(un$Participant)
length(list_participants)
# 29

45*29

# 1305

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(un[un$Participant==participant & un$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(un[un$Participant==participant & un$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating_un <- matrix(tmp_list,ncol=29,byrow=TRUE)
colnames(new_rating_un) <- list_participants
rownames(new_rating_un) <- list_items

colnames(new_rating_un)
rownames(new_rating_un)


new_rating_un<-as.data.frame(new_rating_un)

dim(new_rating_un)
#[1] 45 29

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_un, missing = FALSE)

# Call: ICC(x = new_rating_un, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type  ICC F df1  df2 p lower bound upper bound
# Single_raters_absolute   ICC1 0.19 8  44 1260 0        0.13        0.29
# Single_random_raters     ICC2 0.19 8  44 1232 0        0.13        0.29
# Single_fixed_raters      ICC3 0.19 8  44 1232 0        0.13        0.29
# Average_raters_absolute ICC1k 0.87 8  44 1260 0        0.82        0.92
# Average_random_raters   ICC2k 0.87 8  44 1232 0        0.82        0.92
# Average_fixed_raters    ICC3k 0.87 8  44 1232 0        0.82        0.92
# 
# Number of subjects = 45     Number of Judges =  29

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_un)

#Variable  Item.Total Alpha.Without N
# 1  Experiment_1_participant_10  0.04602873     0.4042687 9
# 2  Experiment_1_participant_11          NA     0.3908832 9
# 3  Experiment_1_participant_12          NA     0.3908832 9
# 4  Experiment_1_participant_13  0.48564293     0.2837177 9
# 5  Experiment_1_participant_14  0.48564293     0.2837177 9
# 6  Experiment_1_participant_15          NA     0.3908832 9
# 7  Experiment_1_participant_16          NA     0.3908832 9
# 8  Experiment_1_participant_17  0.48564293     0.2837177 9
# 9  Experiment_1_participant_18          NA     0.3908832 9
# 10 Experiment_1_participant_19          NA     0.3908832 9
# 11  Experiment_1_participant_2          NA     0.3908832 9
# 12 Experiment_1_participant_20 -0.33567254     0.4747001 9
# 13 Experiment_1_participant_21          NA     0.3908832 9
# 14 Experiment_1_participant_22          NA     0.3908832 9
# 15 Experiment_1_participant_23 -0.15174424     0.4366472 9
# 16 Experiment_1_participant_24  0.48564293     0.2837177 9
# 17 Experiment_1_participant_25          NA     0.3908832 9
# 18 Experiment_1_participant_26 -0.33567254     0.4747001 9
# 19 Experiment_1_participant_27 -0.15174424     0.4366472 9
# 20 Experiment_1_participant_28 -0.15174424     0.4366472 9
# 21 Experiment_1_participant_29          NA     0.3908832 9
# 22  Experiment_1_participant_3          NA     0.3908832 9
# 23 Experiment_1_participant_30          NA     0.3908832 9
# 24 Experiment_1_participant_32          NA     0.3908832 9
# 25  Experiment_1_participant_4          NA     0.3908832 9
# 26  Experiment_1_participant_5          NA     0.3908832 9
# 27  Experiment_1_participant_6  0.48564293     0.2837177 9
# 28  Experiment_1_participant_8  0.31622777     0.2880658 9
# 29  Experiment_1_participant_9  0.31622777     0.2880658 9


# still bad for un


#############Loc

Loc<-rating_Exp1[rating_Exp1$Affix=="Loc",]

Loc<-droplevels(Loc)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(Loc$Item)
length(list_items)
# 24


list_participants <- levels(Loc$Participant)
length(list_participants)
# 29

24*29

# 696

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(Loc[Loc$Participant==participant & Loc$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(Loc[Loc$Participant==participant & Loc$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating_Loc <- matrix(tmp_list,ncol=29,byrow=TRUE)
colnames(new_rating_Loc) <- list_participants
rownames(new_rating_Loc) <- list_items

colnames(new_rating_Loc)
rownames(new_rating_Loc)


new_rating_Loc<-as.data.frame(new_rating_Loc)

dim(new_rating_Loc)
#[1] 24 29

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_Loc, missing = FALSE)

# Call: ICC(x = new_rating_Loc, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type   ICC   F df1 df2       p lower bound upper bound
# Single_raters_absolute   ICC1 0.083 3.6  23 672 3.2e-08       0.039        0.18
# Single_random_raters     ICC2 0.082 3.5  23 644 8.1e-08       0.037        0.17
# Single_fixed_raters      ICC3 0.080 3.5  23 644 8.1e-08       0.036        0.17
# Average_raters_absolute ICC1k 0.725 3.6  23 672 3.2e-08       0.539        0.86
# Average_random_raters   ICC2k 0.721 3.5  23 644 8.1e-08       0.530        0.86
# Average_fixed_raters    ICC3k 0.716 3.5  23 644 8.1e-08       0.523        0.86
# 
# Number of subjects = 24     Number of Judges =  29
 
# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_Loc)
# not even applicable



#############Neg

Neg<-rating_Exp1[rating_Exp1$Affix=="Neg",]

Neg<-droplevels(Neg)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(Neg$Item)
length(list_items)
# 73


list_participants <- levels(Neg$Participant)
length(list_participants)
# 29

73*29

# 2117

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(Neg[Neg$Participant==participant & Neg$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(Neg[Neg$Participant==participant & Neg$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating_Neg <- matrix(tmp_list,ncol=29,byrow=TRUE)
colnames(new_rating_Neg) <- list_participants
rownames(new_rating_Neg) <- list_items

colnames(new_rating_Neg)
rownames(new_rating_Neg)


new_rating_Neg<-as.data.frame(new_rating_Neg)

dim(new_rating_Neg)
#[1] 73 29

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_Neg, missing = FALSE)

# Call: ICC(x = new_rating_Neg, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type   ICC   F df1 df2       p lower bound upper bound
# Single_raters_absolute   ICC1 0.083 3.6  23 672 3.2e-08       0.039        0.18
# Single_random_raters     ICC2 0.082 3.5  23 644 8.1e-08       0.037        0.17
# Single_fixed_raters      ICC3 0.080 3.5  23 644 8.1e-08       0.036        0.17
# Average_raters_absolute ICC1k 0.725 3.6  23 672 3.2e-08       0.539        0.86
# Average_random_raters   ICC2k 0.721 3.5  23 644 8.1e-08       0.530        0.86
# Average_fixed_raters    ICC3k 0.716 3.5  23 644 8.1e-08       0.523        0.86
# 
# Number of subjects = 24     Number of Judges =  29

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_Neg)
# horrible