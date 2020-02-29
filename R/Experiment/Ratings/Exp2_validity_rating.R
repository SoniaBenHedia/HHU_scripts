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

rating2<- read.csv("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Analyses/csv/ratings/ratings_all_participants_dis_ly10_08_2017.csv", sep=",",header = T,  na.string=c("na", "", "NA"))

str(rating2)
# 'data.frame':	4550 obs. of  25 variables:
#   $ ID                    : int  16 16 16 16 16 16 16 16 16 16 ...
# $ Date.submitted        : Factor w/ 25 levels "05.10.16 12:14",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Last.page             : int  2 2 2 2 2 2 2 2 2 2 ...
# $ Start.Language        : Factor w/ 1 level "en": 1 1 1 1 1 1 1 1 1 1 ...
# $ Date.started          : Factor w/ 25 levels "05.10.16 12:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Date.last.action      : Factor w/ 25 levels "05.10.16 12:14",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant           : Factor w/ 25 levels "participant_10A",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ Age                   : int  25 25 25 25 25 25 25 25 25 25 ...
# $ Sex                   : Factor w/ 2 levels "female","male": 1 1 1 1 1 1 1 1 1 1 ...
# $ L1                    : Factor w/ 2 levels "British English",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Bilingual             : Factor w/ 2 levels "Italian","no": 2 2 2 2 2 2 2 2 2 2 ...
# $ Grow_Up_Region        : Factor w/ 24 levels "Aberdeen. Scotland",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ Languages             : Factor w/ 16 levels "Basic French",..: 13 13 13 13 13 13 13 13 13 13 ...
# $ Latin                 : Factor w/ 7 levels "I know a few words and phrases. I have never studied it",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Profession_Studies    : Factor w/ 25 levels "2nd Year Meida Studies",..: 18 18 18 18 18 18 18 18 18 18 ...
# $ University            : Factor w/ 9 levels "Aberdeen University",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Knowledge_English_Ling: Factor w/ 10 levels "2 years","no",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Phonetics             : Factor w/ 6 levels "I went to a few lectures on phonetics in my first year of university.",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Phonology             : Factor w/ 5 levels "no","The above lecture also covered phonology.",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Morphology            : Factor w/ 4 levels "no","Year 1 ARU",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Semantics             : Factor w/ 4 levels "no","Year 1 ARU",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Item                  : Factor w/ 182 levels "aerobically",..: 42 36 46 47 48 49 52 58 61 25 ...
# $ Rating                : int  2 1 3 2 3 3 3 3 3 4 ...
# $ TimeRating            : num  698 698 698 698 698 ...
# $ TotalTime             : num  821 821 821 821 821 ...

# I need to exclude the two participants who are bilingual


table(rating2$Participant,rating2$Bilingual)

# we exlcude 11B, 7A and 7B


rating3<-rating2%>%filter(Participant!="participant_7A")%>% filter(Participant!="participant_7B")%>% filter(Participant!="participant_11B")

rating3<-rating3[!duplicated(rating3),]

rating3<-droplevels(rating3)
str(rating3)

# 'data.frame':	4004 obs. of  25 variables:
#   $ ID                    : int  16 16 16 16 16 16 16 16 16 16 ...
# $ Date.submitted        : Factor w/ 22 levels "05.10.16 12:14",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Last.page             : int  2 2 2 2 2 2 2 2 2 2 ...
# $ Start.Language        : Factor w/ 1 level "en": 1 1 1 1 1 1 1 1 1 1 ...
# $ Date.started          : Factor w/ 22 levels "05.10.16 12:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Date.last.action      : Factor w/ 22 levels "05.10.16 12:14",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Participant           : Factor w/ 22 levels "participant_10A",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ Age                   : int  25 25 25 25 25 25 25 25 25 25 ...
# $ Sex                   : Factor w/ 2 levels "female","male": 1 1 1 1 1 1 1 1 1 1 ...
# $ L1                    : Factor w/ 2 levels "British English",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Bilingual             : Factor w/ 1 level "no": 1 1 1 1 1 1 1 1 1 1 ...
# $ Grow_Up_Region        : Factor w/ 21 levels "Aberdeen. Scotland",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ Languages             : Factor w/ 15 levels "Basic French",..: 12 12 12 12 12 12 12 12 12 12 ...
# $ Latin                 : Factor w/ 7 levels "I know a few words and phrases. I have never studied it",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Profession_Studies    : Factor w/ 22 levels "2nd Year Meida Studies",..: 16 16 16 16 16 16 16 16 16 16 ...
# $ University            : Factor w/ 9 levels "Aberdeen University",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Knowledge_English_Ling: Factor w/ 10 levels "2 years","no",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Phonetics             : Factor w/ 6 levels "I went to a few lectures on phonetics in my first year of university.",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Phonology             : Factor w/ 5 levels "no","The above lecture also covered phonology.",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Morphology            : Factor w/ 4 levels "no","Year 1 ARU",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Semantics             : Factor w/ 4 levels "no","Year 1 ARU",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Item                  : Factor w/ 182 levels "aerobically",..: 42 36 46 47 48 49 52 58 61 25 ...
# $ Rating                : int  2 1 3 2 3 3 3 3 3 4 ...
# $ TimeRating            : num  698 698 698 698 698 ...
# $ TotalTime             : num  821 821 821 821 821 ...

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

# Participant 5 a and 6b did not seem to have understood teh task


# We need to code the data for whether they are simplex or complex words,
# so that we cann see whether the simplex words were rated as being simplex


setwd("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Analyses/csv")


Experiment2 <- read.csv("complete_df_experiment_2.csv", sep=",",header = T,  na.string=c("na", "", "NA"))


experimental_types<-unique(Experiment2$Item)

experimental_types_simplex=as.factor( c(
"dissertation", 
"dissident", 
"dissipate ",
"dissolute ",
"dissonance",  
"twilly",
"trolly",
"swilly",
"skelly",
"lolly",
"belly",
"jelly",
"dilly",
"silly",
"bully",
"lilly"))


# now I make a list in which I save whether an item is simplex or complex

morph_status_list<-list()

for (i in rating3$Item) {
  if (i  %in% experimental_types_simplex) 
  {morph_status_list=append(morph_status_list, "simplex")}
  else if (i  %in% experimental_types) 
  {morph_status_list=append(morph_status_list, "complex")}
  else {morph_status_list = append(morph_status_list, "simplex")}
}


# Now I need a variable in which I code simplex or complex

rating3$MorphStatus<- as.factor(as.character(morph_status_list))

table(rating3$MorphStatus)
# complex simplex 
#    3322     682 





# How many simplex items? 

length(unique(rating3[rating3$MorphStatus=="simplex","Item"]))
# 31

(unique(rating3[rating3$MorphStatus=="simplex","Item"]))

# [1] dissertation dissident    dissonance   twilly       trolly       swilly       skelly      
# [8] lolly        belly        jelly        dilly        silly        bully        lilly       
# [15] district     distance     disturb      family       union        united       university  
# [22] under        uncle        universe     until        unit         Unix         unto        
# [29] unanimous    unless       unique     



table(rating3$MorphStatus, rating3$Item)
#yes, 22 ratings for each

table(rating3$Participant)

# participant_10A participant_10B participant_11A participant_12A participant_12B participant_13A 
# 182             182             182             182             182             182 
# participant_1A  participant_1B  participant_2A  participant_2B  participant_3A  participant_3B 
# 182             182             182             182             182             182 
# participant_4A  participant_4B  participant_5A  participant_5B  participant_6A  participant_6B 
# 182             182             182             182             182             182 
# participant_8A  participant_8B  participant_9A  participant_9B 
# 182             182             182             182 


# all looks good


# now we have to exclude the items which were rated to be unknown

rating7<-rating3[rating3$Rating!=6,]
4004-3932

# Exclusion of 72 items

table(rating7$MorphStatus)
# complex simplex 
#    3293     639 


# now we can have a look at the data


bwplot(~Rating|MorphStatus,rating7)

# all in alml it seems that they made the right choice and understood the task. Let's
# however look at each participant

bwplot(Rating~MorphStatus|Participant,rating7)

#looks good


# Let's exclude invalid ratings now



# We should consider those participants for which we see a sign. differencernece between simplex and 
# complex as valid


kruskal.test(Rating~MorphStatus,rating7)
# Kruskal-Wallis rank sum test
# 
# data:  Rating by MorphStatus
# Kruskal-Wallis chi-squared = 1447.4, df = 1, p-value < 2.2e-16

# so in general there is a significant difference. Niw let's trake a look
# t each participant individually

Participant_list=levels(unique(rating7$Participant))

for (i in Participant_list){
  #print(as.factor(i))
  part_dataset=rating7[rating7$Participant==i,]
  print(i)
  print(kruskal.test(part_dataset$Rating~part_dataset$MorphStatus))
  
}

# [1] "participant_10A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 72.38, df = 1, p-value < 2.2e-16
# 
# [1] "participant_10B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 90.745, df = 1, p-value < 2.2e-16
# 
# [1] "participant_11A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 46.833, df = 1, p-value = 7.728e-12
# 
# [1] "participant_12A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 86.691, df = 1, p-value < 2.2e-16
# 
# [1] "participant_12B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 48.613, df = 1, p-value = 3.117e-12
# 
# [1] "participant_13A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 96.505, df = 1, p-value < 2.2e-16
# 
# [1] "participant_1A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 78.965, df = 1, p-value < 2.2e-16
# 
# [1] "participant_1B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 81.691, df = 1, p-value < 2.2e-16
# 
# [1] "participant_2A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 53.29, df = 1, p-value = 2.878e-13
# 
# [1] "participant_2B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 52.596, df = 1, p-value = 4.098e-13
# 
# [1] "participant_3A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 60.143, df = 1, p-value = 8.82e-15
# 
# [1] "participant_3B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 65.633, df = 1, p-value = 5.431e-16
# 
# [1] "participant_4A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 51.323, df = 1, p-value = 7.836e-13
# 
# [1] "participant_4B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 73.147, df = 1, p-value < 2.2e-16
# 
# [1] "participant_5A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 70.17, df = 1, p-value < 2.2e-16
# 
# [1] "participant_5B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 105.11, df = 1, p-value < 2.2e-16
# 
# [1] "participant_6A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 92.214, df = 1, p-value < 2.2e-16
# 
# [1] "participant_6B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 107.68, df = 1, p-value < 2.2e-16
# 
# [1] "participant_8A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 88.959, df = 1, p-value < 2.2e-16
# 
# [1] "participant_8B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 84.671, df = 1, p-value < 2.2e-16
# 
# [1] "participant_9A"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 64.32, df = 1, p-value = 1.058e-15
# 
# [1] "participant_9B"
# 
# Kruskal-Wallis rank sum test
# 
# data:  part_dataset$Rating by part_dataset$MorphStatus
# Kruskal-Wallis chi-squared = 69.089, df = 1, p-value < 2.2e-16


# ALL GOOD


#########################################################
# Let's have a look at the item-whole corr and the ICC

# in order to have a look at these measurement, 
#we need our table in another format

rating7<-droplevels(rating7)

list_items<-levels(rating7$Item)
length(list_items)
# 182


list_participants <- levels(rating7$Participant)
length(list_participants)
# 22

182*22

# 4004

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


new_rating <- matrix(tmp_list,ncol=22,byrow=TRUE)
colnames(new_rating) <- list_participants
rownames(new_rating) <- list_items

colnames(new_rating)
rownames(new_rating)


new_rating<-as.data.frame(new_rating)

dim(new_rating)
#[1] [1] 182  22


# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating, missing = FALSE)

# Call: ICC(x = new_rating, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type  ICC   F df1  df2 p lower bound upper bound
# Single_raters_absolute   ICC1 0.20 6.5 181 3822 0        0.16        0.24
# Single_random_raters     ICC2 0.20 6.5 181 3801 0        0.16        0.24
# Single_fixed_raters      ICC3 0.20 6.5 181 3801 0        0.16        0.25
# Average_raters_absolute ICC1k 0.85 6.5 181 3822 0        0.81        0.88
# Average_random_raters   ICC2k 0.85 6.5 181 3801 0        0.81        0.88
# Average_fixed_raters    ICC3k 0.85 6.5 181 3801 0        0.81        0.88
# 
# Number of subjects = 182     Number of Judges =  22

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating)

#           Variable Item.Total Alpha.Without   N
# 1  participant_10A  0.3791230     0.8487088 140
# 2  participant_10B  0.3949450     0.8480997 140
# 3  participant_11A  0.4195672     0.8472296 140
# 4  participant_12A  0.4946354     0.8444463 140
# 5  participant_12B  0.4312707     0.8467871 140
# 6  participant_13A  0.4687166     0.8453060 140
# 7   participant_1A  0.4502300     0.8459982 140
# 8   participant_1B  0.5057474     0.8441467 140
# 9   participant_2A  0.5215399     0.8431915 140
# 10  participant_2B  0.4052037     0.8477430 140
# 11  participant_3A  0.4592055     0.8456864 140
# 12  participant_3B  0.4352937     0.8466138 140
# 13  participant_4A  0.4151651     0.8473746 140
# 14  participant_4B  0.3559823     0.8493826 140
# 15  participant_5A  0.4504710     0.8460345 140
# 16  participant_5B  0.4516201     0.8460852 140
# 17  participant_6A  0.4287129     0.8468802 140
# 18  participant_6B  0.4157741     0.8473721 140
# 19  participant_8A  0.3586845     0.8495769 140
# 20  participant_8B  0.3757574     0.8488470 140
# 21  participant_9A  0.3644171     0.8492902 140
# 22  participant_9B  0.2805585     0.8519382 140


ItemTotalDF<-as.data.frame(item.total(new_rating))

head(ItemTotalDF)

# Variable Item.Total Alpha.Without   N
# 1 participant_10A  0.3791230     0.8487088 140
# 2 participant_10B  0.3949450     0.8480997 140
# 3 participant_11A  0.4195672     0.8472296 140
# 4 participant_12A  0.4946354     0.8444463 140
# 5 participant_12B  0.4312707     0.8467871 140
# 6 participant_13A  0.4687166     0.8453060 140


summary(ItemTotalDF$Item.Total)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2806  0.3831  0.4241  0.4210  0.4513  0.5215 

alpha(new_rating)

# Reliability analysis   
# Call: alpha(x = new_rating)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.85      0.85    0.89       0.2 5.6 0.021  1.8 0.57
# 
# lower alpha upper     95% confidence boundaries
# 0.81 0.85 0.89 


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

v1<-levels(rating7$Item)


median<-tapply(rating7$Rating,rating7$Item,median)

median<-as.data.frame(median)

class(median)
#[1] "data.frame"

str(median)
# 
# 'data.frame':	182 obs. of  1 variable:
#   $ median: num [1:182(1d)] 2 1 1 4 4 1 2 1 1 1.5 ...
# ..- attr(*, "dimnames")=List of 1
# .. ..$ : chr  "aerobically" "agriculturally" "associationally" "belly" ...


rating_Exp2<-merge(Experiment2, median, by.x="Item", by.y="row.names")

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

write.csv(median, file="Experiment__median.csv", sep=",")



###################
# Let's look at the ratings for each affix, i.e. at the reliability for each affix

#############un

un<-rating_Exp2[rating_Exp2$Affix=="un",]

un<-droplevels(un)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(un$Item)
length(list_items)
# 43


list_participants <- levels(un$Participant)
length(list_participants)
# 22

43*22

# 946

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


new_rating_un <- matrix(tmp_list,ncol=22,byrow=TRUE)
colnames(new_rating_un) <- list_participants
rownames(new_rating_un) <- list_items

colnames(new_rating_un)
rownames(new_rating_un)


new_rating_un<-as.data.frame(new_rating_un)

dim(new_rating_un)
#[1] 43 22

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_un, missing = FALSE)

# Call: ICC(x = new_rating_un, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type  ICC   F df1 df2 p lower bound upper bound
# Single_raters_absolute   ICC1 0.24 7.9  42 903 0        0.16        0.35
# Single_random_raters     ICC2 0.24 7.8  42 882 0        0.16        0.35
# Single_fixed_raters      ICC3 0.24 7.8  42 882 0        0.16        0.35
# Average_raters_absolute ICC1k 0.87 7.9  42 903 0        0.81        0.92
# Average_random_raters   ICC2k 0.87 7.8  42 882 0        0.81        0.92
# Average_fixed_raters    ICC3k 0.87 7.8  42 882 0        0.81        0.92
# 
# Number of subjects = 43     Number of Judges =  22

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_un)

# Variable   Item.Total Alpha.Without  N
# 1  participant_10A_Experiment_2  0.915810940     0.8875931 13
# 2  participant_10B_Experiment_2  0.926157682     0.8838074 13
# 3  participant_12A_Experiment_2  0.934765429     0.8902913 13
# 4  participant_12B_Experiment_2  0.691489845     0.8931818 13
# 5  participant_13A_Experiment_2  0.934765429     0.8902913 13
# 6   participant_1A_Experiment_2           NA     0.9038128 13
# 7   participant_1B_Experiment_2  0.934765429     0.8902913 13
# 8   participant_2A_Experiment_2  0.934765429     0.8902913 13
# 9   participant_2B_Experiment_2  0.054235251     0.9060385 13
# 10  participant_3A_Experiment_2  0.744580147     0.8918468 13
# 11  participant_3B_Experiment_2  0.926157682     0.8838074 13
# 12  participant_4A_Experiment_2  0.934765429     0.8902913 13
# 13  participant_4B_Experiment_2  0.744580147     0.8918468 13
# 14  participant_5A_Experiment_2  0.586676245     0.8957854 13
# 15  participant_5B_Experiment_2 -0.004937685     0.9166764 13
# 16  participant_6A_Experiment_2           NA     0.9038128 13
# 17  participant_6B_Experiment_2  0.054235251     0.9060385 13
# 18  participant_7A_Experiment_2 -0.009823770     0.9071222 13
# 19  participant_8A_Experiment_2           NA     0.9038128 13
# 20  participant_8B_Experiment_2  0.054235251     0.9060385 13
# 21  participant_9A_Experiment_2  0.054235251     0.9060385 13
# 22  participant_9B_Experiment_2  0.691489845     0.8931818 13
# Warning messages:
#   1: In cor(items[, i], apply(items[, -i], 1, mean)) :
#   Standardabweichung ist Null
# 2: In cor(items[, i], apply(items[, -i], 1, mean)) :
#   Standardabweichung ist Null
# 3: In cor(items[, i], apply(items[, -i], 1, mean)) :
#   Standardabweichung ist Null

# still bad for un


#############dis

dis<-rating_Exp2[rating_Exp2$Affix=="dis",]

dis<-droplevels(dis)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(dis$Item)
length(list_items)
# 50


list_participants <- levels(dis$Participant)
length(list_participants)
# 22

22*50

# 1100

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(dis[dis$Participant==participant & dis$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(dis[dis$Participant==participant & dis$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating_dis <- matrix(tmp_list,ncol=22,byrow=TRUE)
colnames(new_rating_dis) <- list_participants
rownames(new_rating_dis) <- list_items

colnames(new_rating_dis)
rownames(new_rating_dis)


new_rating_dis<-as.data.frame(new_rating_dis)

dim(new_rating_dis)
#[1] 50 22

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_dis, missing = FALSE)

# Intraclass correlation coefficients 
# type   ICC   F df1  df2       p lower bound upper bound
# Single_raters_absolute   ICC1 0.098 3.4  49 1050 2.3e-13       0.058        0.16
# Single_random_raters     ICC2 0.097 3.3  49 1029 5.7e-13       0.057        0.16
# Single_fixed_raters      ICC3 0.096 3.3  49 1029 5.7e-13       0.056        0.16
# Average_raters_absolute ICC1k 0.706 3.4  49 1050 2.3e-13       0.573        0.81
# Average_random_raters   ICC2k 0.703 3.3  49 1029 5.7e-13       0.569        0.81
# Average_fixed_raters    ICC3k 0.701 3.3  49 1029 5.7e-13       0.566        0.81
# 
# Number of subjects = 50     Number of Judges =  22

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_dis)
# not even applicable



#############ly

ly<-rating_Exp2[rating_Exp2$Affix=="ly",]

ly<-droplevels(ly)


# in order to have a look at these measurement, 
#we need our table in another format


list_items<-levels(ly$Item)
length(list_items)
# 72


list_participants <- levels(ly$Participant)
length(list_participants)
# 22

72*22

# 1584

tmp_list=c()



for (participant in list_participants){
  for (item in list_items)
  {
    if (length(ly[ly$Participant==participant & ly$Item==item, "Rating" ])==1)
    {
      rating<- as.numeric(ly[ly$Participant==participant & ly$Item==item,"Rating"])
    }
    else 
    {
      rating<-NA }
    tmp_list<-append(tmp_list,rating)
  }
}


new_rating_ly <- matrix(tmp_list,ncol=22,byrow=TRUE)
colnames(new_rating_ly) <- list_participants
rownames(new_rating_ly) <- list_items

colnames(new_rating_ly)
rownames(new_rating_ly)


new_rating_ly<-as.data.frame(new_rating_ly)

dim(new_rating_ly)
#[1] 72 22

# okay, cool it seems to have worked

# now we can have a look at the ICC

ICC(new_rating_ly, missing = FALSE)

# Call: ICC(x = new_rating_ly, missing = FALSE)
# 
# Intraclass correlation coefficients 
# type  ICC   F df1  df2 p lower bound upper bound
# Single_raters_absolute   ICC1 0.11 3.8  71 1512 0       0.075        0.17
# Single_random_raters     ICC2 0.11 3.8  71 1491 0       0.075        0.17
# Single_fixed_raters      ICC3 0.11 3.8  71 1491 0       0.076        0.17
# Average_raters_absolute ICC1k 0.74 3.8  71 1512 0       0.642        0.82
# Average_random_raters   ICC2k 0.74 3.8  71 1491 0       0.642        0.82
# Average_fixed_raters    ICC3k 0.74 3.8  71 1491 0       0.642        0.82
# 
# Number of subjects = 72     Number of Judges =  22

# sieht ziemlich schlecht aus


# now let's have a look at the ite.total scores

item.total(new_rating_ly)
#not appliavle

#################
# let's get the alpha

alpha(new_rating_ly)
