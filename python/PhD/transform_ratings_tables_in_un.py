# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 09:41:20 2015

@author: sbenhedia
"""

# This script transforms the data from lime survey into a format which is actrually wanted - it transposes some 
# rows etc....
# and then it megres all the different tables we have from different participants

import csv
import datetime



today="{:%d_%m_%Y}".format(datetime.datetime.now())


# Wir öffnen die Datei mit den ratings und lesen sie ein:

ratings= open('C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Analyses/csv/ratings/results-survey-reformat.csv', 'rt')

csv.reader(ratings)


# Wir erstellen eine nue csv-Datei, in die wir unsere Ratings in der Form, in der wir sie haben wollen, reinschreiben (csv, mit
#Komma getrennt und sie soll ratings_un_in_1 heißen)

csvfile = open("ratings_all_participant_" +today+".csv", "w", encoding="utf8")
dis_ly_ratings= csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)

# Wir schreiben Überschriften in die Datei

dis_ly_ratings.writerow(["ID", "Date submitted", "Last page", "Start-Language", "Date started", "Date last action","Participant", "Age","Sex", "L1","Bilingual", "Grow_Up_Region", "Languages", "Latin", "Profession_Studies", "University",  "Knowledge_English_Ling","Phonetics","Phonology","Morphology","Semantics", "TimeRating", "TotalTime", "Item", "Rating"])


# Wir lesen die erste Reihe unserer original ratings Datei ein, da hier die items benannt sind. Wir machen eine Liste
# aus dieser Datei

first_row=ratings.readline()
list_first_row= first_row.split(",")


# Wir gehen durch jede Reihe der original-Datei

for row in ratings:
     #print(row)
    
# Wir setzen einen Zähler
        i= 1
     
# in den Spalten 0-21 sind Informationen zum speaker, die unter speaker information gespeichert werden
# ab spalte 13 finden wir ratings, die 1,2,3 oder 4 als Wert haben, d.h. solange wir uns in einer Zelle befinden,
# die 1,2,3 oder 4 ials Inhalt hat, nehmen wir diesen Wert als rating, holen uns aus derselben Spalte das item (aus der
 # first row Liste) und packen alles zusammen mit der speaker information in current_row_output, der dann in unsere neue csv 
  # geschrieben wird. Nach jeder Zelle, wird i hochgezählt, sodass wir durch jede Spalte einer Zeile gehen, bis wir zum
# nächsten speaker/zur nächsten Zeile springen.
   
        while row.split(",")[(20+i)] == "1 - Very easy to break into parts in/im/un +rest of word" or  row.split(",")[((20+i))] == "2 - Easy to break into parts in/im/un +rest of word" or row.split(",")[(20+i)] == "3 - Difficult to break into parts in/im/un +rest of word" or  row.split(",")[(20+i)] == "4 - Very difficult to break into parts in/im/un +rest of word" or row.split(",")[(20+i)] == "I don't know this word.":
          item= list_first_row[(20+i)]
          item=item[154:-1]
          #print(item)
          rating= row.split(",")[(20+i)]
          rating=rating[0]
          if rating== "I":
              rating="NA"
          #print(rating)
          speaker_information = row.split(",")[:21]
          speaker_information[6]="participant_"+ speaker_information[6]
          #print(speaker_information[6]) 
          #print(speaker_information)
          TotalTime=row.split(",")[-2]
          TimeRating=row.split(",")[-5]
          #print(speaker_information)
          current_row_output=speaker_information
          current_row_output.append(TimeRating)
          current_row_output.append(TotalTime)
          current_row_output.append(item)
          current_row_output.append(rating)
          un_in_ratings.writerow(current_row_output)
          i=i+1
    


# Wir öffnen die Datei mit den ratings von participant 3 and 4 und lesen sie ein:

ratings= open('C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Ratings/participant_3_4.csv', 'rt')

csv.reader(ratings)

# Wir lesen die erste Reihe unserer original ratings Datei ein, da hier die items benannt sind. Wir machen eine Liste
# aus dieser Datei

first_row=ratings.readline()
list_first_row= first_row.split(",")


# Wir gehen durch jede Reihe der original-Datei

for row in ratings:
     print(row)
    
# Wir setzen einen Zähler
        i= 1
     
# in den Spalten 0-12 sind Informationen zum speaker, die unter speaker information gespeichert werden
# ab spalte 13 finden wir ratings, die 1,2,3 oder 4 als Wert haben, d.h. solange wir uns in einer Zelle befinden,
# die 1,2,3 oder 4 ials Inhalt hat, nehmen wir diesen Wert als rating, holen uns aus derselben Spalte das item (aus der
 # first row Liste) und packen alles zusammen mit der speaker information in current_row_output, der dann in unsere neue csv 
  # geschrieben wird. Nach jeder Zelle, wird i hochgezählt, sodass wir durch jede Spalte einer Zeile gehen, bis wir zum
# nächsten speaker/zur nächsten Zeile springen.
   
#        while row.split(",")[(20+i)] == "1 - Very easy to break into parts in/im/un +rest of word" or  row.split(",")[((20+i))] == "2 - Easy to break into parts in/im/un +rest of word" or row.split(",")[(20+i)] == "3 - Difficult to break into parts in/im/un +rest of word" or  row.split(",")[(20+i)] == "4 - Very difficult to break into parts in/im/un +rest of word" or row.split(",")[(20+i)] == "I don't know this word.":
#          item= list_first_row[(20+i)]
#          item=item[154:-1]
#          #print(item)
#          rating= row.split(",")[(20+i)]
#          rating=rating[0]
#          if rating== "I":
#              rating="NA"
#          #print(rating)
#          speaker_information = row.split(",")[:21]
#          speaker_information[6]="participant_"+ speaker_information[6]
#          #print(speaker_information[6]) 
#          #print(speaker_information)
#          TotalTime=row.split(",")[-2]
#          TimeRating=row.split(",")[-5]
#          #print(speaker_information)
#          current_row_output=speaker_information
#          current_row_output.append(TimeRating)
#          current_row_output.append(TotalTime)
#          current_row_output.append(item)
#          current_row_output.append(rating)
#          un_in_ratings.writerow(current_row_output)
#          i=i+1
#
#  
csvfile.close()