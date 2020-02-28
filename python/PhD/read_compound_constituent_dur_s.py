# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 18:11:16 2017

@author: sbenhedia
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Feb  4 10:21:35 2016

@author: sbenhedia
"""

#import shutil
import os, os.path
import fnmatch
import tgt
import csv
import datetime
import re


#regulären Ausdruck für Vokale

vowels=re.compile("[a,e,o,u,i,Q,3,{,@, A,E,I,O, U, V,Y]")

#regulären Ausdruck für Diphtong

diph=re.compile("(aI|AI|eI|OI|@U| I@| eA|  U@| Ai|Ei|@I|IO |EI|OU|Ui)")





 # get the date

today="{:%d_%m_%Y}".format(datetime.datetime.now())


# this script is supposed to read textgrids and get information out of it

# #- number of Segments
# number of syllables
# word duration
# segment durations (preceding, /s/ and following)

# This is the path where you find the textgrids

#TextGridPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/test_items/"
TextGridPath = "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/fertige s items/"


TextGridFilter = "*.TextGrid"


# The csv is which everything is saved is the fiollowing
#OutputPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/test_items_"
OutputPath = "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/csv/durations/s_constituent_durations_"


# Here we find a table with the columns we want to have
TemplatePath =  "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/csv/durations/compound_const_dur_template.csv"


#headers=["ID, Sentence,Participant, Order, Item, WordDur, SyllNum, SegNum, NasalDur, PrecSeg, PrecSegDur, FollSeg, FollSegDur, PrePauseDur, PostPauseDur, SentenceDur, GlottalStop, GlottalStopDur,LocSpeech, PrefixDur, BaseDur"]

# we need three dictionaries in which we code the environment of each compound

# Wir öffnen die Datei mit den types

type_infos= open('C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/scripts/type_info.csv', 'rt')

csv.reader(type_infos)


# we create two lists

Compound_list=list()
Environment_list=list()
# Wir gehen durch jede Reihe der original-Datei

for row in type_infos:
    
            Compound= row.split(",")[0]
            Compound=Compound.replace("_"," ")
            #print(Compound)
            
            Compound_list.append(Compound)
            environment=row.split(",")[1]
            #print(environment)
            Environment_list.append(environment)


# now we create a dictionary 

compound_environment=dict(zip(Compound_list, Environment_list))

print(compound_environment)
#print(compound_environment)

# we make a list of all the textgrids which have to be analyzed

FileWalk = os.walk (TextGridPath)
FileList = []
for SourcePath, Folders, Files in FileWalk:
    for CurrentFile in Files:
        if fnmatch.fnmatch (CurrentFile, TextGridFilter):
            FileList.append ((SourcePath, CurrentFile))

print("There are "+ str(len(FileList))+ " textgrids to be analyzed")            

## we make a list of the tier names and create a dictionary for the tier
TierNames = ["sentence",  "item", "segments", "syllables"]
Tiers = {}
#
#in this definition all the tiers if a textgrids are read and put into a dictionary
def ReadTiers (TextGrid, TierNames):
    D = {}
    for CurrentTierName in TierNames:
        try:
            D [CurrentTierName] = TextGrid.get_tier_by_name (CurrentTierName)
        except ValueError as e:
            print ("TextGrid does not contain a tier '%s'.") % (CurrentTierName)
            raise e

# We specify the boundary alignment accuracy 
BoundaryAlignmentAccuracy = 0.01        


# This is the file

with open(OutputPath + str(today)+ '.csv', 'w', newline='') as csvfile:
    OutputFile= csv.writer(csvfile, delimiter=',')
                            
    for Headers in csv.reader(open(TemplatePath, "r"), delimiter=","):
        OutputFile.writerow(Headers)
        break
    
    
   
    for (Path, Filename) in FileList:
    # find all textgrids
        try:
                        CurrentTextGrid = tgt.read_textgrid (os.path.join(Path, Filename))
                        sentence=CurrentTextGrid.get_tier_by_name("sentence")
                        #print(sentence)
                        item=CurrentTextGrid.get_tier_by_name("item")
                        segments=CurrentTextGrid.get_tier_by_name("segments")
                        syllables=CurrentTextGrid.get_tier_by_name("syllables")

            
            


                 
                        # read items and Pauses
                        
                        # items without  prePause and with post
                        #print (len(Tiers["Item"].intervals))
                        if item.intervals[0].text != "pre" and item.intervals[0].text != "pre " and item.intervals[0].text != " pre":
                            Item= item.intervals [0]
                           # print(Item)
                            PrePauseDur= 0
                            
                            if len(item.intervals)>1:
                                if item.intervals[1].text == "post":
                                 #   print("post 1")
                                    PostPause= item.intervals[1]
                                    PostPauseDur=PostPause.duration() 
                            else:
                                    PostPauseDur=0
                            
                                
                        else:
                            Item= item.intervals [1]
                            #print(Item)

                            PrePause=item.intervals [0]
                            PrePauseDur= PrePause.duration()
                            
                            if len(item.intervals) > 2:

                                 #if Tiers["Item"].intervals[2].text == "post":
                                     #print("post 2")
                                     PostPause= item.intervals[2]
                                     PostPauseDur=PostPause.duration()
                                     #print(PostPause.text)
                            else:
                                    PostPauseDur=0


                        
                        # read Sentence
                        
                        Sentence= sentence.intervals[0]
                        #print(Sentence)
#
#                        # read consonant& FollSeg & PrecSeg


                        # if no pause
                        if len(segments.intervals)==3:
                            
                            Pause="NA"
                            PauseDur=0
                            
                            Consonant2="NA"
                            Consonant2Dur=0                            

                            Consonant=segments.intervals[1]
                            PrecSeg=segments.intervals[0]

                            PrecSegText=segments.intervals[0].text
                            PrecSegDur=segments.intervals[0].duration()
                            
                            FollSegment=segments.intervals[2]
                            FollSegmentText=segments.intervals[2].text
                            FollSegDur=segments.intervals[2].duration()
                            
                            
                                                    # number of syllables:
                        
                            SyllNum= (len(syllables.intervals))
                        
                            SegNum=0
                            for i, element in enumerate(syllables.intervals):
                                if re.search(diph,element.text):
#
                                    dipht=1
                                else:
                                    dipht=0
                                SegmentsCurrentSyllable=(len(element.text))-dipht
                                SegNum= SegNum+SegmentsCurrentSyllable
                            
                     



                      
#                       for items with a a pause between the constituents

                        #for singletons
                        
                        if len(segments.intervals)==4:
                            
                            Consonant2Text="NA"
                            Consonant2Dur=0

                            
                            

                            # number of syllables:
                        
                            SyllNum= (len(syllables.intervals))-1
                        
                            SegNum=-5
                            for i, element in enumerate(syllables.intervals):
                                if re.search(diph,element.text):
#
                                    dipht=1
                                else:
                                    dipht=0
                                SegmentsCurrentSyllable=(len(element.text))-dipht
                                SegNum= SegNum+SegmentsCurrentSyllable
                            
                            
                            
                            #if pause after consonant of first constituent
                            
                            if segments.intervals[2].text=="pause" or segments.intervals[2].text=="Pause" or segments.intervals[2].text==" pause" or segments.intervals[2].text=="pause ":
                                Pause=segments.intervals[2].text
                                PauseDur=segments.intervals[2].duration()
                                Consonant=segments.intervals[1]

                                PrecSeg=segments.intervals[0]
                                PrecSegText=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()

                                FollSegment=segments.intervals[3]
                                FollSegmentText=segments.intervals[3].text
                                FollSegDur=segments.intervals[3].duration()
                                #print(segments.intervals[1].text)
                                #print(segments.intervals[2].text)
                            
                            
                            #if pause before consonant of second constituent
                            elif segments.intervals[1].text=="pause" or segments.intervals[1].text=="Pause"or segments.intervals[1].text==" pause" or segments.intervals[1].text=="pause ":

                                Pause=segments.intervals[1].text
                                PauseDur=segments.intervals[1].duration()
                                Consonant=segments.intervals[2]

                                PrecSeg=segments.intervals[0]
                                PrecSegText=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()

                                FollSegment=segments.intervals[3]#
                                FollSegmentText=segments.intervals[3].text
                                FollSegDur=segments.intervals[3].duration()
                                #print(segments.intervals[1].text)

                            elif segments.intervals[0].text=="pre":

                                Pause=segments.intervals[1].text
                                PauseDur=segments.intervals[1].duration()
                                Consonant=segments.intervals[2]

                                PrecSeg=segments.intervals[0]
                                PrecSegText=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()
                                
                                FollSegment=segments.intervals[3]
                                FollSegmentText=segments.intervals[3].text
                                FollSegDur=segments.intervals[3].duration()                            
                            
                            else:
                                print(Item.text +" in file "+Filename+
                                      " has four intervals but there is no pause between the two constituents."
                                      +segments.intervals[1].text+" "+ segments.intervals[2].text)
                                
                            # if pause between doubles   

                        if len(segments.intervals)==5:

                            Pause=segments.intervals[2].text
                            PauseDur=segments.intervals[2].duration()

                            Consonant=segments.intervals[1]
                            Consonant2=segments.intervals[3]

                            Consonant2Text=segments.intervals[3].text
                            Consonant2Dur=segments.intervals[3].duration()
                            
                            PrecSeg=segments.intervals[0]
                            
                            PrecSegText=segments.intervals[0].text
                            PrecSegDur=segments.intervals[0].duration()
#
                            FollSegment=segments.intervals[4]
                            FollSegmentText=segments.intervals[4].text
                            FollSegDur=segments.intervals[4].duration()
                            
                            
                                                    # number of syllables:
                        
                            SyllNum= (len(syllables.intervals))-1
                        
                            SegNum=-5
                            for i, element in enumerate(syllables.intervals):
                                if re.search(diph,element.text):
#
                                    dipht=1
                                else:
                                    dipht=0
                                SegmentsCurrentSyllable=(len(element.text))-dipht
                                SegNum= SegNum+SegmentsCurrentSyllable
                            

                                            
                            

                       #FollSegVC and PrecSegVC
                        PrecSegString=str(PrecSegText)
                        
                        FollSegmentString=str(FollSegmentText)
                        
                        if not re.search(vowels,PrecSegString):
                             PrecSegVC="C"
                        else:
                             PrecSegVC= "V"
                             
                                               
                        if not re.search(vowels,FollSegmentString):
                             FollSegVC="C"
                        else:
                             FollSegVC= "V"
                             
                             

                        # based on the information we already have, we can 
                        # get the durations of the constituents
                        
                        # the start time of N1 and the end time of N2 are always the
                        # same, independent of environment
                        N1_start= (Item.start_time)
                        N2_end=(Item.end_time)
                        
                        #print((N1_start))
                        
                        # but depending on the environment, the end of N1 and the
                        # start of N2 differ
                        
                        
                        # for VC 
                        if compound_environment[Item.text] == "VC":
                    
                        # the first constituent ends at the end of PrecSeg 
                            #(irrespective of resyllabification)
                        
                            Constituent_1_resyll_end= (PrecSeg.end_time)
                            Constituent_1_no_resyll_end= (PrecSeg.end_time)
                       
                        # and the second starts at the beginning of Consonant
                            #(irrespective of resyllabification)
                        
                            Constituent_2_resyll_start= (Consonant.start_time)
                            Constituent_2_no_resyll_start= (Consonant.start_time)

                        # for CV 
                        elif compound_environment[Item.text] == "CV":
                        
                        # the first constituent ends at the end of Consonant
                            #(when no resyllabification)
                        
                            Constituent_1_no_resyll_end= (Consonant.end_time)
                        
                        # it ends after the preceding vowel if resyll.
                        
                            Constituent_1_resyll_end= (PrecSeg.end_time)
                       
                        # and the second starts at the beginning of Foll Vowel
                            #(when no resyllabification)   
                            
                            Constituent_2_no_resyll_start= (FollSegment.start_time)

                        # it starts at the beginning of the consonant, if resyll.
                        
                            Constituent_2_resyll_start= (Consonant.start_time)

                        # for CC
                        elif compound_environment[Item.text] == "CC":
                        
                        # when there is no pause between the 2 constituents
                            if Consonant2=="NA":
                        # the first constituent ends at the mid duration of 
                        # double consonant
                            #(when no resyllabification)
                        
                                Mid_point=(Consonant.duration())/2
                                #Mid_point=2
                            
                                Constituent_1_no_resyll_end= (Consonant.start_time) + Mid_point
                                             
                        
                        # it ends after the preceding vowel if resyll.
                        
                                Constituent_1_resyll_end= (PrecSeg.end_time)
                       
                        # and the second also starts at the mid duration of the cons.
                            #(when no resyllabification)   
                            
                                Constituent_2_no_resyll_start= Constituent_1_no_resyll_end

                        # it starts at the beginning of the consonant, if resyll.
                        
                                Constituent_2_resyll_start= (Consonant.start_time)


                        # if there is a pause, no resyllabification is possible
                            else:
                                 
                                Constituent_1_resyll_end= (Consonant.end_time)
                       
                                Constituent_2_resyll_start= (Consonant2.start_time)                   
                        
                        # the first constituent ends after the first consonant
                        

                                Constituent_1_no_resyll_end= (Consonant.end_time)
                        
                       
                        # and the second starts at the beginning of the second consonant
                            
                                Constituent_2_no_resyll_start= (Consonant2.start_time)



                        N2_end=float(N2_end)
                        N1_start= float(N1_start)
                        Constituent_1_no_resyll_end= float(Constituent_1_no_resyll_end)
                        Constituent_2_no_resyll_start= float(Constituent_2_no_resyll_start)
                        

 
                        
                        N1Dur=      Constituent_1_no_resyll_end - N1_start
                        N2Dur=      N2_end - Constituent_2_no_resyll_start
                        


                        #if Constituent_1_resyll_end== "NA":
                         #   N1DurRes=   "NA"
                          #  N2DurRes=   "NA"
                            
                        #else:                            

                         #  print(Constituent_1_resyll_end)                
                        Constituent_1_resyll_end= float(Constituent_1_resyll_end)
                        Constituent_2_resyll_start = float(Constituent_2_resyll_start)                        
                        N1DurRes=   Constituent_1_resyll_end - N1_start
                        N2DurRes=   N2_end - Constituent_2_resyll_start   
#                            
                        OutputFile.writerow ([
                        #print([   
                            Filename,                            
                            Item.text, 
                            Item.duration(),
                            N1Dur,
                            N2Dur,
                            N1DurRes,
                            N2DurRes
                            ])
            
          
#           
        except Exception as e:
                     print (e)
                     print((Filename + " has a problem"))
                     #raise e

