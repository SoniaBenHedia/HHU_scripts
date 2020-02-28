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
TextGridPath = "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/fertige n items/"


TextGridFilter = "*.TextGrid"


# The csv is which everything is saved is the fiollowing
#OutputPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/test_items_"
OutputPath = "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/csv/durations/n_"


# Here we find a table with the columns we want to have
TemplatePath =  "C:/Users/sbenhedia/Dropbox/SpoMo_General Stuff/Compunds/csv/durations/compund_dur_tenplate.csv"


#headers=["ID, Sentence,Participant, Order, Item, WordDur, SyllNum, SegNum, NasalDur, PrecSeg, PrecSegDur, FollSeg, FollSegDur, PrePauseDur, PostPauseDur, SentenceDur, GlottalStop, GlottalStopDur,LocSpeech, PrefixDur, BaseDur"]



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

# We create a dictionary for the morpheme type and specify the boundary alignment accuracy 
MorphemeType = {}
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
                            PrecSeg=segments.intervals[0].text
                            PrecSegDur=segments.intervals[0].duration()
#
                            FollSegment=segments.intervals[2].text
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
                            
                            Consonant2="NA"
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

                                PrecSeg=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()
#
                                FollSegment=segments.intervals[3].text
                                FollSegDur=segments.intervals[3].duration()
                                #print(segments.intervals[1].text)
                                #print(segments.intervals[2].text)
                            
                            
                            #if pause before consonant of second constituent
                            elif segments.intervals[1].text=="pause" or segments.intervals[1].text=="Pause"or segments.intervals[1].text==" pause" or segments.intervals[1].text=="pause ":

                                Pause=segments.intervals[1].text
                                PauseDur=segments.intervals[1].duration()
                                Consonant=segments.intervals[2]

                                PrecSeg=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()
#
                                FollSegment=segments.intervals[3].text
                                FollSegDur=segments.intervals[3].duration()
                                #print(segments.intervals[1].text)

                            elif segments.intervals[0].text=="pre":

                                Pause=segments.intervals[1].text
                                PauseDur=segments.intervals[1].duration()
                                Consonant=segments.intervals[2]

                                PrecSeg=segments.intervals[0].text
                                PrecSegDur=segments.intervals[0].duration()
#
                                FollSegment=segments.intervals[3].text
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
                            Consonant2=segments.intervals[3].text
                            Consonant2Dur=segments.intervals[3].duration()
                            PrecSeg=segments.intervals[0].text
                            #print(PrecSeg+" "+Filename)
                            PrecSegDur=segments.intervals[0].duration()
#
                            FollSegment=segments.intervals[4].text
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
                        PrecSeg=str(PrecSeg)
                        
                        FollSegment=str(FollSegment)
                        
                        if not re.search(vowels,PrecSeg):
                             PrecSegVC="C"
                        else:
                             PrecSegVC= "V"
                             
                                               
                        if not re.search(vowels,FollSegment):
                             FollSegVC="C"
                        else:
                             FollSegVC= "V"
                             
                             

                        
                        

                        OutputFile.writerow ([
                        #print([   
                            Sentence.text.replace(",", ";"),

                            Item.text, 
                            Item.duration(),
                            SyllNum,
                            SegNum,
                            Consonant.text,
                             Consonant.duration(), 
                             PrecSeg,
                             PrecSegVC,
                             PrecSegDur, 
                             FollSegment,
                             FollSegVC,
                             FollSegDur,
                             PrePauseDur,
                             PostPauseDur,
                             Sentence.duration(),
                             SegNum/ Item.duration(),     # LocSpeechRate
                             Pause,
                             PauseDur,
                             Consonant2,
                             Consonant2Dur,                             
                             Filename
                            ])
            
          
#           
        except Exception as e:
                     print (e)
                     print((Filename + " has a problem"))
                     #raise e

