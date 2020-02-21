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

 # get the date

today="{:%d_%m_%Y}".format(datetime.datetime.now())


# this script is supposed to read textgrids and get information out of it

# #- number of Segments
# number of syllables
# word duration
# segment durations (preceding, /s/ and following)

# This is the path where you find the textgrids

#TextGridPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/test_items/"
TextGridPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Recordings/Fertige Items/"
#Fertige Items/Sonia/"


TextGridFilter = "*.TextGrid"


# The csv is which everything is saved is the fiollowing
#OutputPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/test_items_"
OutputPath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/durations_dis_ly_TypeOfLY"


# Here we find a table with the columns we want to have
TemplatePath = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Data/Textgrids_auslesen/ScriptGemExpAuslesen2.csv"


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
TierNames = ["ID", "sentence", "Participant", "Order", "Item", "Segments", "SyllableStruct", "MorphemeType", "Accentuation"]
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


# in this definition coding mistakes are fiixed
def FixMorphemeType(S):
    if S == "prefi":
        return "prefix"
    if S == "bas":
        return "base"
    return S
#
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
        
        except IndexError:
            pass 
            print(Filename)
            
        else:
            

# go through them and   
     
            for CurrentTier in CurrentTextGrid.tiers:
                Tiers[CurrentTier.name] = CurrentTier
                #print(CurrentTier)
            
#        check which are the complex words (= 2 morphemes) and which are the simplex
            # items (= one morpheme)
            
                if CurrentTier.name == "MorphemeType":
                 
                 try:
                 
                    #if len(Tiers["MorphemeType"].intervals) == 1:
                    if len(Tiers["MorphemeType"].intervals) == 1 and Tiers["MorphemeType"].intervals[0].text != "baseLY":
                        #print (Tiers["MorphemeType"].intervals[0].text)
                        #print("%s is a base'" % Filename)
            
# HIER ALLES WAS MIT DEN BASEN von dis
                        # read ID
                        ID = Tiers["ID"].intervals[0]
                        #print(ID)
                        # read items and Pauses
                        
                        # items without  prePause and with post
                        #print (len(Tiers["Item"].intervals))
                        if Tiers["Item"].intervals[0].text != "pre" and Tiers["Item"].intervals[0].text != "pre " and Tiers["Item"].intervals[0].text != " pre":
                            Item= Tiers["Item"].intervals [0]
                            PrePauseDur= 0
                            
                            if len(Tiers["Item"].intervals)>1:
                                if Tiers["Item"].intervals[1].text == "post":
                                 #   print("post 1")
                                    PostPause= Tiers["Item"].intervals[1]
                                    PostPauseDur=PostPause.duration() 
                            else:
                                    PostPauseDur=0
                            
                                
                        else:
                            Item= Tiers["Item"].intervals [1]
                            PrePause=Tiers["Item"].intervals [0]
                            PrePauseDur= PrePause.duration()
                            
                            if len(Tiers["Item"].intervals) > 2:

                                 #if Tiers["Item"].intervals[2].text == "post":
                                     #print("post 2")
                                     PostPause= Tiers["Item"].intervals[2]
                                     PostPauseDur=PostPause.duration()
                                     #print(PostPause.text)
                            else:
                                    PostPauseDur=0

                            #print(Item)      

                        # read order
                        Order= Tiers["Order"].intervals [0]
                        
                        
                        
                        # read Sentence
                        
                        Sentence= Tiers["sentence"].intervals[0]


                        # read Participants
                        
                        Participant= Tiers["Participant"].intervals[0]
            
                        #read base

                        Base = Tiers["MorphemeType"].intervals [0].text
                        BaseDur = Tiers["MorphemeType"].intervals [0].duration()
                        #print(Base)
                        
                        # 
                        
                        # since there is no prefix we can't get its duration
                        
                        AffixDur="NA"
                        AffixDurWithoutGlottalStop="NA"

                        # read glottal stop & nasal & FollSeg & PrecSeg
                        
                
                                                
                        if Tiers["Segments"].intervals [0].text =="?":
                            GlottalStopTier=Tiers["Segments"].intervals[0]
                            GlottalStopDur=GlottalStopTier.duration()
                            GlottalStop="GlottalStop"
                            
                             
                            Consonant=Tiers["Segments"].intervals[1]
                            
                            PrecSeg=Tiers["Segments"].intervals[0].text
                            PrecSegDur=Tiers["Segments"].intervals[0].duration()

                            FollSegment=Tiers["Segments"].intervals[2].text
                            FollSegDur=Tiers["Segments"].intervals[2].duration()                          
                        
                        else:
                            GlottalStop= "NoGlottalStop"
                            GlottalStopDur=0
                            Consonant=Tiers["Segments"].intervals[0]
                            PrecSeg="NA"
                            PrecSegDur=0
                            FollSegment=Tiers["Segments"].intervals[1].text
                            FollSegDur=Tiers["Segments"].intervals[1].duration()
                     
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
                             
                             
                        # number of syllables:
                        
                        SyllNum= (len(Tiers["SyllableStruct"].intervals))
                        
                        SegNum=0
                        for i, element in enumerate(Tiers["SyllableStruct"].intervals):
                            SegmentsCurrentSyllable=(len(element.text))
                            SegNum= SegNum+SegmentsCurrentSyllable
                        
                        #FirstSyllDur
                        
                        FirstSyllDur= Tiers["SyllableStruct"].intervals[0].duration()

                        #FirstSyllDur
                        
                        LastSyllDur= Tiers["SyllableStruct"].intervals[-1].duration()

                        # Accentuation:
                        try:
                            Accentuation= Tiers["Accentuation"].intervals[0].text
                        
                        except:
                            Accentuation= "unknown"
                        
                        # WordDuration without glottalstop
                        
                        WordDurWithoutGlottalStop= Item.duration() - GlottalStopDur

                        OutputFile.writerow ([
                        #print([   
                            ID.text,
                            Sentence.text.replace(",", ";"),
                            Participant.text,
                            Order.text,
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
                             GlottalStop,
                             GlottalStopDur,
                             SegNum/ Item.duration(),     # LocSpeechRate
                             AffixDur,
                            BaseDur,
                            FirstSyllDur,
                            WordDurWithoutGlottalStop,
                            AffixDurWithoutGlottalStop,
                            Base,
                            "NA",
                            Accentuation,
                            LastSyllDur
                            ])
            
                    #if len(Tiers["MorphemeType"].intervals) == 1:
                    if len(Tiers["MorphemeType"].intervals) == 1 and Tiers["MorphemeType"].intervals[0].text == "baseLY":
                        #print (Tiers["MorphemeType"].intervals[0].text)
                        #print("%s is a base'" % Filename)
            
# HIER ALLES WAS MIT DEN BASEN von ly
                        # read ID
                        ID = Tiers["ID"].intervals[0]
                        #print(ID)
                        # read items and Pauses
                        
                        # items without  prePause and with post
                        #print (len(Tiers["Item"].intervals))
                        if Tiers["Item"].intervals[0].text != "pre" and Tiers["Item"].intervals[0].text != "pre " and Tiers["Item"].intervals[0].text != " pre":
                            Item= Tiers["Item"].intervals [0]
                            PrePauseDur= 0
                            
                            if len(Tiers["Item"].intervals)>1:
                                if Tiers["Item"].intervals[1].text == "post":
                                 #   print("post 1")
                                    PostPause= Tiers["Item"].intervals[1]
                                    PostPauseDur=PostPause.duration() 
                            else:
                                    PostPauseDur=0
                            
                                
                        else:
                            Item= Tiers["Item"].intervals [1]
                            PrePause=Tiers["Item"].intervals [0]
                            PrePauseDur= PrePause.duration()
                            
                            if len(Tiers["Item"].intervals) > 2:

                                 #if Tiers["Item"].intervals[2].text == "post":
                                     #print("post 2")
                                     PostPause= Tiers["Item"].intervals[2]
                                     PostPauseDur=PostPause.duration()
                                     #print(PostPause.text)
                            else:
                                    PostPauseDur=0

                            #print(Item)      

                        # read order
                        Order= Tiers["Order"].intervals [0]
                        
                        
                        
                        # read Sentence
                        
                        Sentence= Tiers["sentence"].intervals[0]


                        # read Participants
                        
                        Participant= Tiers["Participant"].intervals[0]
            
                        #read base

                        Base = Tiers["MorphemeType"].intervals [0].text
                        BaseDur = Tiers["MorphemeType"].intervals [0].duration()
                        #print(Base)
                        
                        # 
                        
                        # since there is no prefix we can't get its duration
                        
                        AffixDur="NA"
                        AffixDurWithoutGlottalStop="NA"

                        # read glottal stop & nasal & FollSeg & PrecSeg
                        
                
                                                
                        if Tiers["Segments"].intervals [0].text =="?":
                            GlottalStopTier=Tiers["Segments"].intervals[0]
                            GlottalStopDur=GlottalStopTier.duration()
                            GlottalStop="GlottalStop"
                            
                             

                      
                        
                        else:
                            GlottalStop= "NoGlottalStop"
                            GlottalStopDur=0

                        FollSegment="NA"
                        FollSegDur=0
                     
                        
                        Consonant=Tiers["Segments"].intervals[-1]
                        print(Consonant.text)
                            
                        PrecSeg=Tiers["Segments"].intervals[-2].text
                        PrecSegDur=Tiers["Segments"].intervals[-2].duration()                        
                        
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
                             
                             
                        # number of syllables:
                        
                        SyllNum= (len(Tiers["SyllableStruct"].intervals))
                        
                        SegNum=0
                        for i, element in enumerate(Tiers["SyllableStruct"].intervals):
                            SegmentsCurrentSyllable=(len(element.text))
                            SegNum= SegNum+SegmentsCurrentSyllable
                        
                        #FirstSyllDur
                        
                        FirstSyllDur= Tiers["SyllableStruct"].intervals[0].duration()

                        #FirstSyllDur
                        
                        LastSyllDur= Tiers["SyllableStruct"].intervals[-1].duration()

                        # Accentuation:
                        try:
                            Accentuation= Tiers["Accentuation"].intervals[0].text
                        
                        except:
                            Accentuation= "unknown"
                        
                        # WordDuration without glottalstop
                        
                        WordDurWithoutGlottalStop= Item.duration() - GlottalStopDur

                        OutputFile.writerow ([
                        #print([   
                            ID.text,
                            Sentence.text.replace(",", ";"),
                            Participant.text,
                            Order.text,
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
                             GlottalStop,
                             GlottalStopDur,
                             SegNum/ Item.duration(),     # LocSpeechRate
                             AffixDur,
                            BaseDur,
                            FirstSyllDur,
                            WordDurWithoutGlottalStop,
                            AffixDurWithoutGlottalStop,
                            Base,
                            "NA",
                            Accentuation,
                            LastSyllDur
                            ])
                
            
            
                    if len(Tiers["MorphemeType"].intervals) == 2:

                        #print((Tiers["MorphemeType"].intervals[1].text) )
                        
                        # dies sidn die komplexen Wörter

                        # read ID
                        ID = Tiers["ID"].intervals[0]
                        
                        # Accentuation:
                        try:
                            Accentuation= Tiers["Accentuation"].intervals[0].text
                        
                        except:
                            Accentuation= "unknown"          
                        
                        # read items and Pauses
                        
                        # items without  prePause and with post
                        #print (len(Tiers["Item"].intervals))
                        if Tiers["Item"].intervals[0].text != "pre" and Tiers["Item"].intervals[0].text != "pre " and Tiers["Item"].intervals[0].text != " pre":
                            Item= Tiers["Item"].intervals [0]
                            PrePauseDur= 0
                            
                            
                            if len(Tiers["Item"].intervals)>1:
                                if Tiers["Item"].intervals[1].text == "post":
                                    #print("post 1")
                                    PostPause= Tiers["Item"].intervals[1]
                                    PostPauseDur=PostPause.duration() 
                            else:
                                    PostPauseDur=0
                            
                                
                                
                        else:
                            Item= Tiers["Item"].intervals [1]
                            PrePause=Tiers["Item"].intervals [0]
                            PrePauseDur= PrePause.duration()
                            
                            if len(Tiers["Item"].intervals) > 2:

                                 if Tiers["Item"].intervals[2].text == "post":
                                     #print("post 2")
                                     PostPause= Tiers["Item"].intervals[2]
                                     PostPauseDur=PostPause.duration()
                            else:
                                    PostPauseDur=0


                        # read order
                        Order= Tiers["Order"].intervals [0]
                        
                        
                        
                        # read Sentence
                        
                        Sentence= Tiers["sentence"].intervals[0]


                        # read Participants
                        
                        Participant= Tiers["Participant"].intervals[0]
                        

                        
                        #print(Tiers["MorphemeType"].intervals [1].text )

# now we have to distinguish between prefixes and suffixes

                        if Tiers["MorphemeType"].intervals [1].text =="suffix" or Tiers["MorphemeType"].intervals [1].text =="NoMorph":

                                                        
                            # read base
                            Base = Tiers["MorphemeType"].intervals [0].text
                            BaseDur = Tiers["MorphemeType"].intervals [0].duration()
                            
                            
                        # 
                        


                        # read glottal stop & nasal & FollSeg & PrecSeg
                        
                
                                                
                            if Tiers["Segments"].intervals [0].text =="?":
                                GlottalStopTier=Tiers["Segments"].intervals[0]
                                GlottalStopDur=GlottalStopTier.duration()
                                GlottalStop="GlottalStop"

                            
                   
                            else:
                                GlottalStop= "NoGlottalStop"
                                GlottalStopDur=0

                            Consonant=Tiers["Segments"].intervals[-2]
                            
                            PrecSeg=Tiers["Segments"].intervals[-3].text
                            PrecSegDur=Tiers["Segments"].intervals[-3].duration()
                            FollSegment=Tiers["Segments"].intervals[-1].text
                            FollSegDur=Tiers["Segments"].intervals[-1].duration()                           
                                
                       
                       #FollSegVC and PrecSegVC
                            PrecSeg=str(PrecSeg)
                        
                            if not re.search(vowels,PrecSeg):
                                PrecSegVC="C"
                            else:
                                PrecSegVC= "V"
                             
                                               
                            if not re.search(vowels,FollSegment):
                                FollSegVC="C"
                            else:
                                FollSegVC= "V"
                             
                        # we calculate the affix duration by adding the PrecSegDur and the NasalDur 
                        
                            AffixDur= Consonant.duration()+FollSegDur
                        
                        
                            AffixDurWithoutGlottalStop= AffixDur
 
                         # we need the affix
                            Affix=Tiers["MorphemeType"].intervals[1].text
                            
                        # number of syllables:
                        
                            SyllNum= (len(Tiers["SyllableStruct"].intervals))
                        
                            SegNum=0
                            for i, element in enumerate(Tiers["SyllableStruct"].intervals):
                                SegmentsCurrentSyllable=(len(element.text))
                                SegNum= SegNum+SegmentsCurrentSyllable
                        
                        #FirstSyllDur
                            FirstSyllDur= Tiers["SyllableStruct"].intervals[0].duration()
                        
                            LastSyllDur= Tiers["SyllableStruct"].intervals[-1].duration()
                        
                        # WordDuration without glottalstop
                        
                            WordDurWithoutGlottalStop= Item.duration() - GlottalStopDur

##############################################################################
                    # now un
                        
                        elif Tiers["MorphemeType"].intervals [1].text =="base" :



            
                        #read base

                            Base = Tiers["MorphemeType"].intervals [1].text
                            BaseDur = Tiers["MorphemeType"].intervals [1].duration()
                        # 
                        
                        # read glottal stop & nasal & FollSeg & PrecSeg
                        
                
                                                
                            if Tiers["Segments"].intervals [0].text =="?":
                                GlottalStopTier=Tiers["Segments"].intervals[0]
                                GlottalStopDur=GlottalStopTier.duration()
                                GlottalStop="GlottalStop"
                             
                                if Tiers["MorphemeType"].intervals [0].text =="prefix":
                            
                                    Consonant=Tiers["Segments"].intervals[2]
                                    PrecSeg=Tiers["Segments"].intervals[1].text
                                    PrecSegDur=Tiers["Segments"].intervals[1].duration()
                                    FollSegment=Tiers["Segments"].intervals[3].text
                                    FollSegDur=Tiers["Segments"].intervals[3].duration()
                            
                                else:
                                
                                    Consonant=Tiers["Segments"].intervals[3]
                                    PrecSeg=Tiers["Segments"].intervals[2].text
                                    PrecSegDur=Tiers["Segments"].intervals[2].duration()
                                    FollSegment=Tiers["Segments"].intervals[4].text
                                    FollSegDur=Tiers["Segments"].intervals[4].duration()                                
                        
                   
                            else:
                            
                           
                               GlottalStop= "NoGlottalStop"
                               GlottalStopDur=0
                           
                           
                               if Tiers["MorphemeType"].intervals [0].text =="prefix":

                                    Consonant=Tiers["Segments"].intervals[1]
                                    PrecSeg=Tiers["Segments"].intervals[0].text
                                    PrecSegDur=Tiers["Segments"].intervals[0].duration()
                                    FollSegment=Tiers["Segments"].intervals[2].text
                                    FollSegDur=Tiers["Segments"].intervals[2].duration()
  
                               else:
                                   Consonant=Tiers["Segments"].intervals[2]
                                   PrecSeg=Tiers["Segments"].intervals[1].text
                                   PrecSegDur=Tiers["Segments"].intervals[1].duration()
                                   FollSegment=Tiers["Segments"].intervals[3].text
                                   FollSegDur=Tiers["Segments"].intervals[3].duration()                                
                                
                       
                       #FollSegVC and PrecSegVC
                            PrecSeg=str(PrecSeg)
                        
                            if not re.search(vowels,PrecSeg):
                                PrecSegVC="C"
                            else:
                                PrecSegVC= "V"
                             
                                               
                            if not re.search(vowels,FollSegment):
                                FollSegVC="C"
                            else:
                                FollSegVC= "V"
                             
                        # we calculate the prefix duration by adding the PrecSegDur and the NasalDur 
                        
                            AffixDur= GlottalStopDur+Consonant.duration()+PrecSegDur
                        
                        
                            AffixDurWithoutGlottalStop=Consonant.duration()+PrecSegDur

                         # we need the affix
                            Affix=Tiers["MorphemeType"].intervals[0].text
                         
                         
                        # number of syllables:
                        
                            SyllNum= (len(Tiers["SyllableStruct"].intervals))
                        
                            SegNum=0
                            for i, element in enumerate(Tiers["SyllableStruct"].intervals):
                                SegmentsCurrentSyllable=(len(element.text))
                                SegNum= SegNum+SegmentsCurrentSyllable
                        
                        #FirstSyllDur
                        
                            FirstSyllDur= Tiers["SyllableStruct"].intervals[0].duration()

                        #LastSyllDur
                        
                            LastSyllDur= Tiers["SyllableStruct"].intervals[-1].duration()
                        
                        # WordDuration without glottalstop
                        
                            WordDurWithoutGlottalStop= Item.duration() - GlottalStopDur

#####################################################################################################
# now dis


                        elif Tiers["MorphemeType"].intervals [1].text =="baseDIS":
                            
                           
                        #read base

                            #print(Tiers["MorphemeType"].intervals [0].text)
                            Base = Tiers["MorphemeType"].intervals [1].text
                            BaseDur = Tiers["MorphemeType"].intervals [1].duration()
                        # 
                        
                        # read glottal stop & nasal & FollSeg & PrecSeg
                        
                
                                                
                            if Tiers["Segments"].intervals [0].text =="?":
                                GlottalStopTier=Tiers["Segments"].intervals[0]
                                GlottalStopDur=GlottalStopTier.duration()
                                GlottalStop="GlottalStop"
                             
                            
                                Consonant=Tiers["Segments"].intervals[3]
                                PrecSeg=Tiers["Segments"].intervals[2].text
                                PrecSegDur=Tiers["Segments"].intervals[2].duration()
                                FollSegment=Tiers["Segments"].intervals[4].text
                                FollSegDur=Tiers["Segments"].intervals[4].duration()

                   
                            else:
                            
                           
                               GlottalStop= "NoGlottalStop"
                               GlottalStopDur=0
                               Consonant=Tiers["Segments"].intervals[2]
                               PrecSeg=Tiers["Segments"].intervals[1].text
                               PrecSegDur=Tiers["Segments"].intervals[1].duration()
                               FollSegment=Tiers["Segments"].intervals[3].text
                               FollSegDur=Tiers["Segments"].intervals[3].duration()                                
                                
                       
                       #FollSegVC and PrecSegVC
                            PrecSeg=str(PrecSeg)
                        
                            if not re.search(vowels,PrecSeg):
                                PrecSegVC="C"
                            else:
                                PrecSegVC= "V"
                             
                                               
                            if not re.search(vowels,FollSegment):
                                FollSegVC="C"
                            else:
                                FollSegVC= "V"
                             
                        # we calculate the prefix duration by adding the PrecSegDur and the NasalDur 
                        
                            AffixDur= GlottalStopDur+Consonant.duration()+PrecSegDur
                        
                        
                            AffixDurWithoutGlottalStop=Consonant.duration()+PrecSegDur

                         # we need the affix
                            Affix=Tiers["MorphemeType"].intervals[0].text
                         
                         
                        # number of syllables:
                        
                            SyllNum= (len(Tiers["SyllableStruct"].intervals))
                        
                            SegNum=0
                            for i, element in enumerate(Tiers["SyllableStruct"].intervals):
                                SegmentsCurrentSyllable=(len(element.text))
                                SegNum= SegNum+SegmentsCurrentSyllable
                        
                        #FirstSyllDur
                        
                            FirstSyllDur= Tiers["SyllableStruct"].intervals[0].duration()

                        #LastSyllDur
                        
                            LastSyllDur= Tiers["SyllableStruct"].intervals[-1].duration()
                        
                        # WordDuration without glottalstop
                        
                            WordDurWithoutGlottalStop= Item.duration() - GlottalStopDur

                        OutputFile.writerow ([
                        #print([   
                            ID.text,
                            Sentence.text.replace(",", ";"),
                            Participant.text,
                            Order.text,
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
                             GlottalStop,
                             GlottalStopDur,
                             SegNum/ Item.duration(),     # LocSpeechRate
                             AffixDur,
                            BaseDur,
                            FirstSyllDur,
                            WordDurWithoutGlottalStop,
                            AffixDurWithoutGlottalStop,
                            Base,
                            Affix,
                            Accentuation,
                            LastSyllDur                            
                            ])                        #print("%s is a complex word'" % Filename)
                        
                        

#           
#           
                 except Exception as e:
                     print (e)
                     print((Filename + " has a problem"))
                     #raise e

