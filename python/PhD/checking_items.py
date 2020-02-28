# -*- coding: utf-8 -*-
"""
Created on Sun Jan 31 15:38:41 2016

@author: sbenhedia
"""
# This program extracts 10 % of thhe items in each folder of the fertige textgride of the same
# date and saves them in a new folder (Checking Items). It randomly selects 10 textgriuds and teh matching
# sound files from each folder (containing 100 items).

import shutil
import os, os.path
import fnmatch
#import tgt
#import csv
import datetime
import re
import random

zahlen=re.compile("[0-99]")

today="{:%d_%m_%Y}".format(datetime.datetime.now())
#
Sounds_Path = "C:/Users/sbenhedia/Dropbox/Geminates/Experimente/un_in/Files_Sonia/"
SoundWalk=os.walk(Sounds_Path )
FilterSound="*.wav"
FileListSounds = []

for SourcePath, Folders, Files in SoundWalk:
    for CurrentFile in Files:
        if fnmatch.fnmatch (CurrentFile, FilterSound):
            FileListSounds.append (CurrentFile)
#            
#print(FileListSounds)
## now we have to find the mathcing sound files and copy them in the pertinent folder
#
FilterTextGrid = "*.TextGrid*"
Text_Grid_Path="C:/Users/sbenhedia/Dropbox/Geminates/Experimente/un_in/Dokumentation/Praat_Fortschritt/Fertige_Textgrids_" +today+"/"
#
FileWalk = os.walk(Text_Grid_Path )

##Now we need to extraxt 10 % from each folder
##
for SourcePath, Folders, Files in FileWalk:
    if re.search(zahlen,SourcePath[-1]):
        list_items=[]
        for element in Files:
            if fnmatch.fnmatch (element, FilterTextGrid):
                list_items.append(element)
                Path =os.path.abspath(os.path.join(SourcePath,element))
                Path =Path.strip(element)

                random.shuffle(list_items)
                
        # now we have to create the "subfolders" by getting the names of the segmentors
        # and the IDs
                
        Items=str(Path.split("Fertige_Textgrids_09_03_2016")[-1])
        Items=Items.split('''\\''')
        Items= (Items[-3]+ "/" + Items[-2] )
        print(Items)

        Path_checking_items= (Text_Grid_Path + "CheckingItems_"+today + "/"+Items)
        os.makedirs(Path_checking_items)
        for element in list_items[1:11]:
            filename= element[:-9]
            sound_file= filename+".wav"
            print(sound_file)
            shutil.copy2(Path + element,Path_checking_items )
            print (element)
#            
            for element in FileListSounds:
                    if element == sound_file:
                        shutil.copy2(Sounds_Path + element,Path_checking_items )