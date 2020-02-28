import shutil
import os, os.path
import fnmatch
import tgt


Text_Grid_Sounds_Path = "C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Small_Sound_Files/"
FilterTextGrid = "*.TextGrid*"
FilterSound="*.wav"
OutputPathSonia = "C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Files_Sonia/"
OutputPathSabine = "C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Files_Sabine/"


FileWalk = os.walk(Text_Grid_Sounds_Path )

FileListTextGrids = []
FileListSounds = []

GridStart=0.02

# Wir gehen alle Dateien im Ordner durch und schauen, ob es ein
# textgrid ist oder ein sound - dementsprechend wird es einer Liste
# zugeordnet

for SourcePath, Folders, Files in FileWalk:
    for CurrentFile in Files:
        if fnmatch.fnmatch (CurrentFile, FilterTextGrid):
            FileListTextGrids.append (CurrentFile)
        if fnmatch.fnmatch (CurrentFile, FilterSound):
            FileListSounds.append (CurrentFile)




# jetzt werden alle textgrids durchgegangen, die textgrids werden ausgelesen,
# sodass wir den annotierten Satz bekommen
# außerdem setzen wir den string sound_ file fest, sodass wir wissen, wie
# das entsprechener sound_file heißen müsste

# Wir legen  Zähler für die ID-Nummern an

# i für Sonias items:

i=1

# j für Sabines items:
j= 1
        
for element in FileListTextGrids:        
             
            # now we need to get some information
            
            
            # the participant
            participant=(element.split("_")[0]+"_"+element.split("_")[1]) 
            
            # the filename
            filename=element
            
            
            # the order in which the sentence was uttered (it was the ...sentence
            #the participant read)
            ForList=(element[:-9])
            List_needed= (ForList.split("Participant_"))
            last_element =(List_needed[-1])
            last_element=(last_element.split("_"))
            order=last_element[1]
            

            
            # the sentence read (here we are also opening the textgrid)
            CurrentTextGrid = tgt.io.read_textgrid (Text_Grid_Sounds_Path + element)
            sentence= CurrentTextGrid.get_tier_by_name("sentence")
            sentence=sentence[0]
            sentence= str(sentence)
            sentence=sentence.split("\"")
            
            # we are also getting the time at which the textgrid ends (we need
            #this later to create new textgrids)
            Metadata= sentence[0]
            GridEnd=(float(Metadata[14]))
            
            # here we are getting the sentence out of the textgrid
            sentence= sentence [-2]
            
            # here we create the name of the sound file (which should also be
            #found in the folder)
            sound_file= filename[:-8] +"wav"
            #print(sound_file)
            
            
            
            ###########################################################################################-------
            # Jetzt haben wir mit sentence in die Sätze von jedem textgrid+ passendem sound_file :) )
             ##########################################################################################
            
            
            
            # Jetzt prüfen wir, ob es sich um meine Sätze handelt
            if "It is JOHN" in sentence or "John says" in sentence or "It is John" in sentence or "John tells" in sentence:

               # wenn dem so ist, wird das sound file in meinen Ordner kopiert (nachem 
               # geprüft wird, ob es vorhanden ist)
                          
              if sound_file in FileListSounds:
                 
                  shutil.copy2(Text_Grid_Sounds_Path + sound_file,OutputPathSonia )
              
              # Außerdem legen wir eine ID-Nummer an
              
              ID= str(i)
              # Der Zähler wird danach um 1 hochgesetzt
              i=i+1
              
            # außerdem müssen neue textgrids angelegt werden. Dazu brauchen wir die Info, welches
            # item in dem Satz gelesen wurde_
              if "It is JOHN who tells me" in sentence:
                  item= sentence.split("It is JOHN who tells me ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
              if "It is JOHN who tells you" in sentence:
                  item= sentence.split("It is JOHN who tells you ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
              if "It is JOHN who says" in sentence:
                  item= sentence.split("It is JOHN who says ")[1] 
                  item=item.split(" again")[0]
                  item=item.lower()
              if "It is John who tells me" in sentence:
                  item= sentence.split("It is John who tells me ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
              if "It is John who tells you" in sentence:
                  item= sentence.split("It is John who tells you ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
              if "It is John who says" in sentence:
                  item= sentence.split("It is John who says ")[1] 
                  item=item.split(" again")[0]
                  item=item.lower()
              if "John says" in sentence:
                  item= sentence.split("John says ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
              if "John tells me" in sentence:
                  item= sentence.split("John tells me ")[1] 
                  item=item.split(" again")[0]
                  item=item.lower()
              if "John tells you" in sentence:
                  item= sentence.split("John tells you ")[1]
                  item=item.split(" again")[0]
                  item=item.lower()
            
            
            
            


              #print(item)
              
              
             # Jetzt können die textgrids angelegt werden.dazu nehmen wir CurrentTextGrid
            # und fügen die zusätzlichen tiers hinzu  
                
              IDTier = tgt.IntervalTier(GridStart, GridEnd, "ID")
              ParticipantTier = tgt.IntervalTier(GridStart, GridEnd, "Participant")
              ItemTier = tgt.IntervalTier(GridStart, GridEnd, "Item")
              SegmentTier = tgt.IntervalTier(GridStart, GridEnd, "Segments")
              SylStructTier = tgt.IntervalTier(GridStart, GridEnd, "SyllableStruct")
              MorphemeTypeTier = tgt.IntervalTier(GridStart, GridEnd, "MorphemeType")
              OrderOfOccurenceTier=tgt.IntervalTier(GridStart, GridEnd, "Order")
#                
              
              
                   
            # Add intervals to text grids:

              IDTier.add_interval(tgt.Interval(GridStart, GridEnd, ID))
              SegmentTier.add_interval(tgt.Interval(GridStart, GridEnd, ""))
              ItemTier.add_interval(tgt.Interval(GridStart, GridEnd, item))
              ParticipantTier.add_interval(tgt.Interval(GridStart, GridEnd, participant))
              OrderOfOccurenceTier.add_interval(tgt.Interval(GridStart, GridEnd, order))
              SylStructTier.add_interval(tgt.Interval(GridStart, GridEnd, ""))
              MorphemeTypeTier.add_interval(tgt.Interval(GridStart, GridEnd, ""))


              CurrentTextGrid.add_tier(IDTier)
              CurrentTextGrid.add_tier(ParticipantTier)
              CurrentTextGrid.add_tier(OrderOfOccurenceTier)
              CurrentTextGrid.add_tier(ItemTier)
              CurrentTextGrid.add_tier(SegmentTier)
              CurrentTextGrid.add_tier(SylStructTier)
              CurrentTextGrid.add_tier(MorphemeTypeTier)
             
#                #Write text grid to new file:
              #tgt.io.export_to_short_textgrid(CurrentTextGrid)
              tgt.write_to_file(CurrentTextGrid, OutputPathSonia+ filename, format="short", encoding="utf-8")
#                print("Done.")

              #print(CurrentTextGrid)
         
            
            else:
               # wenn es sich um Sabines Sätze handelt,wird das sound file in Sabines Ordner kopiert (nachem 
               # geprüft wird, ob es vorhanden ist)
                          
              if sound_file in FileListSounds:
                   shutil.copy2(Text_Grid_Sounds_Path + sound_file,OutputPathSabine )
             
             
             # Außerdem legen wir eine ID-Nummer 
              ID=str(j)
              # Der Zähler wird danach um 1 hochgesetzt
              j=j+1              
              
            # außerdem müssen neue textgrids angelegt werden. Dazu brauchen wir die Info, welches
              
             # Jetzt können die textgrids angelegt werden.dazu nehmen wir CurrentTextGrid
            # und fügen die zusätzlichen tiers hinzu  
                
              IDTier = tgt.IntervalTier(GridStart, GridEnd, "ID")
              ParticipantTier = tgt.IntervalTier(GridStart, GridEnd, "Participant")
              ItemTier = tgt.IntervalTier(GridStart, GridEnd, " ")
              OrderOfOccurenceTier=tgt.IntervalTier(GridStart, GridEnd, "Order")
             
                   
            # Add intervals to text grids:

              IDTier.add_interval(tgt.Interval(GridStart, GridEnd, ID))
              ItemTier.add_interval(tgt.Interval(GridStart, GridEnd, ""))
              ParticipantTier.add_interval(tgt.Interval(GridStart, GridEnd, participant))
              OrderOfOccurenceTier.add_interval(tgt.Interval(GridStart, GridEnd, order))

              CurrentTextGrid.add_tier(IDTier)
              CurrentTextGrid.add_tier(ParticipantTier)
              CurrentTextGrid.add_tier(OrderOfOccurenceTier)
              CurrentTextGrid.add_tier(ItemTier)
             
              #Write text grid to new file:
              #tgt.io.export_to_short_textgrid(CurrentTextGrid)
              tgt.write_to_file(CurrentTextGrid, OutputPathSabine+ filename, format="short", encoding="utf-8")
           
print("It's done")