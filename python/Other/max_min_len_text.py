
# Text zum Ausprobieren:C:/Users/sbenhedia/Documents/Methoden/Python/minitext_token.txt

myfile= open ("C:/Users/sbenhedia/Documents/Methoden/Python/minitext_token.txt", "r", encoding="utf8")


# Problem: Wie kann ich die values eines dictionaries als set setzen?
# mit dieseem import(defaultdict?)
from collections import defaultdict 

import re

wort= re.compile ("[A-Za-z]")

length_words= defaultdict(set)


for line in myfile:
    
     if re.search(wort,line):
   # \na muss noch weggeschnitten werden.      
             if len(line) in length_words :
                      length_words[len(line)].add(line)
                      # Wie kann ich dann etwas zu dem dict-set hinzufügen? UND
                      #wie bleibt len(line) numerisch?

        

             else:
                     length_words[len(line)] = line



# Hier dann die Funktionen: Diese müsste man so schreiben, dass verschiedene
# Texte eingegeben werden können
#def longest_words(text,number):







