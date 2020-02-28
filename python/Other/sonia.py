import re

#path=input("Welche Datei wollen Sie verwenden?")
#path = "C:/Users/sbenhedia/Documents/Methoden/Python/Scripts/max_min_files/minitext_token.txt"

def make_dict(path):
  """
  string -> dict
  Erzeugt aus einer Datei mit gegebenem Pfad path ein Dictionary.
  """

  myfile= open (path, "r", encoding="utf8")

  wort= re.compile ("[A-Za-z]")

  length_words= dict()


  for line in myfile:
      
       if re.search(wort,line):
           
          line= line.strip("\n")
          
          if len(line) in length_words :
              length_words[len(line)].add(line)
                            

          else:
              wordset= set()
              wordset.add(line)
                       
              length_words[len(line)] = wordset

  return length_words



def max_wort(n,length_words):#=length_words):

          #n=int(input("Die wie viel längsten Wörter sollen ausgegeben werden?"))
          i=1
 
          #global length_words

          max_buchstabenanzahl= max(length_words.keys())
          longest_words= length_words [max(length_words.keys())]
          anzahl_worte = len(longest_words)
         

          print ("Die Wörter/das Wort:" ,longest_words,"haben/hat", max_buchstabenanzahl, "Buchstaben!") 
          
          
          
          while anzahl_worte <n:
              
               max_buchstabenanzahl_i = max_buchstabenanzahl -i
               if max_buchstabenanzahl_i in length_words.keys():
                    
                    longest_words_i=length_words [max_buchstabenanzahl_i]
                    print ("Die Wörter:" ,length_words [max_buchstabenanzahl_i] ,"haben" ,max_buchstabenanzahl_i ,"Buchstaben!")
                    anzahl_worte =anzahl_worte + len(longest_words_i)

               i=i+1




#n=int(input("Die wie viel längsten Wörter sollen ausgegeben werden?"))
#max_wort(n)

def min_wort(n,length_words):#=length_words):

          #n=int(input("Die wie viel kürzesten Wörter sollen ausgegeben werden?"))
          i=1
 
          #global length_words

          min_buchstabenanzahl= min(length_words.keys())
          shortest_words= length_words [min(length_words.keys())]
          anzahl_worte = len(shortest_words)
         

          print ("Die Wörter/das Wort:" ,shortest_words,"haben/hat", min_buchstabenanzahl, "Buchstaben!") 
          
          
          
          while anzahl_worte <n:
              
               min_buchstabenanzahl_i = min_buchstabenanzahl +i
               if min_buchstabenanzahl_i in length_words.keys():
                    
                    shortest_words_i=length_words [min_buchstabenanzahl_i]
                    print ("Die Wörter:" ,length_words [min_buchstabenanzahl_i] ,"haben" ,min_buchstabenanzahl_i ,"Buchstaben!")
                    anzahl_worte =anzahl_worte + len(shortest_words_i)

               i=i+1




#n=int(input("Die wie viel kürzesten Wörter sollen ausgegeben werden?"))
#min_wort(n)




