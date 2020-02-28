# Importe
import os.path
import sys
import time

# Alle unsere (leicht editierten) Programme 
import sonia
import kim
import anna
import pascal
import christine

def run():
  """
  None -> None
  Führt das Programm aus.
  """
  args = sys.argv # Die Liste aller Parameter des Programmaufrufs: {main.py, <1>, <2>, <3>, ..., <n>}
  path = ""
  if len(args)==2:
    path = args[1]
  else:
    path = eingabe_abfrage1()
  while not(os.path.isfile(path)) and path != "": # Prüft, ob der Pfad gültig ist.
    path = eingabe_abfrage1() # Fragt nach Eingabedateipfad
  if path != "": # Path ist nicht das zugelassene leere Wort.
    infile = read_file(path)
    result_list = process_file(infile)

    length_words_dict = result_list[0]  # Dict für Aufgabe 1        {Tokenlänge:Menge aller Tokens dieser Länge}
    freq_words_dict = result_list[1]    # Dict für Aufgabe 2        {Rang:Liste der Wörter mit diesem Rang}
    words_freq_dict = result_list[2]    # Dict für Aufgabe 3 und 5  {Wort:Häufigkeit dieses Wortes}
    length_count_dict = result_list[3]  # Dict für Aufgabe 4        {Tokenlänge:Anzahl der Tokens dieser Länge}
    token_count = result_list[4]        # Int für Aufgabe 4         die Anzahl aller Tokens der gegebenen Datei
    rank_words_dict = result_list[5]    # Dict für Aufgabe 5        {Rang:Liste der Worte dieses Ranges}

    input2 = eingabe_abfrage2()
    while input2!="": # Solange der input nicht das leere Wort ist.

      if input2 == "1": 
        aufgabe1(length_words_dict)

      elif input2 == "2": 
        aufgabe2(freq_words_dict)

      elif input2 == "3": 
        aufgabe3(words_freq_dict)

      elif input2 == "4": 
        aufgabe4(length_count_dict, token_count)

      elif input2 == "5":
        aufgabe5("PLACEHOLDER")

      time.sleep(1) # Wartet n Sekund(en), bis das Programm weiter ausgeführt wird.
      input2 = eingabe_abfrage2()

def eingabe_abfrage1():
  """
  None -> String
  Fragt nach einer Eingabe und gibt sie als String zurück.
  """
  print("Geben Sie eine gültige Token-Datei ein.(Enter zum Beenden)")
  return input(">>>")

def eingabe_abfrage2():
  """
  None -> String
  Fragt nach einer Eingabe und gibt sie als String zurück.
  """
  print("")
  print("Geben Sie 1 ein, um die [n] längsten/kürzesten Wörter zu ermitteln.")
  print("Geben Sie 2 ein, um die [n] häufigsten/seltensten Wörter zu ermitteln.")
  print("Geben Sie 3 ein, um den Anteil eines [Tokens] am Gesamttext zu ermitteln.")
  print("Geben Sie 4 ein, um den Anteil der Worttoken mit Mindest-/Höchstlänge [n] am Gesamttext zu ermitteln.")
  print("Geben Sie 5 ein, um den Text auf das Zipfsche Gesetzt zu prüfen.")
  print("(Enter zum Beenden)")
  return input(">>>")


def read_file(path):
  """
  string -> list
  Erzeugt aus einer Datei mit gegebenem Pfad path eine Liste der Zeilen.
  """
  infile = open(path, mode="r", encoding="utf8" )
  data = infile.readlines()
  infile.close()
  return data

def process_file(file_data):
  """
  list -> list() = {dictionary1,dictionary2,dictionary3,dictionary4,int}
  Erzeugt aus der Datei des gegebenen Dateipfads
  eine Liste mit 2 Elementen:
  einem Dictionary mit {Tokenlänge:Menge aller Tokens dieser Länge},        <-- für A1
  einem Dictionary mit {Häufigkeit:Liste der Wörter mit dieser Häufigkeit}, <-- für A2
  einem Dictionary mit {Wort:Häufigkeit},                                   <-- für A3 und A5
  einem Dictionary mit {Tokenlänge:Anzahl der Tokens dieser Länge},         <-- für A4
  einer Zahl - die Anzahl aller Tokens der gegebenen Datei                  <-- für A4
  einem Dictionary mit {Rang:Liste der Wörter mit diesem Rang},             <-- für A5
  """
  length_words_dict = dict() # Dict für Aufgabe 1
  freq_words_dict = dict() # Dict für Aufgabe 2
  rank_words_dict = dict() # Dict für Aufgabe 5
  words_freq_dict = dict() # Dict für Aufgabe 3 und 5
  length_count_dict = dict() # Dict für Aufgabe 4
  token_count = 0 # Int für Aufgabe 4
  for token in file_data:
    token = token.strip("\n.,!?.;'\"\\/´`^(){}[]1234567890\"").upper()
    length = len(token)

    if length > 0 and token != "":
      token_count = token_count + 1
      # Dict 1 {Tokenlänge:Menge aller Tokens dieser Länge} 
      # Dict 4 {Tokenlänge:Anzahl der Tokens dieser Länge}
      if not(length in length_count_dict): # Wenn ein Länge noch nicht im Dictionary 1 oder 4 vorhanden ist.
        wordset = set()
        wordset.add(token)
        length_words_dict.update({length:wordset}) # Füllt Dict1
        length_count_dict.update({length:1}) # Füllt Dict4
      else:
        length_words_dict[len(token)].add(token) # Füllt Dict1
        length_count_dict.update({length:(length_count_dict[length]+1)}) # Füllt Dict4

      # Dict 3 {Wort:Häufigkeit}
      if token not in words_freq_dict:        
        words_freq_dict.update({token:1})
      else:
        words_freq_dict[token] += 1

  # Dict 2 {Häufigkeit: Liste der Wörter mit dieser Häufigkeit}
  for elem in words_freq_dict:
    key = words_freq_dict[elem]
    value = [elem]
    if key in freq_words_dict:
      freq_words_dict[key] = freq_words_dict[key] + [elem]
    else:
      freq_words_dict[key] = value
  # Dict 5 {Rang: Liste der Wörter mit diesem Rang}
  tmp_list = list(freq_words_dict.keys())
  rank = 1
  for freq in tmp_list:
    rank_words_dict[rank] = freq_words_dict[freq]
    rank += len(freq_words_dict[freq])

  # Ergebnis Liste
  result_list = list()
  result_list.append(length_words_dict)
  result_list.append(freq_words_dict)
  result_list.append(words_freq_dict)
  result_list.append(length_count_dict)  
  result_list.append(token_count)
  result_list.append(rank_words_dict)

  return result_list
   
def aufgabe1(input_dict):
  """
  String -> None
  Führt Aufgabenstellung 1 mit Sonias Programm aus.
  """
  length_words = input_dict
  n = int(input("Die wie viel kürzesten und längsten Wörter sollen ausgegeben werden? "))
  sonia.max_wort(n,length_words)
  sonia.min_wort(n,length_words)


def aufgabe2(input_dict):
  """
  String -> None
  Führt Aufgabenstellung 2 mit Kims Programm aus.
  """
  lex2 = input_dict # Erstellt ein Dictionary
  n = int(input("Die n häufigsten und seltensten Wörter: n = "))
  kim.freq_words(n,lex2)

def aufgabe3(input_dict):
  """
  String -> None
  Führt Aufgabenstellung 3 mit Annas Programm aus.
  """
  type_s = input_dict #anna.types(path) # Erstellt ein Dictionary
  wort = input("Wort dessen Share berechnet werden soll: ").upper()
  wort_anteil = anna.share_type(type_s,wort)
  if wort_anteil != None:
    print(wort_anteil,"%")
  else:
    print("Dieses Wort ist im Text nicht enthalten.")

def aufgabe4(input_dict,token_count):
  """
  String -> None
  Führt Aufgabenstellung 4 mit Pascals Programm aus.
  """
  length_count_dict = input_dict # pascal.make_dict(path) # Erstellt ein Dictionary
  print("Geben Sie an, aus welchem Wortlängen-Interval(n,m) Sie den Anteil berechnen möchten: ")
  n = int(input("Untere Schranke n: "))
  m = int(input("Obere Schranke m: "))
  result = str(pascal.length_perc(n,m,length_count_dict,token_count))
  print(result+"%")

def aufgabe5(placeholder):
  """
  String -> None
  Führt Aufgabenstellung 5 mit Christines Programm aus.
  """
  #christine.bla(1,2,3)

run()
