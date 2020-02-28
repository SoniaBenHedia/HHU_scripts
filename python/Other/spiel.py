import random

Zufallszahl= random.randint (0,50)
my_guess = int(input ("Gib eine Zahl ein:"))
while my_guess != Zufallszahl:
    if my_guess < Zufallszahl:
        print ("Die Zahl ist größer! Probier's nochmal.")
    else:
        print ("Die Zahl ist kleiner!Probier's nochmal!")
   

    my_guess = int(input ("Gib eine weitere Zahl ein:"))

print ("Gewonnen!")
