import re

def count_chars_by_hand ():

    left= []
    right=[]
    
    a=re.compile("[A-Ga-g]")

    h=re.compile("[H-Zh-z]")

    eingabe= input("Geben Sie etwas ein!")

    for element in eingabe:
        if re.match(a,element):
                 left.append(element)
        else:
                 right.append(element)

                 
    #return left
    #return right

    print(len(left) ,"Buchstaben wurden mit links geschrieben")
    print(len(right), "Buchstaben wurden mit rechts geschrieben.")

    eingabe.split()

#>>> count_chars_by_hand("hello")
#Linke Hand: 1
#Rechte Hand: 4
#Nur eine Hand: (keine Ergebnisse)
#>>> count_chars_by_hand("Was geht?")
#Linke Hand: 6
#Rechte Hand: 2
#Nur eine Hand: "Was" 
