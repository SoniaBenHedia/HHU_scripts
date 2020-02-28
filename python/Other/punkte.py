Punkte=open("C:/Users/Student/Desktop/punkte.txt", mode="r", encoding="utf8")

list_punkte=Punkte.readlines()

i=1
Gesamt=0

for element in list_punkte:
    element_strip=element.strip("\n")

    if element_strip[-2:]!="":

        Punktezahl_i= int(element_strip[-2:])

        Gesamt=Gesamt+Punktezahl_i

        i=i+1

Durchschnitt= Gesamt/(i-1)
        

print("Der Mittelwert der Punktzahlen von",i-1, "Teilnehmern beträgt",Durchschnitt)
        
