import types
def share_type(): 

    type_s = types.types()
    wort = input("Wort dessen Share berechnet werden soll: ").upper()
    
    if wort in type_s:
        einzelwert = type_s[wort]
        gesamtwert = 0
        
        for key in type_s:
            gesamtwert = gesamtwert + type_s[key]
            
        wort_anteil = int(einzelwert/gesamtwert*100)
        print (wort_anteil,"%")
          
    elif wort not in type_s: 
        print("Dieses Wort ist im Text nicht enthalten.")    