Deutsch = ["eins", "zwei", "drei", "vier"]
English =["one", "two", "three", "four"]
Fra=["un","deux", "trois", "quatre"]

DE_EN=dict(zip(Deutsch,English))
EN_FR=dict(zip(English,Fra))

def dict(x,y):
    '''dict(str,str)=str
    x=Wort
    y=Sprache, in die das Wort übersetzt werden soll(D,F,E)

    Dieses Programm kann Zahlen bis 4 in die Sprachen Deutsch, English und
    Französisch übersetzen. Es benutzt dazu nur zwei dictionaries

    >>> dict("deux","D")
    zwei

    '''

    if y =="E":

        if x in Deutsch :
            print(DE_EN[x])

        
        else:
            for element in EN_FR:
                     if EN_FR[element]== x:
                        print (element)
           

    if y=="F":

        if x in English:
            print(EN_FR[x])

        
        else:
            print(EN_FR[(DE_EN[x])])


    if y=="D":

        if x in English:
             for element in DE_EN:
                     if DE_EN[element]== x:
                        print (element)

        
        else:           
             for element in EN_FR:
                 
                     if EN_FR[element]== x:
                         x2=element
                         
                         for element in DE_EN:
                             
                             if DE_EN[element]== x2:
                                print (element)
          
               
    
