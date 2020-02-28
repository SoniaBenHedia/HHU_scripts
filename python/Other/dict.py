# dictionary deutsch_ englisch erstellen


Deutsch = ["eins", "zwei", "drei", "vier"]
English =["one", "two", "three", "four"]
Fra=["un","deux", "trois", "quatre"]

DE_EN=dict(zip(Deutsch,English))
EN_FR=dict(zip(English,Fra))

def dict(x):

    if x in Deutsch :
        print(DE_EN[x])

        
    else:
        for element in DE_EN:
            if DE_EN[element]== x:
                print (element)
    
