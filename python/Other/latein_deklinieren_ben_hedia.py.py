def latein_deklinieren(Wort):

    '''(str) --> str
    dekliniert das angegebene Wort 'Wort' für die 5 Fälle des Lateinischen im Singular
    und Plural und speichert diese Formen in einer Datei mit dem Namen des
    Wortes, das dekliniert werden soll. Diese Funktion kann nur in -o und -a
    endende lateinische Wörter deklinieren.
    >> latein_deklinieren (domina)

    Singular:

    Nom domina
    Gen dominae
    Dat dominae
    Acc dominam
    Abl domina

    Plural:

    Nom dominae
    Gen dominarum
    Dat dominis
    Acc dominas
    Abl dominis
    '''
    
    if Wort[-1]=="a":

        file_name=Wort+".txt"
        infile=open(file_name, "w", encoding="utf8")   

        print ("Singular:", file=infile)
        
        print ("Nom " +Wort[:-1] + "a", file=infile)
        print ("Gen " + Wort [:-1] + "ae", file=infile)
        print ("Dat " + Wort [:-1] + "ae", file=infile)
        print ("Acc " + Wort [:-1] + "am", file=infile)
        print ("Abl " + Wort [:-1] + "a", file=infile)
        
        print ("Plural:", file=infile)
        
        print ("Nom " + Wort [:-1] + "ae", file=infile)
        print ("Gen " + Wort [:-1] + "arum", file=infile)
        print ("Dat " + Wort [:-1] + "is", file=infile)
        print ("Acc " + Wort [:-1] + "as", file=infile)
        print ("Abl " + Wort [:-1] + "is", file=infile)

        print ("Die Deklination von "+ Wort+ " wurde in der Datei " + file_name+ " gespeichert!")

        infile.close()

    else:

        if Wort[-1]=="o":

            file_name=Wort+".txt"
            infile=open(file_name, "w", encoding="utf8")   
            
            print ("Singular:", file=infile)
        
            print ("Nom " +Wort[:-1] + "us", file=infile)
            print ("Gen " +Wort[:-1] + "i", file=infile)
            print ("Dat " + Wort[:-1] + "o", file=infile)
            print ("Acc " + Wort[:-1] + "um", file=infile)
            print ("Abl " + Wort[:-1] + "o", file=infile)
        
            print ("Plural:", file=infile)
        
            print ("Nom " + Wort[:-1] + "i", file=infile)
            print ("Gen " + Wort[:-1] + "orum", file=infile)
            print ("Dat " + Wort [:-1]+ "is", file=infile)
            print ("Acc " + Wort[:-1] + "os", file=infile)
            print ("Abl " + Wort [:-1]+ "is", file=infile)

            print ("Die Deklination von "+ Wort+ " wurde in der Datei " + file_name+ " gespeichert!")

            infile.close()

        else:
            print ("Bitte machen Sie eine valide Eingabe!")
            print ("Dieses Programm kann nur lateinische Wörte, die auf a oder o enden deklinieren!")

        
