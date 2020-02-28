#############################################################################################
####                Exp-Items-In-Sentences-In-PPT                                           #
#############################################################################################
####                2015-07-22: Sonia Ben Hedia                                             #
#############################################################################################

##############################################################################################
####                    Program discrition              ######################################
##############################################################################################
# This program takes a list of items which were chosen to be included in a speech production##
#experiments, divides the data in two sets of similar distribution (items belong to different#
#classes),and embedds one set into accented, the other set into unaccented carrier sentences.#
# There are three types of different unaccented and accented carrier sentences in which the  #
# items are embedded. Then all sentences import timeare put into one list which was given out #
#as a txt file when the definition exp_sentences was used. The sentences in this list are for-#
# matted in #
# such a way, that one can copy the list into Latex (Beamer).Each carrier sentence(s) will be#
#displayed on one slide. exp_sentences gives out two txt-files, which both use the same sets-#
#the items which are embedded in accented sentences in the one txt-file, are in unaccented   #
#position in the other textfile.                                                             #
# Außerdem wird eine csv erstellt, die für jedes item in jedem experiment aufzeigt, welche   #
#condition es hat (unaccented oder accented)                                                 #
##############################################################################################


import csv
import random
import datetime
#from time import datetime

### The intitial plan was to use the python-pptx package to compile a ppt with the experimental
#sentences, this did not work however (pptx Did not work without lxml....maybe one day...

#import xlsxwriter
#import lxml###################CAN'T INSTALL THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#import PIL
# Did not wir without lxml!!!! from pptx import presentation

# Wir speichern das Datum, damit wir später alles unter dem aktuellen Datum ablegen können

#today=time.strftime("%d_")
today="{:%d_%m_%Y}".format(datetime.datetime.now())
#Wir legen Listen für jede Kategorie der exp-items an:

list_unn=[]
list_unV=[]


list_imm=[]
list_imPB=[]

list_inn=[]
list_inV=[]

list_base_un=[]
list_base_im=[]
list_base_in=[]


#Wir legen Listen an, die die items in zwei Teile zeilt: accented/unaccented:

unaccented_items=[]
accented_items=[]

        # und für die umgekehrte Variante, denn die items die in einem Durchlauf accented
        #sind, sollen im nächsten unaccented sein

        
unaccented_items_2=[]
accented_items_2=[]

# Wir legen Listen an, für die unaccented und accented Sätzen.
# different versions = different names in the sentece

unaccented_items_version_A=[]
accented_items_version_A=[]
unaccented_items_version_B=[]
accented_items_version_B=[]
unaccented_items_version_C=[]
accented_items_version_C=[]

        # und für die umgekehrte Variante, d.h. für das Einbedden der unaccented_items_2 und
        # der accented_items_2

unaccented_items_version_2_A=[]
accented_items_version_2_A=[]
unaccented_items_version_2_B=[]
accented_items_version_2_B=[]
unaccented_items_version_2_C=[]
accented_items_version_2_C=[]


# Wir legen Listen mit unaccented und accented Sätzen an - hier werden dann alle verschiedenen
# Varianten in eine Liste gepackt

all_accented_sentences_A =[]
all_unaccented_sentences_A= []


        # und für die umgekehrte Variante
        
all_accented_sentences_2_A =[]
all_unaccented_sentences_2_A= []

# Liste mit finalen Sätzen - alle unaccented and alle unaccented Sätze

all_sentences_A=[]
   
        # und für die umgekehrte Variante

all_sentences_2_A=[]


# Wir legen zusätzlich eine Liste mit den filler sentences an


filler_sentences=[]

# Wir lesen die csv ein:

items_exp= open('C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/Python-Zeug-Auswahl/exp_items_un_in.csv', 'rt')
#items_exp = open("exp_items_un_in.csv", "rt")

csv.reader(items_exp)

#jede Reihe wird durchlaufen und die items ihren Listen zugewiesen:

for row in items_exp:

        item=row.split(",")[0].lower()

        if item[0:3]=="unn":
            list_unn.append(item)
            list_base_un.append(item[2:])


        if item[0:3]=="imm":
            list_imm.append(item)

            if item[2:]!="manent":
                if item[2:]!="minent":
                    if item[2:]!="merse":
                        list_base_im.append(item[2:])

        if item[0:3]=="inn":
            list_inn.append(item)

            if item[2:]!="nervate":
                        list_base_in.append(item[2:])


                        
        if item[0:3]=="imp":
            list_imPB.append(item)

        if item[0:3]=="imb":
            list_imPB.append(item)

        if item[0:2]=="un":
            if item[0:3]!="unn":
                list_unV.append(item)
  
        if item[0:2]=="in":
            if item[0:3]!="inn":
                list_inV.append(item)


def rand_list():
        # Die Reihenfolge, in der die items in den Listen vorkommen, randomisieren
           
        random.shuffle(list_imm)
        random.shuffle(list_unn)
        random.shuffle(list_inn)
        random.shuffle(list_base_un)
        random.shuffle(list_base_im)
        random.shuffle(list_base_in)
        random.shuffle(list_imPB)
        random.shuffle(list_unV)
        random.shuffle(list_inV)


        # Now add the desired number of items to each set (unaccented/accented)

        # unaccented items in eine Liste packen: Depending on the category,
        #we need a different number of items:

        #imPB:14
        #unV: 13
        #base_un,  unn, imm: 10
        #base_im: 9
        #inV: 3
        # inn:2
        #base_in: 1
        #Total: 72

        return [list_imm, list_unn, list_inn, 
            list_base_un, list_base_im, list_base_in,
            list_imPB, list_unV, list_inV]
        
def make_accented_and_unaccented_list():

        i=0
        
        while i<1:
                unaccented_items.append((list_base_in[i]))
                unaccented_items.append((list_inn[i]))
                unaccented_items.append((list_inV[i]))
                unaccented_items.append((list_base_im[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_base_un[i]))
                unaccented_items.append((list_imm[i]))
                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1

                
                
        while i<2:
                unaccented_items.append((list_inn[i]))
                unaccented_items.append((list_inV[i]))
                unaccented_items.append((list_base_im[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_base_un[i]))
                unaccented_items.append((list_imm[i]))
                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1
        
        while i<3:

                unaccented_items.append((list_inV[i]))
                unaccented_items.append((list_base_im[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_base_un[i]))
                unaccented_items.append((list_imm[i]))
                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1

        while i<9:

                unaccented_items.append((list_base_im[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_base_un[i]))
                unaccented_items.append((list_imm[i]))
                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1

        while i<10:

                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_base_un[i]))
                unaccented_items.append((list_imm[i]))
                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1


        while i<13:

                unaccented_items.append((list_unV[i]))
                unaccented_items.append((list_imPB[i]))
                i=i+1
       
        while i<14:
                unaccented_items.append((list_imPB[i]))
                i=i+1
                
        #print(unaccented_items)
        #print (len(unaccented_items)) 
        # should be 72


        # Now we can go through the list of each category, and add every items
        # which was not in the unaccented items list, to the accented_items list

        # We should get:

        #imPB:14
        #unV: 13
        #base_un, unn: 10
        #imm : 9
        #base_im: 8
        #inV: 3
        #inn, base_in:2
        # Total: 71

        for element in list_unn:
                if element not in unaccented_items:
                        accented_items.append(element)


        for element in list_base_un:
                if element not in unaccented_items:
                        accented_items.append(element)
        
        for element in list_unV:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_base_im:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_imm:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_imPB:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_inn:
                if element not in unaccented_items:
                        accented_items.append(element)


        for element in list_base_in:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_inV:
                if element not in unaccented_items:
                        accented_items.append(element)
                
        #print(accented_items)
        #print(len(accented_items))
        # should be 71


        # we randomize the order of the lists again
        random.shuffle(accented_items)
        random.shuffle(unaccented_items)

        # now we have the two lists we need to form our sentences.
        # one third of the unaccented items, will be embedded in sentence A, one in sentence B, one in sentence C and then saved
        # in a list
        # the same was done for the accented sentences
        # NOTE: we use \\\ because \\ zeigt in Latex, für das diese Sätze erstellt werden, einen Zeilenumbruch

        #print(accented_items)
        return (accented_items, unaccented_items)


def make_sentence():
    
        # in this version, we will always have the same carrier sentence!

        j=0

#        while j<23:
#
#                item_unaccented= unaccented_items [j]
#
#                sentence='''It was JOHN who said ''' + item_unaccented + ''' again, NOT PETER.'''
#                unaccented_items_version_A.append(sentence)
#
#                item_accented= accented_items [j]
#                item_accented=item_accented.upper()
#                
#                sentence= "Denny said " + item_accented + " again."
#                accented_items_version_A.append(sentence)
#
#                j=j+1
#
#        while j<47:
#
#
#                item_unaccented= unaccented_items [j]
#
#                sentence= '''It was BEN who said ''' + item_unaccented + ''' again, NOT ANNA.'''
#                unaccented_items_version_B.append(sentence)
#
#                item_accented= accented_items [j]
#                item_accented=item_accented.upper()
#
#                
#                sentence= "Betsy said " + item_accented + " again."
#                accented_items_version_B.append(sentence)
#
#                j=j+1

        while j<71:
        
                item_unaccented= unaccented_items [j]

                sentence='''It was TOM who said ''' + item_unaccented + ''' again, NOT HENRY.'''
                unaccented_items_version_C.append(sentence)

                item_accented= accented_items [j]
                item_accented=item_accented.upper()

                
                sentence= "Polly said " + item_accented + " again."
                accented_items_version_C.append(sentence)

                j=j+1

        while j<72:
        
                item_unaccented= unaccented_items [j]

                sentence='''It was TOM who said ''' + item_unaccented + ''' again, NOT HENRY.'''
                unaccented_items_version_C.append(sentence)


                j=j+1

        # Diese print-Befehle waren nur zum Überprüfen

        #print(unaccented_items_version_A[0:2])
        #print(len(unaccented_items_version_A))
        #print (accented_items_version_A[0:2])
        #print(len(accented_items_version_A))


        #print(unaccented_items_version_B[0:2])
        #print(len(unaccented_items_version_B))
        #print (accented_items_version_B[0:2])
        #print(len(accented_items_version_B))

        #print(unaccented_items_version_C[0:2])
        #print(len(unaccented_items_version_C))
        #print (accented_items_version_C[0:2])
        #print(len(accented_items_version_C))


        return (unaccented_items_version_A, accented_items_version_A,
                unaccented_items_version_B, accented_items_version_B,
                unaccented_items_version_C, accented_items_version_C)

def all_sentences_in_list():


        # Jetzt bringen wir alle unaccented und alle accented Sätze in eine Liste bringen

        # Zuerst getrennt nach accented und unaccented (viell. braucht man das noch)

        all_unaccented_sentences_A= unaccented_items_version_A +unaccented_items_version_B + unaccented_items_version_C

        all_accented_sentences_A=accented_items_version_A + accented_items_version_B +accented_items_version_C

        # Diese print Befehle sind zum Überprüfen:

        #print(all_unaccented_sentences_A[0:2])
        #print(len(all_unaccented_sentences_A))
        #print(all_accented_sentences_A[0:2])
        #print(len(all_accented_sentences_A))



        

        # Jetzt müssen wir die filler sentences einlesen und in eine Liste bringen.
        # In dieser Version nehmen wir nur einen Teil der Filler sentences:

        myfile = open ("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/Python-Zeug-Auswahl/filler_items/filler_1_able_ory.txt", "r", encoding="utf8")
        fillers= myfile.readlines()
        
        # Jetzt bringen wir noch alles in eine Liste:

        all_sentences_A=all_unaccented_sentences_A + all_accented_sentences_A + fillers
        #print (all_sentences_A)

        # diese Liste muss in eine zufällige Reihenfolge gebracht werden + die Basen dürfen nicht direkt nach dem item kommen
        # Das mit den Basen muss eigentlich noch eingebaut werden, solange dies noch nicht geschhen ist, muss man es einfach
        #nachher per Hand korrigieren !!!!!!!!!!!!!!!!!!!!1

        random.shuffle(all_sentences_A)
        #print (all_sentences_A)


        # Diese print Befehle sind zum Überprüfen:

        #print(all_sentences_A[0:2])
        #print(len(all_sentences_A))

        return (all_sentences_A)

def vice_versa():

        ####################################################################################
        ##### Jetzt machen wir quasi alles nochmal, um ein "umgekehrtes Set zu erstellen####
        ####################################################################################
        # Es ist wichtig, dass alle item sowohl in accented, als auch in unaccented        #
        #position im experiment vorkommen (gleiche Anzahl). Deshalb werden im nächsten     #
        #Schritt alle unaccented items in accented Sätze gepackt und vice versa            #
        #wir bekommen also direkt 2 Versionen des experiments                              #
        ####################################################################################

        # We take start out again with the lists of accented and unaccented items we compiled
        #earlier

        # we randomize the order of the lists again and rename them - the accented
        #items become the unaccented_items_2 and the unaccented items become the
        # accented_items_2 --> thus accented items will be put in unaccented position and vice
        #versa

        #random.shuffle(accented_items)
        #random.shuffle(unaccented_items)

        unaccented_items_2 = accented_items
        random.shuffle(unaccented_items_2)
        
        accented_items_2 = unaccented_items
        random.shuffle(accented_items_2)

        #print(unaccented_items_2)

        # now we have the two lists we need to form our sentences with these new lists:
        
        
        # In this version of the experiment we only want one carrier sentence
        k=0

#        while k<23:
#                
#
#                item_unaccented_2= unaccented_items_2[k]
#                sentence='''It was JOHN who said ''' + item_unaccented_2 + ''' again, NOT PETER.'''
#                unaccented_items_version_2_A.append(sentence)
#
#                item_accented_2= accented_items_2[k]
#                item_accented_2=item_accented_2.upper()
#                
#                sentence= "Mary said " + item_accented_2 + " again."
#                accented_items_version_2_A.append(sentence)
#
#                k=k+1
#               
#        while k<47:
#
#
#                item_unaccented_2= unaccented_items_2[k]
#
#                sentence='''It was BEN who said ''' + item_unaccented_2 + ''' again, NOT ANNA.'''
#                unaccented_items_version_2_B.append(sentence)
#
#                item_accented_2= accented_items_2[k]
#                item_accented_2=item_accented_2.upper()
#
#                
#                sentence= "Betsy said " + item_accented_2 + " again."
#                accented_items_version_2_B.append(sentence)
#
#                k=k+1

        while k<71:
        
                item_unaccented_2= unaccented_items_2[k]

                sentence='''It was JOHN who said ''' + item_unaccented_2 + ''' again, NOT HENRY.'''
                unaccented_items_version_2_C.append(sentence)

                item_accented_2= accented_items_2[k]
                item_accented_2=item_accented_2.upper()

               
                sentence= "Polly said " + item_accented_2 + " again."
                accented_items_version_2_C.append(sentence)

                k=k+1
        
        while k<72:
                
                sentence= "Polly said " + item_accented_2 + " again."
                accented_items_version_2_C.append(sentence)

                k=k+1


        # Diese print-Befehle waren nur zum Überprüfen

        #print(unaccented_items_version_2_A[0:2])
        #print(len(unaccented_items_version_2_A))
        #print (accented_items_version_2_A[0:2])
        #print(len(accented_items_version_2_A))


        #print(unaccented_items_version_2_B[0:2])
        #print(len(unaccented_items_version_2_B))
        #print (accented_items_version_2_B)
        #print(len(accented_items_version_2_B))

        #print(unaccented_items_version_2_C[0:2])
        #print(len(unaccented_items_version_2_C))
        #print (accented_items_version_2_C[0:2])
        #print(len(accented_items_version_2_C))



        # Die Sätze müssen wir wieder in eine Liste bringen

        # Zuerst getrennt nach accented und unaccented (viell. braucht man das noch)

        all_unaccented_sentences_2_A= unaccented_items_version_2_A +unaccented_items_version_2_B + unaccented_items_version_2_C

        all_accented_sentences_2_A= accented_items_version_2_A + accented_items_version_2_B +accented_items_version_2_C


        #print(all_unaccented_sentences_2_A[0:2])
        #print(len(all_unaccented_sentences_2_A))
        #print(all_accented_sentences_2_A[0:2])
        #print(len(all_accented_sentences_2_A))


        # Jetzt müssen wir die filler sentences einlesen und in eine Liste bringen.))   
        # In dieser Version nehmen wir nur einen Teil der fillers
        myfile = open ("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/Python-Zeug-Auswahl/filler_items/filler_1_able_ory.txt", "r", encoding="utf8") 
        fillers= myfile.readlines()  
        
        # dann in eine Liste

        all_sentences_2_A=all_unaccented_sentences_2_A + all_accented_sentences_2_A + fillers


        # diese Liste muss in eine zufällige Reihenfolge gebracht werden +( die Basen dürfen nicht direkt nach dem item kommen)

        random.shuffle(all_sentences_2_A)

        #print-Befehle zum Überprüfen

        #print(all_sentences_2_A[0:2])
        #print(len(all_sentences_2_A))

        return (all_sentences_2_A, unaccented_items_2, accented_items_2)


#########################################################################################################################
#                     Jetzt haben wir zwei Listen mit gleicher Anzahl an unaccanted und accented                       ##
#                     sentences. Die items, die in der einen Liste accented sind, sind in der anderen                  ##
#                     unaccented! Wir müssen sie nur noch in eine ppt bringen. Dafür die folgende Funtion              ##
#########################################################################################################################


def exp_sentence():



        ''' Dies Funktion erstellt zwei txt-Dateien, die jeweils eine Liste mit Sätzen enthält, in der
        die experimental items enthalten sind. 50% der items sind unaccented, 50% accented. Die items, die
        accented in Liste 1 sind, sind unaccented in Liste 2. Die Liste kann so in Latex kopiert werden, um
        eine Präsentation zu erstellen. Zudem sind zu Anfang 2 Beispielsätze eingebaut.
        Durch die Angabe der version, wird der file name definiert, so dass man die Funktio mehrmals durchlaufen
        lassen kann und mehrere Versionen des Experiments speichern kann.'''

        [list_imm, list_unn, list_inn, list_base_un, list_base_im, list_base_in, list_imPB, list_unV, list_inV] = rand_list()
        
        
        (accented_items, unaccented_items) = make_accented_and_unaccented_list()
        
        (unaccented_items_version_A, accented_items_version_A,
                unaccented_items_version_B, accented_items_version_B,
                unaccented_items_version_C, accented_items_version_C) = make_sentence()
        
        all_sentences_A = all_sentences_in_list()
        
        (all_sentences_2_A, unaccented_items_2, accented_items_2) = vice_versa()

        
        n=input("Which version?")


       


        pres1 = open ("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/Python-Zeug-Auswahl/presentationn_exp_part_1.txt", "r", encoding="utf8")
        latex_start= pres1.readlines()

        pres2 = open ("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/Python-Zeug-Auswahl/presentationn_exp_part_2.txt", "r", encoding="utf8")
        latex_end= pres2.readlines()




        
        myfile=open("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/ppt_exp/experiment_un_in_" +n+ "1_"+ today + ".tex","w", encoding="utf8")
        
        for element in latex_start:
                print (element ,file=myfile)
        

        for element in all_sentences_A:
                print ("\\begin{frame} \\begin{center}"  + element+ "\\end{center} \\end{frame}",file=myfile)


        for element in latex_end:
                 print (element ,file=myfile)
                
        myfile.close()
        

        


        
 

        myfile=open("C:/Users/sbenhedia/Documents/Geminates/Experiments/Data/Methodology/Presentation_exp/ppt_exp/experiment_un_in_" +n+ "2_"+ today + ".tex","w", encoding="utf8")

        for element in latex_start:
                    print (element ,file=myfile)
        
        for element in all_sentences_2_A:
                print ("\\begin{frame} \\begin{center}"  + element+ "\\end{center} \\end{frame}",file=myfile)

        for element in latex_end:
                 print (element ,file=myfile)
                    
        myfile.close()



        # creates a csv file with the item´and condition in each experiment

        csvfile = open("list_accented_unaccented_"+n+ "_" +today+".csv", "w", encoding="utf8")


        list_exp_items= csv.writer(csvfile, delimiter='\t', quotechar='|', quoting=csv.QUOTE_MINIMAL)
        list_exp_items.writerow(["experiment_version", "item", "condition"])
        for element in accented_items:
                list_exp_items.writerow( (n +"1" , element, "accented") )
        for element in unaccented_items:
                list_exp_items.writerow( (n +"1" , element, "unaccented") )
        for element in accented_items_2:
                list_exp_items.writerow( (n +"2" , element, "accented") )
        for element in unaccented_items_2:
                list_exp_items.writerow( (n +"2" , element, "unaccented") )
        csvfile.close()

