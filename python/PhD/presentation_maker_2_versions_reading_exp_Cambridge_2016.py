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
list_unT=[]


list_diss_complex=[]
list_diss_simplex=[]
list_disV=[]
list_disBase=[]


list_lly_simplex=[]
list_Vly=[]
list_lyBase=[]
list_lly_complex=[]
list_lely=[]
list_ally=[]
list_fully=[]


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


        # und für die umgekehrte Variante, d.h. für das Einbedden der unaccented_items_2 und
        # der accented_items_2

unaccented_items_version_2_A=[]
accented_items_version_2_A=[]


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




# Wir lesen die csv ein:

items_exp= open('C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/script_material/final_exp_words.csv', 'rt')


csv.reader(items_exp)

#jede Reihe wird durchlaufen und die items ihren Listen zugewiesen:

for row in items_exp:

        item=row.split(",")[0].lower()
        Environment= row.split(",")[1].lower()
        Orthography= row.split(",")[2].lower()
        ExtraSuffix= row.split(",")[3].strip("\n")


# first un
        if Environment =="n#n":
            list_unn.append(item)

        if Environment =="n#t":
            list_unT.append(item)

# then dis

        if Environment =="#s":
            list_disBase.append(item)

        if Environment =="s#s":
            list_diss_complex.append(item)

        if Environment =="s#v" and Orthography=="s":
                list_disV.append(item)
                
        if Environment =="sv" and Orthography=="ss":
                list_diss_simplex.append(item)
                 
# now ly


        if Environment =="l#":
            list_lyBase.append(item)

        if Environment =="lv" and Orthography=="l":
                list_Vly.append(item)
                
        if  Environment =="lv" and Orthography=="ll":
                list_lly_simplex.append(item)
                 

        if ExtraSuffix=="ful"and Orthography =="ll":
                list_fully.append(item)
                
        if ExtraSuffix=="al" and Orthography =="ll":
                list_ally.append(item)

        if Orthography =="lely":
                list_lely.append(item)

        if ExtraSuffix=="none" and Environment=="l#l" and Orthography =="ll":
                list_lly_complex.append(item)
                 
        


def rand_list():
        # Die Reihenfolge, in der die items in den Listen vorkommen, randomisieren
           
        random.shuffle(list_unn)
        random.shuffle(list_unT)
        random.shuffle(list_diss_complex)
        random.shuffle(list_diss_simplex)
        random.shuffle(list_disBase)
        random.shuffle(list_lly_simplex)
        random.shuffle(list_Vly)
        random.shuffle(list_lyBase)
        random.shuffle(list_lly_complex)
        random.shuffle(list_lely)
        random.shuffle(list_ally)
        random.shuffle(list_fully)



        return [list_unn, list_unT, list_diss_complex, 
            list_diss_simplex, list_disV, list_disBase,
            list_lly_simplex, list_Vly, list_lyBase,list_lly_complex, list_lely,
            list_ally,list_fully ]




       # Now add the desired number of items to each set (unaccented/accented)

        # unaccented items in eine Liste packen: Depending on the category,
        #we need a different number of items:



#list_unn= 11
#list_unT= 10
#
#
#list_diss_complex= 7
#list_diss_simplex= 3
#list_disV= 15
#list_disBase= 5
#
#
#list_lly_simplex= 6
#list_Vly= 15
#list_lyBase= 15
#list_lly_complex= 3
#list_lely= 4
#list_ally= 4
#list_fully= 5
#

# 103





        
def make_accented_and_unaccented_list():
# append list_inC
        i=0
        
        # the number we put in, is the number of items which will be in the list
        
        while i<3:
                unaccented_items.append((list_diss_simplex[i]))
                unaccented_items.append((list_lly_complex[i]))
                unaccented_items.append((list_lely[i]))
                unaccented_items.append((list_ally[i]))
                unaccented_items.append((list_fully[i]))
                unaccented_items.append((list_disBase[i]))
                unaccented_items.append((list_lly_simplex[i]))
                unaccented_items.append((list_diss_complex[i]))
                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1

                

        while i<4:

                unaccented_items.append((list_lely[i]))
                unaccented_items.append((list_ally[i]))
                unaccented_items.append((list_fully[i]))
                unaccented_items.append((list_disBase[i]))
                unaccented_items.append((list_lly_simplex[i]))
                unaccented_items.append((list_diss_complex[i]))
                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1



        while i<5:

                unaccented_items.append((list_fully[i]))
                unaccented_items.append((list_disBase[i]))
                unaccented_items.append((list_lly_simplex[i]))
                unaccented_items.append((list_diss_complex[i]))
                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1



        while i<6:

                unaccented_items.append((list_lly_simplex[i]))
                unaccented_items.append((list_diss_complex[i]))
                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1



        while i<7:

                unaccented_items.append((list_diss_complex[i]))
                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1


        while i<10:

                unaccented_items.append((list_unT[i]))
                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1


        while i<11:

                unaccented_items.append((list_unn[i]))
                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1  
                
        while i<15:

                unaccented_items.append((list_lyBase[i]))
                unaccented_items.append((list_disV[i]))
                unaccented_items.append((list_Vly[i]))
                i=i+1   
                
                

                


        # Now we can go through the list of each category, and add every items
        # which was not in the unaccented items list, to the accented_items list

        # We should get:

#list_unn= 11
#list_unT= 11
#
#
#list_diss_complex= 8
#list_diss_simplex= 2
#list_disV= 15
#list_disBase= 4
#
#
#list_lly_simplex= 5
#list_Vly= 15
#list_lyBase= 16
#list_lly_complex= 2
#list_lely= 4
#list_ally= 5
#list_fully=4
#


 #Total: 102

        for element in list_unn:
                if element not in unaccented_items:
                        accented_items.append(element)


        for element in list_unT:
                if element not in unaccented_items:
                        accented_items.append(element)
        
        for element in list_diss_complex:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_diss_simplex:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_disV:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_disBase:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_lly_simplex:
                if element not in unaccented_items:
                        accented_items.append(element)


        for element in list_Vly:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_lyBase:
                if element not in unaccented_items:
                        accented_items.append(element)
                        
        for element in list_lly_complex:
                if element not in unaccented_items:
                        accented_items.append(element)


        for element in list_lely:
                if element not in unaccented_items:
                        accented_items.append(element)
                        
        for element in list_ally:
                if element not in unaccented_items:
                        accented_items.append(element)

        for element in list_fully:
                if element not in unaccented_items:
                        accented_items.append(element)                        
                    
                

        # we randomize the order of the lists again
        random.shuffle(accented_items)
        random.shuffle(unaccented_items)

        # now we have the two lists we need to form our sentences.

        # NOTE: we use \\\ because \\ zeigt in Latex, für das diese Sätze erstellt werden, einen Zeilenumbruch

        #print(accented_items)
        #print(len(accented_items))
        return (accented_items, unaccented_items)


def make_sentence():
    
            
        for element in unaccented_items:
            
            if element in list_unn or element in list_unT or element in list_disV or element in list_diss_simplex or element in list_diss_complex:
                sentence='''It is JOHN who says ''' + element + ''' again, NOT HENRY.'''
                unaccented_items_version_A.append(sentence)
              
            elif element in list_disBase:
                sentence='''It is JOHN who tells me ''' + element + ''' again, NOT HENRY.'''
                unaccented_items_version_A.append(sentence)
            
            elif element in list_lyBase:
                sentence='''It is JOHN who said ''' + element + ''' into the microphone, NOT HENRY.'''
                unaccented_items_version_A.append(sentence)
        
            else:
                sentence='''It is JOHN who said ''' + element + ''' to the janitor, NOT HENRY.'''
                unaccented_items_version_A.append(sentence)
                
        for element in accented_items:
            
            if element in list_unn or element in list_unT or element in list_disV or element in list_diss_simplex or element in list_diss_complex:
                
                item_accented=element.upper()               
                sentence= "John says " + item_accented + " again."
                accented_items_version_A.append(sentence)
                             
            elif element in list_disBase:
                
                item_accented=element.upper()               
                sentence= "John tells me " + item_accented + " again."
                accented_items_version_A.append(sentence)
            
            elif element in list_lyBase:
                
                item_accented=element.upper()               
                sentence= "John said " + item_accented + " into the microphone."
                accented_items_version_A.append(sentence)
        
            else:                                   
                        item_accented=element.upper()               
                        sentence= "John said " + item_accented + " to the janitor."
                        accented_items_version_A.append(sentence) 
                
        # Diese print-Befehle waren nur zum Überprüfen


        #print(unaccented_items_version_C[0:2])
        #print(len(unaccented_items_version_C))
        #print (accented_items_version_C[0:2])
        #print(len(accented_items_version_C))


        return (unaccented_items_version_A, accented_items_version_A)

def all_sentences_in_list():


        # Jetzt bringen wir alle unaccented und alle accented Sätze in eine Liste bringen

        # Zuerst getrennt nach accented und unaccented (viell. braucht man das noch)

        all_unaccented_sentences_A= unaccented_items_version_A

        all_accented_sentences_A=accented_items_version_A 
        
        # Diese print Befehle sind zum Überprüfen:

        #print(all_unaccented_sentences_A[0:2])
        #print(len(all_unaccented_sentences_A))
        #print(all_accented_sentences_A[0:2])
        #print(len(all_accented_sentences_A))



        

        # Jetzt müssen wir die filler sentences einlesen und in eine Liste bringen.
        # In dieser Version nehmen wir nur einen Teil der Filler sentences:

        myfile = open ("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/script_material/filler_all_new.txt", "r", encoding="utf8")
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
        








        for element in unaccented_items_2:
            
            if element in list_unn or element in list_unT or element in list_disV or element in list_diss_simplex or element in list_diss_complex:
                sentence='''It is JOHN who says ''' + element + ''' again, NOT HENRY.'''
                unaccented_items_version_2_A.append(sentence)
              
            elif element in list_disBase:
                sentence='''It is JOHN who tells me ''' + element + ''' again, NOT HENRY.'''
                unaccented_items_version_2_A.append(sentence)
            
            elif element in list_lyBase:
                sentence='''It is JOHN who said ''' + element + ''' into the microphone, NOT HENRY.'''
                unaccented_items_version_2_A.append(sentence)
        
            else:
                sentence='''It is JOHN who said ''' + element + ''' to the janitor, NOT HENRY.'''
                unaccented_items_version_2_A.append(sentence)
                
        for element in accented_items_2:
            
            if element in list_unn or element in list_unT or element in list_disV or element in list_diss_simplex or element in list_diss_complex:
                
                item_accented=element.upper()               
                sentence= "John says " + item_accented + " again."
                accented_items_version_2_A.append(sentence)
                             
            elif element in list_disBase:
                
                item_accented=element.upper()               
                sentence= "John tells me " + item_accented + " again."
                accented_items_version_2_A.append(sentence)
            
            elif element in list_lyBase:
                
                item_accented=element.upper()               
                sentence= "John said " + item_accented + " into the microphone."
                accented_items_version_2_A.append(sentence)
        
            else:                                   
                        item_accented=element.upper()               
                        sentence= "John said " + item_accented + " to the janitor."
                        accented_items_version_2_A.append(sentence)
                



        # Die Sätze müssen wir wieder in eine Liste bringen

        # Zuerst getrennt nach accented und unaccented (viell. braucht man das noch)

        all_unaccented_sentences_2_A= unaccented_items_version_2_A

        all_accented_sentences_2_A= accented_items_version_2_A


        #print(all_unaccented_sentences_2_A[0:2])
        #print(len(all_unaccented_sentences_2_A))
        #print(all_accented_sentences_2_A[0:2])
        #print(len(all_accented_sentences_2_A))


        # Jetzt müssen wir die filler sentences einlesen und in eine Liste bringen.))   
        # In dieser Version nehmen wir nur einen Teil der fillers
        myfile = open ("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/script_material/filler_all_new.txt", "r", encoding="utf8")
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

        [list_unn, list_unT, list_diss_complex, 
            list_diss_simplex, list_disV, list_disBase,
            list_lly_simplex, list_Vly, list_lyBase,list_lly_complex, list_lely,
            list_ally,list_fully ] = rand_list()
        
        
        (accented_items, unaccented_items) = make_accented_and_unaccented_list()
        
        (unaccented_items_version_A, accented_items_version_A) = make_sentence()
        
        all_sentences_A = all_sentences_in_list()
        
        (all_sentences_2_A, unaccented_items_2, accented_items_2) = vice_versa()
        

        n=input("Which version?")


       


        pres1 = open ("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/script_material/presentationn_exp_part_1.txt", "r", encoding="utf8")
        latex_start= pres1.readlines()

        pres2 = open ("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/script_material/presentationn_exp_part_2.txt", "r", encoding="utf8")
        latex_end= pres2.readlines()




        
        myfile=open("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/Presentations/txt_files/experiment_dis_ly_" +n+ "_A_"+ today + ".tex","w", encoding="utf8")
        
        for element in latex_start:
                print (element ,file=myfile)
        
        
        
        for element in all_sentences_A:
                print ("\\begin{frame} \\begin{center}"  + element+ "\\end{center} \\end{frame}",file=myfile)

     
        for element in latex_end:
                 print (element ,file=myfile)
                
        myfile.close()
        

        


        
 

        myfile=open("C:/Users/sbenhedia/Dropbox/Geminates/Experimente/dis_ly/Experimental design/Making the presentations/Presentations/txt_files/experiment_dis_ly_" +n+ "_B_"+ today + ".tex","w", encoding="utf8")

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
                list_exp_items.writerow( (n +"_A" , element, "accented") )
        for element in unaccented_items:
                list_exp_items.writerow( (n +"_A" , element, "unaccented") )
        for element in accented_items_2:
                list_exp_items.writerow( (n +"_B" , element, "accented") )
        for element in unaccented_items_2:
                list_exp_items.writerow( (n +"_B" , element, "unaccented") )
        csvfile.close()

