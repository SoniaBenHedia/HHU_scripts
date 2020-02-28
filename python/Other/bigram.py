#bigrams

def make_bigrams(x):

    '''(str)->list
    returns a list of bigrams (2 graphemes) of x
    
    >>'''

    list_bigrams=[]

    

    while len(x)>1:

        element_bigram_1=x[:2]
        list_bigrams.append(element_bigram_1)



        element_bigram_2=x[1:3]

        list_bigrams.append(element_bigram_2)

            #print(element_bigram_1)
            #print(element_bigram_2)



            
        x= x[2:]

            #print(x)


    print(list_bigrams)





        
