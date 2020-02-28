def create_freq_dictionary(file):
    ''' create_freq_dictionary(str)=dict,dict
    This function returns two dictionaries from a file. The first dictionary is 
    lex in which each word is a key, and its frequency is the value. The second 
    dictionary has the frequencies as its keys and a list of words with the pertinent
    frequencues as its values
    
    >>> create_freq_dictionary("minitext_token.txt")
    lex
    lex2'''
    
        # read in file
    infile = open(file, "r")
    
    #create empty dictionaries and lists, as well as counters
    lex = dict()
    lex2 = dict()
    words = []
    i = 0
    j = 0
    
        #clean lines and make list
    for line in infile:

            line = line.strip("\n")
            line = line.strip("\".?!,;()/&%$§1234567890'")
            line = line.lower()
    
            if line == "":
                continue
        
            words = words + [line]
        
        # create the first dictionary with words as keys and frequencies as values    
    while (i < len(words)):
            wort = words[i]
            j = 0
        
            for elem in words:
                if elem == wort:
                    j = j + 1
                else:
                    continue
    
            lex[wort] = j
            i = i + 1
    
    # create second dictionary with frequencies as values and lists of words with the
    # pertinent frequencies as values

    for elem in lex:
            key = lex[elem]
            value = [elem]
    
            if key in lex2:
                lex2[key] = lex2[key] + [elem]
            else:
                lex2[key] = value
                
    return(lex,lex2)


def freq_words(m,l,file):
    '''freq_words(int,int,str)=str
    m= number of most frequent words you want to get
    l= number of least frequent words you want to get
    file= text you want to get the frequencies from

    This program counts the frequency of words in a text t is return the m words with the most
    frequency, and the l words with the least frequency

    >>> fre(1,2,"minitext_token.txt")
    Most frequent: 
    Least frequent:

    ''' 

    lex,lex2= create_freq_dictionary(file)
    list_keys =[]
    for element in lex2.keys():
        list_keys.append(element)
    i=0
#    j = 0
	

    #most frequent
    list_keys = list_keys[::-1]
    while(i<m):
        print(lex2[list_keys[i]],"is the ", str(i+1), "most frequent word.")
        i=i+1

	#seltensten
    #liste = liste[::-1]
    #print("least frequent:")
    #while(j<l):
    #    print(lex2[liste[j]])
     #   j=j+1
