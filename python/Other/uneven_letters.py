import re

def consonants(x):
    con=re.compile("[b,c,d,e,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]+")
    consonants_in_string=set()
    

    for element in x:
        if re.match(con,element):
            consonants_in_string.add(element)

    return consonants_in_string
        #uneven.append(element[0])
        #print(x[0])
        #x=x[2:]
        
        
   # return uneven
