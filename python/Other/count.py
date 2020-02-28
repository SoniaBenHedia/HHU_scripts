outfile= open ("test.txt", "w", encoding="utf8")
print("hallo hallo hello", file=outfile)
print("hey", file=outfile)

outfile.close()

infile=open("test.txt", "r", encoding="utf8")

text= infile.read()

proWort=text.split( )

print ("Der Text hat", len(proWort),"Wörter")

# jetzt types zählen

outfile= open ("test.txt", "w", encoding="utf8")
print("hallo hallo hello", file=outfile)
print("hey", file=outfile)

outfile.close()

infile=open("test.txt", "r", encoding="utf8")

text= infile.read()

proWort=text.split( )

types_in_text=set(proWort)

print ("Der Text hat", len(types_in_text),"verschiedene Wörter")

# Jetzt das gleiche mit einem beliebigen im richtigen directory gespeicherten
# text


def count_tokens():

    text= input ("Gib den Namen des Textes ein!")

    infile=open(text, "r", encoding="utf8")

    text= infile.read()

    proWort=text.split( )

    print ("Der Text hat", len(proWort),"Wörter")

# jetzt types zählen

def count_types():

    text= input ("Gib den Namen des Textes ein!")

    infile=open(text, "r", encoding="utf8")

    text= infile.read()

    proWort=text.split( )

    types_in_text=set(proWort)

    print ("Der Text hat", len(types_in_text),"verschiedene Wörter")

   

