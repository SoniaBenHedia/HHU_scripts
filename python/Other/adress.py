import re

def maileingabe(adresse):


    valide_email =re.compile("([A-Za-z0-9\-.]+)\@([A-Za-z0-9\-]+)\.[A-Za-z]{2}")

    m= re.match(valide_email, adresse)
    if m:
                             print("Domain-Name:" , m.group(2))
                             
