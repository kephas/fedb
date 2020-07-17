#!/usr/bin/env python3
import sys
import os
import re

DATADIR="data"

class NameSurname:
  def __init__(self,s):
    self.name=""
    self.surnames=[]
    bits=s.split()
    for b in bits:
      if (b.isupper()):
        self.name+=b
      else:
        self.surnames.append(b)

  def __str__(self):
    s=self.name
    for sn in self.surnames:
      s+=" "+sn
    return s

def CollectData(n):
  files=os.listdir(DATADIR)
  for fn in files:
    if (re.match("res-.*.html.txt",fn)):
      f=open(os.path.join(DATADIR,fn),"rt")
      for line in f.readlines():
        if (not line.isspace()):
          n.append(NameSurname(line))
 
def main():
  names=[]
  CollectData(names)
  print(len(names))
  namedict={}
  surnamedict={}
  
  for n in names:
    if (not (n.name in namedict)):
      d={}
      for ns in n.surnames:
        d[ns]=1
      namedict[n.name]=d
    else:
      for ns in n.surnames:
        if ns in namedict[n.name]:
          namedict[n.name][ns]=namedict[n.name][ns]+1
        else:
          namedict[n.name][ns]=1
    for ns in n.surnames:
      if (not (ns in surnamedict)):
        d={}
        d[n.name]=1
        surnamedict[ns]=d
      else:
        if (n.name in surnamedict[ns]):
          surnamedict[ns][n.name]=surnamedict[ns][n.name]+1
        else:
          surnamedict[ns][n.name]=1
  
  
# --------------------------------------------------------------------------
if __name__ == '__main__':
  main()


