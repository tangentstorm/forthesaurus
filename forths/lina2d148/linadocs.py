"""
tool to extract content from lina docs.

docs are provided in html, but it doesn't seem to be well-formed,
and everything seems to be laid out according to a line-oriented 
template, so it's easier here to just use regexps.
"""
import re
from collections import defaultdict

class ForthWord(object):
    def __init__(self,word,wordset):
        self.word=word
        self.wordset=wordset
        self.FX = [] # stack effects
        self.DS = [] # description
        self.SA = [] # see also

reH1 = re.compile('^<H1>(.*)</H1>')
reH2 = re.compile('^<H2>(.*)</H2>')
reFX = re.compile('^STACKEFFECT:(.*)')
reDS = re.compile('^DESCRIPTION:')
reGI = re.compile('^<A HREF="#GLOSSARY INDEX">')
reSA = re.compile('^SEE ALSO:(.*)')

mgroup,line=[],''
def m(re):
    """match a regexp"""
    global mgroup,line
    mobj = re.match(line)
    mgroup = mobj.groups if mobj else []
    return bool(mobj is not None)


#-- process each line ----------------------------------------------------------

words=[]
def push(word):
    words.append(word); return word

word = None
wordset,state = '','TOP'
for i,line in enumerate(open('fig86.lina.html')):
    line = line.strip().replace('<P>','')
    if not line: continue
    elif m(reH1): wordset=mgroup(1)
    elif m(reH2): word = push(ForthWord(mgroup(1),wordset))
    elif m(reFX): state, word.FX = 'FX', [mgroup(1)]
    elif m(reDS): state = 'DS'
    elif m(reGI): state = 'GI'
    elif m(reSA): state = 'SA'
    elif word: getattr(word,state).append(line)
    elif state=='TOP': pass
    else: raise Exception('unexpected line %i: %r for word: %r in state: %s'
                          % (i,line,word,state))
