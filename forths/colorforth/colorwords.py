from lxml.html.soupparser import parse
from lxml import etree
try: tree
except: tree=parse(open('EVB001-02b.html'))

words={}
color={}

for bnum, block in enumerate(tree.xpath("//td[@class='cf']")):
    b = bnum + 18
    for node in block.xpath('code'):
        c = color.setdefault(node.attrib.get('class', 0), len(words))
        if node.text:
            for word in node.text.split():
                w = words.setdefault(word, len(words))

print words
print color

