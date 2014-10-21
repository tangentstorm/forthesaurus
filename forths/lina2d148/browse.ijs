NB. browser for lina forth blocks in j.
NB. a public domain app by tangentstorm oct 21 2014.
NB. get j at: http://jsoftware.com/stable.htm
NB.
NB. This should run in a terminal because of the way it
NB. clears the screen.

help =: noun define

forth block browser
------------------------------
to browse, use e.g.:  <:go  >:go  15 go  (3-])go  ...
  first empty block:  {.I. blank
first lines (index):  index
  show this message:  help
   clear the screen:  clear
)

bytes =: (1!:1)<'BLOCKS.BLK'
count =: (#bytes) % */ size =: 16 64
'scr clear blocks'=:0;(shell'clear');(count,size)$bytes

go =: adverb define
  echo clear
  echo '(block ',(":scr =: u"0 scr), ' of 0..',(": <: # blocks),')'
  echo 64$'-'
  echo blocks {~ scr
  echo 64$'-'
)

blank =: (16 64$' ')-:"2 blocks
bnums =: (' ' ,~ ":) S:0   ;/ i.# blocks
index =: (-.blank) # (bnums ,. {."2)  blocks

0: go
echo help
