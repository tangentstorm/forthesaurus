NB. browser for lina forth blocks in j.
NB. a public domain app by tangentstorm oct 21 2014.
NB. get j at: http://jsoftware.com/stable.htm
NB.
NB. This should run in a terminal because of the way it
NB. clears the screen.

help =: (0 : 0)

forth block browser
------------------------------
to browse, use e.g.:  <:go  >:go  15:go  (3-])go  ...
  first empty block:  {.I. blank
first lines (index):  index
  show this message:  help
   clear the screen:  clear

)

bytes =: (1!:1)<'BLOCKS.BLK'
count =: (#bytes) % */ size =: 16 64
'scr clear blocks'=:0;(shell'clear');(count,size)$bytes
go=: 1 :'blocks {~ scr =: u scr [ echo clear'

blank =: (16 64$' ')-:"2 blocks
bnums =: (' ' ,~ ":) S:0   ;/ i.# blocks
index =: (-.blank) # (bnums ,. {."2)  blocks

echo help
