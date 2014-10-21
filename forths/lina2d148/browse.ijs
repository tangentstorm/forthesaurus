NB. browser for lina forth blocks in j.
NB. (public domain app by tangentstorm oct 21 2014.)
NB.
NB. This needs to run in a terminal because of the way it
NB.  clears the screen.
NB.
NB. browse with  <:go  >:go  15:go  (3-])go  etc

'scr clear blocks'=:0;(shell'clear');(256 16 64 $(1!:1)<'BLOCKS.BLK')
go=: 1 :'blocks {~ scr =: u scr [ echo clear'

