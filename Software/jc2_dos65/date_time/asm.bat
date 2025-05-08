mads date.asm -d:USE_XMODEM=1 -l:date.lst -o:dateX.bin -t:date.txt
mads date.asm -d:USE_XMODEM=0 -o:date.com
mads time.asm -d:USE_XMODEM=1 -l:time.lst -o:timeX.bin -t:time.txt
mads time.asm -d:USE_XMODEM=0 -o:time.com

