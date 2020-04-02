all: fivetwve.img fivetwve.com

fivetwve.img: fivetwelve.asm
	nasm -f bin -o $@ fivetwelve.asm

fivetwve.com: fivetwelve.asm
	nasm -f bin -o $@ -Dcomfile=1 fivetwelve.asm
