all: serial erlang

serial: serial.c serial.h
	gcc -o serial -ggdb serial.c

erlang: serial.erl zigbee.erl serial.hrl
	erlc +debug_info serial.erl zigbee.erl

clean:
	rm -rf serial serial.dSYM *.beam
