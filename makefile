CC = gcc
CFLAGS = -Wall
HS = stack
HSBUILD = build
HSINSTALL = insall
HSDIR = Conquest-Config-gen

default: wipmap

wipmap: main.o
	$(CC) $(CFLAGS) main.o

main.o: main.c
	$(CC) $(CFLAGS) -c main.c

mapinit.o:  mapinit.c mapinit.h
	$(CC) $(CFLAGS) -c mapinit.c

clean:
	$(RM) wipmap *.o *~

full: default
      config-build

full-install: default
	      config-install

config-build:
	cd $(HSDIR) && $(HS) $(HSBUILD)

config-install:
	cd $(HSDIR) && $(HS) $(HSINSTALL)
