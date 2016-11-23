CC = gcc
CFLAGS = -Wall

all:	    wipmap

wipmap:	    mapinit.o
	$(CC) $(CFLAGS) mapinit.o

mapinit.o:  mapinit.c mapinit.h
	$(CC) $(CFLAGS) -c mapinit.c

clean:
	$(RM) wipmap *.o *~
