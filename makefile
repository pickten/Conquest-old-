CC = gcc
CFLAGS = -Wall

default: wipmap

wipmap: main.o
	$(CC) $(CFLAGS) main.o

main.o: main.c
	$(CC) $(CFLAGS) -c main.c

mapinit.o:  mapinit.c mapinit.h
	$(CC) $(CFLAGS) -c mapinit.c

clean:
	$(RM) wipmap *.o *~
