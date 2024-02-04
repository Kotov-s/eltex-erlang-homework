LIBS=-lortp -lpthread -lrt

all: compile

compile:
	gcc -o ../webRTP/voice_client rtpsend.c $(LIBS)

clean:
	rm  ../webRTP/voice_client