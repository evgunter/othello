HC         = ghc
# STATIC_ARGS = -static -optl-static -optc-static -optl-pthread
STATIC_ARGS =
HFLAGS     = -O2 -Wall $(STATIC_ARGS) --make
HMAKEEXEC  = $(HC) -o $@ $(HFLAGS)
PLAYERNAME  = player

CC          = g++
CFLAGS      = -Wall -ansi -pedantic -ggdb

all: $(PLAYERNAME) testgame testminimax

$(PLAYERNAME): Othello.hs
	$(HMAKEEXEC) Othello.hs

testminimax:
	$(HMAKEEXEC) TestMiniMax.hs

testgame: testgame.o
	$(CC) -o $@ $^

%.o: %.cpp
	$(CC) -c $(CFLAGS) -x c++ $< -o $@

java:
	make -C java/

cleanjava:
	make -C java/ clean

clean:
	rm -f *.o $(PLAYERNAME) testgame testminimax

.PHONY: java
