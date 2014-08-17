# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=main
VER=7.6.3
HCFLAGS=-package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d
HC=ghc $(HCFLAGS)

OBJS=mainc.o MainSDL.o

$(PROGNAME): $(OBJS)
	$(HC) -no-hs-main $(OBJS) -o $@ -package SDL -package vector -package JuicyPixels -package old-time

show: main
	./main

mainc.o: mainc.c
	$(HC) -no-hs-main `sdl-config --cflags` -Wall $*.c -c

%.o: %.hs
	$(HC) -c $< -o $@

clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#
