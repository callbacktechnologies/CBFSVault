#
# CBFS Vault 2022 .NET Edition - Sample Project
#
# This sample project demonstrates the usage of CBFS Vault in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.callback.com/cbfsvault
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lcbfsvault.22.0 -L../../lib64/ -lresolv -liconv -ldl
LD_FLAGS_SRC = -lz -lresolv -liconv -ldl
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o vaultmgr dirent.cpp stdafx.cpp vaultmgr.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/cbfsvault.o
	g++ $(OS_CFLAGS) -o vaultmgr ../../src/cbfsvault.o dirent.cpp stdafx.cpp vaultmgr.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/cbfsvault.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/cbfsvault.o ../../src/cbfsvault.cpp

else # LINUX
ifeq ($(shell uname -m), x86_64)
  LIB=lib64
else
  LIB=lib
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lcbfsvault -ldl -lpthread -L../../$(LIB)/ -ldl -lpthread
LD_FLAGS_SRC = -lz -ldl -lpthread

all:
	g++ -D UNIX -o vaultmgr dirent.cpp stdafx.cpp vaultmgr.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH) 

src: ../../src/cbfsvault.o
	g++ -D UNIX -o vaultmgr ../../src/cbfsvault.o dirent.cpp stdafx.cpp vaultmgr.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/cbfsvault.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/cbfsvault.o ../../src/cbfsvault.cpp

clean:
	rm -f ../../src/*.o
	rm -f vaultmgr *.o
endif