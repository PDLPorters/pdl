open(FOO, ">Makefile");
print FOO <<'FOO';

all: complex.o eigen.o matrix.o sslib.o

%.o: %.c Makefile
	cc -c -O3 $<

FOO
