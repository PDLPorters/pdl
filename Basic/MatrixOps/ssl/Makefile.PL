use warnings;
use Config;

if($^O =~ /mswin32/i) {
  $cc = $Config{cc};

  if($cc eq 'cl') {
     $opt = '-O2';
     $out = '/Fo';
     $obj = '.obj';
     }

  else {
     $opt = '-O3';
     $out = '-o';
     $obj = '.o';
     }

  if(system "$cc $out complex$obj -c $opt complex.c") {die "Failed to compile complex.c"}
  if(system "$cc $out eigen$obj -c $opt eigen.c") {die "Failed to compile eigen.c"}
  if(system "$cc $out matrix$obj -c $opt matrix.c") {die "Failed to compile matrix.c"}
  if(system "$cc $out sslib$obj -c $opt sslib.c") {die "Failed to compile sslib.c"}

  print "\nWriting dummy Makefile for PDL::MatrixOps/ssl\n"; 

  open(FOO, ">Makefile");
  print FOO <<'FOO';

fred:
	@echo "****"
	@echo "All MatrixOps/ssl compilation done when Makefile.PL was run"
	@echo "****"

all: fred

clean ::
	-ren Makefile Makefile.old

realclean ::
	del /F /Q Makefile Makefile.old


FOO
}

else {
open(FOO, ">Makefile");
print FOO <<'FOO';

all: complex.o eigen.o matrix.o sslib.o

%.o: %.c Makefile
	cc -c -O3 $<

FOO
}
