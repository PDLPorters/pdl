use warnings;
use Config;


if($^O =~ /mswin32/i) {
  $cc = $Config{cc};

  if($cc eq 'cl') {
     $opt = '-O2';
     $obj = '.obj';
 }
  
  else {
      $opt = '-O3';
      $obj = '.o';
  }
  
  if($Config{make} eq 'nmake') {$line = ".c$obj:\n\t$cc -c $opt \$*.c"}
  else {$line = "\%.o: \%.c Makefile\n\t$cc -c $opt \$<"}
  
  open(FOO, ">Makefile") or die "Can't open ssl/Makefile for writing: $!";
  print FOO "

all: complex$obj eigen$obj matrix$obj sslib$obj

$line

clean ::
	-ren Makefile Makefile.old <NUL
	-del /F /Q complex$obj eigen$obj matrix$obj sslib$obj <NUL

realclean ::
	del /F /Q Makefile Makefile.old complex$obj eigen$obj matrix$obj sslib$obj <NUL

";

close(FOO) or die "Can't close ssl/Makefile after writing: $!";
}


else {

my $ccflags = $Config{cflags};

$ccflags .= " -fPIC" if($ENV{HOSTTYPE} =~ m/x86_64/);

open(FOO, ">Makefile");
print FOO <<"FOO";

all: complex.o eigen.o matrix.o sslib.o

%.o: %.c Makefile
	cc $ccflags -c -O3 \$<

clean:
	-mv Makefile Makefile.old
	rm -f complex.o eigen.o matrix.o sslib.o

realclean:
	rm -f Makefile.old Makefile complex.o eigen.o matrix.o sslib.o

FOO
}
