use PDL::LiteF;
use PDL::IO::FlexRaw;

$ndata = 10;
$Verbose = 0;
$DEBUG = 0;
$PDL::Verbose = 0;
$Verbose |= $PDL::Verbose;

$ntests = 29;
print "1..$ntests\n";
BEGIN{
	$|=1;
	use PDL::Config;
	$compiler_available = $PDL::Config{WITH_SLATEC};

	if(!$compiler_available) {
		print STDERR "F77 compiler not found - skipping tests\n";
		print "1..0\n";
		print "ok 0\n";
		exit 0;
	}
}

# Configuration
# Get ExtUtils::F77 if run in either PDL/t/ or PDL/
BEGIN{
	if(-e 'flexraw.t') {
		unshift @INC, '../Lib/Slatec/' if -e 'flexraw.t';
	} elsif(-e 'Changes') {
		unshift @INC, 'Lib/Slatec/' if -e 'Changes';
	} else {
		print "I'm not in PDL now, right? Still trying\n";
	}
}

BEGIN {
	eval " use ExtUtils::F77; ";
	$loaded = ($@ ? 0 : 1);
}

if(!$loaded) {
	print STDERR "Can't find ExtUtils::F77 installed - skipping tests\n";
	for(1..$ntests) {print "ok $_\n";}
	exit 0;
}


# use ExtUtils::F77;

if(0) {
print STDERR "Testing F77 compiler: (garbage may result)\n--------";
$compiler_available = ExtUtils::F77->testcompiler;
print STDERR "--------\n";
}

if ($ExtUtils::F77::VERSION > 1.03) {
    $F77 = ExtUtils::F77::compiler();
    $F77flags = ExtUtils::F77::cflags();
} else {
    $F77 = 'f77';
    $F77flags = '';
}

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a->clump(-1)-$b->clump(-1));
	$d = max($c);
	$d < 0.01;
}

sub byte4swap {
    my ($file) = @_;
    my ($ofile) = $file.'~';
    my ($word);
    open (IN, "<$file") or die "Can't open $file to read";
    open (OUT, ">$ofile") or die "Can't open $file to write";
    binmode(IN);
    binmode(OUT);
    while (!eof(IN)) {
	read (IN, $word,4);
	$word = pack 'c4',reverse unpack 'c4',$word;
	print OUT $word;
    }
    close (OUT);
    close (IN);
    rename $ofile, $file;
}

sub inpath {
  my ($prog) = @_;
  my $pathsep = $^O =~ /win32/i ? ';' : ':';
  my $exe = $^O =~ /win32/i ? '.exe' : '';
  for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe"}
  return 0;
}


# Types to test the translation for, perl + f77 forms
%types = ( 'float' => 'real*4', 'double' => 'real*8', 'long' => 'integer*4',
	   'short' => 'integer*2', 'byte' => 'character' );

# Perl and f77 functions should be have the same net effect...
$exprf = '100.*sin(0.01* i)';
$exprp = '100.*sin(0.01*$i)';
#$exprf = 'i';
#$exprp = '$i';

# Two dimensional functions
$expr2f = '100.*sin(0.01* i)*cos(0.01* j)';
# no output autocreation means have this mess...
$expr2p = '(outer(sin(0.01*$i),cos(0.01*$j),$c=null),$c*100.)';

$j = sequence($ndata)+1;
$i = $j;
$testno = 1;

# 1 dimensional --

#
# f77, implied & explicit swapping for 4 byte types, with 2 separate
# writes; and header array as well as header file
#
foreach $pdltype ('float', 'long') {
    print STDERR "Type $pdltype swapped\n" if $Verbose;
    $f77type = $types{$pdltype};
    my($val) = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';
    open(FILE,'>tmprawtest.f');
    print FILE <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
      open(8,file='tmprawdata',status='new',form='unformatted')
      i = $ndata
      write (8) i
      write (8) a
      close(8)
      end

EOT
    close(FILE);

    system("$F77 $F77flags -o tmprawtest tmprawtest.f".
	   (($Verbose || $DEBUG)?'':' 2>/dev/null'));
    unlink 'tmprawdata' if -f 'tmprawdata';
    system('./tmprawtest');

    open(FILE,'>tmprawdata.hdr');
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 1 $ndata
EOT
    close(FILE);
    byte4swap('tmprawdata');
    @a = readflex('tmprawdata');
    # print "@a\n";
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok($testno++,$ok && tapprox($res,$a[1]));
    open(FILE,'>tmprawdata.hdr');
    print FILE <<"EOT";
# FlexRaw file header
swap
f77
# now for data specifiers
long 1 1
# Data
$pdltype 1 $ndata
EOT
    close(FILE);
    @a = readflex('tmprawdata');
    #print "@a\n";
    unlink 'tmprawdata.hdr', 'tmprawtest', 'tmprawtest.f';
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok($testno++,$ok && tapprox($res,$a[1]));

# Now try header array
    $ok = 1;
    $header = [ {Type => 'f77'},
	       {Type => 'long', NDims => 1, Dims => [ 1 ] },
	       {Type => $pdltype, NDims => 1, Dims => [ $ndata ] } ];
    @a = readflex('tmprawdata',$header);
    unlink 'tmprawdata';
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok($testno++,$ok && tapprox($res,$a[1]));
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";
}

# 1d, all types, normal way round, f77 specifier
foreach $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    $f77type = $types{$pdltype};
    my($val) = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';
    open(FILE,'>tmprawtest.f');
    print FILE <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
      open(8,file='tmprawdata',status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT
    close(FILE);

    print "$F77 $F77flags -o tmprawtest tmprawtest.f\n";
    system("$F77 $F77flags -o tmprawtest tmprawtest.f".
	   (($Verbose || $DEBUG)?'':' 2>/dev/null'));
    unlink 'tmprawdata' if -f 'tmprawdata';
    system('./tmprawtest');

    open(FILE,'>tmprawdata.hdr');
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 1 $ndata
EOT
    close(FILE);
    @a = readflex('tmprawdata');
    # print "@a\n";
    unlink 'tmprawdata', 'tmprawdata.hdr', 'tmprawtest', 'tmprawtest.f';
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok($testno++,$ok && tapprox($res,$a[1]));
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";
}

# 1 dimensional, no f77 specifier (format words explicitly ignored)
foreach $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    $f77type = $types{$pdltype};
    my($val) = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';
    open(FILE,'>tmprawtest.f');
    print FILE <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
      open(8,file='tmprawdata',status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT
    close(FILE);

    system("$F77 $F77flags -o tmprawtest tmprawtest.f".
	   (($Verbose || $DEBUG)?'':' 2>/dev/null'));
    unlink 'tmprawdata' if -f 'tmprawdata';
    system('./tmprawtest');

    open(FILE,'>tmprawdata.hdr');
    print FILE <<"EOT";
# FlexRaw header file
byte 1 4
long 1 # Test comments
1      Tricky comment
# Data
$pdltype 1 $ndata
byte 1 4
# and hanging EOF


EOT
    close(FILE);
    @a = readflex('tmprawdata');
    # print "@a\n";
    unlink 'tmprawdata', 'tmprawdata.hdr', 'tmprawtest', 'tmprawtest.f';
    $ok = ($a[1]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok($testno++,$ok && tapprox($res,$a[2]));
    # print $a[2]->getndims()," [",$a[2]->dims,"]\n";
}

# 2 dimensional
foreach $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    $f77type = $types{$pdltype};
    my($val) = $expr2f;
    $val = "char(int($val))" if $pdltype eq 'byte';
    open(FILE,'>tmprawtest.f');
    print FILE <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i, j
      $f77type a($ndata, $ndata)
      do i = 1, $ndata
        do j = 1, $ndata
          a(i,j) = $val
        enddo
      enddo
      open(8,file='tmprawdata',status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT
    close(FILE);

    system("$F77 $F77flags -o tmprawtest tmprawtest.f".
	   (($Verbose || $DEBUG)?'':' 2>/dev/null'));
    unlink 'tmprawdata' if -f 'tmprawdata';
    system('./tmprawtest');

    open(FILE,'>tmprawdata.hdr');
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 2 $ndata $ndata
EOT
    close(FILE);
    @a = readflex('tmprawdata');
#    if ($pdltype eq 'byte') {
#	print "$pdltype @a\n";
#	system('ls -l tmprawdata');
#    }
    unlink 'tmprawdata', 'tmprawdata.hdr', 'tmprawtest', 'tmprawtest.f';
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $expr2p";
    ok($testno++,$ok && tapprox($res,$a[1]));
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";
}

print STDERR "Combined types case\n" if $Verbose;

open(FILE,'>tmprawtest.f');
print FILE <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      character a
      integer*2 i
      integer*4 l
      real*4    f
      real*8    d
      d = 4*atan(1.)
      f = d
      l = 10**d
      i = l
      a = ' '
      open(8,file='tmprawdata',status='new',form='unformatted')
c Choose bad boundaries...
      write (8) a,i,l,f,d
      close(8)
      end

EOT
close(FILE);

system("$F77 $F77flags -o tmprawtest tmprawtest.f".
	(($Verbose || $DEBUG)?'':' 2>/dev/null'));
unlink 'tmprawdata' if -f 'tmprawdata';
system('./tmprawtest');

open(FILE,'>tmprawdata.hdr');
print FILE <<"EOT";
# FlexRaw file header
byte 1 4
byte 0
short 0
long 0
float 0
double 0
byte 1 4
EOT
close(FILE);
@a = readflex('tmprawdata');
#print "@a\n";
shift @a;
$d = double pdl (4*atan2(1,1));
$f = float ($d);
$l = long (10**$f);
$i = short ($l);
$a = byte (32);
@req = ($a,$i,$l,$f,$d);
$ok = 1;
foreach (@req) {
    $h = shift @a;
    $ok &&= tapprox($_,$h);
}
ok($testno++,$ok);

$compress = inpath('compress') ? 'compress' : 'gzip'; # some linuxes
# don't have compress

# Try compressed data
$ok = 1;
system "$compress -c tmprawdata > tmprawdata.Z"; unlink("tmprawdata");
@a = readflex('tmprawdata');
$ok &&= $#a==6;
@a = readflex('tmprawdata.Z');
$ok &&= $#a==6;
system 'gunzip -q tmprawdata.Z';
system 'gzip -q tmprawdata';
@a = readflex('tmprawdata');
$ok &&= $#a==6;
@a = readflex('tmprawdata.gz');
$ok &&= $#a==6;
shift @a;
unlink 'tmprawdata.gz', 'tmprawdata.hdr', 'tmprawtest', 'tmprawtest.f';
$d = double pdl (4*atan2(1,1));
$f = float ($d);
$l = long (10**$f);
$i = short ($l);
$a = byte (32);
@req = ($a,$i,$l,$f,$d);
foreach (@req) {
    $ok &&= tapprox($_,$h = shift @a);
}
ok($testno++,$ok);

# Try writing data
$hdr = writeflex('tmprawdata',@req);
writeflexhdr('tmprawdata',$hdr);
@a = readflex('tmprawdata');
unlink 'tmprawdata.hdr';
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok($testno++,$ok);
@a = readflex('tmprawdata', $hdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok($testno++,$ok);
unlink 'tmprawdata';

$#a = -1;
foreach (@req) {
	push @a,$_->dummy(0,10);
}
$hdr = writeflex('tmprawdata',@a);
$hdr = [ {Type => 'byte',   NDims => 1, Dims => 10},
	 {Type => 'short',  NDims => 1, Dims => 10},
	 {Type => 'long',   NDims => 1, Dims => 10},
	 {Type => 'float',  NDims => 1, Dims => 10},
	 {Type => 'double', NDims => 1, Dims => 10} ];
@a = readflex('tmprawdata', $hdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,slice(shift @a,"(0)"));
}
ok($testno++,$ok);
unlink 'tmprawdata';

# Writing multidimensional data
map {$_ = $_->dummy(0,10)} @req;
$hdr = writeflex('tmprawdata',@req);
writeflexhdr('tmprawdata',$hdr);
@a = readflex('tmprawdata');
unlink 'tmprawdata', 'tmprawdata.hdr';
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok($testno++,$ok);

# Use readflex with an open file handle
@req = (byte(1..3),
        long(5..10),
	float(10..15)->reshape(3,2)/100,
	double(0..99)/1e8);
$hdr = writeflex('tmprawdata', @req);
open(IN, 'tmprawdata');
@a = readflex(\*IN, $hdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
close(IN);
unlink 'tmprawdata';
ok($testno++,$ok);

# use writeflex with an open file handle
open(OUT, ">tmprawdata");
$hdr = writeflex(\*OUT, @req);
close(OUT);
@a = readflex('tmprawdata', $hdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
unlink 'tmprawdata';
ok($testno++,$ok);

__END__
