use PDL::LiteF;
use PDL::IO::FlexRaw;
use PDL::Config;
use Config;

use Test::More;

use strict;

$|=1;

my $ndata = 10;
my $Verbose = 0;
my $DEBUG = 0;
$PDL::Verbose = 0;
$Verbose |= $PDL::Verbose;

my $exec = $^O =~ /win32/i ? '.exe' : '';
my $null = $^O =~ /win32/i ? ' 2>nul' : ' 2>/dev/null';

BEGIN{

    my $ntests = 29;

    unless ( $PDL::Config{WITH_SLATEC} ) {
        plan skip_all => "Skipped tests as F77 compiler not found";
    } elsif ($Config{archname} =~ /(x86_64|ia64)/) {
        plan skip_all => "Skipped tests for 64 bit architecture: $1";
    } else {
       plan tests => $ntests;
    }

    # Configuration
    # Get ExtUtils::F77 if run in either PDL/t/ or PDL/
    #
    if(-e 'flexraw.t') {
	unshift @INC, '../Lib/Slatec/' if -e 'flexraw.t';
    } elsif(-e 'Changes') {
	unshift @INC, 'Lib/Slatec/' if -e 'Changes';
    } else {
	print "I'm not in PDL now, right? Still trying\n";
    }

    eval " use ExtUtils::F77; ";
    if ( $@ ) {
	for ( 1..$ntests ) { skip( "Skip tests as ExtUtils::F77 not found" ); }
	exit 0;
    }

}

# use ExtUtils::F77;

my $F77;
my $F77flags;

if ($ExtUtils::F77::VERSION > 1.03) {
    $F77 = ExtUtils::F77::compiler();
    $F77flags = ExtUtils::F77::cflags();
} else {
    $F77 = 'f77';
    $F77flags = '';
}

sub tapprox {
    my ($a,$b) = @_;
    my $c = abs($a->clump(-1)-$b->clump(-1));
    my $d = max($c);
    $d < 0.01;
}

sub byte4swap {
    my ($file) = @_;
    my ($ofile) = $file.'~';
    my ($word);

    my $ifh = IO::File->new( "<$file" )
      or die "Can't open $file to read";
    my $ofh = IO::File->new( ">$ofile" )
      or die "Can't open $ofile to write";
    binmode $ifh;
    binmode $ofh;
    while ( !$ifh->eof ) {
	$ifh->read( $word, 4 );
	$word = pack 'c4',reverse unpack 'c4',$word;
	$ofh->print( $word );
    }
    $ofh->close;
    $ifh->close;
    rename $ofile, $file;
}

sub byte8swap {
    my ($file) = @_;
    my ($ofile) = $file.'~';
    my ($word);

    my $ifh = IO::File->new( "<$file" )
      or die "Can't open $file to read";
    my $ofh = IO::File->new( ">$ofile" )
      or die "Can't open $ofile to write";
    binmode $ifh;
    binmode $ofh;
    while ( !$ifh->eof ) {
	$ifh->read( $word, 8 );
	$word = pack 'c8',reverse unpack 'c8',$word;
	$ofh->print( $word );
    }
    $ofh->close;
    $ifh->close;
    rename $ofile, $file;
}

# utility to fold long lines preventing problems with 72
# char limit and long text parameters (e.g. filenames)
sub codefold {
   my $oldcode = shift;
   my $newcode = '';

   eval {
      # to simplify loop processing, introduces dependence
      require IO::String;

      my $in = IO::String->new($oldcode);
      my $out = IO::String->new($newcode);

      # find non-comment lines longer than 72 columns and fold
      my $line = '';
      while ($line = <$in>) {

          # clean off line-feed stuff
         chomp $line;

         # pass comments to output
         print $out "$line\n" if $line =~ /^\S/;

         # output code lines (by 72-char chunks if needed)
         while ($line ne '') {

            # output first 72 columns of the line
            print $out substr($line,0,72) . "\n";	# print first 72 cols

               if ( length($line) > 72 ) {
                  # make continuation line of the rest of the line
                  substr($line,0,72) = '     $';
               } else {
                  $line = '';
               }

         }
      }

      # close "files" and return folded code
      close($in);
      close($out);
   };

   $newcode = $oldcode if $@;
   return $newcode;
}

sub inpath {
  my ($prog) = @_;
  my $pathsep = $^O =~ /win32/i ? ';' : ':';
  my $exe = $^O =~ /win32/i ? '.exe' : '';
  for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe"}
  return 0;
}

# createData $head, $code
#
# given a F77 program (in $code), compile and run it.
# It is expected to create a data file called
# "${head}data"
# The executable and code are cleaned up but NOT the
# data file.
#
# Requires the global variables
#   $F77
#   $F77flags
#   $Verbose
#   $DEBUG
#
sub createData {
    my $head = shift;
    my $code = shift;

    # try and provide a modicum of safety, since we call
    # system with $head as the argument
    #
    if($^O =~ /mswin32/i) {
      die '$head [' . $head . '] should match /^[A-Z]:\\/'
            unless $head =~ /^[A-Z]:\\/;
      }      
    else {
      die '$head [' . $head . '] must start with a / or ./'
            unless $head =~ /^(\/|\.\/)/;
      }

    my $file = "${head}.f";
    my $prog = $head;
    my $data = "${head}data";

    my $fh = IO::File->new( "> $file" )
      or die "ERROR: Unable to write F77 code to $file\n";
    $fh->print( $code );
    $fh->close;

    system("$F77 $F77flags -o $prog$exec $file".
	     (($Verbose || $DEBUG)?'': $null));
    
    unlink $data if -f $data;
    system( $prog );

    die "ERROR: code did not create data file $data\n"
      unless -e $data;

    unlink $prog.$exec, $file;

} # sub: createData()

# Types to test the translation for, perl + f77 forms
my %types = ( 'float' => 'real*4', 'double' => 'real*8', 'long' => 'integer*4',
	      'short' => 'integer*2', 'byte' => 'character' );

# Perl and f77 functions should be have the same net effect...
my $exprf = '100.*sin(0.01* i)';
my $exprp = '100.*sin(0.01*$i)';
#$exprf = 'i';
#$exprp = '$i';

# Two dimensional functions
my $expr2f = '100.*sin(0.01* i)*cos(0.01* j)';
# no output autocreation means have this mess...
my $expr2p = '(outer(sin(0.01*$i),cos(0.01*$j),$c=null),$c*100.)';

# need to define/declare variables used in the expressions above
my $j = sequence($ndata)+1;
my $i = $j;
my $c;

my $tmpdir = $PDL::Config{TEMPDIR};
$tmpdir =~ s/\\/\\\\/g;
my $head;
if($^O =~ /mswin32/i) {$head   = $tmpdir . "\\\\tmpraw"}
else {$head   = $tmpdir . "/tmpraw"}
my $data   = $head . "data";
my $hdr    = $data . ".hdr";

# 1 dimensional --

#
# f77, implied & explicit swapping for 4 byte types, with 2 separate
# writes; and header array as well as header file
#
foreach my $pdltype ('float', 'long') {
    print STDERR "Type $pdltype swapped\n" if $Verbose;
    my $f77type = $types{$pdltype};
    my $val = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';

    my $code = <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
     
      open(8,file=
     \$'$data'
     \$,status='new',form='unformatted')
      i = $ndata
      write (8) i
      write (8) a
      close(8)
      end

EOT

    createData $head, codefold($code);
    byte4swap($data);
    open(FILE, "> $hdr");
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 1 $ndata
EOT
    close(FILE);
	
    my @a = readflex($data);
    # print "@a\n";
    my $ok = ($a[0]->at(0) == $ndata);
    my $res = eval "$pdltype $exprp";
    ok( $ok && tapprox($res,$a[1]) );

    open(FILE,">$hdr");
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

    @a = readflex($data);
    #print "@a\n";

    unlink $hdr;

    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok( $ok && tapprox($res,$a[1]) );

# Now try header array
    $ok = 1;
    my $header = [ {Type => 'f77'},
		   {Type => 'long', NDims => 1, Dims => [ 1 ] },
		   {Type => $pdltype, NDims => 1, Dims => [ $ndata ] } ];
    @a = readflex($data,$header);
    unlink $data;
    $ok = ($a[0]->at(0) == $ndata);
    $res = eval "$pdltype $exprp";
    ok( $ok && tapprox($res,$a[1]) );
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";

} # foreach: $pdltype == 'float', 'double'

# 1d, all types, normal way round, f77 specifier
foreach my $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    my $f77type = $types{$pdltype};
    my $val = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';

    my $code = <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
      open(8,file='$data'
     \$,status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT

    createData $head, codefold($code);

    open(FILE, ">$hdr" );
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 1 $ndata
EOT
    close(FILE);

    my @a = readflex($data);
    # print "@a\n";
    unlink $data, $hdr;

    my $ok = ($a[0]->at(0) == $ndata);
    my $res = eval "$pdltype $exprp";
    ok($ok && tapprox($res,$a[1]));
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";

} # foreach: $pdltype ( keys %types )

# 1 dimensional, no f77 specifier (format words explicitly ignored)
foreach my $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    my $f77type = $types{$pdltype};
    my $val = $exprf;
    $val = "char(int($val))" if $pdltype eq 'byte';

    my $code = <<"EOT";

c Program to test i/o of F77 unformatted files
      program rawtest
      implicit none
      integer i
      $f77type a($ndata)
      do i = 1, $ndata
        a(i) = $val
      enddo
      open(8,file='$data'
     \$,status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT

    createData $head, codefold($code);

    open(FILE,">$hdr");
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

    my @a = readflex($data);
    # print "@a\n";
    unlink $data, $hdr;

    my $ok = ($a[1]->at(0) == $ndata);
    my $res = eval "$pdltype $exprp";
    ok( $ok && tapprox($res,$a[2]));
    # print $a[2]->getndims()," [",$a[2]->dims,"]\n";
}

# 2 dimensional
foreach my $pdltype (keys %types) {
    print STDERR "Type $pdltype\n" if $Verbose;
    my $f77type = $types{$pdltype};
    my $val = $expr2f;
    $val = "char(int($val))" if $pdltype eq 'byte';

    my $code = <<"EOT";

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
      open(8,file='$data'
     \$,status='new',form='unformatted')
      i = $ndata
      write (8) i,a
      close(8)
      end

EOT

    createData $head, codefold($code);

    open(FILE,">$hdr");
    print FILE <<"EOT";
# FlexRaw file header
f77
long 1 1
# Data
$pdltype 2 $ndata $ndata
EOT
    close(FILE);
    my @a = readflex($data);
#    if ($pdltype eq 'byte') {
#	print "$pdltype @a\n";
#	system("ls -l $data");
#    }
    unlink $data, $hdr;

    my $ok = ($a[0]->at(0) == $ndata);
    my $res = eval "$pdltype $expr2p";
    ok( $ok && tapprox($res,$a[1]));
    # print $a[1]->getndims()," [",$a[1]->dims,"]\n";
}

print STDERR "Combined types case\n" if $Verbose;

my $code = <<"EOT";

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
      open(8,file='$data'
     \$,status='new',form='unformatted')
c Choose bad boundaries...
      write (8) a,i,l,f,d
      close(8)
      end

EOT

    createData $head, codefold($code);

open(FILE,">$hdr");
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

my @a = readflex($data);
#print "@a\n";
shift @a;

my $d = double pdl (4*atan2(1,1));
my $f = float ($d);
my $l = long (10**$f);
$i = short ($l);
my $a = byte (32);
my @req = ($a,$i,$l,$f,$d);
my $ok = 1;
foreach (@req) {
    my $h = shift @a;
    $ok &&= tapprox($_,$h);
}
ok( $ok );

SKIP: {
   my $compress = inpath('compress') ? 'compress' : 'gzip'; # some linuxes don't have compress
   $compress = 'gzip' if $^O eq 'cygwin';                   # fix bogus compress script prob

   if ( $^O eq 'MSWin32' ) {    # fix for ASPerl + MinGW, needs to be more general
      skip "No compress or gzip command on MSWin32", 1 unless inpath($compress) and $^O;
   }

# Try compressed data
   $ok = 1;
   system "$compress -c $data > ${data}.Z";
   unlink( $data );
   @a = readflex($data);
   $ok &&= $#a==6;
   @a = readflex("${data}.Z");
   $ok &&= $#a==6;
   system "gunzip -q ${data}.Z";
   system "gzip -q $data";
   @a = readflex($data);
   $ok &&= $#a==6;
   @a = readflex("${data}.gz");
   $ok &&= $#a==6;
   shift @a;
   unlink "${data}.gz", $hdr;
   $d = double pdl (4*atan2(1,1));
   $f = float ($d);
   $l = long (10**$f);
   $i = short ($l);
   $a = byte (32);
   @req = ($a,$i,$l,$f,$d);
   foreach (@req) {
      my $h = shift @a;
      $ok &&= tapprox($_,$h);
   }
   ok( $ok );
}

# Try writing data
my $flexhdr = writeflex($data,@req);
writeflexhdr($data,$flexhdr);
@a = readflex($data);
unlink $hdr;
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok( $ok);
@a = readflex($data, $flexhdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok( $ok);
unlink $data;

$#a = -1;
foreach (@req) {
	push @a,$_->dummy(0,10);
}
$flexhdr = writeflex($data,@a);
$flexhdr = [ {Type => 'byte',   NDims => 1, Dims => 10},
	     {Type => 'short',  NDims => 1, Dims => 10},
	     {Type => 'long',   NDims => 1, Dims => 10},
	     {Type => 'float',  NDims => 1, Dims => 10},
	     {Type => 'double', NDims => 1, Dims => 10} ];
@a = readflex($data, $flexhdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,slice(shift @a,"(0)"));
}
ok( $ok);
unlink $data;

# Writing multidimensional data
map {$_ = $_->dummy(0,10)} @req;
$flexhdr = writeflex($data,@req);
writeflexhdr($data,$flexhdr);
@a = readflex($data);
unlink $data, $hdr;
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
ok( $ok);

# Use readflex with an open file handle
@req = (byte(1..3),
        long(5..10),
	float(10..15)->reshape(3,2)/100,
	double(0..99)/1e8);
$flexhdr = writeflex($data, @req);

open(IN, $data);
@a = readflex(\*IN, $flexhdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
close(IN);
unlink $data;
ok( $ok);

# use writeflex with an open file handle
open(OUT, ">$data");
$flexhdr = writeflex(\*OUT, @req);
close(OUT);
@a = readflex($data, $flexhdr);
$ok = 1;
foreach (@req) {
    # print "$_ vs ",@a[0],"\n";
    $ok &&= tapprox($_,shift @a);
}
unlink $data;
ok( $ok);
