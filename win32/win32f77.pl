package F77Conf;
# a minimal hardcoded config designed for win32
# using f2c
use Config;

BEGIN { $F77Conf::done = 0 }

print "Config   ",__PACKAGE__->config(),"\n";
print "Compiler ",__PACKAGE__->compiler(),"\n";
print "Runtime  ",__PACKAGE__->runtime(),"\n";
print "Trail_   ",__PACKAGE__->trail_() ? "yes" : "no", "\n";
print "Cflags   ",__PACKAGE__->cflags(),"\n";

$F77Conf::libs = "C:\\temp\\f2c\\libf77.lib C:\\temp\\f2c\\libi77.lib";
# include path and f2c location are buried in the __DATA__ section

__PACKAGE__->mkcompiler;

sub config {
  return 'win32_f2c';
}

# change location of f2c libs to match your installation
sub runtime {
  $F77Conf::libs;
}

sub trail_ {
  return 1;
}

sub compiler {
  $myf77 = &mkcompiler;
  return "$Config{perl} $myf77";
}

sub cflags {
  return '';
}

sub testcompiler {
  my ($this) = @_;
  return 1;  # for the moment bypass this
    my $file = "/tmp/testf77$$";
    my $ret;
    open(OUT,">$file.f");
    print OUT "      print *, 'Hello World'\n";
    print OUT "      end\n";
    close(OUT);
    print "Compiling the test Fortran program...\n";
    my ($compiler,$cflags) = ($this->compiler,$this->cflags);
    system "$compiler $cflags $file.f -o ${file}_exe";
    print "Executing the test program...\n";
    if (`${file}_exe` ne " Hello World\n") {
       print "Test of Fortran Compiler FAILED. \n";
       print "Do not know how to compile Fortran on your system\n";
       $ret=0;
    }
    else{
       print "Congratulations you seem to have a working f77!\n";
       $ret=1;
    }
    unlink("${file}_exe"); unlink("$file.f"); unlink("$file.o") if -e "$file.o";
    return $ret;
}

sub tmpdir {
  use Cwd;
  my $dir = exists $ENV{TEMP} ? $ENV{TEMP} : exists $ENV{TMP} ? $ENV{TMP} :
    cwd; # current working directory as last resort
}

sub mkcompiler {
  my $myf77 = tmpdir().'\myf77';
  unless ($F77Conf::done) {
    open my $fi, ">$myf77" or
      die "couldn't open $myf77";
    use Config;
    
    print $fi "$Config{startperl}\n";
    print $fi join('',<DATA>);
    close $fi;
  }
  $F77Conf::done = 1;
  return $myf77;
}

1;


__DATA__

use Getopt::Std;
use File::Basename;

getopts('co:');
$cflags = '/nologo /MD /W3 /GX /O2 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /YX /c /I"c:/temp/f2c"'; # this must include the include path for your f2c.h !

$out = '';
$out = $opt_o if defined $opt_o;

$fort = $ARGV[0];
$fort =~ s|/|\\|g;

$out =~ s|/|\\|g;
$out = "/Fo\"$out\"";

$c = $fort;
$c =~ s/\.f$/.c/;
$cdir = '-d' . dirname $c;
$obj = $fort;
$obj =~ s/\.f$/.obj/;
$obj = $opt_o if defined $opt_o;
$f2c = 'c:\\temp\\f2c\\f2c';
print "$f2c $cdir $fort\n";
system "$f2c $cdir $fort";
die "error during f2c execution, no $c\n" unless -f $c;

# now compile
print "cl.exe $out $cflags $c\n";
system "cl.exe $out $cflags $c";
die "error during cl execution\n" unless -f $obj;
