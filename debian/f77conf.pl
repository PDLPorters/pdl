package F77Conf;
# a minimal hardcoded config designed for debian so that we don't need
# ExtUtils::F77 when building PDL

print "Config   ",__PACKAGE__->config(),"\n";
print "Compiler ",__PACKAGE__->compiler(),"\n";
print "Runtime  ",__PACKAGE__->runtime(),"\n";
print "Trail_   ",__PACKAGE__->trail_() ? "yes" : "no", "\n";
print "Cflags   ",__PACKAGE__->cflags(),"\n";


sub config {
  return 'debian';
}

sub runtime {
  my $libpath = `g77 -print-libgcc-file-name`;
  $libpath =~ s/libgcc[.]a$//;
  chomp $libpath;
  "-L$libpath -L/usr/lib -lg2c -lm -lgcc";
}

sub trail_ {
  return 1;
}

sub compiler {
  return 'g77';
}

sub cflags {
  return '-O';
}

sub testcompiler {
  my ($this) = @_;
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

1;

