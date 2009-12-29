
use strict;

use Config;
use File::Basename qw(&basename &dirname);
use IO::File;

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
    if ($^O eq 'VMS' or $^O eq 'os2');  # "case-forgiving"

unlink $file if -f $file;
my $fh = new IO::File "> $file" or die "Can't create $file: $!";

# check for bad value support
use vars qw( $bvalflag );
use File::Spec;
require File::Spec->catfile( "Basic", "Core", "badsupport.p" );

my $usage_info;
if ( $bvalflag ) {
    print "Extracting $file (WITH bad value support)\n";
    $usage_info = "[-a] [-b] [-h] [-s] [-u] <string>";
} else {
    print "Extracting $file (NO bad value support)\n";
    $usage_info = "[-a] [-h] [-s] [-u] <string>";
}                                                                               

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print $fh <<"!GROK!THIS!";
$Config{'startperl'}
    eval 'exec perl -S \$0 "\$@"'
        if 0;
!GROK!THIS!

# In the following, perl variables are not expanded during extraction.

print $fh <<'!NO!SUBS!';

use strict;
$|++;

use PDL::Doc::Perldl;
use File::Basename;

use vars qw( $VERSION );
$VERSION = '0.2';

use PDL::Config;
my $bvalflag = $PDL::Config{WITH_BADVAL} || 0;

my %options = 
    ( a => \&apropos, 
!NO!SUBS!

      print $fh '      b => \&badinfo,' . "\n" if $bvalflag;

print $fh <<'!NO!SUBS!';
      h => \&help, s => \&sig, u => \&usage );

my $name = basename( $0 );
my $usage = <<"EOH";
!NO!SUBS!

    print $fh "Usage: \$name $usage_info\n";

print $fh <<'!NO!SUBS!';

This program provides command-line access to the PDL documentation.
If no flag is specified, -h is assumed.

  -a (apropos) searches the documentation for the string
!NO!SUBS!

print $fh "  -b (badinfo) does the function support bad values?\n" 
    if $bvalflag;
    
print $fh <<'!NO!SUBS!';
  -h (help)    prints the help for the function/module/document
  -s (sig)     prints the signature of the function
  -u (usage)   gives usage information on the function

EOH

my $oflag = $#ARGV > -1 ? substr($ARGV[0],0,1) eq "-" : 0;
die $usage unless ($#ARGV == 0 and not $oflag) or ($#ARGV == 1 and $oflag);

my $option = "h";
if ( $oflag ) {
    $option = substr($ARGV[0],1,1);
    die $usage unless exists $options{$option};
    shift @ARGV;
}

&{$options{$option}}( $ARGV[0] );

exit;

__END__

=head1 NAME

pdldoc - shell interface to PDL documentation

=head1 SYNOPSIS

B<pdldoc> <text>

!NO!SUBS!

    if ( $bvalflag ) {
	print $fh <<'!NO!SUBS!';
B<pdldoc> [B<-a>] [B<-b>] [B<-h>] [B<-s>] [B<-u>] <text>
!NO!SUBS!
} else {
	print $fh <<'!NO!SUBS!';
B<pdldoc> [B<-a>] [B<-h>] [B<-s>] [B<-u>] <text>
!NO!SUBS!
}

print $fh <<'!NO!SUBS!';

=head1 DESCRIPTION

The aim of B<pdldoc> is to provide the same functionality
as the C<apropos>, C<help>, C<sig>, 
!NO!SUBS!

    print $fh "C<badinfo>, \n" if $bvalflag;

print $fh <<'!NO!SUBS!';
and C<usage> commands available in the L<perldl|PDL::perldl> shell.

Think of it as the PDL equivalent of C<perldoc -f>.

=head1 OPTIONS

=over 5

=item B<-h> help

print documentation about a PDL function or module or show a PDL manual.
This is the default option.

=item B<-a> apropos

Regex search PDL documentation database.

!NO!SUBS!

print $fh <<'!NO!SUBS!' if $bvalflag;
=item B<-b> badinfo

Information on the support for bad values provided by the function.

!NO!SUBS!

print $fh <<'!NO!SUBS!';
=item B<-s> sig

prints signature of PDL function.

=item B<-u> usage

Prints usage information for a PDL function.

=back

=head1 VERSION

This is pdldoc v0.2.

=head1 AUTHOR

Doug Burke <burke@ifa.hawaii.edu>.

=cut

!NO!SUBS!

$fh->close;
chmod 0555, $file;

# end
