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
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
    if ($^O eq 'VMS' or $^O eq 'os2');  # "case-forgiving"

unlink $file if -f $file;
my $fh = new IO::File "> $file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";

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
$VERSION = '0.1';

my %options = ( a => \&apropos, h => \&help, s => \&sig, u => \&usage );

my $name = basename( $0 );
my $usage = <<"EOH";
Usage: $name [-a] [-h] [-s] [-u] <string>

This program provides command-line access to the PDL documentation.
If no flag is specified, -h is assumed.

  -a (apropos) searches the documentation for the string
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

B<pdldoc> [B<-a>] [B<-h>] [B<-s>] [B<-u>] <text>

=head1 DESCRIPTION

The aim of B<pdldoc> is to provide the same functionality
as the C<apropos>, C<help>, C<sig>, and C<usage> commands 
available in the L<perldl|perldl> shell.

Think of it as the PDL equivalent of C<perldoc -f>.

=head1 OPTIONS

=over 5

=item B<-h> help

print documentation about a PDL function or module or show a PDL manual.
This is the default option.

=item B<-a> apropos

Regex search PDL documentation database.

=item B<-s> sig

prints signature of PDL function.

=item B<-u> usage

Prints usage information for a PDL function.

=back

=head1 VERSION

This is pdldoc v0.1.

=head1 AUTHOR

Doug Burke <burke@ifa.hawaii.edu>.

=cut

!NO!SUBS!

$fh->close;
chmod 0555, $file;

# end
