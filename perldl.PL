use Config;
use File::Basename qw(&basename &dirname);

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

open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print OUT <<"!GROK!THIS!";
$Config{'startperl'}
    eval 'exec perl -S \$0 "\$@"'
        if 0;
!GROK!THIS!

# In the following, perl variables are not expanded during extraction.

print OUT <<'!NO!SUBS!';

##########################################################################
# Here starts the actual script

=head1 NAME

=head2 perldl - Simple shell for PDL

=head1 DESCRIPTION

perldl provides a simple shell for interactive use of PDL.
perl/PDL commands can simply be typed in - and edited if
you have appropriate version of the ReadLines and ReadKeys
modules installed.

=cut

unshift @INC, "./blib"; 
print <<'EOD';
perlDL shell v1.03
EOD

$PERLDL_ESCAPE = '#'; # Default shell escape

$Modules = "";

if ($ARGV[0] eq "-oo") {
   shift @ARGV; $OO++;
   print "Starting Object-Oriented perlDL\n";
}

if ($OO) {
   eval "use PDL::OO"; 
}else{
   eval "use PDL";
}
  
if ($@ eq "") {
   $PDL_OK = 1;
   print "Loaded PDL v$PDL::VERSION   ";
}else{
   $PDL_OK = 0;
   warn "WARNING: PDL module not found only plain perl available\n";
}

eval "use Term::ReadLine"; 

$readlines = ($@ eq "");

print "ReadLines enabled" if $readlines;

print "\n";

$prompt = "perldl> ";

$term = new Term::ReadLine 'perlDL', \*STDIN, \*STDOUT if $readlines;

$,=" "; # Default

$PDL::verbose = 1; # Make PDL functions waffle

# Global and local startup

for ($ENV{'HOME'}."/.perldlrc", 'local.perldlrc') {
    next unless -e $_;
    print "Reading $_ ...\n";
    require $_ ;
}

$SIG{'INT'} = sub {print "Ctrl-C detected\n"; goto restart}; # Ctrl-C handler
  
$|=1;
while(1) {

restart:

     $sep = $,; $,=""; # Save

     if ($readlines) {
         $_ = $term->readline($prompt);
     }else{
         print $prompt; $_ = <>;
     }
     $,=$sep; #Restore 

     next if /^\s*$/; # Blank line - do nothing

     if (substr($_,0,1) eq substr($PERLDL_ESCAPE,0,1)) { 
        system(substr($_,1)); # Shell escape
        next;
     }else{
        if ($PDL_OK) {
            $code = eval <<"EOD";  # Create code ref
sub {
   $_;
}
EOD
	    %@ = (); # Workaround to prevent spurious loss of $@
            PDL::Core::myeval( $code ); # Do command with $@ keeping
        }else{
            eval $_;
        }
     }
     if ($@) {
         $mess = $@; 

         # Remove surplus parts

         $mess =~ s/^\s*\(in cleanup\)\s+//;   # 'cleanup ...' from Usage:...
         $mess =~ s/\n\s*\(in cleanup\).*$//;  # 'cleanup...'s at end
         $mess =~ s/\s+at \(eval \d+\) line \d+\.?$//; # at eval ?? line ??.

         warn $mess;  # Report error
     }else{
         print "\n";
     }
}


# Short hand for some stuff

sub p { print(@_); }
sub x { exit(); }

!NO!SUBS!
