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

unlink $file if -f $file;
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

# Simple shell for PDL

use vars qw($VERSION $HOME $Modules);

$VERSION = '1.30'; print "perlDL shell v$VERSION
 PDL comes with ABSOLUTELY NO WARRANTY. For details, see the file
 'COPYING' in the PDL distribution. This is free software and you
 are welcome to redistribute it under certain conditions, see
 the same file for details.\n";

# Useful shell variables

$PERLDL::ESCAPE = '#'; 		# Default shell escape
$PERLDL::PROMPT = "perldl> ";
$PERLDL::PAGER  = (exists $ENV{PAGER} ? $ENV{PAGER} : 'more');
				# Default output paging program
$PERLDL::PAGE   = 0;
$PERLDL::PAGING = 0;
@PERLDL::AUTO   = ();
$PERLDL::PREPROCESS  = undef;   # user supplied code to pre-process shell input
$HOME = $ENV{HOME};             # Useful in shell

$HOME = 'c:/Temp' if $HOME eq "" # For want of somewhere better..
   and ($^O =~ /win32/i);

$,=" "; # Default

$Modules = $Modules = "";	# pacify -w

# Parse ARGV

while(defined($_ = shift @ARGV)) {
	if($_ eq "-tk") {
		print "Using Tk";
		eval "use Tk;";
                if ($@ eq "") {
                   print " v$Tk::VERSION\n"
                      if defined $Tk::VERSION; # make -w happy
                } else {
                   print ", sorry can't load module Tk\n";
                }
		next;
	} elsif(/^-f(.*)/) {
		my $file = $1;
		if(0 == length $1) {
			$file = shift @ARGV;
		}
		print "Doing '$file'\n";
		do $file;
		if($@) {
			die "Initialization error: $@";
		}
		next;
	} elsif(/^-w$/){ 
		$^W = 1;
		next;
        } elsif (/^ \- (M|m) (\w+) (\=\w+)? $/x ) {
                my ($way,$m,@im) = ($1,$2,$3?substr($3,1):());
                eval "require $m";
                warn, next if $@;
                if ($way eq 'M') {
                	$m->import(@im);
                } else {
                        $m->unimport(@im);
                }
      } elsif (/^-I (\S*) $/x) {
            my $dir = $1;
            $dir = $ARGV[++$arg]
              if !$dir;
            if ($dir =~ m{^ \/ }x) {
              unshift(@INC, $dir);
            } else {
              require FindBin;
              die "Error: can't find myself" if ! $FindBin::Bin;
              unshift(@INC, "$FindBin::Bin/$dir");
            }

      } else {
            print "\n";
          print "Usage: perldl [options]\n";
          print "  -tk         try to load Tk module (Enables \n";
          print "              readline event-loop processing).\n";
          print "  -f <file>   execute file <file> before starting perldl\n";
          print "  -w          run with warning messages turned-on \n";
          print "  -m <module> unload module <module>\n";
          print "  -M <module> load module <module>\n";
          print "  -I <dir>    Add <dir> to include path.\n";
            print "\n";

          die("Unknown argument $_");
      }

}

eval "use Term::ReadLine";
my $readlines = ($@ eq "");

print "ReadLines enabled" if $readlines;

print "\n";

my($term);
if ( $readlines ){
    $term = new Term::ReadLine 'perlDL', \*STDIN, \*STDOUT ;
    if(defined &Tk::DoOneEvent) {
    	# Attempt to use with Tk
        if(${$term->Features}{tkRunning}) {
		print "Using Tk event loop\n";
		$term->tkRunning(1);
	} else {
		warn("Sorry, cannot use Tk with this version of ReadLine\n");
	}
    }
    if ( ( -e "$HOME/.perldl_hist" )
	&& ( open HIST, "<$HOME/.perldl_hist" ) ) {
	my @allhist = <HIST>;
	close HIST;
	map s/\n//g , @allhist ;
	foreach (@allhist) {
	    $term->addhistory($_);
	}
    }
    eval <<'EOEND';
sub END {

    # Save History in $ENV{'HOME'}/.perldl_hist
    # GetHistory doesn't work on all versions...
    my @a= $term->GetHistory() if $term->can('GetHistory');

    $#a-- if $a[-1] =~ /^(q$|x$|\s*exit\b|\s*quit\b)/; # chop off the exit command
    @a= @a[($#a-50)..($#a)] if $#a > 50 ;
    if( open HIST, ">$HOME/.perldl_hist" ) {
	print HIST join("\n",@a);
	close HIST;
    } else {
	print " Unable to open \"$HOME/.perldl_hist\"\n";
    }
}
EOEND
}

sub l {
  if ($readlines) {
    my $n = $#_ > -1 ? shift : 20;
    my @h = $term->GetHistory();
    my $min = $#h < $n-1 ? 0 : $#h-$n+1;
    map {print "$_: $h[$_]\n"} ($min..$#h);
  }
}

sub page {
  $PERLDL::PAGE = (defined $_[0] ? $_[0] : 1);
}
sub nopage {
  page(0);
}
sub startpage {
  if ($PERLDL::PAGE) {
    open(SAVEOUT, '>&STDOUT');
    open(STDOUT, "| $PERLDL::PAGER");
    $PERLDL::PAGING = 1;
  }
}
sub endpage {
  if ($PERLDL::PAGING) {
    close(STDOUT);
    open(STDOUT, '>&SAVEOUT');
    $PERLDL::PAGING = 0;
  }
}


sub startup_def {
  return "PDL/default.pdl" if $^O =~ /win32/i;
  return "PDL/default.perldlrc";
}

# Global and local startup

my $startup_file = -e "$HOME/.perldlrc" ? "$HOME/.perldlrc" : startup_def();

print "Reading $startup_file...\n";

eval 'require "'.$startup_file.'"';
my $PDL_OK = ($@ eq "");
if ($PDL_OK) {
   require PDL::Version if not defined $PDL::Version::VERSION;
   print "Type 'demo' for online demos\n";
   eval "use PDL::Bad;";
   my $bflag = defined($PDL::Bad::Status) && $PDL::Bad::Status;
   if ( $bflag ) {
      print "Loaded PDL v$PDL::Version::VERSION (supports bad values)\n";
   } else {
      print "Loaded PDL v$PDL::Version::VERSION\n";
   }
}else{
   warn "WARNING: Error loading PDL: '$@' - trying blib. \n";
   eval "use blib";
   eval 'require "'.$startup_file.'"';
   $PDL_OK = ($@ eq "");
   if ($PDL_OK) {
      require PDL::Version if not defined $PDL::Version::VERSION;
      print "Loaded PDL v$PDL::Version::VERSION\n";
   }else{
      warn "WARNING: PDL startup not found only plain perl available\n";

      eval << 'EOD'; # Fallback eval routine - proper one defined in PDL::Core

sub eval_and_report {
   my $__code = shift; # Can be code ref or string
   my $__string;
   $__string = (ref $__code eq "CODE") ? '&$__code()' : $__code;
   eval $__string; # Use boring eval() which misses some errors
   return $@;
}
EOD

   }
}

if (-e 'local.perldlrc') {
    print "Reading local.perldlrc ...\n";
    require 'local.perldlrc' ;
}

# Short hand for some stuff

sub p { local $^W=0; print(@_); }  # suppress possible undefined var message 
                                   # (dirty)

my %demos = 
    (
     'pdl' => 'PDL::Demos::General', # have to protect pdl as it means something
     '3d' => 'PDL::Demos::TriD1',
     '3d2' => 'PDL::Demos::TriD2',
     '3dgal' => 'PDL::Demos::TriDGallery',
     'tk3d' => 'PDL::Demos::TkTriD_demo',
     'pgplot' => 'PDL::Demos::PGPLOT_demo',
     'bad' => 'PDL::Demos::BAD_demo',
     'bad2' => 'PDL::Demos::BAD2_demo',
     );

sub demo {
    local $_ = lc $_[0] ;
    if(/^$/) {
	print <<EOD;
Use:
   demo pdl     # general demo
   demo 3d      # 3d demo (requires TriD compiled with OpenGL or Mesa)
                # as well as an X display properly set up.
   demo 3d2     # 3d demo, second part. This is very memory-heavy, make
                # your window fairly small.
   demo 3dgal   # the 3D gallery: generate interesting images with
                # *very* concise PDL scripts
   demo Tk3d    # Requires the perl Tk module
   demo pgplot  # Requires the PGPLOT package
   demo bad     # Requires bad value support
   demo bad2    # Requires bad value support and PGPLOT

EOD
    return;
    } # if: /^$/

    if ( exists $demos{$_} ) {
	require PDL::Demos::Screen; # Get the routines for screen demos.
	my $name = $demos{$_};
	eval "require $name;"; # see docs on require for need for eval
	$name .= "::run";
	&{$name}();
    } else {
	print "No such demo!\n";
    }

} # sub: demo

$SIG{'INT'} = sub {print "Ctrl-C detected\n"; goto restart}; # Ctrl-C handler

$|=1; my ($sep,$code,$mess);
while(1) {

restart:

     { local $, = "";

	     if ($readlines) {
		if (ref $PERLDL::PROMPT) {
		     $_ = $term->readline(&$PERLDL::PROMPT);
		} else {
		     $_ = $term->readline($PERLDL::PROMPT);
		  }
	      }else{
		  if (ref $PERLDL::PROMPT) {
		    print &$PERLDL::PROMPT;
		  } else {
		    print $PERLDL::PROMPT;
		  }
		  $_ = <>;
	     }
     }

     # Execute the list of auto-code

     for my $c (@PERLDL::AUTO) {
        my $mess = eval_and_report($c);
	warn $mess if $mess;
     }

     if(!defined $_ || lc $_ eq 'q' || lc $_ eq 'x' || lc $_ eq 'quit') {exit};
     next if /^\s*$/; # Blank line - do nothing

     s/^\s*\?\?\s*/apropos /; # Make '??' = 'apropos'
     s/^\s*\?\s*/help /; # Make lone '?' = 'help'

     if (/^\s*(help|usage|apropos|sig|badinfo|demo)\s+/) { # Allow help foo (no quotes)
        @t = split;
        foreach (@t) { s/^["']+//; s/['"]+$//; };
	$t[1] = "'".$t[1]."'" if ($#t == 1 && !($t[1] =~ /^\$/));
	$_ = join(' ',@t);
     }

     if (substr($_,0,1) eq substr($PERLDL::ESCAPE,0,1)) {
        system(substr($_,1)); # Shell escape
        next;
     }else{
        # Send code to pre-processor if defined
        $_ = $PERLDL::PREPROCESS->($_)
          if (defined $PERLDL::PREPROCESS && 
              ref($PERLDL::PREPROCESS) eq 'CODE');

	startpage;
	my $mess = eval_and_report($_);
        warn $mess if $mess;
	endpage;
     }
     print "\n";
}


# Work routine to eval code and post-process messages
# Currently used by 'perldl' shell

sub eval_and_report {
  my $__code = shift; # Can be code ref or string

  my $__coderef = (ref $__code eq "CODE") ? $__code : eval <<"EOD";

sub {
   $__code;
}
EOD

  %@ = (); # Workaround to prevent spurious loss of $@
  PDL::Core::myeval( $__coderef )  # Do command with $@ keeping
    unless($@);  # Check that sub compiled (avoids null coderef bug in myeval).

  if ($@) {
      my $mess = $@;

      # Remove surplus parts

      $mess =~ s/^\s*\(in cleanup\)\s+//;   # 'cleanup ...' from Usage:...
      $mess =~ s/\n\s*\(in cleanup\).*$//;  # 'cleanup...'s at end
      $mess =~ s/\s+at \(eval \d+\) line \d+\.?$//; # at eval ?? line ??.

      return $mess;  # Report error
  }
  return "";
}



__END__

=head1 NAME

perldl - Simple shell for PDL

=head1 SYNOPSIS

	%> perldl
	perldl> $a=sequence(10) # or any other PDL command

=head1 DESCRIPTION

The program B<perldl> is a simple shell (written in perl) for
interactive use of PDL.  perl/PDL commands can simply be typed in - and
edited if you have appropriate version of the ReadLines and ReadKeys
modules installed. In that case B<perldl> also supports a history
mechanism where the last 50 commands are always stored in the file
F<.perldl_hist> in your home directory between sessions. The command
C<l [number]> shows you the last C<number> commands you typed where
C<number> defaults to 20.

e.g.:

   % perldl
   ReadLines enabled
   perldl> $a = rfits "foo.fits"
   BITPIX =  -32  size = 88504 pixels
   Reading  354016 bytes
   BSCALE =  &&  BZERO =

   perldl> imag log($a+400)
   Displaying 299 x 296 image from 4.6939525604248 to 9.67116928100586 ...


Miscellaneous shell features:

=over 4

=item 1.

The shell aliases C<p> to be a convenient short form of C<print>, e.g.

   perldl> p ones 5,3

   [
    [1 1 1 1 1]
    [1 1 1 1 1]
    [1 1 1 1 1]
   ]

'q' and 'x' are short-hand for quit.

'l' lists the history buffer

'?' is an alias for help

'help', 'apropos', 'usage' and 'sig': all words after these commands are
used verbatim and not evaluated by perl. So you can write, e.g.,

    help help

instead of

    help 'help'


=item 2.

If the file F<~/.perldlrc> is found it is sourced at start-up to load default
modules, set shell variables, etc. If it is NOT found the distribution file
F<PDL/default.perldlrc> is read instead. This loads various modules
considered useful by default, and which ensure compatibility with
v1.11. If you don't like this and want a more streamlined set of your
own favourite modules simple create your own F<~/.perldlrc>

To set even more local defaults the file  F<local.perldlrc> (in the current
directory) is sourced if found. This lets you load modules and define
subroutines for the project in the current directory.

The name is chosen specfically because it was found hidden files were
NOT wanted in this circumstances.

=item 3.

Shell variables: (if you don't like the defaults change them in F<~/.perldlrc>

$PERLDL::ESCAPE  - default value '#'

Any line starting with this character is treated as a shell
escape. The default value is chosen because it escapes the
code from the standard perl interpreter.

$PERLDL::PAGER - default value C<more>

External program to filter the output of commands.  Using C<more>
prints output one screenful at a time.  On Unix, setting C<page(1)>
and $PERLDL::PAGER to C<tee -a outfile> will keep a record of the
output generated by subsequent perldl commands (without paging).

$PERLDL::PROMPT - default value 'perldl> '

Enough said  But can also be set to a subroutine reference, e.g.
$PERLDL::PROMPT = sub {join(':',(gmtime)[2,1,0]).'> '} puts the
current time into the prompt.

$HOME

The user's home directory

=item 4.

A useful idiom for developing perldl scripts or editing functions
on-line is

      perldl> # emacs script &
		      -- add perldl code to script and save the file
      perldl> do 'script'

-- substitute your favourite window-based editor for 'emacs' (you may
also need to change the '&' on non-Unix systems).

Running "do 'script'" again updates any variables and function
definitions from the current version of 'script'.

=item 5.

The variable @PERLDL::AUTO is a simple list of perl code strings
and/or code reference. It is used to define code to be
executed automatically every time the user enters a new line.

A simple example would be to print the time of each command:

 perldl> push @PERLDL::AUTO,'print scalar(gmtime),"\n"'

 perldl> print zeroes(3,3)
 Sun May  3 04:49:05 1998

 [
  [0 0 0]
  [0 0 0]
  [0 0 0]
 ]

 perldl> print "Boo"
 Sun May  3 04:49:18 1998
 Boo
 perldl>

Or to make sure any changes in the file 'local.perldlrc' are
always picked up :-

 perldl> push @PERLDL::AUTO,"do 'local.perldlrc'"

This code can of course be put *in* 'local.perldlrc', but
be careful :-) [Hint: add C<unless ($started++)> to above
to ensure it only gets done once!]

Another example application is as a hook for Autoloaders
(e.g. PDL::AutoLoader) to add code too which allows them to
automatically re-scan their files for changes. This is
extremely convenient at the interactive command line. Since
this hook is only in the shell it imposes no inefficiency on
PDL scripts.

Finally note this is a very powerful facility - which means
it should be used with caution!

=item 6

In some cases, it is convenient to process commands before they are
sent to perl for execution. For example, this is the case where the
shell is being presented to people unfamiliar with perl but who wish
to take advantage of commands added locally (eg by automatically 
quoting arguments to certain commands).

The variable $PERLDL::PREPROCESS can be set to a code reference
(usually in a local configuration file) that will be called, with the
current string as argument, just prior to the string being executed by
the shell. The modified string should be returned.

The following code would check for a call to function 'mysub'
and bracket arguments with qw.

 $PERLDL::PREPROCESS = sub {
   my $str = shift;
   $str =~ s/^\s+//;  # Strip leading space
   if ($str =~ /^mysub/) {
     my ($command, $arguments) = split(/\s+/,$str, 2);
     $str = "$command qw( $arguments )" 
       if (defined $arguments && $arguments !~ /^qw/);
   };
   # Return the input string, modified as required
   return $str;
 };

This would convert:

  perldl> mysub arg1 arg2

to

  perldl> mysub qw( arg1 arg2 )

which perl will understand as a list.  Obviously, a little more effort
is required to check for cases where the caller has supplied a normal
list (and so does not require automatic quoting) or variable
interpolation is required.

=back

=head2 Command-line options

=over 4

=item -tk

Load Tk when starting the shell (the perl Tk module, which is
available from CPAN must be installed). This enables readline
event loop processing.

=item -f file

Loads the file before processing any user input. Any errors
during the execution of the file are fatal.

=item -w

Runs with warning messages (i.e. the normal perl C<-w> warnings)
turned-on.

=item -M module

Loads the module before processing any user input.

=item -m module

Unloads the module before processing any user input.

=item -I directory

Adds directory to the include path. (i.e. the @INC array)

=back

=cut

!NO!SUBS!

close OUT;
chmod 0555, $file;
