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

$VERSION = '1.33';

print "perlDL shell v$VERSION
 PDL comes with ABSOLUTELY NO WARRANTY. For details, see the file
 'COPYING' in the PDL distribution. This is free software and you
 are welcome to redistribute it under certain conditions, see
 the same file for details.\n";

# Useful shell variables

$PERLDL::ESCAPE = '#'; 		# Default shell escape
$PERLDL::MULTI = 1;             # Enable multi-lines by default
$PERLDL::NO_EOF = 0;            # Disable EOF protection by default
$PERLDL::PROMPT = "perldl> ";
$PERLDL::PAGER  = (exists $ENV{PAGER} ? $ENV{PAGER} : 'more');
				# Default output paging program
$PERLDL::PAGE   = 0;
$PERLDL::PAGING = 0;
@PERLDL::AUTO   = ();
$PERLDL::PREPROCESS  = undef;   # old interface -- disabled
@PERLDL::PREPROCESS = ();       # new preprocessor pipeline
$HOME = $ENV{HOME};             # Useful in shell

$HOME = 'c:/Temp' if $HOME eq "" # For want of somewhere better..
   and ($^O =~ /win32/i);

$,=" "; # Default

$Modules = $Modules = "";	# pacify -w

sub mypdlconfig {
  require Config; # pick up perl version info
  eval 'require PDL::Version' if not defined $PDL::Version::VERSION;
  eval 'require PDL::Config';
  eval "use Data::Dumper";
  my $hasdumper = $@ eq "" ? 1 : 0;
  eval "use PDL::Bad;";
  my $bflag = defined($PDL::Bad::Status) && $PDL::Bad::Status;

  my $txt = "\nSummary of my PDL configuration\n\n";
  $txt .= "VERSION: PDL v$PDL::Version::VERSION";
  $txt .= " (supports bad values)" if $bflag;
  $txt .= "\n\n";
  if ($hasdumper && defined %PDL::Config) {
    $txt .= Data::Dumper->Dump([{%PDL::Config}],['%PDL::Config']);
  } else {
    $txt .= "Could not obtain \%PDL::Config\n";
  }
  $txt .= Config::myconfig(); # append perl config info
}

sub preproc_registered ($) {
  my ($sub) = @_;
  die "preprocessors must be code references"
     unless ref $sub eq 'CODE';
  return grep ($_ == $sub, @PERLDL::PREPROCESS) > 0;
}

sub preproc_add ($) {
  my ($sub) = @_;
  die "preprocessors must be code references"
     unless ref $sub eq 'CODE';
  push @PERLDL::PREPROCESS, $sub;
  return $sub;
}

sub preproc_del ($) {
  my ($sub) = @_;
  die "preprocessors must be code references"
     unless ref $sub eq 'CODE';
  die "preprocessor can't be deleted: not installed"
    unless preproc_registered $sub;
  @PERLDL::PREPROCESS = grep ($_ != $sub, @PERLDL::PREPROCESS);
  return $sub;
}

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
        } elsif (/^-(M|m)([\w:]+)(\=\w+)?$/x) {
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

      } elsif (/^-V\s*$/) {
         print mypdlconfig();
         exit 0;
      } else {
            print << 'EOP';
Usage: perldl [options]
  -tk         try to load Tk module (Enables
              readline event-loop processing).
  -f <file>   execute file <file> before starting perldl
  -w          run with warning messages turned-on 
  -m <module> unload module <module>
  -M <module> load module <module>
  -I <dir>    Add <dir> to include path.
  -V          print PDL version info (e.g. for a bug report)

EOP
          die("Unknown argument $_");
      }

}

eval "use Term::ReadLine";
my $readlines = ($@ eq "");

my @enabled = ();
push @enabled, "ReadLines" if $readlines;

eval 'use PDL::NiceSlice';
unless ($@) {
  my $report = 0;
  sub report {
    my $ret = $report;
    $report = $_[0] if $#_ > -1;
    return $ret;
  }
  my $preproc = sub { my ($txt) = @_;
	  my $new = PDL::NiceSlice::perldlpp($txt);
	  print STDERR "processed $new\n" if report && $new ne $txt;
	  return $new;
  };
  sub trans {
     preproc_add $preproc unless preproc_registered $preproc;
     preproc_del $preproc if $#_ > -1 && !$_[0] &&
        preproc_registered $preproc;
  }
  sub notrans { trans 0 }
  trans; # switch on by default
  push @enabled, "NiceSlice";
}

eval "use Text::Balanced";
my $multi_ok = ($@ eq "");
$PERLDL::MULTI = 0 unless($multi_ok);
push @enabled,"MultiLines" if $multi_ok;

print join(', ',@enabled)," enabled\n" if @enabled > 0;

my($term);
if ( $readlines ){
    $term = new Term::ReadLine 'perlDL', \*STDIN, \*STDOUT ;
    $PERLDL::TERM = $term;
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
     'ooplot' => 'PDL::Demos::PGPLOT_OO_demo', # note: lowercase
     'bad' => 'PDL::Demos::BAD_demo',
     'bad2' => 'PDL::Demos::BAD2_demo',
     'transform' => 'PDL::Demos::Transform_demo',
     'cartography' => 'PDL::Demos::Cartography_demo'
     );

sub demo {
    local $_ = lc $_[0] ;
    if(/^$/) {
	print <<EOD;
Use:
   demo pdl         # general demo

   demo 3d          # 3d demo (requires TriD with OpenGL or Mesa)
   demo 3d2         # 3d demo, part 2. (Somewhat memory-intensive)
   demo 3dgal       # the 3D gallery: make cool images with 3-line scripts
   demo Tk3d        # Requires the perl Tk module

   demo pgplot      # PGPLOT graphics output (Req.: PGPLOT)
   demo OOplot      # PGPLOT OO interface    (Req.: PGPLOT)

   demo transform   # Coordinate transformations (Req.: PGPLOT)
   demo cartography # Cartographic projections (Req.: PGPLOT)

   demo bad         # Bad-value demo (Req.: bad value support)
   demo bad2        # Bad-values, part 2 (Req.: bad value support and PGPLOT)

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

$SIG{'INT'} = sub {die "Ctrl-C detected\n"}; # Ctrl-C handler


my $preproc_warned = 0;
sub preproc_oldwarn {
  warn << 'EOW';

 Deprecated usage: $PERLDL::PREPROCESS was set.
 Usage of this variable is now strongly deprecated.
 To enable preprocessing with recent versions of perldl
 you should use the 'preproc_add' function. For details
 check the perldl manpage.

EOW
  $preproc_warned = 1; # warn only once
}



#
# count_tags: Return a string containing (in order) the open brackets
# and strings in the string that is passed in.  Used for multi-line parsing.
#
# Works by analysing the error message returned by Text::Balanced -- this
# is sort of fragile against changes in Text::Balanced, but what the heck.
#  --CED 18-Mar-2003
#
sub count_tags {
    my $s = shift;

    $s =~ s/\\.//g; # Ignore all escaped characters

    return undef unless($s =~ m/[^\s]/);
    
    # Anything but a delimiter ('q' is a string begin delimiter in perl,
    # meaning we have to do a little shenanigan to treat things like 
    # "quest" and "arquebus" properly.)
    our($prefix,$delim,%closers);
    unless(defined $prefix) {
	$prefix = qr/ ( [^\{\}\[\]\(\)\'\"\`q] | (\wq) | (q($|\w)) )* /x;
	$delim = '{[(`\'"q)]}';
	%closers = ('{'=>'}','['=>']','('=>')');
    }

    # Run Text::Balanced on the string with a '{' in front of it, to 
    # make sure that all quoted strings are "embedded" in the outermost "{".
    # The whitespace works around a short-string bug in extract_bracketed.
    my $a;
    my @result;
    eval { @result = 
	       Text::Balanced::extract_bracketed("{".$s, $delim, $prefix);
	   $a = $@;
       };
    print "a = $a\nreturn = '",join("','",@result),"'\n" 
	if($PERLDL::debug);

    if($a =~ m/^Did not find/) {
	# No quotes -- this should never happen and is a syntax error.
	print STDERR "[Error in parsing: this should never happen!]\n" 
	    if($PERLDL::debug);
	return undef;
    }
    elsif($a =~ m/^Unmatched emb\w+ quote \((.)\), de\w+ at offset (\d+)/) {
	# Embedded quote: try to close it and reparse.
	$a = $1;
	return count_tags($s.$1) . $a;
    }
    elsif($a =~ m/^Mismatched closing bracket/) {
	# This is an error condition - return false and let perl parse it
	return undef;
    }
    elsif($a =~ m/^Unmatched opening bracket\(s\)\: \{\.\.(.\.\.)+/) {
	$a = $1;
	$a=~ s/\.\.//g;
	return count_tags($s.$closers{$a}) . $a; 
    }
    elsif($a =~ m/^Unmatched opening bracket\(s\)\: \{\.\.\,/) {
	# Should have exactly one unmatched opening bracket.
	return undef;
    } 
    elsif(!$a) {
	return undef;
    }

    print STDERR "Unknown error message '$a' from parser...\n"
	if($PERLDL::debug);
    return undef;
}



#
# process_input -- this is the central grab-some-input-and-execute it loop.
# 
sub process_input { 
  my $lines;


  if($PERLDL::MULTI && !$multi_ok) {
      $PERLDL::MULTI = 0;
      print STDERR "WARNING: Text::Balanced not present; disabling multi-line parsing.\n";
  }
  
  # The {} around the do let us get out with 'last' in the EOF case.
 multiline: { 
     my $cont;
     $lines = "";

     
     do {
	 local $, = "";

	 my $prompt = $cont ? "..$cont".(" "x(5-length($cont)))."> " : 
	     ((ref $PERLDL::PROMPT) ? &$PERLDL::PROMPT : $PERLDL::PROMPT);

	 if ($readlines) {
	     $_ = $term->readline($prompt);
	 }else{
	     print $prompt;
	     $_ = <>;
	 }
	 
	 if(!defined $_) {
	     if($cont) {
		 if( $PERLDL::NO_EOF > 1 && -t STDIN ) {
		     print STDERR "\nEOF ignored.  (Close delimiters to end block. \$PERLDL::NO_EOF = $PERLDL::NO_EOF)\n";
		 } else {
		     last multiline;
		 }
	     } else {
		 if($PERLDL::NO_EOF && -t STDIN ) {
		     print STDERR "EOF ignored. ('q' or 'exit' to quit.  \$PERLDL::NO_EOF = $PERLDL::NO_EOF)\n";
		 } else {
		     exit 0;
		 }
	     }
	 }
	 
	 $lines .= "\n" if($cont); # Make multi-line strings work right.
	 $lines .= $_;
	 print "lines = $lines\n" if($PERLDL::debug);
     } while( $PERLDL::MULTI && ($cont = count_tags($lines)) );

 } 
  
  $_ = $lines;

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
  } else {
      # Send code to pre-processor filters if defined
      for my $filter (@PERLDL::PREPROCESS) {
          $_ = $filter->($_);
      }
      # honor the deprecated interface for now
      if (defined $PERLDL::PREPROCESS && 
	  ref($PERLDL::PREPROCESS) eq 'CODE') {
          preproc_oldwarn() unless $preproc_warned;
          $_ = &$PERLDL::PREPROCESS($_);
      }
      
      startpage;
      my $mess = eval_and_report($_);
      warn $mess if $mess;
      endpage;
  }
  print "\n";
}


######################################################################
######################################################################
#####
##### Main loop is here! (Commands not inside any sub!)

# check for old usage of PERLDL::PREPROCESS
if (defined $PERLDL::PREPROCESS) {
  preproc_oldwarn();
}

$|=1;
while(1) {
  eval {process_input()};
  if ($@) {
    if ($@ =~ /Ctrl-C detected/) {
      print "Ctrl-C detected\n";
      next;
    } else {
      print "Unknown error: $@\n exiting...\n";
      last;
    }
  }
}

#####
#####
######################################################################
######################################################################

# Work routine to eval code and post-process messages
# Currently used by 'perldl' shell

sub eval_and_report {
  my $__code = shift; # Can be code ref or string
  $@ = ""; # clear $@ since we might not execute the eval below


  ## Compile the code ref to execute.  The code gets put inside {} braces
  ## so that there is a trivial loop (the simple block) for 'last' and 'next'
  ## to escape from.  (Otherwise perl 5.6.1 and 5.8 do a little fandango
  ## on stack if you type "last" at the shell).  --CED 18-Mar-2003

  my $__coderef = (ref $__code eq "CODE") ? $__code : eval << "EOD"
							    
							    sub {
							      {
								$__code;
							      }
							    }
EOD
;

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
Compare corresponding C<perl> switch.

=item -m module

Unloads the module before processing any user input.

=item -I directory

Adds directory to the include path. (i.e. the @INC array)
Compare corresponding C<perl> switch.

=item -V

Prints a summary of PDL config. This information should
be included with any PDL bug report. Compare corresponding
C<perl> switch.

=back

=head2 Terminating C<perldl>

A C<perldl> session can be terminated with any of the commands
C<quit>, C<exit> or the shorthands C<x> or C<q>.

=head2 Terminating commands (Ctrl-C handling)

Commands executed within C<perldl> can be terminated prematurely
using C<Ctrl-C> (or whichever key sequence sends an INT signal
to the process on your terminal). Provided your PDL code does not
ignore C<sigint>s this should throw you back at the C<perldl>
command prompt:

  perldl> $result = start_lengthy_computation()
   <Ctrl-C>
 Ctrl-C detected

  perldl>

=head2 Shortcuts and aliases

=over

=item *

The shell aliases C<p> to be a convenient short form of C<print>, e.g.

   perldl> p ones 5,3

   [
    [1 1 1 1 1]
    [1 1 1 1 1]
    [1 1 1 1 1]
   ]

=item *

C<q> and C<x> are short-hand for C<quit>.

=item *

C<l> lists the history buffer

  perldl> l # list last 20 commands

  perldl> l 40 # list last 40 commands

=item *

C<?> is an alias for L<help|PDL::Doc::Perldl/help>

  perldl> ? wpic

=item *

C<??> is an alias for L<apropos|PDL::Doc::Perldl/apropos>

  perldl> ?? PDL::Doc

=item *

L<help|PDL::Doc::Perldl/help>, L<apropos|PDL::Doc::Perldl/apropos>,
L<usage|PDL::Doc::Perldl/usage> and L<sig|PDL::Doc::Perldl/sig>:
all words after these commands are used verbatim and not evaluated
by perl. So you can write, e.g.,

    help help

instead of

    help 'help'

=back


=head2 The startup file F<~/.perldlrc>

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
NOT wanted in these circumstances.

=head2 Shell variables

Shell variables: (I<Note>: if you don't like the defaults change
them in F<~/.perldlrc>)

=over

=item *

$PERLDL::ESCAPE  - default value '#'

Any line starting with this character is treated as a shell
escape. The default value is chosen because it escapes the
code from the standard perl interpreter.

=item *

$PERLDL::PAGER - default value C<more>

External program to filter the output of commands.  Using C<more>
prints output one screenful at a time.  On Unix, setting C<page(1)>
and $PERLDL::PAGER to C<tee -a outfile> will keep a record of the
output generated by subsequent perldl commands (without paging).

=item *

$PERLDL::PROMPT - default value 'perldl> '

Enough said  But can also be set to a subroutine reference, e.g.
$PERLDL::PROMPT = sub {join(':',(gmtime)[2,1,0]).'> '} puts the
current time into the prompt.

=item *

$PERLDL::MULTI - default value 1

If this is set to a true value, then perldl will parse multi-line 
perl blocks: your input will not be executed until you finish a line
with no outstanding group operators (such as quotes, blocks, parenthesis, or 
brackets) still active.  Continuation lines have a different prompt that
shows you what delimiters are still active.

Note that this is not (yet!) a complete perl parser.  In particular,
Text::Balanced appears to be able to ignore quoting operatores 
like C<q/ ... /> within a line, but not to be able to extend them across
lines.  Likewise, there is no support for the '<<' operator.

Multiline conventional strings and {}, [], and () groupings are well
supported.

=item *

$PERLDL::NO_EOF - default value 0

Protects against accidental use of "^D" from the terminal.  If this is
set to a true value, then you can't accidentally exit perldl by typing
"^D".  If you set it to a value larger than 1 (and PERLDL::MULTI is
set), then you can't use "^D" to exit multiline commands either.  If
you're piping commands in from a file or pipe, this variable has no
effect.


=item *

$HOME

The user's home directory

=item *

$PERLDL::TERM

This is the Term::ReadLine object associated with the perldl
shell. It can be used by routines called from perldl if your
command is interactive.

=back

=head2 Executing scripts from the C<perldl> prompt

A useful idiom for developing perldl scripts or editing functions
on-line is

      perldl> # emacs script &
		      -- add perldl code to script and save the file
      perldl> do 'script'

-- substitute your favourite window-based editor for 'emacs' (you may
also need to change the '&' on non-Unix systems).

Running "do 'script'" again updates any variables and function
definitions from the current version of 'script'.

=head2 Automatically execute your own hooks

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

=head2 Command preprocessing

NOTE: This feature is used by default by L<PDL::NiceSlice|PDL::NiceSlice>.
See below for more about slicing at the C<perldl> prompt

In some cases, it is convenient to process commands before they are
sent to perl for execution. For example, this is the case where the
shell is being presented to people unfamiliar with perl but who wish
to take advantage of commands added locally (eg by automatically 
quoting arguments to certain commands).

*I<NOTE>*: The preprocessing interface has changed from earlier
versions! The old way using C<$PERLDL::PREPROCESS> will still
work but is strongly deprecated and might go away in the future.

You can enable preprocessing by registering a filter with the
C<preproc_add> function. C<preproc_add> takes one argument which
is the filter to be installed. A filter is a Perl code reference (usually
set in a local configuration file) that will be called, with the
current command string as argument, just prior to the string being
executed by the shell. The modified string should be returned. Note
that you can make C<perldl> completely unusable if you fail to
return the modified string; quitting is then your only option.

Filters can be removed from the preprocessing pipeline by calling
C<preproc_del> with the filter to be removed as argument.  To find out
if a filter is currently installed in the preprocessing pipeline use
C<preproc_registered>:

  perldl> preproc_add $myfilter unless preproc_registered $myfilter;

Previous versions of C<perldl> used the variable C<$PERLDL::PREPROCESS>.
This will still work but should be avoided. Please change your scripts
to use the C<preproc_add> etc functions.

The following code would check for a call to function 'mysub'
and bracket arguments with qw.

 $filter = preproc_add sub {
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

which Perl will understand as a list.  Obviously, a little more effort
is required to check for cases where the caller has supplied a normal
list (and so does not require automatic quoting) or variable
interpolation is required.

You can remove this preprocessor using the C<preproc_del> function
which takes one argument (the filter to be removed, it must be the
same coderef that was returned from a previous C<preproc_add> call):

  perldl> preproc_del $filter;

An example of actual usage can be found in the C<perldl> script. Look
at the function C<trans> to see how the niceslicing preprocessor is
enabled/disabled.

=head2 C<perldl> and L<PDL::NiceSlice|PDL::NiceSlice>

L<PDL::NiceSlice|PDL::NiceSlice> introduces a more convenient
slicing syntax for piddles. In current versions of C<perldl>
niceslicing is enabled by default (if the required CPAN modules
are installed on your machine).

At startup C<perldl> will let you
know if niceslicing is enabled. The startup message will contain
info to this end, something like this:

   perlDL shell v1.XX
    PDL comes with ABSOLUTELY NO WARRANTY. For details, see the file
    'COPYING' in the PDL distribution. This is free software and you
    are welcome to redistribute it under certain conditions, see
    the same file for details.
   ReadLines, NiceSlice  enabled
   Reading /home/csoelle/.perldlrc...
   Type 'demo' for online demos
   Loaded PDL v2.XX

When you get such a message that indicates C<NiceSlice> is enabled
you can use the enhanced slicing syntax:

  perldl> $a = sequence 10;
  perldl> p $a(3:8:2)

For details consult L<PDL::NiceSlice>.

L<PDL::NiceSlice|PDL::NiceSlice> installs a filter in the
preprocessing pipeline (see above) to enable the enhanced slicing
syntax. You can use a few commands in the C<perldl> shell to
switch this preprocessing on or off and also explicitly check
the substitutions that the NiceSlice filter makes.

You can switch the L<PDL::NiceSlice|PDL::NiceSlice> filter on and off
by typing

  perldl> trans # switch niceslicing on

and

  perldl> notrans # switch niceslicing off

respectively. The filter is on by default.

To see how your commands are translated
switch reporting on:

  perldl> report 1;
  perldl> p $a(3:8:2)
 processed p $a->nslice([3,8,2])
 [3 5 7]

Similarly, switch reporting off as needed

  perldl> report 0;
  perldl>  p $a(3:8:2)
 [3 5 7]

Reporting is off by default.

=cut

!NO!SUBS!

close OUT;
chmod 0555, $file;
