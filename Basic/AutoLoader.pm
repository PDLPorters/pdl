
=head1 NAME

PDL::AutoLoader - MatLab style AutoLoader for PDL

=head1 SYNOPSIS

 use PDL::AutoLoader;
 $a = func1(...);   # Load file func1.pdl
 $b = func2(...);   # Load file func2.pdl

 $PDL::AutoLoader::Rescan = 1; # Enable re-scanning

=head1 DESCRIPTION

This module implements a MatLab style AutoLoader for PDL. If a unknown
function 'func()' is called then a file 'func.pdl' is searched for and
if found is read in to define 'func()' which is then executed.

Files are seached for using the directories in seach path C<@PDLLIB>, which
is initialised from the shell environment variable C<PDLLIB> which is a
colon seperated list of directories.

e.g. in csh

 setenv PDLLIB "/home/kgb/pdllib:/local/pdllib"

Note this is kept seperate from C<PERL5LIB> just in case.

As an added bonus, you can use a leading '+' on a directory name to
search not just that directory but the entire directory tree under it
(excluding symlinks).  The subdirs are determined by explicit search,
and searches occur at startup and again each time you change the number
of elements in @PDLLIB.  

For example,
  setenv PDLLIB "+~kgb/PDL"

will search /home/kgb/PDL and all its subdirectories for .pdl files.

=head2 AUTO-SCANNING

The variable C<$PDL::AutoLoader::Rescan> controls whether files
are automatically re-scanned for changes at the C<perldl> command
line.

If C<$PDL::AutoLoader::Rescan == 1> and the file is changed
then the new definition is reloaded auto-matically before
executing the C<perldl> command line. Which means in practice
you can edit files, save changes and have C<perldl> see the
changes automatically.

The default is '0' - i.e. to have this feature disabled.

As this feature is only pertinent to the C<perldl> shell it imposes
no overhead on PDL scripts. Yes Bob you can have your cake and
eat it too!

Note: files are only re-evaled if they are determined to have
been changed according to their date/time stamp.

No doubt this interface could be improved upon some more. :-)

=head2 Sample file:

 sub foo { # file 'foo.pdl' - define the 'foo' function
   my $x=shift;
   return sqrt($x**2 + $x**3 + 2);
 }
 1; # File returns true (i.e. loaded successfully)

=head1 AUTHOR

Copyright(C) 1997 Karl Glazebrook (kgb@aaoepp.aao.gov.au);
several extensions by Craig DeForest (deforest@boulder.swri.edu)
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=head1 BUGS

No doubt this interface could be improved upon some more. :-)

Will probably be quite slow if C<$PDL::AutoLoader::Rescan == 1>
and thousands of functions have been autoloaded.

There could be a race condition in which the file changes
while the internal autoloader code is being executed but it
should be harmless.

Probably has not been tested enough!

=cut

BEGIN{
  @PDLLIB = (".",split(':',$ENV{"PDLLIB"})) if defined $ENV{"PDLLIB"};
  $PDL::AutoLoader::Rescan=0;
  %PDL::AutoLoader::FileInfo = ();
}

# Code to reload stuff if changed

sub PDL::AutoLoader::reloader {
   return unless $PDL::AutoLoader::Rescan;

   # Now check functions and reload if changed

   my ($file, $old_t);
   for my $func (keys %PDL::AutoLoader::FileInfo) {
       ($file, $old_t) = @{ $PDL::AutoLoader::FileInfo{$func} };
       if ( (stat($file))[9]>$old_t ) { # Reload
          print "Reloading $file as file changed...\n" if $PDL::verbose;
	  &PDL::AutoLoader::autoloader_do($file);
	  $PDL::AutoLoader::FileInfo{$func} = [ $file, (stat($file))[9] ];
       }
   }
}

sub PDL::AutoLoader::import {

my $pkg = (caller())[0];
my $toeval = "package $pkg;\n";

# Make sure that the eval gets NiceSlice if we have it in this level
# (it's a drag that preprocessors aren't transitive...)
$toeval .= "use PDL::NiceSlice;\n" if(defined $PDL::NiceSlice::VERSION);

$toeval .= <<'EOD';
$PDLLIB_CT = 0;

push @PERLDL::AUTO, \&PDL::AutoLoader::reloader;

sub AUTOLOAD {
    local @INC = @INC;
    $AUTOLOAD =~ /::([^:]*)$/;
    my $func = $1;

    # Trap spurious calls from 'use UnknownModule'

    goto &$AUTOLOAD if ord($func)==0;

   # Check if the PDLLIB needs to be expanded and, if so, expand it.
   # This only updates when PDLLIB changes size, which should be OK
   # for most things but doesn't catch new directories in expanded
   # directory trees.  It seems like an OK compromise between never 
   # catching anything and always thrashing through the directories.

   if($PDLLIB_CT != scalar(@PDLLIB)) {
     print "Expanding directories from ".join(':',@PDLLIB)."...\n"
       if($PDL::verbose);
     local $_;
     $PDLLIB_CT = scalar(@PDLLIB);
     foreach $_(@PDLLIB) {
       # Expand ~{name} and ~ conventions
       s/^(\+?)~([a-zA-Z0-9]*)// && 
        ($_ = $1.((getpwnam($2 || getlogin))[7]).$_ );
       
       # If there's a leading '+', include all subdirs too.
       push(@PDLLIB_EXPANDED,
           s/^\+// ? &PDL::AutoLoader::expand_dir($_) : $_
           );
     }
   }


    print "Loading $func.pdl...\n" if $PDL::verbose;
    my $file;

    my $s = "PDL AutoLoader:  Undefined subroutine $func() cannot be autoloaded.\n";

    for my $dir (@PDLLIB_EXPANDED) {
        $file = $dir . "/" . "$func.pdl";
	if (-e $file) {
	  
	  &PDL::AutoLoader::autoloader_do($file);
	  
	  
	  # Remember autoloaded functions and do some reasonably
	  # smart cacheing of file/directory change times
	  
	  if ($PDL::AutoLoader::Rescan) {
	    $PDL::AutoLoader::FileInfo{$func} = [ $file, (stat($file))[9] ];
	  }
	  
	  # Now go to the autoload function
	  goto &$AUTOLOAD unless ($@ || !defined(&{$AUTOLOAD}));

	  die $s."\tWhile parsing file `$file':\n$@\n" if($@);
	  die $s."\tFile `$file' doesn't \n\tdefine ${AUTOLOAD}().\n"
	  
	}
      }

    die $s."\tNo file `$func.pdl' was found in your \@PDLLIB path.\n";
}

EOD

eval $toeval;

}


# Simple 'do' doesn't work with preprocessing -- this replaces
# "do file" and sticks NiceSlice in manually if it's needed (yuck).

sub PDL::AutoLoader::autoloader_do {
  my ($file) = shift;
  
  if(defined($PDL::NiceSlice::VERSION)) {
    
    print "AutoLoader: NiceSlice enabled...\n" if($PDL::debug);
    
    if(open(AUTOLOAD_FILE,"<$file")) {
      my($script) = &PDL::NiceSlice::perldlpp(join("",<AUTOLOAD_FILE>));
      eval $script;
    }
  } else {
    print "AutoLoader: no NiceSlice...\n" if($PDL::debug);
    do $file;
  }
}


# Expand directories recursively...
sub PDL::AutoLoader::expand_dir {
  local $d;  
  local @list;  
  local @subdirs;  

  local $dir = shift;
    
  if(! -d $dir) { return undef; }
  push(@list,$dir);

  opendir(FOO,$dir);

  @subdirs = grep((!m/^\./ && ($_="$dir/$_") && (-d $_)), readdir(FOO));
  closedir FOO;

  while(defined ($d = shift @subdirs)) {
    push(@list,&PDL::AutoLoader::expand_dir($d));
  }
  return @list;
}


;# Exit with OK status

1;
