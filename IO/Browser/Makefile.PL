
# Makefile.PL for PDL::IO module.
#
# - check for win32 systems is now done in ../Makefile.PL
local $| = 1;

use ExtUtils::MakeMaker;
use File::Spec;

my $libdir;

BEGIN {
   if (defined $PDL::Config{PDL_BUILD_DIR}) {
      print STDERR "IO/Browser: got root build dir $PDL::Config{PDL_BUILD_DIR}\n";
      $libdir = File::Spec->catfile($PDL::Config{PDL_BUILD_DIR},'inc');
      print STDERR "IO/Browser: adding $libdir\n";
   }
}

use lib $libdir;

use Devel::CheckLib;

require PDL::Core::Dev;
PDL::Core::Dev->import();

@pack = (["browser.pd",Browser,PDL::IO::Browser]);

%hash = pdlpp_stdargs_int(@::pack);

$hash{'OBJECT'} .= ' browse$(OBJ_EXT)';
$hash{'clean'}{FILES} .= ' browse$(OBJ_EXT) browse$(EXE_EXT) Browser.c Browser.pm Browser.xs Browser$(OBJ_EXT)';

# Here we check for working curses/ncurses
# and the corresponding "curses.h" and "ncurses/curses.h"
#
# (1) Determine which library we have: curses or ncurses
# (2) determine which include path
# (3) determine which include file
# (4) confirm configuration
# (5) write Makefile or dummy as appropriate

my $incstring;
foreach my $incl ( qw( curses.h ncurses/curses.h ncurses.h ncurses/ncurses.h ) ) {
    if (check_lib(header=>$incl)) {
       print STDERR "IO/Browser: have '$incl'\n";
       $incstring = $incl;
       last;
    }
};
$hash{DEFINE} .= ' -DCURSES=' . '\\"' . $incstring . '\\"' if defined $incstring;

my $libstring;
foreach my $libr ( qw( curses ncurses ) ) {
   if (check_lib(lib=>$libr)) {
      print STDERR "IO/Browser: have -l$libr\n";
      $libstring = '-l' . $libr;
      last;
   }
}
push @{$hash{LIBS}} , $libstring if defined $libstring;

if (defined($incstring) && defined($libstring)) {
   WriteMakefile(%hash);
} else {
   write_dummy_make("Curses capable library not found, not building PDL::IO::Browser");
}

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }


