use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec;
eval { require Devel::CheckLib; Devel::CheckLib->import; };

my @pack = (["browser.pd", qw(Browser PDL::IO::Browser)]);

my %hash = pdlpp_stdargs_int(@pack);

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
foreach my $incl ( qw( curses.h ncurses/curses.h ncurses.h ncurses/ncurses.h ncursesw/ncurses.h ) ) {
    if (eval { check_lib(header=>$incl) }) {
       $incstring = $incl;
       last;
    }
};
$hash{DEFINE} .= ' -DCURSES=' . '\\"' . $incstring . '\\"' if defined $incstring;

my $libstring;
foreach my $libr ( qw( curses ncurses ncursesw ) ) {
   if (eval { check_lib(lib=>$libr) }) {
      $libstring = '-l' . $libr;
      last;
   }
}
push @{$hash{LIBS}} , $libstring if defined $libstring;

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

if (defined($incstring) && defined($libstring)) {
   WriteMakefile(%hash);
} else {
   write_dummy_make("Curses capable library not found, not building PDL::IO::Browser");
}
