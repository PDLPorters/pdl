
# Makefile.PL for PDL::IO module.
#
# - check for win32 systems is now done in ../Makefile.PL

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["browser.pd",Browser,PDL::IO::Browser]);

%hash = pdlpp_stdargs_int(@::pack);
push @{$hash{LIBS}} , '-lcurses';
$hash{'OBJECT'} .= ' browse$(OBJ_EXT)';
$hash{'clean'}{FILES} .= ' browse$(OBJ_EXT)';
unshift @{$hash{LIBS}} , '-lncurses'; # For some systems

# Find the relevant file to include
@idirs = ('/usr/include', '/usr/local/include');
@curses = ('curses','ncurses');
@curses = reverse @curses if $^O eq 'freebsd';
CURSES:
foreach $c (@curses) {
    foreach $d (@idirs) {
      if (-e "$d/$c.h") {
      	  my $x = uc $c;
          $hash{DEFINE} .= " -D$x";
          last CURSES;
      }
    }
}


WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }


