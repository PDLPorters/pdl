if ($^O =~ /win32/i) {
  PDL::Core::Dev->import();
  warn "Win32 systems not yet supported. Will not build PDL::IO::Browser";
  write_dummy_make(unsupported('PDL::IO::Browser','win32'));
  return;
}

# Makefile.PL for PDL::IO module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

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


