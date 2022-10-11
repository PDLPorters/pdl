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

sub _read_pipe {
   my (@command) = @_;
   open( my $fh, '-|', @command) or die "Unable to run @command: $!";
   chomp(my $output = do { local $/; <$fh> });
   $output;
}

my @possible_headers = qw( curses.h ncurses/curses.h ncurses.h ncurses/ncurses.h ncursesw/ncurses.h );
my $found_header;
foreach my $incl (@possible_headers) {
    if (eval { check_lib(header=>$incl) }) {
       $found_header = $incl;
       last;
    }
}

sub conf_via_ncurses_config {
   return () unless $found_header;

   my @nc_configs = qw(
      ncurses6-config
      ncursesw6-config
      ncurses5-config
      ncursesw5-config
   );
   for my $nc_conf (@nc_configs) {
      local $ENV{NCURSES_CONFIG_PROG} = $nc_conf;
      if( eval { (my $version = _read_pipe(qw(sh -c), '$NCURSES_CONFIG_PROG --version')) =~ / \A [0-9.]+ \Z /x } ) {
         my $cflags = _read_pipe(qw(sh -c), '$NCURSES_CONFIG_PROG --cflags');
         my $libs   = _read_pipe(qw(sh -c), '$NCURSES_CONFIG_PROG --libs');
         return (
            header => $found_header,
            inc    => $cflags,
            libs   => $libs,
         );
      }
   }
   return ();
}

sub conf_via_checklib {
   return () unless $found_header;

   my $libstring;
   foreach my $libr ( qw( curses ncurses ncursesw ) ) {
      if (eval { check_lib(lib=>$libr) }) {
         $libstring = '-l' . $libr;
         last;
      }
   }

   if( defined $libstring ) {
      return (
         header => $found_header,
         inc    => '',
         libs   => $libstring,
      );
   }

   return ();
}

my %conf_data;
%conf_data = conf_via_ncurses_config();
%conf_data = conf_via_checklib() unless %conf_data;
if( %conf_data ) {
   $hash{DEFINE} .= ' -DCURSES=' . '\\"' . $conf_data{header} . '\\"';
   $hash{INC} .= ' ' . $conf_data{inc} if $conf_data{inc};
   push @{$hash{LIBS}} , $conf_data{libs};
}

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

if ( %conf_data ) {
   WriteMakefile(%hash);
} else {
   write_dummy_make("Curses capable library not found, not building PDL::IO::Browser");
}
