use Test;

plan tests => 2;

my @hasnt = ();
my @test = (['Filter::Util::Call','Filter'],
	    ['Text::Balanced','Text::Balanced'],
	   );	
for my $mod (@test) {
  eval "use $mod->[0]";
  ok !$@;
  push @hasnt, $mod->[1] if $@;
}

if (@hasnt) {
        print STDERR << 'EOP';

********************************************************
* IMPORTANT: Your installation will not work since it  *
* lacks critical modules.                              *
* ALL TESTS WILL FAIL UNLESS YOU IMMEDIATELY           *
* INSTALL THE FOLLOWING MODULES [available from CPAN]: *
*
EOP

    for (@hasnt) { print STDERR "*\t$_\n" }


    print STDERR << 'EOP';
*                                                      *
* Please install the missing module(s) and start the   *
* PDL build process again (perl Makefile.PL; ....)     *
*                                                      *
********************************************************

EOP

}
