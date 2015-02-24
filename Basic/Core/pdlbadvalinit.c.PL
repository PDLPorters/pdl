# using this because XSUBs won't do INCLUDE_COMMAND in BOOT:
use PDL::Core::Dev;
my $file = shift @ARGV;
open my $fh, '>', $file or die "$file: $!";
select $fh;
PDL::Core::Dev::generate_badval_init();
