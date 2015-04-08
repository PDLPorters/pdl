# using this because XSUBs won't do INCLUDE_COMMAND in C part before MODULE
use PDL::Core::Dev;
my $file = shift @ARGV;
open my $fh, '>', $file or die "$file: $!";
select $fh;
PDL::Core::Dev::datatypes_switch();
