#  Can PDL::Lite be loaded twice?
#  The first import was interfering with the second.  

use Test::More tests => 10;

{
    package mk1;
    use PDL::Lite;

    sub x {
        return PDL->pdl (1..10);
    }
}

{
    package mk2;
    use PDL::Lite;

    sub x {
        return PDL->pdl (11..20);
    }
}

foreach my $name (qw /x barf pdl piddle null/) {
    ok (mk1->can($name), "Sub loaded: mk1::" . $name);
    ok (mk2->can($name), "Sub loaded: mk2::" . $name);
}

