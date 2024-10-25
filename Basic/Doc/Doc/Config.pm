package PDL::Doc::Config;

use Config;

$PDL::Doc::pager = $Config{pager};
$PDL::Doc::pager = $ENV{PAGER} if defined $ENV{PAGER};
$PDL::Doc::pager = $ENV{PERLDOC_PAGER} if defined $ENV{PERLDOC_PAGER};
$PDL::Doc::DefaultFile = $Config{man1direxp};

1;
