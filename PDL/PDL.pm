
# Main loader of PDL package

{ # Scope

package PDL;

} # Back to user namespace

# import all the packages

use PDL::Core;
use PDL::Examples;
use PDL::Io;
use PDL::Graphics::PG;
use PDL::Graphics::IIS;

# AutoLoading (in user scope)

@PDLLIB = (".",split(':',$ENV{"PDLLIB"})) if defined $ENV{"PDLLIB"}; 

sub AUTOLOAD { 
    local @INC = @INC;
    $AUTOLOAD =~ /::([^:]*)$/;
    my $func = $1;
    unshift @INC, @PDLLIB;
    eval {require "$func.pdl"};
    goto &$AUTOLOAD unless $@;
    die "Undefined subroutine $func() cannot be AutoLoaded\n";
}

;# Exit with OK status

1;
