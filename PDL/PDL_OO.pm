
# Main loader of PDL package (OO mode)

# import all the packages

{ # Scope

package PDL::OO;

# Need the dummy package declaration so import doesn't barf

} # Back to user namespace

use PDL::Core OO;
use PDL::Examples OO;
use PDL::Io OO;
use PDL::Graphics::PG OO;
use PDL::Graphics::IIS OO;

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
