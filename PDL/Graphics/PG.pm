
# Graphics functions for the PDL module, this module
# requires the PGPLOT module be previously installed.
# PGPLOT functions are also made available to the caller.

use PGPLOT; # For the caller

package PDL::Graphics::PG;

@EXPORT_OK = qw( dev hold release rel env bin cont errb line points
                 imag image ctab hi2d poly vect
);

use PGPLOT;               # For me
use PDL::Core;    # Grab the Core names
use SelfLoader; use Carp;

@ISA = qw( PDL::Exporter SelfLoader ); 

# Global variables for customisation, defaults are:

$AXISCOLOUR = 3;   # Axis colour
$SYMBOL     = 17;  # Plot symbol for points
$ERRTERM    = 1;   # Size of error bar terminators
$HARD_LW    = 4;   # Line width for hardcopy devices
$HARD_CH    = 1.4; # Character height for hardcopy devices
$HARD_FONT  = 2;   # Font for hardcopy devices

# Standard colour tables (args to ctab())

%CTAB = ();
$CTAB{Grey}    = [ pdl([0,1],[0,1],[0,1],[0,1]) ];
$CTAB{Igrey}   = [ pdl([0,1],[1,0],[1,0],[1,0]) ];
$CTAB{Fire}    = [ pdl([0,0.33,0.66,1],[0,1,1,1],[0,0,1,1],[0,0,0,1]) ];
$CTAB{Gray}    = $CTAB{Grey};  # Alias
$CTAB{Igray}   = $CTAB{Igrey}; # Alias
$DEV  = $ENV{"PGPLOT_DEV"} if defined $ENV{"PGPLOT_DEV"};
$DEV  = "?" if $DEV eq ""; # Safe default

END { # Destructor to close plot when perl exits
     my ($state,$len);
     pgqinf('STATE',$state,$len);
     pgend() if $state eq "OPEN";
}

1;# Exit with OK status

__DATA__

# SelfLoaded functions


############ Local functions #################

sub checkarg {  # Check/alter arguments utility
    my ($arg,$dims,$type) = @_;
    $type = $PDL_F unless defined $type;
    $arg = topdl($arg); # Make into a pdl
    $arg = convert($arg,$type) if $$arg{Datatype} != $type;
    croak "Data is not ${dims}D" if $#{$$arg{Dims}} != $dims-1;
    $_[0] = $arg; # Alter
1;}

sub pgdefaults{    # Set up defaults
    my ($hcopy,$len);
    pgask(0);
    pgqinf("HARDCOPY",$hcopy,$len);  
    if ($hcopy eq "YES") {   # hardcopy defaults
       pgslw($HARD_LW); pgsch($HARD_CH);     
       pgscf($HARD_FONT); 
    }
    pgsci(5); pgask(0);
}

sub initdev{  # Ensure a device is open
     my ($state,$len);
     pgqinf('STATE',$state,$len);
     dev() if ($state eq "CLOSED");
1;}

sub initenv{ # Default box
    my ($col); initdev(); pgqci($col); pgsci($AXISCOLOUR); 
    pgenv(@_,0,0); pgsci($col);
    @last = (@_,0,0); 
1;} 

sub redraw_axes {
    pgqci($col); pgsci($AXISCOLOUR);
    pgbox('BCNST',0,0,'BCNST',0,0) unless $hold;
    pgsci($col); 
}


sub CtoF77coords{  # convert a transform array from zero-offset to unit-offset images
    my $tr = pdl(shift); # Copy
    set($tr, 0, at($tr,0)-at($tr,1)-at($tr,2));
    set($tr, 3, at($tr,3)-at($tr,4)-at($tr,5));
    return $tr;
}

############ Exported functions #################

# Open/reopen the graphics device

sub dev {
    my ($dev,$nx,$ny) = @_;
    $dev = $DEV if $dev eq "";
    $nx = 1 unless defined $nx;
    $ny = 1 unless defined $ny;
    my ($state,$len);
    pgqinf('STATE',$state,$len);
    pgend() if ($state eq "OPEN");
    pgbegin(0,$dev,$nx,$ny);
    pgdefaults();
1;}

# hold/release functions for overlays

$hold = 0; 
sub hold    { $hold=1; print "Graphics on HOLD\n" if $PDL::verbose;}; 
sub release { $hold=0; print "Graphics RELEASED\n" if $PDL::verbose;};
sub rel     { release(); } # Alias

# set the envelope for plots and put auto-axes on hold

sub env {
    croak 'Usage: env ( $xmin, $xmax, $ymin, $ymax, [$just, $axis] )' 
       if ($#_==-1 && !defined(@last)) || ($#_>=0 && $#_<=2) || $#_>5;
    my(@args);
    @args = $#_==-1 ? @last : @_;         # No args - use previous
    $args[4] = 0 unless defined $args[4]; # $just 
    $args[5] = 0 unless defined $args[5]; # $axis 
    initdev();
    pgqci($col); pgsci($AXISCOLOUR);
    pgenv(@args);
    @last = @args;
    pgsci($col); 
    hold;
1;}


# Plot a histogram with pgbin()

sub bin {
    croak 'Usage: bin ( [$x,] $data )' if $#_<0 || $#_>1;
    my($x,$data) = @_;
    checkarg($x,1);

    my $n = nelem($x);
    if ($#_==1) {
       checkarg($data,1); croak '$x and $data must be same size' if $n!=nelem($data);
    }else{
       $data = $x; $x = float(sequence($n));
    }

    initenv( min($x), max($x), 0, max($data) ) unless $hold;
    PGPLOT::pgbin_r($n, $$x{Data}, $$data{Data}, 1);
1;}

# display a contour map of an image using pgconb()

sub cont {
    croak 'Usage: cont ( $image,  [$contours, $transform, $misval] )' if $#_<0 || $#_>3;
    my ($image, $contours, $tr, $misval) = @_;
    checkarg($image,2); 
    my($nx,$ny) = @{$$image{Dims}};

    $contours = min($image) + sequence(9)*(max($image)-min($image))/8  # Auto
                unless defined $contours;
    checkarg($contours,1);

    if (defined($tr)) {
       checkarg($tr,1);
       croak '$transform incorrect' if nelem($tr)!=6;
    }else{
       $tr = float [0,1,0, 0,0,1];
    }
    $tr = CtoF77coords($tr);
        
    initenv( 0,$nx-1, 0, $ny-1 ) unless $hold;
    print "Contouring $nx x $ny image from ",min($contours), " to ",
           max($contours), " in ",nelem($contours)," steps\n" if $PDL::verbose;
    
    if (defined($misval)) {
       PGPLOT::pgconb_r( $$image{Data}, $nx,$ny,1,$nx,1,$ny, $$contours{Data}, 
                         nelem($contours), $$tr{Data}, $misval);
    }else{
       PGPLOT::pgcons_r( $$image{Data}, $nx,$ny,1,$nx,1,$ny, $$contours{Data}, 
                         nelem($contours), $$tr{Data});
    }
1;}

# Plot errors with pgerrb()

sub errb {
    croak <<'EOD' if $#_<1 || $#_==4 || $#_>5;
Usage: errb ( $y, $yerrors )
       errb ( $x, $y, $yerrors )
       errb ( $x, $y, $xerrors, $yerrors )
       errb ( $x, $y, $xloerr, $xhierr, $yloerr, $yhierr)
EOD
    my @t = @_; my $i=0; my $n;
    for (@t) {
        checkarg($_, 1); 
        $n = nelem($_) if $i++ == 0;
        croak "Args must have same size" if nelem($_)!=$n;
    }
    my $x = $#t==1 ? float(sequence($n)) : $t[0];
    my $y = $#t==1 ? $t[0] : $t[1];
    initenv( min($x), max($x), min($y), max($y) ) unless $hold;
    PGPLOT::pgerrb_r(6,$n,$$x{Data},$$y{Data},$t[1]{Data},$ERRTERM) if $#t==1;
    PGPLOT::pgerrb_r(6,$n,$$x{Data},$$y{Data},$t[2]{Data},$ERRTERM) if $#t==2;
    PGPLOT::pgerrb_r(5,$n,$$x{Data},$$y{Data},$t[2]{Data},$ERRTERM) if $#t==3;
    PGPLOT::pgerrb_r(6,$n,$$x{Data},$$y{Data},$t[3]{Data},$ERRTERM) if $#t==3;
    if ($#t==5) {
       PGPLOT::pgerrb_r(1,$n,$$x{Data},$$y{Data},$t[3]{Data},$ERRTERM);
       PGPLOT::pgerrb_r(2,$n,$$x{Data},$$y{Data},$t[5]{Data},$ERRTERM);
       PGPLOT::pgerrb_r(3,$n,$$x{Data},$$y{Data},$t[2]{Data},$ERRTERM);
       PGPLOT::pgerrb_r(4,$n,$$x{Data},$$y{Data},$t[4]{Data},$ERRTERM);
    }
1;}


# Plot a line with pgline()

sub line {
    croak 'Usage: line ( [$x,] $y )' if $#_<0 || $#_>1;
    my($x,$y) = @_;
    checkarg($x,1);

    my $n = nelem($x);
    if ($#_==1) {
       checkarg($y,1); croak '$x and $y must be same size' if $n!=nelem($y);
    }else{
       $y = $x; $x = float(sequence($n));
    }

    initenv( min($x), max($x), min($y), max($y) ) unless $hold;
    PGPLOT::pgline_r($n, $$x{Data}, $$y{Data});
1;}

# Plot points with pgpnts()

sub points {
    croak 'Usage: points ( [$x,] $y, [$symbol(s)] )' if $#_<0 || $#_>2;
    my($x,$y,$sym) = @_;
    checkarg($x,1);

    my $n = nelem($x);
    if ($#_>=1) {
       checkarg($y,1); croak '$x and $y must be same size' if $n!=nelem($y);
    }else{
       $y = $x; $x = float(sequence($n));
    }
    $sym = $SYMBOL if $#_ != 2;
    checkarg($sym,1); my $ns = nelem($sym); $sym = long($sym); 

    initenv( min($x), max($x), min($y), max($y) ) unless $hold;
    PGPLOT::pgpnts_r($n, $$x{Data}, $$y{Data}, $$sym{Data}, $ns);
1;}

# display an image using pgimag()/pggray() as appropriate

sub imag {
    croak 'Usage: imag ( $image,  [$min, $max, $transform] )' if $#_<0 || $#_>2;
    my ($image,$min,$max,$tr) = @_;
    checkarg($image,2);
    my($nx,$ny) = @{$$image{Dims}};

    $min = min($image) unless defined $min;
    $max = max($image) unless defined $max;
    if (defined($tr)) {
       checkarg($tr,1);
       croak '$transform incorrect' if nelem($tr)!=6;
    }else{
       $tr = float [0,1,0, 0,0,1];
    }
    $tr = CtoF77coords($tr);

    initenv( -0.5,$nx-0.5, -0.5, $ny-0.5  ) unless $hold;
    print "Displaying $nx x $ny image from $min to $max ...\n" if $PDL::verbose;

    pgqcir($i1, $i2); # Colour range - if too small use pggray dither algorithm
    if ($i2-$i1<16) {
       PGPLOT::pggray_r( $$image{Data}, $nx,$ny,1,$nx,1,$ny, $max, $min, $$tr{Data});
    }
    else{
       ctab(Grey) unless $CTAB; # Start with grey
       PGPLOT::pgimag_r( $$image{Data}, $nx,$ny,1,$nx,1,$ny, $min, $max, $$tr{Data});
    }
    redraw_axes unless $hold; # Redraw box
1;}

sub image {imag(@_)};

# Load a colour table using pgctab()

sub ctab {

    # First indirect arg list through %CTAB

    my(@arg) = @_;
    if ($#arg>=0 && !ref($arg[0])) { # First arg is a name not an object
       my $name = ucfirst(lc(shift @arg)); # My convention is $CTAB{Grey} etc...
       croak "$name is not a standard colour table" unless defined $CTAB{$name};
       unshift @arg, @{$CTAB{$name}};
    }
    if ($#arg<0 || $#arg>5) {
       my @std = keys %CTAB;
       croak <<"EOD";
Usage: ctab ( \$name, [\$contrast, $\brightness] ) # Builtin col table
            [Builtins: @std]
       ctab ( \$ctab, [\$contrast, \$brightness] ) # $ctab is Nx4 array
       ctab ( \$levels, \$red, \$green, \$blue, [\$contrast, \$brightness] )
EOD
    }

    my($ctab, $levels, $red, $green, $blue, $contrast, $brightness, @t, $n);

    if ($#arg<3) { 
       ($ctab, $contrast, $brightness) = @arg;
       @t = @{$$ctab{Dims}}; croak 'Must be a Nx4 array' if $#t != 1 || $t[1] != 4;
       $n = $t[0];
       $ctab   = float($ctab) if $$ctab{Datatype} != $PDL_F;
       $levels = sec($ctab, 0, $n-1, 0,0);
       $red    = sec($ctab, 0, $n-1, 1,1);
       $green  = sec($ctab, 0, $n-1, 2,2);
       $blue   = sec($ctab, 0, $n-1, 3,3);
    }
    else {
       ($levels, $red, $green, $blue, $contrast, $brightness) = @arg;
       checkarg($levels,1);  $n = nelem($levels);
       for($red,$green,$blue) {
          checkarg($_,1); croak 'Arguments must have same size' unless nelem($_) == $n;
       } 
    }
          
    # Now load it

    $contrast   = 1   unless defined $contrast;
    $brightness = 0.5 unless defined $brightness;
    initdev();
    PGPLOT::pgctab_r( $$levels{Data}, $$red{Data}, $$green{Data}, $$blue{Data},
                      $n, $contrast, $brightness );
    $CTAB = 1; # Loaded
1;}

# display an image using pghi2d()

sub hi2d {
    croak 'Usage: hi2d ( $image, [$x, $ioff, $bias] )' if $#_<0 || $#_>3;
    my ($image, $x, $ioff, $bias) = @_;
    checkarg($image,2);
    my($nx,$ny) = @{$$image{Dims}};

    if (defined($x)) {
       checkarg($x,1);
       croak '$x incorrect' if nelem($x)!=$nx;
    }else{
       $x = float(sequence($nx));
    }
    $ioff = 1 unless defined $ioff;
    $bias = 5*max($image)/$ny unless defined $bias;
    $work = float(mkzero($nx));
        
    initenv( 0 ,2*($nx-1), 0, 10*max($image)  ) unless $hold;
    PGPLOT::pghi2d_r($$image{Data}, $nx, $ny, 1,$nx,1,$ny, $$x{Data}, $ioff, 
                     $bias, 1, $$work{Data});
1;}


# Plot a polygon with pgpoly()

sub poly {
    croak 'Usage: poly ( $x, $y )' if $#_<0 || $#_>1;
    my($x,$y) = @_;
    checkarg($x,1);
    checkarg($y,1);
    my $n = nelem($x);
    initenv( min($x), max($x), min($y), max($y) ) unless $hold;
    PGPLOT::pgpoly_r($n, $$x{Data}, $$y{Data});
1;}


# display a vector map of 2 images using pgvect()

sub vect {
    croak 'Usage: vect ( $a, $b, [$scale, $pos, $transform, $misval] )' if $#_<1 || $#_>5;
    my ($a, $b, $scale, $pos, $tr, $misval) = @_;
    checkarg($a,2); checkarg($b,2); 
    my($nx,$ny) = @{$$a{Dims}};
    my($n1,$n2) = @{$$b{Dims}};
    croak 'Dimensions of $a and $b must be the same' unless $n1==$nx && $n2==$ny;

    $scale = 0 unless defined $scale;
    $pos   = 0 unless defined $pos;

    if (defined($tr)) {
       checkarg($tr,1);
       croak '$transform incorrect' if nelem($tr)!=6;
    }else{
       $tr = float [0,1,0, 0,0,1];
    }
    $tr = CtoF77coords($tr);
        
    initenv( 0, $nx-1, 0, $ny-1  ) unless $hold;
    print "Vectoring $nx x $ny images ...\n" if $PDL::verbose;
    
    PGPLOT::pgvect_r( $$a{Data}, $$b{Data}, $nx,$ny,1,$nx,1,$ny, $scale, $pos, 
                      $$tr{Data}, $misval);
1;}

1;# Exit with OK status
