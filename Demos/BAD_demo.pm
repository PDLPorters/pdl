package PDL::Demos::BAD_demo;
use Carp;
require File::Spec;

# try and find m51.fits
my @f = qw(PDL Demos m51.fits);
our $m51file = undef;
foreach my $path ( @INC ) {
    my $file = File::Spec->catfile( $path, @f );
    if ( -f $file ) { $m51file = $file; last; }
}
confess "Unable to find m51.fits within the perl libraries.\n"
    unless defined $m51file;

my @demos = (
[comment => q|
    Welcome to this tour of the bad value support in PDL

    Each ndarray contains a flag - accessible via the badflag() method -
    which indicates whether:

       the ndarray contains no bad values (flag equals 0)
       the ndarray *MAY* contain bad values (flag equals 1)

    If the flag is set, then the routines (well, those that have been
    converted) will process these bad values correctly, otherwise they
    are ignored.

    The code has been written so as to provide as little overhead as
    possible; therefore there should be almost no difference in the
    time it takes to process ndarrays which do not have their bad flag
    set.
|],

[act => q|
    # create an ndarray
    $x = byte(1,2,3);
    print( "Bad flag (x) == ", $x->badflag(), "\n" );

    # set bad flag, even though all the data is good
    $x->badflag(1);
    print( "Bad flag (x) == ", $x->badflag(), "\n" );

    # note the bad flag is infectious
    $y = 2 * $x;
    print( "Bad flag (y) == ", $y->badflag(), "\n\n" );
|],

[act => q|
    # the badflag is also included in the state info of
    # ndarray
    #
    $z = pdl(2,3); # just an ndarray without the badflag set

    print "   Type   Dimension        State          Mem\n";
    print "-------------------------------------------------\n";
    print "x ", $x->info("%-6T %-15D   %-5S  %12M"), "\n";
    print "y ", $y->info("%-6T %-15D   %-5S  %12M"), "\n";
    print "z ", $z->info("%-6T %-15D   %-5S  %12M"), "\n\n";
|],

[act => q|
    print "No bad values:   $x\n";
    # set the middle value bad
    $x->setbadat(1);

    # now print out
    print "Some bad values: $x\n";
    print "b contains:      $y\n";
    $z = $x + $y;
    print "so x + y =       $z\n\n";
|],

[act => q|
    # The module PDL::Bad contains a number of routines designed
    # to make using bad values easy.
    print "x contains ", $x->nbad, " bad elements.\n";
    print "The bad value for type #",$x->get_datatype," is ",$x->badvalue,"\n";
    print "It is easy to find whether a value is good: ", isgood($x), "\n\n";

    print "or to remove the bad values\n";
    $x->inplace->setbadtoval(23);
    print "x = $x and \$x->badflag == ", $x->badflag, "\n\n";
|],

[act => q|
    print "We can even label certain values as bad!\n";
    $x = sequence(3,3);
    $x = $x->setbadif( $x % 2 ); # unfortunately can not be done inplace
    print $x;
|],

[act => q|
    # the issue of how to cope with dataflow is not fully resolved. At
    # present, if you change the badflag of an ndarray, all its children
    # are also changed:
    $x = sequence( byte, 2, 3 );
    $x = $x->setbadif( $x == 3 );
    $y = $x->slice("(1),:");
    print "y = $y\tbadflag = ", $y->badflag, "\n";

    $x->inplace->setbadtoval(3);
    print "y = $y\tbadflag = ", $y->badflag, "\n\n";
|],

[act => q|
    # Note that "boolean" operators return a bad value if either of the
    # operands are bad: one way around this is to replace all bad values
    # by 0 or 1.

    $x = sequence(3,3); $x = $x->setbadif( $x % 2 );
    print $x > 5;
    print setbadtoval($x > 5,0);  # set all bad values to false
|],

[act => q|
    # One area that is likely to cause confusion is the return value from
    # comparison operators (e.g. all and any) when ALL elements are bad.
    # The bad value is returned; if used in boolean context this causes
    # an exception, since it is neither true nor false.

    # There is also the fact that the bad value need not relate to the
    # type of the input ndarray (due to internal conversion to an 'int +').

    $x = ones(3); $x = $x->setbadif( $x == 1 );
    print "Any returns: ", any( $x > 2 ), "\n";
    print "which is the bad value of 'long' (", long->badvalue, ").\n";

    print "Whereas the bad value for \$x is: ", $x->badvalue, "\n";
|],

[comment => q|
    Many of the 'core' routines have been converted to handle bad values.
    However, some (including most of the additional modules) have not,
    either because it does not make sense or it's too much work to do!

    To find out the status of a particular routine, use the 'badinfo'
    command in perldl or pdl2 shell (this information is also included
    when you do 'help'), or the '-b' switch of pdldoc.
|],

(!eval { require PDL::Graphics::Simple; PDL::Graphics::Simple->import; 1 })
? [comment => q|
    The rest of this demo is just a bit of eye-candy to show bad values in
    action, and requires PDL::Graphics::Simple support in PDL which is
    unavailable. Ending.
|]
: (

[comment => q|
    This demo is just a bit of eye-candy to show bad values in action,
    and requires PDL::Graphics::Simple support in PDL. It makes use of
    the image of M51 kindly provided by the Hubble Heritage group at
    the Space Telescope Science Institute.

    It also serves to demonstrate that you often don't need to change
    your code to handle bad values, as the routines may 'do it' for you.
|],

[act => q|
    # read in the image ($m51file has been set up by this demo to
    # contain the location of the file)
    $m51 = rfits $|.__PACKAGE__.q|::m51file;

    # display it
    $just = { JUSTIFY => 1 };
    imag $m51, $just;

    # These are used to create the next image
    ( $nx, $ny ) = $m51->dims;
    $centre = [ $nx/2, $ny/2 ];
|],

[act => q|
    # now, let's mask out the central 40 pixels and display it
    $masked = $m51->setbadif( $m51->rvals({CENTRE=>$centre}) < 40 );

    # since imag auto-scales the output, the bad values are not displayed
    imag $masked, $just;

    # compare the statistics of the images
    print "Original:\n", $m51->stats, "\n";
    print "Masked:\n",   $masked->stats, "\n";
|],

[act => q|
    # let's filter it a little bit
    use PDL::Image2D;
    $nb = 15;
    $filtered = med2d $masked, ones($nb,$nb), { Boundary => 'Truncate' };

    # this is a model of the diffuse component of M51
    imag $filtered, $just;
|],

[act => q|
    # unsharp masking, to bring out the small-scale detail
    $unsharp = $masked - $filtered;

    imag $unsharp, $just;
|],

[act => q|
    # add on some contours showing the large scale structure of the galaxy
    imag $unsharp, $just;
    hold;
    cont $filtered;
    release;
|],

[actnw => q|
    # close the graphics window
    erase;
|],
) # end of graphics-only bit
);
$@ = ''; # reset

sub info {('bad', 'Bad-value demo (Optional: PDL::Graphics::Simple)')}

sub demo { @demos }
sub init { 'eval "use PDL::Graphics::Simple"' }

1;
