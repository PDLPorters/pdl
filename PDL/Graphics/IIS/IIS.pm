
# Access to IIS graphics device (SAOimage, Ximtool, etc...)

package PDL::Graphics::IIS;

use PDL::Core; # Grab the Core names

@EXPORT_OK = qw( iis iiscur iiscirc $stdimage $iisframe saoimage ximtool );

use DynaLoader; use Carp;  use SelfLoader;
@ISA    = qw( PDL::Exporter DynaLoader SelfLoader ); 

# Load compiled code only on demand

BEGIN{ $iis_loaded = 0 }

local $^W=0;  # Do it this way to suppress spurious warnings
eval << 'EOD';
sub AUTOLOAD {
   unless ($iis_loaded) {
      bootstrap PDL::Graphics::IIS;
      print "Graphics::IIS loaded\n" if $PDL::verbose;
      $iis_loaded = 1;
   }
   $SelfLoader::AUTOLOAD = $AUTOLOAD;
   goto &SelfLoader::AUTOLOAD;
}
EOD

$iisframe    = 1;       # Starting defaults
$stdimage    = "imt1024";
$HOME        = $ENV{'HOME'};     # Used a lot so shorten

1; # OK status

__DATA__

# Start of SelfLoaded functions

################ Public routines #################

# Display 

sub iis {
   croak 'Usage: iis ( $image, [$min, $max] )' if $#_<0 || $#_>2;
   my($image,$min,$max)=@_;
   my($nx,$ny) = dims($image);
   fbconfig($stdimage) if $stdimage ne $last_stdimage;
   $min = min($image) unless defined $min;
   $max = max($image) unless defined $max;
   print "Displaying $nx x $ny image from $min to $max ...\n" if $PDL::verbose;
   iis_c($image,$min,$max);
1;}

# Return cursor

sub iiscur {
    croak 'Usage: ($x,$y) = iiscur($ch)' if $#_>=1;
    my ($x,$y,$ch) = iiscur_c();
    $_[0] = $ch; # Pass this back in args
    return ($x,$y);
}

# Draw circles

sub iiscirc {
   croak 'Usage: iiscirc( $x, $y, [$radius, $colour] )' if $#_<1 || $#_>3;
   my($x, $y, $radius, $colour)=@_;
   fbconfig($stdimage) if $stdimage ne $last_stdimage;
   $radius = 10 unless defined $radius;
   $colour = 204 unless defined $colour;
   iiscirc_c($x, $y, $radius, $colour);
1;}

sub saoimage {  # Start SAOimage
   fbconfig($stdimage) if $stdimage ne $last_stdimage;
   if( !($pid = fork)) {	# error or child
      exec("saoimage", -idev => $fifo, -odev => $fifi, @_) if defined $pid;
      die "Can't start saoimage: $!\n";
   }
   return $pid;
}

sub ximtool {  # Start Ximtool
   fbconfig($stdimage) if $stdimage ne $last_stdimage;
   if( !($pid = fork)) {	# error or child
      exec("ximtool", -xrm => "ximtool*input_fifo: $fifi", -xrm => "ximtool*output_fifo: $fifo", @_) if defined $pid;
      die "Can't start ximtool: $!\n";
   }
   return $pid;
}


################ Private routines #################

# Change the frame buffer configuration

sub fbconfig {
    my $name = shift;
    parseimtoolrc() unless $parsed++;
    findfifo() unless $foundfifo++;
    croak 'No frame buffer configuration "'.$name.'" found'."\n" 
       unless defined $imtoolrc{$name};
    ($fbconfig, $fb_x, $fb_y ) = @{ $imtoolrc{$name} };
    print "Using $stdimage - fbconfig=$fbconfig (${fb_x}x$fb_y)\n" if $PDL::verbose;;
    $last_stdimage = $stdimage;
1;}

# Try and find user/system imtoolrc definitions

sub parseimtoolrc {    
   # assoc array holds imtool configuations - init with some standard
   # ones in case imtoolrc goes missing

   %imtoolrc = ( 
     'imt512'  => [1,512,512],   'imt800'  => [2,800,800], 
     'imt1024' => [3,1024,1024], 'imt1600' => [4,1600,1600], 
     'imt2048' => [5,2048,2048], 'imt4096' => [6,4096,4096], 
   );

   # Look for imtoolrc file

   $imtoolrc = "/usr/local/lib/imtoolrc";
   $imtoolrc = "$HOME/.imtoolrc" if -e "$HOME/.imtoolrc";
   if (!-e $imtoolrc) {
      warn "WARNING: unable to find an imtoolrc file in $HOME/.imtoolrc\n".
           "or /usr/local/lib/imtoolrc. Will try \$stdimage = imt1024.\n";
      return 1;
   }

   # Load frame buffer configuartions from imtoolrc file and
   # store in assoc array

   open(IMTOOLRC, $imtoolrc) || die "File $imtoolrc not found";
    while(<IMTOOLRC>) { 
       if  ( /^\s*(\d+)\s+\d+\s+(\d+)\s+(\d+)\s+\#\s*(\S+)\s/ ) {
          foreach $name (split(/\|/,$4)) {
             $imtoolrc{$name} = [$1,$2,$3];
          }
      }
   }close(IMTOOLRC);
1;}

# Try a few obvious places for the FIFO pipe and create if necessary

sub findfifo {
    $fifi = ""; $fifo = "";
    if (-e "/dev/imt1i" && -e "/dev/imt1o") {
       $fifi = "/dev/imt1i"; $fifo = "/dev/imt1o";
    }
    if (-e "$HOME/dev/imt1i" && -e "$HOME/dev/imt1o") {
       $fifi = "$HOME/dev/imt1i"; $fifo = "$HOME/dev/imt1o";
    }
    if (-e "$HOME/iraf/dev/imt1i" && -e "$HOME/iraf/dev/imt1o") {
       $fifi = "$HOME/iraf/dev/imt1i"; $fifo = "$HOME/iraf/dev/imt1o";
    }
    if (defined $ENV{'IMTDEV'} && $ENV{'IMTDEV'} =~ /^fifo:(.*):(.*)$/) {
       $fifi = $1; $fifo = $2;
   }
   if ($fifi eq "" && $fifo eq "") { # Still not found use this default
       warn "WARNING: cannot locate FIFO pipes in /dev/, $HOME/dev, ".
           "$HOME/iraf/dev or environment variable \$IMTDEV\n"; 
       $fifi = "$HOME/dev/imt1i"; $fifo = "$HOME/dev/imt1o";
   }
   print "Using FIFO devices in:  $fifi\n".
         "                   out: $fifo\n" if $PDL::verbose;
   for $pipe ($fifi, $fifo) {
      if (!-p $pipe) {
         print "FIFO $pipe does not exist - try and create now? "; my $ans = <STDIN>;
         system "/usr/etc/mknod $pipe p" if $ans =~ /^y/i;

         if ($ans =~ /^y/i) {
            unlink $pipe if -e $pipe;
            my $path = $ENV{PATH};
            $ENV{PATH} .= ":/etc:/usr/etc";

            # Note system return value is backwards - hence 'and'

            if ( system('mknod', $pipe, 'p') and system('mkfifo',$pipe) ) { 
                die "Failed to create named pipe $pipe\n";
            }
            $ENV{PATH} = $path;
         }
      }
   }
1;}

1; # Exit with OK status



