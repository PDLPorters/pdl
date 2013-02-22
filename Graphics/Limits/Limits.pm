package PDL::Graphics::Limits;

use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
	limits
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	limits
);

our $VERSION = '0.01';
$VERSION = eval $VERSION;


# Preloaded methods go here.

use PDL::Core qw( cat pdl );
use PDL::Primitive qw( append );
use PDL::Fit::Polynomial;
use PDL::Options;
use PDL::Bad;
use Carp;
use POSIX qw( log10 );

use strict;
use warnings;

################################################################################
# figure out what's good in a piddle after a possible transformation which could
# generate Infs or NaN's.  If only everyone used PDL::Bad::UseNaN...
sub set_mask
{
  my ( $mask, $data ) = @_;

  if ( $PDL::Bad::Status )
  {
    my $badflag = $data->badflag();
    $data->badflag(1);

    $mask .= $PDL::Bad::UseNaN ? (! $data->isbad ) : ( $data->isfinite & ! $data->isbad );

    $data->badflag($badflag);
  } else
  {
    $mask .= $data->isfinite;
  }
}



{
  package PDL::Graphics::Limits::DSet;

  use PDL::Core qw( cat pdl );

  *set_mask = \*PDL::Graphics::Limits::set_mask;

  sub new
  {
    my $class = shift;
    my $self = bless {}, $class;

    my ( $min, $max ) = splice( @_, 0, 2 );

    $self->{Vectors} = [ @_ ];
    $self->{MinMax} = [ map{ [ $min, $max ] } 1..@{$self->{Vectors}} ];

    $self;
  }

  sub ndim { scalar @{$_[0]->{Vectors}} }

  sub validate
  {
    my ( $self, $attr) = @_;

    my $ivec = 0;
    my $n;
    foreach my $vec ( @{$self->{Vectors}} )
    {
      die( 'vector ', $ivec+1, ": no data?\n" )
	unless defined $vec->{data};

      $n = $vec->{data}->nelem unless defined $n;

      # if a data set vector has no transformation function, use the
      # default in $attr{Trans}
      $vec->{trans} = $attr->{Trans}[$ivec]
	if ! exists $vec->{trans} && exists $attr->{Trans}[$ivec];

      # remove explicitly undefined trans
      delete $vec->{trans}
	if exists $vec->{trans} && ! defined $vec->{trans};

      # ensure that data and errors have the same length.
      die( 'vector ', $ivec+1, ": attribute $_: ",
	   "inconsistent number of elements",
	   "expected $n, got ", $vec->{$_}->nelem, "\n" )
	foreach
	  grep { exists $vec->{$_} &&
		   defined $vec->{$_} &&
		     $vec->{$_}->nelem != $n }
	    qw( data en ep );
    }
    continue
    {
      $ivec++;
    }

  }

  sub vector
  {
    $_[0]->{Vectors}[$_[1]];
  }

  sub set_minmax
  {
    my ( $dset, $min, $max, $axis ) = @_;

    my $mm = $dset->{MinMax}[$axis];

    $mm->[0] = $min if defined $min;
    $mm->[1] = $max if defined $max;
  }

  sub upd_minmax
  {
    my ( $dset, $min, $max, $axis ) = @_;

    my $mm = $dset->{MinMax}[$axis];

    $mm->[0] = $min if $mm->[0] > $min;
    $mm->[1] = $max if $mm->[1] < $max;
  }

  sub get_minmax
  {
    my ( $dset ) = @_;
    cat( map { pdl( $dset->{MinMax}[$_] ) } 0..$dset->ndim-1 );
  }

  sub calc_minmax
  {
    my $dset = shift;

    my @axes = @_ ? ( $_[0] ) : ( 0 ..$dset->ndims-1 );

    $dset->calc_minmax_axis( $_ ) foreach @axes;
  }

  #####################################################################
  # determine the limits for a dataset.
  sub calc_minmax_axis
  {
    my ( $dset, $axis ) = @_;

    my $vec = $dset->{Vectors}[$axis];
    my $data = $vec->{data};

    my $xfrm = defined $vec->{trans};

    # we need the transformed data point min max in case
    # a transformed data + error is out of range of the transform
    # function (e.g. log(0)).

    my @minmax;

    # reuse these as much as possible to reduce memory hit
    my $tmp;
    my $mask = PDL::null;

    # i know of no way of determining whether a function can be applied inplace.
    # assume not.

    # if xfrm is true, $tmp will be an independent piddle, else its an alias for data
    # no need to create a new piddle unless necessary.
    $tmp = $xfrm ? $vec->{trans}->($data) : $data;
    set_mask( $mask, $tmp );
    push @minmax, $tmp->where($mask)->minmax;

    if ( defined $vec->{errn} )
    {
      # worry about not overwriting the original data!
      if ( $xfrm ) { $tmp .= $vec->{trans}->($data - $vec->{errn}) }
      else         { $tmp  = $data - $vec->{errn} }
      set_mask( $mask, $tmp );
      push @minmax, $tmp->where($mask)->minmax;
    }

    if ( defined $vec->{errp} )
    {
      # worry about not overwriting the original data!
      if ( $xfrm ) { $tmp .= $vec->{trans}->($data + $vec->{errp}) }
      else         { $tmp  = $data + $vec->{errp} }
      set_mask( $mask, $tmp );
      push @minmax, $tmp->where($mask)->minmax;
    }

    my ( $min, $max ) = PDL::Core::pdl( @minmax )->minmax;

    $dset->set_minmax( $min, $max, $axis );
  }

}

#####################################################################

# based upon PGPLOT's pgrnge routine.
sub range_frac
{
  my ( $axis, $frac, $zerofix ) = @_;

  my $expand = $frac * ( $axis->[1] - $axis->[0] );
  my $min = $axis->[0] - $expand;
  my $max = $axis->[1] + $expand;

  if ( $zerofix )
  {
    $min = 0.0
      if $min < 0 && $axis->[0] >= 0.0;

    $max = 0.0
      if $max > 0 && $axis->[1] <= 0.0;
  }

  @{$axis} = ( $min, $max );
}

#####################################################################

# based upon PGPLOT's pgrnd routine

#  routine to find the closest "round" number to X, a "round" number
#  being 1, 2 or 5 times a power of 10.

# If X is negative, round_pow(X) = -round_pow(abs(X)).
# If X is zero, the value returned is zero.

# round_pow( direction, $x )
# where direction is up, down, or both i.e.
#  $ub = round ( up => $x );
#  $lb = round ( down => $x );

our @nice = ( 1, 2, 5, 10 );
our %flip = ( 'up' => 'down', 'down' => 'up' );
sub round_pow
{
  my ( $what, $x ) = @_;

  croak( "incorrect number of arguments" )
    unless 2 == @_;

  if ( $x != 0.0 )
  {
    my $xx = abs($x);
    my $xlog = log10($xx);
    my $ilog = int($xlog);

    $what = $flip{$what} if $x < 0 ;

    $ilog--
      if ( $xlog <= 0 && ( 'down' eq $what || $xlog != $ilog ) )
	||
	  ( $xlog >  0 && 'down' eq $what && $xlog == $ilog ) ;

    my $pwr = 10 ** $ilog;
    my $frac = $xx / $pwr;

    my $i;
    if ( 'up' eq $what )
    {
      $i = 3;
      $i = 2 if $frac < $nice[2];
      $i = 1 if $frac < $nice[1];
      $i = 0 if $frac < $nice[0];
      my $t = ( $x < 0 ? -1 : 1 ) * $pwr * $nice[$i];
      if(abs($t - $x) < 0.0000001) {$i++}
    }

    elsif ( 'down' eq $what )
    {
      $i = 0;
      $i = 1 if $frac > $nice[1];
      $i = 2 if $frac > $nice[2];
      $i = 3 if $frac > $nice[3];
    }

    $x = ( $x < 0 ? -1 : 1 ) * $pwr * $nice[$i];
  }

  $x;
}

#####################################################################

sub setup_multi
{
  my ( $common, $dim, $keys ) = @_;

  my @arr;

  if ( 'ARRAY' eq ref $common )
  {
    return $common;
  }

  elsif ( 'HASH' eq ref $common )
  {
    @arr[ 0..($dim-1)] = map { $common->{$_->{data}} } @{$keys};
  }

  else
  {
    my $value = $common;
    @arr = ($value) x $dim;
  }

  \@arr;
}

#####################################################################
# normalize_dsets
#
# transform the user's heterogeneous list of data sets into a regular
# list of data sets, each with the form
#  { Vectors => \@vectors }
# where each vector is a hashref with the following keys:
#   { data => $data,
#     en   => $err_n,
#     ep   => $err_p,
#     trans => $trans }

sub normalize_dsets
{
  my ( $attr, @udsets ) = @_;
  my @dsets;

  while ( @udsets )
  {
    my $ds = shift @udsets;
    my $ref = ref $ds;

    # peek inside the array to see what's there.  we can have the following
    # [ scalar|piddle, scalar|piddle, ... ] -> a zero dimensional data set
    # [ \@a, \@b, \@c, \%d, ...  ]          -> a bunch of data sets
    # [ \%h, @keys ]                        -> a hash with its keys

    # scalar or piddle, turn it into its own data set
    if ( ! $ref || UNIVERSAL::isa($ds, 'PDL') )
    {
      push @dsets,
	PDL::Graphics::Limits::DSet->new( $attr->{Min}, $attr->{Max},
			    { data => PDL::Core::topdl( $ds ) } );
    }

    elsif ( 'ARRAY' eq $ref )
    {
      normalize_array( \@dsets, $attr, $ds );
    }

    else
    {
      die( "data set: ", scalar @dsets + 1,
	   "illegal type in data set list: not an arrayref, scalar, or piddle\n" );
    }

  }

  # ensure data sets have the same dimensions
  my %dim;
  $dim{$_->ndim}++ foreach @dsets;

  # whoops.  only one allowed
  die( "data sets do not all have the same dimensionality\n" )
    if keys %dim > 1;

  ( $attr->{dims} ) = keys %dim;

  # clean up datasets.
  my $idset = -1;
  foreach my $dset ( @dsets )
  {
    $idset++;

    eval { $dset->validate( $attr ) };
    if ( $@ )
    {
      chomp $@;
      die( "data set $idset: $@\n" );
    }
  }

  @dsets;
}

#####################################################################

# array refs in data set lists may be just a plain ol' data set, or
# it may contain a bunch of other stuff.  here we deal with a single
# array ref.  we tear it apart and (re)build data sets.
sub normalize_array
{
  my ( $dsets, $attr, $aref ) = @_;

  # if the first element is a hash, it's either a hash based data set
  # with a bunch of attributes specific to that hash:
  # [ \%h, @keys ]             -> a hash with its keys
  # in which case the rest of the elements are scalars, or its
  # all hashes.

  eval
  {
    if ( 'HASH' eq ref $aref->[0] )
    {

      # all hashes?
      if ( @$aref == grep { 'HASH' eq ref $_ } @$aref )
      {
	# can't do anything unless we've been told which hash keys
	# we should use, as this format doesn't allow local specification
	die( "must specify hash keys for hash based data set spec\n" )
	  unless defined $attr->{KeySpec} && scalar @{$attr->{KeySpec}};

	foreach ( @{$aref} )
	{
	  push @$dsets, normalize_hash_dset($attr, $_, @{$attr->{Keys}} );
	}
      }

      # hash + scalars?
      elsif ( @$aref > 1 && 1 == grep { ref $_ } @$aref )
      {
	push @$dsets, normalize_hash_dset( $attr, @{$aref} )
      }

      # something wrong
      else
      {
	die( "hash based data specification has an unexpected element" );
      }

    }

    # must be a list of vectors as either scalars, piddles, or array
    # refs (vectors with attributes)
    else
    {
      # for array based data sets, we have to accumulate vectors as we iterate
      # through the array. they are stored here
      my @vecs;

      for my $vec ( @$aref )
      {
	my $ref = ref $vec;

	eval
	{
	  # naked scalar or piddle: data vector with no attributes
	  if ( ! $ref || UNIVERSAL::isa($vec, 'PDL') )
	  {
	    push @vecs, { data => PDL::Core::topdl( $vec ) };
	  }

	  # array: data vector with attributes
	  elsif ( 'ARRAY' eq $ref )
	  {
	    push @vecs, normalize_array_vec( $vec );
	  }

	  else
	  {
	    die( 'vector ', @vecs+1, ": unexpected data type ($ref) in list of data sets\n" );
	  }
	};

	if ( $@ )
	{
	  chomp $@;
	  die( 'vector ', @vecs+1, ": $@\n" );
	}
      }

      push @$dsets,
	PDL::Graphics::Limits::DSet->new( $attr->{Min}, $attr->{Max}, @vecs )
	    if @vecs;
    }
  };

  if ( $@ )
  {
    chomp $@;
    die( 'data set ', @$dsets+1, ": $@\n" );
  }
}

#####################################################################

# parse an array based vector
sub normalize_array_vec
{
  my ( $vec ) = @_;

  # we should have
  #  [ $data, [ $err | $err_n, $err_p ], [ \&func ] ]

  my @el = @$vec;

  die( "too few or too many entries in array based data set spec\n" )
    if @el < 1 || @el > 4;

  my %vec;
  $vec{data} = PDL::Core::topdl( shift @el);

  # if last value is CODE, it's a trans
  $vec{trans} = pop @el if 'CODE' eq ref $el[-1];

  if ( exists $el[2] )
  {
    # if we have 3 elements and the last isn't undef, it's an error.
    # it can't be CODE as we'd have stripped it off in the last statement
    die( "illegal value for trans func: $el[2]\n" )
      if defined $el[2];

    # we need to turn off trans for this element
    $vec{trans} = undef;
    pop @el;
  }

  # two values? asymmetric errors
  if ( @el == 2 )
  {
    $vec{errn} = PDL::Core::topdl($el[0]) if defined $el[0];
    $vec{errp} = PDL::Core::topdl($el[1]) if defined $el[1];
  }

  # one value? symmetric errors
  elsif ( @el == 1 )
  {
    $vec{errn} = PDL::Core::topdl($el[0]) if defined $el[0];
    $vec{errp} = $vec{errn} if defined $vec{errn};
  }

  \%vec;
}

#####################################################################

# this takes a hash and a hash key spec and generates a regularized
# data set array of the form
# [ { data => $data, ep => ..., en => ..., trans => }, ... ]
sub normalize_hash_dset
{
  my ( $attr, $ds, @keys ) = @_;

  my $KeySpec = $attr->{KeySpec};

  my @dset;

  die( "too many local VecKeys (", scalar @keys,
       ") and global VecKeys (", scalar @{$KeySpec}, ")\n" )
    if @keys && @{$KeySpec} && @{$KeySpec} <= @keys;

  my @spec;

  # handle local keys
  if ( @keys )
  {
    my $nvec = 0;
    for my $key ( @keys )
    {
      my %spec;


      # parse the specs for this vector
      eval { %spec = parse_vecspec( $key ) };
      do { chomp $@; die( "vector $nvec: $@" ) }
	if $@;


      # now, merge it with the global KeySpecs

      if ( @{$KeySpec} )
      {
	my $Spec = $KeySpec->[$nvec];

	foreach ( keys %{$Spec} )
	{
	  # only copy from Spec if not present in spec
	  $spec{$_} = $Spec->{$_} if ! exists $spec{$_};
	}
      }

      push @spec, \%spec;
    }
    continue
    {
      $nvec++;
    }

    # handle case where local VecKeys are a subst of global VecKeys
    while ( @{$KeySpec} > @spec )
    {
      push @spec, $KeySpec->[$nvec++];
    }
  }

  # no local keys; use global KeySpec
  else
  {
    @spec = @{$KeySpec};
  }

  my $nvec = 0;
  for my $spec ( @spec )
  {
    $nvec++;
    my %vec;

    die( "vector $nvec: no data spec?\n" )
      unless exists $spec->{data};

    for my $el ( qw( data errn errp trans ) )
    {
      if ( exists $spec->{$el} )
      {

	# if not defined, don't bother looking for it in the data set
	unless ( defined $spec->{$el} )
	{
	  # trans is different from the others in that we need to pass
	  # it as undef if $spec->{trans} is undef (as full handling of
	  # trans is done elsewhere.
	  $vec{trans} = undef if 'trans' eq $el;
	}

	elsif ( exists $ds->{$spec->{$el}} )
	{
	  $vec{$el} = $ds->{$spec->{$el}};
	}

	elsif ( $attr->{KeyCroak} )
	{
	  die( "vector $nvec: missing key in data set hash: ", $spec->{$el}, "\n" )
	}
      }

    }

    # missing data; certainly a fatal error.
    die( "vector $nvec: no data for key $spec->{data}\n" )
      unless defined $vec{data};

    push @dset, \%vec;
  }

  PDL::Graphics::Limits::DSet->new( $attr->{Min}, $attr->{Max}, @dset );
}

#####################################################################
# parse specifications for a hash based data set.  These are the elements
# in the VecKeys attribute.  See the docs for more details.
# Returns a hashref with keys data, en, ep, trans

my $colre = qr/[^&<>=]/;

# these are the different specs available.
my %keyre = ( data => qr/^($colre+)/,
	      errn => qr/<($colre*)/,
	      errp => qr/>($colre*)/,
	      err  => qr/=($colre*)/,
	      trans => qr/\&($colre*)/
	      );

my %vecspeckeys = ( data => 1,
		    err  => 1,
		    errn => 1,
		    errp => 1,
		    trans => 1 );

sub parse_vecspec
{
  my ( $ukeys ) = @_;

  my %k;

  # do we get a hash?
  if ( 'HASH' eq ref $ukeys )
  {
    # complain about keys we don't use
    my @badkeys = grep { ! defined $vecspeckeys{$_} } keys %$ukeys;
    die( "illegal keys: ", join(' ,', @badkeys), "\n" )
      if @badkeys;

    # copy keys we need
    do { $k{$_} = $ukeys->{$_} if exists $ukeys->{$_} }
      foreach keys %vecspeckeys;

  }

  # parse the string.
  else
  {

    # make a local copy, as we modify it in place.
    my $keys = $ukeys;

    # deal with a "default" spec
    if ( ! defined $keys )
    {
      $keys = '';
    }
    else
    {
      # spaces and commas are there for human use only
      $keys =~ s/[\s,]//g;
    }


    # extract the known specs.
    my ( $what, $re );
    $keys =~ s/$re// and $k{$what} = $1 while( ($what, $re) = each %keyre);

    # if there's anything left, it's bogus
    die( "illegal key specification: $ukeys\n" )
      unless $keys eq '';

  }

  # check for consistent error bar specs
  die( "can't specify `=' with `<' or `>'\n" )
    if exists $k{err} && ( exists $k{errn} || exists $k{errp} );

  # error bars are always specified as positive and negative; turn a symmetric
  # spec into that
  $k{errn} = $k{errp} = $k{err} if exists $k{err};
  delete $k{err};

  # set empty values to undefined ones
  do { $k{$_} = undef if $k{$_} eq '' } foreach keys %k;

  %k;
}

sub parse_vecspecs
{
  my $keys = shift;
  my @specs;

  push @specs, { parse_vecspec($_) }
    foreach @$keys;

  \@specs;
}

#####################################################################
# normalize user supplied limits

sub parse_limits
{
  my ( $ndim, $spec, $KeySpec ) = @_;

  $spec = [] unless defined $spec;

  my @limits;

  # array containing limits (as arrays or scalars)
  if ( 'ARRAY' eq ref $spec )
  {
    # no limits; just move on
    unless ( @$spec )
    {
    }

    # multi-dimensional data sets
    elsif ( 'ARRAY' eq ref $spec->[0] )
    {
      my $ilim = 0;
      for my $vlim ( @$spec )
      {
	$ilim++;
	die( "Limit spec element $ilim: expected array ref\n" )
	  if 'ARRAY' ne ref $vlim;

	die( "Limit spec element $ilim: too many values\n" )
	  if @$vlim > 2;

	die( "Limit spec element $vlim: values must be scalars\n" )
	  if grep { ref $_ } @$vlim;

	my @lims = @$vlim;
	$lims[0] = undef unless defined $lims[0];
	$lims[1] = undef unless defined $lims[1];

	push @limits, \@lims;
      }
    }

    # one-dimensional data sets
    elsif ( ! ref $spec->[0] )
    {
      die( "unexpected non-scalar element in Limits spec\n" )
	if grep { ref $_ } @$spec;

      my @lims = @$spec;
      $lims[0] = undef unless defined $lims[0];
      $lims[1] = undef unless defined $lims[1];

      push @limits, \@lims;
    }

    push @limits, [ undef, undef ]
      while ( @limits != $ndim );

  }

  # hash containing vector names and limits
  elsif ( 'HASH' eq ref $spec )
  {
    # first ensure that VecKeys has been specified
    die( "cannot use Limits without VecKeys\n" )
      unless @$KeySpec;

    # make sure that we've got common keys.
    my %vecs = map { ( $_->{data} => 1) } @$KeySpec;

    # identify unknown vectors
    my @badvecs = grep { ! defined $vecs{$_} } keys %$spec;
    die( 'unknown vector(s): ', join(', ', @badvecs), "\n" )
      if @badvecs;

    # work our way through the KeySpec's, filling in values from
    # $spec as appropriate.
    for my $kspec ( @$KeySpec )
    {
      my @lims = ( undef, undef );
      if ( exists $spec->{$kspec->{data}} )
      {
	my $lspec = $spec->{$kspec->{data}};
	$lims[0]  = $lspec->{min} if exists $lspec->{min};
	$lims[1]  = $lspec->{max} if exists $lspec->{max};
      }
      push @limits, \@lims;
    }
  }

  # say what?
  else
  {
    die( "Limits attribute value must be a hashref or arrayref\n" );
  }

  map { { calc  => scalar ( grep { !defined $_ } @{$_} ), range => $_ } } @limits;
}



#####################################################################

sub limits
{
  my $attr = 'HASH' eq ref $_[-1] ? pop @_ : {};

  my @udsets = @_;

  my %attr = iparse( {
    Min => -1.8e308,
    Max => +1.8e308,
    Bounds => 'minmax',
    Clean => 'RangeFrac',
    RangeFrac => 0.05,
    ZeroFix => 0,
    VecKeys => [],
    KeyCroak => 1,
    Limits => [],
    Trans => [],
  }, $attr );

  # turn Trans and VecKeys into arrays if necessary; may be scalars for 1D
  # data sets
  $attr{$_} = [ $attr{$_} ]
    foreach grep { defined $attr{$_} && 'ARRAY' ne ref $attr{$_} }
      qw( VecKeys Trans );

  # parse vector key specs
  $attr{KeySpec} = parse_vecspecs( $attr{VecKeys} );

  # normalize data sets to make life easier later. also
  # counts up the number of dimensions and sets $attr{dims}
  my @dsets = normalize_dsets( \%attr, @udsets );

  # set up the Limits
  my @limits = parse_limits( $attr{dims}, $attr{Limits}, $attr{KeySpec} );

  if ( 'minmax' eq lc $attr{Bounds} )
  {
    for my $dim ( 0..$attr{dims}-1 )
    {
      # only calculate minmax values for those dims which need them.
      my $limits = $limits[$dim];

      foreach ( @dsets )
      {
	# calculate min & max
	$_->calc_minmax( $dim )
	  if $limits->{calc};

	# make sure we pay attention to user specified limits
	$_->set_minmax( @{$limits->{range}}, $dim );
      }
    }
  }

  elsif ( 'zscale' eq lc $attr{Bounds} )
  {
    croak( "zscale only good for dim = 2\n" )
      unless $attr{dims} == 2;

    foreach my $dset ( @dsets )
    {
      $dset->calc_minmax( 0 )
	if $limits[0]{calc};


      if ( $limits[1]{calc} )
      {
	my $y = $dset->vector(1)->{data};

	# this is a waste, as we don't care about the evaluated
	# fit values, just the min and max values.  since we
	# get them all anyway, we'll use them.

	my $mask = PDL::null;
	set_mask( $mask, $y );

	my $fit = fitpoly1d( $y->where($mask)->qsort, 2 );
	$dset->set_minmax( $fit->minmax, 1 );
      }

      $dset->set_minmax( @{$limits[$_]{range}}, $_ ) for 0,1;
    }
  }
  else
  {
    die( "unknown Bounds type: $attr{Bounds}\n" );
  }

  # derive union of minmax limits from data sets
  my $minmax = PDL::Core::null;
  $minmax = append( $minmax, $_->get_minmax ) foreach @dsets;

  # get overall minmax limits
  $minmax = cat(($minmax->minmaximum)[0,1])->transpose;

  my @minmax = map{ [ $minmax->slice(":,$_")->list ] } 0..$attr{dims}-1;

  if ( 'rangefrac' eq lc $attr{Clean} )
  {
    my $RangeFrac =
      setup_multi( $attr{RangeFrac}, $attr{dims}, $attr{KeySpec} );

    my $ZeroFix =
      setup_multi( $attr{ZeroFix}, $attr{dims}, $attr{KeySpec} );

    range_frac( $minmax[$_], $RangeFrac->[$_], $ZeroFix->[$_] )
      for 0..$attr{dims}-1;
  }

  elsif ( 'roundpow' eq lc $attr{Clean} )
  {
    $_ = [ round_pow( down => $_->[0] ),
	   round_pow( up   => $_->[1] ) ]
      foreach @minmax;
  }

  elsif ( 'none' eq lc $attr{Clean} )
  {
    # do nothing
  }

  else
  {
    die( "unknown Clean type: $attr{Clean}\n" );
  }

  if ( wantarray )
  {
    return map { ( @{$_} ) } @minmax;
  }
  else
  {
    my @key;
    if ( @{$attr{KeySpec}} )
    {
      @key = map { $_->{data} } @{$attr{KeySpec}};
    }
    else
    {
      @key = map { 'q' . $_ } ( 1 .. $attr{dims} );
    }

    return { map { ( $key[$_] => { min => $minmax[$_][0],
				   max => $minmax[$_][1] } ) }
	       0.. ( @minmax - 1 ) };
  }
}

1;


__END__

=pod

=head1 NAME

PDL::Graphics::Limits - derive limits for display purposes


=head1 DESCRIPTION

Functions to derive limits for data for display purposes

=head1 SYNOPSIS

  use PDL::Graphics::Limits;


=head1 FUNCTIONS

=head2 limits

=for ref

B<limits> derives global limits for one or more multi-dimensional sets
of data for display purposes.  It obtains minimum and maximum limits
for each dimension based upon one of several algorithms.

=for usage

  @limits = limits( @datasets );
  @limits = limits( @datasets, \%attr );
  $limits = limits( @datasets );
  $limits = limits( @datasets, \%attr );

=head3 Data Sets

A data set is represented as a set of one dimensional vectors, one per
dimension. All data sets must have the same dimensions.
Multi-dimensional data sets are packaged as arrays or hashs; one
dimensional data sets need not be.  The different representations may
be mixed, as long as the dimensions are presented in the same order.
Vectors may be either scalars or piddles.

=over 8

=item One dimensional data sets

One dimensional data sets may be passed directly, with no additional packaging:

  limits( $scalar, $piddle );

=item Data sets as arrays

If the data sets are represented by arrays, each vectors in each array
must have the same order:

  @ds1 = ( $x1_pdl, $y1_pdl );
  @ds2 = ( $x2_pdl, $y2_pdl );

They are passed by reference:

  limits( \@ds1, \@ds2 );

=item Data sets as hashes

Hashes are passed by reference as well, but I<must> be further
embedded in arrays (also passed by reference), in order that the last
one is not confused with the optional trailing attribute hash.  For
example:

  limits( [ \%ds4, \%ds5 ], \%attr );

If each hash uses the same keys to identify the data, the keys
should be passed as an ordered array via the C<VecKeys> attribute:

  limits( [ \%h1, \%h2 ], { VecKeys => [ 'x', 'y' ] } );

If the hashes use different keys, each hash must be accompanied by an
ordered listing of the keys, embedded in their own anonymous array:

  [ \%h1 => ( 'x', 'y' ) ], [ \%h2 => ( 'u', 'v' ) ]

Keys which are not explicitly identified are ignored.

=back

=head3 Errors

Error bars must be taken into account when determining limits; care
is especially needed if the data are to be transformed before plotting
(for logarithmic plots, for example).  Errors may be symmetric (a single
value indicates the negative and positive going errors for a data point) or
asymmetric (two values are required to specify the errors).

If the data set is specified as an array of vectors, vectors with
errors should be embedded in an array. For symmetric errors, the error
is given as a single vector (piddle or scalar); for asymmetric errors, there
should be two values (one of which may be C<undef> to indicate
a one-sided error bar):

  @ds1 = ( $x,                  # no errors
           [ $y, $yerr ],       # symmetric errors
           [ $z, $zn, $zp ],    # asymmetric errors
           [ $u, undef, $up ],  # one-sided error bar
           [ $v, $vn, undef ],  # one-sided error bar
         );

If the data set is specified as a hash of vectors, the names of the
error bar keys are appended to the names of the data keys in the
C<VecKeys> designations.  The error bar key names are always prefixed
with a character indicating what kind of error they represent:

	< negative going errors
	> positive going errors
	= symmetric errors

(Column names may be separated by commas or white space.)

For example,

  %ds1 = ( x => $x, xerr => $xerr, y => $y, yerr => $yerr );
  limits( [ \%ds1 ], { VecKeys => [ 'x =xerr', 'y =yerr' ] } );

To specify asymmetric errors, specify both the negative and positive going
errors:

  %ds1 = ( x => $x, xnerr => $xn, xperr => $xp,
           y => $y );
  limits( [ \%ds1 ], { VecKeys => [ 'x <xnerr >xperr', 'y' ] } );

For one-sided error bars, specify a column just for the side to
be plotted:

  %ds1 = ( x => $x, xnerr => $xn,
           y => $y, yperr => $yp );
  limits( [ \%ds1 ], { VecKeys => [ 'x <xnerr', 'y >yperr' ] } );


Data in hashes with different keys follow the same paradigm:

  [ \%h1 => ( 'x =xerr', 'y =yerr' ) ], [ \%h2 => ( 'u =uerr', 'v =verr' ) ]

In this case, the column names specific to a single data set override
those specified via the C<VecKeys> option.

  limits( [ \%h1 => 'x =xerr' ], { VecKeys => [ 'x <xn >xp' ] } )

In the case of a multi-dimensional data set, one must specify
all of the keys:

  limits( [ \%h1 => ( 'x =xerr', 'y =yerr' ) ],
                  { VecKeys => [ 'x <xn >xp', 'y <yp >yp' ] } )

One can override only parts of the specifications:

  limits( [ \%h1 => ( '=xerr', '=yerr' ) ],
                  { VecKeys => [ 'x <xn >xp', 'y <yp >yp' ] } )

Use C<undef> as a placeholder for those keys for which
nothing need by overridden:

  limits( [ \%h1 => undef, 'y =yerr' ],
                  { VecKeys => [ 'x <xn >xp', 'y <yp >yp' ] } )

=head3 Data Transformation

Normally the data passed to B<limits> should be in their final,
transformed, form. For example, if the data will be displayed on a
logarithmic scale, the logarithm of the data should be passed to
B<limits>.  However, if error bars are also to be displayed, the
I<untransformed> data must be passed, as

  log(data) + log(error) != log(data + error)

Since the ranges must be calculated for the transformed values,
B<range> must be given the transformation function.

If all of the data sets will undergo the same transformation, this may
be done with the B<Trans> attribute, which is given a list of
subroutine references, one for each element of a data set.  An
C<undef> value may be used to indicate no transformation is to be
performed.  For example,

  @ds1 = ( $x, $y );

  # take log of $x
  limits( \@ds1, { trans => [ \&log10 ] } );

  # take log of $y
  limits( \@ds1, { trans => [ undef, \&log10 ] } );

If each data set has a different transformation, things are a bit more
complicated.  If the data sets are specified as arrays of vectors, vectors
with transformations should be embedded in an array, with the I<last>
element the subroutine reference:

  @ds1 = ( [ $x, \&log10 ], $y );

With error bars, this looks like this:

  @ds1 = ( [ $x, $xerr, \&log10 ], $y );
  @ds1 = ( [ $x, $xn, $xp, \&log10 ], $y );

If the C<Trans> attribute is used in conjunction with individual data
set transformations, the latter will override it.  To explicitly
indicate that a specific data set element has no transformation
(normally only needed if C<Trans> is used to specify a default) set
the transformation subroutine reference to C<undef>.  In this case,
the entire quad of data element, negative error, positive error, and
transformation subroutine must be specified to avoid confusion:

  [ $x, $xn, $xp, undef ]

Note that $xn and $xp may be undef. For symmetric errors, simply
set both C<$xn> and C<$xp> to the same value.

For data sets passed as hashes, the subroutine reference is an element
in the hashes; the name of the corresponding key is added to the list
of keys, preceded by the C<&> character:

  %ds1 = ( x => $x, xerr => $xerr, xtrans => \&log10,
           y => $y, yerr => $yerr );

  limits( [ \%ds1, \%ds2 ],
         { VecKeys => [ 'x =xerr &xtrans',  'y =yerr' ] });
  limits( [ \%ds1 => 'x =xerr &xtrans', 'y =yerr' ] );

If the C<Trans> attribute is specified, and a key name is also
specified via the C<VecKeys> attribute or individually for a data set
element, the latter will take precedence.  For example,

  $ds1{trans1} = \&log10;
  $ds1{trans2} = \&sqrt;

  # resolves to exp
  limits( [ \%ds1 ], { Trans => [ \&exp ] });

  # resolves to sqrt
  limits( [ \%ds1 ], { Trans => [ \&exp ],
                      VecKeys => [ 'x =xerr &trans2' ] });

  # resolves to log10
  limits( [ \%ds1 => '&trans1' ], { Trans => [ \&exp ],
                                   VecKeys => [ 'x =xerr &trans2' ] });


To indicate that a particular vector should have no transformation,
use a blank key:

  limits( [ \%ds1 => ( 'x =xerr &', 'y =yerr' ) ], [\%ds2],
	   { Trans => [ \&log10 ] } );

or set the hash element to C<undef>:

  $ds1{xtrans} = undef;


=head3 Range Algorithms

Sometimes all you want is to find the minimum and maximum values.  However,
for display purposes, it's often nice to have "clean" range bounds.  To that
end, B<limits> produces a range in two steps.  First it determines the bounds,
then it cleans them up.

To specify the bounding algorithm, set the value of the C<Bounds> key
in the C<%attr> hash to one of the following values:

=over 8

=item MinMax

This indicates the raw minima and maxima should be used.  This is the
default.

=item Zscale

This is valid for two dimensional data only.  The C<Y> values are sorted,
then fit to a line.  The minimum and maximum values of the evaluated
line are used for the C<Y> bounds; the raw minimum and maximum values
of the C<X> data are used for the C<X> bounds.  This method is good
in situations where there are "spurious" spikes in the C<Y> data which
would generate too large a dynamic range in the bounds.  (Note that
the C<Zscale> algorithm is found in IRAF and DS9; its true origin
is unknown to the author).

=back

To specify the cleaning algorithm, set the value of the C<Clean> key
in the C<%attr> hash to one of the following values:

=over 8

=item None

Perform no cleaning of the bounds.

=item RangeFrac

This is based upon the C<PGPLOT> B<pgrnge> function.  It symmetrically expands
the bounds (determined above) by a fractional amount:

    $expand = $frac * ( $axis->{max} - $axis->{min} );
    $min = $axis->{min} - $expand;
    $max = $axis->{max} + $expand;

The fraction may be specified in the C<%attr> hash with the
C<RangeFrac> key.  It defaults to C<0.05>.

Because this is a symmetric expansion, a limit of C<0.0> may be
transformed into a negative number, which may be inappropriate.  If
the C<ZeroFix> key is set to a non-zero value in the C<%attr> hash,
the cleaned boundary is set to C<0.0> if it is on the other side of
C<0.0> from the above determined bounds.  For example, If the minimum
boundary value is C<0.1>, and the cleaned boundary value is C<-0.1>,
the cleaned value will be set to C<0.0>.  Similarly, if the maximum
value is C<-0.1> and the cleaned value is C<0.1>, it will be set to C<0.0>.

This is the default clean algorithm.

=item RoundPow

This is based upon the C<PGPLOT> B<pgrnd> routine.  It determines a
"nice" value, where "nice" is the closest round number to
the boundary value, where a round number is 1, 2, or 5 times a power
of 10.

=back

=head3 User Specified Limits

To fully or partially override the automatically determined limits,
use the B<Limits> attribute.  These values are used as input to the
range algorithms.

The B<Limits> attribute value may be either an array of arrayrefs, or
a hash.

=over

=item Arrays

The B<Limits> value may be a reference to an array of arrayrefs, one
per dimension, which contain the requested limits.

The dimensions should be ordered in the same way as the datasets.
Each arrayref should contain two ordered values, the minimum and
maximum limits for that dimension.  The limits may have the undefined
value if that limit is to be automatically determined.  The limits
should be transformed (or not) in the same fashion as the data.

For example, to specify that the second dimension's maximum limit
should be fixed at a specified value:

  Limits => [ [ undef, undef ], [ undef, $max ] ]

Note that placeholder values are required for leading dimensions which
are to be handled automatically. For convenience, if limits for a
dimension are to be fully automatically determined, the placeholder
arrayref may be empty.  Also, trailing undefined limits may be
omitted.  The above example may be rewritten as:

  Limits => [ [], [ undef, $max ] ]

If the minimum value was specified instead of the maximum, the following
would be acceptable:

  Limits => [ [], [ $min ] ]

If the data has but a single dimension, nested arrayrefs are not required:

  Limits => [ $min, $max ]


=item Hashes

Th B<Limits> attribute value may be a hash; this can only be used in
conjunction with the B<VecKeys> attribute.  If the data sets are
represented by hashes which do not have common keys, then the user
defined limits should be specified with arrays.  The keys in the
B<Limits> hash should be the names of the data vectors in the
B<VecKeys>. Their values should be hashes with keys C<min> and C<max>,
representing the minimum and maximum limits.  Limits which have the value
C<undef> or which are not specified will be determined from the data.
For example,

  Limits => { x => { min => 30 }, y => { max => 22 } }

=back

=head3 Return Values

When called in a list context, it returns the minimum and maximum
bounds for each axis:

  @limits = ( $min_1, $max_1, $min_2, $max_2, ... );

which makes life easier when using the B<env> method:

  $window->env( @limits );

When called in a scalar context, it returns a hashref with the keys

  axis1, ... axisN

where C<axisN> is the name of the Nth axis. If axis names have not
been specified via the C<VecKeys> element of C<%attr>, names are
concocted as C<q1>, C<q2>, etc.  The values are hashes with keys
C<min> and C<max>.  For example:

  { q1 => { min => 1, max => 2},
    q2 => { min => -33, max => 33 } }

=head3 Miscellaneous

Normally B<limits> complains if hash data sets don't contain specific
keys for error bars or transformation functions.  If, however,
you'd like to specify default values using the C<%attr> argument,
but there are data sets which don't have the data and you'd rather
not have to explicitly indicate that, set the C<KeyCroak> attribute
to zero.  For example,

  limits( [ { x => $x }, { x => $x1, xerr => $xerr } ],
         { VecKeys => [ 'x =xerr' ] } );

will generate an error because the first data set does not have
an C<xerr> key.  Resetting C<KeyCroak> will fix this:

  limits( [ { x => $x }, { x => $x1, xerr => $xerr } ],
         { VecKeys => [ 'x =xerr' ], KeyCroak => 0 } );


=head1 AUTHOR

Diab Jerius, E<lt>djerius@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by the Smithsonian Astrophysical Observatory


This software is released under the GNU General Public License.
You may find a copy at L<http://www.fsf.org/copyleft/gpl.html>.

=cut
