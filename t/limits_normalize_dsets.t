
use Test::More;
use PDL;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 21;
  } else {
    plan skip_all => 'PDL::Slatec not available';
  }
};


*normalize_dsets = \&PDL::Graphics::Limits::normalize_dsets;
*parse_vecspecs = \&PDL::Graphics::Limits::parse_vecspecs;

# temporarily disable warnings to turn off Perl's
# redefinition warning
my $oldw;
BEGIN {
  $oldw = $^W;
  $^W=0;
}

# so can use eq_array w/ piddles. 
{
  package PDL;
  use overload 'eq' => \&PDL::eq,
    'bool' => sub { $_[0]->and } ;
}

BEGIN {
  $^W=$oldw;
}

$x1 = pdl( 1, 2 );
$y1 = pdl( 1, 2 );

$xn = pdl( 0.5, 0.5 );
$xp = pdl( 0.25, 0.25 );

$x2 = pdl( 2, 3 );
$y2 = pdl( 2, 4 );

my %errs = ( en => undef, ep => undef );
%attr = ( KeyCroak => 1 );

@rdsets = (
	    { MinMax => [ [ undef, undef], 
			  [ undef, undef] 
			],
	      Vectors => [ { data => $x1 },
			 {
			  data => $y1 } 
			 ]
	    },

	    { MinMax => [ [ undef, undef], 
			  [ undef, undef] 
			],
	      Vectors => [ { data => $x2 },
			 {
			  data => $y2 } 
			 ]
	    },
	  );


@udsets = ( [ $x1, $y1 ], 
	    [ $x2, $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
ok( eq_array( \@dsets, \@rdsets ), "array" );


my $args = { %attr, KeySpec => [ { data => 'x' }, { data => 'y' }, ] };

@udsets = ( [ { x => $x1, y => $y1 }, 
	      { x => $x2, y => $y2 } ] );
@dsets = normalize_dsets( $args, @udsets );
ok( eq_array( \@dsets, \@rdsets ), "hash" );


@udsets = ( [ { x => $x1, y => $y1 }, 
	      { x => $x2, y => $y2, z => 0 } ] );
@dsets = normalize_dsets( $args, @udsets );
ok( eq_array( \@dsets, \@rdsets ), "hash, extra data" );


@udsets = (  [ $x1, $y1 ], 
	     [ { x => $x2, y => $y2 } ] );
@dsets = normalize_dsets( $args, @udsets );
ok( eq_array( \@dsets, \@rdsets ), "array and hash" );

#############################################################

@udsets = (  $x1, $y1, [ { x => $x2, y => $y2 } ] );
eval { 
  @dsets = normalize_dsets( $args,, @udsets );
};
ok( $@ =~ /same dimensionality/, "dimensions not equal" );

@udsets = (  [ $x1, $y1 ], [ $x1, { x => $x2, y => $y2 } ] );
eval {@dsets = normalize_dsets( $args, @udsets ); };
ok( $@ =~ /unexpected data type/, "bad arg mix" );

@udsets = ( [ $x1, $y1 ], [ { x => $x2, y => $y2 } ] );
eval { 
  @dsets = normalize_dsets( $args, @udsets );
};
ok( !$@, "array hash combo" );

#############################################################

@udsets = (  [ $x1, $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&log ] }, @udsets );

ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "array: global x trans" );

@udsets = (  [ [ $x1, \&log ], $y1 ] ); 
@dsets = normalize_dsets( { %attr }, @udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "array: local x trans" );

@udsets = (  [ [ $x1, \&log ], $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "array: local override x trans" );

@udsets = (  [ [ $x1, undef, undef, undef ], $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), "array: local undef x trans" );

#############################################################

$keys = [ qw( x y ) ];
%keys = ( KeySpec => parse_vecspecs( $keys ) );
$udsets = [  { x => $x1, y => $y1 } ]; 
@dsets = normalize_dsets( { %attr, %keys, Trans => [ \&log ] }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "hash: global x trans" );


$udsets = [ { x => $x1, trans => \&log , y => $y1 } => ( '&trans' ) ]; 
@dsets = normalize_dsets( { %attr, %keys }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "hash: local x trans 1" );


$udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [] }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "hash: local x trans 2" );

$udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), "hash: local override x trans" );

$udsets = [ { x => $x1, trans => undef , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), "hash: local undef x trans 1" );

$udsets = [ { x => $x1, y => $y1 } => qw( x& y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
ok( eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), "hash: local undef x trans 2" );


#############################################################

@udsets = ( [ [ $x1, $xn ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn, errp => $xn }, { data => $y2, } ];
ok( eq_array( $dsets[0]{Vectors}, $exp), "array: symmetric errors" );

@udsets = ( [ [ $x1, $xn, $xp ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn, errp => $xp }, { data => $y2, } ];
ok( eq_array( $dsets[0]{Vectors}, $exp), "array: asymmetric errors 1" );

@udsets = ( [ [ $x1, undef, $xp ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errp => $xp }, { data => $y2, } ];
ok( eq_array( $dsets[0]{Vectors}, $exp), "array: asymmetric errors 2" );

@udsets = ( [ [ $x1, $xn, undef ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn }, { data => $y2, } ];
ok( eq_array( $dsets[0]{Vectors}, $exp), "array: asymmetric errors 3" );
