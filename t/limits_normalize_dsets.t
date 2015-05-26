use PDL;

use Test::More;
use Test::Exception;

use strict;
use warnings;

BEGIN {
	eval {
		require PDL::Slatec;
		require PDL::Graphics::Limits;
		1;
	} or plan skip_all => 'could not load modules';
}

plan tests => 21;

*normalize_dsets = \&PDL::Graphics::Limits::normalize_dsets;
*parse_vecspecs = \&PDL::Graphics::Limits::parse_vecspecs;

my $x1 = pdl( 1, 2 );
my $y1 = pdl( 1, 2 );

my $xn = pdl( 0.5, 0.5 );
my $xp = pdl( 0.25, 0.25 );

my $x2 = pdl( 2, 3 );
my $y2 = pdl( 2, 4 );

my %errs = ( en => undef, ep => undef );
my %attr = ( KeyCroak => 1 );

my @rdsets = (
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

my $args = { %attr, KeySpec => [ { data => 'x' }, { data => 'y' }, ] };

{
	my @udsets = ( [ $x1, $y1 ],
		    [ $x2, $y2 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );


	my %d1 = %{$dsets[0]};
	for (keys(%d1)) {
	    print "1: $_: $d1{$_}\n";
	    my @d1 = @{$d1{$_}};
	    print "  @d1\n";
	    }
	my %d2 = %{$dsets[1]};
	for (keys(%d2)) {
	    print "2: $_: $d2{$_}\n";
	    my @d2 = @{$d2{$_}};
	    print "  @d2\n";
	    }

	is_deeply( \@dsets, \@rdsets, "array" );
}


{
	my @udsets = ( [ { x => $x1, y => $y1 },
		      { x => $x2, y => $y2 } ] );
	my @dsets = normalize_dsets( $args, @udsets );
	is_deeply( \@dsets, \@rdsets, "hash" );
}


{
	my @udsets = ( [ { x => $x1, y => $y1 },
		      { x => $x2, y => $y2, z => 0 } ] );
	my @dsets = normalize_dsets( $args, @udsets );
	is_deeply( \@dsets, \@rdsets, "hash, extra data" );
}


{
	my @udsets = (  [ $x1, $y1 ],
		     [ { x => $x2, y => $y2 } ] );
	my @dsets = normalize_dsets( $args, @udsets );
	is_deeply( \@dsets, \@rdsets, "array and hash" );
}

#############################################################

{
	my @udsets = (  $x1, $y1, [ { x => $x2, y => $y2 } ] );
	throws_ok {
		my @dsets = normalize_dsets( $args,, @udsets );
	} qr/same dimensionality/, "dimensions not equal";
}

{
	my @udsets = (  [ $x1, $y1 ], [ $x1, { x => $x2, y => $y2 } ] );
	throws_ok {
		my @dsets = normalize_dsets( $args, @udsets )
	} qr/unexpected data type/, "bad arg mix";
}

{
	my @udsets = ( [ $x1, $y1 ], [ { x => $x2, y => $y2 } ] );
	lives_ok {
		my @dsets = normalize_dsets( $args, @udsets );
	} "array hash combo";
}

#############################################################

{
	my @udsets = (  [ $x1, $y1 ] );
	my @dsets = normalize_dsets( { %attr, Trans => [ \&log ] }, @udsets );

	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "array: global x trans" );
}

{
	my @udsets = (  [ [ $x1, \&log ], $y1 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "array: local x trans" );
}

{
	my @udsets = (  [ [ $x1, \&log ], $y1 ] );
	my @dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "array: local override x trans" );
}

{
	my @udsets = (  [ [ $x1, undef, undef, undef ], $y1 ] );
	my @dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1 },
			       { data => $y1 },
				]
		    , "array: local undef x trans" );
}

#############################################################

my $keys = [ qw( x y ) ];
my %keys = ( KeySpec => parse_vecspecs( $keys ) );
{
	my $udsets = [  { x => $x1, y => $y1 } ];
	my @dsets = normalize_dsets( { %attr, %keys, Trans => [ \&log ] }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "hash: global x trans" );
}


{
	my $udsets = [ { x => $x1, trans => \&log , y => $y1 } => ( '&trans' ) ];
	my @dsets = normalize_dsets( { %attr, %keys }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "hash: local x trans 1" );
}


{
	my $udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ];
	my @dsets = normalize_dsets( { %attr, KeySpec => [] }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "hash: local x trans 2" );
}

{
	my $udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ];
	my @dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1, trans => \&log },
			       { data => $y1 },
				]
		    , "hash: local override x trans" );
}

{
	my $udsets = [ { x => $x1, trans => undef , y => $y1 } => qw( x&trans y ) ];
	my @dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1 },
			       { data => $y1 },
				]
		    , "hash: local undef x trans 1" );
}

{
	my $udsets = [ { x => $x1, y => $y1 } => qw( x& y ) ];
	my @dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
	is_deeply( $dsets[0]{Vectors}, [
			       { data => $x1 },
			       { data => $y1 },
				]
		    , "hash: local undef x trans 2" );
}


#############################################################

{
	my @udsets = ( [ [ $x1, $xn ], $y2 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );
	my $exp = [ { data => $x1, errn => $xn, errp => $xn }, { data => $y2, } ];
	is_deeply( $dsets[0]{Vectors}, $exp, "array: symmetric errors" );
}

{
	my @udsets = ( [ [ $x1, $xn, $xp ], $y2 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );
	my $exp = [ { data => $x1, errn => $xn, errp => $xp }, { data => $y2, } ];
	is_deeply( $dsets[0]{Vectors}, $exp, "array: asymmetric errors 1" );
}

{
	my @udsets = ( [ [ $x1, undef, $xp ], $y2 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );
	my $exp = [ { data => $x1, errp => $xp }, { data => $y2, } ];
	is_deeply( $dsets[0]{Vectors}, $exp, "array: asymmetric errors 2" );
}

{
	my @udsets = ( [ [ $x1, $xn, undef ], $y2 ] );
	my @dsets = normalize_dsets( { %attr }, @udsets );
	my $exp = [ { data => $x1, errn => $xn }, { data => $y2, } ];
	is_deeply( $dsets[0]{Vectors}, $exp, "array: asymmetric errors 3" );
}

############################################

