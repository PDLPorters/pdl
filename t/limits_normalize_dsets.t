use PDL;

print "1..21\n";

my $got = 0;
eval{require PDL::Slatec;};
if(!$@) {$got = 1}

if($got) {
  eval{require PDL::Graphics::Limits;};
  if($@) {$got = 0}
  }

unless($got) {
  for(1..21){print "ok $_ - skipped\n"}
  exit;
  }

*normalize_dsets = \&PDL::Graphics::Limits::normalize_dsets;
*parse_vecspecs = \&PDL::Graphics::Limits::parse_vecspecs;

# temporarily disable warnings to turn off Perl's
# redefinition warning
my $oldw;
BEGIN {
  $oldw = $^W;
  $^W=0;
}

# so can use _eq_array w/ piddles. 
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
	    { MinMax => [ [ '', ''], 
			  [ '', ''] 
			],
	      Vectors => [ { data => $x1 },
			 {
			  data => $y1 } 
			 ]
	    },

	    { MinMax => [ [ '', ''], 
			  [ '', ''] 
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


%d1 = %{$dsets[0]};
for (keys(%d1)) {
    print "1: $_: $d1{$_}\n";
    my @d1 = @{$d1{$_}};
    print "  @d1\n";
    }
%d2 = %{$dsets[1]};
for (keys(%d2)) {
    print "2: $_: $d2{$_}\n";
    my @d2 = @{$d2{$_}};
    print "  @d2\n";
    }

_ok( _eq_array( \@dsets, \@rdsets ), 1, "array" );


my $args = { %attr, KeySpec => [ { data => 'x' }, { data => 'y' }, ] };

@udsets = ( [ { x => $x1, y => $y1 }, 
	      { x => $x2, y => $y2 } ] );
@dsets = normalize_dsets( $args, @udsets );
_ok( _eq_array( \@dsets, \@rdsets ), 2, "hash" );


@udsets = ( [ { x => $x1, y => $y1 }, 
	      { x => $x2, y => $y2, z => 0 } ] );
@dsets = normalize_dsets( $args, @udsets );
_ok( _eq_array( \@dsets, \@rdsets ), 3, "hash, extra data" );


@udsets = (  [ $x1, $y1 ], 
	     [ { x => $x2, y => $y2 } ] );
@dsets = normalize_dsets( $args, @udsets );
_ok( _eq_array( \@dsets, \@rdsets ), 4, "array and hash" );

#############################################################

@udsets = (  $x1, $y1, [ { x => $x2, y => $y2 } ] );
eval { 
  @dsets = normalize_dsets( $args,, @udsets );
};
_ok( $@ =~ /same dimensionality/, 5, "dimensions not equal" );

@udsets = (  [ $x1, $y1 ], [ $x1, { x => $x2, y => $y2 } ] );
eval {@dsets = normalize_dsets( $args, @udsets ); };
_ok( $@ =~ /unexpected data type/, 6, "bad arg mix" );

@udsets = ( [ $x1, $y1 ], [ { x => $x2, y => $y2 } ] );
eval { 
  @dsets = normalize_dsets( $args, @udsets );
};
_ok( !$@, 7, "array hash combo" );

#############################################################

@udsets = (  [ $x1, $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&log ] }, @udsets );

_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 8, "array: global x trans" );

@udsets = (  [ [ $x1, \&log ], $y1 ] ); 
@dsets = normalize_dsets( { %attr }, @udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 9, "array: local x trans" );

@udsets = (  [ [ $x1, \&log ], $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 10, "array: local override x trans" );

@udsets = (  [ [ $x1, undef, undef, undef ], $y1 ] ); 
@dsets = normalize_dsets( { %attr, Trans => [ \&sin ]}, @udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), 11, "array: local undef x trans" );

#############################################################

$keys = [ qw( x y ) ];
%keys = ( KeySpec => parse_vecspecs( $keys ) );
$udsets = [  { x => $x1, y => $y1 } ]; 
@dsets = normalize_dsets( { %attr, %keys, Trans => [ \&log ] }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 12, "hash: global x trans" );


$udsets = [ { x => $x1, trans => \&log , y => $y1 } => ( '&trans' ) ]; 
@dsets = normalize_dsets( { %attr, %keys }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 13, "hash: local x trans 1" );


$udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [] }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 14, "hash: local x trans 2" );

$udsets = [ { x => $x1, trans => \&log , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1, trans => \&log },
		       { data => $y1 },
			]
	    ), 15, "hash: local override x trans" );

$udsets = [ { x => $x1, trans => undef , y => $y1 } => qw( x&trans y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), 16, "hash: local undef x trans 1" );

$udsets = [ { x => $x1, y => $y1 } => qw( x& y ) ]; 
@dsets = normalize_dsets( { %attr, KeySpec => [], Trans => [\&sin] }, $udsets );
_ok( _eq_array( $dsets[0]{Vectors}, [
		       { data => $x1 },
		       { data => $y1 },
			]
	    ), 17, "hash: local undef x trans 2" );


#############################################################

@udsets = ( [ [ $x1, $xn ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn, errp => $xn }, { data => $y2, } ];
_ok( _eq_array( $dsets[0]{Vectors}, $exp), 18, "array: symmetric errors" );

@udsets = ( [ [ $x1, $xn, $xp ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn, errp => $xp }, { data => $y2, } ];
_ok( _eq_array( $dsets[0]{Vectors}, $exp), 19, "array: asymmetric errors 1" );

@udsets = ( [ [ $x1, undef, $xp ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errp => $xp }, { data => $y2, } ];
_ok( _eq_array( $dsets[0]{Vectors}, $exp), 20, "array: asymmetric errors 2" );

@udsets = ( [ [ $x1, $xn, undef ], $y2 ] );
@dsets = normalize_dsets( { %attr }, @udsets );
$exp = [ { data => $x1, errn => $xn }, { data => $y2, } ];
_ok( _eq_array( $dsets[0]{Vectors}, $exp), 21, "array: asymmetric errors 3" );

##############################################
##############################################

sub _ok {
    if($_[0]) {print "ok $_[1] - $_[2]\n"}
    else {print "not ok $_[1] - $_[2]\n"}
}
    
############################################

sub __deep_check {
    my($e1, $e2) = @_;
    my $ok = 0;

    my $eq;
    {
        # Quiet uninitialized value warnings when comparing undefs.
        no warnings; 

        if( $e1 eq $e2 ) {
            $ok = 1;
        }
        else {
            if( UNIVERSAL::isa($e1, 'ARRAY') and
                UNIVERSAL::isa($e2, 'ARRAY') )
            {
                $ok = _eq_array($e1, $e2);
            }
            elsif( UNIVERSAL::isa($e1, 'HASH') and
                   UNIVERSAL::isa($e2, 'HASH') )
            {
                $ok = _eq_hash($e1, $e2);
            }
            elsif( UNIVERSAL::isa($e1, 'REF') and
                   UNIVERSAL::isa($e2, 'REF') )
            {
                push @Data_Stack, { type => 'REF', vals => [$e1, $e2] };
                $ok = __deep_check($$e1, $$e2);
                pop @Data_Stack if $ok;
            }
            elsif( UNIVERSAL::isa($e1, 'SCALAR') and
                   UNIVERSAL::isa($e2, 'SCALAR') )
            {
                push @Data_Stack, { type => 'REF', vals => [$e1, $e2] };
                $ok = __deep_check($$e1, $$e2);
            }
            else {
                push @Data_Stack, { vals => [$e1, $e2] };
                $ok = 0;
            }
        }
    }

    return $ok;
}

############################################

sub _eq_array  {
    my($a1, $a2) = @_;
    return 1 if $a1 eq $a2;

    my $ok = 1;
    my $max = $#$a1 > $#$a2 ? $#$a1 : $#$a2;
    for (0..$max) {
        my $e1 = $_ > $#$a1 ? $DNE : $a1->[$_];
        my $e2 = $_ > $#$a2 ? $DNE : $a2->[$_];

        push @Data_Stack, { type => 'ARRAY', idx => $_, vals => [$e1, $e2] };
        $ok = __deep_check($e1,$e2);
        pop @Data_Stack if $ok;

        last unless $ok;
    }
    return $ok;
}

#############################################

sub _eq_hash {
    my($a1, $a2) = @_;
    return 1 if $a1 eq $a2;

    my $ok = 1;
    my $bigger = keys %$a1 > keys %$a2 ? $a1 : $a2;
    foreach my $k (keys %$bigger) {
        my $e1 = exists $a1->{$k} ? $a1->{$k} : $DNE;
        my $e2 = exists $a2->{$k} ? $a2->{$k} : $DNE;

        push @Data_Stack, { type => 'HASH', idx => $k, vals => [$e1, $e2] };
        $ok = __deep_check($e1, $e2);
        pop @Data_Stack if $ok;

        last unless $ok;
    }

    return $ok;
}


