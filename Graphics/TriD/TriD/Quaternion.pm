##############################################
#
# Quaternions... inefficiently.
#
# Should probably use PDL and C... ?
#
# Stored as [c,x,y,z].
#
# XXX REMEMBER!!!! First component = cos(angle*2), *NOT* cos(angle)

package PDL::Graphics::TriD::Quaternion;

sub new {
	my($type,$c,$x,$y,$z) = @_;
	my $this;

   if(ref($type)){
	  $this = $type;
	}else{
	  $this = bless [$c,$x,$y,$z],$type;
	}
	return $this;
}

sub copy {
	return new PDL::Graphics::TriD::Quaternion(@{$_[0]});
}

sub new_vrmlrot {
	my($type,$x,$y,$z,$a) = @_;
	my $l = sqrt($x**2+$y**2+$z**2);
	my $this = bless [cos($a/2),map {sin($a/2)*$_/$l} $x,$y,$z],$type;
	$this->normalize_this();
	return $this;
}

sub to_vrmlrot {
	my($this) = @_;
	my $d = POSIX::acos($this->[0]);
	if(abs($d) < 0.0000001) {
		return [0,0,1,0];
	}
	return [(map {$_/sin($d)} @{$this}[1..3]),2*$d];
}

# Yuck
sub multiply {
	my($this,$with) = @_;
	return PDL::Graphics::TriD::Quaternion->new(
		$this->[0] * $with->[0] -
		$this->[1] * $with->[1] -
		$this->[2] * $with->[2] -
		$this->[3] * $with->[3],
			$this->[2] * $with->[3] -
			$this->[3] * $with->[2] +
			$this->[0] * $with->[1] +
			$this->[1] * $with->[0],
		$this->[3] * $with->[1] -
		$this->[1] * $with->[3] +
		$this->[0] * $with->[2] +
		$this->[2] * $with->[0],
			$this->[1] * $with->[2] -
			$this->[2] * $with->[1] +
			$this->[0] * $with->[3] +
			$this->[3] * $with->[0],
	);
}

sub multiply_scalar {
	my($this,$scalar) = @_;
	my $ang = POSIX::acos($this->[0]);
	my $d = sin($ang);
	if(abs($d) < 0.0000001) {
		return new PDL::Graphics::TriD::Quaternion(1,0,0,0);
	}
	$ang *= $scalar;
	my $d2 = sin($ang);
	return new PDL::Graphics::TriD::Quaternion(
		cos($ang), map {$_*$d2/$d} @{$this}[1..3]
	);
}

sub set {
	my($this,$new) = @_;
	@$this = @$new;
}

sub add {
	my($this,$with) = @_;
	return PDL::Graphics::TriD::Quaternion->new(
		$this->[0] * $with->[0],
		$this->[1] * $with->[1],
		$this->[2] * $with->[2],
		$this->[3] * $with->[3]);
}

sub abssq {
	my($this) = @_;
	return  $this->[0] ** 2 +
		$this->[1] ** 2 +
		$this->[2] ** 2 +
		$this->[3] ** 2 ;
}

sub invert {
	my($this) = @_;
	my $abssq = $this->abssq();
	return PDL::Graphics::TriD::Quaternion->new(
		 1/$abssq * $this->[0] ,
		-1/$abssq * $this->[1] ,
		-1/$abssq * $this->[2] ,
		-1/$abssq * $this->[3] );
}

sub invert_rotation_this {
	my($this) = @_;
	$this->[0] = - $this->[0];
}

sub normalize_this {
	my($this) = @_;
	my $abs = sqrt($this->abssq());
	@$this = map {$_/$abs} @$this;
}

sub rotate {
  my($this,$vec) = @_;
  my $q = (PDL::Graphics::TriD::Quaternion)->new(0,@$vec);
  my $m = $this->multiply($q->multiply($this->invert));
  return [@$m[1..3]];
}

sub rotate_foo {
  my ($this,$vec) = @_;
#  print "CP: ",(join ',',@$this)," and ",(join ',',@$vec),"\n";
  return $vec if $this->[0] == 1 or $this->[0] == -1;
# 1. cross product of my vector and rotated vector
# XXX I'm not sure of any signs!
  my @u = @$this[1..3];
  my @v = @$vec;
  my $tl = sqrt($u[0]**2 + $u[1]**2 + $u[2]**2);
  my $up = sqrt($v[0]**2 + $v[1]**2 + $v[2]**2);
  my @cp = (
  	$u[1] * $v[2] - $u[2] * $v[1],
  	$u[0] * $v[2] - $u[2] * $v[0],
  	$u[0] * $v[1] - $u[1] * $v[0],
  );
# Cross product of this and my vector
  my @cp2 = (
  	$u[1] * $cp[2] - $u[2] * $cp[1],
  	$u[0] * $cp[2] - $u[2] * $cp[0],
  	$u[0] * $cp[1] - $u[1] * $cp[0],
  );
  my $cpl = 0.00000001 + sqrt($cp[0]**2 + $cp[1]**2 + $cp[2]**2);
  my $cp2l = 0.0000001 + sqrt($cp2[0]**2 + $cp2[1]**2 + $cp2[2]**2);
  for(@cp) {$_ /= $cpl}
  for(@cp2) {$_ /= $cp2l}
  my $mult1 = $up * sqrt(1-$this->[0]**2);
  # my $mult1 = $up * sqrt(1-$this->[0]**2);
  my $mult2 = $up * $this->[0];
  print "ME: ",(join '    ',@u),"\n";
  print "VEC: ",(join '    ',@v),"\n";
  print "CP: ",(join '    ',@cp),"\n";
  print "CP2: ",(join '    ',@cp2),"\n";
  print "MULT1: $mult1, MULT2: $mult2\n";
  print "CPL: ",$cpl, " TL: $tl  CPLTL: ",$cpl/$tl,"\n";
  my $res = [map {
  	$v[$_] + $mult1 * $cp[$_] + ($mult2 - $cpl/$tl)* $cp2[$_]
  } 0..2];
#  print "RES: ",(join ',',@$res),"\n";
  return $res;
}

1;
