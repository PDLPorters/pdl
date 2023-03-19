package PDL::PP::Signature;

use strict; use warnings;
use PDL::PP::PdlParObj;
use PDL::PP::Dims;
use Carp;

=head1 NAME

PDL::PP::Signature - Internal module to handle signatures

=head1 DESCRIPTION

Internal module to handle signatures

=head1 SYNOPSIS

 use PDL::PP::Signature;

=cut

# Eliminate whitespace entries
sub nospacesplit {grep /\S/, split $_[0],$_[1]}

sub new {
  my ($type,$str,$bvalflag,$otherpars) = @_;
  $bvalflag ||= 0;
  my $this = bless {}, $type;
  my @objects = map PDL::PP::PdlParObj->new($_,$bvalflag, $this), nospacesplit ';',$str;
  $this->{Names} = [ map $_->name, @objects ];
  $this->{Objects} = { map +($_->name => $_), @objects };
  my @objects_sorted = ((grep !$_->{FlagW}, @objects), (grep $_->{FlagW}, @objects));
  $objects_sorted[$_]{Number} = $_ for 0..$#objects_sorted;
  $this->{NamesSorted} = [ map $_->name, @objects_sorted ];
  $this->{DimsObj} = my $dimsobj = PDL::PP::PdlDimsObj->new;
  $_->add_inds($dimsobj) for @objects;
  my (%ind2use, %ind2obj);
  for my $o (@objects) {
    for my $io (@{$o->{IndObjs}}) {
      push @{$ind2use{$io->name}}, $o;
      $ind2obj{$io->name} = $io;
    }
  }
  $this->{Ind2Use} = \%ind2use;
  $this->{Ind2Obj} = \%ind2obj;
  $this->{IndNamesSorted} = [ sort keys %ind2use ];
  my $i=0; my %ind2index = map +($_=>$i++), @{$this->{IndNamesSorted}};
  $this->{Ind2Index} = \%ind2index;
  $ind2obj{$_}->set_index($ind2index{$_}) for sort keys %ind2index;
  @$this{qw(OtherNames OtherObjs OtherAnyOut OtherFlags)} = $this->_otherPars_nft($otherpars||'');
  $this;
}

sub _otherPars_nft {
    my ($sig,$otherpars) = @_;
    my $dimobjs = $sig && $sig->dims_obj;
    my (@names,%types,$type,$any_out,%allflags);
    for (nospacesplit(';',$otherpars)) {
	my (%flags);
	if (s/^\s*$PDL::PP::PdlParObj::sqbr_re\s*//) {
	  %flags = my %lflags = map +($_=>1), split /\s*,\s*/, my $opts = $1;
	  my $this_out = delete $lflags{o};
	  die "Invalid options '$opts' in '$_'" if keys %lflags;
	  $any_out ||= $this_out;
	}
	if (/^\s*([^=]+?)\s*=>\s*(\S+)\s*$/) {
	    # support 'int ndim => n;' syntax
	    my ($ctype,$dim) = ($1,$2);
	    print "OtherPars: setting dim '$dim' from '$ctype'\n" if $::PP_VERBOSE;
	    $type = PDL::PP::CType->new($ctype);
	    ($sig->{Ind2Obj}{$dim} ||= $dimobjs->get_indobj_make($dim))->set_from($type);
	} else {
	    $type = PDL::PP::CType->new($_);
	}
	my $name = $type->protoname;
	croak "Invalid OtherPars name: $name"
	  if $PDL::PP::PdlParObj::INVALID_PAR{$name};
	push @names,$name;
	$types{$name} = $type;
	$types{"${name}_count"} = PDL::PP::CType->new("PDL_Indx ${name}_count") if $type->is_array;
	$allflags{$name} = \%flags;
    }
    (\@names,\%types,$any_out,\%allflags);
}

=head1 AUTHOR

Copyright (C) Tuomas J. Lukka 1997 (lukka@husc.harvard.edu) and by Christian
Soeller (c.soeller@auckland.ac.nz).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

sub names { $_[0]{Names} }
sub names_sorted { $_[0]{NamesSorted} }
sub names_sorted_tuples {
  my ($count, @names) = (0, @{$_[0]{NamesSorted}});
  map [$count++, $_[0]{Objects}{$_}{FlagTemp}, $_], @names;
}

sub objs { $_[0]{Objects} }
sub names_in { my $o=$_[0]->objs; grep !$o->{$_}{FlagOut} && !$o->{$_}{FlagTemp}, @{$_[0]{Names}} }
sub names_out { my $o=$_[0]->objs; grep $o->{$_}{FlagOut}, @{$_[0]{Names}} }
sub names_oca { my $o=$_[0]->objs; grep $o->{$_}{FlagCreateAlways}, @{$_[0]{Names}} }
sub names_out_nca { my $o=$_[0]->objs; grep $o->{$_}{FlagOut} && !$o->{$_}{FlagCreateAlways}, @{$_[0]{Names}} }
sub names_tmp { my $o=$_[0]->objs; grep $o->{$_}{FlagTemp}, @{$_[0]{Names}} }

sub dims_obj { $_[0]->{DimsObj} }
sub dims_count { scalar keys %{$_[0]{DimsObj}} }
sub dims_values { values %{$_[0]{DimsObj}} }

sub ind_used { $_[0]{Ind2Use}{$_[1]} }
sub ind_obj { $_[0]{Ind2Obj}{$_[1]} }
sub ind_names_sorted { @{$_[0]{IndNamesSorted}} }
sub ind_index { $_[0]{Ind2Index}{$_[1]} }

sub othernames {
  my ($self, $omit_count, $except) = @_;
  $except ||= {};
  return $self->{OtherNames} if $omit_count && !keys %$except;
  my $objs = $self->otherobjs;
  my @raw_names = grep !$except->{$_}, @{$self->{OtherNames}};
  @raw_names = map $objs->{$_}->is_array ? ($_, "${_}_count") : $_, @raw_names if !$omit_count;
  \@raw_names;
}
sub otherobjs { $_[0]{OtherObjs} }
sub other_any_out { $_[0]{OtherAnyOut} }
sub other_is_out { $_[0]{OtherFlags}{$_[1]} && $_[0]{OtherFlags}{$_[1]}{o} }
sub other_out { grep $_[0]->other_is_out($_), @{$_[0]{OtherNames}} }

sub allnames { [
  (grep +(!$_[2] || !$_[2]{$_}) && !$_[0]{Objects}{$_}{FlagTemp}, @{$_[0]{Names}}),
  @{$_[0]->othernames(@_[1,2])},
] }
sub allobjs {
  my $pdltype = PDL::PP::CType->new("pdl *__foo__");
  +{ ( map +($_,$pdltype), @{$_[0]{Names}} ), %{$_[0]->otherobjs} };
}
sub alldecls {
  my ($self, $omit_count, $indirect, $except) = @_;
  my $objs = $self->allobjs;
  my @names = @{$self->allnames($omit_count, $except)};
  $indirect = $indirect ? { map +($_=>$self->other_is_out($_)), @names } : {};
  map $objs->{$_}->get_decl($_, {VarArrays2Ptrs=>1,AddIndirect=>$indirect->{$_}}), @names;
}
sub getcomp {
  my ($self) = @_;
  my $objs = $self->otherobjs;
  my @names = @{$self->othernames(0)};
  my $indirect = { map +($_=>$self->other_is_out($_)), @names };
  join '', map "$_;", grep $_, map $objs->{$_}->get_decl($_, {VarArrays2Ptrs=>1,AddIndirect=>$indirect->{$_}}), @names;
}
sub getfree {
  my ($self,$symbol) = @_;
  my $objs = $self->otherobjs;
  join '', map $objs->{$_}->get_free("\$$symbol($_)",
    { VarArrays2Ptrs => 1 }), @{$self->othernames(0)};
}
sub getcopy {
  my ($self, $to_pat) = @_;
  my $objs = $self->otherobjs;
  PDL::PP::pp_line_numbers(__LINE__,
    join '', map $objs->{$_}->get_copy($_,sprintf $to_pat,$_), @{$self->othernames(0)}
  );
}

sub realdims {
  my $this = shift;
  [ map scalar @{$this->{Objects}{$_}{RawInds}}, @{$this->{Names}} ];
}

sub creating {
  my $this = shift;
  croak "you must perform a checkdims before calling creating"
    unless defined $this->{Create};
  return $this->{Create};
}

sub checkdims {
  my $this = shift;
  # we have to recreate to keep defaults currently
  $this->{Dims} = PDL::PP::PdlDimsObj->new;
  $this->{Objects}{$_}->add_inds($this->{Dims}) for @{$this->{Names}};
  my $n = @{$this->{Names}};
  croak "not enough pdls to match signature" unless $#_ >= $n-1;
  my @pdls = @_[0..$n-1];
  if ($PDL::debug) { print "args: ".
		     join(' ,',map { "[".join(',',$_->dims)."]," } @pdls)
		       . "\n"}
  my $i = 0;
  my @creating = map $this->{Objects}{$_}->perldimcheck($pdls[$i++]),
         @{$this->{Names}};
  $i = 0;
  for (@{$this->{Names}}) {
    push @creating, $this->{Objects}{$_}->getcreatedims
      if $creating[$i++];
  }
  $this->{Create} = \@creating;
  $i = 0;
  my $corr = 0;
  for (@{$this->{Names}}) {
    $corr = $this->{Objects}{$_}->finalcheck($pdls[$i++]);
    next unless $#$corr>-1;
    my ($j,$str) = (0,"");
    for (@$corr) {$str.= ":,"x($_->[0]-$j)."(0),*$_->[1],";
			$j=$_->[0]+1 }
    chop $str;
    $_[$i-1] = $pdls[$i-1]->slice($str);
  }
}

1;
