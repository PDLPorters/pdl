package XS;

sub mkproto {
	my($name,$pars) = @_;
	my $shortpars = join ',',map {$_->[0]} @$pars;
	my $longpars = join "\n",map {"\t".$_->[1]->get_decl($_->[0])} @$pars;
	return<<END;

void
$name($shortpars)
$longpars
END


}

1;
