# These objects contain textual descriptions of the graph.
# Placed suitably in relation to origin to be used with a graph.

package PDL::Graphics::TriD::Description;

@ISA=qw/PDL::Graphics::TriD::Object/;

sub new {
	my($type,$text) = @_;
	local $_ = $text;
	s/\\/\\\\/g;
	s/"/\\"/g;
	my $this = bless {
		TText => "[".(join ',',map {"\"$_\""} split "\n",$_)."]"
	},$type;
	return $this;
}

1;
