# Four tests for each of nine types:
use Test::More tests => 35;

#$::PP_VERBOSE = 1;
use PDL::PP qw(foo::bar foo::bar foobar);

# Add some tests for pp_line_numbers:
TODO: {
  
  local $TODO = 'Just starting work on this, so most of it will not work';
#$DB::single = 1;
pp_def(test1 =>
  Pars => 'a(n)',
  Code => pp_line_numbers (__LINE__, q{
    /* line 15, First line */
    threadloop %{
      /* line 17, Line after threadloop */
      loop (n) %{
        /* line 19, Line after loop */
      %}
      /* line 21, Line after close of loop */
    %}
    /* line 23, Line after clos of threadloop */
  })
);

pp_done;

unlink 'foobar.pm';

# Analyze the output of pp_line_numbers by checking the line numbering in
# foobar.xs:
my ($line, $file) = (1, 'foobar.xs');
open my $fh, '<', 'foobar.xs';
LINE: while(<$fh>) {
  # Take note of explicit line directives
  if (/# line (\d+) ".*"/) {
    ($line, $file) = ($1, $2);
    next LINE;
  }
  
  # look for items to check:
  if (m|/\* line (\d+), (.*?) \*/|) {
    my ($actual_line, $description) = ($1, $2);
    is($line, $actual_line, $description);
  }
  $line++;
}

#unlink 'foobar.xs';

};
