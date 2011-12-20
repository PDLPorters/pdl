# DO NOT MODIFY - IT IS VERY FINICKY; see notes below.

$^W = 0;

# Five tests for each of seven types:
use Test::More tests => 35;
use PDL::PP qw(foo::bar foo::bar foobar);

# Add some tests for pp_line_numbers:
pp_def(test1 =>
  Pars => 'a(n)',
  Code => pp_line_numbers (__LINE__, q{
    /* line 13, First line */
    threadloop %{
      /* line 15, Line after threadloop */
      loop (n) %{
        /* line 17, Line after loop */
      %}
      /* line 19, Line after close of loop */
    %}
    /* line 21, Line after close of threadloop */
  })
);

pp_done;

unlink 'foobar.pm';


# Analyze the output of pp_line_numbers by checking the line numbering in
# foobar.xs. Note that the line *after* the #line directive is assigned the
# number of the #line directive. See http://gcc.gnu.org/onlinedocs/cpp/Line-Control.html
my ($line, $file) = (1, 'foobar.xs');
open my $fh, '<', 'foobar.xs';
LINE: while(<$fh>) {
  # Take note of explicit line directives
  if (/#line (\d+) ".*"/) {
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

unlink 'foobar.xs';

__END__

This test is very finicky because it uses __LINE__, but it also explicitly
indicates the line numbers in the /* comments */. As such, if you add a line
of text (comment or code) before or within the pp_def, all of the line
numbers in the /* comments */ will be off. It's a minor headache to adjust
them, so please just don't mess with this test, unless of course you wish to
fix it. :-)

--DCM, December 13, 2011
