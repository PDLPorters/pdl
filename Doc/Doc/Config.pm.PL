#!/usr/local/bin/perl

use Config;
use File::Basename qw(&basename &dirname);

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
	if ($Config{'osname'} eq 'VMS' or
	    $Config{'osname'} eq 'OS2');  # "case-forgiving"
open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";
chmod 0775, $file;

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print OUT "# automatically built from ".basename($0)."\n";
print OUT "# don't modify, all changes will be lost !!!!\n";
print OUT <<'!NO!SUBS!';
package PDL::Doc::Config;

!NO!SUBS!

print OUT <<"EOC";
\$PDL::Doc::pager = \'$Config{'pager'}\';
\$PDL::Doc::pager = \$ENV{PAGER} if defined \$ENV{PAGER};
\$PDL::Doc::DefaultFile = \'$Config{'man1direxp'}\';

1;

EOC
