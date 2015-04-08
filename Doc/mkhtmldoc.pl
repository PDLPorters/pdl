
#
# Should be called like
#
#    perl mkhtmldoc.pl [FULLPATH_TO_SOURCE] [FULLPATH_TO_HTMLDIR]
#
# for example
#
#    perl mkhtmldoc.pl `pwd`/blib/lib `pwd`/html
#
#  reverted to use Pod::Html from normal perl distrib
#   Christian
#
# (mod. by Tjl)

use File::Find;
use File::Basename;
use File::Basename;
use Getopt::Std;
use Pod::Html;
use Cwd;

use IO::File; # for hack_links()

$opt_v = 0;
$opt_s = '';

getopts('vs:');
my $verbose = $opt_v;
my ($strip_path,$add_path) = split(/,/,$opt_s);

##############################################################
## Subroutines

sub has_pod  # does file contain HTML-able pod?
{
    my $line; #mustn't clobber $_ during find
    open(POD,shift) || return 0;
    while ($line=<POD>) {return 1 if $line =~ /^=head/} # only a guess, avoids "=for nobody", etc.
    return 0;
}

sub mkdir_p ($$$) {
    return if -d $_[0];
#    my @dirs = File::Spec->splitdir($_[0]);
    my @dirs = split '/', $_[0];
    pop @dirs;
    if(!@dirs) {die "Couldn't create directory $_[2]"}
#    my $dir = File::Spec->catdir( @dirs );
    my $dir = join '/', @dirs;
    mkdir_p ($dir, $_[1], $_[2]);
    print "Creating directory $_[0]\n" if $verbose;
    mkdir $_[0], $_[1] or die "Couldn't create directory $_[0]";
}

sub fix_pdl_dot_html ($) {
##Links to PDL.html sensibly try to go up one too many directories
##(e.g., to "../PDL.html" instead of "PDL.html").  This hopefully
##fixes that. Shamelessly ripped off hack_html().
    my $infile = shift;
    my $outfile = "${infile}.n";

    my $ifh = new IO::File "<$infile"
	or die "ERROR: Unable to read from <$infile>\n";
    my $ofh = new IO::File ">$outfile"
	or die "ERROR: Unable to write to <$outfile>\n";

    # assume that links do not break across a line
    while ( <$ifh> ) {
	# fix the links
	s{\.\.\/PDL\.html}{PDL.html}g;
	print $ofh $_;
    }
    $ifh->close;
    $ofh->close;

    rename $outfile, $infile
	or die "ERROR: Unable to rename $outfile\n";
}

sub fix_html_path ($) {
    my $infile = shift;
    my $outfile = "${infile}.n";

    my $ifh = new IO::File "<$infile"
	or die "ERROR: Unable to read from <$infile>\n";
    my $ofh = new IO::File ">$outfile"
	or die "ERROR: Unable to write to <$outfile>\n";

    # assume that links do not break across a line
    while ( <$ifh> ) {
	# fix the links
	s{a href="$strip_path}{a href="$add_path}g;
	print $ofh $_;
    }
    $ifh->close;
    $ofh->close;

    rename $outfile, $infile
	or die "ERROR: Unable to rename $outfile\n";
}

sub fix_pp_inline ($) {
    my $infile = shift;
    my $outfile = "${infile}.n";
    
    my $ifh = new IO::File "<$infile"
	or die "ERROR Unable to read from <$infile>\n";
    my $ofh = new IO::File ">$outfile"
	or die "ERROR: Unable to write to <$outfile>\n";
    
    # assume that links do not break across a line
    while ( <$ifh> ) {
	#fix the links
	s|a href="../Inline/Pdlpp.html"|a href="./PP-Inline.html"|g;
	print $ofh $_;
    }
    $ifh->close;
    $ofh->close;
    rename $outfile, $infile
	or die "ERROR: Unable to rename $outfile\n";
}

##############################################################
## Code

$SIG{__DIE__} = sub {print Carp::longmess(@_); die;};

$back = getcwd;
$startdir = shift @ARGV; #$ARGV[0];

unless (defined $startdir) {
    require PDL;
    ($startdir = $INC{'PDL.pm'}) =~ s/\.pm$//i;
    umask 0022;
}
die "couldn't find directory '$startdir'" unless -d $startdir;
chdir $startdir or die "can't change to $startdir";
$startdir = getcwd; # Hack to get absolute pathname
chdir $back;

$htmldir = shift @ARGV; #$ARGV[1];
#$htmldir = File::Spec->catdir( $startdir, "HtmlDocs", "PDL" )
$htmldir = "${startdir}/HtmlDocs/PDL"
    unless defined $htmldir;

mkdir_p $htmldir, 0777, $htmldir;
chdir $htmldir or die "can't change to $htmldir";
$htmldir = getcwd; # Hack to get absolute pathname
chdir $back;

#my $updir = File::Spec->updir;

print "Making HTML docs...\n\n";

print "Put HTML $htmldir\n" if $verbose;
print "Scanning $startdir ... \n\n" if $verbose;
$sub = sub {
    return unless $File::Find::name =~ /[.]pod$/ or
	($File::Find::name =~ /[.]pm$/ and
	 $File::Find::name !~ /PP.pm/  and
	 $File::Find::dir !~ m{/PP|/Gen});

#    if (($File::Find::name =~ /[.]pm$/ and
#	 $File::Find::name !~ /PP.pm/ and
#	 $File::Find::dir !~ m#/PP|/Gen#) or
#	 $File::Find::name =~ /[.]pod$/) {

    if (!&has_pod($File::Find::name)) {
	printf STDERR "%-30s\n", $_ ."... skipped (no pod)"
	  if $verbose;
	return;
    }

    my $re = "\Q$startdir\E";  # ach: '+' in $outdir here!

    my $outdir = $File::Find::dir;
    # $outdir =~ s/$re/$htmldir/;
    $outdir =~ s/$re//;
    $outdir =~ /(^\/)?(.*)$/;

    my $basename = basename($File::Find::name);
    my $outfi;

    # Special case for needed for PDL.pm file since it is in a
    # different location than the other .pm and pod files.
    if( $basename eq 'PDL.pm'){
	$outfi = $basename;
    } else {
	$outfi = $2.($2 =~ /^\s*$/ ? '' : '/').$basename;
	#
	# with the following substitution, everything gets stored in the same directory -
	# so PDL/Graphics/LUT -> PDL_Graphics_LUT for example
	#
	#$outfi =~ s|/|_|g;
    }

    # create the output directory, if required
    if ( $outdir ne "" ) {
#	    $outdir = File::Spec->catdir( $htmldir, $outdir );
	$outdir = "${htmldir}/${outdir}";
	mkdir_p $outdir, 0777, $outdir;
    }

    # print "outdir = $outdir, making $outfi\n"; return;
    # mkdir_p $outdir, 0777, $outdir;

    my $file = $File::Find::name;
#	my $outfile = File::Spec->catfile( $htmldir, $outfi );
    my $outfile = "${htmldir}/${outfi}";
    $outfile =~ s/[.](pm|pod)$//;
    $outfile .= ".html";
    printf STDERR "%-30s\n", $_ ."..."; #  > $outfile";

    chdir $htmldir; # reuse our pod caches
    my $topPerlDir = $startdir;

    # get Directory just above PDL for podroot arg
    $topPerlDir = $1 if ($startdir =~ /^(.+?)\/PDL$/);
    print "startdir: $startdir, podroot: $topPerlDir\n" if $verbose;

    # instead of having htmlroot="../../.."
    # (or even File::Spec->catdir( $updir, $updir, $updir ) )
    # calculate it from the known location of the
    # file we're creating
    my $htmlrootdir = $htmldir;
    $htmlrootdir =~ s|PDL$||;

    my $verbopts = $verbose ? "--verbose" : "--quiet";

    my @pod2html_args = (
      "--podpath=.",
      "--podroot=$topPerlDir",
      "--htmldir=$htmlrootdir",
      "--recurse",
      "--infile=$file",
      "--outfile=$outfile",
      $verbopts,
    );
    if($] <= 5.015) {
      # With perl 5.15.x (for some value of x) and later, '--libpods' is invalid
      # and hence needs to be removed.
      # Beginning with 5.15.x, the generated PDL html docs are a little different
      # (missing some underlining of headings and some <b></b> tagging), though
      # this appears to have nothing to do with the removal of --libpods. Rather,
      # it seems to be the result of some other change to pod2html. Perhaps this
      # can be addressed over time. SIS 23-Feb-2012
      # Cut out "PDL" from the podpath as it crashes the podscan(!) - It doesn't
      # seem to help either -- it looks for cached docs in .../HtmlDocs/pdl/PDL,
      # which is silly.  I left this note because pod paths are pretty arcane to
      # me.  CED 11-Mar-2009
      #    pod2html("--podpath=PDL:.",
      unshift @pod2html_args, "--libpods=perlfaq";
    }
    pod2html(@pod2html_args);
    fix_pdl_dot_html( $outfile);
    fix_html_path( $outfile);
    fix_pp_inline( $outfile);

    chdir $File::Find::dir; # don't confuse File::Find
};

#File::Find::find($sub,$startdir,File::Spec->catdir( $startdir, $updir, "PDL.pm"));
File::Find::find( $sub, $startdir, "${startdir}/../PDL.pm" );

## End
