
#
# Should be called like
#
#    perl mkhtmldoc.pl [FULLPATH_TO_SOURCE] [FULLPATH_TO_HTMLDIR]
#
# for example
#
#    perl mkhtmldoc.pl `pwd`/blib/lib `pwd`/html
#
# To get correct interlinking of pages (L<> directives) I had
# to patch Pod/Html.pm from the perl 5.004_4 distrib. This patch
# is included. There are now only a few duff links left.
#
#   Christian
#
# (mod. by Tjl)

sub mkdir_p ($$$) {
	return if -d $_[0];
	my @a = split '/',$_[0];
	pop @a;
	if(!@a) {die "Couldn't create directory $_[2]"}
	my $d = join '/',@a;
	mkdir_p ($d, $_[1], $_[2]);
	print "Creating directory $_[0]\n";
	mkdir $_[0], $_[1] or die "Couldn't create directory $_[0]";
}

   # start mkhtmldoc.pl
   use File::Find;
   use File::Basename;
   use PDL::Pod::Html;
   use Cwd;

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
   unless (defined $htmldir) {
	$htmldir = "$startdir/HtmlDocs/PDL";
   }

   mkdir_p $htmldir, 0777, $htmldir;
   chdir $htmldir or die "can't change to $htmldir";
   $htmldir = getcwd; # Hack to get absolute pathname
   chdir $back;

   print "Put HTML $htmldir\n";
   print "Scanning $startdir ... \n\n";
   $sub = sub { if (($File::Find::name =~ /[.]pm$/ &&
			$File::Find::name !~ /PP.pm/ &&
			$File::Find::dir !~ m#/PP|/Gen#) or
			  $File::Find::name =~ /[.]pod$/)  {
		       my $outdir = $File::Find::dir;
		       my $re = "\Q$startdir\E";  # ach: '+' in $outdir here!
		       $outdir =~ s/$re/$htmldir/;
		       mkdir_p $outdir, 0777, $outdir;
		       my $file = $File::Find::name;
		       my $outfile = "$outdir/".basename($file);
		       $outfile =~ s/[.](pm|pod)$//;
		       $outfile .= ".html";
		       printf STDERR "%-30s\n", $_ ."... > $outfile";
		       chdir $htmldir; # reuse our pod caches
		       $startdir =~ /^(.+?)\/PDL$/;  # get Directory just above PDL for podroot arg
		       my $topPerlDir = $1;
		       pod2html(
			  "--podpath=PDL",
			  "--podroot=$topPerlDir",
			  "--htmlroot=../",
			  "--libpods=perlfaq",
			  "--recurse",
			  "--infile=$file",
			  "--outfile=$outfile",
			  "--verbose");
		   }
		 };
   File::Find::find($sub,$startdir);
