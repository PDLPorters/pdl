
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

sub has_pod  # does file contain HTML-able pod?
{
   my $line; #mustn't clobber $_ during find
   open(POD,shift) || return 0;
   while ($line=<POD>) {return 1 if $line =~ /^=head/} # only a guess, avoids "=for nobody", etc.
   return 0;
}

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
   use Pod::Html;
   use Cwd;

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
                       if (!&has_pod($File::Find::name)) {
                         printf STDERR "%-30s\n", $_ ."... skipped (no pod)";
                         return;
                       }
		       my $outdir = $File::Find::dir;
		       my $re = "\Q$startdir\E";  # ach: '+' in $outdir here!
                      # $outdir =~ s/$re/$htmldir/;
                      $outdir =~ s/$re//;
                      $outdir =~ /(^\/)?(.*)$/;
		      my $basename = basename($File::Find::name);
		      my $outfi;
		      if( $basename eq 'PDL.pm'){ # Special case for needed for PDL.pm file
			$outfi = $basename;       # since it is in a different location than the other
		      }				  # .pm and pod files.
                      else{
		        $outfi = $2.($2 =~ /^\s*$/ ? '' : '/').$basename;
                        $outfi =~ s|/|_|g;
		      }
                      # print "outdir = $outdir, making $outfi\n"; return;
                      # mkdir_p $outdir, 0777, $outdir;
		       my $file = $File::Find::name;
                      my $outfile = "$htmldir/$outfi";
		       $outfile =~ s/[.](pm|pod)$//;
		       $outfile .= ".html";
		       printf STDERR "%-30s\n", $_ ."... > $outfile";
		       chdir $htmldir; # reuse our pod caches
                      my $topPerlDir = $startdir;
                        # get Directory just above PDL for podroot arg
                      $topPerlDir = $1 if ($startdir =~ /^(.+?)\/PDL$/);
                      print "startdir: $startdir, podroot: $topPerlDir\n";
		       pod2html(
			  "--podpath=PDL:.",
			  "--podroot=$topPerlDir",
                         "--htmlroot=../../..",
			  "--libpods=perlfaq",
			  "--recurse",
			  "--infile=$file",
			  "--outfile=$outfile",
			  "--verbose");
                       chdir $File::Find::dir; # don't confuse File::Find
		   }
		 };
   File::Find::find($sub,$startdir, "$startdir/../PDL.pm");
