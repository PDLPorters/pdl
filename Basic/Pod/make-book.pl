#!/usr/bin/perl -w

use strict;
use Pod::Xhtml;
use File::Basename;

####################
#
# START CONFIGURATION
#

# Book CSS.
my $CssFile = "pdl-book.css";

# Output directory.
my $OutDir = "html";  

# Table of contents.
my $TocFile = 'index.html';
my @TOC = (
	{
		title => 'Introduction',
		chapters => [
			{ title => 'Why PDL?',			file => ''},
			{ title => 'Quick start guide',	file => ''},
		]
	},
	{
		title => 'Installation',
		chapters => [
			{ title => 'Standard installation',	file => ''},
			{ title => 'Manual installation',	file => ''},
		]
	},
	{
		title => 'Migration',
		chapters => [
			{ title => 'Migration for MATLAB users',	file => 'MATLAB.pod'},
			{ title => 'Migration for Scilab users',	file => 'Scilab.pod'},
			{ title => 'Migration for IDL users',		file => ''},
		]
	},
	{
		title => 'Basic features',
		chapters => [
			{ title => 'Slicinig and Indexing',	file => ''},
			{ title => 'Threading',				file => 'Threading.pod'},
		]
	},
	{
		title => 'Plotting',
		chapters => [
			{ title => 'Overview',		file => ''},
			{ title => 'PGPLOT',		file => ''},
			{ title => 'PLplot',		file => ''},
			{ title => '3D Plots',		file => ''},
		]
	},
	{
		title => 'Advanced features',
		chapters => [
			{ title => 'PDL Pre-Processor',	file => ''},
		]
	},
	{
		title => 'Appendices',
		chapters => [
			{ title => 'Frequently Asked Questions',	file => ''},
			{ title => 'Tips and HOWTOs',				file => ''},
		]
	},
);

#
# END CONFIGURATION
#
####################

#
# Functions
#
sub header {
	my $CssTag = "<link rel='stylesheet' type='text/css' href='$CssFile' />";
	my $title = shift;
	return <<EOH
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
	<title>$title</title>
    $CssTag
</head>
<body>

  <h1>$title</h1>

EOH
;
}
sub footer {
	return <<EOH

</body>
</html>

EOH
;
}
sub mkChapter {
	my ($count, $chapter) = @_;
	my $CssTag = "<link rel='stylesheet' type='text/css' href='$CssFile' />";
	my ($parser, $xhtml);
	
	my $title = "Chapter $count. ".$chapter->{title};
	my $in_file = $chapter->{file};
	my $out_file= fileparse($in_file, qr/\.[^.]*/) . ".html";
	
	$parser = new Pod::Xhtml( StringMode => 1 );
	$parser->parse_from_file( $chapter->{file}, "$OutDir/$out_file" );
	
	$xhtml = $parser->asString;
	$xhtml =~ s|</head>|    $CssTag\n</head>|;
	$xhtml =~ s|<title></title>|<title>$title</title>|;
	$xhtml =~ s|<h3 id="TOP">Index</h3>|<h1 class="title" id="TOP">$title</h1>|;
	
	open CHAPTER, ">$OutDir/$out_file" or die "Could not open chapter file: $!\n";
	print CHAPTER $xhtml;
	close CHAPTER;
	
	return $out_file;
}

#
#  Main program.
#


if (! -e $OutDir ) {
	mkdir $OutDir or die "Could not make output directory $OutDir.\n";
}

system("cp $CssFile $OutDir");


# Convert POD to HTML.
open TOC, ">$OutDir/$TocFile" or die "Could not open ToC file.\n";
print TOC header("PDL Book - Table of Contentsx");

my $count = 1;
for my $section (@TOC) {
	print TOC "<h3>".$section->{title}."</h3>\n";
	print TOC "  <ul>\n";
	for my $chapter (@{$section->{chapters}}) {
		my $title = "Chapter $count. ".$chapter->{title};
		if ($chapter->{file}) {
			my $out_file = mkChapter($count, $chapter);
			$title = "<a href='$out_file'>$title</a>";
		}
		
		# Done.
		print TOC "    <li>$title</li>\n";
		$count++;
	}
	print TOC "  </ul>";
}
print TOC footer();

close TOC;

=pod



print $xhtml,"\n";

