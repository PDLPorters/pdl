=pod

=head1 NAME

Dicom.pm - a module for reading DICOM images.

=head1 DESCRIPTION

The PDL::IO::Dicom module enables reading 16-bit gray level Dicom
images into PDL. As Dicom is an extremely complex format, this module
can unfortunately not handle all different image types included in the
DICOM standard. One common format that is currently not supported is
the Papyrus format.

=head1 USAGE

    use PDL;
    use PDL::IO::Dicom;

    $img = rdcm("image.dcm");

=head1 AUTHOR

Copyright (C) Dov Grobgeld <dov@imagic.weizmann.ac.il> 2002.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut
package PDL::IO::Dicom;

use PDL;
use PDL::Core;
use PDL::IO::Misc;

use Exporter;
@ISA = qw( Exporter ); 
@EXPORT = qw( rdcm );

use strict;

my $debug = 0;

my %element_table =
    (
     "0002,0000" => ["Group length","UL"],
     "0002,0001" => ["File Meta Information Version","OB"],
     "0002,0002" => ["Media Storage SOP Class UID","UI"],
     "0002,0003" => ["Media Storage SOP Instance UID","UI"],
     "0002,0010" => ["Transfer Syntax UID","UI"],
     "0002,0012" => ["Implementation Class UID","UI"],
     "0002,0013" => ["Implementation Version Name","SH"],
     "0002,0016" => ["Source Application Entity Title","AE"],
     "0008,0000" => ["Identifying Group Length", "UL"],
     "0008,0001" => ["Length to End"],
     "0008,0005" => ["Specific Character Set","CS"],
     "0008,0008" => ["Image Type","CS"],
     "0008,0010" => ["Recognition Code"],
     "0008,0012" => ["Instance Creation Date"],
     "0008,0013" => ["Instance Creation Time"],
     "0008,0014" => ["Instance Creator UID"],
     "0008,0016" => ["SOP Class UID","UI"],
     "0008,0018" => ["SOP Instance UID","UI"],
     "0008,0020" => ["Study Date","DA"],
     "0008,0021" => ["Series date","DA"],
     "0008,0022" => ["Acquisition Date","DA"],
     "0008,0023" => ["Image Date","DA"],
     "0008,0030" => ["Study Time","TM"],
     "0008,0031" => ["Series Time","TM"],
     "0008,0032" => ["Acquisition Time","TM"],
     "0008,0033" => ["Image Time","TM"],
     "0008,103e" => ["Series Description"],
     "0008,1030" => ["Study Description"],
     "0008,0050" => ["Accession Number","SH"],
     "0008,0060" => ["Modality","CS"],
     "0008,0070" => ["Manufacturer","LO"],
     "0008,0080" => ["Institution Name","LO"],
     "0008,0090" => ["Referring Physician's Name","PN"],
     "0008,1010" => ["Station Name","SH"],
     "0008,1030" => ["Study Description","LO"],
     "0008,103e" => ["Series Description","LO"],
     "0008,1060" => ["Name of Physician(s)","PN"],
     "0008,1090" => ["Manufacturers Model Name",],
     "0010,0000" => ["Patient Information Group"],
     "0010,0010" => ["Patient Name","PN"],
     "0010,0020" => ["Patient ID"],
     "0010,0030" => ["Patient Birth Date"],
     "0010,0040" => ["Patient's Sex"],
     "0010,1010" => ["Patient Age"],
     "0010,1030" => ["Patient Weight"],
     "0018,0000" => ["Acquisition Gnformation Group"],
     "0018,0010" => ["Contrast/Bolus Agent"],
     "0018,0020" => ["Scanning Sequence"],
     "0018,0022" => ["Scan Options"],
     "0018,0050" => ["Slice thickness"],
     "0018,1050" => ["Spatial Resolution", "lO"],
     "0008,1060" => ["Name of Physician(s) Reading Study"],
     "0008,1070" => ["Operator's Name"],
     "0018,0080" => ["Repetition Time"],
     "0018,0081" => ["Echo Time"],
     "0018,0082" => ["Inversion Time"],
     "0018,0083" => ["Number of Averages"],
     "0018,0084" => ["Imaging Frequency"],
     "0018,0085" => ["Imaged Nucleus"],
     "0018,0086" => ["Echo Number(s)"],
     "0018,0087" => ["Magnetic Field Strength"],
     "0018,0088" => ["Spacing Between Slices"],
     "0018,0089" => ["Number of Phase Encoding Steps"],
     "0018,0090" => ["Data Collection Diameter"],
     "0018,0091" => ["Echo Train Length"],
     "0018,0093" => ["Percent Sampling"],
     "0018,0094" => ["Percent Phase Field of View"],
     "0018,0095" => ["Pixel Bandwidth"],
     "0018,1088" => ["Heart Rate","US"],
     "0018,1090" => ["Cardiac Number of Images","US"],
     "0018,1094" => ["Trigger Window","US"],
     "0018,1100" => ["Reconstruction Diameter"],
     "0018,1314" => ["Flip Angle"],
     "0018,1315" => ["Variable Flip Angle Flag"],
     "0018,1316" => ["SAR"],
     "0020,0000" => ["Relationship information group"],
     "0020,000d" => ["Study Instance UID"],
     "0020,000e" => ["Series Instance UID"],
     "0020,0010" => ["Study ID"],
     "0020,0011" => ["Series Number"],
     "0020,0012" => ["Acquisition Number"],
     "0020,0013" => ["Image Number"],
     "0020,0014" => ["Isotope Number"],
     "0020,0015" => ["Phase Number"],
     "0020,0016" => ["Interval Number"],
     "0020,0017" => ["Time Slot Number"],
     "0020,0018" => ["Angle Number"],
     "0020,0020" => ["Patient Orientation"],
     "0020,0022" => ["Overlay Number"],
     "0020,0024" => ["Curve Number"],
     "0020,0026" => ["LUT Number"],
     "0020,0030" => ["Image Position"],
     "0020,0032" => ["Image Position (Patient)"],
     "0020,0035" => ["Image Orientation"],
     "0020,0037" => ["Image Orientation (Patient)"],
     "0020,0050" => ["Location"],
     "0020,0052" => ["Frame of Reference UID"],
     "0020,0060" => ["Laterality"],
     "0020,1002" => ["Images in acqusition"],
     "0020,1040" => ["Position Reference Indicator"],
     "0020,1041" => ["Slice Location"],
     "0028,0000" => ["Relationship information group"],
     "0028,0002" => ["Samples per Pixel"],
     "0028,0004" => ["Photometric Interpretation"],
     "0028,0005" => ["Image Dimensions"],
     "0028,0010" => ["Rows", "US"],
     "0028,0011" => ["Columns", "US"],
     "0028,0030" => ["Pixel Spacing"],
     "0028,0100" => ["Bits Allocated","US"],
     "0028,0101" => ["Bits Stored", "US"],
     "0028,0102" => ["High Bit","US"],
     "0028,0103" => ["Pixel Representation","US"],
     "0028,1052" => ["Rescale Intercept"],
     "0028,1053" => ["Rescale Slope"],
     "0033,1002" => ["IMGF"],
     "7fe0,0000" => ["Pixel Data Information Group", "UL"],
     "7fe0,0010" => ["Pixel Data","OW"]
     );

=head1 FUNCTIONS

=head2 rdcm

=for ref

Read a dicom image.

=for usage

 $dcm = rdcm("filename")

=cut
  
sub rdcm {
    my $file = shift;
    my $options = shift;
    my $do_print_info = 0;
    my (%info, %bin);
    my $do_raw = 0;   # Only for debugging
    my ($rescale_intercept, $rescale_slope) = (0, 1);
    my $do_explicit = 0;
    my $do_guess_endian = 1;

    # options
    if ($options) {
	if (defined $options->{do_print_info}) {
	    $do_print_info = $options->{do_print_info};
	}
    }

    open(IN, $file) || die "Failed opening image $file!\n";
    # read the whole image
    my $header;
    read(IN, $header, -s $file);
    
    # File preamble - a fixed 128 byte field
    my $hpos = 0x80;
    
    # Next four bytes should be DICM
    if (substr($header, $hpos, 4) ne 'DICM') {
	die "This is not a DICM file!\n";
    }
    $hpos+= 4;

    # Precheck if the first entry has explicit vr. Unfortunately this
    # is not enough to determine if the file always has explicit value
    # representation.
    if (substr($header, $hpos+4, 2)=~ /[A-Z]{2}/) {
	$do_explicit++;
    }

    while($hpos < length($header)) {
	my $is_binary = 0;
	my $groupword = unpack("v", substr($header, $hpos, 2));     $hpos+=2;
	my $elementword = unpack("v", substr($header, $hpos, 2));   $hpos+=2;
	my $value_rep = substr($header, $hpos, 2);
	my $key = sprintf("%04x,%04x", $groupword, $elementword);
	my ($lookup) = $element_table{$key};
	my $override_vr = 0;
	
	# Check for explicit value representation. There must be a different
	# way to figure this out, but I still haven't figured out how!
	if ($value_rep =~ /[A-Z][A-Z]/) {
	    $hpos+=2;
	}
	else {
	    $override_vr++;
	    $value_rep = $lookup->[1] || "UN";
	}

	my $elementlength;
	if (substr($header, $hpos, 4) eq "IMGF") {
	    die "No support for IMGF files at the moment!\n";
	}

	# The following calculation agrees with dicom3tools
	if ($override_vr) {
	    $elementlength = unpack("V", substr($header, $hpos, 4));
	    $hpos+= 4;
	}
	elsif (grep($value_rep eq $_, qw(OB OW SQ UN))) {
	    # Long length
	    $is_binary = 1;
	    $hpos+= 2;         # Always zero
	    $elementlength = unpack("V", substr($header, $hpos, 4));
	    $hpos+= 4;
	}
	else {
	    # Short length
	    $elementlength = unpack("v", substr($header, $hpos, 2));
	    $hpos+= 2;
	}

	my ($descr) = "";
	my $contents = substr($header, $hpos, $elementlength) . "";
	
	($descr) = @$lookup if $lookup;
	
	# recode contents
	if    ($value_rep eq "UL") {   $contents = unpack("V", $contents); }
	elsif ($value_rep eq "US") {   $contents = unpack("v", $contents); }
	elsif ($value_rep eq "TM") {   $contents = clean_time($contents); }
	elsif ($value_rep eq "DA") {   $contents = clean_date($contents); }
	elsif (!$is_binary) {
	    $contents=~ s/\s+$//;
	}
	
	# store the contents
	if ($key eq "7fe0,0010") {  # Pixel data
	    $bin{$descr} = $contents;
	}
	else {
	    $info{$descr} = $contents if $descr;
	}
	
	if ($do_print_info) {
	    $contents = "" unless $contents;
	    $contents = "<<Binary>>" if $is_binary;
	    $contents=~ tr/\0-\037\200-\377//d;
	    $contents= substr($contents, 0, 40) unless $do_raw;
	    $contents=~ s/[\0-\037\200]/?/g;
	    $value_rep=~ s/[\0-\037]/?/g;
	    printf STDERR "%04x> %04x,%04x (%04x,$value_rep) %-30s : %s\n",
	    $hpos, $groupword, $elementword, $elementlength, $descr, $contents;
	}
	
	$hpos+= $elementlength;
    }
    
    my($width, $height) = ($info{Columns}, $info{Rows});
    my($bs) = $info{"Bits Allocated"}/8;
    my $img = $bin{"Pixel Data"};
    # The following logics works on Intel endian
    my $do_toggle_endian = $info{"Pixel Representation"};
    $rescale_intercept = $info{"Rescale Intercept"} if defined $info{"Rescale Intercept"};
    $rescale_slope = $info{"Rescale Slope"} if defined $info{"Rescale Slope"};

    my $pdl;
    
    # Create a pdl from the raw data
    if ($bs == 2) {
	$pdl = zeroes(ushort, $width,$height);
	my $hdr = $pdl->gethdr;
	$pdl->make_physical();

	# Store the pixel data
	${$pdl->get_dataref()} = $img;

        # Guess endian
        if ($do_guess_endian) {
	    # Compare spread of high byte with low byte
	    my $high_byte = ($pdl>>8)->byte;
	    my $low_byte = ($pdl & 0xff)->byte;
	    my $max_high = $high_byte->max;
	    my $max_low = $low_byte->max;

	    # print STDERR "max_high max-low = $max_high $max_low\n";

	    # The following might need to be adjusted on different
	    # architectures.
	    $do_toggle_endian = $max_high > $max_low;
        }
    
        # Endian swap
        if ($do_toggle_endian) {
            bswap2($pdl);
	}

        # Rescale and convert to double
        if ($rescale_intercept != 0
	    || $rescale_slope != 1) {
	    print STDERR "scaling with $rescale_slope and $rescale_intercept\n";
	    $pdl = 1.0*($pdl * $rescale_slope) + $rescale_intercept;
	}
    } else {
	die "Sorry! PDL::IO::Dicom currently only supported DICOM with bs=2. bs = $bs\n";
    }

    # Store the info in the pdl header
    $pdl->sethdr(\%info);
    
    return $pdl;
}

sub clean_time {
    my $time = shift;
    my ($hour, $min, $sec, $sec_frac);
    if ($time=~ /(\d+):(\d+):(\d+)\.?(\d*)/) {
	($hour,$min,$sec,$sec_frac) = ($1,$2,$3,$4);
    }
    elsif ($time=~ /(\d\d)(\d\d)(\d\d)\.?(\d+)/) {
	($hour,$min,$sec,$sec_frac) = ($1,$2,$3,$4);
    }

    if (defined $hour) {
	$time = sprintf("%02d:%02d:%02d", $hour, $min, $sec);
	$time .= ".$sec_frac" if $sec_frac;
    }
    return $time;
}

sub clean_date {
    my $date = shift;
    $date=~ s/(\d\d\d\d)(\d\d)(\d\d)/$1-$2-$3/;
    return $date;
}

=head1 AUTHOR

Copyright (C) Dov Grobgeld <dov@imagic.weizmann.ac.il> 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

1;
