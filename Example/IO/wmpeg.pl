#!/usr/bin/perl
#
# Created on: Wed 28 Jul 2010 03:41:45 PM 
# Last saved: Thu 29 Jul 2010 09:11:53 AM 

use PDL;
use PDL::IO::Pic;
use PDL::NiceSlice;

# This is a simple program to create a demo MPEG-1
# movie via the wmpeg() routine in PDL::IO::Pic
# and to test the functionality of using ffmpeg
# in place of the outdated mpeg_encoder.

# a simple parabolic trajectory ("bouncing ball")
# for 30 128x80 image frames
our $coords = pdl q[
                    [  0   1   0]
                    [  4   9   1]
                    [  8  17   2]
                    [ 12  25   3]
                    [ 16  32   4]
                    [ 20  38   5]
                    [ 24  43   6]
                    [ 28  48   7]
                    [ 32  53   8]
                    [ 36  57   9]
                    [ 40  60  10]
                    [ 44  62  11]
                    [ 48  64  12]
                    [ 52  66  13]
                    [ 56  66  14]
                    [ 60  66  15]
                    [ 64  66  16]
                    [ 68  65  17]
                    [ 72  63  18]
                    [ 76  60  19]
                    [ 80  57  20]
                    [ 84  54  21]
                    [ 88  50  22]
                    [ 92  45  23]
                    [ 96  39  24]
                    [100  33  25]
                    [104  27  26]
                    [108  19  27]
                    [112  11  28]
                    [116   3  29]
                   ];

our $frames = zeros byte, 128, 80, 30;
our $val = 250;  # start with white

# make the square ball bounce
$frames->range($coords,[10,10,1]) .= $val;

# now make the movie
$frames = $frames->(*3)->copy;
$frames->wmpeg('bounce.mpg');
