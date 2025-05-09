use strict;
use warnings;
use Test::More;
use ExtUtils::Manifest;

plan tests => 2;

is_deeply [ ExtUtils::Manifest::manicheck() ], [], 'missing';
is_deeply [ ExtUtils::Manifest::filecheck() ], [], 'extra';
