# Perl Data Language (PDL)

![PDL logo](https://pdlporters.github.io/images/icons/pdl.png)

| Service   |  Build status |
|:---------:|--------------:|
| GitHub CI | [![Build Status](https://github.com/PDLPorters/pdl/workflows/perl/badge.svg?branch=master)](https://github.com/PDLPorters/pdl/actions?query=branch%3Amaster) |
| Cirrus CI | [![Build Status](https://api.cirrus-ci.com/github/PDLPorters/pdl.svg?branch=master)](https://cirrus-ci.com/github/PDLPorters/pdl/master) |

[![Coverage Status](https://coveralls.io/repos/PDLPorters/pdl/badge.png?branch=master)](https://coveralls.io/r/PDLPorters/pdl?branch=master)
[![CPAN version](https://badge.fury.io/pl/PDL.svg)](https://metacpan.org/pod/PDL)

PDL ("Perl Data Language") gives standard Perl the ability to *compactly* store and *speedily* manipulate the large N-dimensional data arrays which are the bread and butter of scientific computing.

PDL turns Perl into a free, array-oriented, numerical language similar to (but, we believe, better than) such commercial packages as IDL and MatLab. One can write simple perl expressions to manipulate entire numerical arrays all at once. Simple interactive shells, `pdl2` and `perldl`, are provided for use from the command line along with the `PDL` module for use in Perl scripts.

WARNING: There is absolutely no warranty for this software package. See the file COPYING for details.

## Important reading

Before sending us your questions, please see the following files for further information, and check for any [open issues](https://github.com/PDLPorters/pdl/issues).

- `INSTALL`: Basic installation instructions
- `INTERNATIONALIZATION`: Notes re PDL i18n issue and status
- `DEPENDENCIES`: Other software that PDL relies on (and where to get it)
- `Changes`: A list of features or issues with regard to the current version, always worth checking!
- `Bugs.pod`: How to make a bug report, [`PDL::Bugs`](https://metacpan.org/pod/PDL::Bugs)
- `perldl.conf`: PDL configuration options
- `Basic/Pod/FAQ.pod`: The FAQ in pod format. Try `perldoc PDL::FAQ` after installation.
- `Basic/Pod/QuickStart.pod`: A quick overview of PDL. Try `perldoc PDL::QuickStart` after installation.
- `Basic/Pod/BadValues.pod`: A discussion of the bad value support in PDL
- `Basic/Pod/DeveloperGuide.pod`: How to participate in the development of PDL

**Note:** Most PDL documentation is available online within the PDL shell, `pdl2` (or `perldl`). Try the `help` command within either shell.


## PDL -- the package

The Perl Data Language (a.k.a. PerlDL or PDL) project aims to turn perl into an efficient numerical language for scientific computing. The PDL module gives standard perl the ability to **compactly** store and **speedily** manipulate the large N-dimensional data sets which are the bread and butter of scientific computing.  e.g. `$x=$y+$c` can add two 2048x2048 images in only a fraction of a second.

The aim is to provide tons of useful functionality for scientific and numeric analysis.

Check the [pdl web site](https://pdl.perl.org) for more information.


## Installation

Please read the file `INSTALL` for information on how to configure and install PDL. The `Changes` file contains important version specific information. Be *sure* to check for any [open issues](https://github.com/PDLPorters/pdl/issues) if you have any `INSTALL` issues.

Once you have built PDL and either installed it or done `make doctest`, try either

    perl -Mblib Perldl2/pdl2

from within the root of the PDL tree or just

    pdl2

if you have installed PDL already (`make install`) to get the interactive PDL shell.  In this shell, `help` gives you access to PDL documentation for each function separately (`help help` for more about this) and `demo` gives you some basic examples of what you can do.


## Bug Reports

You can check the existing PDL bugs on GitHub [here](https://github.com/PDLPorters/pdl/issues).

The mailing list archives can be searched/read [here](https://pdl.perl.org/?page=mailing-lists).

Questions about problems and possible bugs can be discussed via the pdl-general mailing list.  This is very useful if you are not sure what you have is a bug or not.  For example, the list is the place to go for install problems.

If you need to post a problem report, and after checking with the pdl-general list that it *is* a bug, please use the GitHub issue tracker system following the guidance in [PDL::Bugs](https://metacpan.org/pod/PDL::Bugs).


## Notes

Directory structure:

`Basic/`
: The stuff that PDL would be no use without

`Demos/`
: Showcase for PDL, type demo at the perldl prompt.

`Doc/`
: Modules for building/using the PDL documentation database

`Example/`
: Sample programs using PDL

`Graphics/`
: The stuff that PDL needs to make pictures

`IO/`
: The stuff that PDL needs to write and read strange files

`Libtmp/`
: The stuff that PDL would still be useful without but which makes PDL even more useful

`Perldl2/`
: The PDL shell version 2 source and development

`cygwin/`
: Platform specific information

`macosx/`
: Platform specific information

`t/`
: PDL tests directory

`utils/`
: Utilities relating to PDL

`win32/`
: Platform specific information


Comments are welcome - so are volunteers to write code and documentation! Please contact the developers mailing list `pdl-devel@lists.sourceforge.net` ([subscription info](https://pdl.perl.org/?page=mailing-lists)) with ideas and suggestions.

The PDL developers.


## Compilation Reports:

The [CPAN Testers' result page](https://www.cpantesters.org) provides a database showing the results of compiling PDL and many other CPAN packages on multiple platforms.
