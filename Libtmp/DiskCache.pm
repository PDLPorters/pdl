=head1 NAME

PDL::DiskCache -- Non-memory-resident array object

=head1 SYNOPSIS

NON-OO:

   use PDL::DiskCache;
   tie @a,'PDL::DiskCache', \@files, \%options;
   imag $a[3];

OO:

   use PDL::DiskCache;
   $x = diskcache(\@files,\%options);
   imag $x->[3];

or

   use PDL::DiskCache;
   $x = new PDL::DiskCache(\@files,\%options);
   imag $x->[4];

=over 3

=item \@files 

an array ref containing a list of file names

=item \%options 

a hash ref containing options for the PDL::DiskCache object (see "TIEARRAY"
below for details)

=back

=head1 DESCRIPTION

A PDL::DiskCache object is a perl L<"tied array"|perltie> that is useful
for operations where you have to look at a large collection of PDLs  one
or a few at a time (such as tracking features through an image sequence).  
You can write prototype code that uses a perl list of a few PDLs, then 
scale up to to millions of PDLs simply by handing the prototype code
a DiskCache tied array instead of a native perl array.  The individual
PDLs are stored on disk and a few of them are swapped into memory on a
FIFO basis.  You can set whether the data are read-only or writeable.

By default, PDL::DiskCache uses FITS files to represent the PDLs, but
you can use any sort of file at all -- the read/write routines are the
only place where it examines the underlying data, and you can specify 
the routines to use at construction time (or, of course, subclass 
PDL::DiskCache).

Items are swapped out on a FIFO basis, so if you have 10 slots
and an expression with 10 items in it then you're OK (but you probably
want more slots than that); but if you use more items in an expression than
there are slots, thrashing will occur!

The hash ref interface is kept for historical reasons; you can access
the sync() and purge() method calls directly from the returned array ref.

=head1 Shortcomings & caveats

There's no file locking, so you could really hose yourself by having two of
these things going at once on the same files.

Since this is a tied array, things like Dumper traverse it transparently.
That is sort-of good but also sort-of dangerous.  You wouldn't want to
PDL::Dumper::sdump() a large PDL::DiskCache, for example -- that would defeat
the purpose of using a PDL::DiskCache in the first place.



=head1 Author, license, no warranty

Copyright 2001, Craig DeForest

This code may be distributed under the same terms as Perl itself
(license available at L<http://www.perl.org>).  Copying, reverse engineering,
distribution, and modification are explicitly allowed so long as this notice
is preserved intact and modified versions are clearly marked as such.

If you modify the code and it's useful, please send a copy of the modified
version to cdeforest@solar.stanford.edu.

This package comes with NO WARRANTY.

=head1 FUNCTIONS

=cut

######################################################################
# Package initialization
$PDL::DiskCache::VERSION = 1.1;
 
use strict;
use Carp;

=head2 diskcache

Object constructor. 

=for usage

  $x = diskcache(\@f,\%options);

Options

=over 3

=item

See the TIEARRAY options,below.

=back

=cut

sub diskcache {
  my($f,$opt) = @_;
  return PDL::DiskCache::new('PDL::DiskCache',$f,$opt);
}

sub PDL::DiskCache::new {
  my($class,$f,$opt) = @_;
  my($x)=[];

  my($y) = tie @{$x},$class,$f,$opt;
  if($opt->{bless}) {
    $x = bless($x,$class);
  }

  if(wantarray) {
    return ($x,bless($y,$class));
  } else {
    return $x;
  }
}

*PDL::DiskCache::diskcache = *diskcache;

=head2 TIEARRAY

=for ref 

Tied-array constructor; invoked by perl during object construction.

=for usage

  TIEARRAY(class,\@f,\%options)

Options

=over 3

=item ro (default 0)

If set, treat the files as read-only (modifications
to the tied array will only persist until the changed elements are
swapped out)

=item rw (default 1)

If set, allow reading and writing to the files.
Because there's currently no way to determine reliably whether a PDL
has been modified, rw files are always written to disk when they're
swapped out -- this causes a slight performance hit.

=item mem (default 20)

Number of files to be cached in memory at once.

=item read (default \&rfits)

A function ref pointing to code that will read
list objects from disk.  The function must have the same syntax as
rfits: $object = rfits(filename).

=item write (default \&wfits)

A function ref pointing to code that will
write list objects to disk.  The function must have the same syntax as
wfits: func(object,filename).

=item bless (default 0)

If set to a nonzero value, then the array ref gets
blessed into the DiskCache class for for easier access to the "purge"
and "sync" methods.  This means that you can say C<< $x->sync >> instead
of the more complex C<< (%{tied @$x})->sync >>, but C<ref $x> will return
"PDL::DiskCache" instead of "ARRAY", which could break some code.

=item verbose (default 0)

Get chatty.

=back

=cut

sub PDL::DiskCache::TIEARRAY { 
  my($class,$f,$opt) = @_;

  croak "PDL::DiskCache needs array ref as 2nd arg (did you pass an array instead?)\n"
    if(ref $f ne 'ARRAY');
  my($new) = {files   => $f                                # File list
	      , n       => scalar(@{$f})                     # no. of el.
	      , write   => $opt->{write} || \&main::wfits  # Write routine
	      , read    => $opt->{read} || \&main::rfits   # Read routine
	      , mem     => $opt->{mem} || 20               # No. of mem slots
	      , rw      => (!($opt->{ro}))                 # rw or ro
	      , fdex    => []    # Current file stored in each slot, by slot
	      , slot    => []    # Current slot in which each file is stored
	      , cache   => []    # Actual cached stuff gets held here
	      , opt     => {}    # Options stashed here for later reference
              , cache_next => 0  # Next cache slot to be used
	      };
  foreach $_(keys %{$opt}) {
    $new->{opt}->{$_} = $opt->{$_};
  }

  return bless($new,$class);
}

=head2 purge

Remove an item from the oldest slot in the cache, writing to disk as necessary.
You also send in how many slots to purge (default 1; sending in -1 purges
everything.)

For most uses, a nice MODIFIED flag in the data structure could save
some hassle here.  But PDLs can get modified out from under us 
with slicing and .= -- so for now we always assume everything is tainted
and must be written to disk.

=cut

sub PDL::DiskCache::purge {
  my($me,$n) = @_,1;
  $me = (tied @{$me}) if("$me" =~ m/^PDL\:\:DiskCache\=ARRAY/);

  $n = $me->{mem} if($n<0);
  
  print "purging $n items..." if($me->{opt}->{verbose});

    
  my($dex) = $me->{cache_next};

  local($_);
  for(1..$n) {
    if($me->{rw}) {
      print "writing $me->{files}->[$me->{fdex}->[$dex]]: " 
	if($me->{opt}->{verbose});

      eval {&{$me->{write}}($me->{cache}->[$dex],
			    $me->{files}->[$me->{fdex}->[$dex]]);
	  };
      print "WARNING: PDL::DiskCache::purge: problems with write of ".$me->{files}->[$me->{fdex}->[$dex]].", item $me->{fdex}->[$dex] from slot $dex: `$@' (".$me->{opt}->{varname}.") \n" if($@);
      $@ = 0;

      print "ok.\n" if($me->{opt}->{verbose});
    }
    

    print "Purging item $dex (file $me->{fdex}->[$dex])...\n" if($me->{opt}->{verbose});
    undef $me->{slot}->[$me->{fdex}->[$dex]];  # Purge from slot location list
    undef $me->{fdex}->[$dex];                 # Purge from slot fdex list
    undef $me->{cache}->[$dex];                # Purge from memory

    $dex++;
    $dex %= $me->{mem};
  }
  print "...done with purge.\n" if($me->{opt}->{verbose});
}

sub PDL::DiskCache::FETCH {
  my($me,$i) = @_;

  if($i < 0 || $i >= $me->{n}) {
    carp("PDL::DiskCache: Element $i is outside range of 0-",$me->{n}-1,"\n");
    return undef;
  }

  if(defined $me->{slot}->[$i]) {
    print "Item $i is in the cache...\n" if ($me->{opt}->{verbose});
    return ($me->{cache}->[$me->{slot}->[$i]]);
  }
  
  ### Got here -- we have to get the item from disk.  

  print "Item $i ($me->{files}->[$i]) not present. Retrieving..."
    if($me->{opt}->{verbose});
  
  if(defined($me->{fdex}->[$me->{cache_next}])) {
    print "cache full..." if($me->{opt}->{verbose});

    $me->purge(1);
  } 
  
  my($x) = $me->{cache_next};
  $me->{cache}->[$x] = eval {
    &{$me->{read}}($me->{files}->[$i])
    } ;
  undef $@; # Keep this from hanging anything else.
  print "result is ",(defined $me->{cache}->[$x] ? "" : "un")."defined.\n"
    if($me->{opt}->{verbose});

  $me->{slot}->[$i] = $me->{cache_next};
  $me->{fdex}->[$me->{cache_next}] = $i;
  $me->{cache_next}++;
  $me->{cache_next} %= $me->{mem};
  $me->{cache}->[$x];
}

sub PDL::DiskCache::STORE {
  my($me, $i, $val) = @_;

  if( $me->{slot}->[$i] ) {
    print "Storing index $i, in cache\n" if($me->{opt}->{verbose});
    $me->sync($i);
    return $me->{cache}->[$me->{slot}->[$i]] = $val;
  } else {
    print "Storing index $i, not in cache\n" if($me->{opt}->{verbose});
    if(defined ($me->{fdex}->[$me->{cache_next}])) {
      print "cache full..." if($me->{opt}->{verbose});
      $me->purge(1);
    }
    
    my($x) = $me->{cache_next};
    $me->{slot}->[$i] = $x;
    $me->{fdex}->[$x] = $i;
    $me->{cache_next}++;
    $me->{cache_next} %= $me->{mem};
    $me->sync($i);
    return $me->{cache}->[$x] = $val;
  }

  croak("This never happens");

}
 
sub PDL::DiskCache::FETCHSIZE { 
  my($me) = shift;

  $me->{n};
}

sub PDL::DiskCache::STORESIZE { 
  my($me,$newsize) = @_;

  if($newsize > $me->{n}) {
    croak("PDL::DiskCache:  Can't augment array size (yet)!\n");
  }
  
  for( my($i) = $newsize-1; $i<$me->{n}; $i++ )  {
    if(defined $me->{slot}->[$i]) {
      if($me->{rw}) {
	print "Writing $me->{files}->[$me->{fdex}->[$i]]\n"
	  if($me->{opt}->{verbose});
	eval {&{$me->{write}}($me->{cache}->[$me->{slot}->[$i]],
			     $me->{files}->[$i]);
	    };
	$@ = 0; # Keep from hanging anything else
      }
      undef $me->{fdex}->[$me->{slot}->[$i]];
    }
  }
  $#{$me->{slot}} = $newsize-1;
  $#{$me->{files}} = $newsize-1;
  $me->{n} = $newsize;
}

=head2 sync

In a rw cache, flush items out to disk but retain them in the cache.

Accepts a single scalar argument, which is the index number of a
single item that should be written to disk. Passing (-1), or no
argument, writes all items to disk, similar to purge(-1).

For ro caches, this is a not-too-slow (but safe) no-op.

=cut

sub PDL::DiskCache::sync {
  my($me) = shift;
  $me = (tied @{$me}) if("$me" =~ m/^PDL\:\:DiskCache\=ARRAY/);
  my($syncn) = shift;
  $syncn = -1 unless defined $syncn;
  print "PDL::DiskCache::sync\n" if($me->{opt}->{verbose});
  
  my @list = $syncn==-1 ? (0..$me->{mem}-1) : ($syncn);

  if($me->{rw}) {
    for(@list) {
      if(defined $me->{fdex}->[$_]) {

	print "  writing $me->{files}->[$me->{fdex}->[$_]]...\n"
	  if($me->{opt}->{verbose});

	eval {&{$me->{write}}($me->{cache}->[$_],
			      $me->{files}->[$me->{fdex}->[$_]]);
	    };
	$@ = 0; # keep from hanging anything else
      }
    }
  }
}

=head2 DESTROY

This is the perl hook for object destruction.  It just makes a call to
"sync", to flush the cache out to disk.  Destructor calls from perl don't
happen at a guaranteed time, so be sure to call "sync" if you need to 
ensure that the files get flushed out, e.g. to use 'em somewhere else.

=cut

sub PDL::DiskCache::DESTROY {
  my($me) = shift;

  $me->sync;
}

# return true
1;
