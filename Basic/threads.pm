package PDL::Parallel::threads;

use strict;
use warnings;
use Carp;
use PDL::LiteF;
use PDL::Exporter;
our $VERSION = '0.07';

our @ISA = ( 'PDL::Exporter' );
our @EXPORT_OK = qw(share_pdls retrieve_pdls free_pdls);
our %EXPORT_TAGS = (Func=>\@EXPORT_OK);

my $can_use_threads;
BEGIN {
	$can_use_threads = eval {
		require threads;
		threads->import();
		require threads::shared;
		threads::shared->import();
		1;
	};
}

# These are the means by which we share data across Perl threads. Note that
# we cannot share ndarrays directly across threads, but we can share arrays
# of scalars, scalars whose integer values are the pointers to ndarray data,
# etc.
my %datasv_pointers :shared;
my %dataref_svs;
my %dim_arrays :shared;
my %types :shared;
my %badflag :shared;
my %badvalue :shared;
my %nbytes :shared;
my %originating_tid :shared;

# PDL data should not be naively copied by Perl. Tell PDL we know about this
$PDL::no_clone_skip_warning = 1;

sub auto_package_name {
	my $name = shift;
	my ($package_name) = caller(1);
	$name = "$package_name/$name" if $name =~ /^\w+$/;
	return $name;
}

sub share_pdls {
	croak("share_pdls: expected key/value pairs")
		unless @_ % 2 == 0;
	my %to_store = @_;

	while (my ($name, $to_store) = each %to_store) {
		$name = auto_package_name($name);

		# Make sure we're not overwriting already shared data
		if (exists $datasv_pointers{$name}) {
			croak("share_pdls: you already have data associated with '$name'");
		}

		if ( eval{$to_store->isa("PDL")} ) {
			# Integers (which can be cast to and from
			# pointers) are easily shared using threads::shared
			# in a shared hash. This method provides a
			# way to obtain the pointer to the datasv for
			# the incoming ndarray, and it increments the
			# SV's refcount.
			$dataref_svs{$name} = eval{
			  croak("the ndarray does not have any allocated memory\n")
			    if !$to_store->allocated;
			  $to_store->get_dataref;
			};
			if ($@) {
				my $error = $@;
				chomp $error;
				delete $datasv_pointers{$name};
				croak('share_pdls: Could not share an ndarray under '
					. "name '$name' because $error");
			}
			$datasv_pointers{$name} = 0+$dataref_svs{$name};
			$to_store->set_donttouchdata($nbytes{$name} = $to_store->nbytes); # protect its memory
			if ($can_use_threads) {
				$dim_arrays{$name} = shared_clone([$to_store->dims]);
				$originating_tid{$name} = threads->tid;
			}
			else {
				$dim_arrays{$name} = [$to_store->dims];
			}
			$types{$name} = $to_store->get_datatype;
			$badflag{$name} = $to_store->badflag;
			my $badval = $to_store->badvalue->sclr;
			$badval = shared_clone([$badval->Re,$badval->Im]) if ref $badval ;
			$badvalue{$name} = $badval;
		}
		else {
			croak("share_pdls passed data under '$name' that it doesn't "
				. "know how to store");
		}
	}
}



# Frees the memory associated with the given names.
sub free_pdls {
	# Keep track of each name that is successfully freed
	my @removed;

	for my $short_name (@_) {
		my $name = auto_package_name($short_name);

		# If it's a regular ndarray, decrement the memory's refcount
		if (exists $datasv_pointers{$name}) {
			delete $dataref_svs{$name};
			delete $datasv_pointers{$name};
			delete $dim_arrays{$name};
			delete $types{$name};
			delete $badflag{$name};
			delete $badvalue{$name};
			delete $nbytes{$name};
			delete $originating_tid{$name};
			push @removed, $name;
		}
		# If its none of the above, indicate that we didn't free anything
		else {
			push @removed, '';
		}
	}

	return @removed;
}

# PDL method to share an individual ndarray
sub PDL::share_as {
	my ($self, $name) = @_;
	share_pdls(auto_package_name($name) => $self);
	return $self;
}

# Method to get an ndarray that points to the shared data associated with the
# given name(s).
sub retrieve_pdls {
	return if @_ == 0;

	my @to_return;
	for my $short_name (@_) {
		my $name = auto_package_name($short_name);

		if (exists $datasv_pointers{$name}) {
			# Make sure that the originating thread still exists, or the
			# data will be gone.
			if ($can_use_threads and $originating_tid{$name} > 0
				and not defined (threads->object($originating_tid{$name}))
			) {
				croak("retrieve_pdls: '$name' was created in a thread that "
						. "has ended or is detached");
			}

			# Create the new thinly wrapped ndarray
			my $new_ndarray = PDL->new_around_datasv($datasv_pointers{$name});
			$new_ndarray->set_datatype($types{$name});
			$new_ndarray->badflag($badflag{$name});
			$new_ndarray->badvalue(ref $badvalue{$name}
				? Math::Complex->make(@{$badvalue{$name}})
				: $badvalue{$name});
			$new_ndarray->setdims(\@{$dim_arrays{$name}});
			$new_ndarray->set_donttouchdata($nbytes{$name}); # protect its memory
			push @to_return, $new_ndarray;
		}
		else {
			croak("retrieve_pdls could not find data associated with '$name'");
		}
	}

	# In list context, return all the ndarrays
	return @to_return if wantarray;

	# Scalar context only makes sense if they asked for a single name
	return $to_return[0] if @_ == 1;

	# We're here if they asked for multiple names but assigned the result
	# to a single scalar, which is probably not what they meant:
	carp("retrieve_pdls: requested many ndarrays... in scalar context?");
	return $to_return[0];
}

1;

__END__

=head1 NAME

PDL::Parallel::threads - sharing PDL data between Perl threads

=head1 SYNOPSIS

 use PDL;
 use PDL::Parallel::threads qw(retrieve_pdls share_pdls);

 # Technically, this is pulled in for you by PDL::Parallel::threads,
 # but using it in your code pulls in the named functions like async.
 use threads;

 # Also, technically, you can use PDL::Parallel::threads with
 # single-threaded programs, and even with perl's not compiled
 # with thread support.

 # Create some shared PDL data
 zeroes(1_000_000)->share_as('My::shared::data');

 # Create an ndarray and share its data
 my $test_data = sequence(100);
 share_pdls(some_name => $test_data);  # allows multiple at a time
 $test_data->share_as('some_name');    # or use the PDL method

 # Kick off some processing in the background
 async {
     my ($shallow_copy)
         = retrieve_pdls('some_name');

     # thread-local memory
     my $other_ndarray = sequence(20);

     # Modify the shared data:
     $shallow_copy++;
 };

 # ... do some other stuff ...

 # Rejoin all threads
 for my $thr (threads->list) {
     $thr->join;
 }

 use PDL::NiceSlice;
 print "First ten elements of test_data are ",
     $test_data(0:9), "\n";

=head1 DESCRIPTION

This module provides a means to share PDL data between different Perl
threads. In contrast to PDL's posix thread support (see L<PDL::ParallelCPU>),
this module lets you work with Perl's built-in threading model. In contrast
to Perl's L<threads::shared>, this module focuses on sharing I<data>, not
I<variables>.

Because this module focuses on sharing data, not variables, it does not use
attributes to mark shared variables. Instead, you must explicitly share your
data by using the L</share_pdls> function or L</share_as> PDL method that this
module introduces. Those both associate a name with your data, which you use
in other threads to retrieve the data with the L</retrieve_pdls>. Once your
thread has access to the ndarray data, any modifications will operate directly
on the shared memory, which is exactly what shared data is supposed to do.
When you are completely done using a piece of data, you need to explicitly
remove the data from the shared pool with the L</free_pdls> function.
Otherwise your data will continue to consume memory until the originating
thread terminates, or put differently, you will have a memory leak.

This module lets you share two sorts of ndarray data. You can share data for
an ndarray that is based on actual I<physical memory>, such as the result of
L<PDL::Core/zeroes>. You can also share data using I<memory mapped> files.
(Note: PDL v2.4.11 and higher support memory mapped ndarrays on all major
platforms, including Windows.) There are other sorts of ndarrays whose data
you cannot share. You cannot directly share ndarrays that have not
been physicalised, though a simple L<PDL::Core/make_physical>,
L<PDL::Core/sever>, or L<PDL::Core/copy> will give you an ndarray
based on physical memory that you can share. Also, certain functions
wrap external data into ndarrays so you can manipulate them with PDL methods.
For example, see L<PDL::Graphics::PLplot/plmap> and
L<PDL::Graphics::PLplot/plmeridians>. These you cannot share directly, but
making a physical copy with L<PDL::Core/copy> will give you
something that you can safely share.

=head2 Physical Memory

The mechanism by which this module achieves data sharing of physical memory
is remarkably cheap. It's even cheaper then a simple affine transformation.
The sharing works by creating a new shell of an ndarray for each call to
L</retrieve_pdls> and setting that ndarray's memory structure to point back to
the same locations of the original (shared) ndarray. This means that you can
share ndarrays that are created with standard constructors like
L<PDL::Core/zeroes>, L<PDL::Core/pdl>, and L<PDL::Basic/sequence>, or which
are the result of operations and function evaluations for which there is no
data flow, such as L<PDL::Core/cat> (but not L<PDL::Core/dog>), arithmetic,
L<PDL::Core/copy>, and L<PDL::Core/sever>. When in doubt, C<sever> your
ndarray before sharing and everything should work.

There is an important nuance to sharing physical memory: The memory will
always be freed when the originating thread terminates, even if it terminated
cleanly. This can lead to segmentation faults when one thread exits and
frees its memory before another thread has had a chance to finish
calculations on the shared data. It is best to use barrier synchronization
to avoid this (via L<PDL::Parallel::threads::SIMD>), or to share data solely
from your main thread.

=head2 Memory Mapped Data

As of 0.07, data sharing of memory-mapped ndarrays is exactly the
same as any other. It has not been tested with L<PDL::IO::FlexRaw>-mapped
ndarrays.

=head2 Package and Name Munging

C<PDL::Parallel::threads> lets you associate your data with a specific text
name. Put differently, it provides a global namespace for data. Users of the
C<C> programming language will immediately notice that this means there is
plenty of room for developers using this module to choose the same name for
their data. Without some combination of discipline and help, it would be
easy for shared memory names to clash. One solution to this would be to
require users (i.e. you) to choose names that include their current package,
such as C<My-Module-workspace> or, following L<perlpragma>,
C<My::Module/workspace> instead of just C<workspace>. This is sometimes
called name mangling. Well, I decided that this is such a good idea that
C<PDL::Parallel::threads> does the second form of name mangling for you
automatically! Of course, you can opt out, if you wish.

The basic rules are that the package name is prepended to the name of the
shared memory as long as the name is only composed of word characters, i.e.
names matching C</^\w+$/>. Here's an example demonstrating how this works:

 package Some::Package;
 use PDL;
 use PDL::Parallel::threads 'retrieve_pdls';

 # Stored under '??foo'
 sequence(20)->share_as('??foo');

 # Shared as 'Some::Package/foo'
 zeroes(100)->share_as('foo');

 sub do_something {
   # Retrieve 'Some::Package/foo'
   my $copy_of_foo = retrieve_pdls('foo');

   # Retrieve '??foo':
   my $copy_of_weird_foo = retrieve_pdls('??foo');

   # ...
 }

 # Move to a different package:
 package Other::Package;
 use PDL::Parallel::threads 'retrieve_pdls';

 sub something_else {
   # Retrieve 'Some::Package/foo'
   my $copy_of_foo = retrieve_pdls('Some::Package/foo');

   # Retrieve '??foo':
   my $copy_of_weird_foo = retrieve_pdls('??foo');

   # ...
 }

The upshot of all of this is that if you use some module that also uses
C<PDL::Parallel::threads>, namespace clashes are highly unlikely to occur
as long as you (and the author of that other module) use simple names,
like the sort of thing that works for variable names.

=head1 FUNCTIONS

This module provides three stand-alone functions and adds one new PDL method.

=head2 share_pdls

=for ref

Shares ndarray data across threads using the given names.

=for usage

  share_pdls (name => ndarray, name => ndarray, ...)

This function takes key/value pairs where the value is the ndarray to store,
and the key is the name under which to store
the ndarray. You can later retrieve the memory
with the L</retrieve_pdls> method.

Sharing an ndarray with physical memory (or that is memory-mapped)
increments the data's reference count;
you can decrement the reference count by calling L</free_pdls> on the given
C<name>. In general this ends up doing what you mean, and freeing memory
only when you are really done using it.

=for example

 my $data1 = zeroes(20);
 my $data2 = ones(30);
 share_pdls(foo => $data1, bar => $data2);

This can be combined with constructors and fat commas to allocate a
collection of shared memory that you may need to use for your algorithm:

 share_pdls(
     main_data => zeroes(1000, 1000),
     workspace => zeroes(1000),
     reduction => zeroes(100),
 );

=for bad

C<share_pdls> preserves the badflag and badvalue on ndarrays.

=head2 share_as

=for ref

Method to share an ndarray's data across threads under the given name.

=for usage

  $pdl->share_as(name)

This PDL method lets you directly share an ndarray. It does the exact same
thing as L</shared_pdls>, but its invocation is a little different:

=for example

 # Directly share some constructed memory
 sequence(20)->share_as('baz');

 # Share individual ndarrays:
 my $data1 = zeroes(20);
 my $data2 = ones(30);
 $data1->share_as('foo');
 $data2->share_as('bar');

Like many other PDL methods, this method returns the just-shared ndarray.
This can lead to some amusing ways of storing partial calculations partway
through a long chain:

 my $results = $input->sumover->share_as('pre_offset') + $offset;

 # Now you can get the result of the sumover operation
 # before that offset was added, by calling:
 my $pre_offset = retrieve_pdls('pre_offset');

This function achieves the same end as L</share_pdls>: There's More Than One
Way To Do It, because it can make for easier-to-read code. In general I
recommend using the C<share_as> method when you only need to share a single
ndarray memory space.

=for bad

C<share_as> preserves the badflag and badvalue on ndarrays.

=head2 retrieve_pdls

=for ref

Obtain ndarrays providing access to the data shared under the given names.

=for usage

  my ($copy1, $copy2, ...) = retrieve_pdls (name, name, ...)

This function takes a list of names and returns a list of ndarrays that
provide access to the data shared under those names. In scalar context the
function returns the ndarray corresponding with the first named data set,
which is usually what you mean when you use a single name. If you specify
multiple names but call it in scalar context, you will get a warning
indicating that you probably meant to say something differently.

=for example

 my $local_copy = retrieve_pdls('foo');
 my @both_ndarrays = retrieve_pdls('foo', 'bar');
 my ($foo, $bar) = retrieve_pdls('foo', 'bar');

=for bad

C<retrieve_pdls> preserves the badflag and badvalue on ndarrays.

=head2 free_pdls

=for ref

Frees the shared memory (if any) associated with the named shared data.

=for usage

  free_pdls(name, name, ...)

This function marks the memory associated with the given names as no longer
being shared, handling all reference counting and other low-level stuff.
You generally won't need to worry about the return value. But if you care,
you get a list of values---one for each name---where a successful removal
gets the name and an unsuccessful removal gets an empty string.

So, if you say C<free_pdls('name1', 'name2')> and both removals were
successful, you will get C<('name1', 'name2')> as the return values. If
there was trouble removing C<name1> (because there is no memory associated
with that name), you will get C<('', 'name2')> instead. This means you
can handle trouble with perl C<grep>s and other conditionals:

 my @to_remove = qw(name1 name2 name3 name4);
 my @results = free_pdls(@to_remove);
 if (not grep {$_ eq 'name2'} @results) {
     print "That's weird; did you remove name2 already?\n";
 }
 if (not $results[2]) {
     print "Couldn't remove name3 for some reason\n";
 }

=for bad

This function simply removes an ndarray's memory from the shared pool. It
does not interact with bad values in any way. But then again, it does not
interfere with or screw up bad values, either.

=head1 DIAGNOSTICS

=over

=item C<< share_pdls: expected key/value pairs >>

You called C<share_pdl> with an odd number of arguments, which means that
you could not have supplied key/value pairs. Double-check that every ndarray
(or filename) that you supply is preceded by its shared name.

=item C<< share_pdls: you already have data associated with '$name' >>

You tried to share some data under C<$name>, but some data is already
associated with that name. Typo? You can avoid namespace clashes with other
modules by using simple names and letting C<PDL::Parallel::threads> mangle
the name internally for you.

=item C<< share_pdls: Could not share an ndarray under name '$name' because ... >>

=over

=item C<< ... the ndarray does not have any allocated memory. >>

You tried to share an ndarray that does not have any memory associated with it.

=item C<< ... the ndarray's data does not come from the datasv. >>

You tried to share an ndarray that has a funny internal structure, in which
the data does not point to the buffer portion of the datasv. I'm not sure
how that could happen without triggering a more specific error, so I hope
you know what's going on if you get this. :-)

=back

=item C<< share_pdls passed data under '$name' that it doesn't know how to
store >>

C<share_pdls> only knows how to store raw data
ndarrays. It'll croak if you try to share other kinds of ndarrays, and it'll
throw this error if you try to share anything else, like a hashref.

=item C<< retrieve_pdls: '$name' was created in a thread that has ended or
is detached >>

In some other thread, you added some data to the shared pool. If that thread
ended without you freeing that data (or the thread has become a detached
thread), then we cannot know if the data is available. You should always
free your data from the data pool when you're done with it, to avoid this
error.

=item C<< retrieve_pdls could not find data associated with '$name' >>

Pretty simple: either data has never been added under this name, or data
under this name has been removed.

=item C<< retrieve_pdls: requested many ndarrays... in scalar context? >>

This is just a warning. You requested multiple ndarrays (sent multiple names)
but you called the function in scalar context. Why do such a thing?

=back

=head1 LIMITATIONS

You cannot share memory mapped files that require
features of L<PDL::IO::FlexRaw>. That is a cool module that lets you pack
multiple ndarrays into a single file, but simple cross-thread sharing is not
trivial and is not (yet) supported.

If you are dealing
with a physical ndarray, you have to be a bit careful
about how the memory gets freed. If you don't call C<free_pdls> on the data,
it will persist in memory until the end of the originating thread, which
means you have a classic memory leak. If another thread
creates a thread-local copy of the data before the originating thread ends,
but then tries to access the data after the originating thread ends,
this will be fine as the reference count of the C<datasv> will have
been increased.

=head1 BUGS

None known at this point.

=head1 SEE ALSO

L<PDL::ParallelCPU>, L<MPI>, L<PDL::Parallel::MPI>, L<OpenCL>, L<threads>,
L<threads::shared>

=head1 AUTHOR, COPYRIGHT, LICENSE

This module was written by David Mertens. The documentation is copyright (C)
David Mertens, 2012. The source code is copyright (C) Northwestern University,
2012. All rights reserved.

This module is distributed under the same terms as Perl itself.

=head1 DISCLAIMER OF WARRANTY

Parallel computing is hard to get right, and it can be exacerbated by errors
in the underlying software. Please do not use this software in anything that
is mission-critical unless you have tested and verified it yourself. I cannot
guarantee that it will perform perfectly under all loads. I hope this is
useful and I wish you well in your usage thereof, but BECAUSE THIS SOFTWARE
IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE SOFTWARE, TO THE
EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING
THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE SOFTWARE "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT
NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
SOFTWARE IS WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE
COST OF ALL NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL
ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE
THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU
OR THIRD PARTIES OR A FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER
SOFTWARE), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

=cut
