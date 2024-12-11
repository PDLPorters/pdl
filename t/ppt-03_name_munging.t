# Boilerplate
use strict;
use warnings;

package My::Foo;
use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls);
use Test::More;
use Test::Exception;

sequence(20)->sqrt->share_as('test');
my $short_name = retrieve_pdls('test');
my $long_name;
lives_ok { $long_name = retrieve_pdls('My::Foo/test') } 'Retrieving fully '
	. 'resolved name does not croak (that is, they exist)';
ok(all($short_name == $long_name), 'Regular names get auto-munged with the '
		. 'current package name');

sequence(20)->share_as('??foo');
lives_ok { retrieve_pdls('??foo') } 'Basic retrieval with funny name works';
throws_ok {
	retrieve_pdls('My::Foo/??foo')
} qr/retrieve_pdls could not find data associated with/
, 'Names with weird characters are not auto-munged';

done_testing;
