
/* A small test program for the new create / delete routines */

int main() {
	pdl *bar;
	pdl *foo = pdl_create(PDL_PERM);
	int inds[2] = {1,1};
	pdl_dump(foo);

	pdl_reallocdims(foo,2);
	foo->dims[0] = 5;
	foo->dims[1] = 6;
	pdl_reallocphysdata(foo);

	pdl_dump(foo);

	bar = pdl_createtrans(foo, pdl_affine_rectslice_transvtable);
	pdl_dump(bar);
	pdl_trans_affine_rectslice *trans =
		   ((pdl_trans_affine_rectslice *)(foo->trans));

	trans->starts[0] = 1;
	trans->ends[0] = 3;

	trans->starts[1] = 2;
	trans->ends[1] = 4;
	trans->steps[1] = 2;

	pdl_transchanged(bar);

	pdl_dump(bar);

	pdl_make_physical_affine(bar);

	pdl_dump(bar);

	pdl_make_physical(bar);

	pdl_dump(bar);

	pdl_set(bar,2.0,inds);
	pdl_changed(bar);

	pdl_dump(foo);

	pdl_make_physical_affine(foo);
}


