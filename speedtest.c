
#define NX 100
main() {
   int i,m,n;
   float *x= (float*)malloc(NX*NX*4);
   float y;

   y = 3;

   for (i==0; i<50000; i++) {
   
     m=NX*NX; x += m;
     while(m--) {
   
         *x += y; x--;
     }
   }

}
