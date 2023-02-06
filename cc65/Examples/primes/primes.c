#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#define  MaxPrime 20000

void printflags(char *flags, int count);

void main() {
  char buffer[100], *result;
  char *flags;
  int  *flaginit;
  int count = 0,i,halfcount,a,b;

  while(count <=0 || count >= MaxPrime) {
    printf("Enter the number of primes 2-%d:>",MaxPrime);
    result = fgets(buffer,100,stdin);
    if(result == NULL) continue;
    count = atoi(buffer);
    if (count <= 1 || count > MaxPrime) {
      printf(" Value must be between 2 and %d\n",MaxPrime);
      continue;
    }
    break;
  }

  count++;
  halfcount = count/2;
  flags = malloc(count);
  flaginit = (int *)&flags[3];

  for(i=0; i < halfcount; i++) flaginit[i] = 1;
  flags[0]=0;
  flags[1]=0;
  flags[2]=1;
  flags[count]=1;
  //printflags(flags,count);

//300 print "Begin scan loop through from 2 to ";n;" and mark numbers, initialized  ";n;" Entries"
//320 a = 3 : b = 6
//
//330 @$[b] = 0 : b = b + a : if b <= n goto .
//350 inc a : if (a > n or a+a > n )  goto 390
//360 if @$[a] = 0 goto 350
//370 b = a + a : goto 330
//
//390 print "Completed scan"

  for (a = 3; (a+a) < count; a++) {
    if (flags[a] == 0) continue;
    for (b = a+a; b < count; b += a) flags[b] = 0;
  }

  printf("Completed scan\nList of the itsy Primes :\n");
  //printflags(flags,count);

  a=0;
  for(b = 0; b<count; b++ ) {

    if(flags[b] == 1) {
      if(a%12 == 0) printf("\n:: ");
      fprintf(stdout," %6d",b);
      a++;
    }

  }

  printf("\nTotal Primes = %d\n",a);

}

void printflags(char *flags, int count) {
  int i;
  for(i=0;i<count;i++) {
    if( i % 20 == 0) printf("\n%4d : ",i);
    printf("%1d ",flags[i]);
  }
  printf("\n\n");
}
