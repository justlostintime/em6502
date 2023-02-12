#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>

#define  MaxPrime 20000

void printflags(char *flags, int count);

void main() {
  char buffer[100], *result;
  char *flags;
  int  *flaginit;
  int count = 0,i,halfcount,a,b;
  unsigned long starttime,endtime,final;
  
  init_timer(timer_10ms);

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

  starttime = get_timer();
  
  count++;
  halfcount = count/2;
  flags = malloc(count);
  flaginit = (int *)&flags[3];

  for(i=0; i < halfcount; i++) flaginit[i] = 1;
  flags[0]=0;
  flags[1]=0;
  flags[2]=1;
  flags[count]=1;

  for (a = 3; (a+a) < count; a++) {
    if (flags[a] == 0) continue;
    for (b = a+a; b < count; b += a) flags[b] = 0;
  }

  endtime = get_timer();
  final = (endtime-starttime) * get_timer_tick_length();
  stop_timer();
  
  printf("Completed scan in %lu.%lu seconds\nList of the Primes 0-%d:\n", final/1000,final%1000,count);
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
