#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

int copy(char *source, char *dest);
void seperator();

#define  blocklen    1
#define  blockcount  1019
#define  bufferlen   1020

void main() {
  char *source = "source.txt";
  char *dest   = "acopy.txt";
  int i,j,k;
  char *ptr, *result;
  FILE *myfile;
  long fnum = 26 ;

  for(i=0; i < 10; i++) {
    ptr = malloc(bufferlen);
    strcpy(ptr, "this is a string");
    printf("\n Dynamic hello world %s - %d\n",ptr,i);
    printf("\n Static Hello World\n");
    free(ptr);
  }

  k = 0;
  printf("Begin Loop test 500,000");
  for(i=0; i<100; i++) {
    for(j=0; j<50; j++) {
      k++;
    }
  }

  printf("End loop test\n");
  seperator();
  printf("Try to putchar('a')\n");

  for(i=0; i < 10; i++) putchar('a');
  seperator();
  printf("Completed putch test\n");
  fnum = 0x20000;
  printf("The long integer number is %ld\n",fnum);

  seperator();
  printf("Begin open close read write tests\n");
  i = open(source,O_RDONLY);
  seperator();
  printf("Test reading a file \n Open %s returned %d, Error number %d\n",source,i,errno);
  ptr = malloc(bufferlen);
  k=1;
  seperator();
  printf("Begin Reading test from %s Data: \n",source);

  while (k != 0) {
    k = read(3,ptr,bufferlen-1);
    if (k > 0) {
      printf("\nRead %d bytes from %s Data:\n---------------------------------\n",k,source);
      ptr[k] = 0;
      printf("%s\n",ptr);
    }
  }
  seperator();
  printf("Read Test Complete for %s\n",source);

  j = close(i);
  printf("Closing %s returned %d, Error number %d\n",source,j,errno);
  seperator();

  myfile = fopen(source,"r");
  printf("Begining test of fread/fclose/fopen of %s returned %d, Error number %d\n",source,myfile,errno);

  k = fread(ptr,1,blockcount,myfile);
  ptr[k*blocklen] = 0;
  printf("fread %s Count returned %d bytes and %d elements, Error number %d\nData fread:\n",source,k*blocklen,k,errno);
  printf("%s\n",ptr);
  seperator();
  i = fclose(myfile);
  printf("\nfclose %s returned %d, Error number %d\n",source,i,errno);
  seperator();
  printf("Begin Copy test\n");

  ptr[0] = 0;  // clear the buffer
  free(ptr);

  if (!copy(source,dest)) {
    printf("Copy completed correctly, reading %s to comfirm\n",dest);
    myfile = fopen(dest,"r");
    printf("After Copy fopen %s returned %d, Error number %d\nDoing fscanf\n",dest,myfile,errno);

    if (myfile != 0) {
      ptr = malloc(bufferlen);
      result = fgets(ptr,bufferlen-1,myfile);
      printf("fgets() of created file %s Read returned pointer  %d :\n",dest,result);

      if(result != NULL) {
        printf("Data Read from %s:\n%s\nEnd of print\n",dest,ptr);
        } else {
        printf("File %s Read after copy failed\n",dest);
      }

      free(ptr);
      k = fclose(myfile);
      printf("fclose created %s file returned %d, Error number %d\n",dest,k,errno);

    }
    //k = remove("acopy.txt");
    //printf("Remove returned %d, Error number %d\n",k,errno);
  }
  printf("All tests complete");


}

int copy(char *source, char *dest) {
  char *buffer;
  int index = 0, i = 1;
  FILE *myfile;
  seperator();
  printf("\nCopy file function called\n");
  buffer = malloc(bufferlen);

  if (buffer == NULL) {
    printf("Copy Memory allocation failed\n");
    return -1;
    } else{
    printf("Copy memory allocation address = %d\n",buffer);
  }


  myfile = fopen(source,"r");
  if (myfile == NULL) {
    printf("Copy Unable to open source file %s error number %d\n",source,errno);
    return -1;
  }

  printf("Copy fopen source %s returned %d, Error number %d\nUsing fread to read each line\n============\n",source,myfile,errno);
  buffer[0] = 0;

  while(i > 0 && i < blockcount-1 ) {
    i = fread(buffer+index,blocklen,blockcount-1,myfile);
    if(i>0) {
      index = blocklen*i;
      printf("Copy fread returned %d elements read, %d bytes\n",i,index);
    }
  }

  buffer[index] = 0;
  seperator();
  printf("Copy Read %d bytes from file %s\n",index,source);

  i = fclose(myfile);
  printf("Copy fclose source file %s returned %d, Error number %d\n",source ,i,errno);
  seperator();
  myfile = fopen(dest,"w");

  printf("Copy fopen %s returned %d, Error number %d\n",dest,myfile,errno);

  if (myfile != NULL) {
    printf("Copy writing to %s writing  %d bytes\n",dest,index);
    i = fwrite(buffer, 1 ,index , myfile);
    printf("Copy wrote %d bytes to destination %s\n",i,dest);
    fclose(myfile);
    } else {
    seperator();
    printf("Copy failed to %s\n",dest);
    free(buffer);
    return -1;
  }

  printf("Copy completed from %s to %s\n",source,dest);
  seperator();
  free(buffer);
  return 0;
}

void seperator() {
  printf("\n==============================================================\n");
  return;
}









