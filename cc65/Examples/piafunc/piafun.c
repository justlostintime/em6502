#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>

#define  clockrsplen

void printDate(struct clock_info *c);

char getclockinfo[] = { 0x07};

void main() {
  struct clock_info buffer;
  struct clock_info newtime;

  char *textbuff ;
  int length = 0, i =0;

  int pio_rdwr;

  pio_rdwr = open("/dev/pio",O_RDWR);

  printf("open read/write returned %d\n",pio_rdwr);
  if (pio_rdwr != 4) {
    printf("Error on open WR=%d, Error number %d\n",pio_rdwr,errno);
    return;
  }

  printf("Sending message get clock info length =%d\n",sizeof(getclockinfo));
  length = write(pio_rdwr, getclockinfo , sizeof(getclockinfo));
  printf("write returned : %d\n",length);
  printf("Reading message get clock info length =%d, buffer address :%04x\n",sizeof(struct clock_info),&buffer);
  length = read(pio_rdwr, &buffer, sizeof(buffer));

  textbuff =(char*)&buffer;
  printf("\nRecieved buffer %04x:",textbuff);
  for (i=0; i < length; i++){
    printf("$%02X,",textbuff[i]);
  }
  printf("\n");

  printf("read returned : %d\n",length);
  printDate(&buffer);
  printf("Now do same with system function buffer address %04x\n",&newtime);

  GetClock(&newtime);

  printDate(&newtime);
  
  printf("Now move time and date back in time\n");
  newtime.month   = 0x12;
  newtime.day     = 0x1;
  newtime.century = 0x19;
  newtime.year    = 0x26;
  newtime.hour    = 0x0;
  newtime.minute  = 0x1;
  newtime.second  = 0x30;
  newtime.dayofweek = 0x2;
  printDate(&newtime);
  SetClock(&newtime);
  GetClock(&newtime);
  printDate(&newtime);
  
}

void printDate(struct clock_info *c) {

  printf("%02x-%02x-%02x%02x %02x:%02x:%02x %02x\n",c->month,c->day,c->century,c->year,c->hour,c->minute,c->second,c->dayofweek);

}







