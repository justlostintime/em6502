
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>


int __fastcall__ GetClock (void *clockinfo)
{
  
  int result = 0;
  int fn;
 
  //printf("\n***GetClock clockinfo address passed %04x***\n",clockinfo);
  
  fn = open("/dev/pio",O_RDWR);
  if (fn == -1 || fn != 4) {
    //printf("Open error %d file# %d\n",errno,fn);
    return fn;
  }
  
  *(char*)clockinfo = pio_req_getclock;
  result = write(fn,clockinfo,1) ;                       // request is only one byte
  if (result != 1) {
    result = -1 ;
  }
  else {
    result = read(fn,clockinfo,sizeof(struct clock_info));
    if (result != sizeof(struct clock_info)) {
         result = -1;
    } else {
      if (*(char *)clockinfo == pio_nak) {
        result = -2;
      }else {
        result = 0;
      }
    }
  }

  close(fn);

  return result;
}

int __fastcall__ SetClock (void *clockinfo)
{
  int result = 0;
  int fn;
  char * buffer;
  buffer = (char *)clockinfo;

  //printf("\n***SetClock clockinfo address passed %04x-%04x***\n",clockinfo,buffer);
  
  fn = open("/dev/pio",O_RDWR);
  if (fn == -1) {
    return fn;
  }

  buffer[0]  = pio_req_setclock;
  result = write(fn,buffer,sizeof(struct clock_info));           // request is only one byte
  
  //printf("SetClock Wrote request of %d bytes\n",result);
  
  if (result != sizeof(struct clock_info)) {
    //printf("SetClock invalid number of characters written %d\n",result);
    result = -1 ;
  }
  else {
    //printf("SetClock read ack/nak message from set\n");
    result = read(fn,clockinfo,1); 
    //printf("SetClock Read %d bytes, value = %02x\n",result,buffer[0]);                              // ack or nak
    if (result != 1) {
      //printf("SetClock write failed result=%d, returned message = %02x",result, buffer[0]);
       result = -1;
    } else{
       if (buffer[0] == pio_nak) {
         //printf("SetClock Nak recieved to request\n");
         result = -2;
       } else {
        result = 0;
       }
    }
  }

  close(fn);

  return result;
}