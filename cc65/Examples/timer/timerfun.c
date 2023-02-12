#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>

char *thedatofweek[] = {"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"};

void printDateTime() {
  struct clock_info c;
  GetClock(&c);
  printf("%02x-%02x-%02x%02x %02x:%02x:%02x %s\n",c.month,c.day,c.century,c.year,c.hour,c.minute,c.second,thedatofweek[c.dayofweek]);
}

void main() {
  unsigned long currenttime = 0;
  int result;
  int loops = 20;
  result = init_timer(timer_10ms);
  printf("Init timer returned %04x, timeout parm = %d\n",result,get_timer_tick_length());
  if (result){           // start the timer running
    printf("Timer was correctly started\n");
    for (loops = 0; loops < 20; loops++) {
      printf("Sleep 1 second\n");
      sleep(1);
      printDateTime();
      printf("Call get_timer...");
      currenttime = get_timer();
      printf("%09lu tick len = %d ms\n",currenttime,get_timer_tick_length());
    };
    } else {
    printf("Timer failed to start\n");
  }
  
  stop_timer();
  
  printf("Completed test\n");

}






