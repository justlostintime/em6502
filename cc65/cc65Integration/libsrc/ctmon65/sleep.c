//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>

// This provides the interface to sleep and msleep functions
// these are implemented as loops

unsigned __fastcall__ sleep(unsigned seconds) {
      unsigned result = 0;
      msleep(seconds*1000);         // for now we just call msleep no signals are possible!
      return result;
}

void   __fastcall__ msleep(unsigned milliseconds) {
       int timertick = get_timer_tick_length();
       unsigned waitticks = 0;
       unsigned long target;
       unsigned waited = 0;
       
       if(timertick > milliseconds) return;           // just return cant handle
       waitticks = milliseconds / timertick;          // calculate number of ticks to waitticks
       target = get_timer() + waitticks;
       while( get_timer() < target) waited++;
       return;
}