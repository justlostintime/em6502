/*****************************************************************************/
/*                                                                           */
/*                                  CT65MON.h                                */
/*                                                                           */
/*                     CT65MON system-specific definitions                   */
/*                                                                           */
/*                                                                           */
/*                                                                           */
/* (C) 2023      Westwood                                                    */
/* Email:        justlostintime@yahoo.com                                    */
/*                                                                           */
/* This software is provided 'as-is', without any expressed or implied       */
/* warranty.  In no event will the authors be held liable for any damages    */
/* arising from the use of this software.                                    */
/*                                                                           */
/* Permission is granted to anyone to use this software for any purpose,     */
/* including commercial applications, and to alter it and redistribute it    */
/* freely, subject to the following restrictions:                            */
/*                                                                           */
/* 1. The origin of this software must not be misrepresented; you must not   */
/*    claim that you wrote the original software. If you use this software   */
/*    in a product, an acknowledgment in the product documentation would be  */
/*    appreciated but is not required.                                       */
/* 2. Altered source versions must be plainly marked as such, and must not   */
/*    be misrepresented as being the original software.                      */
/* 3. This notice may not be removed or altered from any source              */
/*    distribution.                                                          */
/*                                                                           */
/*****************************************************************************/

#ifndef _CTMON65_H
#define _CTMON65_H

/* Check for errors */
#if !defined(__KIM1__)
#  error This module may only be used when compiling for the CTMON65!
#  We are pretending to be a KIM1 according to the .cfg file ....
#endif

/*****************************************************************************/
/*                                   Data                                    */
/*****************************************************************************/
struct clock_info {
  char  id ;           // message id binary value
  char  month;         // bcd value 2 digits
  char  day;           // bcd value 2 digits
  char  century;       // bcd value 4 digits
  char  year;          // bcd value 4 digits
  char  hour;          // bcd value 2 digits
  char  minute;        // bcd value 2 digits
  char  second;        // bcd value 2 digits
  char  dayofweek;     // bcd value 2 digits
};

union clock {
  struct clock_info clock_stuff;
  char   clock_bytes[9];
};

struct timer_info {
  char  id;           // message id binary
  char  timout;       // single byte timout value
};

// the timer parameter value list and meaning 
#define   timer_0ms   0     // disbale timer and irq
#define   timer_10ms  1     // 10 ms timer etc
#define   timer_20ms  2
#define   timer_30ms  3
#define   timer_40ms  4
#define   timer_50ms  5
#define   timer_100ms 6
#define   timer_250ms 7
#define   timer_500ms 8
#define   timer_1sec  9

#define    pio_req_getclock  0x07
#define    pio_rsp_getclock  0x87
#define    pio_req_setclock  0x08
#define    pio_req_settimer  0x1E
#define    pio_ack           0x82
#define    pio_nak           0x83

/*****************************************************************************/
/*                                 Hardware                                  */
/*****************************************************************************/
// when setting clock, the day of week is ignored, it is auto set by the date
int __fastcall__ GetClock (void *clockinfo); // updates the clock info from clock
int __fastcall__ SetClock (void *clockinfo); // updated the clock with the data
// -1 send data failed
// -2 Request returned a nak

int __fastcall__ init_timer (char timer_value);           // set the time and turn on interupts
unsigned long __fastcall__ get_timer ();                  // get the current timer value
int __fastcall__ get_timer_tick_length ();                 // return the parameter used to set the timer

/*****************************************************************************/
/*                                   Code                                    */
/*****************************************************************************/
#define  debugit 0
void   __fastcall__ msleep(unsigned milliseconds); // timeout in milliseconts

/* End of CT65MON.h */
#endif
