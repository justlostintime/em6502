#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <ctmon65.h>

//  Define the physical interface to the screen
#define ScreenBase  0xE030                       // The io address of the screen

enum GraphicRegisters {CmdRegister=ScreenBase, ScreenBorderL, ScreenBorderH,DrawAreaBgL,DrawAreaBgH,PixelColorH,WriteAddrL,WriteAddrH,SetPixelInc,SetPixel};
char *RegisterNames[] = {"CmdRegister", "ScreenBorderL", "ScreenBorderH", "DrawAreaBgL", "DrawAreaBgH", "PixelColorH", "WriteAddrL", "WriteAddrH", "SetPixelInc", "SetPixel"};
// Define the Commands
#define doRefresh     1                          // Refresh the screen
#define doClear       doRefresh+1                // Clear the screen
#define doScrollUp    doClear+1                  // Scroll up 1 pixel
#define doScrollDown  doScrollUp+1               // Scroll down 1 pixel
#define doScrollLeft  doScrollDown+1             // Scroll left one pixel
#define doScrollRignt doScrollLeft+1             // Scroll right one pixel



void main() {
  unsigned int i;
  unsigned char *ScreenPtr = 0;
  unsigned char *ScreenRegister = 0;
  unsigned int  *ScreenAltReg = 0;
  printf("Begin the graphics program\n");
  for(i=CmdRegister; i <= SetPixel; i++) {
    printf("Register %s at %4x\n",RegisterNames[i-ScreenBase],i);
  }
  
  for(ScreenPtr=1; ScreenPtr < (320*200); ScreenPtr += 320 ) {
    ScreenAltReg    = WriteAddrL;
    *ScreenAltReg   = (int)ScreenPtr;
    ScreenRegister = SetPixel;
    *ScreenRegister = 0x25;
  }
  
  ScreenRegister = CmdRegister;
  *ScreenRegister = doRefresh;
  
}