//=============================================================================
// This sketch provides an interface between older 8 bit processors and an
// SD card via a parallel port.  This is not a perfectly clean implementation
// and can be improved, but it's good enough to get the job done.
//
// This came about when I was building an SWTPC clone system for Corsham
// Technologies and people kept asking if I was going to design a disk controller
// card.  I looked at existing designs but decided there were better modern
// technologies and decided to use a micro SD card instead.  It would allow
// a modern computer to put files to/from the SD, and use the common DSK file
// format to access the large number of available collections of files for
// older machines.
//
// This uses a Link class object to communicate with the other device.  This
// was initially a parallel connection but can be almost any kind of data
// path.
//
// For terminology, the other processor is the host, and the Arduino (this
// code) is the slave.  The host drives all communications and controls
// the flow of data.
//
// This was originally done on a basic Arduino with an ATMEGA328 but it was
// a constant battle to reduce RAM usage, so I switched to a MEGA instead.
// The code is a bit easier to follow now, and a lot more functionality
// can be added as desired without running out of code space.  However, you'll
// notice inconsistent coding styles as I was trying to make this lean-and-mean
// at first, but then starting doing things a bit cleaner later.  Feel free
// to clean up code or make it better... please!
//
// Legal mumbo-jumbo: I don't really follow all the current licensing
// nonsense, so I'll just clearly state my desires: This code includes
// other open code, such as the Arduino SD library.  Observe their
// requirements.  I simply ask: (A) anything derived from this code must
// include my name/email as the original author, (B) any commerical use must
// make the current code available for free to users, and (C) I'd like an
// email letting me know what you're doing with the code, what platforms
// you're using it on, etc... it's fun to see how others are using it!
//
// Note that the protocol used between the host and this code is The
// Remote Disk Protocol Guide, available for download from the
// Corsham Tech website.
//
// This is NOT a complete implementation of the protocol!  Know problems
// (but there are probably others):
//
//    * Only supports 256 byte sectors
//    * RTC messages do not handle fields with values of FF nor the century byte
//    * File writes always append, never deletes file
//
// 04/11/2014 - Bob Applegate, Corsham Technologies.  bob@corshamtech.com
//              wwww.corshamtech.com

#include <Arduino.h>
#include <SPI.h>
#include <SD.h>
#include "Link.h"
#include "Disks.h"
#include "UserInt.h"
#include <Wire.h>
#include "RTC.h"
#include "Errors.h"



// This sets the intervale between the passes through the poll functions, expressed
// in milliseconds.

#define POLL_DELAY  100      // 1/10th of a second


// The link to the remote system.

static Link *link;

// For file operations, have a file handy.  Anyone using the file should be
// darned sure it gets closed when done!

static File myFile;

// This is the collection of disks

Disks *disks;

// This is the user interface.  Its a singleton.

UserInt *uInt;

// The real time clock

RTC *rtc;

static unsigned long lastPoll;



//=============================================================================
// This is the usual Arduino setup function.  Do initialization, then return.

void setup()
{
        Serial.begin(9600);

        Serial.println("SD Drive version 1.0");
        Serial.println("Brought to you by Bob Applegate and Corsham Technologies");
        Serial.println("bob@corshamtech.com, www.corshamtech.com");
        
        // Start up the UI soon so it can display some initial info
        // while the rest of the system comes up.
        
        uInt = UserInt::getInstance();
        
        pinMode(SD_PIN, OUTPUT);    // required by SD library

        link = new Link();
        link->begin();

        disks = new Disks();
        disks->mountDefaults();
        freeRam("Initialization complete");
        Wire.begin();

        rtc = new RTC();
        lastPoll = millis();
}



//=============================================================================
// Displays the amount of free memory to the serial port.  Useful for debugging
// but will be removed eventually.

int freeRam(char *text)
{
        extern int __heap_start, *__brkval;
        int v;
        int freemem = (int) &v - (__brkval == 0 ? (int) &__heap_start : (int) __brkval);
        Serial.print(text);
        Serial.print(" - Free memory: ");
        Serial.println(freemem);
}



//=============================================================================
// This is the main loop.  Loop gets called over and over by the Arduino
// framework, so it can return and get called again, or just have an infinite
// loop.

void loop()
{
        // Poll for any incoming commands.  If there is something to process,
        // go ahead and handle it now.  There is no time delay here; check often
        // and process incoming messages ASAP.
        
        if (link->poll())
        {
                Event *ep = link->getEvent();  // this waits for an event
                bool deleteEvent = processEvent(ep);
                
                // If the event isn't needed, delete it.  Some events need
                // responses sent back and the event is re-used.
                
                if (deleteEvent)
                {
                        link->freeAnEvent(ep);
                }
        }
        
        // See if it's time to poll the various subsystems.  This is a
        // slow poll, so put high speed polling before this logic.
        
        if (lastPoll + POLL_DELAY <= millis())
        {
                lastPoll = millis();
                
                // Poll the various subsystems
        
                uInt->poll();    // user interface
                disks->poll();   // disk subsystem
        }
}




//=============================================================================
// This processes an event that came from the host.  Returns a flag indicating
// if the event should be freed or not.  If the event is reused, returns false.
// Else returns true indicating the caller should dispose of the event.

static bool processEvent(Event *ep)
{
        bool deleteEvent = false;    // assume event should not be deleted
        
        // Now process the event based on the type.
                
        switch (ep->getType())
        {
                case EVT_GET_DIRECTORY:
                        //Serial.println("Got GET DIRECTORY");
                        link->freeAnEvent(ep);    // free it up so sendDirectory can use it
                        sendDirectory();
                        break;
                                
                case EVT_TYPE_FILE:
                        //Serial.print("Got TYPE FILE: ");
                        //Serial.println((char *)(ep->getData()));
                        typeFile(ep);
                        break;
                                
                case EVT_SEND_DATA:
                        //Serial.println("Main loop got SEND_DATA");
                        // they want bytes from the open file
                        nextDataBlock(ep);
                        break;

                case EVT_GET_MOUNTED:
                        //Serial.println("Got request for mounted drives");
                        link->freeAnEvent(ep); // free it up for sendMounted to use
                        sendMounted();
                        break;
                        
                case EVT_MOUNT:
                {
                        byte *bptr = ep->getData(); // pointer to data
                        byte drive = *bptr++;       // drive number
                        byte readonly = *bptr++;    // read-only flag
                        if (disks->mount(drive, (char *)bptr, readonly))
                        {
                                ep->clean(EVT_ACK);
                        }
                        else
                        {
                                ep->clean(EVT_NAK);
                                ep->addByte(disks->getErrorCode());
                        }
                        link->sendEvent(ep);
                        break;
                }
                                
                case EVT_UNMOUNT:   // unmount a drive.  One more byte which is the drive number
                {
                        byte *bptr = ep->getData();
                        disks->unmount(*bptr);
                        ep->clean(EVT_ACK);
                        link->sendEvent(ep);
                        break;
                }
                                
                case EVT_READ_SECTOR:
                        readSector(ep);
                        break;
                                
                case EVT_WRITE_SECTOR:
                        writeSector(ep);
                        break;
                        
                case EVT_GET_STATUS:
                        getDriveStatus(ep);
                        break;
                        
                case EVT_GET_VERSION:
                {
                        ep->clean(EVT_VERSION_INFO);  // same event type but clear all other data
                        byte *ptr = ep->getData();
                        strcpy((char *)ptr, "Corsham Technology\r\nv0.1");
                        link->sendEvent(ep);
                        break;
                }

                case EVT_GET_CLOCK:
                        //Serial.println("Got a clock request");
                        ep->clean(EVT_CLOCK_DATA);
                        if (rtc->getClock(ep->getData()) == false)
                        {
                                // Failed to get the data, so send back an error.
                                ep->clean(EVT_NAK);
                                ep->addByte(ERR_DEVICE_NOT_PRESENT);
                        }
                        link->sendEvent(ep);
                        break;

                case EVT_SET_CLOCK:
                        //Serial.println("Got a set clock request");
                        if (rtc->setClock(ep->getData()))
                        {
                                ep->clean(EVT_ACK);
                        }
                        else
                        {
                                ep->clean(EVT_NAK);
                                ep->addByte(ERR_DEVICE_NOT_PRESENT);
                        }
                        link->sendEvent(ep);
                        break;

                case EVT_DONE:
                        // Close any open file.

                        myFile.close();   // be sure it's closed
                        deleteEvent = true;
                        break;

                case EVT_WRITE_FILE:
                {
                        byte *ptr = ep->getData();
                        Serial.print("Got request to open a file for writing: ");
                        Serial.println((char *)ptr);
                        myFile = SD.open((char *)(ep->getData()), FILE_WRITE);
                        if (myFile)
                        {
                                ep->clean(EVT_ACK);
                        }
                        else
                        {
                                ep->clean(EVT_NAK);
                                ep->addByte(ERR_WRITE_ERROR);
                        }
                        link->sendEvent(ep);
                        break;
                }

                case EVT_WRITE_BYTES:
                {
                        // The first byte is the length.  If zero then the length is actually 256,
                        // else the length is the actual length.
                        
                        byte *ptr = ep->getData();
                        unsigned length = *ptr++;
                        if (length == 0)
                                length = 256;

                        Serial.print("Writing ");
                        Serial.print(length);
                        Serial.println(" bytes to the raw file");
                        
                        // Now write the block of data
                        if (myFile.write(ptr, length) == length)
                        {
                                ep->clean(EVT_ACK);     // send back an ACK.
                        }
                        else
                        {
                                Serial.println("Got a NAK!");
                                ep->clean(EVT_NAK);
                                ep->addByte(ERR_WRITE_ERROR);
                        }
                        myFile.flush();
                        link->sendEvent(ep);
                        break;
                }
                        
                default:
                        // All the unwanted toys end up here.  Maybe a garbage Event,
                        // maybe an old type, or one we haven't implemented yet.
                        
                        Serial.print("processEvent: Got unknown event type: 0x");
                        Serial.println(ep->getType(), HEX);
                        deleteEvent = true;
                        break;
        }
        return deleteEvent;
}




//=============================================================================
// This sends a directory to the host.  On entry, it is assumed there is at
// least one Event available.  Ie, free it before calling this.  This sends
// only file names at the top level, not directories, and it does not
// recurse.

static void sendDirectory()
{
        Event *eptr;
        File dir;
        
        bool go_on = true;
        
        // Open the root directory

        dir.close();
        dir = SD.open("/");
        dir.rewindDirectory();
        if (!dir.available())
        {
                Serial.println("DIR not available");
        }
                
        File entry;
                                
        while (go_on)
        {
                entry =  dir.openNextFile();
                if (entry == NULL)
                {
                        // no more files
                        go_on = false;
                }

                // Only process file names, not directories.

                if (entry && !entry.isDirectory())
                {
                        eptr = link->getAnEvent();
                        eptr->clean(EVT_DIR_INFO);    // this is a directory entry
                        
                        // Npw copy the filename into the event and send it.
                        
                        byte *bptr = (byte *)(entry.name());
                        while (*bptr)
                        {
                                eptr->addByte(*bptr++);
                        }
                        eptr->addByte(0);    // terminate the name
                        link->sendEvent(eptr);
                }
                entry.close();
        }
        
        dir.close();
        
        // Send an event letting the host know the directory is done
        
        eptr->clean(EVT_DIR_END);
        link->sendEvent(eptr);
}




//=============================================================================
// Given an event with a EVT_TYPE_FILE type, verify the file can be read and
// send back either an ACK or NAK.

static void typeFile(Event *ep)
{
        myFile.close();    // make sure an existing file is closed

        // Attempt to open the file
                                
        myFile = SD.open((char *)(ep->getData()));
        if (myFile)
        {
                ep->clean(EVT_ACK);  // woohoo!
                myFile.seek(0);
        }
        else
        {
                Serial.print("typeFile: error opening ");
                Serial.print((char *)(ep->getData()));

                 ep->clean(EVT_NAK);
                 ep->addByte(ERR_FILE_NOT_FOUND);    // misc error
         }

         link->sendEvent(ep);
}




//=============================================================================
// This sends the next block of the open file.

static void nextDataBlock(Event *ep)
{
        // They sent the maximum length they can handle.
        byte length = *(ep->getData());  // get maximum length
                                
//        Serial.print("Byte count: ");
//        Serial.println(length);

        ep->clean(EVT_FILE_DATA);
                                
        // We're going to cheat a bit here.  Add a length of 0.
        // Once we have written the data and know the length,
        // go back and put the actual length into the buffer.
                           
        ep->addByte(0);    // length is first
                                
        byte actualCount = 0;

        while (myFile.available() && actualCount < length)
        {
                ep->addByte(myFile.read());
                actualCount++;
        }
                                
        // Now go back and put in the actual length
                                
        byte *bptr = ep->getData();  // get start of buffer
        *bptr = actualCount;    // and drop in the actual length
                                
        // If end of file, close the file
                                
        if (actualCount == 0)
        {
                //Serial.println("Reached EOF, closing file");
                myFile.close();
        }
                                
        link->sendEvent(ep);
}




//=============================================================================
// This handles a request to read a disk sector.  The data structure contains
// a standard header with five bytes of data: (1) Drive number, 0 to 3, (2)
// sector size (coded), (3) track number, zero based, (3) sector number, 0
// based, and sectors per track (actual value, one based).

static void readSector(Event *ep)
{
        byte *bptr = ep->getData();  // start of arguments
        byte drive = *bptr++;
        byte sectorSize = *bptr++;    // note used for now
        unsigned long track = (unsigned long)(*bptr++);
        unsigned long sector = (unsigned long)(*bptr++);
        unsigned long sectorsPerTrack = (unsigned long)(*bptr++);
        
        // NOTE: Need to add checks for valid drive, track, sector, etc
        
        // Compute the offset.  Very simple offset calculation.
        
        unsigned long offset = ((track * sectorsPerTrack) + sector) * SECTOR_SIZE;        
        
        // Now prepare the event for sendig back the data.  Same event type,
        // but get rid of the old data and replace with exactly SECTOR_SIZE
        // number of bytes.

#ifdef LOG_READ
        Serial.print("readSector drive ");
        Serial.print(drive);
        Serial.print(", track ");
        Serial.print(track);
        Serial.print(", sector ");
        Serial.print(sector);
        Serial.print(", sec/track ");
        Serial.print(sectorsPerTrack);
        Serial.print(", offset 0x");
        Serial.print(offset >> 16, HEX);
        Serial.println(offset & 0xffff, HEX);
#endif  // LOG_READ

        byte *ptr = ep->getData();
        *ptr++ = 2;      // sector size 256 bytes
        ep->clean(EVT_READ_SECTOR);  // same event type but clear all other data
        if (disks->read(drive, offset, ptr) == false)
        {
                ep->clean(EVT_NAK);  // send error status
                ep->addByte(disks->getErrorCode());
        }
        
        // send back something
        link->sendEvent(ep);
}




//=============================================================================
// This handles a request to read a disk sector.  The data structure contains
// a standard header with five bytes of data: (1) Drive number, 0 to 3, (2)
// sector size (coded), (3) track number, zero based, (3) sector number, 0
// based, and sectors per track (actual value, one based).
//
// That is followed by one sector's worth of data to be written.

static void writeSector(Event *ep)
{
        // Pull off the arguments from the start of the message
        
        byte *bptr = ep->getData();  // start of arguments
        byte drive = *bptr++;
        byte sectorSize = *bptr++;    // note used for now
        unsigned long track = (unsigned long)(*bptr++);
        unsigned long sector = (unsigned long)(*bptr++);
        unsigned long sectorsPerTrack = (unsigned long)(*bptr++);

        // bptr is now pointing to the start of the user data to be written.
        
        // NOTE: Need to add checks for valid drive, sector, etc
        
        // Compute the offset.  Very simple offset calculation.
        
        unsigned long offset = ((track * sectorsPerTrack) + sector) * SECTOR_SIZE;
        
        // Dump the data

#ifdef LOG_WRITE
        Serial.print("writeSector drive ");
        Serial.print(drive);
        Serial.print(", track ");
        Serial.print(track);
        Serial.print(", sector ");
        Serial.print(sector);
        Serial.print(", sec/track ");
        Serial.print(sectorsPerTrack);
        Serial.print(", offset 0x");
        Serial.print(offset >> 16, HEX);
        Serial.println(offset & 0xffff, HEX);
#endif  // LOG_WRITE

        if (disks->write(drive, offset, bptr))
        {
                ep->clean(EVT_ACK);
        }
        else
        {
                ep->clean(EVT_NAK);  // send error status
                ep->addByte(disks->getErrorCode());
                Serial.print("got write error: ");
                Serial.println(disks->getErrorCode());
        }
        
        // no error handling at all!
        link->sendEvent(ep);
}




//=============================================================================
// This gets a disk drive's status, such as whether it's available or not,
// read-only, etc.

static void getDriveStatus(Event *ep)
{
        byte *bptr = ep->getData();  // start of arguments
        byte drive = *bptr++;
        byte result = disks->getStatus(drive);
        
        ep->clean(EVT_DISK_STATUS);
        ep->addByte(result);
        link->sendEvent(ep);
}



//=============================================================================
// Send a list of all mounted drives

void sendMounted(void)
{
        Event *eptr;
        char *cptr;
        
        for (int i = 0; i < MAX_DISKS; i++)
        {
                eptr = link->getAnEvent();
                eptr->clean(EVT_MOUNTED);

                // add the info about this mounted drive
                eptr->addByte((byte)i);   // drive number

                // if the drive is mounted, then fill in all the details.

                if (disks->isMounted(i))
                {
                        eptr->addByte(disks->isReadOnly(i));

                        cptr = disks->getFilename(i);
                        while (*cptr)
                        {
                                eptr->addByte(*cptr++);
                        }
                        eptr->addByte(0);
                }
                else  // not mounted so indicate so
                {
                        eptr->addByte(0);
                        eptr->addByte(0);
                }
                        
                link->sendEvent(eptr);
        }

        // Indicate all the drives were sent
        
        eptr = link->getAnEvent();
        eptr->clean(EVT_DIR_END);
        link->sendEvent(eptr);
}




//=============================================================================
// This does a simple hex dump to the serial port.  On entry, this is given a
// pointer to the data and the number of bytes to dump.  Very simplistic
// function that has no features.

void hexdump(unsigned char *ptr, unsigned int size)
{
        unsigned int offset = 0;
        
        while (size)
        {
                Serial.print(*ptr++, HEX);
                Serial.print(" ");
                size--;
        }
        Serial.println("");
}



//=============================================================================
// Given a standard disk sector size code, convert to the actual number and
// return it.  If no valid value is provided, this always returns 256.

unsigned int getSectorSize(byte code)
{
        unsigned int size = 256;    // default size
        
        switch (code)
        {
                case 1:
                        size = 128;
                        break;
                      
                case 2:
                        size = 256;
                        break;
                      
                case 3:
                        size = 512;
                        break;
                      
                case 4:
                        size = 1024;
                        break;

                default:
                        Serial.print("Got bad sector size value: ");
                        Serial.println(code);
        }
        
        return size;
}



