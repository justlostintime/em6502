//=============================================================================
// This is a collection of Disk objects.  The drive emulator isn't much use if
// only a single drive wer available, so this class maintains a collection of
// the Drive objects.  It also provides a standard interface for requesting
// services for a given virtual drive.
//
// Bob Applegate - K2UT, bob@corshamtech.com

#include <SD.h>
#include "Disks.h"
#include "Errors.h"

// This is the configuration file that is read to get the initial files
// to mount.

#define CONFIG_FILE  "SD.cfg"


// Pin with the presence sensor

#define PRESENCE_PIN 19


// Number of consecutive readings that the present pin must be in
// the same state before acting on it.

#define DEBOUNCE_THRESHOLD 5



//=============================================================================
// The constructor prepares for mounting drives.  It creates instances of
// Disk for each possible drive.

Disks::Disks(void)
{
        SD.begin(SD_PIN);

        // Create the Disk objects
        
        for (int d = 0; d < MAX_DISKS; d++)
        {
                disks[d] = new Disk();
        }
        state = FIRST_CHAR;   // for reading the config file

        userInt = UserInt::getInstance();

        pinMode(PRESENCE_PIN, INPUT);   // pin with presence bit
        presentState = false;     // assume an SD card is there
}




//=============================================================================
// The destructor.  This is never called, so don't bother doing anything.

Disks::~Disks(void)
{
}




//=============================================================================
// This gets called every 100ms for whatever needs to be done.  Currently this
// just monitors for SD card removals and insertions.

void Disks::poll(void)
{
        static bool last = false;
        static int debounceCounter = 0;

        // Get current state of the present bit
        
        bool state = digitalRead(PRESENCE_PIN);

        // I don't know if debouncing is really necessary or not, but do it
        // just in case.  If this state is the same as the last reading, then
        // see if it's debounced enough.
        
        if (state == last)
        {
                // Only increase the counter if we're below the threshold
                
                if (debounceCounter < DEBOUNCE_THRESHOLD)
                {
                        if (++debounceCounter == DEBOUNCE_THRESHOLD)
                        {
                                // Threshold is reached, so now see if the
                                // current state is different than the
                                // last known state.  No sense doing anything
                                // unless there is a change..
                                
                                if (state != presentState)
                                {
                                        // Finally, we can do the appropriate
                                        // action based on whether the card is
                                        // now present or not.
                                        
                                        if (state == true)
                                        {
                                                Serial.println("Disks::poll detected card removal");
                                                userInt->sendEvent(UI_SD_REMOVED);
                                                
                                                //tell all disks to close/unmount
                                                closeAll();
                                        }
                                        else
                                        {
                                                Serial.println("Disks::poll detected card insertion");
                                                userInt->sendEvent(UI_SD_INSERTED);
                                                SD.begin(SD_PIN);
                                                
                                                //Mount all default drives
                                                mountDefaults();
                                        }
                                        presentState = state;
                                }
                        }
                }
        }
        else
        {
                // state is different than last time, reset counter.
                
                debounceCounter = 0;
                last = state;
        }
}




//=============================================================================
// This mounts the default drives
//
// The config file is really primitive, and there is little error checking, so
// please be sure to follow the examples EXACTLY as specified.  Don't insert
// extra whitespace, etc.
//
//    # comments
//    x:filename.ext
//    xR:filename.ext
//
// Where 'r' is a digit from 0 to 3, R (if present) indicates read-only.
//
// Example:
//
//    0:SD_BOOT.DSK
//    1:CT_UTILS.DSK
//    2R:DANGER.DSK
//    3:PLAY.DSK

void Disks::mountDefaults(void)
{
        Serial.print("Reading configuration file ");
        Serial.println(CONFIG_FILE);

        // Now open and read the configuration file to see which drives
        // we should mount by default
        
        if (SD.exists(CONFIG_FILE))
        {
                file = SD.open(CONFIG_FILE, FILE_READ);
                if (!file)
                {
                        Serial.println("failed to open config file");
                        return;
                }

                // This is a crude little state machine that processes each
                // character from the config file.
                
                state = FIRST_CHAR;
                while (file.available())
                {
                        char token = file.read();   // get one character from file
#if 0
                        Serial.print("Got char '");
                        Serial.print(token);
                        Serial.print("' state ");
                        Serial.println(state);
#endif

                        switch (state)
                        {
                                case FIRST_CHAR:    // get first character of line
                                        if (token == '#')    // comment
                                        {
                                                state = WAIT_EOL;   // wait for end of line
                                        }
                                        else if (token >= '0' && token <= '3')  // drive
                                        {
                                                drive = token - '0';  // compute drive
                                                readOnly = false;
                                                state = AFTER_DRIVE;
                                        }
                                        break;
                                               
                                case WAIT_EOL:
                                        // In this state, keep reading characters
                                        // until an EOL is found.
                                        
                                        if (token == '\n')
                                                state = FIRST_CHAR;
                                        break;
           
                                case AFTER_DRIVE:    // should be either R or :
                                        if (token == ':')
                                        {
                                                state = FILENAME;
                                                fnptr = filename;
                                        }
                                        else if (token == 'R' || token == 'r')
                                        {
                                                readOnly = true;
                                        }
                                        break;
                                
                                case FILENAME:
                                        if (token == '\n')
                                        {
                                                state = FIRST_CHAR;
                                                *fnptr = '\0';    // terminate the filename
                                                mount(drive, filename, readOnly);
                                        }
                                        else if (token > ' ' && token < '~')
                                        {
                                                *fnptr++ = token;
                                        }
                                        break;
                        }
                }
                file.close();
        }
}




//=============================================================================
// This closes any open files.  This is kind of an emergency sort of function
// used to close open files in case another piece of code needs to open a file.

void Disks::closeAll(void)
{
        for (int d = 0; d < MAX_DISKS; d++)
        {
                if (disks[d]->isOpen())
                {
                        disks[d]->close();
                }
        }
}





//=============================================================================
// This is called to mount a disk image to one of the drives.  
// Returns false on error

bool Disks::mount(byte drive, char *filename, bool readOnly)
{
        bool ret = false;    // assume no error
        
        Serial.print("Got mount request for drive ");
        Serial.print(drive);
        Serial.print(": \"");
        Serial.print(filename);
        Serial.print("\"");
        if (readOnly)
                Serial.print(" - read only");
        Serial.println("");
        
        disks[drive]->mount(filename, readOnly);
        if (disks[drive]->isGood())
        {
                ret = true;
                Serial.println(" - SUCCESS!");
        }
        else
        {
                setError(disks[drive]->getError());    // move their error into our error code
                Serial.print(" - FAILED!  Error code ");
                Serial.println(errorCode);
        }
        
        return ret;
}




//=============================================================================
// Unmount just one drive, the number being passed in.  Returns true on error
// false if not.

bool Disks::unmount(byte drive)
{
        bool ret = false;    // assume no error
        
        disks[drive]->unmount();
        
        return ret;
}




//=============================================================================
// Read a sector.  On entry this is given the drive (0-3), the offset from the
// start of the file, and a pointer to a buffer large enought to receive the
// data.  Returns true if no error, false if error

bool Disks::read(byte drive, unsigned long offset, byte *buf)
{
        bool ret = false;
        
        // Is the drive even mounted?
        
        if (disks[drive]->isMounted())
        {
                if (disks[drive]->read(offset, buf))
                {
                        ret = true;
                        errorCode = ERR_NONE;
                }
                else    // error
                {
                        Serial.println("**** read error ****");
                        Serial.flush();
                        errorCode = disks[drive]->getError();
                }
        }
        else
        {
                errorCode = ERR_NOT_MOUNTED;
        }

        return ret;
}




//=============================================================================
// Write a sector.  On entry this is given the drive (0-3), the offset from the
// start of the file, and a pointer to the buffer with the data.  Returns true
// if successful, false if not.

bool Disks::write(byte drive, unsigned long offset, byte *buf)
{
        bool ret = false;
        
        // Is the drive even mounted?
        
        if (disks[drive]->isMounted())
        {
                if (disks[drive]->write(offset, buf))
                {
                        ret = true;
                        errorCode = ERR_NONE;
                }
                else    // error
                {
                        Serial.println("**** write error ****");
                        errorCode = disks[drive]->getError();
                }
        }
        else
        {
                errorCode = ERR_NOT_MOUNTED;
        }

        return ret;
}



//=============================================================================
// Returns the status of a particular drive.

byte Disks::getStatus(byte drive)
{
        return disks[drive]->getStatus();
}



//=============================================================================
// This formats a disk.  It will create the specified number of tracks with the
// specified number of sectors, filling each with the specified fill value.
// Returns true if successful, false if error.
//
// NOTE: The method used to format a drive has changed; now the file is
//       opened and each sector is written.  This should probably be deleted
//       but figured it might come in handy in the future.

#if 0
bool Disks::format(char *filename, int tracks, int sectorsPerTrack, byte fillPattern)
{
        bool ret = true;
        byte buffer[SECTOR_SIZE];

        unsigned int sectors = tracks * sectorsPerTrack;

        // fill sector with fill pattern

        for (int i = 0; i < SECTOR_SIZE; i++)
        {
                buffer[i] = fillPattern;
        }

        // open file for writing

        // write the sectors

        while (sectors-- && ret == true)
        {
        }
        
        return ret;
}
#endif

                

