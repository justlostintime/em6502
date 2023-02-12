//=============================================================================
// This is the code for the RTC class, which provides an interface to a
// real time clock.  While this module is fairly generic, the code here is
// written for the DS3231 chip.
//
// While reseraching this progress, I found this page which was extremely
// helpful:
//
// http://tronixstuff.com/2014/12/01/tutorial-using-ds1307-and-ds3231-real-time-clock-modules-with-arduino/
//
// MANY THANKS to tronixstuff for having a great example on how to use the
// DS33231 chip.
//

#include "RTC.h"
#include <Wire.h>

// Define this to force errors to test the application code

#undef FORCE_ERRORS


// This is the I2C address of the DS3231 RTC.

#define DS3231_I2C_ADDRESS  0x68


//=============================================================================
// The initializer doesn't do much.  The logic needs to be fixed so it
// properly sets present to false if the RTC is not installed.

RTC::RTC(void)
{
        Wire.begin();
        Serial.println("RTC with DS3231 installed");
        present = true;
}



//=============================================================================
// And the destructor does even less!

RTC::~RTC(void)
{
}



//=============================================================================
// Given a pointer to a data area of 8 bytes, this gets the current time and
// date from the RTC and puts it into the buffer in this order:
//
//    MDYYHMSd

bool RTC::getClock(byte *ptr)
{
        bool ret = false;
        
#ifndef FORCE_ERRORS
        if (present)
        {
                // go get the time from the hardware.  The low level function takes
                // parameters in a different order than our buffer needs them, so
                // re-order the data as needed.
        
                readDS3231time(&ptr[6], &ptr[5], &ptr[4], &ptr[7], &ptr[1], &ptr[0], &ptr[3]);
                ret = true;
        }
#endif

        return ret;
}



//=============================================================================
// Given a pointer to an 8 character buffer, set the RTC to the specified time
// and date.  The data in the buffer is in this order:
//
//    MDYYHMSd

bool RTC::setClock(byte *ptr)
{
        bool ret = false;
        
#ifndef FORCE_ERRORS
        if (present)
        {
                byte *orig = ptr;
                Serial.print("set time: ");
                for (int i = 0; i < 8; i++)
                {
                        Serial.print(*ptr++, DEC);
                        Serial.print(" ");
                }
                Serial.println("");
                ptr = orig;

                setRtcTime(ptr[6], ptr[5], ptr[4], ptr[7], ptr[1], ptr[0], ptr[3]);
                ret = true;
        }
#endif

        return ret;
}



//=============================================================================
// The low-level routine to set the time and date.
// Directly from tronixstuff.  Thank them!

void RTC::setRtcTime(byte second, byte minute, byte hour, byte dayOfWeek,
        byte dayOfMonth, byte month, byte year)
{
        // sets time and date data to DS3231
        Wire.beginTransmission(DS3231_I2C_ADDRESS);
        Wire.write(0); // set next input to start at the seconds register
        Wire.write(decToBcd(second)); // set seconds
        Wire.write(decToBcd(minute)); // set minutes
        Wire.write(decToBcd(hour)); // set hours
        Wire.write(decToBcd(dayOfWeek)); // set day of week (1=Sunday, 7=Saturday)
        Wire.write(decToBcd(dayOfMonth)); // set date (1 to 31)
        Wire.write(decToBcd(month)); // set month
        Wire.write(decToBcd(year)); // set year (0 to 99)
        Wire.endTransmission();
}



//=============================================================================
// Low level function to get the time and date from the RTC.
// Directly from tronixstuff.  Thank them!

void RTC::readDS3231time(byte *second, byte *minute, byte *hour, byte *dayOfWeek,
        byte *dayOfMonth, byte *month, byte *year)
{
        Wire.beginTransmission(DS3231_I2C_ADDRESS);
        Wire.write(0); // set DS3231 register pointer to 00h
        Wire.endTransmission();
        Wire.requestFrom(DS3231_I2C_ADDRESS, 7);
        // request seven bytes of data from DS3231 starting from register 00h
        *second = bcdToDec(Wire.read() & 0x7f);
        *minute = bcdToDec(Wire.read());
        *hour = bcdToDec(Wire.read() & 0x3f);
        *dayOfWeek = bcdToDec(Wire.read());
        *dayOfMonth = bcdToDec(Wire.read());
        *month = bcdToDec(Wire.read());
        *year = bcdToDec(Wire.read());
}


// Convert normal decimal numbers to binary coded decimal
byte RTC::decToBcd(byte val)
{
        return (val /10 * 16) + (val % 10);
}


// Convert binary coded decimal to normal decimal numbers
byte RTC::bcdToDec(byte val)
{
        return  (val / 16 * 10) + (val % 16);
}



              
