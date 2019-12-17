# ulisp-esp
A version of the Lisp programming language for ESP32-based boards, supporting 4MB PSRAM.

For more information see: http://www.ulisp.com/show?21T5

Thanks to David for writing and supporting this fine version of lisp.


## General remarks

I decided to split the documentation to make it easier to find the informations you need.
Please look at the documentation folder to get more informations.
Currently (April 2019) there are the following descriptions:
* *'ESP-WROVER-KIT setup.md'*
  describes the steps needed to get the ESP-WROVER-KIT with TFT and PS/2 keyboard
  support up and running. A small hardware modification is needed to be able using
  the TFT and the SD-Card in parallel.

* *'reference-sleep-functions.md'*
  a description of the added light- and deepsleep uLisp functions.
  You can enable wakeup resources as you need for your project.
  
* *'reference-tft-functions.md'* -- a description of uLisp functions depending the tft:
  * set foreground and background text colors
  * plot a pixel
  * get a pixel's color
  * scroll the display
  ...

* *'reference-sd-card-functions.md'* -- uLisp functions depending on sd-card usage:
  * (ls)    -- list directory (and all subdirectories)
  * (mkdir) -- create a new directory
  * (rmdir) -- remove an empty directory
  * (rm)    -- remove a file
  * functions added to the Lisp-Library (available with uLisp's (require) command): (load), (save), (cat)



## Changelogs

### Changelog ulisp 3.0a-esp (Kaef)

* 2019-12.16: testing support for FabGl (VGA output & PS/2 keyboard library)
  * not tested in combination with LARGE_WORKSPACE yet
  * sdcard-support not tested
  * backspace did not work in uLisp repl (but this should be possible writing a new repl in uLisp)
  * escape sequences for terminal modifications not tested (ie. setting text and background color, moving cursor etc,)
* 2019-12-13: dropped ESP8266 support, because this makes the code complexer and I did not add any functions for the esp8266 
              Please use David's uLisp version for ESP8266.
* 2019-12-12: updated to David's uLisp-3.0a


### Changelog uLisp 2.9-esp (Kaef)

* 2019-12-11: Bugfix: functions returning an error if argument is zero: (scroll), (setCursor), (plot), (setTextColor), (readPixel)
* 2019-10-09: all changes made by David in uLisp-2.9-esp applied


### Changelog uLisp 2.8-esp (Kaef)

* 2019-12-12: Bugfix: functions returning an error if argument is zero: (scroll), (setCursor), (plot), (setTextColor), (readPixel)
* 2019-08: all changes made by David in uLisp-2.8-esp applied


### Changelog uLisp 2.7-esp (Kaef)

* 2019-06-24:
  * update to uLisp 2.7c, all changes made by David applied
  * from uLisp forum, 'How to pprint all globals': correction pprinting vars applied (values are quoted)
* 2019-05-22: update to uLisp 2.7, all changes made by David applied to this fork of uLisp


### Changelog uLisp 2.6-esp (Kaef)

* 2019-04-11:
  * update to uLisp 2.6, all changes made by David applied (all functions added to uLisp-2.5c (Kaef) are still available)
  * small change in I2Cstart(), I2Crestart() to better support i2c multi-master setups (untested)
  

### Changelog uLisp 2.5c-esp (Kaef)

* 2019-04-04:
  * added (readPixel x y)
* 2019-04-03:
  * added (rm filename)
  * added (rmdir filename)
  * added (mkdir filename)
  
* 2019-04-02: ESP\_WROVER\_KIT
  * added (setCursor x y)
  * added (plot x y [color])
  * added (setTextColor foregroundColor565 backgroundColor565)
  
* 2019-03-29: ESP\_WROVER\_KIT
  * expressions can be entered with line-breaks (CR starts a new line)
  * auto-ident (every parenthesis adds two spaces at beginning of line)
  * drop support for WROVER\_KIT\_LCD\_KAEF library, only TFT\_eSPI library supported (ILI9341, WROVER\_KIT\_LCD V.4.1, library must be reconfigured to support other display driver chips)
  * use ESC-key during input to throw away current line (same as Ctrl-C in unix shell)
  * added command (scroll lines [bg-color]), see 'reference-tft-functions.md'
  * added uLisp function (color565 r g b) to lispLibrary
  
* 2019-03-22:
  * floating point fix, to test enter 70.0 -- if 70.0 given back, fix is applied
  * [ESP\_WROVER\_KIT]: fix a bug with parenthesis highlighting when entering a string
  * changed I2C Pins from (scl, sda) (22, 21) (ESP32 WROVER defaults) to (12, 4) because default pins are used by the tft
    * with changed pins to (16, 17) i2c interface doesn't work -- why? => GPIO 16, 17 are used for Clk, CS from PSRAM chip!
    * using i2c-pins (22, 21): i2c interface is working, but display is freezing when using i2c interface (and it doesn't recover)...
    * use (12, 4), but this may interfere with the sd-card if used in 2- or 4-bit-mode, **seems to work, need more tests to proof**


### Changelog uLisp 2.5b-esp (Kaef):

* 2019-03-21: 
  * changed grafic library to TFT_eSPI (WROVER\_KIT\_LCD\_KAEF library can be used if wanted) - I think the display is working much better now
  * enabled command (enable-gpio-wakeup), see 'reference-sleep-functions.md' (esp32 board support version 1.0.1 or higher needed!)

* 2019-03-20: PS/2 keyboard support (including parenthesis highlighting)

  * I think the keyboard is working smoothly now, only US keyboard layout is supported
  * It is possible to use the serial connection in parallel with the PS/2 keyboard.

* 2019-03-17: added WROVER\_KIT\_LCD\_KAEF and PS2Kbd libraries
* 2019-03-16: added support for ESP-WROVER-KIT

  * OBSOLETE (use TFT_eSPI library now)

    * TFT support through WROVER\_KIT\_LCD library (based on scrolltest example)

    * It's just a proof of concept now, I'm not satisfied with the solution because the example seems to be written for writing whole lines of text, not single characters. I will investigate after keyboard support is included. Anyway, the display is working... ;-)
    * I had to patch the WROVER\_KIT\_LCD library to make the lcd and the sd card work in parallel. I renamed the library to WROVER\_KIT\_LCD\_KAEF and included it in the uLisp directory. Please copy the lib to your sketchbook/libraries (Arduino/libraries on windows) folder!
  * SD card support included
  * Keyboard support with PS2Kbd library from https://github.com/michalhol/ps2kbdlib
  Most USB-keyboards supports the ps/2 mode (you can buy cheap usb -> ps/2 adapters for that).
  I connect my USB keyboard to the WROVER\_KIT\_LCD:
  ```
  /---------------\ 
  |   USB Keyb.   | (Keyboard connector (keyboard side) shown)
  |___ ___ ___ ___|
   GND CLK DTA +5V
  ```
  * I cut a usb cable with a USB-A female connector and connect it to the board.
    Update: using a small pcb with a USB female connector now (2019-04-02)
  * use two 10k resistors from CLK to 5V and from DTA to 5V
  * connect CLK to GPIO27
  * connect DTA to GPIO26
  * try the simpleRead example from PS2Kbd library (the keyboard should work)

* 2019-02: LARGE\_WORKSPACE flag set:
  * extend SYMBOLTABLESIZE to 32kBytes, extend the number of long symbols (no restrictions), UNTESTED! (wrote a #warning message in the sources)

* 2019-02: added (ls) command to list the files from the sd-card (see 'reference-sd-card-functions.md')
* 2019-01-27: moved ulisp-esp V. 2.5 to subdirectory ulisp-2.5-esp, renamed ulisp-esp.ino to ulisp-2.5-esp.ino
* 2019-01-27: adding new ulisp-features done by David becomes a heavy task I decided to work another way: I'll use David's 'official' ulisp version and redo my esp32 patches (PSRAM support, esp32-deepsleep, sdcard-pin-configuration)
* 2019-01-27: this version of ulisp only supports esp32 boards, esp8266 is not tested
* 2018-12-03: added changes made in ulisp-2.5-esp (technoblogy:master)
* 2018-10-18: This is a fork of ulisp-esp (currently version 2.4, will be updated) where I added some features for esp32.

### Changelog ulisp-2.5-esp
* merged changes manually from technoblogy:master repository
  (sorry, I don't find out how to do this automatically, but it seems easy enough to do)

### Changelog ulisp-2.4-esp
* 2018-12-03: latest ulisp-2.4 version (ea23ecc3e795f2b0de135c5cc10c3849f755eeb0) -- next commit will include changes made by David in ulisp-2.5-esp version
* 2018-11-26: merged branch dev-deepsleep to master -- removed dev-deepsleep so every deepsleep-addition is in master branch now
* usage of the 4 MByte PSRAM on ESP-WROVER-32 modules
  (other boards with or without PSRAM (only 4MB supported) should work too)
* sd-card support: possibility to define sd-card pins (SPI pins used for the sd-card) in uLisp without changing arduino defaults
* new lisp function: (reset-reason)
* Deepsleep functions (see 'reference-sleep-functions.md')
* (sleep) uses lightsleep, therefore program will continue after sleep
* added (debug-flags NUM) to enable/disable debug output
  supported flags:
    0x0001: DEBUG\_SLEEP  -- debug information for sleep, lightsleep and deepsleep functions
    0x0002: DEBUG\_SDCARD -- debug information for sdcard (closing before deepsleep)
  

