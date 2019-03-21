# ulisp-esp
A version of the Lisp programming language for ESP32-based boards
For more information see:
http://www.ulisp.com/show?21T5


## General remarks and changelog:
* 2019-03-21: changed grafic library to TFT_eSPI (WROVER\_KIT\_LCD\_KAEF library can be used if wanted)

### Installation
A big thanks to everyone who wrote the below libraries. Please respect the licences of the used libraries.
* TFT_eSPI: (works with WROVER-KIT-LCD V.4.1 only (ILI9341 chip only unless re-configured)
  use arduino library manager to install TFT_eSPI library
  copy ulisp-esp/libraries/TFT_eSPI/User_setup.h to arduino-libraries-folder/TFT_eSPI/User_setup.h (overwrite existing file)
OR
* WROVER\_KIT\_LCD\_KAEF: (should work with ILI9341 (tested) and ST7789V (untested))
  copy ulisp-esp/libraries/WROVER\_KIT\_LCD\_KAEF folder to arduino-libraries-folder (normally under sketchbook/libraries)
  use arduino library manager to install Adafruit\_GFX library
  comment out the line "#define USE\_TFT\_eSPI\_LIB in file esp-wrover-kit-display.h (around line 11)
* PS2Kbd:
  copy ulisp-esp/libraries/PS2Kbd folder to arduino-libraries-folder or download PS2Kbd library from github

### Changelog:
* 2019-03-20: PS/2 keyboard support (including parenthesis highlighting)

  It seems keyboard is working smoothly, only US keyboard layout is supported now.
  It is possible to use the serial connection in parallel with the PS/2 keyboard.

* 2019-03-17: added WROVER\_KIT\_LCD\_KAEF and PS2Kbd libraries
* 2019-03-16: added support for ESP-WROVER-KIT

  * TFT support through WROVER\_KIT\_LCD library (based on scrolltest example)
  It's just a proof of concept now, I'm not satisfied with the solution because the example seems to be written for writing whole lines of text, not single characters. I will investigate after keyboard support is included. Anyway, the display is working... ;-)
  I had to patch the WROVER\_KIT\_LCD library to make the lcd and the sd card work in parallel. I renamed the library to WROVER\_KIT\_LCD\_KAEF and included it in the uLisp directory. Please copy the lib to your sketchbook/libraries (Arduino/libraries on windows) folder!
  * SD card support included
  * Keyboard support with PS2Kbd library from https://github.com/michalhol/ps2kbdlib
  Most USB-keyboards supports the ps/2 mode (you can buy cheap usb -> ps/2 adapters for that).
  I connect my USB keyboard to the WROVER\_KIT\_LCD:
  ```
  /---------------\ 
  |   USB Keyb.   |
  |___ ___ ___ ___|
  .GND CLK DTA +5V
  ```
  I cut a usb cable with a USB-A female connector and connect it to the board.
  use a 4k7 resistor from CLK to 5V and from DTA to 5V
  connect CLK to GPIO27
  connect DTA to GPIO26
  try the simpleRead example from PS2Kbd library (the keyboard should work).

* 2019-02: extend SYMBOLTABLESIZE to 32kBytes, extend the number of long symbols (no restrictions), UNTESTED! (wrote a #warning message in the sources)

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
  (other boards with or without PSRAM (only 4MB supported now, but this is easy to extend) should work too)
* sd-card support: possibility to define sd-card pins (SPI pins used for sd-card)
* new lisp function: reset-reason
* Deepsleep functions (see below)
* (sleep) uses lightsleep, therefore program will continue after sleep (this is the expected behaviour for sleep;
  restarting the system (as deepsleep does) is very unusual).
* added (debug-flags NUM) to enable/disable debug output
  supported flags:
    0x0001: DEBUG_SLEEP  -- debug information for sleep, lightsleep and deepsleep functions
    0x0002: DEBUG_SDCARD -- debug information for sdcard (closing before deepsleep)
  tbd.


### Added functions
* sleep functions (deepsleep and lightsleep), see next chapter
* sd-card functions
  * (ls) -- list directory
  this function has just side effects, printing the file-list at screen
  a later version may return a list of files (or list of (list of file, size))
  
### Deepsleep functions (developed in brach 'dev-deepsleep', merged back to master (2018-11-26))
Because ESP32 has many wakeup possibilities which can be combined I decided to
add separate functions to activate the wakeup-sources, similar to the
Espressif idf-framework (not all functions supported).
  ```
  (enable-timer-wakeup 60)
  (enable-ext0-wakeup 0 0)
  (deepsleep-start)
  ```
  This sequence sets esp32 in deepsleep-mode. It will wakeup after 60 seconds or if GPIO0 is pulled to GND.
  You can use (get-reset-reason) and (get-sleep-wakeup-cause) to find out what happend.

At the beginning I will support the following functions:

* enable-timer-wakeup(secs)

  secs can be a positive integer or float value
  ```
  (enable-timer-wakeup 10)
  or: (enable-timer-wakeup 20.5)
  ```
  will configure the timer-wakeup 10 (20.5) seconds after entering sleep-mode. To enter deepsleep-mode you need to call
  (deep-sleep-start).
  Returns wakeup-time or throughs an error (non integer or negativ parameter).
  
* deep-sleep-start()

  Sets the system to deepsleep mode (see Espressif-documentation for details) with the
  previous set wakeup-mode(s). It is important to set at least one wakeup-mode before calling this function,
  otherwise the command abords with an error message ("Please configure wakeup-mode(s) first!").

  If sdcardsupport is defined, the function needs ~200ms to close open files (caused by a delay() to enable flushing buffers).
  
  The system restarts after wakeup, so you need to check (reset-reason) to decide if the system
  restarts due a power cycle or due sleep-wakeup. For detailed information of the wakeup-cause see
  (get-sleep-wakeup-cause) below.
  The system-restart takes some seconds and ulisp boots completely (all informations from before deep-sleeping
  will be lost!). To continue with your ulisp program you need to use (save-image 'function) or
  the LispLibrary-function to load and start your code.

* isolate-gpio(GPIO_NUM)

  To reduce the current consumption in sleep-modes you can isolate the gpios which have external
  pullup- or pulldown-resistors.
  I'm not sure wheather it's a good idea to isolate GPIOs which are configured for wakeup (but it seems to work correctly).
  ```
  (isolate-gpio 25)
  ```
  GPIOs allowed to isolate are: 0..5, 12-33
  (because 6..11 are used for SPI-flash, 34..39 has no software pullup or pulldown so there's no need to isolate)
  see Espressif documentation of rtc_gpio_isolate() for details.
  Returns GPIO-num on success or nil if GPIO_NUM can't be isolated.

  
* enable-ext0-wakeup(GPIO_NUM, LEVEL)

  Use a GPIO to wakeup the system. Allowed GPIOs are: 0, 2, 4, 12..15, 25..27, 32..39 (see Espressif documentation).
  LEVEL must be 0 or 1.
  ```
  (enable-ext0-wakeup 0 0)
  ```
  This enables a LOW signal on GPIO0 to wakeup the system. Call (deepsleep-start) to enter deepsleep mode.

  Returns: list of given arguments on success, error-message if it fails.

* get-sleep-wakeup-cause()

  Returns the sleep-wakup-cause, see Espressif-documentation for details.
  Here are some values I saw on my system: 0: no sleep wakeup (maybe normal boot), 1: ext0 sleep wakeup, 3: timer wakeup
       
* enable-gpio-wakeup(GPIO_NUM, LEVEL) (light sleep only)

  Configure any gpio (same GPIOs as for enable-ext0-wakeup possible) to wakeup the system after lightsleep.
  LEVEL must be 0 or 1.

  **WARNING: Wakeup does not work, because arduino-esp32-idf V. 1.0.0 is too old (esp_sleep_enable_gpio_wakeup() not supported).**
  If calling this function ulisp returns an error message (I will fix it as soon as a newer arduino-esp32-idf version is released.)
  ```
  (enable-gpio-wakeup 0 0)
  ```

* lightsleep-start()

  Sets the system to lightsleep mode. A wakeup must be enabled before calling this function, otherwise an error will be shown.

  In contrast to deepsleep lightsleep will **not reboot** the system, but continue with the next instruction.
  Therefore lightsleep works somewhat similar to delay, but some system components are powered off (see Espressif-documentation for details).
  
There's no support (at least in the first step) for:
* disable-wakeup-source(source)
* enable-uart-wakeup(num_chars)
* enable-ext1-wakeup
* get-ext1-wakeup-status
* enable-touchpad-wakeup
* get-touchpad-wakeup-status
* sleep-pd-config(...)
If you need some of these functions you can add it by yourself or you
can send me a feature-request (but I do not guaranty to include it soon ;-) ).
