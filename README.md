# ulisp-esp
A version of the Lisp programming language for ESP8266 and ESP32-based boards
For more information see:
http://www.ulisp.com/show?21T5


## Kaef, 2018-10-18:

This is a fork of ulisp-esp (Version 2.4) where I added some features for esp32.
For the detailed changelog see ulisp-esp.ino.

### Brief changelog
* usage of the 4 MByte PSRAM on ESP-WROVER-32 modules (other boards may work too)
* sd-card support: possibility to define sd-card pins (SPI pins used for sd-card)
* new lisp function: reset-reason
* sleep uses esp32 deepsleep (will change soon)
* TODO: new lisp function: deepsleep (configurable timed- and external wakeup)

### Deepsleep functions
Because ESP32 has many wakeup possibilities which can be combined I decided to
add separate functions to activate the wakeup-sources, rather similar to the
Espressif idf-framework (but not complete).
At the beginning I will support the following functions:
* enable-timer-wakeup(secs)

  secs can be a positive integer or float value
  ```
  (enable-timer-wakeup 10)
  (enable-timer-wakeup 20.5)
  ```
  will configure the timer-wakeup 10 (20.5) seconds after entering sleep-mode. To enter deepsleep-mode you need to call
  ```
  (deep-sleep-start)
  ```
  Returns wakeup-time or throughs an error (non integer or negativ parameter).
* deep-sleep-start()

  Sets the system to deepsleep mode (see Espressif-documentation for details) with the
  previous set wakeup-mode(s). It is important to set at least one wakeup-mode before calling this function,
  otherwise the system will sleep **very** long (don't know if it wakes up sometime...).

  If sdcardsupport is defined the function needs ~200ms to close open files (caused by a delay() to enable flushing buffers).
  
  The system restarts after wakeup, so you need to check (reset-reason) to decide if the system
  restarts due a power cycle or due sleep-wakeup. For detailed information of the wakeup-cause see
  (get-sleep-wakeup-cause) below.
  The system-restart takes some seconds and ulisp boots completely (all informations from before deep-sleeping
  will be lost!). To continue with your ulisp program you need to use (save-image 'function) or
  the LispLibrary-function to load and start your code.
* isolate-gpio()

  To reduce the current consumption in sleep-modes you can isolate the gpios which have external
  pullup- or pulldown-resistors.
  TODO: I'm not sure wheather it's a good idea to isolate GPIOs which are configured for wakeup... **tbd**
  ```
  (isolate-gpio <GPIO_NUM>)
  ```
  GPIOs allowed to isolate are: 0..5, 12-33
  (because 6..11 are used for SPI-flash, 34..39 has no software pullup or pulldown)
  see Espressif documentation of rtc_gpio_isolate() for details.
  Returns GPIO-num on success or nil if GPIO_NUM can't be isolated.
* enable-ext0-wakeup(port, level)

  tbd.
* get-sleep-wakeup-cause()

  tbd.
* disable-wakeup-source(source)

  tbd.

There's no support (at least in the first step) for:
* gpio-wakeup (light sleep only)
* light-sleep-start()
* enable-uart-wakeup(num_chars)
* enable-ext1-wakeup
* get-ext1-wakeup-status
* enable-touchpad-wakeup
* get-touchpad-wakeup-status
* sleep-pd-config(...)
If you need some of these functions you can add it by yourself or you
can send me a feature-request (but I do not guaranty to include it soon ;-) ).
