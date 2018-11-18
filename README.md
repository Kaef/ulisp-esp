# ulisp-esp
A version of the Lisp programming language for ESP8266 and ESP32-based boards
For more information see:
http://www.ulisp.com/show?21T5


## Kaef, 2018-10-18:

This is a fork of ulisp-esp (Version 2.4) where I added some features for esp32.

### Brief changelog
* usage of the 4 MByte PSRAM on ESP-WROVER-32 modules
  (other boards with or without PSRAM (only 4MB supported now, but this is easy to extend) should work too)
* sd-card support: possibility to define sd-card pins (SPI pins used for sd-card)
* new lisp function: reset-reason
* sleep uses esp32 deepsleep
* Deepsleep functions (see below)
* (sleep) uses lightsleep, therefore program will continue after sleep (this is the expected behaviour for sleep;
  restarting the system (as deepsleep does) is very unusual).

### Deepsleep functions
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

* isolate-gpio(<GPIO_NUM>)

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

  
* enable-ext0-wakeup(<GPIO_NUM>, <LEVEL>)

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
       
* enable-gpio-wakeup(<GPIO_NUM>, <LEVEL>) (light sleep only)

  Configure any gpio to wakeup the system after lightsleep.

  **WARNING: Wakeup does not work, I don't know the reason now. I will investigate in this topic.**
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
