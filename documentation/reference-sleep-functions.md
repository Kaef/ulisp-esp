# ulisp-esp
A version of the Lisp programming language for ESP32-based boards, supporting 4MB PSRAM.

For more information see: http://www.ulisp.com/show?21T5

Thanks to David for writing and supporting this fine version of lisp.


This file describes the added sleep functions for ESP32 modules.
uLisp now supports lightsleep and deepsleep modes, bringing down current used by ESP32 modules
to 5 microamps (deepsleep mode)!
You can define different wakeup sources: timer-wakeup, ext0-wakeup (GPIO wakeup) and more.
If the system is alive again you can ask for the wakeup-cause.
When  using uLisp autorun feature you can restart your application after wakeup and decide what
to do next depending on the wakeup-cause. This is very useful for sensor applications
(ie. reading some sensors every N seconds, posting its value to the internet and go to sleep again).
Another usecase would be to detect when a door is opened (change on GPIO pin) and do whatever you want
when this event occures.


## ESP32 sleep functions

### Sleep functions (developed in brach 'dev-deepsleep', merged back to master (2018-11-26))

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

At time of writing (April 2019) uLisp now supports the following sleep and wakeup functions:

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

  **WARNING: arduino-esp32-idf V. 1.0.1 is needed!**
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
