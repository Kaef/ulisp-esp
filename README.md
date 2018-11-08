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
  ```
  (enable-timer-wakeup 10)
  ```
  will configure the timer-wakeup after 10 seconds. To enter deepsleep you need to call
  ```
  (deep-sleep-start)
  ```
* deep-sleep-start()
  Sets the system in deepsleep mode (see Espressif-documentation for details) with the
  previous set wakeup-mode(s). It is neccessary to set the wakeup-mode(s) before calling this function.
* isolate-gpio()
  To reduce the current consumption in sleep-modes you can isolate the gpios which have external
  pullup- or pulldown-resistors.
  TODO: I'm not sure wheather it's a good idea to isolate GPIOs which are configured for wakeup... *tbd*
  ```
  (isolate-gpio <GPIO_NUM>)
  ```
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
