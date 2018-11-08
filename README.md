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
* deep-sleep-start()
* isolate-gpio()
* enable-ext0-wakeup(port, level)
* get-sleep-wakeup-cause()
* disable-wakeup-source(source)

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
