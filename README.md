# ulisp-esp
A version of the Lisp programming language for ESP8266 and ESP32-based boards
For more information see:
http://www.ulisp.com/show?21T5


Kaef, 2018-10-18:
=================
This is a fork of ulisp-esp (Version 2.4) where I added some features for esp32.
For the detailed changelog see ulisp-esp.ino.

Brief changelog
* usage of the 4 MByte PSRAM on ESP-WROVER-32 modules (other boards may work too)
* sd-card support: possibility to define sd-card pins (SPI pins used for sd-card)
* new lisp function: reset-reason
* sleep uses esp32 deepsleep (will change soon)
* TODO: new lisp function: deepsleep (configurable timed- and external wakeup)
