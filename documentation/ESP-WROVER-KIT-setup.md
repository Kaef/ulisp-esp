# Using ESP-WROVER-KIT

* to use the ESP-WROVER-KIT with uLisp just flash the uLisp version to the board
* to be able to use the TFT and the SD-Card in parallel a small change to the board is needed:
  * unsolder the 0 ohm resistor R167 (position is next to the sd-card slot (WROVER side)
    this is the sd-card detect switch which can't be used anymore (but it's not neccessary)
    it is descriped on the Espressif ESP-WROVER-KIT documentation page
  * make sure that '#define ESP_WROVER_KIT' is enabled in uLisp-x.y-esp.ino!
* if you want to use a ps/2 keyboard with uLisp you need to add a keyboard connector to
  GPIO 26 (DTA) and 27 (CLK).
  * If you want to use a USB keyboard (in PS/2 compatibily mode)
    add two 10k pullup resistors to D+ and D- (see picture 'ESP32_LM_Keyboard_connection.jpg').
  * make sure that '#define PS2_KEYBOARD' is enabled in uLisp-x.y-esp.ino (around line 37)!
  
## Arduino Installation (TFT_eSPI, support for display & PS/2 keyboard) (uLisp 2.5c, 2019-03-22)

I copied the used libraries to the ulisp-esp/libraries folder. Copy them to your arduino/sketchbook/libraries folder
before compiling uLisp!

A big thanks to everyone who wrote the below libraries. 
* TFT_eSPI: (works with WROVER-KIT-LCD V.4.1 only (ILI9341 chip only unless re-configured)
  * use arduino library manager to install TFT_eSPI library
  * copy ulisp-esp/libraries/TFT_eSPI/User_setup.h to arduino-libraries-folder/TFT_eSPI/User_setup.h (overwrite existing file)
* PS2Kbd:
  * copy ulisp-esp/libraries/PS2Kbd folder to arduino-libraries-folder or download PS2Kbd library from github

Please respect the licences of the used libraries.

