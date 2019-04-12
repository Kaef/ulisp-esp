# ESP-WROVER-KIT-LCD GPIO USAGE
     Kaef, 2019-03-22


## Most GPIOs are already used by the Espressif layout:

* TFT   - 18, 19, 21, 22, 23, 25, 5
* SD    - (12), 13, 14, 15, 2, (4)
* Q     - 32, 33
* PSRAM - 16, 17

=> usable GPIOs: 26, 27, (12), (4), (0), 34(I), 35(I)


## Application usage (uLisp-esp):

* Keyboard - 26 (DTA), 27 (CLK)
* I2C      - (12), (4)  ?? (seems to work, may conflict with SD)

=> using a bluetooth keyboard would free GPIO 26, 27


## Unused GPIOs:

* INPUT   - 34, 35 (unused at time of writing)
* OUTPUT  - (0)


## Remark:

* not sure about GPIOs in parenthesis!
