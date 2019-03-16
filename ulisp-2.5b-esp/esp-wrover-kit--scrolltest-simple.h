#ifndef ESP_WROVER_KIT__SCROLLTEST_SIMPLE_H
#define ESP_WROVER_KIT__SCROLLTEST_SIMPLE_H
/*
 * Kaef: Original comment from sketch scrolltest (mostly obsolete):
    This sketch demonstrates how to use the hardware scrolling on a portion of the screen.
    It performs a basic WiFi scan to generate some terminal-like animation.
    Courtesy of tobozo (Dec.2018)

 * Kaef, 2019-03:
    I extracted some core routines from the esp-wrover-kit-lcd 'scrolltest' example
      to use it with uLisp.
    Anyway I did not fully understand how the display controller is working, further
      investigation is needed...
    Scrolling text is difficult, but it seems to work...
    I had to do a small change to the WROVER_KIT_LCD library to make 
      tft and sdcard working together (use a different spi object for tft)
    I think this whole thing should be rewritten to get something like a 'canvas'
      to write text to or draw graphics on.
      It should include auto-scrolling when adding text at the bottom of the display.
      A scroll (lines or textlines) function should be included too.
*/

#include <Adafruit_GFX.h>    // Core graphics library
#include "WROVER_KIT_LCD_KAEF.h"
WROVER_KIT_LCD_KAEF tft(HSPI);

// Prototypes:
void scrollText(const char* str);

/*
 * setup the WROVER_KIT_LCD_KAEF library...
 */
void setup_tft() {
    tft.begin();
    tft.setRotation( 0 ); // portrait mode 0 is required
    tft.setTextColor(WROVER_YELLOW);
    tft.setupScrollArea(0, 0);
    // clear it
    tft.fillRect(0, 0, tft.width(), tft.height(), WROVER_BLACK);
    tft.setCursor(0, 0);
}

/*
 * function used by uLisp to draw a char to the display
 */
void displayPrintChar (const char c) {
    char buf[2];
    memset(buf, 0, sizeof(buf));
    buf[0] = c;
    scrollText(buf);
    yield();
}

/* -----------------------------------------------------------------------
 * some helpers... (based on the WROVER_KIT_LCD scrolltest example):
 */
void doScroll(int scrollPosY, int lines, int wait) {
    int yStart = tft.getCursorY();
    for (int i = 0; i < lines; i++) {
        yStart++;
        if (yStart == tft.height()) yStart = 0;
        tft.scrollTo(yStart);
        delay(wait);
    }
}

void scrollText(const char* str) {
    static int scrollPosX, scrollPosY = -1;
    uint16_t w_tmp, h_tmp, h_dummy;
    int16_t  x1_tmp, y1_tmp;

    scrollPosX = tft.getCursorX();
    scrollPosY = tft.getCursorY();
    if (scrollPosY >= tft.height()) {
        scrollPosY = scrollPosY % tft.height();
    }
    // calculate text height:
    tft.getTextBounds(" ", scrollPosX, scrollPosY, &x1_tmp, &y1_tmp, &w_tmp, &h_tmp);
    // calculate other parameters:
    tft.getTextBounds(str, scrollPosX, scrollPosY, &x1_tmp, &y1_tmp, &w_tmp, &h_dummy);
    if(h_dummy > h_tmp)
        h_tmp = h_dummy;
    if (scrollPosX == 0) {
        tft.fillRect(0, scrollPosY, tft.width(), h_tmp, WROVER_BLACK);
    } else { // fill the horizontal gap
        tft.fillRect(scrollPosX, scrollPosY, tft.width() - w_tmp, h_tmp, WROVER_BLACK);
    }
    tft.setCursor(scrollPosX, scrollPosY);
    doScroll(scrollPosY, h_tmp, 0); // Scroll lines
    tft.print(str);
    scrollPosY = tft.getCursorY();
}

#endif // ESP_WROVER_KIT__SCROLLTEST_SIMPLE_H
