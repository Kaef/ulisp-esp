#ifndef ESP_WROVER_KIT_DISPLAY__H
#define ESP_WROVER_KIT_DISPLAY__H
/*
    Kaef, 2019-03:

    This file capsulate the display routines. Just a few functions (located at
    the beginning of this file) are used by uLisp, others are helpers.
    This could also be written as a C++ class (and will be, maybe ;-) )

    I extracted some core routines from the esp-wrover-kit-lcd 'scrolltest' example
      to use it with uLisp (marked with ##SCROLLTEST##).
    Anyway I did not fully understand how the display controller is working, further
      investigation is needed...

    I had to do a small change to the WROVER_KIT_LCD library to make
      tft and sdcard working together (use a different spi object for tft)

    I think the display functions should be rewritten to get something like a 'canvas'
      to write text to or draw graphics on.
      It should include auto-scrolling when adding text at the bottom of the display.
      A scroll (lines or textlines) function should be included too.
*/

#include <Adafruit_GFX.h>    // Core graphics library
#include "WROVER_KIT_LCD_KAEF.h"
static WROVER_KIT_LCD_KAEF tft(HSPI);

static const uint16_t TEXT_COLOR    = WROVER_YELLOW;
static const uint16_t TEXT_BG_COLOR = WROVER_BLACK;

// Prototypes:
void scrollText(const char* str);


void setupWroverKit() {
    // TFT:
    tft.begin();
    tft.setRotation( 0 ); // portrait mode 0 is required
    tft.setTextColor(TEXT_COLOR);
    tft.setupScrollArea(0, 0);
    // clear it
    tft.fillRect(0, 0, tft.width(), tft.height(), TEXT_BG_COLOR);
    tft.setCursor(0, 0);
}

/*
    function used by uLisp to draw a char to the display
*/
void displayPrintChar (const char c) {
    char buf[2];
    memset(buf, 0, sizeof(buf));
    buf[0] = c;
    scrollText(buf);
    yield();
}

void displayPrintCharInverse(const char c) {
    tft.setTextColor(TEXT_BG_COLOR, TEXT_COLOR); // TODO: grab used colors
    displayPrintChar(' ');
    tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR); // TODO: grab used colors
}

void removeLastChar() {
    int16_t x = tft.getCursorX();
    int16_t y = tft.getCursorY();
    uint16_t w_tmp, h_tmp, h_dummy;
    int16_t  x1_tmp, y1_tmp;
    tft.getTextBounds("_", x, y, &x1_tmp, &y1_tmp, &w_tmp, &h_tmp);
    x -= w_tmp; if (x < 0) {
        x += tft.width();
        y -= h_tmp;
    }
    tft.setCursor(x, y);
    displayPrintChar(' ');
    tft.setCursor(x, y);
}

void displayPrintStringBack(char *s) {
    // Serial.print(__FUNCTION__); Serial.print(" '"); Serial.print(s); Serial.println(" '");
    // save current cursor position
    int x = tft.getCursorX();
    int y = tft.getCursorY();
    // calculate length of s
    uint16_t w_tmp, w_char, h_tmp, h_line;
    int16_t  x1_tmp, y1_tmp;
    int16_t textWidth = 0, textHeight = 0;
    for(int i=0; i < strlen(s); i++) {
        char sh[2]; sh[0] = s[i]; sh[1] = 0;
        tft.getTextBounds(sh, 0, 0, &x1_tmp, &y1_tmp, &w_tmp, &h_tmp);
        textWidth += w_tmp;
        if(h_tmp > textHeight) textHeight = h_tmp;
    }
    // calculate starting cursor position of s
    int xs = x - textWidth;
    int ys = y;
    while(xs < 0) {
        xs += tft.width();
        ys -= textHeight;
    }
    while(ys < 0) {
        ys += tft.height();
    }
    /* *
    Serial.print(x); Serial.print(" "); Serial.print(y); Serial.print(" "); 
    Serial.print(xs); Serial.print(" "); Serial.println(ys);
    Serial.print(textWidth); Serial.print(" "); Serial.println(textHeight);
    // */
    // setCursor to starting position of s
    tft.setCursor(xs, ys);
    // print s
    tft.print(s);
}

void displayPrintStringBackInverse(char *s) {
    tft.setTextColor(TEXT_BG_COLOR, TEXT_COLOR); // TODO: grab used colors
    displayPrintStringBack(s);
    tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR); // TODO: grab used colors
}

// show Cursor behind text already inserted
// (will not work if cursor is inside a text!)
void showCursor(bool show) {
    static bool cursorShown = false;
    if (show == cursorShown) return;

    cursorShown = show;
    if (show) {
        displayPrintCharInverse(' ');
    } else {
        removeLastChar();
    }
}

/*  -----------------------------------------------------------------------
    ##SCROLLTEST##
    some helpers... (based on the WROVER_KIT_LCD scrolltest example):
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
    if (h_dummy > h_tmp)
        h_tmp = h_dummy;
    if (scrollPosX >= tft.width()) {
        scrollPosX -= tft.width();
        scrollPosY += h_tmp;
    }
    if (scrollPosX == 0) {
        tft.fillRect(0, scrollPosY, tft.width(), h_tmp, TEXT_BG_COLOR);
    } else { // fill the horizontal gap
        tft.fillRect(scrollPosX, scrollPosY, tft.width() - w_tmp, h_tmp, TEXT_BG_COLOR);
    }
    tft.setCursor(scrollPosX, scrollPosY);
    doScroll(scrollPosY, h_tmp, 0); // Scroll lines
    tft.print(str);
}
//  -----------------------------------------------------------------------

#endif // ESP_WROVER_KIT_DISPLAY__H
