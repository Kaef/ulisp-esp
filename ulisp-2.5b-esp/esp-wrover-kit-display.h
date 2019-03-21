#ifndef ESP_WROVER_KIT_DISPLAY__H
#define ESP_WROVER_KIT_DISPLAY__H

// The first implementation uses the WROVER_KIT_LCD_KAEF library,
// whereas for the second try I used the TFT_eSPI library.
// I think the TFT_eSPI library works a bit smoother than the WROVER_KIT_LCD_KAEF lib.
// (it may depend on the implementation anyway...)

// --------------------------- USER SETTINGS -----------------------------------------------
// comment out the next line to use WROVER_KIT_LCD_KAEF library:
#define USE_TFT_eSPI_LIB

#ifdef USE_TFT_eSPI_LIB
// -----------------------------------------------------------------------------------------
// This is the second implementation using TFT_eSPI library
// Install TFT_eSPI library through arduino library manager and
//   copy ulisp-esp/libraries/TFT_eSPI/User_Setup.h to your libraries/TFT_eSPI folder!

// Taken from the example sketch 'TFT_Terminal' from TFT_eSPI library:

// ===============================================================================
// 2019-03-21 Kaef: remarks
// I still think this implementation is not good enough now.
// There are two globals variables xPos and yDraw;
// the library uses setCursorPos(x, y) and getCursorX(), getCursorY().
// Why not using the library functions to handle cursor positions?
// Right now the implementation is somewhat fragile, because local and library
// cursor positions must stay synchronize to work correctly.
// Maybe I'm overlooking something, I'll try to fix this soon.
// ===============================================================================

/*
    The sketch works with the ILI9341 TFT 240x320 display and
    the called up libraries.

    The sketch uses the hardware scrolling feature of the
    display. Modification of this sketch may lead to problems
    unless the ILI9341 data sheet has been understood!

    Updated by Bodmer 21/12/16 for TFT_eSPI library:
    https://github.com/Bodmer/TFT_eSPI

    BSD license applies, all text above must be included in any
    redistribution
 *************************************************************/
#include <TFT_eSPI.h> // Hardware-specific library
#include <SPI.h>

TFT_eSPI tft = TFT_eSPI();       // Invoke custom library

static const uint8_t FONT = 1;

// yellow on dark blue: color565(0xff, 0xff, 0x00) == 0xFFE0, color565(0, 0, 8) == 8
static const uint16_t TEXT_COLOR    = 0xFFE0; // TFT_WHITE;
static const uint16_t TEXT_BG_COLOR = 0x0008; // TFT_BLACK;


// The scrolling area must be a integral multiple of fontHeight
#define BOT_FIXED_AREA 0 // Number of lines in bottom fixed area (lines counted from bottom of screen)
#define TOP_FIXED_AREA 0 // Number of lines in top fixed area (lines counted from top of screen)
#define YMAX 320 // Bottom of screen area

// The initial y coordinate of the top of the scrolling area
static uint16_t yStart = TOP_FIXED_AREA;
// yArea must be a integral multiple of TEXT_HEIGHT
static uint16_t yArea = YMAX - TOP_FIXED_AREA - BOT_FIXED_AREA;
// The initial y coordinate of the top of the bottom text line
static uint16_t yDraw;

// Keep track of the drawing x coordinate
static uint16_t xPos = 0;

// A few test variables used during debugging
static boolean change_colour = 1;
static boolean selected = 1;

// We have to blank the top line each time the display is scrolled, but this takes up to 13 milliseconds
// for a full width line, meanwhile the serial buffer may be filling... and overflowing
// We can speed up scrolling of short text lines by just blanking the character we drew
static const int CHARS_PER_LINE = 40;
static int blank[CHARS_PER_LINE]; // We keep all the strings pixel lengths to optimise the speed of the top line blanking

// Prototypes:
void setupScrollArea(uint16_t tfa, uint16_t bfa);
int scroll_line();
void scrollAddress(uint16_t vsp);


void setupWroverKit() {
    tft.init();
    tft.setRotation( 0 ); // portrait mode 0 is required
    tft.setTextFont(FONT);
    yDraw = YMAX - BOT_FIXED_AREA - tft.fontHeight();
    tft.fillScreen(TEXT_BG_COLOR);
    tft.setTextColor(TEXT_COLOR);
    if (TOP_FIXED_AREA > 0) {
        tft.fillRect(0, 0, 240, 16, TFT_BLUE);
        tft.drawCentreString(" uLisp ", 120, 0, FONT);
    }
    // Change colour for scrolling zone text
    tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR);

    // Setup scroll area
    setupScrollArea(TOP_FIXED_AREA, BOT_FIXED_AREA);

    // Zero the array
    for (byte i = 0; i < CHARS_PER_LINE; i++) blank[i] = 0;
    tft.setCursor(xPos, yDraw);
}

// function used by uLisp to draw a char to the display
void displayPrintChar (const char c) {
    // If it is a CR or we are near end of line then scroll one line
    if ((c == '\r') || (c == '\n') || (xPos > (tft.width() - tft.textWidth(" ", FONT)))) {
        xPos = 0;
        yDraw = scroll_line(); // It can take 13ms to scroll and blank 16 pixel lines
    }
    if (c > 31 && c < 128) {
        xPos += tft.drawChar(c, xPos, yDraw, FONT);
        blank[(CHARS_PER_LINE - 1 + (yStart - TOP_FIXED_AREA) / tft.fontHeight()) % CHARS_PER_LINE] = xPos; // Keep a record of line lengths
    }
    //change_colour = 1; // Line to indicate buffer is being emptied
    tft.setCursor(xPos, yDraw);
}

void displayPrintCharInverse(const char c) {
    tft.setTextColor(TEXT_BG_COLOR, TEXT_COLOR); // TODO: grab used colors
    displayPrintChar(' ');
    tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR); // TODO: grab used colors
}

void removeLastChar() {
    int16_t s_width = tft.textWidth("_", FONT);
    xPos -= s_width; 
    if (xPos < 0) {
        xPos += tft.width();
        yDraw -= tft.fontHeight();
    }
    int16_t x = xPos; 
    int16_t y = yDraw;
    displayPrintChar(' ');
    tft.setCursor(x, y);
    xPos = x; yDraw = y;
}

void displayPrintStringBack(char *s) {
    // Serial.print(__FUNCTION__); Serial.print(" '"); Serial.print(s); Serial.println(" '");
    // save current cursor position
    int16_t s_width = tft.textWidth(s, FONT);
    // calculate starting cursor position of s
    int xs = xPos - s_width;
    int ys = yDraw;
    while (xs < 0) {
        xs += tft.width();
        ys -= tft.fontHeight();
    }
    while (ys < 0) {
        ys += tft.height();
    }

    // Serial.print(x); Serial.print(" "); Serial.print(y); Serial.print(" ");
    // Serial.print(xs); Serial.print(" "); Serial.println(ys);
    // Serial.print(textWidth); Serial.print(" "); Serial.println(textHeight);

    // setCursor to starting position of s
    tft.setCursor(xs, ys);
    // print s
    // save current cursor position:
    int16_t x = xPos, y = yDraw;
    tft.print(s);
    tft.setCursor(x, y);
    xPos = x; yDraw = y;
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

// ##############################################################################################
// Call this function to scroll the display one text line
// ##############################################################################################
int scroll_line() {
    int yTemp = yStart; // Store the old yStart, this is where we draw the next line
    // Use the record of line lengths to optimise the rectangle size we need to erase the top line
    tft.fillRect(0, yStart, blank[(yStart - TOP_FIXED_AREA) / tft.fontHeight()], tft.fontHeight(), TEXT_BG_COLOR);

    // Change the top of the scroll area
    yStart += tft.fontHeight();
    // The value must wrap around as the screen memory is a circular buffer
    if (yStart >= YMAX - BOT_FIXED_AREA)
        yStart = TOP_FIXED_AREA + (yStart - YMAX + BOT_FIXED_AREA);
    // Now we can scroll the display
    scrollAddress(yStart);
    return yTemp;
}

// ##############################################################################################
// Setup a portion of the screen for vertical scrolling
// ##############################################################################################
// We are using a hardware feature of the display, so we can only scroll in portrait orientation
void setupScrollArea(uint16_t tfa, uint16_t bfa) {
    tft.writecommand(ILI9341_VSCRDEF); // Vertical scroll definition
    tft.writedata(tfa >> 8);           // Top Fixed Area line count
    tft.writedata(tfa);
    tft.writedata((YMAX - tfa - bfa) >> 8); // Vertical Scrolling Area line count
    tft.writedata(YMAX - tfa - bfa);
    tft.writedata(bfa >> 8);           // Bottom Fixed Area line count
    tft.writedata(bfa);
}

// ##############################################################################################
// Setup the vertical scrolling start address pointer
// ##############################################################################################
void scrollAddress(uint16_t vsp) {
    tft.writecommand(ILI9341_VSCRSADD); // Vertical scrolling pointer
    tft.writedata(vsp >> 8);
    tft.writedata(vsp);
}

#else
// -----------------------------------------------------------------------------------------
// This is the first implementation using WROVER_KIT_LCD_KAEF library:
//  Kaef, 2019-03:

//  This file capsulate the display routines. Just a few functions (located at
//  the beginning of this file) are used by uLisp, others are helpers.
//  This could also be written as a C++ class (and will be, maybe ;-) )

//  I extracted some core routines from the esp-wrover-kit-lcd 'scrolltest' example
//    to use it with uLisp (marked with ##SCROLLTEST##).
//  Anyway I did not fully understand how the display controller is working, further
//    investigation is needed...

//  I had to do a small change to the WROVER_KIT_LCD library to make
//    tft and sdcard working together (use a different spi object for tft)

//  I think the display functions should be rewritten to get something like a 'canvas'
//    to write text to or draw graphics on.
//    It should include auto-scrolling when adding text at the bottom of the display.
//    A scroll (lines or textlines) function should be included too.


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

// function used by uLisp to draw a char to the display
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
    // TFT_eSPI: int16_t textWidth(const char *string, uint8_t font);
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
    // TFT_eSPI: int16_t textWidth(const char *string, uint8_t font);
    for (int i = 0; i < strlen(s); i++) {
        char sh[2]; sh[0] = s[i]; sh[1] = 0;
        tft.getTextBounds(sh, 0, 0, &x1_tmp, &y1_tmp, &w_tmp, &h_tmp);
        textWidth += w_tmp;
        if (h_tmp > textHeight) textHeight = h_tmp;
    }
    // calculate starting cursor position of s
    int xs = x - textWidth;
    int ys = y;
    while (xs < 0) {
        xs += tft.width();
        ys -= textHeight;
    }
    while (ys < 0) {
        ys += tft.height();
    }

    // Serial.print(x); Serial.print(" "); Serial.print(y); Serial.print(" ");
    // Serial.print(xs); Serial.print(" "); Serial.println(ys);
    // Serial.print(textWidth); Serial.print(" "); Serial.println(textHeight);

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

//  -----------------------------------------------------------------------
//  ##SCROLLTEST##
//  some helpers... (based on the WROVER_KIT_LCD scrolltest example):
//
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
#endif // USE_TFT_eSPI_LIB

#endif // ESP_WROVER_KIT_DISPLAY__H
