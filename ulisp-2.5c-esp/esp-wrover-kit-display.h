#ifndef ESP_WROVER_KIT_DISPLAY__H
#define ESP_WROVER_KIT_DISPLAY__H

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
static const uint16_t TEXT_COLOR    = TFT_WHITE;
static const uint16_t TEXT_BG_COLOR = TFT_BLACK;


// The scrolling area must be a integral multiple of fontHeight
#define BOT_FIXED_AREA 0 // Number of lines in bottom fixed area (lines counted from bottom of screen)
#define TOP_FIXED_AREA 0 // Number of lines in top fixed area (lines counted from top of screen)
#define YMAX 320 // Bottom of screen area

// The initial y coordinate of the top of the scrolling area
static uint16_t yStart = TOP_FIXED_AREA;
// yArea must be a integral multiple of TEXT_HEIGHT
static uint16_t yArea = YMAX - TOP_FIXED_AREA - BOT_FIXED_AREA;
// The initial y coordinate of the top of the bottom text line
static int16_t yDraw;
// Keep track of the drawing x coordinate
static int16_t xPos = 0;

// A few test variables used during debugging
static boolean change_colour = 1;
static boolean selected = 1;

// We have to blank the top line each time the display is scrolled, but this takes up to 13 milliseconds
// for a full width line, meanwhile the serial buffer may be filling... and overflowing
// We can speed up scrolling of short text lines by just blanking the character we drew
static const int CHARS_PER_LINE = 40;
static int blank[CHARS_PER_LINE]; // We keep all the strings pixel lengths to optimise the speed of the top line blanking
uint16_t yOrigin = 0;

static uint16_t expressionStartPosX = 0;
static uint16_t expressionStartPosY = 0;

// Prototypes:
void setupScrollArea(uint16_t tfa, uint16_t bfa);
int scroll_line(bool fastScrollMode);
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
    //tft.textsize = 1;
    // Setup scroll area
    setupScrollArea(TOP_FIXED_AREA, BOT_FIXED_AREA);

    // Zero the array
    for (byte i = 0; i < CHARS_PER_LINE / tft.textsize; i++) blank[i] = 0;
    tft.setCursor(xPos, yDraw);
}

// function used by uLisp to draw a char to the display
void displayPrintChar (const char c) {
    // If it is a CR or we are near end of line then scroll one line
    if ((c == '\r') || (c == '\n') || (xPos > (tft.width() - tft.textWidth(" ", FONT)))) {
        xPos = 0;
        // when CR/LF is entered don't use fastScrollMode (ie. clean whole line):
        bool fastScrollMode = (c != '\r') && (c != '\n');
        if ((yDraw + tft.fontHeight()) >= yOrigin) yDraw = scroll_line(fastScrollMode);
        else yDraw += tft.fontHeight();
    }
    if (c > 31 && c < 128) {
        xPos += tft.drawChar(c, xPos, yDraw, FONT);
        blank[(CHARS_PER_LINE / tft.textsize - 1 + (yStart - TOP_FIXED_AREA) / tft.fontHeight()) % (CHARS_PER_LINE / tft.textsize)] = xPos; // Keep a record of line lengths
    }
    //change_colour = 1; // Line to indicate buffer is being emptied
    tft.setCursor(xPos, yDraw);
}

void displayPrintCharInverse(const char c) {
    tft.setTextColor(TEXT_BG_COLOR, TEXT_COLOR); // TODO: grab used colors
    displayPrintChar(' ');
    tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR); // TODO: grab used colors
}

// Redraw current input string (slow, with updated cursor position):
void displayUpdateString(const char *s) {
    tft.setCursor(expressionStartPosX, expressionStartPosY);
    xPos = expressionStartPosX; yDraw = expressionStartPosY;
    for (int i = 0; i < strlen(s); i++) {
        displayPrintChar(s[i]);
    }
}

void removeLastChar() {
    int16_t s_width = tft.textWidth("_", FONT);
    xPos -= s_width;
    if (xPos < 0) {
        //Serial.print("removeLastChar(): "); Serial.print(xPos); Serial.print(", "); Serial.println(yDraw);
        xPos += tft.width();
        yDraw -= tft.fontHeight();
        //Serial.print("removeLastChar(): "); Serial.print(xPos); Serial.print(", "); Serial.println(yDraw);
    }
    int16_t x = xPos;
    int16_t y = yDraw;
    tft.setCursor(x, y);
    displayPrintChar(' ');
    tft.setCursor(x, y);
    xPos = x; yDraw = y;
}

// Reprint string (fast, w/o updated cursor position):
void displayReprintString(char *s, int16_t inverseStartPos) {
    // setCursor to starting position of s
    tft.setCursor(expressionStartPosX, expressionStartPosY);
    tft.setTextWrap(true, true);
    if (inverseStartPos >= 0) {
        // print first part of s:
        char h = s[inverseStartPos];
        s[inverseStartPos] = (char)0;
        //displayPrintString(s);
        tft.print(s);
        s[inverseStartPos] = h;
        // inverse
        tft.setTextColor(TEXT_BG_COLOR, TEXT_COLOR); // TODO: grab used colors
        // print rest of s
        tft.print(&s[inverseStartPos]);
        // normal
        tft.setTextColor(TEXT_COLOR, TEXT_BG_COLOR); // TODO: grab used colors
    }
    else
        tft.print(s);
}

void storeStartPosition() {
    expressionStartPosX = tft.getCursorX();
    expressionStartPosY = tft.getCursorY();
    //Serial.print("expressionStartPos: "); Serial.print(expressionStartPosX); Serial.print(", "); Serial.println(expressionStartPosY);
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
int scroll_line(bool fastScrollMode) {
    //#define THIS_FUNCTION_TIME_MEASUREMENT
#ifdef THIS_FUNCTION_TIME_MEASUREMENT
    unsigned long startTime = micros();
#endif
    int yTemp = yStart; // Store the old yStart, this is where we draw the next line
    // Use the record of line lengths to optimise the rectangle size we need to erase the top line
    if (fastScrollMode)
        tft.fillRect(0, yStart, blank[(yStart - TOP_FIXED_AREA) / tft.fontHeight()], tft.fontHeight(), TEXT_BG_COLOR);
    else tft.fillRect(0, yStart, tft.width(), tft.fontHeight(), TEXT_BG_COLOR);

    // Change the top of the scroll area
    yStart += tft.fontHeight();
    // The value must wrap around as the screen memory is a circular buffer
    if (yStart >= YMAX - BOT_FIXED_AREA)
        yStart = TOP_FIXED_AREA + (yStart - YMAX + BOT_FIXED_AREA);
    // Now we can scroll the display
    scrollAddress(yStart);
    yOrigin = yStart; // ?? evtl. + tft.fontHeight() ??
#ifdef THIS_FUNCTION_TIME_MEASUREMENT
    startTime = micros() - startTime;
    Serial.print("scroll line took us: "); Serial.println(startTime);
#endif
    return yTemp;
}
int scroll(uint16_t lines, uint16_t bgColor = TEXT_BG_COLOR) {
    int yTemp = yStart; // Store the old yStart, this is where we draw the next line
    // Use the record of line lengths to optimise the rectangle size we need to erase the top line
    tft.fillRect(0, yStart, tft.width(), lines, bgColor);

    // Change the top of the scroll area
    yStart += lines;
    // The value must wrap around as the screen memory is a circular buffer
    if (yStart >= YMAX - BOT_FIXED_AREA)
        yStart = TOP_FIXED_AREA + (yStart - YMAX + BOT_FIXED_AREA);
    // Now we can scroll the display
    scrollAddress(yStart);
    yDraw = yStart - tft.fontHeight();
    if (yDraw <= TOP_FIXED_AREA)
        yDraw = TOP_FIXED_AREA + (YMAX - BOT_FIXED_AREA - yDraw); // ?? correct ??
    xPos = 0;
    tft.setCursor(0, yStart);
    yOrigin = yStart;
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

#endif // ESP_WROVER_KIT_DISPLAY__H
