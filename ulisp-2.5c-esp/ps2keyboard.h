#ifndef PS2KEYBOARD_H
#define PS2KEYBOARD_H

#include <PS2Kbd.h>
#define KEYBOARD_DATA (uint8_t)26
#define KEYBOARD_CLK  (uint8_t)27
PS2Kbd keyboard(KEYBOARD_DATA, KEYBOARD_CLK);

#ifdef ESP_WROVER_KIT
#include "esp-wrover-kit-display.h"
#endif

static uint16_t WritePtr = 0, ReadPtr = 0;
static const int KybdBufSize = 256;
static char KybdBuf[KybdBufSize + 1];
static bool KybdAvailableMarker = false;



bool KybdAvailable()  {
    return KybdAvailableMarker;
}

void resetKybdBuf() {
    KybdAvailableMarker = false;
    WritePtr = 0;
    ReadPtr = 0;
}

bool getNextKeyboardChar(char *ch) {
    if ((ReadPtr < KybdBufSize) && (ReadPtr < WritePtr)) {
        *ch = KybdBuf[ReadPtr++];
        if (ReadPtr == WritePtr) resetKybdBuf();
        return true;
    }
    resetKybdBuf();
    return false;
}

void setupPS2Keyboard() {
    keyboard.begin();
    resetKybdBuf();
}

void ProcessKey (char c) {
#ifdef ESP_WROVER_KIT
    static int parenthesis = -1;
    static bool withinString = false;
    
    // Undo previous parenthesis highlight
    if (parenthesis != -1) {
        displayPrintStringBack(&KybdBuf[parenthesis]);
        parenthesis = -1;
    }
#endif
    // this seems to be not working...
    if (c == 27) { Serial.println("ESC"); setflag(ESCAPE); return; }    // Escape key

    if ( (c == '\n') || ((c >= 0x20) && (c <= 0x7F)) ) {
        KybdBuf[WritePtr++] = c;
        KybdBuf[WritePtr] = 0;
    }
    if (c == 8) {
        if (WritePtr > 0) WritePtr--;   // Backspace key
        else c = 0;
    }
#ifdef ESP_WROVER_KIT
    // display current char:
    showCursor(false);
    if (c == 0) yield();            // do nothing
    if (c == 8) removeLastChar();   // Backspace key
    if ((c >= 0x20) && (c <= 0x7F)) displayPrintChar(c);

    // Highlight parenthesis
    if (c == '\"') withinString = !withinString;
    if ((!withinString) && (c == ')')) {
        int search = WritePtr - 1, level = 0;
        bool withinString = false; // be carful, this overwrites the variable outside!
        while (search >= 0 && parenthesis == -1) {
            c = KybdBuf[search--];
            if (c == '\"') withinString = !withinString;
            if ((!withinString) && (c == ')')) level++;
            if ((!withinString) && (c == '(')) {
                level--;
                if (level == 0) {
                    parenthesis = search + 1;
                }
            }
        }
        if (parenthesis >= 0) {
            displayPrintStringBackInverse(&KybdBuf[parenthesis]);
        }
    } else if (c != '\n') showCursor(true);
#endif
    if ((!withinString && (c == '\n')) || (WritePtr >= KybdBufSize)) {
        KybdAvailableMarker = true;
    }
}

#endif // PS2KEYBOARD_H
