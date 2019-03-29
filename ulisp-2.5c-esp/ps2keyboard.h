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
    memset(KybdBuf, 0, KybdBufSize + 1);
    //scroll(100, TFT_YELLOW); // test scroll() function
    //scroll(8); // use TEXT_BG_COLOR when no color given
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

bool isDelimiter(const char *str, const uint16_t currentPos, const bool insideString) {
    const char *delimiters = "()\"";
    if (currentPos == 0) {
        if ((delimiters, str[currentPos]) != 0) return true;
        else return false;
    }
    if (insideString) {
        // only '"' is a delimiter
        if ((str[currentPos] == '\"') && (str[currentPos - 1] != '\\')) return true;
        else return false;
    }
    // not inside a string
    if ((strchr(delimiters, str[currentPos]) != 0) && (str[currentPos - 1] != '\\'))  return true;
    return false;
}

uint16_t findMatchingParent(const char *str, int16_t currentPos, bool insideString) {
    if ((str[currentPos] == ')') && isDelimiter(KybdBuf, WritePtr - 1, insideString)) {
        int level = 0;
        while (currentPos >= 0) {
            char c = str[currentPos];
            if ((c == '\"') && isDelimiter(str, currentPos, insideString)) insideString = !insideString;
            if ((c == ')') && isDelimiter(str, currentPos, insideString)) level++;
            if ((c == '(') && isDelimiter(str, currentPos, insideString)) {
                level--;
                if (level == 0) {
                    break;
                }
            }
            currentPos--;
        }
    } else return -1;
    return currentPos;
}

void testIsDelimiter()
{
    // Pos:                       11 1
    //               0   4 5    9 01 23
    const char *s = "(abc)\"def\\\")\")";
    //                        (      a      b      c      )      "      d      e      f      \      "      )      "     )
    const bool inside[14] = {false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  true, false};
    const bool result[14] = {true,  false, false, false, true,  true,  false, false, false, false, false, false, true, true };
    for (int i = 0; i < strlen(s); i++) {
        Serial.print("Test "); Serial.print(i); Serial.print(": ");
        if (isDelimiter(s,  i, inside[i]) == result[i]) Serial.println("Ok"); else  Serial.println("failed!");
    }
}


void setupPS2Keyboard() {
    keyboard.begin();
    resetKybdBuf();
    //testIsDelimiter();
}


void ProcessKey (char c) {
#ifdef ESP_WROVER_KIT
    static int16_t parenthesis = -1;
    static bool insideString = false;
    static int16_t parentLevel = 0;

    // Undo previous parenthesis highlight
    if (parenthesis >= 0) {
        displayReprintString(KybdBuf, -1);
        parenthesis = -1;
    }
#endif
    // this seems to be not working...
    if (c == 27) {
        Serial.println("ESC");    // Escape key
        setflag(ESCAPE);
        // through away current input line:
        resetKybdBuf();
        KybdBuf[WritePtr++] = '(';
        KybdBuf[WritePtr++] = ')';
        KybdBuf[WritePtr] = (char)0;
        KybdAvailableMarker = true;
        return;
    }
    if ( (c == '\n') || ((c >= 0x20) && (c <= 0x7F)) ) {
        if (WritePtr == 0) parentLevel = 0;
        KybdBuf[WritePtr++] = c;
        KybdBuf[WritePtr] = 0;
    }
    if (c == 8) {                  // Backspace key
        if (WritePtr > 0) {
            WritePtr--;   // drop last entered char
            // correct parentLevel count:
            if ((KybdBuf[WritePtr] == '(') && isDelimiter(KybdBuf, WritePtr, insideString)) parentLevel--;
            if ((KybdBuf[WritePtr] == ')') && isDelimiter(KybdBuf, WritePtr, insideString)) parentLevel++;
            // toggle 'insideString' when String terminator is deleted
#ifdef ESP_WROVER_KIT
            if ((KybdBuf[WritePtr] == '\"') && isDelimiter(KybdBuf, WritePtr, insideString)) insideString = !insideString;
#endif
            // add nullterminator to KybdBuf
            KybdBuf[WritePtr] = 0;
        }
        else c = 0;
    }
    if ((c == '(') && isDelimiter(KybdBuf, WritePtr - 1, insideString)) parentLevel++;
    if ((c == ')') && isDelimiter(KybdBuf, WritePtr - 1, insideString)) parentLevel--;

#ifdef ESP_WROVER_KIT
    // display current char:
    showCursor(false);
    if (c == 0) yield();            // do nothing
    if (c == 8) {                   // Backspace key
        removeLastChar();
        if (xPos == (tft.width() - tft.textWidth(" ", FONT))) displayUpdateString(KybdBuf);
    }

    // maybe we want to print '\n' during input,
    // but parenthesis highlightning and backspace key doesn't work with linebreaks now...
    if ( (c == '\n') || ((c >= 0x20) && (c <= 0x7F)) ) displayPrintChar(c);
    // auto ident:
    if (c == '\n') {
        for (int i = 0; i < (parentLevel * 2); i++) {
            ProcessKey(' ');
        }
    }
    // toggle 'insideString' when String terminator is entered
    if ((c == '\"') && isDelimiter(KybdBuf, WritePtr - 1, insideString)) insideString = !insideString;
    // Highlight parenthesis
    parenthesis = findMatchingParent(KybdBuf, WritePtr - 1, insideString);
    if ((parenthesis >= 0) && (parenthesis < WritePtr)) {
        displayReprintString(KybdBuf, parenthesis);
    }
    if ((parenthesis == -1) && (c != '\n')) showCursor(true);
    if ((!insideString && (c == '\n') && (parentLevel == 0)) || (WritePtr >= KybdBufSize)) {
#else
    if (WritePtr >= KybdBufSize) {
#endif
        KybdAvailableMarker = true;
    }
}

#endif // PS2KEYBOARD_H
