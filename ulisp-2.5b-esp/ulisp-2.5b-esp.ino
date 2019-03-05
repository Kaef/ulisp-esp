/*  uLisp ESP 2.5b - www.ulisp.com
    David Johnson-Davies - www.technoblogy.com - 20th January 2019

    Licensed under the MIT license: https://opensource.org/licenses/MIT

    patched by Kaef (esp32 PSRAM support, esp32 [light | deep]sleep support, sd-pin interface)
    27th January 2019
*/

// Compile options

// #define resetautorun
#define printfreespace
#define serialmonitor
// #define printgcs
#define sdcardsupport
// #define lisplibrary

// Kaef: BEG BLOCK
#ifdef sdcardsupport
#define SD_CARD_DEBUG
#endif
#ifndef ESP32
#error "this version of ulisp supports esp32 only!"
#endif
// Kaef: END BLOCK

// Includes
#include "LispLibrary.h"
// const char LispLibrary[] PROGMEM = "(defun hi () (print 42))"; // Kaef: just for testing

// Kaef: BEG deepsleep
#ifdef ESP32 /* Kaef */
extern "C" {
#include <driver/rtc_io.h> // (Kaef deepsleep) rtc_gpio_isolate()
#include <rom/rtc.h>       // (Kaef reset_reason) rtc_get_reset_reason(int cpuNum)
#include <esp_wifi.h>      // esp_wifi_stop()
#include <esp_sleep.h>     // esp_sleep_get_wakeup_cause()
#include <driver/gpio.h>   // gpio_reset_pin
}
#endif
// Kaef: END deepsleep
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <EEPROM.h>
#if defined (ESP8266)
#include <ESP8266WiFi.h>
#elif defined (ESP32)
#include <WiFi.h>
#endif

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif

// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           (((object *) (x))->car)
#define second(x)          (car(cdr(x)))
#define cddr(x)            (cdr(cdr(x)))
#define third(x)           (car(cdr(cdr(x))))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && (x)->type == FLOAT)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZERO = 0, SYMBOL = 2, NUMBER = 4, STREAM = 6, CHARACTER = 8, FLOAT = 10, STRING = 12, PAIR = 14 }; // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM };

enum function { SYMBOLS, NIL, TEE, NOTHING, AMPREST, LAMBDA, LET, LETSTAR, CLOSURE, SPECIAL_FORMS, QUOTE,
                DEFUN, DEFVAR, SETQ, LOOP, PUSH, POP, INCF, DECF, SETF, DOLIST, DOTIMES, TRACE, UNTRACE, FORMILLIS,
                WITHSERIAL, WITHI2C, WITHSPI, WITHSDCARD, WITHCLIENT, TAIL_FORMS, PROGN, RETURN, IF, COND, WHEN, UNLESS,
                AND, OR, FUNCTIONS, NOT, NULLFN, CONS, ATOM, LISTP, CONSP, SYMBOLP, STREAMP, EQ, CAR, FIRST, CDR, REST,
                CAAR, CADR, SECOND, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, THIRD, CDAAR, CDADR, CDDAR, CDDDR, LENGTH,
                LIST, REVERSE, NTH, ASSOC, MEMBER, APPLY, FUNCALL, APPEND, MAPC, MAPCAR, ADD, SUBTRACT, MULTIPLY, DIVIDE,
                MOD, ONEPLUS, ONEMINUS, ABS, RANDOM, MAXFN, MINFN, NOTEQ, NUMEQ, LESS, LESSEQ, GREATER, GREATEREQ, PLUSP,
                MINUSP, ZEROP, ODDP, EVENP, INTEGERP, NUMBERP, FLOATFN, FLOATP, SIN, COS, TAN, ASIN, ACOS, ATAN, SINH,
                COSH, TANH, EXP, SQRT, LOG, EXPT, CEILING, FLOOR, TRUNCATE, ROUND, CHAR, CHARCODE, CODECHAR, CHARACTERP,
                STRINGP, STRINGEQ, STRINGLESS, STRINGGREATER, SORT, STRINGFN, CONCATENATE, SUBSEQ, READFROMSTRING,
                PRINCTOSTRING, PRIN1TOSTRING, LOGAND, LOGIOR, LOGXOR, LOGNOT, ASH, LOGBITP, EVAL, GLOBALS, LOCALS,
                MAKUNBOUND, BREAK, READ, PRIN1, PRINT, PRINC, TERPRI, READBYTE, READLINE, WRITEBYTE, WRITESTRING,
                WRITELINE, RESTARTI2C, GC, ROOM, SAVEIMAGE, LOADIMAGE, CLS, PINMODE, DIGITALREAD, DIGITALWRITE,
                ANALOGREAD, ANALOGWRITE, DELAY, MILLIS, SLEEP, NOTE, EDIT, PPRINT, PPRINTALL, AVAILABLE, WIFISERVER,
                WIFISOFTAP, CONNECTED, WIFILOCALIP, WIFICONNECT, REQUIRE, LISTLIBRARY,
                // Kaef: BEG Block
                RESETREASON, ENABLETIMERWAKEUP, DEEPSLEEPSTART, ISOLATEGPIO, ENABLEEXT0WAKEUP, GETSLEEPWAKEUPCAUSE,
                GPIOWAKEUP, LIGHTSLEEPSTART, DEBUGFLAGS,
                // Kaef: END Block
                ENDFUNCTIONS
              };

// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
    union {
        struct {
            sobject *car;
            sobject *cdr;
        };
        struct {
            unsigned int type;
            union {
                symbol_t name;
                int integer;
                float single_float;
            };
        };
    };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
    const char *string;
    fn_ptr_type fptr;
    uint8_t min;
    uint8_t max;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);

// Workspace
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(ESP8266)
#define PSTR(s) s
#define PROGMEM
#define WORKSPACESIZE 3072-SDSIZE       /* Cells (8*bytes) */
#define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
#define SYMBOLTABLESIZE 512             /* Bytes */
#define SDCARD_SS_PIN 10
uint8_t _end;
typedef int BitOrder;

#elif defined(ESP32)
#define LARGE_WORKSPACE        /* Kaef: large workspace patches */
const unsigned int PSRAMWORKSPACESIZE = (4 * 1024 * 1024) / 8; /* Kaef PSRAM */
unsigned int WORKSPACESIZE = (8000 - SDSIZE);   /* Cells (8*bytes) */ /* Kaef PSRAM */
#define EEPROMSIZE 8192                         /* Bytes available for EEPROM */
#define SYMBOLTABLESIZE 512                     /* Bytes */
#define analogWrite(x,y) dacWrite((x),(y))

// the names are a bit misleading: here, the gpio-nums has to be configured, not the pin nums!
/* */
#define SDCARD_CLK_IO    18 // Arduino standard: 18, WEMOS ESP32 WOVER: 14
#define SDCARD_MISO_IO   19 // Arduino standard: 19, WEMOS ESP32 WOVER:  2
#define SDCARD_MOSI_IO   23 // Arduino standard: 23, WEMOS ESP32 WOVER: 15
#define SDCARD_SS_PIN     5 // Kaef            :  5, WEMOS ESP32 WOVER: 13
// */
/* *
#define SDCARD_CLK_IO   14  // ESP32-WROVER-KIT V4.1
#define SDCARD_MISO_IO   2  // ESP32-WROVER-KIT V4.1
#define SDCARD_MOSI_IO  15  // ESP32-WROVER-KIT V4.1
#define sdcardSSPin     13  // ESP32-WROVER-KIT V4.1
// */

// I2C Pins ('(gpio_num_t)-1' to use default pins):
const gpio_num_t I2C_SCL = (gpio_num_t)-1; //GPIO_NUM_22; // 22 = WROVER PIN 33 (esp32 wrover default gpio)
const gpio_num_t I2C_SDA = (gpio_num_t)-1; //GPIO_NUM_21; // 21 = WROVER PIN 36 (esp32 wrover default gpio)

bool sleepModeConfigured = false;

uint8_t _end;
typedef int BitOrder;

#endif

/* Kaef PSRAM START */
#if defined ESP8266
object Workspace[WORKSPACESIZE] WORDALIGNED;
#elif defined ESP32
object *Workspace;
#else
#error "Platform not supported!"
#endif
/* Kaef PSRAM END */

// Kaef BEG Block
#define DEBUG_SLEEP  ((unsigned int) 0x0001)
#define DEBUG_SDCARD ((unsigned int) 0x0002)
unsigned int debugFlags = 0;
// Kaef END Block

char SymbolTable[SYMBOLTABLESIZE];

// Global variables

jmp_buf exception;
unsigned int Freespace = 0;
object *Freelist;
char *SymbolTop = SymbolTable;
unsigned int I2CCount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
int GlobalStringIndex = 0;
char BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;
char PrintReadably = 1;

// Flags
enum flag { RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED };
volatile char Flags;

// Forward references
object *tee;
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read ();
void repl(object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);

// Set up workspace

// Kaef PSRAM START
void setupWorkspace () {
#if defined ESP32
    pfstring(PSTR("  initworkspace "), pserial);
    if (psramFound()) {
        pfstring(PSTR("PSRAM "), pserial);
        WORKSPACESIZE = PSRAMWORKSPACESIZE;
        // Kaef: a few bytes (~ 51) of PSRAM is used by espressif
        //       because I didn't found a description for this, so I iterativly try to allocate memory
        Workspace = NULL;
        while ((Workspace == NULL) && (WORKSPACESIZE > 0)) {
            WORKSPACESIZE--;
            Workspace = (object*)ps_calloc(WORKSPACESIZE, sizeof(object));
        }
        pint(WORKSPACESIZE, pserial); pfstring(PSTR("("), pserial);
        pint(PSRAMWORKSPACESIZE / 1024, pserial); pfstring(PSTR("k - "), pserial);
        pint(PSRAMWORKSPACESIZE - WORKSPACESIZE, pserial);
        pfstring(PSTR(") Workspace cells allocated. "), pserial);
    } else {
        Workspace = (object*)calloc(WORKSPACESIZE, sizeof(object));
        pfstring(PSTR("done"), pserial);
    }
    if (Workspace == 0) {
        error(PSTR("Allocating workspace failed, entering endless loop..."));
        while (true)
            delay(1000);
    }
    memset(Workspace, 0, sizeof(*Workspace));
    pfl(pserial);
#endif
}
// Kaef PSRAM END

void initworkspace () {
    setupWorkspace(); // Kaef
    Freelist = NULL;
    for (int i = WORKSPACESIZE - 1; i >= 0; i--) {
        object *obj = &Workspace[i];
        car(obj) = NULL;
        cdr(obj) = Freelist;
        Freelist = obj;
        Freespace++;
    }
}

object *myalloc () {
    if (Freespace == 0) error(PSTR("No room"));
    object *temp = Freelist;
    Freelist = cdr(Freelist);
    Freespace--;
    return temp;
}

inline void myfree (object *obj) {
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
}

// Make each type of object

object *number (int n) {
    object *ptr = myalloc();
    ptr->type = NUMBER;
    ptr->integer = n;
    return ptr;
}

object *makefloat (float f) {
    object *ptr = myalloc();
    ptr->type = FLOAT;
    ptr->single_float = f;
    return ptr;
}

object *character (char c) {
    object *ptr = myalloc();
    ptr->type = CHARACTER;
    ptr->integer = c;
    return ptr;
}

object *cons (object *arg1, object *arg2) {
    object *ptr = myalloc();
    ptr->car = arg1;
    ptr->cdr = arg2;
    return ptr;
}

object *symbol (symbol_t name) {
    object *ptr = myalloc();
    ptr->type = SYMBOL;
    ptr->name = name;
    return ptr;
}

object *newsymbol (symbol_t name) {
    // Kaef: BEG Block
#if (defined LARGE_WORKSPACE) /* Kaef: large workspace patch */
    return symbol(name); // Kaef: speed patch for large workspaces
#endif
    // Kaef END Block
    for (int i = WORKSPACESIZE - 1; i >= 0; i--) {
        object *obj = &Workspace[i];
        if (obj->type == SYMBOL && obj->name == name) return obj;
    }
    return symbol(name);
}

object *stream (unsigned char streamtype, unsigned char address) {
    object *ptr = myalloc();
    ptr->type = STREAM;
    ptr->integer = streamtype << 8 | address;
    return ptr;
}

// Garbage collection

void markobject (object *obj) {
MARK:
    if (obj == NULL) return;
    if (marked(obj)) return;

    object* arg = car(obj);
    unsigned int type = obj->type;
    mark(obj);

    if (type >= PAIR || type == ZERO) { // cons
        markobject(arg);
        obj = cdr(obj);
        goto MARK;
    }

    if (type == STRING) {
        obj = cdr(obj);
        while (obj != NULL) {
            arg = car(obj);
            mark(obj);
            obj = arg;
        }
    }
}

void sweep () {
    Freelist = NULL;
    Freespace = 0;
    for (int i = WORKSPACESIZE - 1; i >= 0; i--) {
        object *obj = &Workspace[i];
        if (!marked(obj)) myfree(obj); else unmark(obj);
    }
}

void gc (object *form, object *env) {
#if defined(printgcs)
    int start = Freespace;
#endif
    markobject(tee);
    markobject(GlobalEnv);
    markobject(GCStack);
    markobject(form);
    markobject(env);
    sweep();
#if defined(printgcs)
    pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
#endif
}

// Compact image
// Kaef: BEG Block
void movepointer (int idxMaxUsedCon, object *from, object *to) {
    for (int i = 0; i < idxMaxUsedCon; i++) {
        // Kaef: END Block
        object *obj = &Workspace[i];
        unsigned int type = (obj->type) & ~MARKBIT;
        if (marked(obj) && (type >= STRING || type == ZERO)) {
            if (car(obj) == (object *)((uintptr_t)from | MARKBIT))
                car(obj) = (object *)((uintptr_t)to | MARKBIT);
            if (cdr(obj) == from) cdr(obj) = to;
        }
    }
    // Fix strings
    for (int i = 0; i < idxMaxUsedCon; i++) { // Kaef
        object *obj = &Workspace[i];
        if (marked(obj) && ((obj->type) & ~MARKBIT) == STRING) {
            obj = cdr(obj);
            while (obj != NULL) {
                if (cdr(obj) == to) cdr(obj) = from;
                obj = (object *)((uintptr_t)(car(obj)) & ~MARKBIT);
            }
        }
    }
}

int compactimage (object **arg) {
    markobject(tee);
    markobject(GlobalEnv);
    markobject(GCStack);
    object *firstfree = Workspace;
    while (marked(firstfree)) firstfree++;
    object *obj = &Workspace[WORKSPACESIZE - 1];

    // Kaef: BEG Looking for last used con in Workspace
    int idxMaxUsedCon = WORKSPACESIZE - 1;
    /* */
    for (int i = WORKSPACESIZE - 1; i >= 0; i--) {
        /* Kaef, 2018-11-18: user ZERO, not NULL to suppress compiler warning */
        if (Workspace[i].type != ZERO) {
            idxMaxUsedCon = i;
            break;
        }
    } // */
    int count = 0; // Kaef: for debugging, see below
    // Kaef: END Search for last used cons

    while (firstfree < obj) {
        if (marked(obj)) {
            // Kaef: BEG Block
            pfstring(PSTR("."), pserial);
            if (++count > 60) {
                count = 0;
                pln(pserial);
            }
            // Kaef: End Block
            car(firstfree) = car(obj);
            cdr(firstfree) = cdr(obj);
            unmark(obj);
            movepointer(idxMaxUsedCon, obj, firstfree); // Kaef

            if (GlobalEnv == obj) GlobalEnv = firstfree;
            if (GCStack == obj) GCStack = firstfree;
            if (*arg == obj) *arg = firstfree;
            while (marked(firstfree)) firstfree++;
        }
        obj--;
    }
    sweep();
    return firstfree - Workspace;
}

// Make SD card filename

char *MakeFilename (object *arg) {
    char *buffer = SymbolTop;
    int max = maxbuffer(buffer);
    buffer[0] = '/';
    int i = 1;
    do {
        char c = nthchar(arg, i - 1);
        if (c == '\0') break;
        buffer[i++] = c;
    } while (i < max);
    buffer[i] = '\0';
    return buffer;
}

// Save-image and load-image

void EpromWriteInt(int *addr, int data) {
    EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data >> 8 & 0xFF);
}

void EpromWritePtr(int *addr, uintptr_t data) {
    EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data >> 8 & 0xFF);
    EEPROM.write((*addr)++, data >> 16 & 0xFF); EEPROM.write((*addr)++, data >> 24 & 0xFF);
}

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
    file.write(data & 0xFF); file.write(data >> 8 & 0xFF);
    file.write(data >> 16 & 0xFF); file.write(data >> 24 & 0xFF);
}
#endif

int saveimage (object *arg) {
    int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
    mySPIbegin(SDCARD_SS_PIN); // Kaef
    SD.begin(SDCARD_SS_PIN);
    File file;
    if (stringp(arg)) {
        file = SD.open(MakeFilename(arg), FILE_WRITE);
        arg = NULL;
    } else if (arg == NULL || listp(arg)) file = SD.open("/ULISP.IMG", FILE_WRITE);
    else error(PSTR("'save-image' illegal argument"));
    if (!file) error(PSTR("Problem saving to SD card"));
    SDWriteInt(file, (uintptr_t)arg);
    SDWriteInt(file, imagesize);
    SDWriteInt(file, (uintptr_t)GlobalEnv);
    SDWriteInt(file, (uintptr_t)GCStack);
#if SYMBOLTABLESIZE > BUFFERSIZE
    SDWriteInt(file, (uintptr_t)SymbolTop);
    for (int i = 0; i < SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
#endif
    for (unsigned int i = 0; i < imagesize; i++) {
        object *obj = &Workspace[i];
        SDWriteInt(file, (uintptr_t)car(obj));
        SDWriteInt(file, (uintptr_t)cdr(obj));
    }
    file.close();
    return imagesize;
#else
    if (!(arg == NULL || listp(arg))) error(PSTR("'save-image' illegal argument"));
    int bytesneeded = imagesize * 8 + SYMBOLTABLESIZE + 36;
    if (bytesneeded > EEPROMSIZE) {
        pfstring(PSTR("Error: Image size too large: "), pserial);
        pint(imagesize, pserial); pln(pserial);
        GCStack = NULL;
        longjmp(exception, 1);
    }
    EEPROM.begin(EEPROMSIZE);
    int addr = 0;
    EpromWritePtr(&addr, (uintptr_t)arg);
    EpromWriteInt(&addr, imagesize);
    EpromWritePtr(&addr, (uintptr_t)GlobalEnv);
    EpromWritePtr(&addr, (uintptr_t)GCStack);
#if SYMBOLTABLESIZE > BUFFERSIZE
    EpromWritePtr(&addr, (uintptr_t)SymbolTop);
    for (int i = 0; i < SYMBOLTABLESIZE; i++) EEPROM.write(addr++, SymbolTable[i]);
#endif
    for (unsigned int i = 0; i < imagesize; i++) {
        object *obj = &Workspace[i];
        EpromWritePtr(&addr, (uintptr_t)car(obj));
        EpromWritePtr(&addr, (uintptr_t)cdr(obj));
    }
    EEPROM.commit();
    return imagesize;
#endif
    error(PSTR("'save-image' illegal argument"));
}

unsigned int EpromReadInt (int *addr) {
    int lo = EEPROM.read((*addr)++); int hi = EEPROM.read((*addr)++);
    return lo | hi << 8;
}

object *EpromReadPtr (int *addr) {
    uint8_t b0 = EEPROM.read((*addr)++); uint8_t b1 = EEPROM.read((*addr)++);
    uint8_t b2 = EEPROM.read((*addr)++); uint8_t b3 = EEPROM.read((*addr)++);
    return (object *)(b0 | b1 << 8 | b2 << 16 | b3 << 24);
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
    uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
    uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
    return b0 | b1 << 8 | b2 << 16 | b3 << 24;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
    mySPIbegin(SDCARD_SS_PIN); // Kaef
    SD.begin(SDCARD_SS_PIN);
    File file;
    if (stringp(arg)) file = SD.open(MakeFilename(arg));
    else if (arg == NULL) file = SD.open("/ULISP.IMG");
    else error(PSTR("'load-image' illegal argument"));
    if (!file) error(PSTR("Problem loading from SD card"));
    SDReadInt(file);
    int imagesize = SDReadInt(file);
    GlobalEnv = (object *)SDReadInt(file);
    GCStack = (object *)SDReadInt(file);
#if SYMBOLTABLESIZE > BUFFERSIZE
    SymbolTop = (char *)SDReadInt(file);
    for (int i = 0; i < SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
#endif
    for (int i = 0; i < imagesize; i++) {
        object *obj = &Workspace[i];
        car(obj) = (object *)SDReadInt(file);
        cdr(obj) = (object *)SDReadInt(file);
    }
    file.close();
    gc(NULL, NULL);
    return imagesize;
#else
    EEPROM.begin(EEPROMSIZE);
    int addr = 0;
    EpromReadPtr(&addr); // Skip eval address
    int imagesize = EpromReadInt(&addr);
    if (imagesize == 0 || imagesize == 0xFFFF) error(PSTR("No saved image"));
    GlobalEnv = EpromReadPtr(&addr);
    GCStack = EpromReadPtr(&addr);
#if SYMBOLTABLESIZE > BUFFERSIZE
    SymbolTop = (char *)EpromReadPtr(&addr);
    for (int i = 0; i < SYMBOLTABLESIZE; i++) SymbolTable[i] = EEPROM.read(addr++);
#endif
    for (int i = 0; i < imagesize; i++) {
        object *obj = &Workspace[i];
        car(obj) = EpromReadPtr(&addr);
        cdr(obj) = EpromReadPtr(&addr);
    }
    gc(NULL, NULL);
    return imagesize;
#endif
}

void autorunimage () {
    // Kaef: BEG Block
    pinMode(0, INPUT_PULLUP);
    if (digitalRead(0) == HIGH) { // Kaef: patch to not load workspace if button 0 is pressed
        // Kaef: END Block
#if defined(sdcardsupport)
        mySPIbegin(SDCARD_SS_PIN); // Kaef
        SD.begin(SDCARD_SS_PIN);
        File file = SD.open("/ULISP.IMG");
        if (!file) error(PSTR("Problem autorunning from SD card"));
        object *autorun = (object *)SDReadInt(file);
        object *nullenv = NULL;
        file.close();
        if (autorun != NULL) {
            loadimage(NULL);
            apply(autorun, NULL, &nullenv);
        }
#else
        object *nullenv = NULL;
        EEPROM.begin(EEPROMSIZE);
        int addr = 0;
        object *autorun = EpromReadPtr(&addr);
        if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
            loadimage(NULL);
            apply(autorun, NULL, &nullenv);
        }
#endif
    } // Kaef
}

// Error handling

void error (const char *string) {
    pfl(pserial); pfstring(PSTR("Error: "), pserial);
    pfstring(string, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
}

void error2 (object * symbol, const char *string) {
    pfl(pserial); pfstring(PSTR("Error: "), pserial);
    if (symbol == NULL) pfstring(PSTR("function "), pserial);
    else {
        pserial('\'');
        printobject(symbol, pserial);
        pfstring(PSTR("' "), pserial);
    }
    pfstring(string, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
}

// Tracing

boolean tracing (symbol_t name) {
    int i = 0;
    while (i < TRACEMAX) {
        if (TraceFn[i] == name) return i + 1;
        i++;
    }
    return 0;
}

void trace (symbol_t name) {
    if (tracing(name)) error(PSTR("Already being traced"));
    int i = 0;
    while (i < TRACEMAX) {
        if (TraceFn[i] == 0) {
            TraceFn[i] = name;
            TraceDepth[i] = 0;
            return;
        }
        i++;
    }
    error(PSTR("Already tracing 3 functions"));
}

void untrace (symbol_t name) {
    int i = 0;
    while (i < TRACEMAX) {
        if (TraceFn[i] == name) {
            TraceFn[i] = 0;
            return;
        }
        i++;
    }
    error(PSTR("It wasn't being traced"));
}

// Helper functions

boolean consp (object * x) {
    if (x == NULL) return false;
    unsigned int type = x->type;
    return type >= PAIR || type == ZERO;
}

boolean atom (object * x) {
    if (x == NULL) return true;
    unsigned int type = x->type;
    return type < PAIR && type != ZERO;
}

boolean listp (object * x) {
    if (x == NULL) return true;
    unsigned int type = x->type;
    return type >= PAIR || type == ZERO;
}

int toradix40 (char ch) {
    if (ch == 0) return 0;
    if (ch >= '0' && ch <= '9') return ch - '0' + 30;
    ch = ch | 0x20;
    if (ch >= 'a' && ch <= 'z') return ch - 'a' + 1;
    return -1; // Invalid
}

int fromradix40 (int n) {
    if (n >= 1 && n <= 26) return 'a' + n - 1;
    if (n >= 30 && n <= 39) return '0' + n - 30;
    return 0;
}

int pack40 (char *buffer) {
    return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}

boolean valid40 (char *buffer) {
    return (toradix40(buffer[0]) >= 0 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}

int digitvalue (char d) {
    if (d >= '0' && d <= '9') return d - '0';
    d = d | 0x20;
    if (d >= 'a' && d <= 'f') return d - 'a' + 10;
    return 16;
}

char *name (object * obj) {
    if (obj->type != SYMBOL) error(PSTR("Error in name"));
    symbol_t x = obj->name;
    if (x < ENDFUNCTIONS) return lookupbuiltin(x);
    else if (x >= 64000) return lookupsymbol(x);
    char *buffer = SymbolTop;
    buffer[3] = '\0';
    for (int n = 2; n >= 0; n--) {
        buffer[n] = fromradix40(x % 40);
        x = x / 40;
    }
    return buffer;
}

int integer (object * obj) {
    if (!integerp(obj)) error2(obj, PSTR("is not an integer"));
    return obj->integer;
}

float fromfloat (object * obj) {
    if (!floatp(obj)) error2(obj, PSTR("is not a float"));
    return obj->single_float;
}

float intfloat (object * obj) {
    if (integerp(obj)) return obj->integer;
    if (!floatp(obj)) error2(obj, PSTR("is not an integer or float"));
    return obj->single_float;
}

int fromchar (object * obj) {
    if (!characterp(obj)) error2(obj, PSTR("is not a character"));
    return obj->integer;
}

int istream (object * obj) {
    if (!streamp(obj)) error2(obj, PSTR("is not a stream"));
    return obj->integer;
}

int issymbol (object * obj, symbol_t n) {
    return symbolp(obj) && obj->name == n;
}

int eq (object * arg1, object * arg2) {
    if (arg1 == arg2) return true;  // Same object
    if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
    if (arg1->cdr != arg2->cdr) return false;  // Different values
    if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
    if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
    if (floatp(arg1) && floatp(arg2)) return true; // Same float
    if (characterp(arg1) && characterp(arg2)) return true;  // Same character
    return false;
}

int listlength (object * list) {
    int length = 0;
    while (list != NULL) {
        list = cdr(list);
        length++;
    }
    return length;
}

// Association lists

object *assoc (object * key, object * list) {
    while (list != NULL) {
        object *pair = first(list);
        if (eq(key, car(pair))) return pair;
        list = cdr(list);
    }
    return nil;
}

object *delassoc (object * key, object **alist) {
    object *list = *alist;
    object *prev = NULL;
    while (list != NULL) {
        object *pair = first(list);
        if (eq(key, car(pair))) {
            if (prev == NULL) *alist = cdr(list);
            else cdr(prev) = cdr(list);
            return key;
        }
        prev = list;
        list = cdr(list);
    }
    return nil;
}

// String utilities

void indent (int spaces, pfun_t pfun) {
    for (int i = 0; i < spaces; i++) pfun(' ');
}

void buildstring (char ch, int *chars, object **head) {
    static object* tail;
    static uint8_t shift;
    if (*chars == 0) {
        shift = (sizeof(int) - 1) * 8;
        *chars = ch << shift;
        object *cell = myalloc();
        if (*head == NULL) *head = cell; else tail->car = cell;
        cell->car = NULL;
        cell->integer = *chars;
        tail = cell;
    } else {
        shift = shift - 8;
        *chars = *chars | ch << shift;
        tail->integer = *chars;
        if (shift == 0) *chars = 0;
    }
}

object *readstring (char delim, gfun_t gfun) {
    object *obj = myalloc();
    obj->type = STRING;
    int ch = gfun();
    if (ch == -1) return nil;
    object *head = NULL;
    int chars = 0;
    while ((ch != delim) && (ch != -1)) {
        if (ch == '\\') ch = gfun();
        buildstring(ch, &chars, &head);
        ch = gfun();
    }
    obj->cdr = head;
    return obj;
}

int stringlength (object * form) {
    int length = 0;
    form = cdr(form);
    while (form != NULL) {
        int chars = form->integer;
        for (int i = (sizeof(int) - 1) * 8; i >= 0; i = i - 8) {
            if (chars >> i & 0xFF) length++;
        }
        form = car(form);
    }
    return length;
}

char nthchar (object * string, int n) {
    object *arg = cdr(string);
    int top;
    if (sizeof(int) == 4) {
        top = n >> 2;
        n = 3 - (n & 3);
    }
    else {
        top = n >> 1;
        n = 1 - (n & 1);
    }
    for (int i = 0; i < top; i++) {
        if (arg == NULL) return 0;
        arg = car(arg);
    }
    if (arg == NULL) return 0;
    return (arg->integer) >> (n * 8) & 0xFF;
}

char *cstringbuf (object * arg) {
    cstring(arg, SymbolTop, SYMBOLTABLESIZE - (SymbolTop - SymbolTable));
    return SymbolTop;
}

char *cstring (object * form, char *buffer, int buflen) {
    int index = 0;
    form = cdr(form);
    while (form != NULL) {
        int chars = form->integer;
        for (int i = (sizeof(int) - 1) * 8; i >= 0; i = i - 8) {
            char ch = chars >> i & 0xFF;
            if (ch) {
                if (index >= buflen - 1) error(PSTR("no room for string"));
                buffer[index++] = ch;
            }
        }
        form = car(form);
    }
    buffer[index] = '\0';
    return buffer;
}

object *lispstring (char *s) {
    object *obj = myalloc();
    obj->type = STRING;
    char ch = *s++;
    object *head = NULL;
    int chars = 0;
    while (ch) {
        if (ch == '\\') ch = *s++;
        buildstring(ch, &chars, &head);
        ch = *s++;
    }
    obj->cdr = head;
    return obj;
}

// Lookup variable in environment

object *value (symbol_t n, object * env) {
    while (env != NULL) {
        object *pair = car(env);
        if (pair != NULL && car(pair)->name == n) return pair;
        env = cdr(env);
    }
    return nil;
}

object *findvalue (object * var, object * env) {
    symbol_t varname = var->name;
    object *pair = value(varname, env);
    if (pair == NULL) pair = value(varname, GlobalEnv);
    if (pair == NULL) error2(var, PSTR("unknown variable"));
    return pair;
}

object *findtwin (object * var, object * env) {
    while (env != NULL) {
        object *pair = car(env);
        if (pair != NULL && car(pair) == var) return pair;
        env = cdr(env);
    }
    return NULL;
}

// Handling closures

object *closure (int tc, object * fname, object * state, object * function, object * args, object **env) {
    int trace = 0;
    if (fname != NULL) trace = tracing(fname->name);
    if (trace) {
        indent(TraceDepth[trace - 1] << 1, pserial);
        pint(TraceDepth[trace - 1]++, pserial);
        pserial(':'); pserial(' '); pserial('('); printobject(fname, pserial);
    }
    object *params = first(function);
    function = cdr(function);
    // Push state if not already in env
    while (state != NULL) {
        object *pair = first(state);
        if (findtwin(car(pair), *env) == NULL) push(pair, *env);
        state = cdr(state);
    }
    // Add arguments to environment
    while (params != NULL && args != NULL) {
        object *value;
        object *var = first(params);
        if (var->name == AMPREST) {
            params = cdr(params);
            var = first(params);
            value = args;
            args = NULL;
        } else {
            value = first(args);
            args = cdr(args);
        }
        object *pair = findtwin(var, *env);
        if (tc && (pair != NULL)) cdr(pair) = value;
        else push(cons(var, value), *env);
        params = cdr(params);
        if (trace) {
            pserial(' ');
            printobject(value, pserial);
        }
    }
    if (params != NULL) error2(fname, PSTR("has too few parameters"));
    if (args != NULL) error2(fname, PSTR("has too many parameters"));
    if (trace) {
        pserial(')');
        pln(pserial);
    }
    // Do an implicit progn
    return tf_progn(function, *env);
}

object *apply (object * function, object * args, object **env) {
    if (symbolp(function)) {
        symbol_t name = function->name;
        int nargs = listlength(args);
        if (name >= ENDFUNCTIONS) error2(function, PSTR("is not valid here"));
        if (nargs < lookupmin(name)) error2(function, PSTR("has too few arguments"));
        if (nargs > lookupmax(name)) error2(function, PSTR("has too many arguments"));
        return ((fn_ptr_type)lookupfn(name))(args, *env);
    }
    if (listp(function) && issymbol(car(function), LAMBDA)) {
        function = cdr(function);
        object *result = closure(0, NULL, NULL, function, args, env);
        return eval(result, *env);
    }
    if (listp(function) && issymbol(car(function), CLOSURE)) {
        function = cdr(function);
        object *result = closure(0, NULL, car(function), cdr(function), args, env);
        return eval(result, *env);
    }
    error2(function, PSTR("is an illegal function"));
    return NULL;
}

// In-place operations

object **place (object * args, object * env) {
    if (atom(args)) return &cdr(findvalue(args, env));
    object* function = first(args);
    if (issymbol(function, CAR) || issymbol(function, FIRST)) {
        object *value = eval(second(args), env);
        if (!listp(value)) error(PSTR("Can't take car"));
        return &car(value);
    }
    if (issymbol(function, CDR) || issymbol(function, REST)) {
        object *value = eval(second(args), env);
        if (!listp(value)) error(PSTR("Can't take cdr"));
        return &cdr(value);
    }
    if (issymbol(function, NTH)) {
        int index = integer(eval(second(args), env));
        object *list = eval(third(args), env);
        if (atom(list)) error(PSTR("'nth' second argument is not a list"));
        while (index > 0) {
            list = cdr(list);
            if (list == NULL) error(PSTR("'nth' index out of range"));
            index--;
        }
        return &car(list);
    }
    error(PSTR("Illegal place"));
    return nil;
}

// Checked car and cdr

inline object *carx (object * arg) {
    if (!listp(arg)) error(PSTR("Can't take car"));
    if (arg == nil) return nil;
    return car(arg);
}

inline object *cdrx (object * arg) {
    if (!listp(arg)) error(PSTR("Can't take cdr"));
    if (arg == nil) return nil;
    return cdr(arg);
}

// I2C interface

void I2Cinit(bool enablePullup) {
    (void) enablePullup;
    Wire.begin (I2C_SDA, I2C_SCL);
}

inline uint8_t I2Cread() {
    return Wire.read();
}

inline bool I2Cwrite(uint8_t data) {
    return Wire.write(data);
}

bool I2Cstart(uint8_t address, uint8_t read) {
    if (read == 0) Wire.beginTransmission(address);
    else Wire.requestFrom(address, I2CCount);
    return true;
}

bool I2Crestart(uint8_t address, uint8_t read) {
    int error = (Wire.endTransmission(true) != 0);
    if (read == 0) Wire.beginTransmission(address);
    else Wire.requestFrom(address, I2CCount);
    return error ? false : true;
}

void I2Cstop(uint8_t read) {
    if (read == 0) Wire.endTransmission(); // Check for error?
}

// Streams

inline int spiread () {
    return SPI.transfer(0);
}
#if defined(ESP32) || defined(ESP8266)
inline int serial1read () {
    while (!Serial1.available()) testescape();
    return Serial1.read();
}
#endif
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
    if (LastChar) {
        char temp = LastChar;
        LastChar = 0;
        return temp;
    }
    return SDgfile.read();
}
#endif

WiFiClient client;
WiFiServer server(80);

inline int WiFiread () {
    if (LastChar) {
        char temp = LastChar;
        LastChar = 0;
        return temp;
    }
    return client.read();
}

void serialbegin (int address, int baud) {
#if defined(ESP32) || defined(ESP8266)
    if (address == 1) Serial1.begin((long)baud * 100);
    else error(PSTR("'with-serial' port not supported"));
#endif
}

void serialend (int address) {
#if defined(ESP32) || defined(ESP8266)
    if (address == 1) {
        Serial1.flush();
        Serial1.end();
    }
#endif
}

gfun_t gstreamfun (object * args) {
    int streamtype = SERIALSTREAM;
    int address = 0;
    gfun_t gfun = gserial;
    if (args != NULL) {
        int stream = istream(first(args));
        streamtype = stream >> 8; address = stream & 0xFF;
    }
    if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
    else if (streamtype == SPISTREAM) gfun = spiread;
    else if (streamtype == SERIALSTREAM) {
        if (address == 0) gfun = gserial;
        else if (address == 1) gfun = serial1read;
    }
#if defined(sdcardsupport)
    else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
#endif
    else if (streamtype == WIFISTREAM) gfun = (gfun_t)WiFiread;
    else error(PSTR("Unknown stream type"));
    return gfun;
}

inline void spiwrite (char c) {
    SPI.transfer(c);
}
inline void WiFiwrite (char c) {
    client.write(c);
}
#if defined(sdcardsupport)
inline void SDwrite (char c) {
    SDpfile.write(c);
}
#endif

pfun_t pstreamfun (object * args) {
    int streamtype = SERIALSTREAM;
    int address = 0;
    pfun_t pfun = pserial;
    if (args != NULL && first(args) != NULL) {
        int stream = istream(first(args));
        streamtype = stream >> 8; address = stream & 0xFF;
    }
    if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
    else if (streamtype == SPISTREAM) pfun = spiwrite;
    else if (streamtype == SERIALSTREAM) {
        if (address == 0) pfun = pserial;
    }
#if defined(sdcardsupport)
    else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
#endif
    else if (streamtype == WIFISTREAM) pfun = (pfun_t)WiFiwrite;
    else error(PSTR("unknown stream type"));
    return pfun;
}

// Check pins

void checkanalogread (int pin) {
#if defined(ESP8266)
    if (pin != 17) error(PSTR("'analogread' invalid pin"));
#elif defined(ESP32)
    if (!(pin == 0 || pin == 2 || pin == 4 || (pin >= 12 && pin <= 15) || (pin >= 25 && pin <= 27) || (pin >= 32 && pin <= 36) || pin == 39))
        error(PSTR("'analogread' invalid pin"));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ESP8266)
    if (!(pin >= 0 && pin <= 16)) error(PSTR("'analogwrite' invalid pin"));
#elif defined(ESP32)
    if (!(pin >= 25 && pin <= 26)) error(PSTR("'analogwrite' invalid pin"));
#endif
}

// Note

void tone (int pin, int note) {
    (void) pin, (void) note;
}

void noTone (int pin) {
    (void) pin;
}

const int scale[] PROGMEM = {4186, 4435, 4699, 4978, 5274, 5588, 5920, 6272, 6645, 7040, 7459, 7902};

void playnote (int pin, int note, int octave) {
    int prescaler = 8 - octave - note / 12;
    if (prescaler < 0 || prescaler > 8) error(PSTR("'note' octave out of range"));
    tone(pin, scale[note % 12] >> prescaler);
}

void nonote (int pin) {
    noTone(pin);
}

// Sleep

void initsleep () { }

// Kaef: BEG Block
void isolatePins () {
#ifdef ESP32
    // Isolate GPIO pins from external circuits. This is needed for modules
    // which have an external pull-up resistor on GPIOs (such as ESP32-WROVER on GPIO12)
    // to minimize current consumption.
    // Other pins should be isolated too.
    rtc_gpio_isolate(GPIO_NUM_0);
    rtc_gpio_isolate(GPIO_NUM_2);
    rtc_gpio_isolate(GPIO_NUM_5);
    rtc_gpio_isolate(GPIO_NUM_12);
    rtc_gpio_isolate(GPIO_NUM_15);
#endif
}

void prepareSleepTimer (float secs, bool isolate) {
#ifdef ESP32
    if (isolate) isolatePins();
    esp_sleep_enable_timer_wakeup((int)(secs * 1E6));
#else
#warning "Platform not supported!"
#endif
}

void shutdownSDCard() {
#ifdef sdcardsupport
    if (debugFlags & DEBUG_SDCARD) {
        pfstring(PSTR("Close sdcard files..."), pserial); pln(pserial); //Serial.flush();
    }
    SDpfile.close(); SDgfile.close();
#endif
}
// Kaef: END Block

void sleep (int secs) {
    // Kaef: BEG lightsleep
#ifdef ESP32
    if (debugFlags & DEBUG_SLEEP) {
        pfstring(PSTR("entering lightsleep for "), pserial); pint(secs, pserial);
        pfstring(PSTR(" seconds"), pserial); pln(pserial);
    }
    delay(50); // give time to flush serial buffer
    prepareSleepTimer(secs, true);
    if (ESP_OK != esp_light_sleep_start()) {
        pfstring(PSTR("**ERR** WiFi or BT not stopped, using delay"), pserial); pln(pserial);
        delay(1000 * secs);
    }
    // Kaef: END lightleep
#else
    delay(1000 * secs);
#endif
}

// Special forms

object *sp_quote (object * args, object * env) {
    (void) env;
    return first(args);
}

object *sp_defun (object * args, object * env) {
    (void) env;
    object *var = first(args);
    if (var->type != SYMBOL) error2(var, PSTR("is not a symbol"));
    object *val = cons(symbol(LAMBDA), cdr(args));
    object *pair = value(var->name, GlobalEnv);
    if (pair != NULL) {
        cdr(pair) = val;
        return var;
    }
    push(cons(var, val), GlobalEnv);
    return var;
}

object *sp_defvar (object * args, object * env) {
    object *var = first(args);
    if (var->type != SYMBOL) error2(var, PSTR("is not a symbol"));
    object *val = NULL;
    args = cdr(args);
    if (args != NULL) val = eval(first(args), env);
    object *pair = value(var->name, GlobalEnv);
    if (pair != NULL) {
        cdr(pair) = val;
        return var;
    }
    push(cons(var, val), GlobalEnv);
    return var;
}

object *sp_setq (object * args, object * env) {
    object *arg = eval(second(args), env);
    object *pair = findvalue(first(args), env);
    cdr(pair) = arg;
    return arg;
}

object *sp_loop (object * args, object * env) {
    clrflag(RETURNFLAG);
    object *start = args;
    for (;;) {
        yield();
        args = start;
        while (args != NULL) {
            object *result = eval(car(args), env);
            if (tstflag(RETURNFLAG)) {
                clrflag(RETURNFLAG);
                return result;
            }
            args = cdr(args);
        }
    }
}

object *sp_push (object * args, object * env) {
    object *item = eval(first(args), env);
    object **loc = place(second(args), env);
    push(item, *loc);
    return *loc;
}

object *sp_pop (object * args, object * env) {
    object **loc = place(first(args), env);
    object *result = car(*loc);
    pop(*loc);
    return result;
}

// Special forms incf/decf

object *sp_incf (object * args, object * env) {
    object **loc = place(first(args), env);
    args = cdr(args);

    object *x = *loc;
    object *inc = (args != NULL) ? eval(first(args), env) : NULL;

    if (floatp(x) || floatp(inc)) {
        float increment;
        float value = intfloat(x);

        if (inc == NULL) increment = 1.0;
        else increment = intfloat(inc);

        *loc = makefloat(value + increment);
    } else {
        int increment;
        int value = integer(x);

        if (inc == NULL) increment = 1;
        else increment = integer(inc);

        if (increment < 1) {
            if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
            else *loc = number(value + increment);
        } else {
            if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
            else *loc = number(value + increment);
        }
    }
    return *loc;
}

object *sp_decf (object * args, object * env) {
    object **loc = place(first(args), env);
    args = cdr(args);

    object *x = *loc;
    object *dec = (args != NULL) ? eval(first(args), env) : NULL;

    if (floatp(x) || floatp(dec)) {
        float decrement;
        float value = intfloat(x);

        if (dec == NULL) decrement = 1.0;
        else decrement = intfloat(dec);

        *loc = makefloat(value - decrement);
    } else {
        int decrement;
        int value = integer(x);

        if (dec == NULL) decrement = 1;
        else decrement = integer(dec);

        if (decrement < 1) {
            if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
            else *loc = number(value - decrement);
        } else {
            if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
            else *loc = number(value - decrement);
        }
    }
    return *loc;
}

object *sp_setf (object * args, object * env) {
    object **loc = place(first(args), env);
    object *result = eval(second(args), env);
    *loc = result;
    return result;
}

object *sp_dolist (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    object *result;
    object *list = eval(second(params), env);
    if (!listp(list)) error(PSTR("'dolist' argument is not a list"));
    push(list, GCStack); // Don't GC the list
    object *pair = cons(var, nil);
    push(pair, env);
    params = cdr(cdr(params));
    object *forms = cdr(args);
    while (list != NULL) {
        cdr(pair) = first(list);
        list = cdr(list);
        result = eval(tf_progn(forms, env), env);
        if (tstflag(RETURNFLAG)) {
            clrflag(RETURNFLAG);
            return result;
        }
    }
    cdr(pair) = nil;
    pop(GCStack);
    if (params == NULL) return nil;
    return eval(car(params), env);
}

object *sp_dotimes (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    object *result;
    int count = integer(eval(second(params), env));
    int index = 0;
    params = cdr(cdr(params));
    object *pair = cons(var, number(0));
    push(pair, env);
    object *forms = cdr(args);
    while (index < count) {
        cdr(pair) = number(index);
        index++;
        result = eval(tf_progn(forms, env), env);
        if (tstflag(RETURNFLAG)) {
            clrflag(RETURNFLAG);
            return result;
        }
    }
    cdr(pair) = number(index);
    if (params == NULL) return nil;
    return eval(car(params), env);
}

object *sp_trace (object * args, object * env) {
    (void) env;
    while (args != NULL) {
        trace(first(args)->name);
        args = cdr(args);
    }
    int i = 0;
    while (i < TRACEMAX) {
        if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
        i++;
    }
    return args;
}

object *sp_untrace (object * args, object * env) {
    (void) env;
    if (args == NULL) {
        int i = 0;
        while (i < TRACEMAX) {
            if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
            TraceFn[i] = 0;
            i++;
        }
    } else {
        while (args != NULL) {
            untrace(first(args)->name);
            args = cdr(args);
        }
    }
    return args;
}

object *sp_formillis (object * args, object * env) {
    object *param = first(args);
    unsigned long start = millis();
    unsigned long now, total = 0;
    if (param != NULL) total = integer(first(param));
    eval(tf_progn(cdr(args), env), env);
    do {
        now = millis() - start;
        testescape();
    } while (now < total);
    if (now <= INT_MAX) return number(now);
    return nil;
}

object *sp_withserial (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    int address = integer(eval(second(params), env));
    params = cddr(params);
    int baud = 96;
    if (params != NULL) baud = integer(eval(first(params), env));
    object *pair = cons(var, stream(SERIALSTREAM, address));
    push(pair, env);
    serialbegin(address, baud);
    object *forms = cdr(args);
    object *result = eval(tf_progn(forms, env), env);
    serialend(address);
    return result;
}

object *sp_withi2c (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    int address = integer(eval(second(params), env));
    params = cddr(params);
    int read = 0; // Write
    I2CCount = 0;
    if (params != NULL) {
        object *rw = eval(first(params), env);
        if (integerp(rw)) I2CCount = integer(rw);
        read = (rw != NULL);
    }
    I2Cinit(1); // Pullups
    object *pair = cons(var, (I2Cstart(address, read)) ? stream(I2CSTREAM, address) : nil);
    push(pair, env);
    object *forms = cdr(args);
    object *result = eval(tf_progn(forms, env), env);
    I2Cstop(read);
    return result;
}

// Kaef: BEG Block

void mySPIbegin (int sdcardSSPin) {
#if ((defined SDCARD_CLK_IO) && (defined SDCARD_MISO_IO) && (defined SDCARD_MOSI_IO))
#if (defined ESP32)
    SPI.begin(SDCARD_CLK_IO, SDCARD_MISO_IO, SDCARD_MOSI_IO, sdcardSSPin);
#elif (defined ESP8266)
    SPI.begin();
    SPI.pins(SDCARD_CLK_IO, SDCARD_MISO_IO, SDCARD_MOSI_IO, sdcardSSPin);
#else
#error "Platform not supported"
#endif
#else
#if (defined ESP32)
    SPI.begin(sdcardSSPin);
#elif (defined ESP8266)
    SPI.begin();
    pfstring(PSTR("*** WARNING: using standard SPI pins and standard SPI-SS pin! Need to define SDCARD_PINS. ***"), pserial);
    pln(pserial);
#else
#error "Platform not supported"
#endif

    pfstring(PSTR("*** WARNING SPI: not all gpios defined, using arduino standard SPI GPIOs! ***"), pserial);
    pln(pserial);
#endif
}
// Kaef: END Block

object *sp_withspi (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    int pin = integer(eval(second(params), env));
    int divider = 0, mode = 0, bitorder = 1;
    object *pair = cons(var, stream(SPISTREAM, pin));
    push(pair, env);
    mySPIbegin(pin); // Kaef
    params = cddr(params);
    if (params != NULL) {
        int d = integer(eval(first(params), env));
        if (d < 1 || d > 7) error(PSTR("'with-spi' invalid divider"));
        if (d == 7) divider = 3;
        else if (d & 1) divider = (d >> 1) + 4;
        else divider = (d >> 1) - 1;
        params = cdr(params);
        if (params != NULL) {
            bitorder = (eval(first(params), env) == NULL);
            params = cdr(params);
            if (params != NULL) mode = integer(eval(first(params), env));
        }
    }
    pinMode(pin, OUTPUT);
    digitalWrite(pin, LOW);
    SPI.setBitOrder((BitOrder)bitorder);
    SPI.setClockDivider(divider);
    SPI.setDataMode(mode);
    object *forms = cdr(args);
    object *result = eval(tf_progn(forms, env), env);
    digitalWrite(pin, HIGH);
    SPI.end();
    return result;
}

object *sp_withsdcard (object * args, object * env) {
#if defined(sdcardsupport)
    object *params = first(args);
    object *var = first(params);
    object *filename = eval(second(params), env);
    params = cddr(params);
    mySPIbegin(SDCARD_SS_PIN); // Kaef
    SD.begin();
    int mode = 0;
    if (params != NULL && first(params) != NULL) mode = integer(first(params));
    const char *oflag = FILE_READ;
    if (mode == 1) oflag = FILE_APPEND; else if (mode == 2) oflag = FILE_WRITE;
    if (mode >= 1) {
        SDpfile = SD.open(MakeFilename(filename), oflag);
        if (!SDpfile) error(PSTR("Problem writing to SD card"));
    } else {
        SDgfile = SD.open(MakeFilename(filename), oflag);
        if (!SDgfile) error(PSTR("Problem reading from SD card"));
    }
    object *pair = cons(var, stream(SDSTREAM, 1));
    push(pair, env);
    object *forms = cdr(args);
    object *result = eval(tf_progn(forms, env), env);
    if (mode >= 1) SDpfile.close(); else SDgfile.close();
    return result;
#else
    (void) args, (void) env;
    error(PSTR("with-sd-card not supported"));
    return nil;
#endif
}

object *sp_withclient (object * args, object * env) {
    object *params = first(args);
    object *var = first(params);
    params = cdr(params);
    int n;
    if (params == NULL) {
        client = server.available();
        if (!client) return nil;
        n = 2;
    } else {
        object *address = eval(first(params), env);
        object *port = eval(second(params), env);
        int success;
        if (stringp(address)) success = client.connect(cstringbuf(address), integer(port));
        else if (integerp(address)) success = client.connect(integer(address), integer(port));
        else error(PSTR("'with-client' invalid address"));
        if (!success) return nil;
        n = 1;
    }
    object *pair = cons(var, stream(WIFISTREAM, n));
    push(pair, env);
    object *forms = cdr(args);
    object *result = eval(tf_progn(forms, env), env);
    client.stop();
    return result;
}

// Tail-recursive forms

object *tf_progn (object * args, object * env) {
    if (args == NULL) return nil;
    object *more = cdr(args);
    while (more != NULL) {
        object *result = eval(car(args), env);
        if (tstflag(RETURNFLAG)) return result;
        args = more;
        more = cdr(args);
    }
    return car(args);
}

object *tf_return (object * args, object * env) {
    setflag(RETURNFLAG);
    return tf_progn(args, env);
}

object *tf_if (object * args, object * env) {
    if (args == NULL || cdr(args) == NULL) error(PSTR("'if' missing argument(s)"));
    if (eval(first(args), env) != nil) return second(args);
    args = cddr(args);
    return (args != NULL) ? first(args) : nil;
}

object *tf_cond (object * args, object * env) {
    while (args != NULL) {
        object *clause = first(args);
        if (!consp(clause)) error2(clause, PSTR("is an illegal clause"));
        object *test = eval(first(clause), env);
        object *forms = cdr(clause);
        if (test != nil) {
            if (forms == NULL) return test; else return tf_progn(forms, env);
        }
        args = cdr(args);
    }
    return nil;
}

object *tf_when (object * args, object * env) {
    if (args == NULL) error(PSTR("'when' missing argument"));
    if (eval(first(args), env) != nil) return tf_progn(cdr(args), env);
    else return nil;
}

object *tf_unless (object * args, object * env) {
    if (args == NULL) error(PSTR("'unless' missing argument"));
    if (eval(first(args), env) != nil) return nil;
    else return tf_progn(cdr(args), env);
}

object *tf_and (object * args, object * env) {
    if (args == NULL) return tee;
    object *more = cdr(args);
    while (more != NULL) {
        if (eval(car(args), env) == NULL) return nil;
        args = more;
        more = cdr(args);
    }
    return car(args);
}

object *tf_or (object * args, object * env) {
    object *more = cdr(args);
    while (more != NULL) {
        object *result = eval(car(args), env);
        if (result != NULL) return result;
        args = more;
        more = cdr(args);
    }
    return car(args);
}

// Core functions

object *fn_not (object * args, object * env) {
    (void) env;
    return (first(args) == nil) ? tee : nil;
}

object *fn_cons (object * args, object * env) {
    (void) env;
    return cons(first(args), second(args));
}

object *fn_atom (object * args, object * env) {
    (void) env;
    return atom(first(args)) ? tee : nil;
}

object *fn_listp (object * args, object * env) {
    (void) env;
    return listp(first(args)) ? tee : nil;
}

object *fn_consp (object * args, object * env) {
    (void) env;
    return consp(first(args)) ? tee : nil;
}

object *fn_symbolp (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    return symbolp(arg) ? tee : nil;
}

object *fn_streamp (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    return streamp(arg) ? tee : nil;
}

object *fn_eq (object * args, object * env) {
    (void) env;
    return eq(first(args), second(args)) ? tee : nil;
}

// List functions

object *fn_car (object * args, object * env) {
    (void) env;
    return carx(first(args));
}

object *fn_cdr (object * args, object * env) {
    (void) env;
    return cdrx(first(args));
}

object *fn_caar (object * args, object * env) {
    (void) env;
    return carx(carx(first(args)));
}

object *fn_cadr (object * args, object * env) {
    (void) env;
    return carx(cdrx(first(args)));
}

object *fn_cdar (object * args, object * env) {
    (void) env;
    return cdrx(carx(first(args)));
}

object *fn_cddr (object * args, object * env) {
    (void) env;
    return cdrx(cdrx(first(args)));
}

object *fn_caaar (object * args, object * env) {
    (void) env;
    return carx(carx(carx(first(args))));
}

object *fn_caadr (object * args, object * env) {
    (void) env;
    return carx(carx(cdrx(first(args))));
}

object *fn_cadar (object * args, object * env) {
    (void) env;
    return carx(cdrx(carx(first(args))));
}

object *fn_caddr (object * args, object * env) {
    (void) env;
    return carx(cdrx(cdrx(first(args))));
}

object *fn_cdaar (object * args, object * env) {
    (void) env;
    return cdrx(carx(carx(first(args))));
}

object *fn_cdadr (object * args, object * env) {
    (void) env;
    return cdrx(carx(cdrx(first(args))));
}

object *fn_cddar (object * args, object * env) {
    (void) env;
    return cdrx(cdrx(carx(first(args))));
}

object *fn_cdddr (object * args, object * env) {
    (void) env;
    return cdrx(cdrx(cdrx(first(args))));
}

object *fn_length (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (listp(arg)) return number(listlength(arg));
    if (!stringp(arg)) error(PSTR("'length' argument is not a list or string"));
    return number(stringlength(arg));
}

object *fn_list (object * args, object * env) {
    (void) env;
    return args;
}

object *fn_reverse (object * args, object * env) {
    (void) env;
    object *list = first(args);
    if (!listp(list)) error(PSTR("'reverse' argument is not a list"));
    object *result = NULL;
    while (list != NULL) {
        push(first(list), result);
        list = cdr(list);
    }
    return result;
}

object *fn_nth (object * args, object * env) {
    (void) env;
    int n = integer(first(args));
    object *list = second(args);
    if (!listp(list)) error(PSTR("'nth' second argument is not a list"));
    while (list != NULL) {
        if (n == 0) return car(list);
        list = cdr(list);
        n--;
    }
    return nil;
}

object *fn_assoc (object * args, object * env) {
    (void) env;
    object *key = first(args);
    object *list = second(args);
    if (!listp(list)) error(PSTR("'assoc' second argument is not a list"));
    return assoc(key, list);
}

object *fn_member (object * args, object * env) {
    (void) env;
    object *item = first(args);
    object *list = second(args);
    if (!listp(list)) error(PSTR("'member' second argument is not a list"));
    while (list != NULL) {
        if (eq(item, car(list))) return list;
        list = cdr(list);
    }
    return nil;
}

object *fn_apply (object * args, object * env) {
    object *previous = NULL;
    object *last = args;
    while (cdr(last) != NULL) {
        previous = last;
        last = cdr(last);
    }
    if (!listp(car(last))) error(PSTR("'apply' last argument is not a list"));
    cdr(previous) = car(last);
    return apply(first(args), cdr(args), &env);
}

object *fn_funcall (object * args, object * env) {
    return apply(first(args), cdr(args), &env);
}

object *fn_append (object * args, object * env) {
    (void) env;
    object *head = NULL;
    object *tail = NULL;
    while (args != NULL) {
        object *list = first(args);
        if (!listp(list)) error(PSTR("'append' argument is not a list"));
        while (list != NULL) {
            object *obj = cons(first(list), NULL);
            if (head == NULL) {
                head = obj;
                tail = obj;
            } else {
                cdr(tail) = obj;
                tail = obj;
            }
            list = cdr(list);
        }
        args = cdr(args);
    }
    return head;
}

object *fn_mapc (object * args, object * env) {
    object *function = first(args);
    object *list1 = second(args);
    object *result = list1;
    if (!listp(list1)) error(PSTR("'mapc' second argument is not a list"));
    object *list2 = cddr(args);
    if (list2 != NULL) {
        list2 = car(list2);
        if (!listp(list2)) error(PSTR("'mapc' third argument is not a list"));
        while (list1 != NULL && list2 != NULL) {
            apply(function, cons(car(list1), cons(car(list2), NULL)), &env);
            list1 = cdr(list1);
            list2 = cdr(list2);
        }
    } else {
        while (list1 != NULL) {
            apply(function, cons(car(list1), NULL), &env);
            list1 = cdr(list1);
        }
    }
    return result;
}

object *fn_mapcar (object * args, object * env) {
    object *function = first(args);
    object *list1 = second(args);
    if (!listp(list1)) error(PSTR("'mapcar' second argument is not a list"));
    object *list2 = cddr(args);
    if (list2 != NULL) {
        list2 = car(list2);
        if (!listp(list2)) error(PSTR("'mapcar' third argument is not a list"));
    }
    object *head = NULL;
    object *tail = NULL;
    if (list2 != NULL) {
        while (list1 != NULL && list2 != NULL) {
            object *result = apply(function, cons(car(list1), cons(car(list2), NULL)), &env);
            object *obj = cons(result, NULL);
            if (head == NULL) {
                head = obj;
                push(head, GCStack);
                tail = obj;
            } else {
                cdr(tail) = obj;
                tail = obj;
            }
            list1 = cdr(list1);
            list2 = cdr(list2);
        }
        pop(GCStack);
    } else if (list1 != NULL) {
        while (list1 != NULL) {
            object *result = apply(function, cons(car(list1), NULL), &env);
            object *obj = cons(result, NULL);
            if (head == NULL) {
                head = obj;
                push(head, GCStack);
                tail = obj;
            } else {
                cdr(tail) = obj;
                tail = obj;
            }
            list1 = cdr(list1);
        }
        pop(GCStack);
    }
    return head;
}

// Arithmetic functions

object *add_floats (object * args, float fresult) {
    while (args != NULL) {
        object *arg = car(args);
        fresult = fresult + intfloat(arg);
        args = cdr(args);
    }
    return makefloat(fresult);
}

object *fn_add (object * args, object * env) {
    (void) env;
    int result = 0;
    while (args != NULL) {
        object *arg = car(args);

        if (floatp(arg)) return add_floats(args, (float)result);

        int val = integer(arg);
        if (val < 1) {
            if (INT_MIN - val > result) return add_floats(args, (float)result);
        }
        else {
            if (INT_MAX - val < result) return add_floats(args, (float)result);
        }
        result = result + val;
        args = cdr(args);
    }
    return number(result);
}

object *subtract_floats (object * args, float fresult) {
    while (args != NULL) {
        object *arg = car(args);
        fresult = fresult - intfloat(arg);
        args = cdr(args);
    }
    return makefloat(fresult);
}

object *negate (object * arg) {
    if (integerp(arg)) {
        int result = integer(arg);
        if (result == INT_MIN) return makefloat(-fromfloat(arg));
        else return number(-result);
    } else return makefloat(-fromfloat(arg));
}

object *fn_subtract (object * args, object * env) {
    (void) env;

    object *arg = car(args);
    args = cdr(args);

    if (args == NULL) return negate(arg);
    else if (floatp(arg)) return subtract_floats(args, fromfloat(arg));
    else {
        int result = integer(arg);

        while (args != NULL) {
            arg = car(args);

            if (floatp(arg)) return subtract_floats(args, result);

            int val = integer(car(args));
            if (val < 1) {
                if (INT_MAX + val < result) return subtract_floats(args, result);
            }
            else {
                if (INT_MIN + val > result) return subtract_floats(args, result);
            }
            result = result - val;
            args = cdr(args);
        }
        return number(result);
    }
}

object *multiply_floats(object * args, float fresult) {
    while (args != NULL) {
        object *arg = car(args);
        fresult = fresult * intfloat(arg);
        args = cdr(args);
    }
    return makefloat(fresult);
}

object *fn_multiply (object * args, object * env) {
    (void) env;
    int result = 1;
    while (args != NULL) {
        object *arg = car(args);

        if (floatp(arg)) return multiply_floats(args, result);

        int64_t val = result * (int64_t)integer(arg);
        if ((val > INT_MAX) || (val < INT_MIN)) return multiply_floats(args, result);
        result = val;

        args = cdr(args);
    }
    return number(result);
}

object *divide_floats (object * args, float fresult) {
    while (args != NULL) {
        object *arg = car(args);
        float f = intfloat(arg);
        if (f == 0.0) error(PSTR("Division by zero"));
        fresult = fresult / f;
        args = cdr(args);
    }
    return makefloat(fresult);
}

object *fn_divide (object * args, object * env) {
    (void) env;
    object* arg = first(args);
    args = cdr(args);
    // One argument
    if (args == NULL) {
        if (floatp(arg)) {
            float f = fromfloat(arg);
            if (f == 0.0) error(PSTR("Division by zero"));
            return makefloat(1.0 / f);
        } else {
            int i = integer(arg);
            if (i == 0) error(PSTR("Division by zero"));
            else if (i == 1) return number(1);
            else return makefloat(1.0 / i);
        }
    }
    // Multiple arguments
    if (floatp(arg)) return divide_floats(args, fromfloat(arg));
    else {
        int result = integer(arg);
        while (args != NULL) {
            arg = car(args);
            if (floatp(arg)) {
                return divide_floats(args, result);
            } else {
                int i = integer(arg);
                if (i == 0) error(PSTR("Division by zero"));
                if ((result % i) != 0) return divide_floats(args, result);
                if ((result == INT_MIN) && (i == -1)) return divide_floats(args, result);
                result = result / i;
                args = cdr(args);
            }
        }
        return number(result);
    }
}

object *fn_mod (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    object *arg2 = second(args);
    if (integerp(arg1) && integerp(arg2)) {
        int divisor = integer(arg2);
        if (divisor == 0) error(PSTR("Division by zero"));
        int dividend = integer(arg1);
        int remainder = dividend % divisor;
        if ((dividend < 0) != (divisor < 0)) remainder = remainder + divisor;
        return number(remainder);
    } else {
        float fdivisor = intfloat(arg2);
        if (fdivisor == 0.0) error(PSTR("Division by zero"));
        float fdividend = intfloat(arg1);
        float fremainder = fmod(fdividend , fdivisor);
        if ((fdividend < 0) != (fdivisor < 0)) fremainder = fremainder + fdivisor;
        return makefloat(fremainder);
    }
}

object *fn_oneplus (object * args, object * env) {
    (void) env;
    object* arg = first(args);
    if (floatp(arg)) return makefloat(fromfloat(arg) + 1.0);
    else {
        int result = integer(arg);
        if (result == INT_MAX) return makefloat(integer(arg) + 1.0);
        else return number(result + 1);
    }
}

object *fn_oneminus (object * args, object * env) {
    (void) env;
    object* arg = first(args);
    if (floatp(arg)) return makefloat(fromfloat(arg) - 1.0);
    else {
        int result = integer(arg);
        if (result == INT_MIN) return makefloat(integer(arg) - 1.0);
        else return number(result - 1);
    }
}

object *fn_abs (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (floatp(arg)) return makefloat(abs(fromfloat(arg)));
    else {
        int result = integer(arg);
        if (result == INT_MIN) return makefloat(abs((float)integer(arg)));
        else return number(abs(result));
    }
}

object *fn_random (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (integerp(arg)) return number(random(integer(arg)));
    else return makefloat((float)rand() / (float)(RAND_MAX / fromfloat(arg)));
}

object *fn_maxfn (object * args, object * env) {
    (void) env;
    object* result = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg = car(args);
        if (integerp(result) && integerp(arg)) {
            if ((integer(arg) > integer(result))) result = arg;
        } else if ((intfloat(arg) > intfloat(result))) result = arg;
        args = cdr(args);
    }
    return result;
}

object *fn_minfn (object * args, object * env) {
    (void) env;
    object* result = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg = car(args);
        if (integerp(result) && integerp(arg)) {
            if ((integer(arg) < integer(result))) result = arg;
        } else if ((intfloat(arg) < intfloat(result))) result = arg;
        args = cdr(args);
    }
    return result;
}

// Arithmetic comparisons

object *fn_noteq (object * args, object * env) {
    (void) env;
    while (args != NULL) {
        object *nargs = args;
        object *arg1 = first(nargs);
        nargs = cdr(nargs);
        while (nargs != NULL) {
            object *arg2 = first(nargs);
            if (integerp(arg1) && integerp(arg2)) {
                if ((integer(arg1) == integer(arg2))) return nil;
            } else if ((intfloat(arg1) == intfloat(arg2))) return nil;
            nargs = cdr(nargs);
        }
        args = cdr(args);
    }
    return tee;
}

object *fn_numeq (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg2 = first(args);
        if (integerp(arg1) && integerp(arg2)) {
            if (!(integer(arg1) == integer(arg2))) return nil;
        } else if (!(intfloat(arg1) == intfloat(arg2))) return nil;
        arg1 = arg2;
        args = cdr(args);
    }
    return tee;
}

object *fn_less (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg2 = first(args);
        if (integerp(arg1) && integerp(arg2)) {
            if (!(integer(arg1) < integer(arg2))) return nil;
        } else if (!(intfloat(arg1) < intfloat(arg2))) return nil;
        arg1 = arg2;
        args = cdr(args);
    }
    return tee;
}

object *fn_lesseq (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg2 = first(args);
        if (integerp(arg1) && integerp(arg2)) {
            if (!(integer(arg1) <= integer(arg2))) return nil;
        } else if (!(intfloat(arg1) <= intfloat(arg2))) return nil;
        arg1 = arg2;
        args = cdr(args);
    }
    return tee;
}

object *fn_greater (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg2 = first(args);
        if (integerp(arg1) && integerp(arg2)) {
            if (!(integer(arg1) > integer(arg2))) return nil;
        } else if (!(intfloat(arg1) > intfloat(arg2))) return nil;
        arg1 = arg2;
        args = cdr(args);
    }
    return tee;
}

object *fn_greatereq (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    args = cdr(args);
    while (args != NULL) {
        object *arg2 = first(args);
        if (integerp(arg1) && integerp(arg2)) {
            if (!(integer(arg1) >= integer(arg2))) return nil;
        } else if (!(intfloat(arg1) >= intfloat(arg2))) return nil;
        arg1 = arg2;
        args = cdr(args);
    }
    return tee;
}

object *fn_plusp (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (floatp(arg)) return (fromfloat(arg) > 0.0) ? tee : nil;
    return (integer(arg) > 0) ? tee : nil;
}

object *fn_minusp (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (floatp(arg)) return (fromfloat(arg) < 0.0) ? tee : nil;
    return (integer(arg) < 0) ? tee : nil;
}

object *fn_zerop (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (floatp(arg)) return (fromfloat(arg) == 0.0) ? tee : nil;
    return (integer(arg) == 0) ? tee : nil;
}

object *fn_oddp (object * args, object * env) {
    (void) env;
    return ((integer(first(args)) & 1) == 1) ? tee : nil;
}

object *fn_evenp (object * args, object * env) {
    (void) env;
    return ((integer(first(args)) & 1) == 0) ? tee : nil;
}

// Number functions

object *fn_integerp (object * args, object * env) {
    (void) env;
    return integerp(first(args)) ? tee : nil;
}

object *fn_numberp (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    return (integerp(arg) || floatp(arg)) ? tee : nil;
}

// Floating-point functions

object *fn_floatfn (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    return (floatp(arg)) ? arg : makefloat((float)integer(arg));
}

object *fn_floatp (object * args, object * env) {
    (void) env;
    return floatp(first(args)) ? tee : nil;
}

object *fn_sin (object * args, object * env) {
    (void) env;
    return makefloat(sin(intfloat(first(args))));
}

object *fn_cos (object * args, object * env) {
    (void) env;
    return makefloat(cos(intfloat(first(args))));
}

object *fn_tan (object * args, object * env) {
    (void) env;
    return makefloat(tan(intfloat(first(args))));
}

object *fn_asin (object * args, object * env) {
    (void) env;
    return makefloat(asin(intfloat(first(args))));
}

object *fn_acos (object * args, object * env) {
    (void) env;
    return makefloat(acos(intfloat(first(args))));
}

object *fn_atan (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    int div = 1;
    args = cdr(args);
    if (args != NULL) div = integer(first(args));
    return makefloat(atan2(intfloat(arg), div));
}

object *fn_sinh (object * args, object * env) {
    (void) env;
    return makefloat(sinh(intfloat(first(args))));
}

object *fn_cosh (object * args, object * env) {
    (void) env;
    return makefloat(cosh(intfloat(first(args))));
}

object *fn_tanh (object * args, object * env) {
    (void) env;
    return makefloat(tanh(intfloat(first(args))));
}

object *fn_exp (object * args, object * env) {
    (void) env;
    return makefloat(exp(intfloat(first(args))));
}

object *fn_sqrt (object * args, object * env) {
    (void) env;
    return makefloat(sqrt(intfloat(first(args))));
}

object *fn_log (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    float fresult = log(intfloat(arg));
    args = cdr(args);
    if (args == NULL) return makefloat(fresult);
    else return makefloat(fresult / log(intfloat(first(args))));
}

int intpower (int base, int exp) {
    int result = 1;
    while (exp) {
        if (exp & 1) result = result * base;
        exp = exp / 2;
        base = base * base;
    }
    return result;
}

object *fn_expt (object * args, object * env) {
    (void) env;
    object *arg1 = first(args); object *arg2 = second(args);
    float float1 = intfloat(arg1);
    float value = log(abs(float1)) * intfloat(arg2);
    if (integerp(arg1) && integerp(arg2) && (integer(arg2) > 0) && (abs(value) < 21.4875))
        return number(intpower(integer(arg1), integer(arg2)));
    if (float1 < 0) error(PSTR("'expt' invalid result"));
    return makefloat(exp(value));
}

object *fn_ceiling (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    args = cdr(args);
    if (args != NULL) return number(ceil(intfloat(arg) / intfloat(first(args))));
    else return number(ceil(intfloat(arg)));
}

object *fn_floor (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    args = cdr(args);
    if (args != NULL) return number(floor(intfloat(arg) / intfloat(first(args))));
    else return number(floor(intfloat(arg)));
}

object *fn_truncate (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    args = cdr(args);
    if (args != NULL) return number((int)(intfloat(arg) / intfloat(first(args))));
    else return number((int)(intfloat(arg)));
}

int myround (float number) {
    return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}

object *fn_round (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    args = cdr(args);
    if (args != NULL) return number(myround(intfloat(arg) / intfloat(first(args))));
    else return number(myround(intfloat(arg)));
}

// Characters

object *fn_char (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (!stringp(arg)) error2(arg, PSTR("is not a string"));
    char c = nthchar(arg, integer(second(args)));
    if (c == 0) error(PSTR("'char' index out of range"));
    return character(c);
}

object *fn_charcode (object * args, object * env) {
    (void) env;
    return number(fromchar(first(args)));
}

object *fn_codechar (object * args, object * env) {
    (void) env;
    return character(integer(first(args)));
}

object *fn_characterp (object * args, object * env) {
    (void) env;
    return characterp(first(args)) ? tee : nil;
}

// Strings

object *fn_stringp (object * args, object * env) {
    (void) env;
    return stringp(first(args)) ? tee : nil;
}

bool stringcompare (object * args, bool lt, bool gt, bool eq) {
    object *arg1 = first(args);
    object *arg2 = second(args);
    if (!stringp(arg1) || !stringp(arg2)) error(PSTR("String compare argument is not a string"));
    arg1 = cdr(arg1);
    arg2 = cdr(arg2);
    while ((arg1 != NULL) || (arg2 != NULL)) {
        if (arg1 == NULL) return lt;
        if (arg2 == NULL) return gt;
        if (arg1->integer < arg2->integer) return lt;
        if (arg1->integer > arg2->integer) return gt;
        arg1 = car(arg1);
        arg2 = car(arg2);
    }
    return eq;
}

object *fn_stringeq (object * args, object * env) {
    (void) env;
    return stringcompare(args, false, false, true) ? tee : nil;
}

object *fn_stringless (object * args, object * env) {
    (void) env;
    return stringcompare(args, true, false, false) ? tee : nil;
}

object *fn_stringgreater (object * args, object * env) {
    (void) env;
    return stringcompare(args, false, true, false) ? tee : nil;
}

object *fn_sort (object * args, object * env) {
    if (first(args) == NULL) return nil;
    object *list = cons(nil, first(args));
    push(list, GCStack);
    object *predicate = second(args);
    object *compare = cons(NULL, cons(NULL, NULL));
    object *ptr = cdr(list);
    while (cdr(ptr) != NULL) {
        object *go = list;
        while (go != ptr) {
            car(compare) = car(cdr(ptr));
            car(cdr(compare)) = car(cdr(go));
            if (apply(predicate, compare, &env)) break;
            go = cdr(go);
        }
        if (go != ptr) {
            object *obj = cdr(ptr);
            cdr(ptr) = cdr(obj);
            cdr(obj) = cdr(go);
            cdr(go) = obj;
        } else ptr = cdr(ptr);
    }
    pop(GCStack);
    return cdr(list);
}

object *fn_stringfn (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    int type = arg->type;
    if (type == STRING) return arg;
    object *obj = myalloc();
    obj->type = STRING;
    if (type == CHARACTER) {
        object *cell = myalloc();
        cell->car = NULL;
        uint8_t shift = (sizeof(int) - 1) * 8;
        cell->integer = fromchar(arg) << shift;
        obj->cdr = cell;
    } else if (type == SYMBOL) {
        char *s = name(arg);
        char ch = *s++;
        object *head = NULL;
        int chars = 0;
        while (ch) {
            if (ch == '\\') ch = *s++;
            buildstring(ch, &chars, &head);
            ch = *s++;
        }
        obj->cdr = head;
    } else error(PSTR("Cannot convert to string"));
    return obj;
}

object *fn_concatenate (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    symbol_t name = arg->name;
    if (name != STRINGFN) error(PSTR("'concatenate' only supports strings"));
    args = cdr(args);
    object *result = myalloc();
    result->type = STRING;
    object *head = NULL;
    int chars = 0;
    while (args != NULL) {
        object *obj = first(args);
        if (obj->type != STRING) error2(obj, PSTR("not a string"));
        obj = cdr(obj);
        while (obj != NULL) {
            int quad = obj->integer;
            while (quad != 0) {
                char ch = quad >> ((sizeof(int) - 1) * 8) & 0xFF;
                buildstring(ch, &chars, &head);
                quad = quad << 8;
            }
            obj = car(obj);
        }
        args = cdr(args);
    }
    result->cdr = head;
    return result;
}

object *fn_subseq (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (!stringp(arg)) error(PSTR("'subseq' first argument is not a string"));
    int start = integer(second(args));
    int end;
    args = cddr(args);
    if (args != NULL) end = integer(car(args)); else end = stringlength(arg);
    object *result = myalloc();
    result->type = STRING;
    object *head = NULL;
    int chars = 0;
    for (int i = start; i < end; i++) {
        char ch = nthchar(arg, i);
        if (ch == 0) error(PSTR("'subseq' index out of range"));
        buildstring(ch, &chars, &head);
    }
    result->cdr = head;
    return result;
}

int gstr () {
    if (LastChar) {
        char temp = LastChar;
        LastChar = 0;
        return temp;
    }
    char c = nthchar(GlobalString, GlobalStringIndex++);
    return (c != 0) ? c : '\n'; // -1?
}

object *fn_readfromstring (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    if (!stringp(arg)) error(PSTR("'read-from-string' argument is not a string"));
    GlobalString = arg;
    GlobalStringIndex = 0;
    return read(gstr);
}

void pstr (char c) {
    buildstring(c, &GlobalStringIndex, &GlobalString);
}

object *fn_princtostring (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    object *obj = myalloc();
    obj->type = STRING;
    GlobalString = NULL;
    GlobalStringIndex = 0;
    char temp = PrintReadably;
    PrintReadably = 0;
    printobject(arg, pstr);
    PrintReadably = temp;
    obj->cdr = GlobalString;
    return obj;
}

object *fn_prin1tostring (object * args, object * env) {
    (void) env;
    object *arg = first(args);
    object *obj = myalloc();
    obj->type = STRING;
    GlobalString = NULL;
    GlobalStringIndex = 0;
    printobject(arg, pstr);
    obj->cdr = GlobalString;
    return obj;
}

// Bitwise operators

object *fn_logand (object * args, object * env) {
    (void) env;
    int result = -1;
    while (args != NULL) {
        result = result & integer(first(args));
        args = cdr(args);
    }
    return number(result);
}

object *fn_logior (object * args, object * env) {
    (void) env;
    int result = 0;
    while (args != NULL) {
        result = result | integer(first(args));
        args = cdr(args);
    }
    return number(result);
}

object *fn_logxor (object * args, object * env) {
    (void) env;
    int result = 0;
    while (args != NULL) {
        result = result ^ integer(first(args));
        args = cdr(args);
    }
    return number(result);
}

object *fn_lognot (object * args, object * env) {
    (void) env;
    int result = integer(car(args));
    return number(~result);
}

object *fn_ash (object * args, object * env) {
    (void) env;
    int value = integer(first(args));
    int count = integer(second(args));
    if (count >= 0)
        return number(value << count);
    else
        return number(value >> abs(count));
}

object *fn_logbitp (object * args, object * env) {
    (void) env;
    int index = integer(first(args));
    int value = integer(second(args));
    return (bitRead(value, index) == 1) ? tee : nil;
}

// System functions

object *fn_eval (object * args, object * env) {
    return eval(first(args), env);
}

object *fn_globals (object * args, object * env) {
    (void) args;
    if (GlobalEnv == NULL) return nil;
    return fn_mapcar(cons(symbol(CAR), cons(GlobalEnv, nil)), env);
}

object *fn_locals (object * args, object * env) {
    (void) args;
    return env;
}

object *fn_makunbound (object * args, object * env) {
    (void) env;
    object *key = first(args);
    deletesymbol(key->name);
    return (delassoc(key, &GlobalEnv) != NULL) ? tee : nil;
}

object *fn_break (object * args, object * env) {
    (void) args;
    pfstring(PSTR("\rBreak!\r"), pserial);
    BreakLevel++;
    repl(env);
    BreakLevel--;
    return nil;
}

object *fn_read (object * args, object * env) {
    (void) env;
    gfun_t gfun = gstreamfun(args);
    return read(gfun);
}

object *fn_prin1 (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    printobject(obj, pfun);
    return obj;
}

object *fn_print (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    pln(pfun);
    printobject(obj, pfun);
    (pfun)(' ');
    return obj;
}

object *fn_princ (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    char temp = PrintReadably;
    PrintReadably = 0;
    printobject(obj, pfun);
    PrintReadably = temp;
    return obj;
}

object *fn_terpri (object * args, object * env) {
    (void) env;
    pfun_t pfun = pstreamfun(args);
    pln(pfun);
    return nil;
}

object *fn_readbyte (object * args, object * env) {
    (void) env;
    gfun_t gfun = gstreamfun(args);
    int c = gfun();
    return (c == -1) ? nil : number(c);
}

object *fn_readline (object * args, object * env) {
    (void) env;
    gfun_t gfun = gstreamfun(args);
    return readstring('\n', gfun);
}

object *fn_writebyte (object * args, object * env) {
    (void) env;
    int value = integer(first(args));
    pfun_t pfun = pstreamfun(cdr(args));
    (pfun)(value);
    return nil;
}

object *fn_writestring (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    char temp = PrintReadably;
    PrintReadably = 0;
    printstring(obj, pfun);
    PrintReadably = temp;
    return nil;
}

object *fn_writeline (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    char temp = PrintReadably;
    PrintReadably = 0;
    printstring(obj, pfun);
    pln(pfun);
    PrintReadably = temp;
    return nil;
}

object *fn_restarti2c (object * args, object * env) {
    (void) env;
    int stream = first(args)->integer;
    args = cdr(args);
    int read = 0; // Write
    I2CCount = 0;
    if (args != NULL) {
        object *rw = first(args);
        if (integerp(rw)) I2CCount = integer(rw);
        read = (rw != NULL);
    }
    int address = stream & 0xFF;
    if (stream >> 8 != I2CSTREAM) error(PSTR("'restart' not i2c"));
    return I2Crestart(address, read) ? tee : nil;
}

object *fn_gc (object * obj, object * env) {
    int initial = Freespace;
    unsigned long start = micros();
    gc(obj, env);
    unsigned long elapsed = micros() - start;
    pfstring(PSTR("Space: "), pserial);
    pint(Freespace - initial, pserial);
    pfstring(PSTR(" bytes, Time: "), pserial);
    pint(elapsed, pserial);
    pfstring(PSTR(" us\r"), pserial);
    return nil;
}

object *fn_room (object * args, object * env) {
    (void) args, (void) env;
    return number(Freespace);
}

object *fn_saveimage (object * args, object * env) {
    if (args != NULL) args = eval(first(args), env);
    return number(saveimage(args));
}

object *fn_loadimage (object * args, object * env) {
    (void) env;
    if (args != NULL) args = first(args);
    return number(loadimage(args));
}

object *fn_cls (object * args, object * env) {
    (void) args, (void) env;
    pserial(12);
    return nil;
}

// Arduino procedures

object *fn_pinmode (object * args, object * env) {
    (void) env;
    int pin = integer(first(args));
    object *mode = second(args);
    if ((integerp(mode) && mode->integer == 1) || mode != nil) pinMode(pin, OUTPUT);
    else if (integerp(mode) && mode->integer == 2) pinMode(pin, INPUT_PULLUP);
#if defined(INPUT_PULLDOWN)
    else if (integerp(mode) && mode->integer == 4) pinMode(pin, INPUT_PULLDOWN);
#endif
    else pinMode(pin, INPUT);
    return nil;
}

object *fn_digitalread (object * args, object * env) {
    (void) env;
    int pin = integer(first(args));
    if (digitalRead(pin) != 0) return tee; else return nil;
}

object *fn_digitalwrite (object * args, object * env) {
    (void) env;
    int pin = integer(first(args));
    object *mode = second(args);
    if (integerp(mode)) digitalWrite(pin, mode->integer);
    else digitalWrite(pin, (mode != nil));
    return mode;
}

object *fn_analogread (object * args, object * env) {
    (void) env;
    int pin = integer(first(args));
    checkanalogread(pin);
    return number(analogRead(pin));
}

object *fn_analogwrite (object * args, object * env) {
    (void) env;
    int pin = integer(first(args));
    checkanalogwrite(pin);
    object *value = second(args);
    analogWrite(pin, integer(value));
    return value;
}

object *fn_delay (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    delay(integer(arg1));
    return arg1;
}

object *fn_millis (object * args, object * env) {
    (void) args, (void) env;
    return number(millis());
}

object *fn_sleep (object * args, object * env) {
    (void) env;
    object *arg1 = first(args);
    sleep(integer(arg1));
    return arg1;
}

object *fn_note (object * args, object * env) {
    (void) env;
    static int pin = 255;
    if (args != NULL) {
        pin = integer(first(args));
        int note = 0;
        if (cddr(args) != NULL) note = integer(second(args));
        int octave = 0;
        if (cddr(args) != NULL) octave = integer(third(args));
        playnote(pin, note, octave);
    } else nonote(pin);
    return nil;
}

// Tree Editor

object *fn_edit (object * args, object * env) {
    object *fun = first(args);
    object *pair = findvalue(fun, env);
    clrflag(EXITEDITOR);
    object *arg = edit(eval(fun, env));
    cdr(pair) = arg;
    return arg;
}

object *edit (object * fun) {
    while (1) {
        if (tstflag(EXITEDITOR)) return fun;
        char c = gserial();
        if (c == 'q') setflag(EXITEDITOR);
        else if (c == 'b') return fun;
        else if (c == 'r') fun = read(gserial);
        else if (c == '\n') {
            pfl(pserial);
            superprint(fun, 0, pserial);
            pln(pserial);
        }
        else if (c == 'c') fun = cons(read(gserial), fun);
        else if (atom(fun)) pserial('!');
        else if (c == 'd') fun = cons(car(fun), edit(cdr(fun)));
        else if (c == 'a') fun = cons(edit(car(fun)), cdr(fun));
        else if (c == 'x') fun = cdr(fun);
        else pserial('?');
    }
}

// Pretty printer

const int PPINDENT = 2;
const int PPWIDTH = 80;

void pcount (char c) {
    LastPrint = c;
    if (c == '\n') GlobalStringIndex++;
    GlobalStringIndex++;
}

int atomwidth (object * obj) {
    GlobalStringIndex = 0;
    printobject(obj, pcount);
    return GlobalStringIndex;
}

boolean quoted (object * obj) {
    return (consp(obj) && car(obj) != NULL && car(obj)->name == QUOTE && consp(cdr(obj)) && cddr(obj) == NULL);
}

int subwidth (object * obj, int w) {
    if (atom(obj)) return w - atomwidth(obj);
    if (quoted(obj)) return subwidthlist(car(cdr(obj)), w - 1);
    return subwidthlist(obj, w - 1);
}

int subwidthlist (object * form, int w) {
    while (form != NULL && w >= 0) {
        if (atom(form)) return w - (2 + atomwidth(form));
        w = subwidth(car(form), w - 1);
        form = cdr(form);
    }
    return w;
}

void superprint (object * form, int lm, pfun_t pfun) {
    if (atom(form)) {
        if (symbolp(form) && form->name == NOTHING) pstring(name(form), pfun);
        else printobject(form, pfun);
    }
    else if (quoted(form)) {
        pfun('\'');
        superprint(car(cdr(form)), lm + 1, pfun);
    }
    else if (subwidth(form, PPWIDTH - lm) >= 0) supersub(form, lm + PPINDENT, 0, pfun);
    else supersub(form, lm + PPINDENT, 1, pfun);
}

const int ppspecials = 15;
const char ppspecial[ppspecials] PROGMEM =
{ DOTIMES, DOLIST, IF, SETQ, TEE, LET, LETSTAR, LAMBDA, WHEN, UNLESS, WITHI2C, WITHSERIAL, WITHSPI, WITHSDCARD, WITHCLIENT };

void supersub (object * form, int lm, int super, pfun_t pfun) {
    int special = 0, separate = 1;
    object *arg = car(form);
    if (symbolp(arg)) {
        int name = arg->name;
        if (name == DEFUN) special = 2;
        else for (int i = 0; i < ppspecials; i++) {
                if (name == ppspecial[i]) {
                    special = 1;
                    break;
                }
            }
    }
    while (form != NULL) {
        if (atom(form)) {
            pfstring(PSTR(" . "), pfun);
            printobject(form, pfun);
            pfun(')');
            return;
        }
        else if (separate) {
            pfun('(');
            separate = 0;
        }
        else if (special) {
            pfun(' ');
            special--;
        }
        else if (!super) pfun(' ');
        else {
            pln(pfun);
            indent(lm, pfun);
        }
        superprint(car(form), lm, pfun);
        form = cdr(form);
    }
    pfun(')'); return;
}

object *fn_pprint (object * args, object * env) {
    (void) env;
    object *obj = first(args);
    pfun_t pfun = pstreamfun(cdr(args));
    pln(pfun);
    superprint(obj, 0, pfun);
    return symbol(NOTHING);
}

object *fn_pprintall (object * args, object * env) {
    (void) args, (void) env;
    object *globals = GlobalEnv;
    while (globals != NULL) {
        object *pair = first(globals);
        object *var = car(pair);
        object *val = cdr(pair);
        if (listp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
            pln(pserial);
            superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pserial);
            pln(pserial);
        }
        globals = cdr(globals);
    }
    return symbol(NOTHING);
}

object *fn_require (object * args, object * env) {
    object *arg = first(args);
    object *globals = GlobalEnv;
    if (!symbolp(arg)) error(PSTR("'require' argument is not a symbol"));
    while (globals != NULL) {
        object *pair = first(globals);
        object *var = car(pair);
        if (symbolp(var) && var == arg) return nil;
        globals = cdr(globals);
    }
    GlobalStringIndex = 0;
    object *line = read(glibrary);
    while (line != NULL) {
        // Is this the definition we want
        if ( (first(line)->name == DEFUN || first(line)->name == DEFVAR) && symbolp(second(line))
                // Kaef: '->name' added on both sides:
                && second(line)->name == arg->name) {
            eval(line, env);
            return tee;
        }
        line = read(glibrary);
    }
    return nil;
}

object *fn_listlibrary (object * args, object * env) {
    (void) args, (void) env;
    GlobalStringIndex = 0;
    object *line = read(glibrary);
    while (line != NULL) {
        if (first(line)->name == DEFUN || first(line)->name == DEFVAR) {
            pstring(name(second(line)), pserial); pserial(' ');
        }
        line = read(glibrary);
    }
    return symbol(NOTHING);
}

// Wi-fi

object *fn_available (object * args, object * env) {
    (void) env;
    if (istream(first(args)) >> 8 != WIFISTREAM) error(PSTR("'available' invalid stream"));
    return number(client.available());
}

object *fn_wifiserver (object * args, object * env) {
    (void) args, (void) env;
    server.begin();
    return nil;
}

object *fn_wifisoftap (object * args, object * env) {
    (void) env;
    char ssid[33], pass[65];
    if (args == NULL) return WiFi.softAPdisconnect(true) ? tee : nil;
    object *first = first(args); args = cdr(args);
    if (args == NULL) WiFi.softAP(cstring(first, ssid, 33));
    else {
        object *second = first(args);
        args = cdr(args);
        int channel = 1;
        boolean hidden = false;
        if (args != NULL) {
            channel = integer(first(args));
            args = cdr(args);
            if (args != NULL) hidden = (first(args) != nil);
        }
        WiFi.softAP(cstring(first, ssid, 33), cstring(second, pass, 65), channel, hidden);
    }
    return lispstring((char*)WiFi.softAPIP().toString().c_str());
}

object *fn_connected (object * args, object * env) {
    (void) env;
    if (istream(first(args)) >> 8 != WIFISTREAM) error(PSTR("'connected' invalid stream"));
    return client.connected() ? tee : nil;
}

object *fn_wifilocalip (object * args, object * env) {
    (void) args, (void) env;
    return lispstring((char*)WiFi.localIP().toString().c_str());
}

object *fn_wificonnect (object * args, object * env) {
    (void) env;
    char ssid[33], pass[65];
    if (args == NULL) {
        WiFi.disconnect(true);
        return nil;
    }
    if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
    else WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
    int result = WiFi.waitForConnectResult();
    if (result == WL_CONNECTED) return lispstring((char*)WiFi.localIP().toString().c_str());
    else if (result == WL_NO_SSID_AVAIL) error(PSTR("Network not found"));
    else if (result == WL_CONNECT_FAILED) error(PSTR("Connection failed"));
    else error(PSTR("Unable to connect"));
    return nil;
}

// Insert your own function definitions here
// Kaef: BEG (large) Block
// BEG (Kaef reset_reason)
object *fn_resetreason (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    return number(rtc_get_reset_reason(0));
#endif
    return nil;
}
// END (Kaef reset_reason)

object *fn_enabletimerwakeup (object * args, object * env) {
    (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    object *current_arg = car(args);
    if ((integerp(current_arg)) || (floatp(current_arg))) {
        if (intfloat(current_arg) >= 0.) {
            prepareSleepTimer(intfloat(current_arg), false);
            sleepModeConfigured = true;
        }
        else error(PSTR("Argument must be >= 0!"));
    } else {
        error(PSTR("Argument should be integer or float!"));
    }
#else
#error "Platform not supported!"
#endif
    return car(args);
}

object *fn_deepsleepstart (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    if (!sleepModeConfigured) error(PSTR("Please configure wakeup-mode(s) first!"));
    shutdownSDCard();
    if (debugFlags & DEBUG_SLEEP) pfstring(PSTR("Entering deepsleep..."), pserial);
    delay(50); // give some time to flush buffers...
    //esp_bluedroid_disable(); esp_bt_controller_disable(); // BT not used at the moment
    // esp_wifi_stop(); // should be called (Espressif), but leads to segfault!
    esp_deep_sleep_start();
#else
#error "Platform not supported!"
#endif
    return nil;
}

object *fn_isolategpio (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    int pins[] = {0, 1, 2, 3, 4, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33
                 };
    object *current_arg = car(args);
    if (integerp(current_arg)) {
        bool success = false;
        for (int i = 0; i < (sizeof(pins) / sizeof(pins[0])); i++) {
            if (pins[i] == integer(current_arg)) {
                success = true;
                break;
            }
        }
        if (success) {
            rtc_gpio_isolate((gpio_num_t)integer(current_arg));
            return current_arg;
        } else {
            error(PSTR("Pin not valid"));
        }

    } else {
        error(PSTR("Argument should be integer (GPIO_NUM)!"));
    }
#else
#error "Platform not supported!"
#endif
    return nil;
}

object *fn_enableExt0Wakeup (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    int pins[] = {0, 2, 4, 12, 13, 14, 15, 25, 26, 27, 32, 33, 34, 35, 36, 37, 38, 39};
    object *opin = first(args);
    object *olevel = second(args);
    if (integerp(opin) && integerp(olevel)) {
        bool success = false;
        for (int i = 0; i < (sizeof(pins) / sizeof(pins[0])); i++) {
            if (pins[i] == integer(opin)) {
                success = true;
                break;
            }
        }
        if ((integer(olevel) < 0) || (integer(olevel) > 1)) error(PSTR("Level should be 0 or 1"));
        if (success) {
            if (ESP_OK == esp_sleep_enable_ext0_wakeup((gpio_num_t)integer(opin), integer(olevel))) {
                sleepModeConfigured = true;
                return args;
            } else {
                return nil;
            }
        } else {
            error(PSTR("Pin not valid"));
        }

    } else {
        error(PSTR("Arguments should be integer (GPIO_NUM, level (0, 1))!"));
    }
#else
#error "Platform not supported!"
#endif
    return nil;
}

object *fn_getSleepWakeupCause (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    return number(esp_sleep_get_wakeup_cause());
#endif
    return nil;
}

object *fn_enableGpioWakeup (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    error(PSTR("enable-gpio-wakeup not implemented yet, use enable-ext0-wakeup instead\n       (need to wait for update of ardiuino-idf (> 1.0.0))"));

    int pins[] = {0, 2, 4, 12, 13, 14, 15, 25, 26, 27, 32, 33, 34, 35, 36, 37, 38, 39};
    object *opin = first(args);
    object *olevel = second(args);
    if (integerp(opin) && integerp(olevel)) {
        bool success = false;
        for (int i = 0; i < (sizeof(pins) / sizeof(pins[0])); i++) {
            if (pins[i] == integer(opin)) {
                success = true;
                break;
            }
        }
        if ((integer(olevel) < 0) || (integer(olevel) > 1)) error(PSTR("Level should be 0 or 1"));
        if (success) {
            gpio_int_type_t intLevel = GPIO_INTR_LOW_LEVEL;
            if (integer(olevel) == 1) intLevel = GPIO_INTR_HIGH_LEVEL;
            if (ESP_OK == gpio_wakeup_enable((gpio_num_t)integer(opin), intLevel)) {
                // sleepModeConfigured = true;
                // function does not exists, but Espressif docu says it should be used...
                // need to wait for arduino-esp32-idf > 1.0.0!!
                /*
                    if (ESP_OK == esp_sleep_enable_gpio_wakeup()) {
                    sleepModeConfigured = true;
                    return args;
                    } else return nil; // */
            } else {
                return nil;
            }
        } else {
            error(PSTR("Pin not valid"));
        }

    } else {
        error(PSTR("Arguments should be integer (GPIO_NUM, level (0, 1))!"));
    }
#else
#error "Platform not supported!"
#endif
    return nil;
}

object *fn_lightsleepstart (object * args, object * env) {
    (void) args, (void) env;
#ifdef ESP8266
    error(PSTR("Not supported on ESP8266"));
#elif (defined ESP32)
    if (!sleepModeConfigured) error(PSTR("Please configure wakeup-mode(s) first!"));
    shutdownSDCard();
    if (debugFlags & DEBUG_SLEEP) pfstring(PSTR("Entering lightsleep..."), pserial);
    delay(50); // give some time to flush buffers...
    //esp_bluedroid_disable(); esp_bt_controller_disable(); // BT not used at the moment
    // esp_wifi_stop(); // should be called (Espressif), but leads to segfault!
    if (ESP_OK != esp_light_sleep_start()) error(PSTR("WiFi or BT not stopped"));
#else
#error "Platform not supported!"
#endif
    return nil;
}


void printDebugStatus(unsigned int mask) {
    if ((debugFlags & mask) == 0) pfstring(PSTR(" not "), pserial);
    pfstring(PSTR(" set"), pserial);
}

object *fn_debugFlags (object * args, object * env) {
    (void) env;
    if (args != NULL) {
        object *mask = first(args);
        if (integerp(mask)) {
            debugFlags = integer(mask);
        }
    }
    pfstring(PSTR("Debug flags:"), pserial);
    pfl(pserial); pfstring(PSTR("  0x0001: DEBGUG_SLEEP "), pserial);
    printDebugStatus(DEBUG_SLEEP);
    pfl(pserial); pfstring(PSTR("  0x0002: DEBGUG_SDCARD"), pserial);
    printDebugStatus(DEBUG_SDCARD);
    pfl(pserial);
    return cons(number(debugFlags), NULL);
}
// Kaef: END (large) Block

// Built-in procedure names - stored in PROGMEM

const char string0[] PROGMEM = "symbols";
const char string1[] PROGMEM = "nil";
const char string2[] PROGMEM = "t";
const char string3[] PROGMEM = "nothing";
const char string4[] PROGMEM = "&rest";
const char string5[] PROGMEM = "lambda";
const char string6[] PROGMEM = "let";
const char string7[] PROGMEM = "let*";
const char string8[] PROGMEM = "closure";
const char string9[] PROGMEM = "special_forms";
const char string10[] PROGMEM = "quote";
const char string11[] PROGMEM = "defun";
const char string12[] PROGMEM = "defvar";
const char string13[] PROGMEM = "setq";
const char string14[] PROGMEM = "loop";
const char string15[] PROGMEM = "push";
const char string16[] PROGMEM = "pop";
const char string17[] PROGMEM = "incf";
const char string18[] PROGMEM = "decf";
const char string19[] PROGMEM = "setf";
const char string20[] PROGMEM = "dolist";
const char string21[] PROGMEM = "dotimes";
const char string22[] PROGMEM = "trace";
const char string23[] PROGMEM = "untrace";
const char string24[] PROGMEM = "for-millis";
const char string25[] PROGMEM = "with-serial";
const char string26[] PROGMEM = "with-i2c";
const char string27[] PROGMEM = "with-spi";
const char string28[] PROGMEM = "with-sd-card";
const char string29[] PROGMEM = "with-client";
const char string30[] PROGMEM = "tail_forms";
const char string31[] PROGMEM = "progn";
const char string32[] PROGMEM = "return";
const char string33[] PROGMEM = "if";
const char string34[] PROGMEM = "cond";
const char string35[] PROGMEM = "when";
const char string36[] PROGMEM = "unless";
const char string37[] PROGMEM = "and";
const char string38[] PROGMEM = "or";
const char string39[] PROGMEM = "functions";
const char string40[] PROGMEM = "not";
const char string41[] PROGMEM = "null";
const char string42[] PROGMEM = "cons";
const char string43[] PROGMEM = "atom";
const char string44[] PROGMEM = "listp";
const char string45[] PROGMEM = "consp";
const char string46[] PROGMEM = "symbolp";
const char string47[] PROGMEM = "streamp";
const char string48[] PROGMEM = "eq";
const char string49[] PROGMEM = "car";
const char string50[] PROGMEM = "first";
const char string51[] PROGMEM = "cdr";
const char string52[] PROGMEM = "rest";
const char string53[] PROGMEM = "caar";
const char string54[] PROGMEM = "cadr";
const char string55[] PROGMEM = "second";
const char string56[] PROGMEM = "cdar";
const char string57[] PROGMEM = "cddr";
const char string58[] PROGMEM = "caaar";
const char string59[] PROGMEM = "caadr";
const char string60[] PROGMEM = "cadar";
const char string61[] PROGMEM = "caddr";
const char string62[] PROGMEM = "third";
const char string63[] PROGMEM = "cdaar";
const char string64[] PROGMEM = "cdadr";
const char string65[] PROGMEM = "cddar";
const char string66[] PROGMEM = "cdddr";
const char string67[] PROGMEM = "length";
const char string68[] PROGMEM = "list";
const char string69[] PROGMEM = "reverse";
const char string70[] PROGMEM = "nth";
const char string71[] PROGMEM = "assoc";
const char string72[] PROGMEM = "member";
const char string73[] PROGMEM = "apply";
const char string74[] PROGMEM = "funcall";
const char string75[] PROGMEM = "append";
const char string76[] PROGMEM = "mapc";
const char string77[] PROGMEM = "mapcar";
const char string78[] PROGMEM = "+";
const char string79[] PROGMEM = "-";
const char string80[] PROGMEM = "*";
const char string81[] PROGMEM = "/";
const char string82[] PROGMEM = "mod";
const char string83[] PROGMEM = "1+";
const char string84[] PROGMEM = "1-";
const char string85[] PROGMEM = "abs";
const char string86[] PROGMEM = "random";
const char string87[] PROGMEM = "max";
const char string88[] PROGMEM = "min";
const char string89[] PROGMEM = "/=";
const char string90[] PROGMEM = "=";
const char string91[] PROGMEM = "<";
const char string92[] PROGMEM = "<=";
const char string93[] PROGMEM = ">";
const char string94[] PROGMEM = ">=";
const char string95[] PROGMEM = "plusp";
const char string96[] PROGMEM = "minusp";
const char string97[] PROGMEM = "zerop";
const char string98[] PROGMEM = "oddp";
const char string99[] PROGMEM = "evenp";
const char string100[] PROGMEM = "integerp";
const char string101[] PROGMEM = "numberp";
const char string102[] PROGMEM = "float";
const char string103[] PROGMEM = "floatp";
const char string104[] PROGMEM = "sin";
const char string105[] PROGMEM = "cos";
const char string106[] PROGMEM = "tan";
const char string107[] PROGMEM = "asin";
const char string108[] PROGMEM = "acos";
const char string109[] PROGMEM = "atan";
const char string110[] PROGMEM = "sinh";
const char string111[] PROGMEM = "cosh";
const char string112[] PROGMEM = "tanh";
const char string113[] PROGMEM = "exp";
const char string114[] PROGMEM = "sqrt";
const char string115[] PROGMEM = "log";
const char string116[] PROGMEM = "expt";
const char string117[] PROGMEM = "ceiling";
const char string118[] PROGMEM = "floor";
const char string119[] PROGMEM = "truncate";
const char string120[] PROGMEM = "round";
const char string121[] PROGMEM = "char";
const char string122[] PROGMEM = "char-code";
const char string123[] PROGMEM = "code-char";
const char string124[] PROGMEM = "characterp";
const char string125[] PROGMEM = "stringp";
const char string126[] PROGMEM = "string=";
const char string127[] PROGMEM = "string<";
const char string128[] PROGMEM = "string>";
const char string129[] PROGMEM = "sort";
const char string130[] PROGMEM = "string";
const char string131[] PROGMEM = "concatenate";
const char string132[] PROGMEM = "subseq";
const char string133[] PROGMEM = "read-from-string";
const char string134[] PROGMEM = "princ-to-string";
const char string135[] PROGMEM = "prin1-to-string";
const char string136[] PROGMEM = "logand";
const char string137[] PROGMEM = "logior";
const char string138[] PROGMEM = "logxor";
const char string139[] PROGMEM = "lognot";
const char string140[] PROGMEM = "ash";
const char string141[] PROGMEM = "logbitp";
const char string142[] PROGMEM = "eval";
const char string143[] PROGMEM = "globals";
const char string144[] PROGMEM = "locals";
const char string145[] PROGMEM = "makunbound";
const char string146[] PROGMEM = "break";
const char string147[] PROGMEM = "read";
const char string148[] PROGMEM = "prin1";
const char string149[] PROGMEM = "print";
const char string150[] PROGMEM = "princ";
const char string151[] PROGMEM = "terpri";
const char string152[] PROGMEM = "read-byte";
const char string153[] PROGMEM = "read-line";
const char string154[] PROGMEM = "write-byte";
const char string155[] PROGMEM = "write-string";
const char string156[] PROGMEM = "write-line";
const char string157[] PROGMEM = "restart-i2c";
const char string158[] PROGMEM = "gc";
const char string159[] PROGMEM = "room";
const char string160[] PROGMEM = "save-image";
const char string161[] PROGMEM = "load-image";
const char string162[] PROGMEM = "cls";
const char string163[] PROGMEM = "pinmode";
const char string164[] PROGMEM = "digitalread";
const char string165[] PROGMEM = "digitalwrite";
const char string166[] PROGMEM = "analogread";
const char string167[] PROGMEM = "analogwrite";
const char string168[] PROGMEM = "delay";
const char string169[] PROGMEM = "millis";
const char string170[] PROGMEM = "sleep";
const char string171[] PROGMEM = "note";
const char string172[] PROGMEM = "edit";
const char string173[] PROGMEM = "pprint";
const char string174[] PROGMEM = "pprintall";
const char string175[] PROGMEM = "available";
const char string176[] PROGMEM = "wifi-server";
const char string177[] PROGMEM = "wifi-softap";
const char string178[] PROGMEM = "connected";
const char string179[] PROGMEM = "wifi-localip";
const char string180[] PROGMEM = "wifi-connect";
const char string181[] PROGMEM = "require";
const char string182[] PROGMEM = "list-library";
// Kaef: BEG Block
const char string183[] PROGMEM = "reset-reason"; // (Kaef reset_reason)
const char string184[] PROGMEM = "enable-timer-wakeup";
const char string185[] PROGMEM = "deepsleep-start";
const char string186[] PROGMEM = "isolate-gpio";
const char string187[] PROGMEM = "enable-ext0-wakeup";
const char string188[] PROGMEM = "get-sleep-wakeup-cause";
const char string189[] PROGMEM = "enable-gpio-wakeup";
const char string190[] PROGMEM = "lightsleep-start";
const char string191[] PROGMEM = "debug-flags";
// Kaef: END Block

const tbl_entry_t lookup_table[] PROGMEM = {
    { string0, NULL, NIL, NIL },
    { string1, NULL, 0, 0 },
    { string2, NULL, 1, 0 },
    { string3, NULL, 1, 0 },
    { string4, NULL, 1, 0 },
    { string5, NULL, 0, 127 },
    { string6, NULL, 0, 127 },
    { string7, NULL, 0, 127 },
    { string8, NULL, 0, 127 },
    { string9, NULL, NIL, NIL },
    { string10, sp_quote, 1, 1 },
    { string11, sp_defun, 0, 127 },
    { string12, sp_defvar, 2, 2 },
    { string13, sp_setq, 2, 2 },
    { string14, sp_loop, 0, 127 },
    { string15, sp_push, 2, 2 },
    { string16, sp_pop, 1, 1 },
    { string17, sp_incf, 1, 2 },
    { string18, sp_decf, 1, 2 },
    { string19, sp_setf, 2, 2 },
    { string20, sp_dolist, 1, 127 },
    { string21, sp_dotimes, 1, 127 },
    { string22, sp_trace, 0, 1 },
    { string23, sp_untrace, 0, 1 },
    { string24, sp_formillis, 1, 127 },
    { string25, sp_withserial, 1, 127 },
    { string26, sp_withi2c, 1, 127 },
    { string27, sp_withspi, 1, 127 },
    { string28, sp_withsdcard, 2, 127 },
    { string29, sp_withclient, 1, 2 },
    { string30, NULL, NIL, NIL },
    { string31, tf_progn, 0, 127 },
    { string32, tf_return, 0, 127 },
    { string33, tf_if, 2, 3 },
    { string34, tf_cond, 0, 127 },
    { string35, tf_when, 1, 127 },
    { string36, tf_unless, 1, 127 },
    { string37, tf_and, 0, 127 },
    { string38, tf_or, 0, 127 },
    { string39, NULL, NIL, NIL },
    { string40, fn_not, 1, 1 },
    { string41, fn_not, 1, 1 },
    { string42, fn_cons, 2, 2 },
    { string43, fn_atom, 1, 1 },
    { string44, fn_listp, 1, 1 },
    { string45, fn_consp, 1, 1 },
    { string46, fn_symbolp, 1, 1 },
    { string47, fn_streamp, 1, 1 },
    { string48, fn_eq, 2, 2 },
    { string49, fn_car, 1, 1 },
    { string50, fn_car, 1, 1 },
    { string51, fn_cdr, 1, 1 },
    { string52, fn_cdr, 1, 1 },
    { string53, fn_caar, 1, 1 },
    { string54, fn_cadr, 1, 1 },
    { string55, fn_cadr, 1, 1 },
    { string56, fn_cdar, 1, 1 },
    { string57, fn_cddr, 1, 1 },
    { string58, fn_caaar, 1, 1 },
    { string59, fn_caadr, 1, 1 },
    { string60, fn_cadar, 1, 1 },
    { string61, fn_caddr, 1, 1 },
    { string62, fn_caddr, 1, 1 },
    { string63, fn_cdaar, 1, 1 },
    { string64, fn_cdadr, 1, 1 },
    { string65, fn_cddar, 1, 1 },
    { string66, fn_cdddr, 1, 1 },
    { string67, fn_length, 1, 1 },
    { string68, fn_list, 0, 127 },
    { string69, fn_reverse, 1, 1 },
    { string70, fn_nth, 2, 2 },
    { string71, fn_assoc, 2, 2 },
    { string72, fn_member, 2, 2 },
    { string73, fn_apply, 2, 127 },
    { string74, fn_funcall, 1, 127 },
    { string75, fn_append, 0, 127 },
    { string76, fn_mapc, 2, 3 },
    { string77, fn_mapcar, 2, 3 },
    { string78, fn_add, 0, 127 },
    { string79, fn_subtract, 1, 127 },
    { string80, fn_multiply, 0, 127 },
    { string81, fn_divide, 1, 127 },
    { string82, fn_mod, 2, 2 },
    { string83, fn_oneplus, 1, 1 },
    { string84, fn_oneminus, 1, 1 },
    { string85, fn_abs, 1, 1 },
    { string86, fn_random, 1, 1 },
    { string87, fn_maxfn, 1, 127 },
    { string88, fn_minfn, 1, 127 },
    { string89, fn_noteq, 1, 127 },
    { string90, fn_numeq, 1, 127 },
    { string91, fn_less, 1, 127 },
    { string92, fn_lesseq, 1, 127 },
    { string93, fn_greater, 1, 127 },
    { string94, fn_greatereq, 1, 127 },
    { string95, fn_plusp, 1, 1 },
    { string96, fn_minusp, 1, 1 },
    { string97, fn_zerop, 1, 1 },
    { string98, fn_oddp, 1, 1 },
    { string99, fn_evenp, 1, 1 },
    { string100, fn_integerp, 1, 1 },
    { string101, fn_numberp, 1, 1 },
    { string102, fn_floatfn, 1, 1 },
    { string103, fn_floatp, 1, 1 },
    { string104, fn_sin, 1, 1 },
    { string105, fn_cos, 1, 1 },
    { string106, fn_tan, 1, 1 },
    { string107, fn_asin, 1, 1 },
    { string108, fn_acos, 1, 1 },
    { string109, fn_atan, 1, 2 },
    { string110, fn_sinh, 1, 1 },
    { string111, fn_cosh, 1, 1 },
    { string112, fn_tanh, 1, 1 },
    { string113, fn_exp, 1, 1 },
    { string114, fn_sqrt, 1, 1 },
    { string115, fn_log, 1, 2 },
    { string116, fn_expt, 2, 2 },
    { string117, fn_ceiling, 1, 2 },
    { string118, fn_floor, 1, 2 },
    { string119, fn_truncate, 1, 2 },
    { string120, fn_round, 1, 2 },
    { string121, fn_char, 2, 2 },
    { string122, fn_charcode, 1, 1 },
    { string123, fn_codechar, 1, 1 },
    { string124, fn_characterp, 1, 1 },
    { string125, fn_stringp, 1, 1 },
    { string126, fn_stringeq, 2, 2 },
    { string127, fn_stringless, 2, 2 },
    { string128, fn_stringgreater, 2, 2 },
    { string129, fn_sort, 2, 2 },
    { string130, fn_stringfn, 1, 1 },
    { string131, fn_concatenate, 1, 127 },
    { string132, fn_subseq, 2, 3 },
    { string133, fn_readfromstring, 1, 1 },
    { string134, fn_princtostring, 1, 1 },
    { string135, fn_prin1tostring, 1, 1 },
    { string136, fn_logand, 0, 127 },
    { string137, fn_logior, 0, 127 },
    { string138, fn_logxor, 0, 127 },
    { string139, fn_lognot, 1, 1 },
    { string140, fn_ash, 2, 2 },
    { string141, fn_logbitp, 2, 2 },
    { string142, fn_eval, 1, 1 },
    { string143, fn_globals, 0, 0 },
    { string144, fn_locals, 0, 0 },
    { string145, fn_makunbound, 1, 1 },
    { string146, fn_break, 0, 0 },
    { string147, fn_read, 0, 1 },
    { string148, fn_prin1, 1, 2 },
    { string149, fn_print, 1, 2 },
    { string150, fn_princ, 1, 2 },
    { string151, fn_terpri, 0, 1 },
    { string152, fn_readbyte, 0, 2 },
    { string153, fn_readline, 0, 1 },
    { string154, fn_writebyte, 1, 2 },
    { string155, fn_writestring, 1, 2 },
    { string156, fn_writeline, 1, 2 },
    { string157, fn_restarti2c, 1, 2 },
    { string158, fn_gc, 0, 0 },
    { string159, fn_room, 0, 0 },
    { string160, fn_saveimage, 0, 2 },
    { string161, fn_loadimage, 0, 1 },
    { string162, fn_cls, 0, 0 },
    { string163, fn_pinmode, 2, 2 },
    { string164, fn_digitalread, 1, 1 },
    { string165, fn_digitalwrite, 2, 2 },
    { string166, fn_analogread, 1, 1 },
    { string167, fn_analogwrite, 2, 2 },
    { string168, fn_delay, 1, 1 },
    { string169, fn_millis, 0, 0 },
    { string170, fn_sleep, 1, 1 },
    { string171, fn_note, 0, 3 },
    { string172, fn_edit, 1, 1 },
    { string173, fn_pprint, 1, 2 },
    { string174, fn_pprintall, 0, 0 },
    { string175, fn_available, 1, 1 },
    { string176, fn_wifiserver, 0, 0 },
    { string177, fn_wifisoftap, 0, 4 },
    { string178, fn_connected, 1, 1 },
    { string179, fn_wifilocalip, 0, 0 },
    { string180, fn_wificonnect, 0, 2 },
    { string181, fn_require, 1, 1 },
    { string182, fn_listlibrary, 0, 0 },
    // Kaef: BEG Block
    { string183, fn_resetreason, 0, 0 }, // (Kaef reset-reason)
    { string184, fn_enabletimerwakeup, 1, 1},
    { string185, fn_deepsleepstart, 0, 0},
    { string186, fn_isolategpio, 1, 1},
    { string187, fn_enableExt0Wakeup, 2, 2},
    { string188, fn_getSleepWakeupCause, 0, 0},
    { string189, fn_enableGpioWakeup, 2, 2},
    { string190, fn_lightsleepstart, 0, 0},
    { string191, fn_debugFlags, 0, 1},
    // Kaef: END Block
};

// Table lookup functions

int builtin (char* n) {
    int entry = 0;
    while (entry < ENDFUNCTIONS) {
        if (strcmp(n, (char*)lookup_table[entry].string) == 0)
            return entry;
        entry++;
    }
    return ENDFUNCTIONS;
}

int longsymbol (char *buffer) {
    char *p = SymbolTable;
    int i = 0;
    while (strcmp(p, buffer) != 0) {
        p = p + strlen(p) + 1;
        i++;
    }
    if (p == buffer) {
        // Add to symbol table?
        char *newtop = SymbolTop + strlen(p) + 1;
        if (SYMBOLTABLESIZE - (newtop - SymbolTable) < BUFFERSIZE) error(PSTR("No room for long symbols"));
        SymbolTop = newtop;
    }
    if (i > 1535) error(PSTR("Too many long symbols"));
    return i + 64000; // First number unused by radix40
}

intptr_t lookupfn (symbol_t name) {
    return (intptr_t)lookup_table[name].fptr;
}

uint8_t lookupmin (symbol_t name) {
    return lookup_table[name].min;
}

uint8_t lookupmax (symbol_t name) {
    return lookup_table[name].max;
}

char *lookupbuiltin (symbol_t name) {
    char *buffer = SymbolTop;
    strcpy(buffer, (char *)lookup_table[name].string);
    return buffer;
}

char *lookupsymbol (symbol_t name) {
    char *p = SymbolTable;
    int i = name - 64000;
    while (i > 0 && p < SymbolTop) {
        p = p + strlen(p) + 1;
        i--;
    }
    if (p == SymbolTop) return NULL; else return p;
}

void deletesymbol (symbol_t name) {
    char *p = lookupsymbol(name);
    if (p == NULL) return;
    char *q = p + strlen(p) + 1;
    *p = '\0'; p++;
    while (q < SymbolTop) *(p++) = *(q++);
    SymbolTop = p;
}

void testescape () {
    if (Serial.read() == '~') error(PSTR("Escape!"));
}

// Main evaluator

uint8_t End;

object *eval (object * form, object * env) {
    int TC = 0;
EVAL:
    yield(); // Needed to avoid Soft WDT Reset
    // Enough space?
    if (End != 0xA5) error(PSTR("Stack overflow"));
    if (Freespace <= WORKSPACESIZE >> 4) gc(form, env);
    // Escape
    if (tstflag(ESCAPE)) {
        clrflag(ESCAPE);
        error(PSTR("Escape!"));
    }
#if defined (serialmonitor)
    testescape();
#endif

    if (form == NULL) return nil;

    if (integerp(form) || floatp(form) || characterp(form) || stringp(form)) return form;

    if (symbolp(form)) {
        symbol_t name = form->name;
        if (name == NIL) return nil;
        object *pair = value(name, env);
        if (pair != NULL) return cdr(pair);
        pair = value(name, GlobalEnv);
        if (pair != NULL) return cdr(pair);
        else if (name <= ENDFUNCTIONS) return form;
        error2(form, PSTR("undefined"));
    }

    // It's a list
    object *function = car(form);
    object *args = cdr(form);
    if (!listp(args)) error(PSTR("Can't evaluate a dotted pair"));

    // List starts with a symbol?
    if (symbolp(function)) {
        symbol_t name = function->name;

        if ((name == LET) || (name == LETSTAR)) {
            int TCstart = TC;
            object *assigns = first(args);
            object *forms = cdr(args);
            object *newenv = env;
            push(newenv, GCStack);
            while (assigns != NULL) {
                object *assign = car(assigns);
                if (!consp(assign)) push(cons(assign, nil), newenv);
                else if (cdr(assign) == NULL) push(cons(first(assign), nil), newenv);
                else push(cons(first(assign), eval(second(assign), env)), newenv);
                car(GCStack) = newenv;
                if (name == LETSTAR) env = newenv;
                assigns = cdr(assigns);
            }
            env = newenv;
            pop(GCStack);
            form = tf_progn(forms, env);
            TC = TCstart;
            goto EVAL;
        }

        if (name == LAMBDA) {
            if (env == NULL) return form;
            object *envcopy = NULL;
            while (env != NULL) {
                object *pair = first(env);
                if (pair != NULL) {
                    object *val = cdr(pair);
                    if (integerp(val)) val = number(val->integer);
                    push(cons(car(pair), val), envcopy);
                }
                env = cdr(env);
            }
            return cons(symbol(CLOSURE), cons(envcopy, args));
        }

        if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
            return ((fn_ptr_type)lookupfn(name))(args, env);
        }

        if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
            form = ((fn_ptr_type)lookupfn(name))(args, env);
            TC = 1;
            goto EVAL;
        }
    }

    // Evaluate the parameters - result in head
    object *fname = car(form);
    int TCstart = TC;
    object *head = cons(eval(car(form), env), NULL);
    push(head, GCStack); // Don't GC the result list
    object *tail = head;
    form = cdr(form);
    int nargs = 0;

    while (form != NULL) {
        object *obj = cons(eval(car(form), env), NULL);
        cdr(tail) = obj;
        tail = obj;
        form = cdr(form);
        nargs++;
    }

    function = car(head);
    args = cdr(head);

    if (symbolp(function)) {
        symbol_t name = function->name;
        if (name >= ENDFUNCTIONS) error2(fname, PSTR("is not valid here"));
        if (nargs < lookupmin(name)) error2(fname, PSTR("has too few arguments"));
        if (nargs > lookupmax(name)) error2(fname, PSTR("has too many arguments"));
        object *result = ((fn_ptr_type)lookupfn(name))(args, env);
        pop(GCStack);
        return result;
    }

    if (listp(function) && issymbol(car(function), LAMBDA)) {
        form = closure(TCstart, fname, NULL, cdr(function), args, &env);
        pop(GCStack);
        int trace = tracing(fname->name);
        if (trace) {
            object *result = eval(form, env);
            indent((--(TraceDepth[trace - 1])) << 1, pserial);
            pint(TraceDepth[trace - 1], pserial);
            pserial(':'); pserial(' ');
            printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
            printobject(result, pserial); pln(pserial);
            return result;
        } else {
            TC = 1;
            goto EVAL;
        }
    }

    if (listp(function) && issymbol(car(function), CLOSURE)) {
        function = cdr(function);
        form = closure(TCstart, fname, car(function), cdr(function), args, &env);
        pop(GCStack);
        TC = 1;
        goto EVAL;
    }

    error2(fname, PSTR("is an illegal function")); return nil;
}

// Print functions

inline int maxbuffer (char *buffer) {
    return SYMBOLTABLESIZE - (buffer - SymbolTable) - 1;
}

void pserial (char c) {
    LastPrint = c;
    if (c == '\n') Serial.write('\r');
    Serial.write(c);
}

const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
                                    "Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

void pcharacter (char c, pfun_t pfun) {
    if (!PrintReadably) pfun(c);
    else {
        pfun('#'); pfun('\\');
        if (c > 32) pfun(c);
        else {
            const char *p = ControlCodes;
            while (c > 0) {
                p = p + strlen(p) + 1;
                c--;
            }
            pfstring(p, pfun);
        }
    }
}

void pstring (char *s, pfun_t pfun) {
    while (*s) pfun(*s++);
}

void printstring (object * form, pfun_t pfun) {
    if (PrintReadably) pfun('"');
    form = cdr(form);
    while (form != NULL) {
        int chars = form->integer;
        for (int i = (sizeof(int) - 1) * 8; i >= 0; i = i - 8) {
            char ch = chars >> i & 0xFF;
            if (PrintReadably && (ch == '"' || ch == '\\')) pfun('\\');
            if (ch) pfun(ch);
        }
        form = car(form);
    }
    if (PrintReadably) pfun('"');
}

void pfstring (const char *s, pfun_t pfun) {
    int p = 0;
    while (1) {
        char c = s[p++];
        if (c == 0) return;
        pfun(c);
    }
}

void pint (int i, pfun_t pfun) {
    int lead = 0;
#if INT_MAX == 32767
    int p = 10000;
#else
    int p = 1000000000;
#endif
    if (i < 0) pfun('-');
    for (int d = p; d > 0; d = d / 10) {
        int j = i / d;
        if (j != 0 || lead || d == 1) {
            pfun(abs(j) + '0');
            lead = 1;
        }
        i = i - j * d;
    }
}

void pmantissa (float f, pfun_t pfun) {
    int sig = floor(log10(f));
    int mul = pow(10, 5 - sig);
    int i = round(f * mul);
    boolean point = false;
    if (i == 1000000) {
        i = 100000;
        sig++;
    }
    if (sig < 0) {
        pfun('0'); pfun('.'); point = true;
        for (int j = 0; j < - sig - 1; j++) pfun('0');
    }
    mul = 100000;
    for (int j = 0; j < 7; j++) {
        int d = (int)(i / mul);
        pfun(d + '0');
        i = i - d * mul;
        if (i == 0) {
            if (!point) {
                pfun('.');
                pfun('0');
            } return;
        }
        if (j == sig && sig >= 0) {
            pfun('.');
            point = true;
        }
        mul = mul / 10;
    }
}

void pfloat (float f, pfun_t pfun) {
    if (isnan(f)) {
        pfstring(PSTR("NaN"), pfun);
        return;
    }
    if (f == 0.0) {
        pfun('0');
        return;
    }
    if (isinf(f)) {
        pfstring(PSTR("Inf"), pfun);
        return;
    }
    if (f < 0) {
        pfun('-');
        f = -f;
    }
    // Calculate exponent
    int e = 0;
    if (f < 1e-3 || f >= 1e5) {
        e = floor(log(f) / 2.302585); // log10 gives wrong result
        f = f / pow(10, e);
    }

    pmantissa (f, pfun);

    // Exponent
    if (e != 0) {
        pfun('e');
        pint(e, pfun);
    }
}

inline void pln (pfun_t pfun) {
    pfun('\n');
}

void pfl (pfun_t pfun) {
    if (LastPrint != '\n') pfun('\n');
}

void printobject (object * form, pfun_t pfun) {
    if (form == NULL) pfstring(PSTR("nil"), pfun);
    else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
    else if (listp(form)) {
        pfun('(');
        printobject(car(form), pfun);
        form = cdr(form);
        while (form != NULL && listp(form)) {
            pfun(' ');
            printobject(car(form), pfun);
            form = cdr(form);
        }
        if (form != NULL) {
            pfstring(PSTR(" . "), pfun);
            printobject(form, pfun);
        }
        pfun(')');
    } else if (integerp(form)) {
        pint(integer(form), pfun);
    } else if (floatp(form)) {
        pfloat(fromfloat(form), pfun);
    } else if (symbolp(form)) {
        if (form->name != NOTHING) pstring(name(form), pfun);
    } else if (characterp(form)) {
        pcharacter(form->integer, pfun);
    } else if (stringp(form)) {
        printstring(form, pfun);
    } else if (streamp(form)) {
        pfstring(PSTR("<"), pfun);
        if ((form->integer) >> 8 == SPISTREAM) pfstring(PSTR("spi"), pfun);
        else if ((form->integer) >> 8 == I2CSTREAM) pfstring(PSTR("i2c"), pfun);
        else if ((form->integer) >> 8 == SDSTREAM) pfstring(PSTR("sd"), pfun);
        else pfstring(PSTR("serial"), pfun);
        pfstring(PSTR("-stream "), pfun);
        pint(form->integer & 0xFF, pfun);
        pfun('>');
    } else
        error(PSTR("Error in print."));
}

// Read functions

int glibrary () {
    if (LastChar) {
        char temp = LastChar;
        LastChar = 0;
        return temp;
    }
    char c = LispLibrary[GlobalStringIndex++];
    return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object * env) {
    GlobalStringIndex = 0;
    object *line = read(glibrary);
    while (line != NULL) {
        eval(line, env);
        line = read(glibrary);
    }
}

int gserial () {
    if (LastChar) {
        char temp = LastChar;
        LastChar = 0;
        return temp;
    }
    while (!Serial.available());
    char temp = Serial.read();
    if (temp != '\n') pserial(temp);
    return temp;
}

object *nextitem (gfun_t gfun) {
    int ch = gfun();
    while (isspace(ch)) ch = gfun();

    if (ch == ';') {
        while (ch != '(') ch = gfun();
        ch = '(';
    }
    if (ch == '\n') ch = gfun();
    if (ch == -1) return nil;
    if (ch == ')') return (object *)KET;
    if (ch == '(') return (object *)BRA;
    if (ch == '\'') return (object *)QUO;

    // Parse string
    if (ch == '"') return readstring('"', gfun);

    // Parse symbol, character, or number
    int index = 0, base = 10, sign = 1;
    char *buffer = SymbolTop;
    int bufmax = maxbuffer(buffer); // Max index
    unsigned int result = 0;
    boolean isfloat = false;
    float fresult = 0.0;

    if (ch == '+') {
        buffer[index++] = ch;
        ch = gfun();
    } else if (ch == '-') {
        sign = -1;
        buffer[index++] = ch;
        ch = gfun();
    } else if (ch == '.') {
        buffer[index++] = ch;
        ch = gfun();
        if (ch == ' ') return (object *)DOT;
        isfloat = true;
    } else if (ch == '#') {
        ch = gfun() & ~0x20;
        if (ch == '\\') base = 0; // character
        else if (ch == 'B') base = 2;
        else if (ch == 'O') base = 8;
        else if (ch == 'X') base = 16;
        else if (ch == 0x07); // Ignore '
        else error(PSTR("Illegal character after #"));
        ch = gfun();
    }
    int valid; // 0=undecided, -1=invalid, +1=valid
    if (ch == '.') valid = 0; else if (digitvalue(ch) < base) valid = 1; else valid = -1;
    boolean isexponent = false;
    int exponent = 0, esign = 1;
    buffer[2] = '\0'; // In case symbol is one letter
    float divisor = 10.0;

    while (!isspace(ch) && ch != ')' && ch != '(' && index < bufmax) {
        buffer[index++] = ch;
        if (base == 10 && ch == '.' && !isexponent) {
            isfloat = true;
            fresult = result;
        } else if (base == 10 && (ch == 'e' || ch == 'E')) {
            if (!isfloat) {
                isfloat = true;
                fresult = result;
            }
            isexponent = true;
            if (valid == 1) valid = 0; else valid = -1;
        } else if (isexponent && ch == '-') {
            esign = -esign;
        } else if (isexponent && ch == '+') {
        } else {
            int digit = digitvalue(ch);
            if (digitvalue(ch) < base && valid != -1) valid = 1; else valid = -1;
            if (isexponent) {
                exponent = exponent * 10 + digit;
            } else if (isfloat) {
                fresult = fresult + digit / divisor;
                divisor = divisor * 10.0;
            } else {
                result = result * base + digit;
            }
        }
        ch = gfun();
    }

    buffer[index] = '\0';
    if (ch == ')' || ch == '(') LastChar = ch;
    if (isfloat && valid == 1) return makefloat(fresult * sign * pow(10, exponent * esign));
    else if (valid == 1) {
        if (base == 10 && result > ((unsigned int)INT_MAX + (1 - sign) / 2))
            return makefloat((float)result * sign);
        return number(result * sign);
    } else if (base == 0) {
        if (index == 1) return character(buffer[0]);
        const char* p = ControlCodes; char c = 0;
        while (c < 33) {
            if (strcasecmp(buffer, p) == 0) return character(c);
            p = p + strlen(p) + 1; c++;
        }
        error(PSTR("Unknown character"));
    }

    int x = builtin(buffer);
    if (x == NIL) return nil;
    if (x < ENDFUNCTIONS) return newsymbol(x);
    else if (index < 4 && valid40(buffer)) return newsymbol(pack40(buffer));
    else return newsymbol(longsymbol(buffer));
}

object *readrest (gfun_t gfun) {
    object *item = nextitem(gfun);
    object *head = NULL;
    object *tail = NULL;

    while (item != (object *)KET) {
        if (item == (object *)BRA) {
            item = readrest(gfun);
        } else if (item == (object *)QUO) {
            item = cons(symbol(QUOTE), cons(read(gfun), NULL));
        } else if (item == (object *)DOT) {
            tail->cdr = read(gfun);
            if (readrest(gfun) != NULL) error(PSTR("Malformed list"));
            return head;
        } else {
            object *cell = cons(item, NULL);
            if (head == NULL) head = cell;
            else tail->cdr = cell;
            tail = cell;
            item = nextitem(gfun);
        }
    }
    return head;
}

object *read (gfun_t gfun) {
    object *item = nextitem(gfun);
    if (item == (object *)KET) error(PSTR("Incomplete list"));
    if (item == (object *)BRA) return readrest(gfun);
    if (item == (object *)DOT) return read(gfun);
    if (item == (object *)QUO) return cons(symbol(QUOTE), cons(read(gfun), NULL));
    return item;
}

// Kaef: BEG Block
#if (defined SD_CARD_DEBUG) && (defined ESP32) /* Kaef */
void listDir(const char * dirname, uint8_t levels) {
    Serial.printf("Listing directory: %s\n", dirname);

    File root = SD.open(dirname);
    if (!root) {
        Serial.println("Failed to open directory");
        return;
    }
    if (!root.isDirectory()) {
        Serial.println("Not a directory");
        return;
    }

    File file = root.openNextFile();
    while (file) {
        if (file.isDirectory()) {
            Serial.print("  DIR : ");
            Serial.println(file.name());
            if (levels) {
                listDir(file.name(), levels - 1);
            }
        } else {
            Serial.print("  FILE: ");
            Serial.print(file.name());
            Serial.print("  SIZE: ");
            Serial.println(file.size());
        }
        file = root.openNextFile();
    }
    Serial.println("done");
}

void sd_test () {
    mySPIbegin(SDCARD_SS_PIN);
    Serial.print("SDCARD_SS_PIN = "); Serial.println(SDCARD_SS_PIN);

    if (!SD.begin(SDCARD_SS_PIN)) {
        Serial.println("Card Mount Failed!");
        return;
    }
    uint64_t cardSize = SD.cardSize() / (1024 * 1024);
    Serial.printf("SD Card Size: %lluMB\n", cardSize);

    listDir("/", 5);
}
#endif

// Kaef:
void printEnabledFeatures () {
    pln(pserial); pfstring(PSTR("  Features:"), pserial);
#if (defined resetautorun)
    pfstring(PSTR(" resetautorun"), pserial);
#endif
#if (defined printfreespace)
    pfstring(PSTR(" printfreespace"), pserial);
#endif
#if (defined serialmonitor)
    pfstring(PSTR(" serialmonitor"), pserial);
#endif
#if (defined printgcs)
    pfstring(PSTR(" printgcs"), pserial);
#endif
#if (defined sdcardsupport)
    pfstring(PSTR(" sdcardsupport"), pserial);
#endif
#if (defined SD_CARD_DEBUG)
    pfstring(PSTR(" SD_CARD_DEBUG"), pserial);
#endif
#if (defined lisplibrary)
    pfstring(PSTR(" lisplibrary"), pserial);
#endif
#if defined LARGE_WORKSPACE
    pfstring(PSTR(" LARGE_WORKSPACE_SETUP"), pserial);
#endif
    pln(pserial);
}


void printFreeHeap () {
#ifdef ESP32
    pfstring(PSTR("  Free Heap: "), pserial); pint(esp_get_free_heap_size(), pserial);
#endif
    pln(pserial);
}

void welcomeMessage () {
    pln(pserial); pfstring(PSTR("uLisp 2.5b -- forked and extended by Kaef (https://github.com/kaef)"), pserial); pln(pserial);
    pfstring(PSTR("(c) by David Johnson-Davies - www.technoblogy.com"), pserial); pln(pserial);
    pfstring(PSTR("Licensed under the MIT license: https://opensource.org/licenses/MIT"), pserial); pln(pserial);
    pln(pserial); pfstring(PSTR("System information:"), pserial); pln(pserial);
#ifdef ESP32
    pfstring(PSTR("  reset reason: "), pserial); pint(rtc_get_reset_reason(0), pserial);
    pfstring(PSTR("  wakeup cause: "), pserial); pint(esp_sleep_get_wakeup_cause(), pserial); pln(pserial);
#endif
    pfstring(PSTR("  compiled: "), pserial);
    pfstring(PSTR(__DATE__), pserial); pfstring(PSTR(" "), pserial);
    pfstring(PSTR(__TIME__), pserial);
    printEnabledFeatures();
    //printFreeHeap();
    // Kaef: SD Card test
#if (defined SD_CARD_DEBUG) && (defined ESP32)
    sd_test();
#endif
    printFreeHeap();

#ifdef resetautorun
    pln(pserial);
    pfstring(PSTR("Press Btn0 to suppress restautorun..."), pserial); pln(pserial);
    // give user the chance to press Btn0
    while (millis() < 1000) yield();
#endif
    pln(pserial);
}
// Kaef: END Block


// Setup

void initenv () {
    GlobalEnv = NULL;
    tee = symbol(TEE);
}

void setup () {
    Serial.begin(115200); // Kaef
    int start = millis();
    while (millis() - start < 5000) {
        if (Serial) break;
    }
    welcomeMessage(); // Kaef
    initworkspace();
    initenv();
    initsleep();
    // Kaef    pfstring(PSTR("uLisp 2.5b "), pserial); pln(pserial);
}

// Read/Evaluate/Print loop

void repl (object * env) {
    for (;;) {
        randomSeed(micros());

        // Kaef: BEG Block
#if !(defined LARGE_WORKSPACE) /* Kaef: large workspace patch */
        gc(NULL, env); // Kaef: speed patch for large workspaces
#endif
        // gc(NULL, env);
        // Kaef: END Block

#if defined (printfreespace)
        pint(Freespace, pserial);
#endif
        if (BreakLevel) {
            pfstring(PSTR(" : "), pserial);
            pint(BreakLevel, pserial);
        }
        pfstring(PSTR("> "), pserial);
        object *line = read(gserial);
        if (BreakLevel && line == nil) {
            pln(pserial);
            return;
        }
        if (line == (object *)KET) error(PSTR("Unmatched right bracket"));
        push(line, GCStack);
        pfl(pserial);
        line = eval(line, env);
        pfl(pserial);
        printobject(line, pserial);
        pop(GCStack);
        pfl(pserial);
        pln(pserial);
    }
}

void loop () {
    End = 0xA5;      // Canary to check stack
    if (!setjmp(exception)) {
#if defined(resetautorun)
        volatile int autorun = 12; // Fudge to keep code size the same
#else
        volatile int autorun = 13;
#endif
        if (autorun == 12) autorunimage();
    }
    // Come here after error
    delay(100); while (Serial.available()) Serial.read();
    for (int i = 0; i < TRACEMAX; i++) TraceDepth[i] = 0;
#if defined(sdcardsupport)
    SDpfile.close(); SDgfile.close();
#endif
#if defined(lisplibrary)
    if (!tstflag(LIBRARYLOADED)) {
        setflag(LIBRARYLOADED);
        loadfromlibrary(NULL);
    }
#endif
    client.stop();
    repl(NULL);
}
