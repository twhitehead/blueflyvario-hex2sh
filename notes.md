# Hex file

:<len><addr1><addr0><cmd><data><sum>

## Commands

0 - data
1 - end of file
4 - upper addres (<addr3><addr2>)

# PIC memory view

Word size is 16 bits

Program memory can be conceptualized as two parallel 16 bit memories
with byte addressable blocks (8 bit increments) giving the upper and
lower program words with the upper byte of the upper word always 0.

- PC address both of these simultaneously
- mapping maps a portion of the lower program memory into data memory
- upper and lower table instructions provide direct access

Program memory

- read only (can be written using table instructions)
- aligned word accessed from both memories simultaneously only
- addressed via 23 bit PC with lower bit 0 (aligned lower half only)

Data memory

- read/write (write depends on which section of memory)
- byte or word access (word access have to be aligned)
- addressed via 16 bit EA

Table instructions to access program memory

- read/write
- separate instructions for upper and lower memories
- byte or word access (word access have to be aligned)
- address generated with 8 bit TBLPAG + 16 bit EA
- can access to full 24 bit range

Mapping of lower program memory into upper data memory

- read only
- needs to be enabled via CORCON
- byte or word access (word access have to be aligned)
- address generated with 8 bit PSVGA + 15 bit EA
- can only access the lower 23 bit range


# Flash memory programming

- write in blocks of 32 instructions
- erase memory in blocks of 32, 64, or 128 instructions
- NVMOP bits determine erase block size
- NVCOM controls which blocks are erased
- NVKEY provides safey (have to toggle bits to perform erasure)

# ds30loader

## Initial communication

Send hello

  hello = 0xc1

Recieve device info and 'K'

  d7 d6 d5 d4 d3 d2 d1 d0 - d9..d0 - device id (35 for p24f32ka301)
  d8 m6 m5 m4 m3 m2 m1 m0 - m6..m0 . n2..n0 . r3..r0
  d9 n2 n1 n0 r3 r2 r1 r0

  'K'

## Command loop

Send command

  command = address command count data checksum

  address = a2 a1 a0
  command = e0
    0x00 - erase page
    0x01 - write row
    0x02 - write eerom
    0x03 - write config
  count = c0
  data = d3 d0 d1  d3 d0 d1  d3 d0 d1 ... (total bytes = c0)
  checksum = s0

Recieve response

  'K' - Okay
  'N' - Checksum error
  'V' - Verification failure
  'P' - Boot loader protection
  'U' - Unknown command

## Bootloader settings

.equ __24F32KA301, 1      ;xxx uncomment and change if using PIC24F device

.equiv IS_24F, 1
.equiv VALID_DEV,  1
.equiv FLASHSIZE,  0x5800
.equiv RAM_START,  0x800
.equiv RAM_SIZEB,  2048
.equiv DEVICEID,   35
.equiv EESIZEB,    512
.equiv HAS_UART1,  1
.equiv HAS_UART2,  1
.equiv FL_ER_CODE, 0x4058
.equiv FL_WR_CODE, 0x4004
.equiv EE_ER_CODE, 0x4058
.equiv EE_WR_CODE, 0x4004

.equiv PAGESIZER,  1       /* pagesize [rows] */
.equiv ROWSIZEW,   32      /* rowsize [words] */
.equiv FCY,        8000000 ;xxx speed of internal cycle clock[Hz], used to calculate uart brg and delay
.equiv BLINIT,     60000   ;xxx hello receive timeout [ms]
.equiv HELLOTRIES, 2       ;xxx number of non hello characters received before branching to the user application
.equiv BLTIME,     3000    ;xxx data receive timeout [ms]
.equiv USE_UART2,  1       ;xxx uncomment to use uart2
.equiv WRITE_VER,  1       ;xxx do flash write verification
.equiv EWRITE_VER, 1       ;xxx do eeprom write verification
.equiv PROT_GOTO,  1       ;protect goto at 0x00
.equiv PROT_BL,    1       ;protect bootloader
.equiv BLPLP,      7       ;bootloader placement, pages from end
.equiv BLSIZEP,    7       ;bootloader size [pages], used by bootloader protection

config __FBS,     BSS_OFF /*Boot Segment Code Protect*/ & BWRP_OFF /*Boot Segment Write Protect Enable*/
config __FGS,     GSS0_OFF /*General Segment Code Protect*/ & GWRP_OFF /*General Segment Write Protect Enable*/
config __FOSCSEL, FNOSC_FRCPLL /*Initial Oscillator Select*/ & IESO_OFF /*Two Speed Start-up*/ & SOSCSRC_DIG & LPRCSEL_HP
config __FOSC,    FCKSM_CSDCMD /*Clock switching and Fail-Safe Clock monitor*/ & SOSCSEL_SOSCHP /*Secondary Oscillator Select*/ & POSCFREQ_HS /*Primary Oscillator Frequency Range*/ & OSCIOFNC_OFF /*OSCO Pin Configuration*/ & POSCMOD_NONE /*Oscillator Selection*/
config __FWDT,    FWDTEN_OFF /*Watchdog Timer*/ & WINDIS_OFF /*Windowed WDT*/ & FWPSA_PR128 /*Watchdog prescaler*/ & WDTPS_PS32768 /*Watchdog postscale*/
config __FPOR,    MCLRE_ON /*Master Clear Enable*/ & BORV_V18 /*Brown Out Voltage*/ & I2C1SEL_PRI /*I2C1 pins Select*/ & PWRTEN_ON /*Power Up Timer*/ & BOREN_BOR3 /*Brown Out Reset*/ & LVRCFG_OFF
config __FICD,    ICS_PGx2 /*ICD pins select*/
config __FDS,     DSWDTEN_OFF /*Deep Sleep Watchdog Timer*/ & DSBOREN_OFF /*Deep Sleep BOR*/ & DSWDTOSC_LPRC /*Deep Sleep Watchdog Oscillator Clock Select*/ & DSWDTPS_DSWDTPSF /*Deep Sleep Watchdog Postscale*/

.equiv VERMAJ, 4 /*firmware version major*/
.equiv VERMIN, 0 /*fimrware version minor*/
.equiv VERREV, 3 /*firmware version revision*/

.equiv HELLO,       0xC1
.equiv OK,          'K'  /* erase/write ok */
.equiv CHECKSUMERR, 'N'  /* checksum error */
.equiv VERFAIL,     'V'  /* verification failed */
.equiv BLPROT,      'P'  /* bl protection tripped */
.equiv UCMD,        'U'  /* unknown command */

.equiv PWOK, 0xFE

.equiv BLSTART, ( BLINIT * (FCY / 1000) / (65536 * 7) ) /* count for boot receive delay */
.equiv BLDELAY, ( BLTIME * (FCY / 1000) / (65536 * 7) ) /* count for receive delay */

.equiv STARTADDR,  ( FLASHSIZE - BLPLP * PAGESIZER * ROWSIZEW * 2 )     /* bootloader placement */
.equiv BLSTARTROW, (STARTADDR / ROWSIZEW / 2)
.equiv BLENDROW,   (STARTADDR / ROWSIZEW / 2 + (BLSIZEP*PAGESIZER) - 1)
