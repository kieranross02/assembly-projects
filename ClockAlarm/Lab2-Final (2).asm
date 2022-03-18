; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P1.1
MINUS_BUTTON equ P0.0
PLUS_BUTTON equ P0.3
TIME_BUTTON equ P0.6
SET_BUTTON equ P2.4
SNOOZE_BUTTON equ P2.2

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
SECONDS:  ds 1
MINUTES: ds 1
HOURS: ds 1
ALARM_MINUTES: ds 1
ALARM_HOURS: ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
pm_flag: dbit 1 ; keep track of if AM or PM <1 - PM, 0 - AM>
alarm_pm_flag: dbit 1 ; ^^^
ta_flag: dbit 1 ; 1 - editing timer, 0 - editing alarm
time_flag: dbit 1 ; 1 - mins, 0 - hours
alarm_flag: dbit 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Timer:xx:xx:xxPM', 0
Alarm_Message:  db 'Alarm:xx:xxPM', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	jnb alarm_flag, Done
	cpl SOUND_OUT ; Connect speaker to P1.1!
Done:
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the seconds
	
	mov a, SECONDS
    add a, #0x01
    da a 
    mov SECONDS, a
    cjne a, #0x60, Timer2_ISR_done
    mov SECONDS, #0x00

    ; Increment the minutes
    mov a, MINUTES
    add a, #0x01
    da a 
    mov MINUTES, a
    cjne a, #0x60, Timer2_ISR_done
    mov MINUTES, #0x00

    ; Increment the hours
    mov a, HOURS
    add a, #0x01
    da a 
    mov HOURS, a
    cjne a, #0x12, NO_AMPM_CHECK
    Set_Cursor(1,15)
    jnb pm_flag, PM_SET
    Display_char(#'A')
    clr pm_flag
    sjmp NO_AMPM_CHECK
    
PM_SET:
	Display_char(#'P')
	setb pm_flag
	
    
NO_AMPM_CHECK:
    cjne a, #0x13, Timer2_ISR_done
    mov HOURS, #0x01
    
;Timer2_ISR_decrement:
	;add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
;Timer2_ISR_da:
	;da a ; Decimal adjust instruction.  Check datasheet for more details!
	;mov BCD_counter, a
	
Timer2_ISR_done:
	
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#Alarm_Message)
    setb half_seconds_flag
    setb pm_flag
    setb alarm_pm_flag
    setb ta_flag
    setb time_flag
    clr alarm_flag
    mov SECONDS, #0x50
    mov MINUTES, #0x58
    mov HOURS, #0x11
	mov ALARM_MINUTES, #0x59
	mov ALARM_HOURS, #0x11
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the SECONDS
	mov SECONDS, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
loop_a:
	jnb half_seconds_flag, loop
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
    Display_BCD(HOURS) ; This macro is also in 'LCD_4bit.inc'

    Set_Cursor(1, 10)     ; the place in the LCD where we want the BCD counter value
    Display_BCD(MINUTES) ; This macro is also in 'LCD_4bit.inc'

    Set_Cursor(1, 13)     ; the place in the LCD where we want the BCD counter value
    Display_BCD(SECONDS)
    
    Set_Cursor(2, 7)     ; the place in the LCD where we want the BCD counter value
    Display_BCD(ALARM_HOURS) ; This macro is also in 'LCD_4bit.inc'

    Set_Cursor(2, 10)     ; the place in the LCD where we want the BCD counter value
    Display_BCD(ALARM_MINUTES) ; This macro is also in 'LCD_4bit.inc'
	
	mov a, ALARM_MINUTES
    cjne a, MINUTES, endloop_b
    mov a, ALARM_HOURS
	cjne a, HOURS, endloop_b
	mov a, alarm_pm_flag
	cjne a, pm_flag, endloop_b
	setb alarm_flag
endloop_b:
    ljmp loop_c
loop_c:
	jb SET_BUTTON, loop_d  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SET_BUTTON, loop_d  ; if the 'BOOT' button is not pressed skip
	jnb SET_BUTTON, $
	jb ta_flag, Turn_Off
	setb ta_flag
	ljmp loop_d
Turn_Off:
	clr ta_flag
loop_d:
	jb PLUS_BUTTON, leaveloop_d  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PLUS_BUTTON, leaveloop_d  ; if the 'BOOT' button is not pressed skip
	jnb PLUS_BUTTON, $
	jb ta_flag, ALARM
	jnb time_flag, MINTIM
	ljmp HOURTIM
ALARM:
	jnb time_flag, MINALARM
	ljmp HOURALARM
MINTIM:
	mov a, MINUTES
    add a, #0x01
    da a 
    mov MINUTES, a
    cjne a, #0x60, endloop_d
    mov MINUTES, #0x00
HOURTIM:
	mov a, HOURS
    add a, #0x01
    da a 
    mov HOURS, a
    cjne a, #0x12, NO_AMPM_CHECK1
    Set_Cursor(1,15)
    jnb pm_flag, PM_SET1
    Display_char(#'A')
    clr pm_flag
    ljmp NO_AMPM_CHECK1
PM_SET1:
	Display_char(#'P')
	setb pm_flag
NO_AMPM_CHECK1:
    cjne a, #0x13, endloop_d
    mov HOURS, #0x01
leaveloop_d:
	ljmp endloop_d
MINALARM:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, endloop_d
    mov ALARM_MINUTES, #0x00
HOURALARM:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NO_AMPM_CHECK2
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PM_SET2
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NO_AMPM_CHECK2
PM_SET2:
	Display_char(#'P')
	setb alarm_pm_flag
NO_AMPM_CHECK2:
    cjne a, #0x13, endloop_d
    mov ALARM_HOURS, #0x01
endloop_d:
loop_e:
	jb TIME_BUTTON, loop_f  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb TIME_BUTTON, loop_f  ; if the 'BOOT' button is not pressed skip
	jnb TIME_BUTTON, $
	jb time_flag, Turn_Off1
	setb time_flag
	ljmp loop_f
Turn_Off1:
	clr time_flag
loop_f:
	jb MINUS_BUTTON, leaveloop_f  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MINUS_BUTTON, leaveloop_f  ; if the 'BOOT' button is not pressed skip
	jnb MINUS_BUTTON, $
	jb ta_flag, ALARMm
	jnb time_flag, MINTIMm
	ljmp HOURTIMm
ALARMm:
	jnb time_flag, MINALARMm
	ljmp HOURALARMm
MINTIMm:
	mov a, MINUTES
    add a, #0x99
    da a 
    mov MINUTES, a
    cjne a, #0x99, endloop_f
    mov MINUTES, #0x59
HOURTIMm:
	mov a, HOURS
    add a, #0x99
    da a 
    mov HOURS, a
    cjne a, #0x11, NO_AMPM_CHECK1m
    Set_Cursor(1,15)
    jnb pm_flag, PM_SET1m
    Display_char(#'A')
    clr pm_flag
    ljmp NO_AMPM_CHECK1m
PM_SET1m:
	Display_char(#'P')
	setb pm_flag
NO_AMPM_CHECK1m:
    cjne a, #0x00, endloop_f
    mov HOURS, #0x12
leaveloop_f:
	ljmp endloop_f
MINALARMm:
	mov a, ALARM_MINUTES
    add a, #0x99
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x99, endloop_f
    mov ALARM_MINUTES, #0x59
HOURALARMm:
	mov a, ALARM_HOURS
    add a, #0x99
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x11, NO_AMPM_CHECK2m
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PM_SET2m
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NO_AMPM_CHECK2m
PM_SET2m:
	Display_char(#'P')
	setb alarm_pm_flag
NO_AMPM_CHECK2m:
    cjne a, #0x00, endloop_f
    mov ALARM_HOURS, #0x12
endloop_f:
loop_g:
	jb SNOOZE_BUTTON, LeaveSnooze  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SNOOZE_BUTTON, LeaveSnooze  ; if the 'BOOT' button is not pressed skip
	jnb SNOOZE_BUTTON, $
	jnb alarm_flag, LeaveSnooze
	clr alarm_flag
	ljmp MinUp1
LeaveSnooze:
	ljmp DoneSnooze
MinUp1:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, MinUp2
    mov ALARM_MINUTES, #0x00
HourCheck1:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NoCheck1
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PMCheck1
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NoCheck1
PMCheck1:
	Display_char(#'P')
	setb alarm_pm_flag
NoCheck1:
    cjne a, #0x13, MinUp2
    mov ALARM_HOURS, #0x01
MinUp2:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, MinUp3
    mov ALARM_MINUTES, #0x00
HourCheck2:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NoCheck2
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PMCheck2
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NoCheck2
PMCheck2:
	Display_char(#'P')
	setb alarm_pm_flag
NoCheck2:
    cjne a, #0x13, MinUp3
    mov ALARM_HOURS, #0x01
MinUp3:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, MinUp4
    mov ALARM_MINUTES, #0x00
HourCheck3:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NoCheck3
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PMCheck3
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NoCheck3
PMCheck3:
	Display_char(#'P')
	setb alarm_pm_flag
NoCheck3:
    cjne a, #0x13, MinUp4
    mov ALARM_HOURS, #0x01
MinUp4:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, MinUp5
    mov ALARM_MINUTES, #0x00
HourCheck4:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NoCheck4
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PMCheck4
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NoCheck4
PMCheck4:
	Display_char(#'P')
	setb alarm_pm_flag
NoCheck4:
    cjne a, #0x13, MinUp5
    mov ALARM_HOURS, #0x01
MinUp5:
	mov a, ALARM_MINUTES
    add a, #0x01
    da a 
    mov ALARM_MINUTES, a
    cjne a, #0x60, DoneSnooze
    mov ALARM_MINUTES, #0x00
HourCheck5:
	mov a, ALARM_HOURS
    add a, #0x01
    da a 
    mov ALARM_HOURS, a
    cjne a, #0x12, NoCheck5
    Set_Cursor(2,12)
    jnb alarm_pm_flag, PMCheck5
    Display_char(#'A')
    clr alarm_pm_flag
    ljmp NoCheck5
PMCheck5:
	Display_char(#'P')
	setb alarm_pm_flag
NoCheck5:
    cjne a, #0x13, DoneSnooze
    mov ALARM_HOURS, #0x01
DoneSnooze:
	ljmp loop
END
