#include<ADUC841.H>
;********************************************************************
; Gil Ya'akov
; Date : 03/12/2020
; File: Pulse_measurement.asm
; Hardware : Any 8052 based MicroConverter (ADuC8xx)
; Description : This program measure the number of pulses
; appearing on P3.4 in each second. The program also causes
; the microcontroller to "upload" the number to the host using
; the UART.
; Setting: Baud rate = 115,200.
;********************************************************************
PIN EQU P3.4
CSEG AT 0000H ; When "wake up" jump to START.
     JMP START
CSEG AT 0023H 
    JMP UART_ISR ; UART ISR
CSEG AT 002BH
    JMP TR2_ISR ; Timer 2 ISR
CSEG AT 0100H
  UART_ISR:
     JBC TI, TRANSMIT ; Jump TRANSMIT label and clear TI if TI=1 (there was a transmit interrupt).
     TRANSMIT:
	    PUSH ACC
        MOV A, @R1 ; Move the next byte to the accumulator.
        JZ END_TRANMITS ; Check if we are at the end of the string ('0' = null terminator).
        MOV SBUF, A ; Output the word to the UART.
        INC R1 ; Move DPTR to the next word in the string.

 END_TRANMITS:
        POP ACC
        RETI ; Return from the interrupt.

CSEG AT 0200H
   TR2_ISR:
           INC R0
		   CPL P3.4 ;TEST
   		   CJNE R0,#225d,NOT_SECOND ; Jump to label if 1 sec don't passed
		   MOV R0,#0d ; CLR R0 for counting from start
		   SETB SECOND ; Turn on the flag to let the main know that 1 seconde have passed 
NOT_SECOND:
           CLR TF2       
		   RETI

CSEG AT 0300H
    START:
        MOV SP, #50H ; Set SP to 50H address.
		MOV R0,#0 ; Will use to count 225d
		MOV R1,#0 ; Will use as a pointer to #COUNTER_STRING
		MOV R2,#0 ; Will use to set #COUNTER_STRING
		MOV R3,#0 ; Will use to set #COUNTER_STRING
		MOV R4,#0 ; Will use to set #COUNTER_STRING
		MOV R5,#0 ; Will use to set #COUNTER_STRING
		;UART config
		CLR SM0
		SETB SM1 ; Set MODE 1: 8-bit UART variable baud rate.
		SETB REN ; Enable receiving.
        ;Timer 3 config
		MOV T3FD, #32d ; Set T3FD to 32d value
		ORL T3CON, #10000010b ; Turn on bit 1 & 7 in T3CON.
		ANL T3CON, #11111010b ; Turn off bit 0 & 2 in T3CON - DIV = 2d.
		;Timer 2 config
        CLR CNT2
        CLR CAP2
        MOV RCAP2H, #028H ; Cause Timer 2 to interrupt every 5 ms.
        MOV RCAP2L, #000H
        MOV TH2, #028H
        MOV TL2, #000H
        ;Timer 0 config
		ORL TMOD, #00000101b ; Turn on bit 0 & 2 in TMOD.
		ANL TMOD, #11110101b ; Turn off bit 1 & 3 in TMOD.
		MOV TH0, #0d
        MOV TL0, #0d
		CLR SECOND ; CLR the flag
		SETB TR0
		SETB TR2
		SETB ET2 ; Enable Timer 2 interrupt.
        SETB ES ; Enable the serial interrupt.
        SETB EA ; Enable interrupts globally.
      L:JNB SECOND,$ ; Sit here & wait that 1 second will pass 
	    CLR SECOND ; CLR the flag
		CALL SEND_COUNTER ; Call subroutine
		JMP L
		
SEND_COUNTER:
             MOV R2, TL0 ; Set R2 to the value of TL0
			 MOV R3, TH0 ; Set R3 to the value of TH0
			 MOV R4, TL0 ; Set R4 to the value of TL0
			 MOV R5, TH0 ; Set R5 to the value of TH0
			 MOV TL0, #0 ; Initialize TL0 to start from 0
			 MOV TH0, #0 ; Initialize TH0 to start from 0
			 ;30H Value
			 MOV A, R2
			 ANL A, #00001111b ; Keep the 4 lower bit of TL0
			 ;HEXA - ASCII convert
			 CJNE A,#9d,BIGGER1
			 ADD A,#30H
			 JMP NEXT1
	 BIGGER1:JC SMALLER1
             ADD A,#37H
			 JMP NEXT1
    SMALLER1:ADD A,#30H
	   NEXT1:		 
			 ;END HEXA - ASCII convert
			 MOV R2, A
			 ;32H Value
			 MOV A, R3
			 ANL A, #00001111b ; Keep the 4 lower bit of TH0
			 ;HEXA - ASCII convert
			 CJNE A,#9d,BIGGER2
			 ADD A,#30H
			 JMP NEXT2
	 BIGGER2:JC SMALLER2
             ADD A,#37H
			 JMP NEXT2
    SMALLER2:ADD A,#30H
	   NEXT2:		 
			 ;END HEXA - ASCII convert
			 MOV R3, A
			 ;31H Value
			 MOV A, R4
			 RR A
			 RR A
             RR A
			 RR A
			 ANL A, #00001111b ; Keep the 4 high bit of TL0
			 ;HEXA - ASCII convert
			 CJNE A,#9d,BIGGER3
			 ADD A,#30H
			 JMP NEXT3
	 BIGGER3:JC SMALLER3
             ADD A,#37H
			 JMP NEXT3
    SMALLER3:ADD A,#30H
	   NEXT3:		 
			 ;END HEXA - ASCII convert
			 MOV R4, A
			 ;33H Value
			 MOV A, R5
			 RR A
			 RR A
             RR A
			 RR A
			 ANL A, #00001111b ; Keep the 4 high bit of TH0
			 ;HEXA - ASCII convert
			 CJNE A,#9d,BIGGER4
			 ADD A,#30H
			 JMP NEXT4
	 BIGGER4:JC SMALLER4
             ADD A,#37H
			 JMP NEXT4
    SMALLER4:ADD A,#30H
	   NEXT4:		 
			 ;END HEXA - ASCII convert
			 MOV R5, A
			 ; Build COUNTER_STRING
             MOV COUNTER_STRING, R5
             MOV COUNTER_STRING+1, R3
             MOV COUNTER_STRING+2, R4			 
             MOV COUNTER_STRING+3, R2
			 MOV COUNTER_STRING+4, #13
			 MOV COUNTER_STRING+5, #10
			 MOV COUNTER_STRING+6, #0
             MOV R1,#COUNTER_STRING
             SETB TI
             RET
		
DSEG AT 0030H
COUNTER_STRING: DS 7 
BSEG
SECOND: DBIT 1
END