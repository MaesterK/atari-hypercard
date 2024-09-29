        processor 6502
        include vcs.h


;konstanten und variablen

;grafikpufferbereich fuer anzuzeigenden text im ram..wird ueber stackpointer gefuellt
textcol5 equ $80
textcol4 equ $85
textcol3 equ $8a
textcol2 equ $8f
textcol1 equ $94
textcol0 equ $99
textcolz equ $9e ;stackpointer muss auf "eins darueber" liegendes element gesetzt werden


xbuff equ $a0
ybuff equ $a1
abuff equ $a2
curseltextindex  equ $a3
curselindex equ $a4
gameselection_low equ $a5
gameselection_high equ $a6
textline equ $a7
spbuff equ $a8
; equ $a9
tempx equ $aa
grafbuff equ $ab
curtextpos equ $ac
curstackpos equ $ad
lastiostate equ $ae
starttextline equ $af

lowerascii0 equ lowerfont0-32 ;TODO so machen statt mit dem aktuellen gewurschtel!
GAMECOUNT equ 15
GAMEVIEWCOUNT equ 8

;geklautes sleep-makro
  MAC SLEEP
    IF {1} = 1
      ECHO "ERROR: SLEEP 1 not allowed !"
      END
    ENDIF
    IF {1} & 1
      nop $00
      REPEAT ({1}-3)/2
        nop
      REPEND
    ELSE
      REPEAT ({1})/2
        nop
      REPEND
    ENDIF
  ENDM

;makro zum speicher und register initialiseren..stellt ausserdem sicher dass interrupts und dezimal-modus aus sind
	MAC INITSYSTEM
	sei ;interrupts aus (disable an)
    cld ; bcd-modus aus
	
	;register alle auf null
	lda #0
	tay
	tax
	;.. und dann durch tia und ram (0000-00ff) iterieren und alles auf 0 setzen
.clear
	sta 0,x
	inx
	bne .clear
	ENDM



;beginn programm
    org $F000


Start

	INITSYSTEM ;sichere arbeitsverhaeltnisse herstellen

;variablen und register initialisieren

		lda <#gameselectionarea ;unteres byte adresse selektionsbereich
		sta gameselection_low
		lda >#gameselectionarea ;oberes byte selektionsbereich
		sta gameselection_high
		
        lda #$9C
		sta COLUBK
		lda #$00
        sta COLUP0
 ;       lda #$99
        sta COLUP1
        lda #255
        sta PF0
        sta PF1
        lda #0
        sta PF2
		sta starttextline

		sta curseltextindex
		sta curselindex
		;joystickports als output
		sta SWACNT
		lda #$FF
		sta lastiostate
		
        lda #%00000101
		
		
        sta WSYNC ;3c
		
		;hier beginnt eine neue Zeile bei 0c
		;1 cpu-zyklus = 1c = 3 pixel!
		
        sta CTRLPF ;3c spielfeldkonfiguration
		
		
        sta GRP0 ;3c
        sta GRP1 ;3c
		
		;spritemodi auf 3fach mit geringem abstand schalten
        lda #$03  ;2c         
        sta NUSIZ0 ;3c
        sta NUSIZ1 ;3c
		
		;vertikal delay fuer ausgabelatch sprites aktivieren
        lda #$01     ;      
        sta VDELP0		
        sta $2d 
        sta VDELP1		
		
		;8 cyclen warrten fuer positionierung
		nop ;2c
		nop ;2c
		nop ;2c
		nop ;2c
		
		;hier position 37 cpu-zyklen, damit pixelpos 37*3 -68 hblank = 43
	
		;spritepos setzen;player0 pixelpos 5 bzw player1 8px rechts ddavon
        sta.w RESP0 ;3c
        sta RESP1 ;3c
		;positionskorrektur ueber move-register
        lda #%11110000 ;2c -2 => player0 zwei pixel nach rechts
        sta HMP1 ;3c
        lda #%11100000 ;2c -1 => player1 einen pixel nach links
        sta HMP0 ;3c

        sta WSYNC ;3c
		;!! hmove muss (soll) direkt nach wsync ausgefuehrt werden!!
        sta HMOVE;3c versatz er sprites aus HMPx-regs uebernehmen



GameLoop
        jsr VSync       ;start vertical retrace

        jsr VBlank      ; spare time during screen blank
        jsr Picture     ; draw one screen
        jsr overscan    ; do overscan

        jmp GameLoop    ;back to top


VSync
        lda #2          ;bit 1 needs to be 1 to start retrace
        sta VSYNC       ;start retrace
        sta WSYNC       ;wait a few lines
        sta WSYNC
        lda #44         ;prepare timer to exit blank period (44)
        sta TIM64T      ;turn it on
        sta WSYNC       ;wait one more
        sta VSYNC       ;done with retrace, write 0 to bit 1

        rts ; VSync



VBlank

        rts ; VBlank




overscan

        sta WSYNC

        lda #$0E
        sta COLUBK

        lda #36         ; Use the timer to make sure overscan takes (34)
        sta TIM64T      ; 30 scan lines.  29 scan lines * 76 = 2204 / 64 = 34.4

; hier ist platz um bissl was zu tun...solange es weniger als die oben erwaehnten 34 clockcycles sind		

;eingabe von joystick abfragen
;dabei nur auf flanke reagieren
		lda lastiostate
		eor SWCHA
		beq noiochanged
		
		;aktuelle eingabe laden
		lda SWCHA
		;..und fuer vergleich bei naechstem durchlauf sichern
		sta lastiostate
		
		lda #%00100000;runter
		bit SWCHA
		bne ionotdown
		
		;einhaltung der grenze checken
		ldy curselindex
		iny
		cpy #GAMECOUNT
		beq ionotdown
		
		sty curselindex
		lda curseltextindex
		clc  ;carry flag vor addition loeschen!
		adc #12
		sta curseltextindex	

		;ggf startposition verschieben
		cpy #5
		bcc ionotdown
		cpy #GAMECOUNT-#3
		bcs ionotdown
		
		lda starttextline
		clc
		adc #12
		sta starttextline
		
ionotdown

		lda #%00010000;hoch
		bit SWCHA
		bne ionotup
		
		ldy curselindex
		dey
		bmi ionotup;neuen wert nur uebernehmen falls kein ueberlauf		
		sty curselindex
		lda curseltextindex
		;textindex auch updaten
		sec  ;carry flag vor subtraktion setzen!
		sbc #12
		sta curseltextindex

		;ggf startposition verschieben
		cpy #4
		bcc ionotup
		cpy #GAMECOUNT-#3-#1
		bcs ionotup
		
		lda starttextline
		sec
		sbc #12
		sta starttextline

		
ionotup
noiochanged

;bei "fire" ausgewaehltes rom starten
	lda INPT4
	bmi nofire ;bmi prueft vorzeichen == vorderstes bit ==7. bit
	;lda #$10
	;sta COLUBK
	lda gameselection_low
	clc ;carry flag for additon loeschen!
	adc curselindex
	sta gameselection_low
	ldy curselindex
	lda gameselectionarea,y
	jmp restarttoselection
	
nofire



		
endOS
        lda INTIM       ; We finished, but wait for timer
        bne endOS       ; by looping till zero

        sta WSYNC       ; End last scanline

        lda #$82
        sta VBLANK
        lda #$02
        sta VBLANK

        rts     ; overscan





Picture
	ldx #$9C
	stx COLUP0
	stx COLUP1
;warten, bis VBLANK zeitspanne vorbei ist (timer gesetzt)
pictureLoop
        lda INTIM       
        bne pictureLoop 

        sta WSYNC
        lda #$80
        sta VBLANK      ;vblank beenden
		

        lda #$9C
        sta COLUPF

        lda #0



        tsx
        stx spbuff
        
        SLEEP 46


        jsr showImage        
		;lda #0
		lda starttextline
		sta textline
		
		
		ldx #0
printline
		jsr printText
		inx
		cpx #GAMEVIEWCOUNT
		bne printline

		
        lda #0
        sta GRP0
        sta GRP1
        sta GRP0
        sta GRP1


; restore stack pointer

        ldx spbuff
        txs


        ldx #11

ScanLoop
        sta WSYNC
        dex
        bne ScanLoop

        sta COLUPF

        rts     ; Picture




	align 256
showImage

;zeilenweise bild ausgeben
        
		ldx #$30;2c  zaheler fuer zeilenposition
		
drawImageLine
        sta WSYNC ;3c zum naechsten zeilenanfang
		
		;spalte 5 fuer zugriff ohne index puffern
		;wir haben spater nicht genug zeit fuer die letzte spalte ein register wiederzuverwenden daher muessen wir x hernehmen
		stx tempx;x fuer spaeter hinterlegen (muss am ende der schleife rekonstruiert werden)
		lda col5,x;da x natuerlich nicht gleichzeitig als index verwendet werden kann muss das byte im ram geparkt werden
		sta grafbuff
		
        lda col0,x   ;3c
        sta GRP0        ;3c
        lda col1,x   ;3c
        sta GRP1      ;4c
        lda col2,x   ;3c
        sta GRP0        ;3c
        lda col3,x   ;3c
        ldy col4,x   ;3c
        ldx grafbuff   ;3c

        sta GRP1        ;3c
        sty GRP0        ;3c
        stx GRP1        ;3c
        sty GRP0        ;3c
		
		ldx tempx
		
		dex
;		cpx #$26
		bne drawImageLine

		rts


		
printText
	;registerzustaende sichern sichern
	stx xbuff
	sty ybuff
	sta abuff
	;stackpointer auf buchstabenbereich legen
	tsx
	stx curstackpos;stackpointer fuer spaetere wiederherstellung sichern
	;..und stackpointer auf adresse "1 vor" der ersten spalte setzen
	ldx #textcolz
	txs

	;playersprites transparent machen um anzeigebereich zw zeilen "leer" zu lassen
	lda #$0
	sta GRP0
	sta GRP1
	sta GRP0

	;zeilenposition updaten und dabei auf aktive selektion pruefen
	lda textline
	
	ldx #0
	cmp curseltextindex
	bne notselected
	ldx #$96
notselected	
	stx COLUP0
	stx COLUP1
	tax	
	clc ;carry flag vor addition loeschen!
	adc #12
	sta textline;textline"pointer" fuer spateren vergleich u naechste zeile aktualisieren

charloop;durch buchstaben iterieren und spaltenweise fuer ausgabe ablegen (2 zeichen pro byte)
	lda text,x
	inx
	ldy text,x
	inx
	; x sichern um register fuer anderweitige verwendung frei zu machen
	stx curtextpos
	tax
	
	;zeilenweise aktuelles buchstabenpaar zusammenfuegen und mit hilfe des stackpointers im ram ablegen
	lda upperfont0,x
	ora lowerfont0,y
	pha
	lda upperfont1,x
	ora lowerfont1,y
	pha
	lda upperfont2,x
	ora lowerfont2,y
	pha
	lda upperfont3,x
	ora lowerfont3,y
	pha
	lda upperfont4,x
	ora lowerfont4,y
	pha

	ldx curtextpos
	cpx textline;textposition der kommenden (und damit ende der aktuellen) zeile erreicht?
	bne charloop
	
	;stackpointer wiederherstellen
	ldx curstackpos
	txs
	
	;textzeile ausgeben
	ldx #5; font ist 5 pixel hoch
	
drawTextLine
	sta WSYNC ;zum naechsten zeilenanfang
		
	;wir haben spater nicht genug zeit fuer die letzte spalte ein register wiederzuverwenden daher muessen wir x hernehmen
	stx tempx;x fuer spaeter hinterlegen (muss am ende von jedem durchlauf rekonstruiert werden)
	lda textcol5,x;da x natuerlich nicht gleichzeitig als index verwendet werden kann muss das byte im ram geparkt werden
	sta grafbuff;TODO anderen namen..col5buff vllt?
	
	lda textcol0,x   ;4
	sta GRP0        ;3
	lda textcol1,x   ;4
	sta GRP1      ;3
	lda textcol2,x   ;4
	sta GRP0        ;3
	lda textcol3,x   ;4
	ldy textcol4,x   ;4
	ldx grafbuff   ;3

	sta GRP1        ;3
	sty GRP0        ;3
	stx GRP1        ;3
	sty GRP0        ;3
	
	ldx tempx; x als zeilencounter wiederherstellen
	
	dex
	bne drawTextLine
	
	
	;register wiederherstellen
	lda abuff
	ldx xbuff
	ldy ybuff
	
	rts

;text

        align 256
text byte "ADVENTURE   "
 byte 	   "BERZERK     "
 byte      "COMBAT      "
 byte      "DEMON ATTACK"
 byte      "DONKEY KONG "
 byte      "FROGGER     "
 byte      "MISSLE CMD  " 
 byte      "PAC MAN     "
 byte      "PITFALL!    " 

 byte      "Q*BERT      "
 byte      "RIVER RAID  " 
 byte      "SPACEINVADER" 
 byte      "BREAKOUT    "
 byte      "PONG        "
 byte      "YARS REVENGE" 
 byte      "TEST 5      "
 ;newtext byte 0,1,2,3,4,5,6,7,8,9,10,11


;grafiken und font
	align 256
logo

col0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $20
        byte $20
        byte $21
        byte $61
        byte $7b
        byte $4e
        byte $42
        byte $42
        byte $2
        byte $2
        byte $0
        byte $0
        byte $1
        byte $3
        byte $1
        byte $1
        byte $0
        byte $0
        byte $30
        byte $18
        byte $c
        byte $7
        byte $2
        byte $0
        byte $1b
        byte $f
        byte $2
        byte $0
        byte $1
        byte $3
        byte $1
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
col1
        byte $0
        byte $0
        byte $0
        byte $1
        byte $1
        byte $1
        byte $1
        byte $10
        byte $3c
        byte $4
        byte $5
        byte $25
        byte $75
        byte $5d
        byte $49
        byte $49
        byte $9
        byte $3
        byte $0
        byte $0
        byte $0
        byte $80
        byte $80
        byte $c0
        byte $c0
        byte $60
        byte $60
        byte $24
        byte $6
        byte $c7
        byte $e7
        byte $67
        byte $33
        byte $db
        byte $eb
        byte $30
        byte $e0
        byte $f0
        byte $80
        byte $38
        byte $38
        byte $36
        byte $6
        byte $1
        byte $1
        byte $0
        byte $0
        byte $0
col2
        byte $0
        byte $0
        byte $e2
        byte $13
        byte $1
        byte $0
        byte $80
        byte $88
        byte $ec
        byte $b8
        byte $80
        byte $80
        byte $c0
        byte $e0
        byte $a7
        byte $bd
        byte $94
        byte $76
        byte $67
        byte $6
        byte $0
        byte $38
        byte $38
        byte $38
        byte $30
        byte $70
        byte $70
        byte $70
        byte $60
        byte $60
        byte $8
        byte $1c
        byte $1c
        byte $38
        byte $38
        byte $70
        byte $3
        byte $f
        byte $7
        byte $0
        byte $7
        byte $5
        byte $fe
        byte $5b
        byte $4d
        byte $1d
        byte $8
        byte $0
col3
        byte $0
        byte $0
        byte $4
        byte $c
        byte $f8
        byte $98
        byte $d0
        byte $70
        byte $70
        byte $20
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $a0
        byte $a0
        byte $20
        byte $70
        byte $70
        byte $50
        byte $18
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $3
        byte $f
        byte $3e
        byte $78
        byte $60
        byte $0
        byte $e0
        byte $e0
        byte $c0
        byte $0
        byte $f
        byte $f0
        byte $30
        byte $a0
        byte $d8
        byte $7
        byte $c0
        byte $40
col4
        byte $0
        byte $0
        byte $86
        byte $8c
        byte $98
        byte $f0
        byte $8c
        byte $c4
        byte $c4
        byte $f8
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $80
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $f0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
col5
        byte $0
        byte $0
        byte $c0
        byte $f8
        byte $8c
        byte $86
        byte $c2
        byte $42
        byte $46
        byte $fc
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0
        byte $0


		
        align 256
		
lowerfont0
		byte "                                "
        byte $00
        byte $04
        byte $0a
        byte $0a
        byte $04
        byte $0a
        byte $04
        byte $04
        byte $02
        byte $08
        byte $00
        byte $00
        byte $00
        byte $00
        byte $00
        byte $02
        byte $0e
        byte $04
        byte $0e
        byte $0e
        byte $0a
        byte $0e
        byte $0e
        byte $0e
        byte $0e
        byte $0e
        byte $00
lowerfont1		
        byte $00
        byte $02
        byte $00
        byte $08
        byte $0e
        byte $0e
        byte $0e
        byte $0e
        byte $0e
        byte $0c
        byte $0e
        byte $0e
        byte $0e
        byte $0a
        byte $0e
        byte $02
        byte $0a
        byte $08
        byte $0a
        byte $02
        byte $0e
        byte $0e
        byte $0e
        byte $0c
        byte $0e
        byte $0e
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $00


        byte $00
        byte $04
        byte $0a
        byte $0e
        byte $0c
        byte $02
        byte $0a
        byte $04
        byte $04
        byte $04
        byte $0a
        byte $04
        byte $00
        byte $00
        byte $00
        byte $04
        byte $0a
        byte $0c
        byte $02
        byte $02
        byte $0a
        byte $08
        byte $08
        byte $02
        byte $0a
        byte $0a
        byte $04
lowerfont2		
        byte $04
        byte $04
        byte $0e
        byte $04
        byte $02
        byte $0a
        byte $0a
        byte $0a
        byte $08
        byte $0a
        byte $08
        byte $08
        byte $08
        byte $0a
        byte $04
        byte $02
        byte $0a
        byte $08
        byte $0e
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $08
        byte $04
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $0a
        byte $00


        byte $00
        byte $04
        byte $00
        byte $0a
        byte $0a
        byte $04
        byte $04
        byte $00
        byte $04
        byte $04
        byte $04
        byte $0e
        byte $00
        byte $0e
        byte $00
        byte $04
        byte $0a
        byte $04
        byte $04
        byte $06
        byte $0e
        byte $0e
        byte $0e
        byte $04
        byte $0e
        byte $0e
        byte $00
lowerfont3		
        byte $00
        byte $08
        byte $00
        byte $02
        byte $04
        byte $0a
        byte $0e
        byte $0c
        byte $08
        byte $0a
        byte $0c
        byte $0c
        byte $0a
        byte $0e
        byte $04
        byte $02
        byte $0c
        byte $08
        byte $0a
        byte $0e
        byte $0a
        byte $0e
        byte $0a
        byte $0c
        byte $0e
        byte $04
        byte $0a
        byte $0a
        byte $0a
        byte $04
        byte $04
        byte $00


        byte $00
        byte $00
        byte $00
        byte $0e
        byte $06
        byte $08
        byte $0a
        byte $00
        byte $04
        byte $04
        byte $0a
        byte $04
        byte $00
        byte $00
        byte $00
        byte $04
        byte $0a
        byte $04
        byte $08
        byte $02
        byte $02
        byte $02
        byte $0a
        byte $04
        byte $0a
        byte $02
        byte $04
lowerfont4		
        byte $00
        byte $04
        byte $0e
        byte $04
        byte $00
        byte $08
        byte $0a
        byte $0a
        byte $08
        byte $0a
        byte $08
        byte $08
        byte $0a
        byte $0a
        byte $04
        byte $0a
        byte $0a
        byte $08
        byte $0a
        byte $0a
        byte $0a
        byte $08
        byte $0a
        byte $0a
        byte $02
        byte $04
        byte $0a
        byte $0a
        byte $0e
        byte $0a
        byte $04
        byte $00


        byte $00
        byte $04
        byte $00
        byte $0a
        byte $04
        byte $0a
        byte $06
        byte $00
        byte $02
        byte $08
        byte $00
        byte $00
        byte $04
        byte $00
        byte $04
        byte $08
        byte $0e
        byte $0e
        byte $0e
        byte $0e
        byte $02
        byte $0e
        byte $0e
        byte $04
        byte $0e
        byte $0e
        byte $00
upperfont0		
        byte $04
        byte $02
        byte $00
        byte $08
        byte $04
        byte $0e
        byte $0a
        byte $0e
        byte $0e
        byte $0c
        byte $0e
        byte $08
        byte $0e
        byte $0a
        byte $0e
        byte $0e
        byte $0a
        byte $0e
        byte $0a
        byte $08
        byte $0e
        byte $08
        byte $0e
        byte $0a
        byte $0e
        byte $04
        byte $0e
        byte $04
        byte $0a
        byte $0a
        byte $04
        byte $0e


        byte $00
        byte $40
        byte $a0
        byte $a0
        byte $40
        byte $a0
        byte $40
        byte $40
        byte $20
        byte $80
        byte $00
        byte $00
        byte $00
        byte $00
        byte $00
        byte $20
        byte $e0
        byte $40
        byte $e0
        byte $e0
        byte $a0
        byte $e0
        byte $e0
        byte $e0
        byte $e0
	
        byte $e0
		
        byte $00
upperfont1			
        byte $00
        byte $20
        byte $00
        byte $80
        byte $e0
        byte $e0
        byte $e0
        byte $e0
        byte $e0
        byte $c0
        byte $e0
        byte $e0
        byte $e0
        byte $a0
        byte $e0
        byte $20
        byte $a0
        byte $80
        byte $a0
        byte $20
        byte $e0
        byte $e0
        byte $e0
        byte $c0
        byte $e0
        byte $e0
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $00


        byte $00
        byte $40
        byte $a0
        byte $e0
        byte $c0
        byte $20
        byte $a0
        byte $40
        byte $40
        byte $40
        byte $a0
        byte $40
        byte $00
        byte $00
        byte $00
        byte $40
        byte $a0
        byte $c0
        byte $20
        byte $20
        byte $a0
        byte $80
        byte $80
        byte $20
        byte $a0
        byte $a0
        byte $40
upperfont2		
        byte $40
        byte $40
        byte $e0
        byte $40
        byte $20
        byte $a0
        byte $a0
        byte $a0
        byte $80
        byte $a0
        byte $80
        byte $80
        byte $80
        byte $a0
        byte $40
        byte $20
        byte $a0
        byte $80
        byte $e0
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $80
        byte $40
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $a0
        byte $00


        byte $00
        byte $40
        byte $00
        byte $a0
        byte $a0
        byte $40
        byte $40
        byte $00
        byte $40
        byte $40
        byte $40
        byte $e0
        byte $00
        byte $e0
        byte $00
        byte $40
        byte $a0
        byte $40
        byte $40
        byte $60
        byte $e0
        byte $e0
        byte $e0
        byte $40
        byte $e0
        byte $e0
        byte $00
upperfont3		
        byte $00
        byte $80
        byte $00
        byte $20
        byte $40
        byte $a0
        byte $e0
        byte $c0
        byte $80
        byte $a0
        byte $c0
        byte $c0
        byte $a0
        byte $e0
        byte $40
        byte $20
        byte $c0
        byte $80
        byte $a0
        byte $e0
        byte $a0
        byte $e0
        byte $a0
        byte $c0
        byte $e0
        byte $40
        byte $a0
        byte $a0
        byte $a0
        byte $40
        byte $40
        byte $00


        byte $00
        byte $00
        byte $00
        byte $e0
        byte $60
        byte $80
        byte $a0
        byte $00
        byte $40
        byte $40
        byte $a0
        byte $40
        byte $00
        byte $00
        byte $00
        byte $40
        byte $a0
        byte $40
        byte $80
        byte $20
        byte $20
        byte $20
        byte $a0
        byte $40
        byte $a0
        byte $20
        byte $40
upperfont4		
        byte $00
        byte $40
        byte $e0
        byte $40
        byte $00
        byte $80
        byte $a0
        byte $a0
        byte $80
        byte $a0
        byte $80
        byte $80
        byte $a0
        byte $a0
        byte $40
        byte $a0
        byte $a0
        byte $80
        byte $a0
        byte $a0
        byte $a0
        byte $80
        byte $a0
        byte $a0
        byte $20
        byte $40
        byte $a0
        byte $a0
        byte $e0
        byte $a0
        byte $40
        byte $00


        byte $00
        byte $40
        byte $00
        byte $a0
        byte $40
        byte $a0
        byte $60
        byte $00
        byte $20
        byte $80
        byte $00
        byte $00
        byte $40
        byte $00
        byte $40
        byte $80
        byte $e0
        byte $e0
        byte $e0
        byte $e0
        byte $20
        byte $e0
        byte $e0
        byte $40
        byte $e0
        byte $e0
        byte $00
        byte $40
        byte $20
        byte $00
        byte $80
        byte $40
        byte $e0
        byte $a0
        byte $e0
        byte $e0
        byte $c0
        byte $e0
        byte $80
        byte $e0
        byte $a0
        byte $e0
        byte $e0
        byte $a0
        byte $e0
        byte $a0
        byte $80
        byte $e0
        byte $80
        byte $e0
        byte $a0
        byte $e0
        byte $40
        byte $e0
        byte $40
        byte $a0
        byte $a0
        byte $40
        byte $e0

restarttoselection
	;speicherbereich und register fuer naechstes zu ladendes rom sicherheitshalber aufrauemen
	INITSYSTEM
	jmp (resetvektor)		
		
	org $FFE0
gameselectionarea
	byte $aa
	byte $bb
	byte $cc
	byte $dd
	byte $ee
	byte $ff
	byte $00
	byte $11
	byte $22
	byte $33
	byte $44

	

		

      


;reset vektor

        org $FFFC
resetvektor		
        .word Start
        .word Start