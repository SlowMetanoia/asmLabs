;Вариант 12: "Вычислить произведение чисел кратных 3 и
;             разделить его на сумму отрицательных чисел."
d1 SEGMENT para public USE16 'data'
    videoSeg         equ 0b800h     ; начало видеопамяти
    redColor         equ 01000000b  ; красный фон
    orangeColor      equ 01100000b  ; оранжевый фон
    greenColor       equ 00100000b  ; зеленый фон
    blueColor        equ 00010000b  ; синий фон
    pinkColor        equ 01010000b  ; пурпурный фон
    greyColor        equ 01110000b  ; серый фон
    newYearColor     equ 10000010b  ; зеленый цвет поздравления
    waitBgColor      equ 01011110b  ; синий  фон и зеленые буквы
    waitBgColor      equ 01011110b  ; синий  фон и зеленые буквы
    mainBgColor      equ 00011110b  ; синий фон и желтые буквы
    errorBgColor     equ 01000000b	; красный фон и черные буквы
    errorBgColorWait equ 11000000b	; красный фон и черные моргающие буквы
    resultBGColor    equ 00100100b	; фиолетовый фон и желтые буквы

    WaitToStartText     db 'Press Enter to start, q to exit                                   yOu_NExT_XIII$'
    newYearText         db 'Merry Christmas and Happy new year!$'
    WaitAnyKeyToExit    db 'Press any key to exit.$'
    Result				db 'Results$'
    WaitToReloadOrExit1 db 'Press Enter to move result window, q to exit                      yOu_NExT_XIII$'
    WaitToMoveWiindow   db 'Use the arrows to move window, press q to exit                    yOu_NExT_XIII$'

    in_str label byte   	 ; Cтрока символов (не более 6)
	razmer db 7              ; Размер буфера
	kol db (?)               ; Количество введеных символов
	stroka db 7 dup (?)      ; Буфер ввода для каждого введеного числа
	NegSum dw 0              ; Сумма отрицательных чисел
	Ostatok dw 0             ; Остаток от деления на 3
	Mul3Res dw 0             ; Произведение чисел кратных 3
	DivRes dw 0              ; Результат деления
	number dw 5 dup (0)  	 ; Mассив чисел
	siz dw 5              	 ; Kоличество чисел
	
	perevod db 10,13,'$'
	text_err1 db 'Input Error!$'
	div_zero db 'Divition by zero!$'
	messovf db 'Overflow!', 10,10,'$'
	Mul3ResText db 'Numbers of multiples of 3: ','$'
	NegSumText db 'Sum of negative numbers:   ','$'
	DivResText db 'Result:                    ','$'
	out_str db 6 dup (' '),'$'
	mess1 db 'Input number from -29999 to 29999: $'
	
	flag_err equ 1

    mainWindowXStart db 0d
	mainWindowYStart db 0d
	mainWindowXEnd   db 49d
	mainWindowYEnd   db	6d
	
	coursorX db 0
	coursorY db 0

    lastWindowXStart db 20d
	lastWindowYStart db 9d
	lastWindowXEnd   db 65d
	lastWindowYEnd   db	16d

    ResultY db 10d
    ResultX db 39d
    Mul3ResTextY db 12d
    Mul3ResTextX db 26d
    NegSumTextY db 13d
    NegSumTextX db 26d
    DivResTextY db 14d
    DivResTextX db 26d



d1 ENDS

st1 SEGMENT para stack USE16 'stack'
	dw 100 dup (?)
st1 ENDS

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Макрос рисования окна >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; xStart - левый верхний угол - столбец
; yStart - левый верхний угол - строка
; xEnd   - правый нижний угол - столбец
; yEnd   - правый нижний угол - строка
drawWindow macro xStart, yStart, xEnd, yEnd, color	
	mov ah, 06
	mov al, 00
	
	mov ch, yStart				; левый верхний угол - строка
	mov cl, xStart				; левый верхний угол - столбец
	
	mov dh, yEnd				; правый нижний угол - строка
	mov dl, xEnd				; правый нижний угол - столбец
	
	mov bh, color				; устанавливаем цвета фона и цвета букв
	
	int 10h						; прерывание отрисовки
endm

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Макрос вывода в окне >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; string - текст для вывода
; row    - строка вывода
; column - колонка вывода 
printInWindow macro string, row, column
	push ax
	push dx
	
	mov ah,2
	mov dh,row
	mov dl,column
	mov bh,0
	int 10h
	
	mov ah, 09h
	mov dx, offset string
	int 21h
	
	pop dx
	pop ax
endm

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Макрос ожидания при помощи функции 86h прерывания Int 15h >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; time - время в миллисекундах 
sleep macro time
	mov al, 0
	mov ah, 86h
	mov cx, time
	int 15h
endm

print macro f1		; Bывод сообщений на экран
	push ax
	push dx
	mov dx, offset f1
	mov ah, 09h
	int 21h
	pop dx
	pop ax
endm


input macro f2 	; Bвод строки символов
	push ax
	push dx
	mov dx, offset f2
	mov ah, 0Ah
	int 21h
	pop dx
	pop ax
endm


.386
c1 SEGMENT para public USE16 'code'
ASSUME cs:c1, ds:d1, ss:st1

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Процедура прятания курсора >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; устанавливает курсор за пределами окна	
hideCursor PROC	
	mov ah,2			; прячем курсор
	mov dh,26			; устанавливаем его за пределы экрана
	mov dl,81
	mov bh,0
	int 10h	
	ret
ENDP

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Процедура сдвига окна влево >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
leftShift PROC
	cmp lastWindowXStart, 0
	je retleftShift
	dec lastWindowXStart
	dec lastWindowXEnd
    dec ResultX
    dec Mul3ResTextX
    dec NegSumTextX
    dec DivResTextX
retleftShift:
	ret
ENDP



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Процедура сдвига окна вправо >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rightShift PROC
	cmp lastWindowXEnd, 79
	je retrightShift
	inc lastWindowXStart
	inc lastWindowXEnd
    inc ResultX
    inc Mul3ResTextX
    inc NegSumTextX
    inc DivResTextX
retrightShift:
	ret
ENDP



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Процедура сдвига окна вверх >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
upShift PROC
	cmp lastWindowYStart, 0
	je relupShift
	dec lastWindowYStart
	dec lastWindowYEnd
    dec ResultY
    dec Mul3ResTextY
    dec NegSumTextY
    dec DivResTextY
relupShift:
	ret
ENDP



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Процедура сдвига окна вниз >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
downShift PROC
	cmp lastWindowYEnd, 23
	je downshitRet
	inc lastWindowYStart
	inc lastWindowYEnd
    inc ResultY
    inc Mul3ResTextY
    inc NegSumTextY
    inc DivResTextY
downshitRet:
	ret
ENDP

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Поехали! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
start:
    mov ax, videoSeg
	mov es, ax
	mov ax, d1
	mov ds, ax
; вызоваем функцию 0 -  установка 3 текстового видеорежима, очистка экрана
	mov ax,03h  ;ah=0 (номер функции),al=3 (номер режима)
	int 10h
; рисуем подарки
    call hideCursor
    sleep 10

    drawWindow 13, 14, 22, 18, greenColor
    drawWindow 17, 14, 18, 18, redColor
    drawWindow 15, 13, 16, 13, redColor
    drawWindow 19, 13, 20, 13, redColor
    call hideCursor

    sleep 10

    drawWindow 50, 14, 59, 18, blueColor
    drawWindow 54, 14, 55, 18, pinkColor
    drawWindow 52, 13, 53, 13, pinkColor
    drawWindow 56, 13, 57, 13, pinkColor
    call hideCursor

    sleep 10

    drawWindow 13, 3, 22, 7, orangeColor
    drawWindow 17, 3, 18, 7, greenColor
    drawWindow 15, 2, 16, 2, greenColor
    drawWindow 19, 2, 20, 2, greenColor
    call hideCursor

    sleep 10

    drawWindow 50, 3, 59, 7, greyColor
    drawWindow 54, 3, 55, 7, orangeColor
    drawWindow 52, 2, 53, 2, orangeColor
    drawWindow 56, 2, 57, 2, orangeColor
    call hideCursor

    sleep 10

    drawWindow 20, 10, 60, 10, newYearColor
    printInWindow newYearText, 10, 20

    waitToStart:
	drawWindow 0, 24, 80, 24, waitBgColor
	printInWindow WaitToStartText, 24, 0
	call hideCursor
	mov ah, 000								; ожидаем ввода клавиши
	int 16h
    cmp ax, 1071h                           ; сравниваем с ASCII и скан кодом клавиши q
	je programEnd
	cmp ax, 1C0Dh							; сравниваем с ASCII и скан кодом клавиши Enter							
	je startInput
	loop waitToStart
    
startInput:
	mov ax, 03h		; очищаем экран
	int 10h	
	drawWindow mainWindowXStart, mainWindowYStart, mainWindowXEnd, mainWindowYEnd, mainBgColor
	
	mov al, mainWindowXStart
	mov coursorX, al
	
	mov al, mainWindowYStart
	mov coursorY, al
	
	inc coursorX

    xor di,di		; di - номер числа в массиве
    mov cx, siz		; cx - размер массива

inputValues:
	push cx

	inc coursorY

    ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Выводим сообщение о вводе строки >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	printInWindow mess1, coursorY, coursorX	
	input in_str 		; вводим число в виде строки
	
	call diapazon		; проверка диапазон вводимых чисел (-29999,+29999)
	cmp bh, flag_err  	; сравниваем bh и flag_err
	je inErr         	; если равен 1 сообщение об ошибке ввода

	call dopust			; проверяем допустимость вводимых символов
	cmp bh, flag_err  	; сравниваем bh и flag_err
	je inErr         	; если равен 1 сообщение об ошибке ввода
	
	call AscToBin 	    ; преобразовываем строку в число
	inc di
	inc di
	pop cx
	loop inputValues
	jmp search
	
inErr:  
	drawWindow 27, 9, 53, 16, errorBgColor
	drawWindow 27, 15, 53, 16, errorBgColorWait
	printInWindow text_err1, 12, 34
	printInWindow WaitAnyKeyToExit, 15, 30
	call hideCursor
	mov ah, 000			; ожидаем ввод клавиши
	int 16h
	jmp programEnd

search:
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Поиск суммы отрицательных чисел >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mov cx, siz 	; В (cx) - размер массива
    mov si, offset number
    xor ax, ax
sumNegative:
    mov ax,[si]
    cmp ax, 0
    jge positive
    add NegSum,ax
    jo overflow      ; если переполнение, то переход   ;   jo т.к. числа со знаком 
positive:		
    inc si
    inc si
    loop sumNegative
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Поиск произведения чисел кратным 3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mov cx, siz 	; В (cx) - размер массива
    mov si, offset number
    mov ax, 1
    xor dx, dx
mul3:
    mov bx,[si]
    cmp bx, 0
    jge plusMul3
    je notMul3
    push bx
    push ax
    push dx
    xor dx, dx
    mov ax, bx
    mov bx, -1
    imul bx
    mov bx, 3
    idiv bx
    mov Ostatok, dx
    pop dx
    pop ax
    pop bx
    jmp nextMul3
plusMul3:
    push bx
    push ax
    push dx
    xor dx, dx
    mov ax, bx
    mov bx, 3
    idiv bx
    mov Ostatok, dx
    pop dx
    pop ax
    pop bx
nextMul3:
    cmp Ostatok, 0
    jne notMul3
    imul bx
    jo overflow  ;   jo т.к. числа со знаком
notMul3:		
    inc si
    inc si
    loop mul3
    mov Mul3Res, ax
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Поиск частного >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    cmp NegSum, 0
    je zero
    mov ax, Mul3Res
    cwd
    mov bx, NegSum
    idiv bx
    mov DivRes, ax
    
    jmp vivod

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Выводим предупреждения >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
overflow:		
		drawWindow 27, 9, 53, 16, errorBgColor
        drawWindow 27, 15, 53, 16, errorBgColorWait
        printInWindow messovf, 12, 36
        printInWindow WaitAnyKeyToExit, 15, 30
        call hideCursor
        mov ah, 00
        int 16h
        jmp programEnd
zero:
		drawWindow 27, 9, 53, 16, errorBgColor
        drawWindow 27, 15, 53, 16, errorBgColorWait
        printInWindow div_zero, 12, 36
        printInWindow WaitAnyKeyToExit, 15, 30
        call hideCursor
        mov ah, 00
        int 16h
        jmp programEnd   

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Вывод >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vivod:
    drawWindow lastWindowXStart, lastWindowYStart, lastWindowXEnd, lastWindowYEnd, resultBGColor
	printInWindow Result, ResultY, ResultX
	printInWindow Mul3ResText, Mul3ResTextY, Mul3ResTextX
	mov ax, Mul3Res
	call BinToAsc
	print out_str

	mov cx,6			; очищаем буфер вывода
	xor si,si
clear1:		
	mov [out_str+si],' '
	inc si
	loop clear1

	printInWindow NegSumText, NegSumTextY, NegSumTextX
	mov ax,NegSum	
	call BinToAsc
	print out_str

	mov cx,6			; очищаем буфер вывода
	xor si,si
clear2:		
	mov [out_str+si],' '
	inc si
	loop clear2

	printInWindow DivResText, DivResTextY, DivResTextX
	mov ax,DivRes	
	call BinToAsc
	print out_str

    drawWindow 0, 24, 80, 24, waitBgColor
	printInWindow WaitToReloadOrExit1, 24, 0

pressWait1:
	call hideCursor
	mov ah, 000								; ожидаем ввод клавиши
	int 16h
	cmp ax, 1071h							; сравневаем с ASCII-кодом клавиши q
	je programEnd
	cmp ax, 1C0Dh							; сравниваем с ASCII-кодом клавиши enter
	je moveWindow
	loop pressWait1

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Двигаем окно >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
moveWindow:
	mov ax, 03h		; очищаем экран
	int 10h			
	drawWindow lastWindowXStart, lastWindowYStart, lastWindowXEnd, lastWindowYEnd, resultBGColor
    printInWindow Result, ResultY, ResultX
	printInWindow Mul3ResText, Mul3ResTextY, Mul3ResTextX
	mov ax, Mul3Res
	call BinToAsc
	print out_str

	mov cx,6			; очищаем буфер вывода
	xor si,si
clear1_1:		
	mov [out_str+si],' '
	inc si
	loop clear1_1

	printInWindow NegSumText, NegSumTextY, NegSumTextX
	mov ax,NegSum	
	call BinToAsc
	print out_str

	mov cx,6			; очищаем буфер вывода
	xor si,si
clear2_2:		
	mov [out_str+si],' '
	inc si
	loop clear2_2

	printInWindow DivResText, DivResTextY, DivResTextX
	mov ax,DivRes	
	call BinToAsc
	print out_str
	
	mov cx,6			; очищаем буфер вывода
	xor si,si
	
	drawWindow 0, 24, 80, 24, waitBgColor
	printInWindow WaitToMoveWiindow, 24, 0
	call hideCursor

	mov ah, 00								; ожидаем ввод клавиши
	int 16h

	cmp ax, 1071h							; сравниваем с ASCII-кодом клавиши q
	je programEnd
	
	cmp ax, 4B00h							; сравниваем с ASCII и скан кодом клавиши 'стрелка влево'
	je moveLeft
	
	cmp ax, 4D00h							; сравниваем с ASCII и скан кодом клавиши 'стрелка вправо'
	je moveRight
	
	cmp ax, 4800h							; сравниваем с ASCII и скан кодом клавиши 'стрелка вниз'
	je moveUp
	
	cmp ax, 5000h							; сравниваем с ASCII и скан кодом клавиши 'стрелка вверх'
	je moveDown
	
	jmp moveWindow
	
moveLeft:
	call leftShift
	jmp moveWindow
	
moveRight:
	call rightShift
	jmp moveWindow
	
moveUp:
	call upShift
	jmp moveWindow
	
moveDown:
	call downShift
	jmp moveWindow

programEnd:
	mov ax, 03h		; очищаем экран
	int 10h			
	mov ax, 4c00h	; завершаем работу
	int 21h

;****************************************************
;* Проверка диапазона вводимых чисел -29999,+29999	*
;* Аргументы:										*
;* 		Буфер ввода - stroka						*
;* 													*
;* Результат:										*
;* 		bh - флаг ошибки ввода						*
;****************************************************
DIAPAZON PROC
    xor bh, bh
	xor si, si
	
	cmp kol, 05h 	; Если ввели менее 5 символов, проверим их допустимость
	jb dop
		
	cmp stroka, 2dh 	; Eсли ввели 5 или более символов проверим является ли первый минусом
	jne plus 	; Eсли 1 символ не минус, проверим число символов
	
	cmp kol, 06h 	; Eсли первый - минус и символов меньше 6 проверим допустимость символов 
	jb dop        
	
	inc si		; Иначе проверим первую цифру
	jmp first

plus:   
	cmp kol,6	; Bведено 6 символов и первый - не минус 
	je error1	; Oшибка
	
first:  
	cmp stroka[si], 32h	; Cравним первый символ с '2'
	jna dop		; Eсли первый <= '2' - проверим допустимость символов
	
error1:
	mov bh, flag_err	; Иначе bh = flag_err
	
dop:	
	ret
DIAPAZON ENDP



;****************************************************
;* Проверка допустимости вводимых символов			*
;* Аргументы:										*
;* 		Буфер ввода - stroka						*
;*		si - номер символа в строке					*
;* 													*
;* Результат:										*
;* 		bh - флаг ошибки ввода						*
;****************************************************
DOPUST PROC

	xor bh, bh
    xor si, si
	xor ah, ah
	xor ch, ch
	
	mov cl, kol	; В (cl) количество введенных символов
	
m11:	
	mov al, [stroka + si] 	; B (al) - первый символ
	cmp al, 2dh	; Является ли символ минусом
	jne testdop	; Если не минус - проверка допустимости
	cmp si, 00h	; Если минус  - является ли он первым символом
	jne error2	; Если минус не первый - ошибка
	jmp m13
	
testdop:
	cmp al, 30h	;Является ли введенный символ цифрой
	jb error2
	cmp al, 39h
	ja error2
	
m13: 	
	inc si
	loop m11
	jmp m14
	
error2:	
	mov bh, flag_err	; При недопустимости символа bh = flag_err
	
m14:	
	ret
DOPUST ENDP



;****************************************************
;* ASCII to number									*
;* Аргументы:										*
;* 		B cx количество введенных символов			*
;*		B bx - номер символа начиная с последнего 	*
;* 													*
;* Результат:										*
;* 		Буфер чисел - number						*
;*		B di - номер числа в массиве				*
;****************************************************
AscToBin PROC
	xor ch, ch
	mov cl, kol
	xor bh, bh
	mov bl, cl
	dec bl
	mov si, 01h  ; В si вес разряда
	
n1:	
	mov al, [stroka + bx]
	xor ah, ah
	cmp al, 2dh	; Проверим знак числа
	je otr	; Eсли число отрицательное
	sub al,	30h
	mul si
	add [number + di], ax
	mov ax, si
	mov si, 10
	mul si
	mov si, ax
	dec bx
	loop n1
	jmp n2
otr:	
	neg [number + di]	; Представим отрицательное число в дополнительном коде
	
n2:	
	ret
AscToBin ENDP



;****************************************************
;* Number to ASCII									*
;* Аргументы:										*
;* 		Число передается через ax					*
;* 													*
;* Результат:										*
;* 		Буфер чисел - out_str						*
;****************************************************
BinToAsc PROC
	xor si, si
	add si, 05h
	mov bx, 0Ah
	push ax
	cmp ax, 00h
	jnl mm1
	neg ax
	
mm1:	
	cwd
	idiv bx
	add dl,30h
	mov [out_str + si], dl
	dec si
	cmp ax, 00h
	jne mm1
	pop ax
	cmp ax, 00h
	jge mm2
	mov [out_str + si], 2dh
	
mm2:	
	ret
BinToAsc ENDP
c1 ENDS	
end start