  ;######################################################################################
  ;MACROSES
  ;положить ссылку на что-то в стек
  put macro param
    lea ax, param
    push ax
  endm
  ;--------------------------------------------------------------------------------------
  ;WHY DO YOU BELIEVE IN MONTERS?
  ;закидывание нескольких параметров в стек по ссылке
  put_params macro a1,a2,a3,a4,a5,a6,a7
  ifb<a7>
    ifb<a6>
      ifb<a5>
        ifb<a4>
          ifb <a3>
            ifb <a2>
              ifb <a1>
              else 
                put a1
              endif
            else
              put a1
              put a2
            endif
          else 
            put a1
            put a2
            put a3
          endif
        else
          put a1
          put a2
          put a3
          put a4
        endif
      else
        put a1
        put a2
        put a3
        put a4
        put a5
      endif
    else
      put a1
      put a2
      put a3
      put a4
      put a5
      put a6
    endif
  else
    put a1
    put a2
    put a3
    put a4
    put a5
    put a6
    put a7
  endif
  endm

  ;считать следующий символ с клавиатуры, вывод в ax
  nextCharAX macro
    mov ah, 08h
    int 21h
  endm
  ;--------------------------------------------------------------------------------------
  ;вывод без переноса строки
  print macro str
    push    ax
    push    dx
    mov     dx, offset str
    mov     ah, 09h
    int     21h
    pop     dx
    pop     ax
  endm
  ;--------------------------------------------------------------------------------------
  ;вывод с переносом строки
  println macro str
    ifb <str>
      print endl
    else
      print str
      print endl
    endif
  endm
  ;--------------------------------------------------------------------------------------
  ;пауза
  pause macro
    push ax
    mov ah,10h
    int 16h
    pop ax
  endm
  ;--------------------------------------------------------------------------------------
  ;завершение процесса
  endProcess macro
    println
    println onProcessEnd
    ;;pause
    ;;Не знаю почему, но мне кажется, завершать программу 4 карбоксильными группами - это достойно!
    mov ax, 4C00h
    int 21h
  endm
  ;--------------------------------------------------------------------------------------
  ;if
  _if macro pred,value,f1,f2,param
    put_params pred,value,f1,f2,param
    call ifelse
    pop ax
    pop ax
    pop ax
    pop ax
    pop ax
  endm
  ;--------------------------------------------------------------------------------------
  ;вытаскивание последнего аргумента
  movLastArg macro reg
    mov reg,[bp + 4]
  endm
  ;--------------------------------------------------------------------------------------
  ;for
  _for macro init,pred,mutate,code,counter
    put_params init,pred,mutate,code,counter
    call forProc
    pop ax
    pop ax
    pop ax
    pop ax
    pop ax
  endm
  ;--------------------------------------------------------------------------------------
  ;пролог процедуры
  initf macro
    push bp
    mov bp,sp
  endm
  ;--------------------------------------------------------------------------------------
  ;эпилог процедуры
  return macro
    pop bp 
    ret
  endm
  ;--------------------------------------------------------------------------------------
  ;вернуть истину
  ifT macro
    mov al,1
    mov ifval,al
    return
  endm
  ;--------------------------------------------------------------------------------------
  ;вернуть ложь
  ifF macro
    mov al,0
    mov ifval,al
    return
  endm

;######################################################################################
;STACK SEGMENT
stk segment stack
  db 100h dup (0)
stk ends

;######################################################################################
;CODE SEGMENT
cat segment
  org 100h
  assume cs:cat,ds:data,ss:stk
;......................................................................................
;PROCEDURES
helloWorld proc
  mov     dx, offset hello
  mov     ah, 09h
  int     21h
  println hello
  ret
helloWorld endp 

;передача переменной по ссылке.
param_proc proc
  push bp
  mov bp,sp

  mov bx,[bp + 4]
  mov ax,ds:[bx]
  inc ax
  mov ds:[bx],ax

  pop bp 
  ret
param_proc endp 

;аппликация.
applicate proc
  push bp
  mov bp,sp

  ;процедура - первый параметр
  mov bx,[bp + 6]
  ;переменная - второй параметр
  mov ax,[bp + 4]

  ;заносим ссылку на переменную в стек
  push ax
  ;вызываем функцию
  call bx
  pop ax

  pop bp 
  ret
applicate endp 

;WHY DO YOU BELIEVE IN HELL?
;if (pred(value)) f1(param) else f2(param)
;5 аргументов: 
;pred, value - pred должен класть в ifval
;f1, f2, param
;результат - результат f1 или f2 возвращается через param
ifelse proc
  push bp
  mov bp,sp

  ;предикат
  mov bx, [bp + 12]
  ;параметр для предиката
  mov ax, [bp + 10]

  ;вычсляем предикат
  push ax
  call bx
  pop  ax

  ;собственно, ветвление
  mov al,ifval
  cmp al,1
  je  OnTrue
  jne OnFalse 

OnTrue:
  ;f1
  mov bx,[bp + 8]
  ;param
  mov ax,[bp + 4]
  push ax
  call bx
  pop ax
  jmp retIfElse

OnFalse:
  ;f2
  mov bx,[bp + 6]
  ;param
  mov ax,[bp + 4]
  push ax
  call bx
  pop ax
  jmp retIfElse

retIfElse:
  pop bp 
  ret
endp ifelse

;tmp ifelse tests
iftestPred proc
  mov al,testval
  mov ifval,al
  ret
endp iftestPred

ifTestF1 PROC
  mov dx,1
  println str1
  ret
ifTestF1 ENDP

ifTestF2 PROC
  mov dx,2
  println str2
  ret
ifTestF2 ENDP

;WHY DO YOU BELIEVE IN LOVE DIVINE?
;for(init(counter),pred(counter),mutate(counter)){code(counter)}
;5 аргументов:
;init     - функция инициализации счётчика
;pred     - предикат продолжения цикла
;mutate   - функция изменения счётчика
;code     - тело цикла
;counter  - счётчик
forProc PROC
  push bp
  mov bp,sp

  ;init
  mov bx,[bp+12]
  movLastArg ax
  push ax
  call bx
  pop ax

forLoop:
  ;pred
  mov bx,[bp+10]
  movLastArg ax
  push ax
  call bx
  pop ax
  ;проверяем значение предиката
  mov al,ifval
  cmp al,0
  je loopEnd
  ;выполняем тело цикла
  mov bx,[bp + 6]
  movLastArg ax
  push ax
  call bx
  pop ax
  ;изменяем счётчик
  mov bx,[bp + 8]
  movLastArg ax
  push ax
  call bx
  pop ax

  jmp forLoop

loopEnd:
  pop bp 
  ret
forProc ENDP
;WITH YOUR DARK AND TWISTED MIND...

;temp forTests
forInit PROC
  initf

  ;movLastArg ax
  movLastArg bx
  mov ax, 7h
  mov ds:[bx],ax

  return
forInit ENDP
;arg = 0
forZeroInit proc
  initf

  movLastArg bx
  mov ax, 0h
  mov ds:[bx],ax

  return
forZeroInit endp

;arg > 0
isAboveZero PROC
  initf

  movLastArg bx
  mov ax,ds:[bx]
  cmp ax,0h
  ja forPT
forPF:
  mov al,0
  mov ifval,al
  jmp forPEnd
forPT:
  mov al,1
  mov ifval,al

forPEnd:
  return
isAboveZero ENDP

;arg++
Increment PROC
  initf

  movLastArg bx
  mov ax,ds:[bx]
  inc ax
  mov ds:[bx],ax

  return
Increment ENDP

;arg--
Decrement PROC
  initf

  movLastArg bx
  mov ax,ds:[bx]
  dec ax
  mov ds:[bx],ax

  return
Decrement ENDP

;test2[i] = test1[i]
forTestCode PROC
  initf

  movLastArg bx
  mov bx,ds:[bx]
  dec bx
  lea dx,forTest1
  mov si,dx
  lea dx,forTest2
  mov di,dx
  mov ax,[si + bx]
  mov [di + bx],ax

  return
forTestCode ENDP

;--------------------------------------------------------------------------------------
;lab2 codes

;фуекция, которая не делает ничего
idle PROC
  nop
  ret
idle ENDP


;......................................................................................
;MAIN
main:
  mov ax,data
  mov ds,ax

  ;_if  iftestPred, ifval,        ifTestF1,   ifTestF2,     ifval
  ;_for init, predicat,  mutator,  body,  counter

  ;ввод значений
  ;_for forZeroInit, lab2InputForPred, Increment, lab2InputForBody, lab2Counter
  lea dx,lab2InputStart
  mov ah,0ah
  int 21h

  mov cl,lab2Counter

  cmp cl,0
  je  processEnd

  mov dl,lab2Counter
  shr dl,1
  inc dl
  mov lab2Counterdiv2,dx

  lea ax,lab2Output
  mov di,ax
  lea ax,lab2Input
  mov si,ax
loop1:
  mov bx,cx
  dec bx
  mov al,[si + bx]
  mov [di],al
  inc di
  cmp cx,lab2Counterdiv2
  je leaveLoop
  loop loop1
leaveLoop:
  dec cx
loop2:
  mov al,[si]
  mov [di],al
  inc si
  inc di
  loop loop2

  println
  println lab2Output

processEnd:
  nop
  mov ax, 4C00h
  int 21h
ends cat

;######################################################################################
;DATA SEGMENT
data segment
  ;template part
  endl            db 0dh,0ah,'$'
  ifval           db 0h
  counterI        dw 0h
  counterJ        dw 0h
  counterK        dw 0h
  onProcessEnd    db 'shutting down...$'
  ;there
  hello           db 'hello world $'
  somenum         dw 16h
  str1            db 'testStr1$'
  str2            db 'testStr2$'
  testval         db 0h

  forTest1        db 7 dup(7)
  forTest2        db 7 dup(?)

  lab2InputStart  db 19
  lab2Counter     db 0h
  lab2Input       db 20 dup(?)
  lab2Output      db 20 dup('$')
  lab2Counterdiv2 dw 0
data ends

end main