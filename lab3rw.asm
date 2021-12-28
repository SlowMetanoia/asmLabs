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
  ;завершение процесса
  endProcess macro
    println
    println onProcessEnd
    ;;pause
    ;;Не знаю почему, но мне кажется, завершать программу 4 карбоксильными группами - это достойно!
    mov ax, 4C00H
    int 21h
  endm
  ;--------------------------------------------------------------------------------------
  ;throw
  throw MACRO str
    println str
    endProcess
  ENDM
  ;--------------------------------------------------------------------------------------
  ;почитать строчку в str
  readLine macro str
    push cx
    lea dx,str
    mov ah,0ah
    int 21h
    println
    pop cx
  endm
  ;--------------------------------------------------------------------------------------
  ;вытаскивание последнего аргумента
  movLastArg macro reg
    mov reg,[bp + 4]
  endm









;######################################################################################
;STACK SEGMENT
stk segment stack
  db 100h dup (0)
stk ends

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
  overflowError   db 'Error: Numeric overflow!$'
  wrongInputError db 'Error: Wrong Input!$'
  wrongNumberOfNumbersError db 'Error: Wrong number of numbers, try other$'
  divideByZeroError db 'Error: tryed to divide by zero somewhere'
  ;there
  inputBark       db 'Input number of numbers:$'
  numbersBark     db 'Input your numbers:$'
  resultBark      db 'That is your result.$'
  divminBark      db 'Sum divided by min:$'
  divmaxBark      db 'Sum divided by max:$'
  minElemBark     db 'Min element:$'
  maxElemBark     db 'Max element:$'
  sumBark         db 'Sum:$'
  needTimeBark    db 'Not enough time, so not yet implemented... sorry :(',10,13,'sum and max must be > 0'
  stringFromInt   db 10 dup('$')
  maxInput        db 20
  inputLength     db 0h
  inputStart      db 21 dup(?)
  minElem         dw 7fffh
  maxElem         dw 8000h
  numberOfNumbers dw 0h
  numbers         dw 256 dup(?)
  outputInt       dw 0h
  sum             dw 0h

data ends

;######################################################################################
;CODE SEGMENT
cat segment
  org 100h
  assume cs:cat,ds:data,ss:stk

resetOutput PROC
  initf
  
  lea bx,stringFromInt
  mov di,bx
  mov cx,9
outputResetLoop:
  mov [di],'$'
  loop outputResetLoop

  return
resetOutPut ENDP

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
  ;перевод положительного числа в строку
  ;====================================================================================
int2String PROC
  initf

  mov ax,[bp + 4]
  cmp ax,10
  jb less
  xor dx,dx
  mov bx,10
  div bx
  push dx
  push ax
  call int2String
  pop ax
  pop dx
  add dx,'0'
  mov [di],dx
  inc di
  return
less:
  add al,'0'
  mov [di],al
  inc di
  return
int2String ENDP
  ;====================================================================================
anyInt2String PROC
  initf

  call resetOutPut
  mov ax, [bp + 4]
  mov bx, ax
  shl bx, 1
  jnc positiveInt

  mov [di],'-'
  inc di
  neg ax
positiveInt:
  push ax
  call int2String
  pop bx

  return
anyInt2String ENDP

  ;перевод строки в число
  ;====================================================================================
string2Int PROC
  initf

  push cx


  mov bx, [bp + 6]
  mov cl, [bx]
  mov ax, [bp + 4]
  mov si, ax

  xor dx, dx
  xor bx, bx

  mov bl, [si]
  cmp bl, '-'
  jne positive

  mov dx, 1
  inc si
  dec cx
positive:
  push dx
  xor ax, ax
  xor bx, bx
  xor dx, dx
readLoop:
  mov bx, 10
  mul bx
  xor bx, bx
  cmp dx, 0
  jne overflow
  mov bl, [si]
  cmp bl, '0'
  jb  wrongInput
  cmp bl, '9'
  ja  wrongInput
  sub bl, '0'
  add ax, bx
  inc si
  loop readLoop
  pop dx
  cmp dx,0
  je notNegative
  neg ax
  jo overflow
notNegative:

  pop cx
  return
wrongInput:
  throw wrongInputError
overflow:
  throw overflowError
wrongNumberOfNumbers:
  throw wrongNumberOfNumbersError
string2Int ENDP



;......................................................................................
;MAIN
main:
  mov ax,data
  mov ds,ax

  println inputBark
 ;число вводимых чисел
  readLine maxInput
  put_params inputLength, inputStart
  call string2Int
  mov numberOfNumbers,ax
  cmp ax, 255
  jo  wrongNumberOfNumbers
  ja  wrongNumberOfNumbers
  pop bx
  pop bx
  mov ax,numberOfNumbers

  println 
  println numbersBark
  ;ввод чисел
  mov cx,numberOfNumbers
  lea di,numbers
inputLoop:
  readLine maxInput
  put_params inputLength, inputStart
  call string2Int
  mov [di],ax
  pop ax
  pop ax
  inc di
  inc di
  loop inputLoop

  ;вычисление максимума и минимума
  mov cx, numberOfNumbers  
  lea si, numbers
  mov bx, minElem
  mov dx, maxElem
minMaxLoop:
  mov ax,[si]
  inc si
  inc si
  cmp ax,bx
  jl  lessMin
  jmp maxCheck
lessMin:
  mov bx,ax
maxCheck:
  cmp ax,dx
  jg moreMax
  jmp cycle
moreMax:
  mov dx,ax
cycle:
  loop minMaxLoop
  mov minElem,bx
  mov maxElem,dx

  ;вывод минимума и максимума
  lea di, stringFromInt
  mov ax,minElem
  push ax
  call anyInt2String
  pop ax
  println minElemBark
  println stringFromInt

  lea di, stringFromInt
  mov ax,maxElem
  push ax
  call anyInt2String
  pop ax
  println maxElemBark
  println stringFromInt


  ;вычисляем сумму всех, что делятся на 10
  mov cx, numberOfNumbers
  lea si, numbers
  xor ax,ax
  xor dx,dx
divTenSumLoop:
  xor dx,dx
  mov ax,[si]
  cmp ax,0
  js  negativeSum
  mov bx,10
  div bx
  cmp dx, 0
  jne countinueLoop
  mov bx,[si]
  add sum,bx
  jo  overflow2
  jmp countinueLoop
negativeSum:
  neg ax
  mov bx,10
  div bx
  cmp dx, 0
  jne countinueLoop
  mov bx,[si]
  neg bx
  sub sum,bx
  jo  overflow2
countinueLoop:
  inc si
  inc si
  loop divTenSumLoop
  mov ax,sum
  lea di, stringFromInt
  push ax
  call anyInt2String
  pop ax
  println sumBark
  println stringFromInt
  mov ax,sum
  cmp ax,0
  js needTime

  jmp jumpOver
overflow2:
  jmp overflow
needTime:
  throw needTimeBark
divZ:
  jmp divZero
jumpOver:
  ;делим сумму на минимальный и максимальный эелемент + выводим это.
  println divminBark
  xor dx, dx
  mov ax, sum
  mov bx, minElem
  cmp bx,0
  je divZ
  cmp bx,0
  js divMinNeg
  div bx
  push ax
  lea di, stringFromInt
  call anyInt2String
  pop ax
  println stringFromInt
  jmp divMax
divMinNeg:
  neg bx
  div bx
  neg ax
  push ax
  lea di, stringFromInt
  call anyInt2String
  pop ax
  println stringFromInt
  jmp divMax
dZ:
  throw divideByZeroError
divMax:
  println divmaxBark
  xor dx, dx
  mov ax, sum
  mov bx, maxElem
  cmp bx,0
  je dZ
  cmp bx,0
  js divMaxNeg
  div bx
  push ax
  lea di, stringFromInt
  call anyInt2String
  pop ax
  println stringFromInt
  endProcess
divMaxNeg:
  throw needTimeBark
  neg bx
  div bx
  neg ax
  push ax
  lea di, stringFromInt
  call anyInt2String
  pop ax
  println stringFromInt

theEnd:
  endProcess
divZero:
  throw divideByZeroError
ends cat

end main