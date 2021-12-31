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
  ;неисполняемый макрос и метка для доступа к нему.
  jumpOver macro markname,macroName,trigger,macroParam
    jmp markname
    ifb <macroParam>
      trigger:
      macroName
    else
      trigger:
      macroName macroParam
    endif
    ret
    markname:
  endm
  decr macro reg
    dec reg
  endm
put macro param
  lea ax,param
  push ax
endm


  EndOfDay macro
    mov ax, 4C00h
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
  ;throw
  throw MACRO str
    println str
    endProcess
  ENDM
  ;--------------------------------------------------------------------------------------
  ;ErrorMacroBlock
  StartErrorMacroBlock MACRO 
    wrongInput:
      throw wrongInputError
    overflow:
      throw overflowError
    wrongNumberOfNumbers:
      throw wrongNumberOfNumbersError
  ENDM
  ;--------------------------------------------------------------------------------------
  ;ErrorBlock
  ErrorBlock macro idName
    idName&wrongInput:
      jmp wrongInput
    idName&overflow:
      jmp overflow
    idName&wrongNumberOfNumbers:
      jmp wrongNumberOfNumbers
  endm
  ;--------------------------------------------------------------------------------------
  ;Apply
  apply MACRO procedure,a1,a2,a3,a4,a5,a6,a7
    push ax
    put_params a1,a2,a3,a4,a5,a6,a7
    call procedure
    ifb <a7>
    else
      pop ax
    endif
    ifb <a6>
    else
      pop ax
    endif
    ifb <a5>
    else
      pop ax
    endif
    ifb <a4>
    else
      pop ax
    endif
    ifb <a3>
    else
      pop ax
    endif
    ifb <a2>
    else
      pop ax
    endif
    ifb <a1>
    else
      pop ax
    endif
    pop ax
  ENDM 
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
;arg++
Increment PROC
  push bp
  mov bp,sp

  mov bx,[bp + 4]
  mov ax,ds:[bx]
  inc ax
  mov ds:[bx],ax

  pop bp 
  ret
Increment ENDP
;процедура, которая не делает ничего
idle PROC
  ret
idle ENDP
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

;temp if tests
keepIFVal PROC
  initf
  mov bx,[bp + 4]
  mov ax,[bx]
  return
keepIFVal ENDP

;description
onTrueF PROC
  initf
  mov bx,[bp + 4]
  mov ax,[bx]
  println onTrueStr
  return
onTrueF ENDP

;description
onFalseF PROC
  initf
  mov bx,[bp + 4]
  mov ax,[bx]
  println onFalseStr
  return
onFalseF ENDP





;......................................................................................
;MAIN
main:
  mov ax,data
  mov ds,ax

  apply ifelse,keepIFVal,num,onTrueF,onFalseF,num
  jmp
pred1:

ifelseEnd:
  
processEnd:
  EndOfDay
ends cat
;######################################################################################
;DATA SEGMENT
data segment
  ;template data
  endl            db 0dh,0ah,'$'
  ifval           db 1h
  counterI        dw 0h
  counterJ        dw 0h
  counterK        dw 0h
  onProcessEnd    db 'shutting down...$'
  overflowError   db 'Error: Numeric overflow!$'
  wrongInputError db 'Error: Wrong Input!$'
  wrongNumberOfNumbersError db 'Error: Wrong number of numbers, try other$'
  divideByZeroError db 'Error: tryed to divide by zero somewhere'
  ;there
  num             db 44h
  onTrueStr       db 'TRUE$'
  onFalseStr      db 'FALSE$'
  data ends
coda segment
  assume cs:coda,ds:data,ss:stk
  wellwell PROC
    push bp
    mov bp,sp
    mov ax,[bp + 4]
    pop bp
    retf
  wellwell ENDP
ends coda
end main
