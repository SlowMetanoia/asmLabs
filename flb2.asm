  ;------------------------------------------------------
  ;������� ��������� ������ � ����������, ����� � ax
  nextCharAX macro
    mov ah, 08h
    int 21h
  endm
  ;------------------------------------------------------
  ;����� ��� �������� ������
  print macro str
    push    ax
    push    dx
    mov     dx, offset str
    mov     ah, 09h
    int     21
    pop     dx
    pop     ax
  endm
  ;------------------------------------------------------
  ;����� � ��������� ������
  println macro str
    ifb <str>
      print endl
    else
      print str
      print endl
    endif
  endm

  ;------------------------------------------------------
  ;�����
  pause macro
    push ax
    mov ah,10h
    int 16h
    pop ax
  endm
  ;------------------------------------------------------
  ;���������� ��������
  endProcess macro
    println
    println onProcessEnd
    ;pause
    ;�� ���� ������, �� ��� �������, ��������� ��������� 4 �������������� �������� - ��� ��������!
    mov ax, 4C00h
    int 21h
  endm
  ;������ � ������ � ���������.
  prol macro
    push bp
    mov bp,sp
  endm
  return macro
    pop bp
    ret
  endm
  
  ;code--------------------------------------------------
cat segment
org 100h
assume cs:cat, ds:data, ss:stk
;���������.
  outp proc 
    println outStr2
    ret
  outp endp
  increment proc
    push bp
    mov bp,sp
    mov bx,[bp+4]
    mov ax,[bx]
    inc ax
    mov [bx],ax
    pop bp
    ret
  increment endp 
main:
  mov ax,data
  mov ds,ax

  println outStr1
  call outp
  lea bx,number
  push bx
  call increment
  endProcess
cat ends
  ;data segment--------------------------------
data segment
  outStr1 db 'less ops!? $'
  outStr2 db 'less everything! $'
  onProcessEnd db "ending this up!$"
  endl db 0dh,0ah,'$'
  number db 0h
data ends  
;stack segment-------------------------------
stk segment stack
  db 100h dup (0)
stk ends
end main
