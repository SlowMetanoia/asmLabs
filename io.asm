kode segment
org 100h
assume cs: kode, ds: data;
begin:
        mov ax, data
        mov ds, ax
; ����� ������ �� ���� ���� �����
        mov ah, 09h
        mov dx, offset zapros
        int 21h
; ���� ���� �����
        mov cx, 2
inpt:  mov ah, 08h
        int 21h
        ; �������� ������?
        cmp al, '0'
        jb inpt       ; ���� ���� ���� ��������� ����
        cmp al, '9'
        ja inpt       ; ���� ������ ������ ��������� ����
        ; ����� �����
        mov ah, 02h
        mov dl, al
        int 21h
        loop inpt     ; ����� ��������� �����
; �������� ��� ��������� �����������
        mov ah, 01h
        int 21h
; ����� 
;        mov ax, 4C00h
;        int 21h
;wait
        inc ax
        mov ah,09h
        mov dx,offset String
        int 21h

        mov ah,10h
        int 16h
        mov ah,4ch
        mov al,0
        int 21h
kode ends
;--------------------------------------------------
data segment
        zapros db 'Enter two numbers ->  $'
        String db 'press any key...$'
data ends
;--------------------------------------------------
stk segment stack
        db 100h dup (0)
stk ends
;--------------------------------------------------
end begin
