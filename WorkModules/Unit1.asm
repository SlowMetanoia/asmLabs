.386

.model flat


;place your data here
.data
String db 'press any key...$'
;place your executable code here
.code
start:  
    mov ah,9
    mov dx,offset String
    int 21h

    mov ah,10h
    int 16h
    mov ah,4ch
    mov al,0
    int 21h
end start
