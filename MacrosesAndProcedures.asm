;ЧИСТЫЕ макросы и процедуры
;---------------------------------------------
;вывод без переноса строки
print macro str
    push    ax
    push    dx
    mov     dx, offset str
    mov     ah, 09h
    int     21
    pop     dx
    pop     ax
endm

;---------------------------------------------
;вывод с переносом строки
println macro str
    ifb <str>
        print endl
    else
        print str
        print endl
    endif
endm

data segment
    endl db 0dh,0ah,'$'
ends data