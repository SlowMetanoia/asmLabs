CSEG segment
org 100h
Start:
;����� ������ � ������-�� ������ (������ �����? ������ ��������� �� ���������?)
    mov al,0
    mov ah,9
    mov dx,offset String
    int 21h

    mov ah,10h
    int 16h
    mov ah,4ch
    mov al,0
    int 21h
;���������� ( ���������� � ������ db(double byte) ����)
String db 'press any key...$'
CSEG ends
end Start
