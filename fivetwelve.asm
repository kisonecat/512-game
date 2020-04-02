	;; The 512 Game -- like 2048, but fiting in a boot sector
	;;
	;; (c) 2020, Jim Fowler
	;; 
	;; This program is free software: you can redistribute it
	;; and/or modify it under the terms of the GNU General Public
	;; License as published by the Free Software Foundation, either
	;; version 3 of the License, or (at your option) any later
	;; version.
	;; 
	;; This program is distributed in the hope that it will be
	;; useful, but WITHOUT ANY WARRANTY; without even the implied
	;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
	;; PURPOSE.  See the GNU General Public License for more details.
	;;
	;; You should have received a copy of the GNU General Public
	;; License along with this program.  If not, see
	;; <https://www.gnu.org/licenses/>.

%ifdef comfile
        org 0x0100      ; Code for COM files is loaded at 100h
%else
        org 0x7c00      ; Boot sector is loaded at 7C00h
%endif

base:	equ 0xfc80      ; Base of memory (in video!)
board:	equ base	; 4x4 array for the current board
seed:	equ base+16	; random seed
	
graphics_mode:	
        mov ax,0xa000   ; Set DS = 0xA000, video memory
        mov ds,ax
        mov ax,0x0013   ; BIOS set VGA mode 13h
        int 0x10
        cld		; FIXME: why is this needed?

	mov word [seed], 17 	; initialize random seed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_init:
	mov bx, board
	mov di, 0
	mov cx, 16
clear_spot:
	mov byte [bx+di], 0
	inc di	
	dec cx
	jnz clear_spot

	jmp _draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; merge block at bx and bx+di
;;; destroys al but preserves other registers
merge_blocks:
	mov al, [bx+di]		; if [bx+di] is zero...
	cmp al, 0
	je different_blocks	; then do nothing
	cmp al, [bx]		; else if the two blocks are the same
	jne different_blocks
	inc byte [bx]	    	; then merge the blocks...
	mov byte [bx+di], 0	; but leave an empty space so additional blocks are not merged
different_blocks:		; else nothing to do 
	ret

merge_all_blocks:
	push bx
	call merge_blocks
	add bx, di
	call merge_blocks
	add bx, di
	call merge_blocks
	add bx, di	
	pop bx
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; merge blank space at bx and bx+di
;;; destroys ax but preserves other registers	
merge_blanks:
	mov al, [bx+di]		; if [bx+di] is zero...
	cmp al, 0
	jne nonempty_blanks	
	xchg al, [bx]		; then swap [bx] and [bx+di]
	mov [bx+di], al
nonempty_blanks:		; else...
	ret			; nothing to do 

merge_all_blanks:
	push bx
	call merge_blanks
	add bx, di
	call merge_blanks
	add bx, di
	call merge_blanks
	add bx, di	
	pop bx
	ret	

merge_and_move: 
	call merge_all_blanks
	call merge_all_blanks
	call merge_all_blanks
	call merge_all_blocks
	call merge_all_blanks
	call merge_all_blanks
	call merge_all_blanks
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get random number between 0 and 16
random:
	push bx
	
	mov ax, [seed]
	mov bx, [seed]
	
	shl ax, 7
	xor bx, ax

	mov ax, bx
	shr ax, 9
	xor bx, ax

	mov ax, bx
	shl ax, 8
	xor bx, ax

	mov [seed], bx
	mov ax, bx
	and ax, 15
	
	pop bx

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clear keyboard buffer directly
clearkey:
	push ds
	push es
	mov ax,$40
	mov es,ax
	mov ds,ax
	mov di,$1a
	mov si,$1c
	movsw
	pop es
	pop ds
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add a random block at a blank space on the board
add_random_block:
try_again:	
	call random
	mov di, ax
	mov ah, [board+di]
	cmp ah, 0
	jne try_again
	mov byte [board+di], 1
	ret

add_if_any_empty:	
	mov bx, board
	mov di, 0
	mov cx, 16
check_spot:
	mov al, [bx+di]
	cmp al, 0
	jz add_random_block
	
	inc di	
	dec cx
	jnz check_spot
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
_update:
	mov ah, 0
	int 0x16
	cmp ah, 0x48
	je keyup
	cmp ah, 0x4b
	je keyleft
	cmp ah, 0x4d
	je keyright
	cmp ah, 0x50
	je keydown
	hlt
	jmp _update

keydown:
	mov di, 4
	
	mov bx, board
	call merge_and_move

	mov bx, board+1
	call merge_and_move

	mov bx, board+2
	call merge_and_move

	mov bx, board+3
	call merge_and_move

	jmp _draw

keyleft:
	mov di, -1
	mov bx, board+3
	call merge_and_move

	mov bx, board+7
	call merge_and_move

	mov bx, board+11
	call merge_and_move

	mov bx, board+15
	call merge_and_move
	
	jmp _draw

keyright:
	mov di, 1
	mov bx, board
	call merge_and_move

	mov bx, board+4
	call merge_and_move

	mov bx, board+8
	call merge_and_move

	mov bx, board+12
	call merge_and_move
	
	jmp _draw

keyup:
	mov di, -4
	mov bx, board+12
	call merge_and_move

	mov bx, board+13
	call merge_and_move

	mov bx, board+14
	call merge_and_move

	mov bx, board+15
	call merge_and_move

	jmp _draw		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
_draw:
	call add_if_any_empty
	
	mov dx,4
draw_row:
	mov cx,4
draw_col:
	mov di, dx
	shl di, 2
	add di, cx
	mov al, byte [board+di-1-4]
	
	call drawbox	

	dec cx
	jnz draw_col
	dec dx	
	jnz draw_row

	call clearkey
	hlt			; end of _draw
	jmp _update	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw rectangle in color al at (cx,dx)
drawrect:
WIDTH:	equ 48
HEIGHT:	equ 48
	
	mov ah,0x0c 		; prepare bios call to set pixel

	mov bx,WIDTH		; loop on bx
	add dx,HEIGHT
rect_col:
	push bx
	
	mov bx,HEIGHT		; inner loop on bx
	sub dx,HEIGHT
rect_row:
	int 0x10		; bios call to set pixel
	
	inc dx			; move down a row
	dec bx			; countdown until finished with row
	jnz rect_row
	
	pop bx
	inc cx			; move onto next row
	dec bx			; countdown until finished with col
	jnz rect_col

	ret
	
;;; draw value al at (cx,dx)
;;; preserves cx, dx
drawbox:
	push cx
	dec cx
	push dx
	dec dx
	
	push ax
	
	mov ax, 50
	mul cl
	mov cx, ax

	mov ax, 50
	mul dl
	mov dx, ax	

	pop ax
	inc cx
	inc dx
	add cx, 60
	;call drawrect		

	pop dx
	pop cx

	push cx
	push dx

	mov ah, 0x02
	mov bh, 0
	mov dh, cl
	xchg dh, dl
	int 0x10
	
	mov bh, 0 		; write character
	mov ah, 0x09
	mov bl, 15
	mov cx, 1
	or al, 48
	int 0x10
	
	
	pop dx
	pop cx
	ret
	
%ifdef comfile
%else
        times 510-($-$$) db 0x4f
        db 0x55,0xaa            ; Make it a bootable sector
%endif
