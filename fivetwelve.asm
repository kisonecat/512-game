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
changed:	equ base+18	; random seed	
	
graphics_mode:	
        mov ax,0xa000   ; Set DS = 0xA000, video memory
        mov ds,ax
        mov ax,0x0013   ; BIOS set VGA mode 13h
        int 0x10

	mov word [seed], 17 	; initialize random seed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_init:
	mov di, 0
clear_spot:
	mov byte [board+di], 0
	inc di
	cmp di, 16
	jnz clear_spot

	call add_if_any_empty
	call add_if_any_empty	
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
	mov byte [changed], 1	; and remember something changed
different_blocks:		; else nothing to do 
	ret

merge_all_blocks:
	push bx
%rep 3
	call merge_blocks
	add bx, di
%endrep
	pop bx
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; merge blank space at bx and bx+di
merge_blanks:
	mov al, [bx+di]		; if [bx+di] is zero...
	cmp al, 0
	jne nonempty_blanks	
	xchg al, [bx]		; then swap [bx] and [bx+di]
	mov [bx+di], al
	or byte [changed], al	; and remember something changed
nonempty_blanks:		; else...
	ret			; do nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; push tiles to one side
merge_all_blanks:
	push bx			; save the start of the board
%rep 3				; for each of the 3 neighboring pairs of pairs
	call merge_blanks	; push the tiles over
	add bx, di		; move to next pair
%endrep	
	pop bx			; restore the pointer to the board
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; push tiles, combine matches, and push again
merge_and_move:
%rep 3
	call merge_all_blanks	; push tiles to the side
%endrep
	call merge_all_blocks	; combine two neighboring matching tiles
%rep 3
	call merge_all_blanks	; combining tiles may have result in gaps; push tiles again
%endrep	
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add a random block at a blank space on the board
add_if_any_empty:	
	mov bx, board		; bx points to the start of the board

	mov di, 0
check_spot:
	mov al, [bx+di]	
	cmp al, 0
	jz add_random_block
	
	inc di	
	cmp di, 16
	jnz check_spot
	ret

add_random_block:
try_again:
	;;; get random number between 0 and 16
	mov bx, [seed]		; load seed into bx
	
	mov ax, bx		; seed = seed xor (seed << 7)
	shl ax, 7
	xor bx, ax

	mov ax, bx		; seed = seed xor (seed << 9)
	shr ax, 9
	xor bx, ax

	mov ax, bx		; seed = seed xor (seed << 8)
	shl ax, 8
	xor bx, ax

	mov [seed], bx		; store transformed seed 

	mov di, bx		; di = random number between 0 and 15
	and di, 15
	
	cmp byte [board+di], 0	; is that spot on the board empty?
	jne try_again		; if nonempty, get another random spot
	mov byte [board+di], 1	; put a "2" tile in that spot
	
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform the state of the game based on player input
_update:
	mov ah, 0		; read a key (blocking) from bios
	int 0x16		; the scan code is in ah
	
	cmp ah, 0x01		; escape key resets game
	je _init

	mov byte [changed], 0	; changed = 0, to record if anything changes
	
	cmp ah, 0x48		; jump to handlers for arrow keys
	je keyup
	cmp ah, 0x4b
	je keyleft
	cmp ah, 0x4d
	je keyright
	cmp ah, 0x50
	je keydown

	;; if we are here, we typed a key we ignore
	hlt			; wait for something to happen
	jmp _update		; continue the game loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
keydown:
	mov di, 4
%assign i 0
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+1
%endrep	
	jmp _draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyleft:
	mov di, -1

%assign i 3
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+4
%endrep		
	jmp _draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyright:
	mov di, 1
%assign i 0
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+4
%endrep
	jmp _draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyup:
	mov di, -4
%assign i 12
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+1
%endrep	
	jmp _draw		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
_draw:
	cmp byte [changed], 0
	je nothing_changed
	call add_if_any_empty	; if there's a blank space somewhere, add a new "2" tile
nothing_changed:
	
	mov dx,4		; for dx = 4 ... 0, the row
draw_row:
	mov cx,4		; for cx = 4 ... 0, the column
draw_col:
	mov di, dx		; di := 4*dx + cx
	shl di, 2
	add di, cx
	mov al, byte [board+di-1-4] ; al := board[4*(dx-1) + (cx-1)]
	
	call drawbox		; draw the tile with value al at (cx,dx)

	dec cx			; cx := cx - 1
	jnz draw_col		; continue unless cx == 0
	
	dec dx			; dx := dx - 1
	jnz draw_row		; continu unless dx == 0
	
	;; clear keyboard buffer directly
	push ds			; save segment pointers
	push es
	mov ax,0x40		; es := 0x0040
	mov es,ax
	mov ds,ax		; ds := 0x0040
	mov di,0x1a
	mov si,0x1c
	movsw			; set 0x40:0x1c = 0x40:0x1a

	pop es			; restore segment pointers
	pop ds
	
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
	call drawrect		

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
