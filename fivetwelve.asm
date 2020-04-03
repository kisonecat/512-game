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
changed:	equ base+18	; mark if anything changed
px:	equ base+20
py:	equ base+22
	
graphics_mode:
	push 0xA000		; ds = 0xA000
	pop ds

        mov ax,0x0013   ; BIOS set VGA mode 13h
        int 0x10
        cld		; FIXME: is this needed?
	
	inc word [seed]		; initialize random seed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_init:
	push ds
	pop es

	mov di, board
	mov cx, 16
	mov al, 0
	rep stosb

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
	push ds		; es := ds
	pop es
	
	mov cx, 16
	mov di, board
	mov al, 0
	repne scasb
	jz add_random_block
	
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
	hlt			; wait for something to happen

	;; wait for key press ("make")
	in al,60h
	test al,0x80
	jz _update
	
keypress:
	;; wait for key release ("break")
	in al,60h
	test al,0x80
	jnz keypress
	
	and al,0x7F		; get the scan code from the "break" code
	
	cmp al, 0x01		; escape key resets game
	je _init

	mov byte [changed], 0	; changed = 0, to record if anything changes
	
	push _draw
	cmp al, 0x48		; jump to handlers for arrow keys
	je keyup
	cmp al, 0x4b
	je keyleft
	cmp al, 0x4d
	je keyright
	cmp al, 0x50
	je keydown

	ret
	;; if we are here, we typed a key we ignore

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
keydown:
	mov di, 4
	mov bx, board
%rep 4	
	call merge_and_move
	inc bx	
%endrep	
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyleft:
	mov di, -1

%assign i 3
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+4
%endrep		
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyright:
	mov di, 1
%assign i 0
%rep 4	
	mov bx, board+i
	call merge_and_move
%assign i i+4
%endrep
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
keyup:
	mov di, -4
	mov bx, board+12
%rep 4	
	call merge_and_move
	inc bx
%endrep	
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
_draw:
	cmp byte [changed], 0
	je nothing_changed
	call add_if_any_empty	; if there's a blank space somewhere, add a new "2" tile
nothing_changed:
	
	mov dh,4		; for dh = 4 ... 0, the row
draw_row:
	mov dl,4		; for dl = 4 ... 0, the column
draw_col:
	mov bx, dx		; bx := 4*dh + dl
	shl bh, 2
	add bh, dl
	shr bx, 8
	
	mov al, byte [bx+board-1-4] ; al := board[4*(dx-1) + (cx-1)]

	;; draw the tile with value al at 
	push dx
	push ax

	dec dl			; dl and dh are between 0 and 3
	dec dh

	mov al, dl		; ax = dl * 40
	mov bl, 40
	mul bl
	add ax, 80
	push ax			
	
	mov al, dh		; ax = dh * 40
	mov bl, 40
	mul bl
	add ax, 8*2
	pop bx			; bx = dl*40, ax = dh*40

	mov cx, 320		; ax = (dh * 40) * 320
	mul cx
	add bx, ax		; bx = (dl*40)*320 + (dh*40)

	pop ax

	mov di, bx

	push ds			; es := ds
	pop es

	mov bx, 40
	
next_line:	
	mov cx, 40
	rep stosb
	add di, 320-40
	dec bx
	jnz next_line

	pop dx
	push dx
	
	dec dl
	dec dh

	mov bx, dx
	
	shl dl, 2
	add dl, bl

	shl dh, 2
	add dh, bh

	add dl, 11
	add dh, 2+2
	
	mov ah, 0
	mov bp, numbers
	mov bl, 3
	mul bl
	add bp, ax

	push cs 		; es := cs
	pop es

	mov ax, 0x1300
	mov bx, 15
	mov cx, 3
	int 0x10

	pop dx			; dh,dl = the position

	dec dl			; dl := dl - 1
	jnz draw_col		; continue unless dl == 0
	
	dec dh			; dh := dh - 1
	jnz draw_row		; continue unless dh == 0
	
	jmp _update		; end of _draw



numbers: db '    2  4  8  16 32 64128256512'
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thanks to Oscar Toledo G. for this!
;;;
;;; "The last two bytes must be 0x55, 0xAA to be recognized as a boot sector."
%ifdef comfile
%else
        times 510-($-$$) db 0x4f
        db 0x55,0xaa            ; Make it a bootable sector!
%endif
