; ==============================================================================
; === Overlay support (aka "far calls") ========================================
; ==============================================================================

; The compiler is able to generate overlays for platforms that support RAM bank
; switching. Overlays are kept very simple. They are 8K in size, and normally
; only a single one is active at each time. There is no overlay manager and no
; relocation. The compiler generates far calls whenever a routine in an overlay
; is called from outside that overlay. A far call goes through a special
; "trampoline" that checks if the required overlay is the current one. If not,
; it switches the overlay bank, does the call, and restores the old memory
; configuration afterwards. In order to not have a changed stack layout for far
; calls (which is important for parameters and return values) there is a small
; local stack that holds the return addresses of the callers and the previously
; active overlays. Up to 16 levels of nested overlay switches are possible.
;
; A platform that supports overlays needs to provide a "banksel" function that
; switches to the overlay passed in A. In theory it is also possible to support
; overlays via disk files, but these would have to i) contain rather complex
; (= long-running) routines to outweigh the overhead of disk loading and ii) be
; compiled in non-absolute mode, so local variables are stored on the stack and
; don't get lost when the routine calls into another overlay.

globalsp:       ds      2                       ; Saved global stack pointer
localsp:        dw      mystack + 64            ; Pointer inside local stack
curpage:        db      0                       ; Currently active overlay
depth:          db      17                      ; Nesting levels left plus one, 17..1
mystack:        ds      64                      ; Local stack with 16 entries

; Performs a "far" call into an overlay. Switches overlays as needed and
; restores old memory configuration afterwards. There's a fast track: If no
; switching is needed we simply jump into the actual callee, making it a near
; call. There is no forced correlation between overlay numbers and RAM page
; numbers.
;
; TODO Find a good solution for initial case
;
; In:   A (overlay), HL (callee address)
; Out:  -
;
farcall:        ld      c,a
                ld      a,(curpage)             ; See if we need to switch bank
                cp      c
                jr      nz,farcall1
                jp      (hl)                    ; Fast lane, use callee's ret
farcall1:       di

                ld      b,a                     ; A holds curpage, needed below -- save
                ld      a,(depth)               ; it in B (free here, faster than
                dec     a                       ; push/pop) around the depth check
                jp      z,__ovlerr              ; No nesting levels left
                ld      (depth),a
                ld      a,b                     ; Restore curpage

                pop     de                      ; Fetch our return address
                ld      (globalsp),sp           ; Save the global SP
                ld      sp,(localsp)            ; Activate our local stack
                push    de                      ; Push return address to local stack
                push    af                      ; Push old RAM bank to local stack
                ;inc     sp                     ; Save a byte?
                ld      (localsp),sp            ; Save local SP
                ld      sp,(globalsp)           ; Activate global stack
                ld      a,c
                ld      (curpage),a
                call    banksel                 ; Change bank

                ei

                ld      de,farcall2             ; Perform the call, looks a bit ugly
                push    de
                jp      (hl)
farcall2:       di

                ld      hl,depth                ; HL is free here (not yet loaded with
                inc     (hl)                    ; the return address), no need to
                ld      b,(hl)                  ; protect A like in farcall1; stash the
                                                ; new depth in B, checked after the pops

                ld      (globalsp),sp           ; Save the global SP
                ld      sp,(localsp)            ; Activate our local stack
                ;dec     sp                     ; Save a byte?
                pop     af                      ; Fetch the old bank
                pop     hl                      ; Fetch our return address
                ld      (localsp),sp            ; Save local SP
                ld      sp,(globalsp)           ; Activate global stack

                ld      c,a                     ; Stash old bank (B holds depth)
                ld      a,b
                cp      17
                jr      z,farcall2_skip         ; Back to the outermost level: whatever
                                                ; is currently banked in is only ever
                                                ; read by other overlay code, never by
                                                ; the non-overlay caller we're returning
                                                ; to here, so leave curpage/hardware
                                                ; alone instead of switching back just
                                                ; to (maybe) switch away again next call

                ld      a,c
                ld      (curpage),a
                call    banksel                 ; Change bank
farcall2_skip:
                ei

                jp      (hl)                    ; Return to caller

__ovlerr:
        ld      hl,__ovlerr_message
        call    __puts
        jp      __done
__ovlerr_message:
        db      29,"Too many nested overlay calls"
