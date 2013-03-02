;;Let putty/mintty support more key-chords (such as Shift+F1, C-home)
;;by sending corresponding XTerm control sequences on PuTTY

;;References:
;;http://code.google.com/p/mintty/wiki/Keycodes
;;http://www.xfree86.org/current/ctlseqs.html#PC-Style%20Function%20Keys
;;http://en.wikipedia.org/wiki/ANSI_escape_code

;;`input-decode-map' in GNU Emacs (or: M-[ C-h, M-O C-h)
;;http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/xterm-extras.el
;;http://emacswiki.org/emacs/PuTTY#toc9 Using Emacs over PuTTY: how to use all function keys
;;http://emacswiki.org/emacs/PuTTY#toc10 ;Windows PuTTY client: xterm broken

;;http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
;;http://blog.akinori.org/2013/01/18/pasting-a-text-literally-to-emacs-under-terminal/
;;http://www.joshstaiger.org/archives/2005/04/fixing_the_righ.html

;;AutoHotkey:  Ctrl ^ Alt ! Shift +
;;XTerm control sequences: Shift 1, Alt 2;;Ctrl  4


;;putty/mintty/putty-nd/mobaxterm
#if WinActive("ahk_class PuTTY") or WinActive("ahk_class mintty") or WinActive("ahk_class SysTabControl32") or WinActive("ahk_class TMobaXtermForm") 

;;* ===== Fx ==========
;;** Shift+Fx
;;+F1::SendInput  {ESC}[1;2P
;;+F2::SendInput  {ESC}[1;2Q
;;+F3::SendInput  {ESC}[1;2R
;;+F4::SendInput  {ESC}[1;2S
;;xterm-extra.el uses SS3 sequences
+F1::SendInput  {ESC}O2P
+F2::SendInput  {ESC}O2Q
+F3::SendInput  {ESC}O2R
+F4::SendInput  {ESC}O2S
+F5::SendInput  {Esc}[15;2~

+F6::SendInput  {Esc}[17;2~
+F7::SendInput  {Esc}[18;2~
+F8::SendInput  {Esc}[19;2~
+F9::SendInput  {Esc}[20;2~
+F10::SendInput {Esc}[21;2~
+F11::SendInput {Esc}[23;2~
+F12::SendInput {Esc}[24;2~

;;** Alt+Fx
;;xterm-extra.el uses SS3 sequences
!F1::SendInput  {ESC}O3P
!F2::SendInput  {ESC}O3Q
!F3::SendInput  {ESC}O3R
!F4::SendInput  {ESC}O3S
!F5::SendInput  {Esc}[15;3~

!F6::SendInput  {Esc}[17;3~
!F7::SendInput  {Esc}[18;3~
!F8::SendInput  {Esc}[19;3~
!F9::SendInput  {Esc}[20;3~
!F10::SendInput {Esc}[21;3~
!F11::SendInput {Esc}[23;3~
!F12::SendInput {Esc}[24;3~

;;** Alt+Shift+F1
;;xterm-extra.el uses SS3 sequences
+!F1::SendInput  {ESC}O4P
+!F2::SendInput  {ESC}O4Q
+!F3::SendInput  {ESC}O4R
+!F4::SendInput  {ESC}O4S
;;** Ctrl+Fx
;;xterm-extra.el uses SS3 sequences
^F1::SendInput  {ESC}O5P
^F2::SendInput  {ESC}O5Q
^F3::SendInput  {ESC}O5R
^F4::SendInput  {ESC}O5S
^F5::SendInput  {Esc}[15;5~

^F6::SendInput  {Esc}[17;5~
^F7::SendInput  {Esc}[18;5~
^F8::SendInput  {Esc}[19;5~
^F9::SendInput  {Esc}[20;5~
^F10::SendInput {Esc}[21;5~
^F11::SendInput {Esc}[23;5~
^F12::SendInput {Esc}[24;5~

;;** Ctrl+Shift+Fx
^+F1::SendInput  {ESC}[1;6P
^+F2::SendInput  {ESC}[1;6Q
^+F3::SendInput  {ESC}[1;6R
^+F4::SendInput  {ESC}[1;6S
^+F5::SendInput  {Esc}[15;6~

^+F6::SendInput  {Esc}[17;6~
^+F7::SendInput  {Esc}[18;6~
^+F8::SendInput  {Esc}[19;6~
^+F9::SendInput  {Esc}[20;6~
^+F10::SendInput {Esc}[21;6~
^+F11::SendInput {Esc}[23;6~
^+F12::SendInput {Esc}[24;6~

;;** Ctrl+Alt+Fx
;;xterm-extra.el uses SS3 sequences
^!F1::SendInput  {ESC}O7P
^!F2::SendInput  {ESC}O7Q
^!F3::SendInput  {ESC}O7R
^!F4::SendInput  {ESC}O7S
^!F5::SendInput  {Esc}[15;7~
^!F6::SendInput  {Esc}[17;7~
^!F7::SendInput  {Esc}[18;7~
^!F8::SendInput  {Esc}[19;7~
^!F9::SendInput  {Esc}[20;7~
^!F10::SendInput {Esc}[21;7~
^!F11::SendInput {Esc}[23;7~
^!F12::SendInput {Esc}[24;7~

;;* ===== Cursor keys ==========
;;** Shift+...
;;xterm-extra.el uses SS3 sequences
+Up::SendInput    {Esc}O2A
+Down::SendInput  {Esc}O2B
+Left::SendInput  {Esc}O2D
+Right::SendInput {Esc}O2C
+Home::SendInput  {ESC}O2H
+End::SendInput   {ESC}O2F

;;** Alt+...
;;Alt+Up/Down/Left/Right/ work fine
;;!Up::SendInput {ESC}[1;3A

;;** Alt+Shift...
;;xterm-extra.el uses SS3 sequences
+!Up::SendInput {ESC}O4A
+!Down::SendInput {ESC}O4B
;;** Ctrl+..
;;xterm-extra.el uses SS3 sequences
^Up::SendInput    {Esc}O5A
^Down::SendInput  {Esc}O5B
^Left::SendInput  {Esc}O5D
^Right::SendInput {Esc}O5C
^Home::SendInput  {ESC}O5H
^End::SendInput   {ESC}O5F



;;** Ctrl+Shif+
;;xterm-extra.el uses SS3 sequences
^+Up::SendInput    {Esc}O6A
^+Down::SendInput  {Esc}O6B
^+Left::SendInput  {Esc}O6D
^+Right::SendInput {Esc}O6C
^+Home::SendInput  {ESC}O6H
^+End::SendInput   {ESC}O6F


;;* ====== Ins/Del ==========
;;* Shift+...
;;^+nsert::SendInput {Esc}[2;5~    (Windows: paste from clipboard)
+Delete::SendInput {Esc}[3;2~
+PGUP::SendInput {Esc}[5;2~
+PGDN::SendInput {Esc}[6;2~

;;** Ctrl+...
;;^Insert::SendInput {Esc}[2;5~    (Windows: copy selection)
^Delete::SendInput {Esc}[3;5~
^PGUP::SendInput {Esc}[5;5~
^PGDN::SendInput {Esc}[6;5~

;;** Alt+
;;Alft+Ins/Del/Home/End/PgUp/PgDwn work fine



;;* ====== some customized keys introducted by xterm-extra.el ======
+Tab::SendInput {Esc}[z2a
;!Tab::SendInput {Esc}[z3a
^Tab::SendInput {Esc}[z5a

+Enter::SendInput {Esc}[z2b
!Enter::SendInput {Esc}[z3b
^Enter::SendInput {Esc}[z5b

+BackSpace::SendInput {Esc}[z2c
!BackSpace::SendInput {Esc}[z3c
^BackSpace::SendInput {Esc}[z5c

;;* ==== for keypad ====
;;http://vim.wikia.com/wiki/PuTTY_numeric_keypad_mappings

;;for the top row, putty sends \eOP, \eOQ,\eOR \eOS,
;;  but they conflicts with xterm's F1..F4  (also used by mintty/gnome-terminal/xfce-terminal)
;;  (putty itself uses \e[11~ ..\e[14~ for F1..F4)
;;for other rows, putty sends \eOl ..\eOp

;;NOTE: Application keypad mode can be turned on and off by the server, depending on the application.
;;Emacs would turn on application keypad mode

;;With the following scripts, keypad acts like normal PC keyboard:
;;when NumLock on, keypad sends 0-9 and +-*/
;;when NumLock off, keypad send sequences \eOn .. \eOy
~NumLock::return
NumpadDiv:: SendInput /
NumpadMult::SendInput *
NumpadSub:: SendInput -
NumpadAdd:: SendInput +
;NumpadAdd:: SendInput {Esc}Ol
NumpadEnter::SendInput {Enter}
;NumpadEnter::SendInput {Esc}Om

NumpadDel::SendInput {Esc}On
;;to kp-0 .. kp-9                           
NumpadIns::SendInput {Esc}Op
NumpadEnd::SendInput {Esc}Oq
NumpadDown::SendInput {Esc}Or
NumpadPgdn::SendInput {Esc}Os
NumpadLeft::SendInput {Esc}Ot
NumpadClear::SendInput {Esc}Ou
NumpadRight::SendInput {Esc}Ov
NumpadHome::SendInput {Esc}Ow
NumpadUp::SendInput {Esc}Ox
NumpadPgup::SendInput {Esc}Oy

NumpadDot::SendInput .
Numpad0::SendInput 0
Numpad1::SendInput 1
Numpad2::SendInput 2
Numpad3::SendInput 3
Numpad4::SendInput 4
Numpad5::SendInput 5
Numpad6::SendInput 6
Numpad7::SendInput 7
Numpad8::SendInput 8
Numpad9::SendInput 9

;;* =========== Misc ======================

;;super/hyper modifiers (only for Emacs)
*RWin::SendInput ^x@s
*AppsKey::SendInput ^x@h


;;on some system, <end> would be recognized as <select>
;End::SendInput {Esc}OF
