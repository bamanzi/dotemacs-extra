;;Let putty/mintty support more key-chords (such as Shift+F1, C-home)
;;by sending corresponding XTerm control sequences on PuTTY

;;References:
;;http://code.google.com/p/mintty/wiki/Keycodes
;;http://www.xfree86.org/current/ctlseqs.html#PC-Style%20Function%20Keys
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

;;* ===== Fn ==========
;;** Shift+Fn
+F1::SendInput  {ESC}[1;2P
+F2::SendInput  {ESC}[1;2Q
+F3::SendInput  {ESC}[1;2R
+F4::SendInput  {ESC}[1;2S
+F5::SendInput  {Esc}[15;2~
+F6::SendInput  {Esc}[17;2~
+F7::SendInput  {Esc}[18;2~
+F8::SendInput  {Esc}[19;2~
+F9::SendInput  {Esc}[20;2~
+F10::SendInput {Esc}[21;2~
+F11::SendInput {Esc}[23;2~
+F12::SendInput {Esc}[24;2~

;;** Alt+Fn
!F1::SendInput  {ESC}[1;3P
!F2::SendInput  {ESC}[1;3Q
!F3::SendInput  {ESC}[1;3R
!F4::SendInput  {ESC}[1;3S
!F5::SendInput  {Esc}[15;3~
!F6::SendInput  {Esc}[17;3~
!F7::SendInput  {Esc}[18;3~
!F8::SendInput  {Esc}[19;3~
!F9::SendInput  {Esc}[20;3~
!F10::SendInput {Esc}[21;3~
!F11::SendInput {Esc}[23;3~
!F12::SendInput {Esc}[24;3~

;;** Ctrl+Fn
^F1::SendInput  {ESC}[1;5P
^F2::SendInput  {ESC}[1;5Q
^F3::SendInput  {ESC}[1;5R
^F4::SendInput  {ESC}[1;5S
^F5::SendInput  {Esc}[15;5~
^F6::SendInput  {Esc}[17;5~
^F7::SendInput  {Esc}[18;5~
^F8::SendInput  {Esc}[19;5~
^F9::SendInput  {Esc}[20;5~
^F10::SendInput {Esc}[21;5~
^F11::SendInput {Esc}[23;5~
^F12::SendInput {Esc}[24;5~

;;** Ctrl+Shift+Fn
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

;;** Ctrl+Alt+Fn					   
;;C-M-PF1..PF4 supported by SS3
;; ^!F1::SendInput  {ESC}O7P
;; ^!F2::SendInput  {ESC}O7Q
;; ^!F3::SendInput  {ESC}O7R
;; ^!F4::SendInput  {ESC}O7S
;;NOTE: might not supported by your your terminfo
^!F1::SendInput  {ESC}[1;7P
^!F2::SendInput  {ESC}[1;7Q
^!F3::SendInput  {ESC}[1;7R
^!F4::SendInput  {ESC}[1;7S					   
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
+Up::SendInput    {Esc}[1;2A
+Down::SendInput  {Esc}[1;2B
+Left::SendInput  {Esc}[1;2D
+Right::SendInput {Esc}[1;2C
+Home::SendInput  {ESC}[1;2H
+End::SendInput   {ESC}[1;2F

;;** Alt+...
;;Alt+Up/Down/Left/Right/ work fine

;;** Alt+Shift...
+!Up::SendInput    {ESC}[1;4A
+!Down::SendInput  {ESC}[1;4B
+!Left::SendInput  {Esc}[1;4D
+!Right::SendInput {Esc}[1;4C
+!Home::SendInput  {ESC}[1;4H
+!End::SendInput   {ESC}[1;4F

;;** Ctrl+..
^Up::SendInput    {Esc}[1;5A
^Down::SendInput  {Esc}[1;5B
^Left::SendInput  {Esc}[1;5D
^Right::SendInput {Esc}[1;5C
^Home::SendInput  {ESC}[1;5H
^End::SendInput   {ESC}[1;5F

						
;;** Ctrl+Shif+
^+Up::SendInput    {Esc}[1;6A
^+Down::SendInput  {Esc}[1;6B
^+Left::SendInput  {Esc}[1;6D
^+Right::SendInput {Esc}[1;6C
^+Home::SendInput  {ESC}[1;6H
^+End::SendInput   {ESC}[1;6F


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


;; ====== Misc ===========
;;on some system, <end> would be recognized as <select>
;;End::SendInput {Esc}OF

;;SS3 sequences
NumLock::SendInput {Esc}OP
NumpadDiv::SendInput {Esc}OQ
NumpadMult::SendInput {Esc}OR
NumpadSub::SendInput {Esc}OS
Numpad0::SendInput {Esc}Op
Numpad1::SendInput {Esc}Oq
Numpad2::SendInput {Esc}Or
Numpad3::SendInput {Esc}Os
Numpad4::SendInput {Esc}Ot
Numpad5::SendInput {Esc}Ou
Numpad6::SendInput {Esc}Ov
Numpad7::SendInput {Esc}Ow
Numpad8::SendInput {Esc}Ox
Numpad9::SendInput {Esc}Oy


;; ===== MobaXterm doesn't map Alt to Meta
#if WinActive("ahk_class TMobaXtermForm")
;;AutoHotkey:  Ctrl ^ Alt ! Shift +
!x::SendInput {Esc}x
!s::SendInput {Esc}s
!g::SendInput {Esc}g
!f::SendInput {Esc}f
!b::SendInput {Esc}b
!p::SendInput {Esc}p
!n::SendInput {Esc}n
!q::SendInput {Esc}q
!v::SendInput {Esc}v
!y::SendInput {Esc}y
!/::SendInput {Esc}/




