;; * Let putty support more key-chords (such as Shift+F1, C-home)
;;by sending corresponding XTerm control sequences

;;all of these sequences are translated from term/xterm.el .
;;http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/term/xterm.el?h=emacs-23
;;if emacs can't create keymap correctly from you TERM
;;you can force to load it
;; (if (and (not (display-graphics-p))
;;          (load-library "term/xterm"))
;;   (terminal-init-xterm))

;;   (note: PuTTY's function key & keypad mode settings is not respected)


;; ** References:
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

;; *

putty_emulate_mode=xterm

putty_do_send_seq(key_name, term_seq, default_seq)
{
    if (term_seq = "FIXME")
    {
        OutputDebug,%key_name% => %default_seq%  ('%putty_emulate_mode%' doesn't support this key. default sequence used.)
        SendInput,%default_seq%
    } else if (term_seq = "")
        SendInput,%default_seq%
    else
        SendInput,%term_seq%
}

putty_send_key(key_name, default_seq, xterm_seq:="", putty_seq:="", gnome_seq:="", emacs_seq:="")
{
    global putty_emulate_mode

    if (putty_emulate_mode = "putty")
        putty_do_send_seq(key_name, putty_seq, default_seq)
    else if (putty_emulate_mode = "gnome")
        putty_do_send_seq(key_name, gnome_seq, default_seq)
    else if (putty_emulate_mode = "emacs")
        putty_do_send_seq(key_name, emacs_seq, default_seq)
    else
        putty_do_send_seq(key_name, xterm_seq, default_seq)
}

;;putty/mintty/putty-nd/mobaxterm
#if WinActive("ahk_class PuTTY") or WinActive("ahk_class mintty") or WinActive("ahk_class SysTabControl32") or WinActive("ahk_class TMobaXtermForm")

;; * F1 .. F12
;;PuTTY:
;;  - in default/VT400/VT100+ mode, putty sends ESC [11~ .. ESC [14~  (CSI sequences) for F1..F4
;;  - in Xterm R6 mode, putty sends ESC OP .. ESC OS (SS3 seqences) for F1..F4
;;  - in Linux or SCO mode... who cares?
;;http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-funkeys
;;
;;but xterm R6 & mintty/gnome-terminal/xfce-terminal send SS3 sequences

;; |          | putty(win) | putty(linux) | mintty      | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |          | 0.62       |              | 1.1         | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |----------+------------+--------------+-------------+---------------+----------------+---------------+----------+----------------|
;; | f1       | ^[[11~     | ^[[11~       | ^[OP        | ^[OP  (help)  | ^[OP (help)    | ^[OP  (help)  | ^[OP     | ^[OP  ^[[11~   |
;; | f2       | ^[[12~     | ^[[12~       | ^[OQ        | ^[OQ          | ^[OQ           | ^[OQ          | ^[OQ     | ^[OQ  ^[[12~   |
;; | f3       | ^[[13~     | ^[[13~       | ^[OR        | ^[OR          | ^[OR           | ^[OR          | ^[OR     | ^[OR  ^[[13~   |
;; | f4       | ^[[14~     | ^[[14~       | ^[OS        | ^[OS          | ^[OS           | ^[OS          | ^[OS     | ^[OS  ^[[14~   |
;; | f5       | ^[[15~     | ^[[15~       | ^[[15~      | ^[[15~        | ^[[15~         | ^[[15~        | ^[[15~   | ____  ^[[15~   |
;; | f6       | ^[[17~     | ^[[17~       | ^[[17~      | ^[[17~        | ^[[16~         | ^[[16~        | ^[[16~   | ____  ^[[16~   |

;;                      key         default             xterm   putty           gnome   emacs
F1::    putty_send_key("F1",    "{ESC}OP",      "",         "{ESC}[11~",    "",         "")
F2::    putty_send_key("F2",    "{ESC}OQ",      "",         "{ESC}[12~",    "",         "")
F3::    putty_send_key("F3",    "{ESC}OR",      "",         "{ESC}[13~",    "",         "")
F4::    putty_send_key("F4",    "{ESC}OS",      "",         "{ESC}[14~",    "",         "")
F5::    putty_send_key("F5",    "{ESC}[15~",    "",         "",                 "",         "")
F6::    putty_send_key("F6",    "{ESC}[17~",    "",         "",                 "",         "")
F7::    putty_send_key("F7",    "{ESC}[18~",    "",         "",                 "",         "")
F8::    putty_send_key("F8",    "{ESC}[19~",    "",         "",                 "",         "")
F9::    putty_send_key("F9",    "{ESC}[20~",    "",         "",                 "",         "")
F10::   putty_send_key("F10",   "{ESC}[21~",    "",         "",                 "",         "")
F11::   putty_send_key("F11",   "{ESC}[23~",    "",         "",                 "",         "")
F12::   putty_send_key("F12",   "{ESC}[24~",    "",         "",                 "",         "")
;;For F5..F12, most terminals sends ESC [15~ .. ESC[24~ (CSI sequences)

;; ** Shift+Fx

;;for ctrl/shift/alt+f1/f2/f3/f4, xterm-r6, mintty emit CSI sequences

;;NOTE: term/xterm.el maps both CSI and SS3 sequences for S-f1..S-f4
;;(but only SS3 for C-f1..C-f4, S-f1..S-f4)

;;NOTE: xfce4-terminal & newer gnome-terminal emit wrong sequences for C/S/M-f1..f4
;;       https://bugs.launchpad.net/ubuntu/+source/gnome-terminal/+bug/96676
;;      (gnome-terminal 2.16 proves to be correct (emiting SS3 sequences)

;; |       | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |       | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |-------+------------+--------------+----------+---------------+----------------+---------------+----------+----------------|
;; | S-f1  | ^[[23~     | ^[[23~       | ^[[1;2P  | ^[O1;2P  x    | ^[O2P          | ^[O1;2P x     | ^[[1;2P  | ^[O2P ^[[1;2P  |
;; | S-f2  | ^[[24~     | ^[[24~       | ^[[1;2Q  | ^[O1;2Q  x    | ^[O2Q          | ^[O1;2Q x     | ^[[1;2Q  | ^[O2Q ^[[1;2Q  |
;; | S-f5  | ^[[28~     | ^[[28~       | ^[[15;2~ | ^[[15;2~      | ^[[15;2~       | ^[[15;2~      | ^[[15;2~ | ____  ^[[15;2~ |
;; | S-f6  | ^[[29~     | ^[[29~       | ^[[17;2~ | ^[[17;2~      | ^[[17;2~       | ^[[17;2~      | ^[[17;2~ | ____  ^[[17;2~ |
;; | S-f11 | ^[[23~ ?   |              | ^[[23;2~ | ^[[23;2~      | ^[[23;2~       | ^[[23;2~      | ^[[23;2~ | ____  ^[[23;2~ |
;; | S-f12 | ^[[24~ ?   |              | ^[[24;2~ | ^[[24;2~      | ^[[24;2~       | ^[[24;2~      | ^[[24;2~ | ____  ^[[24;2~ |
;; |       |            |              |          |               |                |               |          |                |

;;                     key              default         xterm   putty           gnome           emacs
+F1::   putty_send_key("Shift+F1",      "{ESC}[1;2P",   "",     "{ESC}[23~",    "{ESC}O2P",     "")
+F2::   putty_send_key("Shift+F2",      "{ESC}[1;2Q",   "",     "{ESC}[24~",    "{ESC}O2Q",     "")
+F3::   putty_send_key("Shift+F3",      "{ESC}[1;2R",   "",     "{ESC}[13~",    "{ESC}O2R",     "")
+F4::   putty_send_key("Shift+F4",      "{ESC}[1;2S",   "",     "{ESC}[14~",    "{ESC}O2S",     "")
+F5::   putty_send_key("Shift+F5",      "{ESC}[15;2~",  "",     "{ESC}[23~",    "",             "")
+F6::   putty_send_key("Shift+F6",      "{ESC}[17;2~",  "",     "{ESC}[24~",    "",             "")
+F7::   putty_send_key("Shift+F7",      "{ESC}[18;2~",  "",     "{ESC}[27~",    "",             "")
+F8::   putty_send_key("Shift+F8",      "{ESC}[19;2~",  "",     "{ESC}[28~",    "",             "")
+F9::   putty_send_key("Shift+F9",      "{ESC}[20;2~",  "",     "{ESC}[29~",    "",             "")
+F10::  putty_send_key("Shift+F10",     "{ESC}[21;2~",  "",     "{ESC}[30~",    "",             "")
+F11::  putty_send_key("Shift+F11",     "{ESC}[23;2~",  "",     "FIXME",        "",             "")
+F12::  putty_send_key("Shift+F12",     "{ESC}[24;2~",  "",     "FIXME",        "",             "")

;; ** Alt+Fx

;;NOTE: When numlock off, M-f1..f4 not compatible with xterm when numlock off,
;;because xterm emits CSI sequences for M-f1..M-f12
;;but term/xterm.el has only SS3 sequences for M-f1..M-f4, only CSI sequences for M-f5..M-f12

;; |      | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |      | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |------+------------+--------------+----------+---------------+----------------+---------------+----------+----------------|
;; | M-f1 | ^[^[[11~   | ^[^[[11~     | ^[[1;3P  | ^[O1;3P  x    | ^[O3P          | ^[O1;3P x     | ^[[1;3P  | ^[O3P ____     |
;; | M-f2 | ^[^[[12~   | ^[^[[12~     | ^[[1;2Q  | ^[O1;3Q  x    | ^[O2Q          | ^[O1;3Q x     | ^[[1;3Q  | ^[O3Q ____     |
;; | M-f5 | ^[^[[15~   | ^[^[[15~     | ^[[15;3~ | ^[[15;3~      | ^[[15;3~       | ^[[15;3~      | ^[[15;3~ | ____  ^[[15;3~ |

;;                     key              default         xterm   putty           gnome           emacs
!F1::   putty_send_key("Alt+F1",        "{ESC}[1;3P",   "",     "{ESC}{ESC}[11~",    "{ESC}O3P",     "")
!F2::   putty_send_key("Alt+F2",        "{ESC}[1;3Q",   "",     "{ESC}{ESC}[12~",    "{ESC}O3Q",     "")
!F3::   putty_send_key("Alt+F3",        "{ESC}[1;3R",   "",     "{ESC}{ESC}[13~",    "{ESC}O3R",     "")
!F4::   putty_send_key("Alt+F4",        "{ESC}[1;3S",   "",     "{ESC}{ESC}[14~",    "{ESC}O3S",     "")
!F5::   putty_send_key("Alt+F5",        "{ESC}[15;3~",  "",     "{ESC}{ESC}[15~",    "",             "")
!F6::   putty_send_key("Alt+F6",        "{ESC}[17;3~",  "",     "{ESC}{ESC}[17~",    "",             "")
!F7::   putty_send_key("Alt+F7",        "{ESC}[18;3~",  "",     "{ESC}{ESC}[18~",    "",             "")
!F8::   putty_send_key("Alt+F8",        "{ESC}[19;3~",  "",     "{ESC}{ESC}[19~",    "",             "")
!F9::   putty_send_key("Alt+F9",        "{ESC}[20;3~",  "",     "{ESC}{ESC}[20~",    "",             "")
!F10::  putty_send_key("Alt+F10",       "{ESC}[21;3~",  "",     "{ESC}{ESC}[21~",    "",             "")
!F11::  putty_send_key("Alt+F11",       "{ESC}[23;3~",  "",     "{ESC}{ESC}[23~",    "",             "")
!F12::  putty_send_key("Alt+F12",       "{ESC}[24;3~",  "",     "{ESC}{ESC}[24~",    "",             "")


;; ** Alt+Shift+F1
;; |        | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |        | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |--------+------------+--------------+----------+---------------+----------------+---------------+----------+----------------|
;; | M-S-f1 | ^[^[23~    | ^[^[23~      | ^[[1;4P  | ^[O1;4P  x    | ^[O4P          | ^[O1;4P x     | ^[[1;4P  | ^[O4P ____     |
;; | M-S-f1 | ^[^[24~    | ^[^[24~      | ^[[1;4Q  | ^[O1;4Q  x    | ^[O4Q          | ^[O1;4Q x     | ^[[1;4Q  | ^[O4Q ____     |
;; | M-S-f5 | ^[^[[28~   | ^[^[[28~     | ^[[15;4~ | ^[[15;4~      | ^[[15;4~       | ^[[15;4~      | ^[[15;4~ | ____  ^[[15;4~ |
;; |        |            |              |          |               |                |               |          |                |

;;                     key                  default         xterm   putty               gnome           emacs
+!F1::  putty_send_key("Alt+Shift+F1",      "{ESC}[1;4P",   "",     "{ESC}{ESC}[23~",   "{ESC}O4P",     "")
+!F2::  putty_send_key("Alt+Shift+F2",      "{ESC}[1;4Q",   "",     "{ESC}{ESC}[24~",   "{ESC}O4Q",     "")
+!F3::  putty_send_key("Alt+Shift+F3",      "{ESC}[1;4R",   "",     "{ESC}{ESC}[13~",   "{ESC}O4R",     "")
+!F4::  putty_send_key("Alt+Shift+F4",      "{ESC}[1;4S",   "",     "{ESC}{ESC}[14~",   "{ESC}O4S",     "")
+!F5::  putty_send_key("Alt+Shift+F5",      "{ESC}[15;4~",  "",     "{ESC}{ESC}[23~",   "",             "")
+!F6::  putty_send_key("Alt+Shift+F6",      "{ESC}[17;4~",  "",     "{ESC}{ESC}[24~",   "",             "")
+!F7::  putty_send_key("Alt+Shift+F7",      "{ESC}[18;4~",  "",     "{ESC}{ESC}[27~",   "",             "")
+!F8::  putty_send_key("Alt+Shift+F8",      "{ESC}[19;4~",  "",     "{ESC}{ESC}[28~",   "",             "")
+!F9::  putty_send_key("Alt+Shift+F9",      "{ESC}[20;4~",  "",     "{ESC}{ESC}[29~",   "",             "")
+!F10:: putty_send_key("Alt+Shift+F10",     "{ESC}[21;4~",  "",     "{ESC}{ESC}[30~",   "",             "")
+!F11:: putty_send_key("Alt+Shift+F11",     "{ESC}[23;4~",  "",     "FIXME",            "",             "")
+!F12:: putty_send_key("Alt+Shift+F12",     "{ESC}[24;4~",  "",     "FIXME",            "",             "")

;; ** Ctrl+Fx
;;Xterm: emits CSI sequences for C-f1..C-f12
;;Emacs: term/xterm.el has SS3 sequences only for C-f1..C-f4, 
;;       and CSI sequences for only C-f5..C-f12

;; |      | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |      | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |------+------------+--------------+----------+---------------+----------------+---------------+----------+----------------|
;; | C-f1 | -          | -            | ^[[1;5P  | /             | /              | /             | ^[[1;5P  | ^[O5P ____     |
;; | C-f2 | -          | -            | ^[[1;5Q  | ^[O1;5Q  x    | ^[O5Q          | ^[O1;5Q x     | ^[[1;5Q  | ^[O5Q ____     |
;; | C-f5 | -          | -            | ^[[15;5~ | ^[[15;5~      | ^[[15;5~       | ^[[15;5~      | ^[[15;5~ | ____  ^[[15;5~ |

;;                     key              default         xterm   putty       gnome           emacs
^F1::   putty_send_key("Ctrl+F1",      "{ESC}[1;5P",     "",     "FIXME",    "{ESC}O5P",     "")
^F2::   putty_send_key("Ctrl+F2",      "{ESC}[1;5Q",     "",     "FIXME",    "{ESC}O5Q",     "")
^F3::   putty_send_key("Ctrl+F3",      "{ESC}[1;5R",     "",     "FIXME",    "{ESC}O5R",     "")
^F4::   putty_send_key("Ctrl+F4",      "{ESC}[1;5S",     "",     "FIXME",    "{ESC}O5S",     "")
^F5::   putty_send_key("Ctrl+F5",      "{ESC}[15;5~",    "",     "FIXME",    "",             "")
^F6::   putty_send_key("Ctrl+F6",      "{ESC}[17;5~",    "",     "FIXME",    "",             "")
^F7::   putty_send_key("Ctrl+F7",      "{ESC}[18;5~",    "",     "FIXME",    "",             "")
^F8::   putty_send_key("Ctrl+F8",      "{ESC}[19;5~",    "",     "FIXME",    "",             "")
^F9::   putty_send_key("Ctrl+F9",      "{ESC}[20;5~",    "",     "FIXME",    "",             "")
^F10::  putty_send_key("Ctrl+F10",     "{ESC}[21;5~",    "",     "FIXME",    "",             "")
^F11::  putty_send_key("Ctrl+F11",     "{ESC}[23;5~",    "",     "FIXME",    "",             "")
^F12::  putty_send_key("Ctrl+F12",     "{ESC}[24;5~",    "",     "FIXME",    "",             "")



;; ** Ctrl+Shift+Fx

;; |        | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm    | term/xterm.el  |
;; |        | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |          | SS3   CSI      |
;; |--------+------------+--------------+----------+---------------+----------------+---------------+----------+----------------|
;; | C-S-f1 | -          | -            | ^[[1;6P  | ^[O1;6P  x    | ^[O6P          | ^[O1;6P x     | ^[[1;6P  | ^[O6P ____     |
;; | C-S-f5 | -          | -            | ^[[15;6~ | ^[[15;6~      | ^[[15;6~       | ^[[15;6~      | ^[[15;6~ | ____  ^[[15;6~ |

;;                     key              default         xterm   putty       gnome           emacs
^+F1::   putty_send_key("Ctrl+Shift+F1",      "{ESC}[1;6P",     "",     "FIXME",    "{ESC}O6P",     "")
^+F2::   putty_send_key("Ctrl+Shift+F2",      "{ESC}[1;6Q",     "",     "FIXME",    "{ESC}O6Q",     "")
^+F3::   putty_send_key("Ctrl+Shift+F3",      "{ESC}[1;6R",     "",     "FIXME",    "{ESC}O6R",     "")
^+F4::   putty_send_key("Ctrl+Shift+F4",      "{ESC}[1;6S",     "",     "FIXME",    "{ESC}O6S",     "")
^+F5::   putty_send_key("Ctrl+Shift+F5",      "{ESC}[15;6~",    "",     "FIXME",    "",             "")
^+F6::   putty_send_key("Ctrl+Shift+F6",      "{ESC}[17;6~",    "",     "FIXME",    "",             "")
^+F7::   putty_send_key("Ctrl+Shift+F7",      "{ESC}[18;6~",    "",     "FIXME",    "",             "")
^+F8::   putty_send_key("Ctrl+Shift+F8",      "{ESC}[19;6~",    "",     "FIXME",    "",             "")
^+F9::   putty_send_key("Ctrl+Shift+F9",      "{ESC}[20;6~",    "",     "FIXME",    "",             "")
^+F10::  putty_send_key("Ctrl+Shift+F10",     "{ESC}[21;6~",    "",     "FIXME",    "",             "")
^+F11::  putty_send_key("Ctrl+Shift+F11",     "{ESC}[23;6~",    "",     "FIXME",    "",             "")
^+F12::  putty_send_key("Ctrl+Shift+F12",     "{ESC}[24;6~",    "",     "FIXME",    "",             "")

;;
** Ctrl+Alt+Fx

;;|        | putty(win) | putty(linux) | mintty   | xfce-terminal | gnome-terminal | mate-terminal | xterm | term/xterm.el |
;;|        | 0.62       |              | 1.1      | 0.48          | 2.16           | 1.4           |       | SS3   CSI     |
;;|--------+------------+--------------+----------+---------------+----------------+---------------+-------+---------------|
;;| C-M-f1 | -          | -            | ^[[1;7P  | -             | -              | -             |       | ____  ____    |
;;| C-M-f2 | -          | -            | ^[[1;7Q  | -             | -              | -             | -     | ____  ____    |
;;| C-M-f5 | -          | -            | ^[[15;7~ | -             | -              | -             |       | ____  _____   |


;; * Cursor keys
;;Cursor keycodes without modifier keys depend on whether "application cursor key mode"
;;http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-appcursor
;;http://code.google.com/p/mintty/wiki/Keycodes#Cursor_keys


;;NOTE: when numlock off, not xterm compatible, most terminals emit ^[OH or ^[[1~
;;      (xterm/mintty emit ^[[H, but term/xterm.el has no mapping for ^[[H)

Home::
  if GetKeyState("Numlock", "T")
    ;; putty way
    SendInput {Esc}[1~
  else
    ;; xterm 102/220 / gnome-terminal way
    SendInput {Esc}OH
  return
End::
  if GetKeyState("Numlock", "T")
    SendInput {Esc}[4~
  else
    SendInput {Esc}OF
  return


;; ** Shift+Cursor

;;NOTE: old gnome-terminal (at least 2.16) emits strange sequences for C/S/M-up/down/left/right/
;;      (e.g. Shift-left emits ^[[2D  but SS3 sequences is ^[O2D, CSI sequences is ^[[1;2D)
+Up::SendInput    {Esc}[1;2A
+Down::SendInput  {Esc}[1;2B
+Left::SendInput  {Esc}[1;2D
+Right::SendInput {Esc}[1;2C

+Home::SendInput  {ESC}[1;2H
+End::SendInput   {ESC}[1;2F
;;Other versions of xterm might emit these.
;;+Up::SendInput    {Esc}O2A
;;+Down::SendInput  {Esc}O2B
;;+Left::SendInput  {Esc}O2D
;;+Right::SendInput {Esc}O2C
;;+Home::SendInput  {ESC}O2H
;;+End::SendInput   {ESC}O2F


;; ** Alt+Cursor
;;Alt+Up/Down/Left/Right/ work fine
;;!Up::SendInput {ESC}[1;3A

;; ** Alt+Shift+Cursor
+!Up::SendInput {ESC}[1;4A
+!Down::SendInput {ESC}[1;4B

;; ** Ctrl+Cursor
^Up::SendInput    {Esc}[1;5A
^Down::SendInput  {Esc}[1;5B
^Left::SendInput  {Esc}[1;5D
^Right::SendInput {Esc}[1;5C
^Home::SendInput  {ESC}[1;5H
^End::SendInput   {ESC}[1;5F


;; ** Ctrl+Shif+Cursor
^+Up::SendInput    {Esc}[1;6A
^+Down::SendInput  {Esc}[1;6B
^+Left::SendInput  {Esc}[1;6D
^+Right::SendInput {Esc}[1;6C
^+Home::SendInput  {ESC}[1;6H
^+End::SendInput   {ESC}[1;6F


;; * Ins/Del
;;* Shift+...
;;^+nsert::SendInput {Esc}[2;5~    (Windows: paste from clipboard)
+Delete::SendInput {Esc}[3;2~
;;+PGUP::SendInput {Esc}[5;2~
;;+PGDN::SendInput {Esc}[6;2~

;;** Ctrl+...
;;^Insert::SendInput {Esc}[2;5~    (Windows: copy selection)
^Delete::SendInput {Esc}[3;5~
^PGUP::SendInput {Esc}[5;5~
^PGDN::SendInput {Esc}[6;5~

;;** Alt+
;;Alft+Ins/Del/Home/End/PgUp/PgDwn work fine


;; * some punctions
;;Among Linux terminal emulators, only available on xterm>216, and need
;;special sequences to turn on this feature.
;;emacs term/xterm.el would detect xterm version and let it turn on

;;NOTE: by default, Ctrl+Shift is used for switching betweenn  different input methods
;; to press C-!, C-# etc, maybe you need to disable this (or use other key combos)

;;                     key              default         xterm           putty           gnome   emacs
^'::    putty_send_key("Ctrl+'",        "{Esc}[27;5;39~",       "", "FIXME", "FIXME", "")
^,::    putty_send_key("Ctrl+,",        "{Esc}[27;5;44~",       "", "FIXME", "FIXME", "")
^-::    putty_send_key("Ctrl+-",        "{Esc}[27;5;45~",       "", "FIXME", "FIXME", "")
^.::    putty_send_key("Ctrl+.",        "{Esc}[27;5;46~",       "", "FIXME", "FIXME", "")
^/::    putty_send_key("Ctrl+/",        "{Esc}[27;5;47~",       "", "FIXME", "FIXME", "")
^`;::   putty_send_key("Ctrl+;",        "{Esc}[27;5;59~",       "", "FIXME", "FIXME", "")
^=::    putty_send_key("Ctrl+=",        "{Esc}[27;5;61~",       "", "FIXME", "FIXME", "")
;;C-\:: putty_send_key("Ctrl+\",        "{Esc}[27;5;92~",       "", "FIXME", "FIXME", "")
^!::    putty_send_key("Ctrl+!",        "{Esc}[27;6;33~",       "", "FIXME", "FIXME", "")
;;^"::   putty_send_key("Ctrl+"",   "{Esc}[27;6;34~",       "", "FIXME", "FIXME", "") ;;FIXME: this would affect ^ key
^#::    putty_send_key("Ctrl+#",        "{Esc}[27;6;35~",       "", "FIXME", "FIXME", "")
^$::    putty_send_key("Ctrl+$",        "{Esc}[27;6;36~",       "", "FIXME", "FIXME", "")
^%::    putty_send_key("Ctrl+%",        "{Esc}[27;6;37~",       "", "FIXME", "FIXME", "")
^&::    putty_send_key("Ctrl+&",        "{Esc}[27;6;38~",       "", "FIXME", "FIXME", "")
;;^(::  putty_send_key("Ctrl+(",        "{Esc}[27;6;40~",       "", "FIXME", "FIXME", "") ;;FIXME: Invalid hotkey
;;^)::  putty_send_key("Ctrl+)",        "{Esc}[27;6;41~",       "", "FIXME", "FIXME", "") ;;FIXME: Invalid hotkey
^*::    putty_send_key("Ctrl+*",        "{Esc}[27;6;42~",       "", "FIXME", "FIXME", "")
^+::    putty_send_key("Ctrl++",        "{Esc}[27;6;43~",       "", "FIXME", "FIXME", "")
;;^:::  putty_send_key("Ctrl+:",        "{Esc}[27;6;58~",       "", "FIXME", "FIXME", "") ;;FIXME: AHK bug here: how to set C-: as hotkey?
^<::    putty_send_key("Ctrl+<",        "{Esc}[27;6;60~",       "", "FIXME", "FIXME", "")
^>::    putty_send_key("Ctrl+>",        "{Esc}[27;6;62~",       "", "FIXME", "FIXME", "")
^?::    putty_send_key("Ctrl+?",        "{Esc}[27;6;63~",       "", "FIXME", "FIXME", "")


;;mintty
;; | Key   | plain | Shift | Ctrl    | Ctrl+Shift | 
;; |-------+-------+-------+---------+------------| 
;; | Tab   | ^I    | ^[[Z  | ^[[1;5I | ^[[1;6I    | 
;; | Enter | ^M    | ^J    | ^^      | U+009E     | 
;; | Bksp  | ^?    | ^?    | ^_      | U+009F     | 

;;most terminals emit ^[[Z for Shift-Tab
+Tab::
  if not GetKeyState("Numlock", "T")
     SendInput {Esc}[Z
  else
    SendInput {Esc}[27;2;9~
  return
;;!Tab::SendInput {Esc}{Tab}
^Tab::SendInput {Esc}[27;5;9~

+Enter::SendInput {Esc}[27;2;13~
^Enter::SendInput {Esc}[27;5;13~
;;!Enter::SendInput {Esc}{Enter}

;;NOTE:  Backspace => DEL  != Delete (=> <delete>)
;; Alt+Bksp  = M-DEL
;; map C-backspace to M-backspace
^BackSpace::SendInput {Esc}{BackSpace}


;; * keypad
;;PuTTY original behavior:
;;  - for the top row, putty sends \eOP, \eOQ,\eOR \eOS,
;;    but they conflicts with xterm's F1..F4  (also used by mintty/gnome-terminal/xfce-terminal)
;;    (putty itself uses \e[11~ ..\e[14~ for F1..F4)
;;  - for other rows, putty sends \eOl ..\eOp
;;NOTE: Application keypad mode can be turned on and off by the server, depending on the application.
;;Emacs would turn on application keypad mode

;;With the following scripts, keypad acts similar to normal PC keyboard:
;;when NumLock on, keypad sends 0-9 and +-*/
;;when NumLock off, keypad send sequences \eOj .. \eOy (refer term/xterm.el)

;; see also
;; http://vim.wikia.com/wiki/PuTTY_numeric_keypad_mappings (wrong sequences for keypad +-*/ ?

~NumLock::return
NumpadMult::
  if GetKeyState("Numlock", "T")
    SendInput *
  else
    SendInput {Esc}Oj
  return
NumpadAdd::
  if GetKeyState("Numlock", "T")
    SendInput +
  else
    SendInput {Esc}Ok
  return
;;\e[0l         kp-separator ?
NumpadSub::
  if GetKeyState("Numlock", "T")
    SendInput -
  else
    SendInput {Esc}Om
  return
NumpadDiv::
  if GetKeyState("Numlock", "T")
    SendInput /
  else
    SendInput {Esc}Oo
  return


NumpadEnter::
  if GetKeyState("Numlock", "T")
    SendInput -
  else
    SendInput {Esc}OM
  return


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

;;\e[OI         kp-tab



;;* ====== misc ==========
;;; http://code.google.com/p/mintty/wiki/Keycodes#Special_keys

;;\e[1~         find
;;\e[28~        help
;;\e[29~        print (menu?)


;;* =========== Misc ======================

;;super/hyper modifiers (only for Emacs)
*RWin::SendInput ^x@s
*AppsKey::SendInput ^x@h

;;tmux/gnu-screen
<#Tab::SendInput ^bn
<#1::SendInput ^b1
<#2::SendInput ^b2
<#3::SendInput ^b3
<#4::SendInput ^b4

;;on some system, <end> would be recognized as <select>
;End::SendInput {Esc}OF


;;* =========== MobaXterm only ======================
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

^h::SendInput {f1}


;; -*- indent-tabs: nil; comment-start: ";;"; comment-end: ""; orgtbl-comment: t -*-
