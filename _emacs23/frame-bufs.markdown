Frame-bufs is intended as a convenience for those who like to organize their
workflow in Emacs by using specific frames for different projects. It extends
Emacs’s buffer menu so that it understands a distinction between those
buffers that are associated with a given frame and those that are not. The
buffer menu can be toggled between a list of all buffers and a list of only
those buffers associated with the selected frame. The criteria governing
which buffers are associated with a frame can be customized through various
options.  In addition, buffers can be manually added to and removed from the
list of buffers associated with a frame. The package interacts properly with
`other-buffer` and respects changes in buffer ordering made by
`bury-buffer`.

Installation
============

Put the package file in your load path and put:

  `(require 'frame-bufs)`

in your `.emacs`.  To enable frame-bufs, use the command
`frame-bufs-mode`, or put `(frame-bufs-mode t)` in your `.emacs`.

Features Overview
=================

When frame-bufs-mode is enabled, it keeps a record of which buffers are
associated with each frame.  In the typical case, a buffer becomes associated
with a frame if it is selected in one of that frame's windows.  Note, then,
that a buffer can be associated with more than one frame.  We sometimes speak
of a buffer that's associated with a frame as "frame-associated," and refer
to the list of buffers that are associated with a given frame as that frame's
"associated-buffer list."  (Frame-bufs does not alter the `buffer-list` or
`buried-buffer-list` frame parameters of any frame; it keeps its own record
of the buffers associated with each frame.)
  
The buffer menu now has two modes:  it can either list all buffers (we call
this "full-list mode") or only buffers associated with the selected frame
("frame-list mode").  By typing `F` one can toggle between the two modes.  In
full-list mode there is an additional column after the initial `CRM` bit
columns: the `F` column.  Buffers associated with the selected frame are
indicated with an `o` in this column.  In frame-list mode, the fourth column
is suppressed.  Here are screenshots of the buffer menu in the two different
modes:

#### Frame-List Mode

![screenshot](https://github.com/alpaker/Frame-Bufs/raw/master/FrameBufsLocalMode.png)

<i>Buffer menu in frame-list mode, sorted by buffer size.  Only buffers associated with this frame are shown.</i>

#### Full-List Mode

![screenshot](https://github.com/alpaker/Frame-Bufs/raw/master/FrameBufsFullMode.png)

<i>Buffer menu in full-list mode, sorted by buffer size. All buffers are shown; the buffers associated with this frame are
distinguished by an `o` bit in the fourth `F` column.</i>

When first called up, the buffer menu appears in full-list mode.  In
subsequent calls it opens in whatever mode it was last in.

In addition to toggling between the two modes, there are two other new
commands available in the buffer menu:

* A buffer can marked as to be added to the selected frame's
  associated-buffer list by typing `A`.

* A buffer can be marked as to be removed from the associated-buffer list by
  typing `N`.

As with save and delete commands in the buffer menu, these requested changes
are effected by calling `Buffer-menu-execute`.

Further Control of the Local Buffer List
========================================

As mentioned above, a buffer automatically becomes associated with a frame if
it is selected in one of that frame's windows.  Finer-grained control over
which buffers are automatically associated with a frame is provided by the
following variables:

* If `frame-bufs-include-displayed-buffers` is non-nil, then buffers that
  are merely displayed on a frame become associated with it, even if they
  have not been selected.  (The default value is nil.)

* If a buffer's name is a member of `frame-bufs-always-include-names`, then
  that buffer is associated with every frame.  (The default value is
  `("*scratch*")`.)

* If the command that creates a new frame also creates new buffers, then one
  might want those buffers to be automatically associated with the new
  frame.  (For example, when one calls `gnus` and thereby creates a new
  frame, it might be desirable to have the auxiliary buffers created by
  `gnus` associated with the Gnus frame, even if those buffers haven't been
  selected.)  If `frame-bufs-include-new-buffers` is non-nil, then such
  buffers are added to the new frame's associated-buffer list, even if they
  have not been selected or displayed.  (The default value is nil.)

* If `frame-bufs-include-init-buffer` is non-nil, then the buffer that is
  current when the command creating a new frame is called (the "init-buffer")
  will be associated with the new frame.  If nil, it will not.  (The default
  value is nil.)

* If `frame-bufs-new-frames-inherit` is non-nil, then the associated
  buffers of a new frame's "parent"--the frame that was selected when the
  command creating the new frame is called--will be associated with the new
  frame.  (The default value is nil.)

Other Commands and Features
===========================

Frame-bufs also provides several options and commands that apply everywhere,
not just in the buffer menu:

* If the variable `frame-bufs-use-buffer-predicate` is non-nil, each frame's
  buffer predicate is set so that `other-buffer` will prefer frame-associated
  buffers.  Thus, when a buffer is removed from a window and automatically
  replaced with another (as happens, say, when one kills a buffer), the newly
  displayed buffer will, if possible, be one associated with that
  frame.  (The default value is t.)

* The command `frame-bufs-dismiss-buffer` is somewhat analogous to
  `bury-buffer`.  It removes a buffer from a frame's associated-buffer list,
  and if that buffer is displayed in any windows on the frame, it is replaced
  by another buffer.  When called with no arguments, it acts on the current
  buffer and selected frame.

* The command `frame-bufs-reset-frame` resets a frame's associated-buffer
  list.  Specifically, it sets the associated-buffer list to the list of
  buffers that have been selected on the frame.  When called with no
  argument, it acts on the current frame.

* The command `frame-bufs-reset-all-frames` resets the associated-buffer list
  of all frames.

Compatibility and Other Considerations
======================================

Frame-bufs is compatible with `buff-menu+`.  It does not affect the
operation of `electric-buffer-list`, `bs-show`, or other buffer listing
commands.

A Note on v24
-------------

The buffer display routines in v24 are currently being rewritten in
preparation for the release of v24.1. As they've been changing on a
near-daily basis, I'm not going to try to keep up with them until the
relevant code stabilizes (which is supposed to happen by August,
2011).  Until then, users who build v24 from source using a recent rev (where
"recent" means after April, 2011) might see irregular behavior.
