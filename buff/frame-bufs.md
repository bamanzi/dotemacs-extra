`frame-bufs` extends Emacs’s buffer menu so that it understands a distinction between
those buffers that "belong" to a frame and those that do not. The buffer menu
can be toggled between listing all buffers and listing only those buffers
associated with the selected frame. The criteria governing which buffers are
associated with a frame can be customized through various options.  In
addition, buffers can be manually added to and removed from the list of
buffers associated with a frame.  

The package interacts properly with `other-buffer` and respects changes in
buffer ordering made by `bury-buffer`. It does not alter the `buffer-list` or
`buried-buffer-list` frame parameters.  It is not compatible with non-`nil`
values of `pop-up-frames`.

### Installation

Put the package file in your load path and put:

  `(require 'frame-bufs)`

in your `.emacs`.  To enable frame-bufs, use the command
`frame-bufs-mode`, or put `(frame-bufs-mode t)` in your `.emacs`.

### Features Overview

When frame-bufs-mode is enabled, it keeps a record of which buffers are
associated with each frame.  In the typical case, a buffer becomes associated
with a frame when it is selected in one of that frame's windows.  Note, then,
that a buffer can be associated with more than one frame.
  
The buffer menu now has two modes:  It can either list all buffers (we call
this "global mode") or only buffers associated with the selected frame
("local mode").  By typing `F` one can toggle between the two modes.  In
global mode there is an additional column after the initial `CRM` bit
columns: the `F` column.  Buffers associated with the selected frame are
indicated with an `o` in this column.  In local mode, the fourth column
is suppressed.  Here are screenshots of the buffer menu in the two different
modes:

#### Local Mode

![screenshot](https://github.com/alpaker/Frame-Bufs/raw/master/FrameBufsLocalMode.png)

<i>Buffer menu in local mode, sorted by buffer size.  Only buffers associated with this frame are shown.</i>

#### Global Mode

![screenshot](https://github.com/alpaker/Frame-Bufs/raw/master/FrameBufsGlobalMode.png)

<i>Buffer menu in global mode, sorted by buffer size. All buffers are shown; the buffers associated with this frame are
distinguished by an `o` bit in the fourth `F` column.</i>

When first called up, the buffer menu appears in global mode.  In
subsequent calls it opens in whatever mode it was last in.

In addition to toggling between the two modes, there are two other new
commands available in the buffer menu:

* A buffer can marked as to be added to the selected frame's
  associated-buffer list by typing `A`.

* A buffer can be marked as to be removed from the associated-buffer list by
  typing `N`.

As with saving and killing buffers from the buffer menu, these requested
changes are effected by calling `Buffer-menu-execute`.

### Further Control of the Local Buffer List

As mentioned above, a buffer automatically becomes associated with a frame if
it is selected in one of that frame's windows.  Finer-grained control over
which buffers are automatically associated with a frame is provided by the
following variables:

* If `frame-bufs-include-displayed-buffers` is non-`nil`, then buffers that
  are merely displayed on a frame become associated with it, even if they
  have not been selected.  (The default value is `nil`.)

* If a buffer's name is a member of `frame-bufs-always-include-names`, then
  that buffer is associated with every frame.  (The default value is
  `("*scratch*" "*notes*")`.)

* If the command that creates a new frame also creates new buffers, then one
  might want those buffers to be automatically associated with the new
  frame.  If `frame-bufs-include-new-buffers` is non-`nil`, then such
  buffers are added to the new frame's associated-buffer list, even if they
  have not been selected or displayed.  (The default value is `nil`.)

* If `frame-bufs-include-init-buffer` is non-`nil`, then the buffer that is
  current when the command creating a new frame is called will be associated
  with the new frame.  If `nil`, it will not.  (The default value is `nil`.)

* If `frame-bufs-new-frames-inherit` is non-`nil`, then the associated
  buffers of a new frame's "parent"--the frame that was selected when the
  command creating the new frame is called--will be associated with the new
  frame.  (The default value is `nil`.)

### Other Commands and Features

* If the variable `frame-bufs-use-buffer-predicate` is non-`nil`, each frame's
  buffer predicate is set so that `other-buffer` will prefer frame-associated
  buffers.  Thus, when a buffer is removed from a window and automatically
  replaced with another (as happens, say, when one kills a buffer), the newly
  displayed buffer will, if possible, be one associated with that
  frame.  (The default value is t.)  Changes to this varible do not take
  effect until the mode is re-enabled.

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
