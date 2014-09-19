## 23.1

| ext  | dlls                                                     | location                                              |
|------|----------------------------------------------------------|-------------------------------------------------------|
| xpm  | libxpm.dll, xpm4.dll or libxpm-nox4.dll                  | (libxpm.dll included in emacs zip)                    |
| png  | libpng12d.dll, libpng12.dll, libpng.dll or libpng13d.dll | http://gnuwin32.sourceforge.net/packages/libpng.htm   |
| jpeg | jpeg62.dll, libjpeg.dll, jpeg-62.dll or jpeg.dll         | http://gnuwin32.sourceforge.net/packages/jpeg.htm     |
| tiff | libtiff3.dll, libtiff.dll                                | http://gnuwin32.sourceforge.net/packages/tiff.htm     |
|      |                                                          | (note it requires jpeg62.dll & zlib.dll)              |
|      |                                                          | http://gnuwin32.sourceforge.net/packages/zlib.htm     |
| gif  | giflib4.dll, libungif4.dll or libungif.dll               | http://gnuwin32.sourceforge.net/packages/giflib.htm   |
|      |                                                          | http://gnuwin32.sourceforge.net/packages/libungif.htm |

## 23.3

```elisp
 (describe-variable 'image-library-alist)
```

```elisp
((xpm "libxpm.dll" "xpm4.dll" "libXpm-nox4.dll")
 (png "libpng12d.dll" "libpng12.dll" "libpng.dll" "libpng13d.dll" "libpng13.dll")
 (jpeg "jpeg62.dll" "libjpeg.dll" "jpeg-62.dll" "jpeg.dll")
 (tiff "libtiff3.dll" "libtiff.dll")
 (gif "giflib4.dll" "libungif4.dll" "libungif.dll")
 (svg "librsvg-2-2.dll")
 (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
 (glib "libglib-2.0-0.dll")
 (gobject "libgobject-2.0-0.dll"))
```

## 24.3

```elisp
 (describe-variable 'dynamic-library-alist)
```

```elisp
((xpm "libxpm.dll" "xpm4.dll" "libXpm-nox4.dll")
 (png "libpng14-14.dll" "libpng14.dll")
 (jpeg "jpeg62.dll" "libjpeg.dll" "jpeg-62.dll" "jpeg.dll")
 (tiff "libtiff3.dll" "libtiff.dll")
 (gif "giflib4.dll" "libungif4.dll" "libungif.dll")
 (svg "librsvg-2-2.dll")
 (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
 (glib "libglib-2.0-0.dll")
 (gobject "libgobject-2.0-0.dll")
 (gnutls "libgnutls-28.dll" "libgnutls-26.dll")
 (libxml2 "libxml2-2.dll" "libxml2.dll"))
```

| ext  | dlls                                                  | location                                                    |
|------|-------------------------------------------------------|-------------------------------------------------------------|
| xpm  | libxpm.dll, xpm4.dll or libxpm-nox4.dll               | (libxpm.dll included in emacs zip)                          |
| png  | libpng14-14.dll or libpng14.dll (libpng12 won't work) | http://ftp.cn.debian.org/gnome/binaries/win32/dependencies/ |
| jpeg | jpeg62.dll, libjpeg.dll, jpeg-62.dll or jpeg.dll      | http://gnuwin32.sourceforge.net/packages/jpeg.htm           |
| tiff | libtiff3.dll, libtiff.dll                             | http://gnuwin32.sourceforge.net/packages/tiff.htm           |
|      |                                                       | (note it requires jpeg62.dll & zlib1.dll)                   |
|      |                                                       | http://gnuwin32.sourceforge.net/packages/zlib.htm           |
| gif  | giflib4.dll, libungif4.dll or libungif.dll            | http://gnuwin32.sourceforge.net/packages/giflib.htm         |
|      |                                                       | http://gnuwin32.sourceforge.net/packages/libungif.htm       |

