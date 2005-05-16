{ GtkGLExt - OpenGL Extension to GTK+
  Copyright (C) 2002-2004  Naofumi Yasufuku
  These Pascal bindings copyright 2005 Michalis Kamburelis

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA.
}

{ Translated from C header /usr/include/gtkglext-1.0/gtk/gtkgl.h
  (this is probably standardized system-wide location
  of this header). }

{$mode objfpc}

unit gtkglext;

interface

uses Glib2, Gdk2, Gtk2, GdkGLExt;

const
  GtkGLExtLib = 
    {$ifdef WIN32} 'libgtkglext-win32-1.0-0.dll'
    {$else}        'libgtkglext-x11-1.0.so'
    {$endif};

{ gtkglext does not (for now) define any objects ("objects" in the glib sense),
  so "read_interface_types" section is not really used now. }
{type}
  {$define read_interface_types}
  {$I gtkglext_includes.inc}
  {$undef read_interface_types}

{$define read_interface_rest}
{$I gtkglext_includes.inc}
{$undef read_interface_rest}

implementation

{$define read_implementation}
{$I gtkglext_includes.inc}

end.