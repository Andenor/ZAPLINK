*    This program is part of ZAPLink framework. https://code.google.com/p/zaplink/
*    Copyright (C) 2010  Taryck BENSIALI
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
REPORT  zaplink MESSAGE-ID zaplink.

INCLUDE zaplink_top.
INCLUDE zaplink_scr.

*/----------------------main------------------------------------------\
START-OF-SELECTION.
  PERFORM create_file.
*   create empty Container
  CASE abap_true.
    WHEN a_create.    PERFORM create.
    WHEN a_auto.      PERFORM auto.
    WHEN a_synch.     PERFORM refresh.
    WHEN a_add.       PERFORM add.          " add object to Container
    WHEN a_import.    PERFORM import.       " import Container
    WHEN a_dsp.       PERFORM display.      " display objects in a Container
    WHEN a_uninst.    PERFORM uninstall.    " Uninstall a Container
    when a_add_p.     PERFORM add_comp.
    WHEN a_add_tr.    PERFORM add_tr.
  ENDCASE.

END-OF-SELECTION.
*\--------------------------------------------------------------------/
  INCLUDE zaplink_frm.
