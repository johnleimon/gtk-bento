-----------------------------------------------------------------
--                                                             --
-- Cario Canvas Specification                                  --
--                                                             --
-- Copyright (c) 2017  John Leimon                             --
--                                                             --
-- Permission to use, copy, modify, and/or distribute          --
-- this software for any purpose with or without fee           --
-- is hereby granted, provided that the above copyright        --
-- notice and this permission notice appear in all copies.     --
--                                                             --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR             --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE       --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY         --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE         --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL         --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS       --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF            --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING      --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      --
-- THIS SOFTWARE.                                              --
-----------------------------------------------------------------
with Cairo;            use Cairo;
with GTK.Drawing_Area;

package Cairo_Canvas is

   type Canvas_Type_Record is new
      GTK.Drawing_Area.GTK_Drawing_Area_Record with private;
   type Canvas_Type is access all Canvas_Type_Record'Class;

   procedure GTK_New
      (Buffer : out Canvas_Type);

   function  Get_Surface
      (Buffer : access Canvas_Type_Record)
       return Cairo_Surface;

private

   type Canvas_Type_Record is new
      GTK.Drawing_Area.GTK_Drawing_Area_Record
   with record
      Surface : Cairo_Surface := Cairo.Null_Surface;
   end record;

end Cairo_Canvas;
