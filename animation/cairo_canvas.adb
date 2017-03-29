-----------------------------------------------------------------
--                                                             --
-- Cario Canvas                                                --
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
with Cairo;
with Cairo.Image_Surface; use Cairo.Image_Surface;
with Cairo.Surface;    use Cairo.Surface;
with GDK;              use GDK;
with GLib;             use GLib;
with GTK.Enums;        use GTK.Enums;
with GDK.Cairo;        use GDK.Cairo;
with GDK.Drawable;     use GDK.Drawable;
with GDK.Event;        use GDK.Event;
with GDK.Window;       use GDK.Window;
with GTK.Drawing_Area; use GTK.Drawing_Area;
with GTK.Handlers;     use GTK.Handlers;

package body Cairo_Canvas is

   package Return_Handler is new GTK.Handlers.Return_Callback
      (Canvas_Type_Record, Boolean);
   package Handler is new GTK.Handlers.Callback
      (Canvas_Type_Record);

   function On_Resize
      (Buffer : access Canvas_Type_Record'Class;
       Event  : GDK.Event.GDK_Event)
       return Boolean;

   function On_Redraw
      (Buffer : access Canvas_Type_Record'Class;
       Event  : GDK.Event.GDK_Event)
       return Boolean;

   procedure On_Destroy
      (Buffer : access Canvas_Type_Record'Class);

   -----------------
   -- Get_Surface --
   -----------------

   function  Get_Surface
      (Buffer : access Canvas_Type_Record)
       return Cairo_Surface
   is
   begin
      return Buffer.Surface;
   end Get_Surface;

   -------------
   -- GTK_New --
   -------------

   procedure GTK_New (Buffer : out Canvas_Type)
   is
   begin
      Buffer := new Canvas_Type_Record;
      GTK.Drawing_Area.Initialize (Buffer);

      Return_Handler.Connect
         (Buffer,
          "configure_event",
          Return_Handler.To_Marshaller (On_Resize'Access));
      Return_Handler.Connect
         (Buffer,
          "expose_event",
          Return_Handler.To_Marshaller (On_Redraw'Access));
      Handler.Connect
         (Buffer,
          "destroy",
          Handler.To_Marshaller (On_Destroy'Access));
   end GTK_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
      (Buffer : access Canvas_Type_Record'Class)
   is
   begin
      if Buffer.Surface /= Cairo.Null_Surface then
         Destroy (Buffer.Surface);
      end if;
   end On_Destroy;

   ---------------
   -- On_Redraw --
   ---------------

   function On_Redraw
      (Buffer : access Canvas_Type_Record'Class;
       Event  : GDK.Event.GDK_Event)
       return Boolean
   is
      pragma Unreferenced (Event);
      Context : Cairo_Context;
   begin
      Context := Create (GDK.Drawable.GDK_Drawable (Get_Window (Buffer)));
      Set_Source_Surface (Context, Buffer.Surface, 0.0, 0.0);
      Paint (Context);
      Destroy(Context);
      return False;
   end On_Redraw;

   ---------------
   -- On_Resize --
   ---------------

   function On_Resize
      (Buffer : access Canvas_Type_Record'Class;
       Event  : GDK.Event.GDK_Event)
       return Boolean
   is
      pragma Unreferenced (Event);
      Old_Surface : Cairo_Surface := Buffer.Surface;
      Context     : Cairo_Context;
   begin
      Buffer.Surface := Create (Cairo_Format_RGB24,
                                GInt (Get_Allocation_Width (Buffer)),
                                GInt (Get_Allocation_Height (Buffer)));

      if Old_Surface /= Cairo.Null_Surface then
         -- Copy previous surface content to avoid flickering --
         Context := Create (Buffer.Surface);
         Set_Source_Surface (Context, Old_Surface, 0.0, 0.0);
         Paint (Context);
         Destroy (Context);
         Destroy (Old_Surface);
      end if;

      return True;
   end On_Resize;

end Cairo_Canvas;
