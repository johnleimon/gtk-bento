-----------------------------------------------------------------
--                                                             --
-- Gtk Clock                                                   --
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
with Ada.Calendar;              use Ada.Calendar;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics;              use Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Cairo_Canvas;              use Cairo_Canvas;
with Cairo;                     use Cairo;
with GDK.Window;                use GDK.Window;
with GLib;                      use GLib;
with GLib.Object;               use GLib.Object;
with GNAT;                      use GNAT;
with GNAT.Calendar;             use GNAT.Calendar;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GDK.Threads;               use GDK.Threads;
with GTK.Enums;                 use GTK.Enums;
with GTK.Handlers;              use GTK.Handlers;
with GTK.Main;                  use GTK.Main;
with GTK.Widget;                use GTK.Widget;
with GTK.Window;                use GTK.Window;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

procedure Clock is

   type Direction is (Up, Down);

   Font_Scaling_Factor : constant := 3.0;
   Time_Format         : constant Picture_String := "%H%M";

   Seed                : Generator;

   Background_Red      : constant := 0.0;
   Background_Green    : constant := 0.0;
   Background_Blue     : constant := 0.0;

   Stars               : Constant := 300;
   Color_Acceleration  : Constant := 0.06;

   Window              : GTK_Window;
   Canvas              : Canvas_Type;
   Die                 : Boolean := False;

   Current_Direction   : Direction := Up;

   Color               : Float := 0.30;

   package Handler is new GTK.Handlers.Callback (GTK_Window_Record);

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : access GTK_Window_Record'class)
   is
      pragma Unreferenced (Window);
   begin
      Die := True;
      Put_Line ("[ON_DESTROY]");
      GTK.Main.GTK_Exit (0);
   end On_Destroy;

   -------------
   -- Animate --
   -------------

   procedure Animate
   is
      task Animation_Task;

      Frame_Number : Natural := 0;

      ----------------
      -- Draw_Frame --
      ----------------

      procedure Draw_Frame (Canvas : Canvas_Type)
      is
         Context   : Cairo_Context;
         Timestamp : Time;
         X         : GInt;
         Y         : GInt;
         Width     : GInt;
         Height    : GInt;
         Depth     : GInt;
      begin

         Timestamp := Clock;
         Context   := Create (Get_Surface (Canvas));

         Get_Geometry (Get_Window (Window), X, Y, Width, Height, Depth);

         -- Paint Background --
         Set_Source_RGB (Context,
                         Background_Red,
                         Background_Green,
                         Background_Blue);
         Paint (Context);

         Set_Source_RGB (Context,
                         1.0,
                         1.0,
                         1.0);

         for Point in 0 .. Stars loop
            declare
                X : GDouble := GDouble (Random (Seed)) * GDouble (Width);
                Y : GDouble := GDouble (Random (Seed)) * GDouble (Height);
            begin
                Rectangle (Context,
                           X,
                           Y,
                           1.0,
                           1.0);
                Stroke (Context);
            end;
         end loop;

	 case Current_Direction is
            when Up =>
	      if Color >= 1.0 then
                 Current_Direction := Down;
              else
                 Color := Color + Color_Acceleration;
              end if;
            when Down =>
	      if Color <= 0.25 then
                 Current_Direction := Up;
              else
                 Color := Color - Color_Acceleration;
              end if;
	 end case;

         Set_Source_RGB (Context,
                         GDouble (Color),
                         0.0,
                         0.0);
         Select_Font_Face (Context,
                           "Hack",
                           Cairo_Font_Slant_Normal,
                           Cairo_Font_Weight_Normal);
         Set_Font_Size (Context, GDouble (Width) / Font_Scaling_Factor);
         Set_Line_Width (Context, 0.5);

         Height := Get_Allocation_Height (Window);
         Width  := Get_Allocation_Width (Window);

         declare
            Time_Extents : Aliased Cairo_Text_Extents;
            Time_String  : Constant String := GNAT.Calendar.Time_IO.Image
                                                 (Timestamp,
                                                  Time_Format);
         begin
            -- Paint clock text --
            Text_Extents (Context,
                          Interfaces.C.Strings.New_String (Time_String),
                          Time_Extents'Access);
            Move_To (Context,
                     (GDouble (Width) - Time_Extents.Width) / 2.0,
                      GDouble (Height) / 2.0 + Time_Extents.Height / 2.0);
            Show_Text (Context, Time_String);
         end;

         Canvas.Queue_Draw;
         Destroy (Context);
      end Draw_Frame;

      task body Animation_Task is
      begin
         loop
            GDK.Threads.Enter;
            Draw_Frame (Canvas);
            GDK.Threads.Leave;
            delay 0.01;
            Frame_Number := Frame_Number + 1;
            exit when Die = True;
         end loop;
         GTK.Main.GTK_Exit(0);
      end Animation_Task;
   begin
      GDK.Threads.Enter;
      GTK.Main.Main;
      GDK.Threads.Leave;
   end Animate;

begin
   GDK.Threads.G_Init;
   GDK.Threads.Init;
   GTK.Main.Init;
   GTK_New (Window, Window_Toplevel);
   GTK_New (Canvas);
   Handler.Connect (Window, "destroy", Handler.To_Marshaller (On_Destroy'Access));
   Set_Title (Window, "Clock");
   Set_USize (Canvas, 400, 400);
   Add (Window, Canvas);
   Show_All (Window);
   Animate;
end Clock;
