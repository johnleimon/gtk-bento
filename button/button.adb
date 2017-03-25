-----------------------------------------------------------------
--                                                             --
-- GTK Button Example                                          --
--                                                             --
-- Copyright (c) 2017, John Leimon                             --
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
with Ada.Text_IO;  use Ada.Text_IO;
with GTK;
with GTK.Box;      use GTK.Box;
with GTK.Button;   use GTK.Button;
with GTK.Enums;    use GTK.Enums;
with GTK.Handlers; use GTK.Handlers;
with GTK.Main;
with GTK.Window;   use GTK.Window;

procedure Button is

   Window       : Gtk_Window;
   HBox         : Gtk_Box;
   Left_Button  : Gtk_Button;
   Right_Button : Gtk_Button;

   package Handler is new Callback (GTK_Window_Record);

   procedure On_Destroy (Window : access GTK_Window_Record'Class)
   is
      pragma Warnings (Off, Window);
   begin
      GTK.Main.Main_Quit;
      Put_Line("[DESTROY RECEIVED]");
   end On_Destroy;

   procedure On_Left_Click (Window : access GTK_Window_Record'Class)
   is
      pragma Warnings (Off, Window);
   begin
      Put_Line("[LEFT BUTTON CLICK]");
   end On_Left_Click;

   procedure On_Right_Click (Window : access GTK_Window_Record'Class)
   is
      pragma Warnings (Off, Window);
   begin
      Put_Line("[RIGHT BUTTON CLICK]");
   end On_Right_Click;

begin
   GTK.Main.Init;
   GTK_New (Window, Window_TopLevel);
   GTK_New (Left_Button, "Left");
   GTK_New (Right_Button, "Right");
   GTK_New_HBox (HBox);
   Set_Title (Window, "Button Example");
   Set_Border_Width (Window, 10);
   ReSize (Window, 400, 200);
   Add (HBox, Left_Button);
   Add (HBox, Right_Button);
   Add (Window, HBox);
   Handler.Connect (Window, "destroy", Handler.To_Marshaller (On_Destroy'Access));
   Handler.Object_Connect
      (Left_Button,
       "clicked",
       Handler.To_Marshaller (On_Left_Click'Access),
       Window);
   Handler.Object_Connect
      (Right_Button,
       "clicked",
       Handler.To_Marshaller (On_Right_Click'Access),
       Window);
   Show_All (Window);
   GTK.Main.Main;
end Button;
