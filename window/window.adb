-----------------------------------------------------------------
--                                                             --
-- GTK Window Example                                          --
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
with GTK.Enums;    use GTK.Enums;
with GTK.Handlers; use GTK.Handlers;
with GTK.Main;
with GTK.Window;   use GTK.Window;

procedure Window is

   Window : Gtk_Window;

   package Handler is new Callback (GTK_Window_Record);

   procedure On_Destroy (Window : access GTK_Window_Record'Class)
   is
      pragma Warnings (Off, Window);
   begin
      GTK.Main.Main_Quit;
      Put_Line("[DESTROY RECEIVED]");
   end On_Destroy;

begin
   GTK.Main.Init;
   GTK_New (Window, Window_TopLevel);
   Set_Title (Window, "Window Example");
   Set_Border_Width (Window, 10);
   ReSize (Window, 500, 400);
   Handler.Connect (Window, "destroy", Handler.To_Marshaller (On_Destroy'Access));
   Show_All (Window);
   GTK.Main.Main;
end Window;
