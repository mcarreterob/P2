with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Lower_Layer_UDP;


package Chat_Messages is

   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   
	type Message_Type is (Init, Writer, Server);
	type Seq_N_T is mod Integer'Last;

end Chat_Messages;
