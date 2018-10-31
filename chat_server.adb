with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Chat_Messages;
with Client_Collections;
with Ada.Command_Line;

procedure Chat_Server is
   package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM renames Chat_Messages;
	package CC renames Client_Collections;
	use type CM.Message_Type;
	use type ASU.Unbounded_String;

   Server_EP: LLU.End_Point_Type;
   Client_EP: LLU.End_Point_Type;
   Buffer: aliased LLU.Buffer_Type(1024);
   Request: ASU.Unbounded_String;
   Reply: ASU.Unbounded_String;
	Maquina: ASU.Unbounded_String := ASU.To_Unbounded_String(LLU.Get_Host_Name);   
	IP: String := LLU.To_IP(ASU.To_String(Maquina));
	Expired: Boolean;
	Mess: CM.Message_Type;
	Nick: ASU.Unbounded_String;
	Reader_List: CC.Collection_Type;
	Writer_List: CC.Collection_Type;

begin

   -- construye un End_Point en una dirección y puerto concretos:
	--IP: la de la maquina en la que se ejecute el programa
	--Puerto: el que se le pasa por la linea de comandos
   Server_EP := LLU.Build (IP, Integer'Value(ACL.Argument(1)));

   -- se ata al End_Point para poder recibir en él
   LLU.Bind (Server_EP);

   -- bucle infinito
   loop
      -- reinicializa (vacía) el buffer para ahora recibir en él
      LLU.Reset(Buffer);

      -- espera 1000.0 segundos a recibir algo dirigido al Server_EP
      --   . si llega antes, los datos recibidos van al Buffer
      --     y Expired queda a False
      --   . si pasados los 1000.0 segundos no ha llegado nada,
		--		se abandona la espera y Expired queda a True
      LLU.Receive (Server_EP, Buffer'Access, 1000.0, Expired);

      if Expired then
         Ada.Text_IO.Put_Line ("Plazo expirado, vuelvo a intentarlo");
      else
			--Saca del buffer el tipo de mensaje que recibe
			Mess := CM.Message_Type'Input(Buffer'Access);
			if Mess = CM.Init then
				--saco los datos del cliente: EP y Nick
				Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
				Nick := ASU.Unbounded_String'Input(Buffer'Access);
				if Nick = "reader" then
					CC.Add_Client(Reader_List, Client_EP, Nick, False);
					ATIO.Put_Line("INIT received from " & ASU.To_String(Nick));
				elsif Nick /= "reader" then
					begin
						CC.Add_Client(Writer_List, Client_EP, Nick, True);
						ATIO.Put_Line("INIT received from " & ASU.To_String(Nick));
						-- reinicializa (vacía) el buffer
         			LLU.Reset (Buffer);
						Mess := CM.Server;
						CM.Message_Type'Output(Buffer'Access, Mess);
						-- introduce el Nick en el Buffer
         			ASU.Unbounded_String'Output (Buffer'Access, Nick);
						Reply := ASU.To_Unbounded_String("server: " & ASU.To_String(Nick)
																	& " joins the chat");
						-- introduce el Unbounded_String en el Buffer
         			ASU.Unbounded_String'Output (Buffer'Access, Reply);
						CC.Send_To_All(Reader_List, Buffer'Access);
						exception
							when CC.Client_Collection_Error =>
								ATIO.Put_Line("INIT received from" & ASU.To_String(Nick)
													& ". IGNORED, nick already used");
					end;
				end if;
			elsif Mess = CM.Writer then
				-- Saco los datos del cliente: EP
				Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
				Reply := ASU.Unbounded_String'Input(Buffer'Access);
				begin
					Nick := CC.Search_Client(Writer_List, Client_EP);
					ATIO.Put_Line("Writer received from " & ASU.To_String(Nick)
										& ": " & ASU.To_String(Reply));
					-- Reinicializa (vacía) el buffer
         		LLU.Reset (Buffer);
					Mess := CM.Server;
					-- Introduce en el Buffer el tipo de mensaje
					CM.Message_Type'Output(Buffer'Access, Mess);
					-- Introduce el Nick en el Buffer
					ASU.Unbounded_String'Output(Buffer'Access, Nick);
					-- Introduce el Unbounded_String en el Buffer
					ASU.Unbounded_String'Output(Buffer'Access, Reply);
					CC.Send_To_All(Reader_List, Buffer'Access);
					exception
						when CC.Client_Collection_Error =>
							ATIO.Put_Line("WRITER received from unknown client. IGNORED");
				end;
			end if;
      end if;
   end loop;

   -- Nunca se alcanza este punto
   -- Si se alcanzara, habría que llamar a LLU.Finalize;

exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Server;
