with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Chat_Messages;
with Ada.Command_Line;
with Client_Collections;

procedure Chat_Client is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
	package ATIO renames Ada.Text_IO;
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
   Expired : Boolean;
	Mess: CM.Message_Type;
	Maquina: ASU.Unbounded_String := ASU.To_Unbounded_String(ACL.Argument(1));
	Nick: ASU.Unbounded_String;
	IP: String := LLU.To_IP(ASU.To_String(Maquina));
	Puerto: Integer;
	Finish: Boolean := False;
	Comprobacion: ASU.Unbounded_String;

begin
	--Maquina := ASU.To_Unbounded_String(ACL.Argument(1));
	--IP := ASU.To_String(ASU.To_Unbounded_String(LLU.To_IP(Maquina)));
	Puerto := Integer'Value(ACL.Argument(2));
	Nick := ASU.To_Unbounded_String(ACL.Argument(3));

   -- Construye el End_Point en el que está atado el servidor
   Server_EP := LLU.Build(IP, Puerto);
   -- Construye un End_Point libre cualquiera y se ata a él
   LLU.Bind_Any(Client_EP);

	if Nick = "reader" then
		--MODO LECTOR
		-- reinicializa el buffer para empezar a utilizarlo
   	LLU.Reset(Buffer);
		Mess := CM.Init;
		CM.Message_Type'Output(buffer'Access, Mess);

		-- introduce el End_Point del cliente en el Buffer
		-- para que el servidor sepa dónde responder
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP);

		-- introduce el Unbounded_String en el Buffer
   	-- (se coloca detrás del End_Point introducido antes)
   	ASU.Unbounded_String'Output(Buffer'Access, Nick);

		-- envía el contenido del Buffer
   	LLU.Send(Server_EP, Buffer'Access);

	   -- reinicializa (vacía) el buffer para ahora recibir en él
   	LLU.Reset(Buffer);

		loop
			-- espera 2.0 segundos a recibir algo dirigido al Client_EP
   		--   . si llega antes, los datos recibidos van al Buffer
		   --     y Expired queda a False
   		--   . si pasados los 2.0 segundos no ha llegado nada, se
			--abandona la espera y Expired queda a True
   		LLU.Receive(Client_EP, Buffer'Access, 1000.0, Expired);
			if Expired then
				Ada.Text_IO.Put_Line ("Plazo expirado");
			else
				-- saca del Buffer un Unbounded_String
				Mess := CM.Message_Type'Input(Buffer'Access);
				Nick := ASU.Unbounded_String'Input(Buffer'Access);
				Reply := ASU.Unbounded_String'Input(Buffer'Access);
				ATIO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Reply));
				--ATIO.Put_Line(ASU.To_String(Reply));
				LLU.Reset(Buffer);
			end if;
		end loop;
	else
		--MODO ESCRITOR
		-- reinicializa el buffer para empezar a utilizarlo
   	LLU.Reset(Buffer);
		Mess := CM.Init;
		CM.Message_Type'Output(buffer'Access, Mess);

		-- introduce el End_Point del cliente en el Buffer
		-- para que el servidor sepa dónde responder
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP);

		-- introduce el Unbounded_String en el Buffer
   	-- (se coloca detrás del End_Point introducido antes)
   	ASU.Unbounded_String'Output(Buffer'Access, Nick);

		-- envía el contenido del Buffer
   	LLU.Send(Server_EP, Buffer'Access);

	   -- reinicializa (vacía) el buffer para ahora recibir en él
   	LLU.Reset(Buffer);

		while not Finish loop
			--MODO ESCRITOR
			-- reinicializa el buffer para empezar a utilizarlo
			LLU.Reset(Buffer);
			Mess := CM.Writer;
			CM.Message_Type'Output(buffer'Access, Mess);
			ATIO.Put("Message: ");
			Request := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);
			if Request = ".quit" then
				Finish := True;
			else
				-- introduce el End_Point del cliente en el Buffer
				-- para que el servidor sepa dónde responder
				LLU.End_Point_Type'Output(Buffer'Access, Client_EP);

				-- introduce el Unbounded_String en el Buffer
				-- (se coloca detrás del End_Point introducido antes)
				ASU.Unbounded_String'Output(Buffer'Access, Request);

				-- envía el contenido del Buffer
				LLU.Send(Server_EP, Buffer'Access);
			end if;
			-- reinicializa (vacía) el buffer para ahora recibir en él
   		LLU.Reset(Buffer);
		end loop;
	end if;

   -- termina Lower_Layer_UDP
   LLU.Finalize;

exception
   when Ex:others =>
      ATIO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Client;
