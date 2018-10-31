with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Maps;

package body Client_Collections is
   package ATIO renames Ada.Text_IO;
	package ASM renames Ada.Strings.Maps;
   use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;

	procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);

   procedure Add_Client(Collection: in out Collection_Type;
                         EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String;
                         Unique: in Boolean) is
      P_Aux: Cell_A;
      Found: Boolean := False;
   begin
		P_Aux := Collection.P_First;
		if Collection.P_First = null then
			Collection.P_First := new Cell'(EP, Nick, null);
   	   Collection.Total := 1;
		--Si Unique = False es lista reader y añade siempre
		elsif Unique = False then
			P_Aux := new Cell'(EP, Nick, Collection.P_First);
			Collection.P_First := P_Aux;
			Collection.Total := Collection.Total + 1;
		--Si Unique = True es lista writer y añade si no lo encuentra.
		--Si lo encuentra, eleva la excepcion.
		elsif Unique = True then
		   while not Found and P_Aux /= null loop
				if P_Aux.Nick = Nick then
			      raise Client_Collection_Error;
		    	end if;
				P_Aux := P_Aux.Next;
			end loop;
			if not Found then
				P_Aux := new Cell'(EP, Nick, Collection.P_First);
				Collection.P_First := P_Aux;
				Collection.Total := Collection.Total + 1;
			end if;
		end if;
   end Add_Client;

   procedure Delete_Client (Collection: in out Collection_Type;
                            Nick: in ASU.Unbounded_String) is
		P_Aux: Cell_A;
		P_Aux_2: Cell_A;
      Found: Boolean := False;
	begin
		P_Aux := Collection.P_First;
		P_Aux_2 := P_Aux;
		while not Found and P_Aux /= null loop
		   --Si el cliente es el primero de la lista, Collection.P_First
		   --apunta al siguiente y se borra la primera celda
			if P_Aux.Nick = Nick and P_Aux = Collection.P_First then
				Collection.P_First := P_Aux.Next;
				ATIO.Put_Line("|" & ASU.To_String(P_Aux.Nick) & "| deleted");
				Free(P_Aux);
				Found := True;
		   --Si el cliente no es el primero, libero con P_Aux y
		   --enlazo con P_Aux_2
         elsif P_Aux.Nick = Nick and P_Aux /= Collection.P_First then
            P_Aux_2.Next := P_Aux.Next;
				ATIO.Put_Line("|" & ASU.To_String(P_Aux.Nick) & "| deleted");
            Free(P_Aux);
            P_Aux := P_Aux_2.Next;
            Found := True;
			--Si no lo encuentra, sigue recorriedo la lista
			else
				P_Aux_2 := P_Aux;
				P_Aux := P_Aux.Next;
			end if;
		end loop;
	end Delete_Client;

	function Search_Client (Collection: in Collection_Type;
									 EP: in LLU.End_Point_Type)
									 return ASU.Unbounded_String is
		P_Aux: Cell_A;
		Nick: ASU.unbounded_String;
		Found: Boolean;
	begin
		P_Aux := Collection.P_First;
		Found := False;
		while not Found and P_Aux /= null loop
		--Si lo encuentra y existe la lista, lo muestra.
		--Si no, continua recorriendo la lista
			if P_Aux.Client_EP = EP then
				Nick := P_Aux.Nick;
				Found := True;
			end if;
			P_Aux := P_Aux.Next;
		end loop;
			if Found = False then
				raise Client_Collection_Error;
			end if;
		return Nick;
	end Search_Client;

   procedure Send_To_All (Collection: in Collection_Type;
								P_Buffer: access LLU.Buffer_Type) is
      P_Aux: Cell_A;
   begin
      P_Aux := Collection.P_First;
      while P_Aux /= Null loop
			LLU.Send(P_Aux.Client_EP, P_Buffer);
         P_Aux := P_Aux.Next;
      end loop;      
   end Send_To_All;

	function Collection_Image(Collection: in Collection_Type) return String is
		P_Aux : Cell_A;
		Client_Data_Collection: ASU.Unbounded_String;
		Client_EP: LLU.End_Point_Type;
		Client_Nick: ASU.Unbounded_String;
		Client_IP: ASU.Unbounded_String;
		Client_Port: ASU.Unbounded_String;
		Image_Line: ASU.Unbounded_String;
		Position: Integer;
	begin
        P_Aux := Collection.P_First;
      if P_Aux = Null then
         ATIO.Put_Line("No clients.");
      else
         while P_Aux /= null loop
				Client_EP := P_Aux.Client_EP;
            Client_Nick := P_Aux.Nick;
				LLU.Bind_Any(Client_EP);
				Image_Line := ASU.To_Unbounded_String(LLU.Image(Client_EP));
				Position := ASU.Index(Image_Line, ASM.To_Set(":")) + 1;
				Image_Line := ASU.Tail(Image_Line, ASU.Length(Image_Line) - Position);
				Position := ASU.Index(Image_Line, ASM.To_Set(","));
				Client_IP := ASU.Head(Image_Line, Position - 1);
				Image_Line := ASU.Tail(Image_Line, ASU.Length(Image_Line) - Position);
				Position := ASU.Index(Image_Line, ASM.To_Set(":")) + 1;
				Client_Port := ASU.Tail(Image_Line, Position - 1);
				Client_Data_Collection := ASU.To_Unbounded_String (ASU.To_String(Client_Data_Collection) 
													& ASU.To_String(Client_IP) & ":"
													& ASU.To_String(Client_Port) & " "
													& ASU.To_String(Client_Nick) & ASCII.LF);
				P_Aux := P_Aux.Next;
         end loop;
      end if;
		return ASU.To_String(Client_Data_Collection);
	end Collection_Image;
end Client_Collections;
