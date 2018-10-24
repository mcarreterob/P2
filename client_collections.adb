with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Client_Collections is
   package ATIO renames Ada.Text_IO;
   use type ASU.Unbounded_String;

   --Compruebo si la lista está vacía
   function Is_Empty(Collection: Collection_Type) return Boolean is
   begin
     return Collection.P_First = null;
   end Is_Empty;

	procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);

   procedure Add_Client(Collection: in out Collection_Type;
                         EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String;
                         Unique: in Boolean) is
      P_Aux: Cell_A;
      Found: Boolean := False;
   begin
		P_Aux := Collection.P_First;
		if Is_Empty(Collection) then
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

	procedure Search_Word (List: in Word_List_Type;
			                 Word: in ASU.Unbounded_String;
			                 Count: out Natural) is
		P_Aux: Word_List_Type;
		Searched_Word: ASU.unbounded_String;
		Found: Boolean;
	begin
		P_Aux := List;
		Found := False;
		Count := 0;
		while not Found and P_Aux /= null loop
		--Si lo encuentra y existe la lista, lo muestra.
		--Si no, continua recorriendo la lista
			if P_Aux.Word = Word then
				Searched_Word := P_Aux.Word;
				Count := P_Aux.Count;
				Found := True;
				ATIO.Put_Line("|" & ASU.To_String(Searched_Word)
									& "| - " & Natural'Image(Count));
			else
				P_Aux := P_Aux.Next;
			end if;
		end loop;
		if Found = False then
			ATIO.Put_Line("The word you are searching is not in the list");
		end if;
	end Search_Word;

	procedure Max_Word (List: in Word_List_Type;
	                    Word: out ASU.Unbounded_String;
		                 Count: out Natural) is
		P_Aux: Word_List_Type;
	begin
		P_Aux := List;
		Word := P_Aux.Word;
		Count := P_Aux.Count;
		while P_Aux /= null loop
		--Va comparando cada campo Count con el Count de la siguiente celda,
		--si el de la siguiente celda es mayor, se lo guarda y continua
		--hasta que llega al final de la lista
			if Count < P_aux.Count then
				Word := P_Aux.Word;
				Count := P_Aux.Count;
			else
				P_Aux := P_Aux.Next;
			end if;
		end loop;
		ATIO.Put("The most frequent word: |" & ASU.To_String(Word)
					& "| - " & Natural'Image(Count));
	end Max_Word;

   procedure Sent_To_All (Collection: in Collection_Type;
								P_Buffer: access LLU.Buffer_Type) is
      P_Aux: Cell_A;
   begin
      P_Aux := Collection.P_First;
      while not Is_Empty(Collection) loop
			LLU.Send(P_Aux.Client_EP, P_Buffer);
         P_Aux := P_Aux.Next;
      end loop;      
   end Sent_To_All;

	procedure Delete_List (List: in out Word_List_Type) is
		P_Aux : Word_List_Type;
	begin
      if Is_Empty(List) then
         ATIO.Put_Line("No words.");
      else
         P_Aux := List;
         while not Is_Empty(P_Aux) loop
				List := List.Next;
            Free(P_Aux);
				P_Aux := List;
         end loop;
      end if;
	end Delete_List;
end Client_Collections;
