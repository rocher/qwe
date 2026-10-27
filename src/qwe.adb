with Tree_Sitter;     use Tree_Sitter;
with Tree_Sitter_API; use Tree_Sitter_API;
with Ada.Text_IO;     use Ada.Text_IO;

procedure Qwe is
   --  Importem la funció que genera la gramàtica simple
   function tree_sitter_qwe_ada return TSLanguage
   with Import, Convention => C, External_Name => "tree_sitter_qwe_ada";

   P    : Tree_Sitter.Parser;
   T    : Tree_Sitter.Tree;
   Root : Tree_Sitter.Node;

   Input_Text : constant String :=
     "@lp primera línia" & ASCII.LF & "@lp segona línia";
begin
   Put_Line ("Iniciant test de QWE...");

   --  1. Configurem el llenguatge
   P.Set_Language (tree_sitter_qwe_ada);
   Put_Line ("  - llenguatge configurat.");

   --  2. Parsegem el text
   declare
      T : constant Tree_Sitter.Tree := P.Parse_String (Input_Text);
   begin
      Root := T.Root_Node;
      Put_Line ("  - text parsejat.");

      --  3. Verifiquem resultats
      Put_Line ("Tipus de node arrel: " & Kind (Root));
      Put_Line ("Número de fills: " & Child_Count (Root)'Image);

      for I in 0 .. Child_Count (Root) - 1 loop
         declare
            C : constant Tree_Sitter.Node := Child (Root, I);
         begin
            Put_Line ("  Node [" & I'Image & "]: " & Kind (C));
         end;
      end loop;
   end;

   Put_Line ("Test finalitzat correctament!");
end Qwe;
