------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . D O C                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2016 ESA & ISAE.                    --
--                                                                          --
-- Ocarina  is free software; you can redistribute it and/or modify under   --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. Ocarina is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY; without even the implied warranty of               --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Charset;           use Charset;
with Ocarina.Backends.Utils;
with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Instances; use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;
use Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.BE_AADL.Components;

with Ocarina.Backends.Utils;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Ocarina.Backends.Doc is
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ocarina.ME_AADL;
   use Ocarina.Backends.Utils;
   use AIN;
   use Ocarina.BE_AADL.Components;

   FD_System         : File_Type;

   FD               : File_Type;

   --------------------------------------
   -- Procedura che visita i vari nodi --
   --------------------------------------

   procedure Visit (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);

   procedure Print_Title ( Title : String );
   procedure Print_Subtitle ( Title : String );
   procedure Print_Header (CharIn : String; Title : String);
   procedure Print_Func_Output (Func : String; Output : String);

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Registration of the generator
      Register_Backend ("doc", Generate'Access, Doc_Backend);
   end Init;

   --------------
   -- Generate --
   --------------
   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
   begin
      --  Instantiate the AADL tree
      Instance_Root := Instantiate_Model (AADL_Root);
      if No (Instance_Root) then
         raise Program_Error;
      end if;
      Print_Title ("Inizio Documentazione");
      Visit (Root_System (Instance_Root));
   end Generate;

   -----------
   -- Visit --
   -----------
   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Visit (Root_System (E));

         when K_Component_Instance =>
            Visit_Component_Instance (E);

         when others =>
            null;
      end case;
   end Visit;

   procedure Visit_Component_Instance (E : Node_Id) is
	Category    : constant Component_Category := Get_Category_Of_Component (E);
	Comp_Name   : constant Name_Id := Display_Name (Identifier (E));
	F           : Node_Id;
   begin
      -- La Normalize_Name sotituisce i punti . con dei trattini bassi _
      -- Estremamante scomoda
      Print_Func_Output ("Normalize_Name (Display_Name (Identifier (E)))", Get_Name_String (Normalize_Name (Comp_Name)));
      Print_Func_Output ("Display_Name (Identifier (E))", Get_Name_String (Comp_Name));
      
      Print_Func_Output ("Get_Category_Of_Component (E)", Component_Category'Image (Category));
      
      Print_Func_Output ("Namespace", Get_Name_String (Display_Name (Identifier (Namespace (E)))));
      
      Print_Title ("Features");

      -- Se si usa la 'Image di ADA spesso si ottengono numeri, bisogna quindi usare
      -- la funzione di Ocarina Get_Name_String che restituisce il nome effettivo usato nell'AADL
      
      -- Features (E) ritorna un List_Id
      -- First_Node converte da List_Id a Node_Id
      if Present (Features (E)) then
         Print_Func_Output ("Present (Features (E))", "True");
         
         F := First_Node (Features (E));
         
         -- Faccio un loop e fintanto che possiedo una feature F su cui indagare
         -- vado avanti. Si passa alla Feature successiva con la chiamata:
         -- F := Next_Node (F);
         
         while Present (F) loop
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));
            
            -- ###############################
            -- ### Direction: in/out/inout ###
            -- ###############################            
            declare
               Direction_Kind : Name_Id;
            begin
               -- Controllo se la feature controllata (che è un nodo dell'albero) è di tipo
               -- K_Port_Spec_Instance. I tipi sono definiti nel file ocarina-me_aadl-aadl_instances-nodes.ads
               -- come una enum chiama NodeKind
               if Kind (F) = K_Port_Spec_Instance then
                  
                  if Is_In (F) and then not Is_Out (F) then
                     Direction_Kind := Get_String_Name ("in");
                  elsif (not Is_In (F)) and then Is_Out (F) then
                     Direction_Kind := Get_String_Name ("out");
                  elsif Is_In (F) and then Is_Out (F) then
                     Direction_Kind := Get_String_Name ("inout");
                  end if;
                  
               else
                  Direction_Kind := Get_String_Name ("none");
               end if;
               
               Print_Func_Output ("Direction_Kind", Get_Name_String (Direction_Kind));
               
               
            end;
            
            -- ###################################
            -- ### Type: event/data/event data ###
            -- ###################################
            declare
               Type_Kind : Name_Id;
               Name_F : Name_Id;
            begin
               -- Controllo la tipologia di porta. Per le porte NON event posso anche chiedere
               -- il nome del tipo associato, mentre Ocarina va in crash se lo si chiede per
               -- quelle di tipo data (ed infatti in AADL non lo si può neanche specificare).
               Name_F := Get_String_Name ("none");
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_Event (F) and then not Is_Data (F) then
                     Type_Kind := Get_String_Name ("event");
                  elsif not Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("data");
                     Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
                  elsif Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("event_data");
                     Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
                  end if;
               elsif Kind (F) = K_Subcomponent_Access_Instance then
                  Type_Kind := Get_String_Name ("access");
                  Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
               else
                  Type_Kind := Get_String_Name ("feature");
                  Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
               end if;
               
               Print_Func_Output ("Type_Kind", Get_Name_String (Type_Kind));
               Print_Func_Output ("Display_Name",  Get_Name_String (Name_F));
            end;
           
            
            Put_Line("");
            -- Passo alla Feature successiva
            F := Next_Node (F);
         end loop;
                
      else
         Print_Func_Output ("Present (Feature (E))", "False");
      end if;
      
      Print_Title ("Properties");
      
      if Present (Properties (E)) then
         Print_Func_Output ("Present (Properties (E))", "True");
         
         F := First_Node (Properties (E));
         while Present (F) loop
            
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));
            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Properties (E))", "False");
      end if;
      
      Print_Title ("Connections");
      
      if Present (Connections (E)) then
         Print_Func_Output ("Present (Connections (E))", "True");
         
         F := First_Node (Connections (E));
         while Present (F) loop
            
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));
            Print_Func_Output ("Kind (F)", Node_Kind'Image (Kind (F)));
            Print_Func_Output ("Get_Category_Of_Connection (F)", Port_Connection_Type'Image ( Get_Category_Of_Connection (F)) );
            
            if Get_Category_Of_Connection (F) = CT_Port_Connection then
               
               Print_Func_Output ("Source",  Get_Name_String (Display_Name (Identifier (Get_Referenced_Entity (Source (F))))));
               Print_Func_Output ("Dest",  Get_Name_String (Display_Name (Identifier (Get_Referenced_Entity (Destination (F))))));
               
               Print_Func_Output ("Parent Source",  Get_Name_String (Display_Name (Identifier (Parent_Component ((Get_Referenced_Entity (Source (F))))))));
               Print_Func_Output ("Parent Source Name",  Get_Name_String (Display_Name (Identifier (Item (AIN.First_Node (Path (Source (F))))))));
               
               Print_Func_Output ("Parent Dest",  Get_Name_String (Display_Name (Identifier (Parent_Component ((Get_Referenced_Entity (Destination (F))))))));
               Print_Func_Output ("Parent Dest Name",  Get_Name_String (Display_Name (Identifier (Item (AIN.First_Node (Path (Destination (F))))))));
               
               
               
            end if;
            
            
            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Connections (E))", "False");
      end if;

      Print_Title ("Subcomponents");
      if Present (Subcomponents (E)) then
         Print_Func_Output ("Present (Subcomponents (E))", "True");
         F := First_Node (Subcomponents (E));
         while Present (F) loop
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));
            -- Print_Func_Output ("Fully_Qualified_Instance_Name (E)", Get_Name_String (Fully_Qualified_Instance_Name (Corresponding_Instance (F))));
      
            
            Visit (Corresponding_Instance (F));
            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Subcomponents (E))", "False");
      end if;
      
      Put_Line("");
      Put_Line("------------------------");
      Put_Line("");
      
   end Visit_Component_Instance;


   procedure Print_Func_Output (Func : String; Output : String) is
      Temp_String : Unbounded_String;
   begin
      Append (Temp_String, "- ");
      Append (Temp_String, Func);
      Append (Temp_String, ": ");
      Append (Temp_String, Output);
      Put_Line (To_String (Temp_String));
   end Print_Func_Output;
   
   
   procedure Print_Subtitle (Title : String) is
   begin
      Print_Header ("-", Title);
   end Print_Subtitle;
   
   procedure Print_Title (Title : String) is
   begin
      Print_Header ("#", Title);
   end Print_Title;
   
   -----------------------------------------
   -- Stampa a schermo un titolo visibile --
   -- nell'output a terminale             --
   -----------------------------------------
   procedure Print_Header (CharIn : String; Title : String) is
      Temp_Title_String : Unbounded_String;
   begin
      for I in 1 .. Title'Length + 8 loop
         Append (Temp_Title_String, CharIn);
      end loop;
      Put_Line (To_String (Temp_Title_String));
      Temp_Title_String := To_Unbounded_String ("");
      
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, " ");
      Append (Temp_Title_String, Title);
      
      Append (Temp_Title_String, " ");
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Put_Line (To_String (Temp_Title_String));
      
      Temp_Title_String := To_Unbounded_String ("");
      
      for I in 1 .. Title'Length + 8 loop
         Append (Temp_Title_String, CharIn);
      end loop;
      Put_Line ( To_String (Temp_Title_String));
      
   end Print_Header;
	

   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Doc;
