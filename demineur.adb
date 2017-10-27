with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
-- ajouter des librairies selon vos besoins

procedure demineur is
   -- type tableau pour l'aire de jeu
   type T_Board is array(Integer range <>,Integer range <>) of Integer;
   -- type tableau pour les drapeaux et les cases ouvertes/ferm�es
   type T_Display is array(Integer range <>,Integer range <>) of Boolean;

-------------------------------------------------------------------------
   -- Proc�dure d'affichage de l'aire de jeu tenant compte des cases
   -- ouvertes, ferm�es et avec drapeau
   -- Param�tre <Visibilite> : cases ouvertes/ferm�es
   --           <Flags>      : cases avec ou sans drapeau
   --           <Board>      : aire de jeu 
   -- NE PAS MODIFIER CETTE PROCEDURE
   procedure Affichage(Visibilite, Flags : in T_Display;
                       Board : in T_Board) is
   begin
      Put("   ");
      for J in Visibilite'range(2) loop
         Put(J,4);
      end loop;
      Put_Line(Standard_Error,"");
      for I in Visibilite'range(1) loop
         Put(I,4);
         for J in Visibilite'range(2) loop
            if Flags(I,J) then
               Put(Standard_Error,"  D ");
            else
               if Visibilite(I,J) then 
                  Put(Standard_Error,Board(I,J),3); Put(" ");                 
               else
                  Put(Standard_Error,"  . ");
               end if;
            end if;
         end loop;
         Put_Line(Standard_Error,"");
      end loop;
      Put_Line(Standard_Error,"");
   end Affichage;
   
-------------------------------------------------------------------------
   -- Proc�dure qui place les bombes aux positions pass�es en argument sur la
   -- ligne de commande via les variables <Argument_Count> (de type Natural) 
   -- et <Argument> (de type tableau de String) du paquetage <Command_Line>
   procedure Poser_Bombes(Board : in out T_Board) is
      Li,Co : Integer := 0;
   begin
      for I in 1..(Argument_Count-2)/2 loop        
         Li := Integer'Value(Argument(2*I+1));
         Co := Integer'Value(Argument(2*I+2));
         -- Place les bombes sur le terrain
         Board(Li, Co) := 9;
         -- Remplacer cette instruction pour mettre une valeur enti�re
         -- repr�sentant une bombe dans la case Board(Li,Co)
         Put_Line("Placer une bombe dans la case (" 
                  & Integer'Image(Li) & "," & Integer'Image(Co) & " )"
                 );
      end loop;
   end Poser_Bombes;

-------------------------------------------------------------------------
   -- Proc�dure qui place <Nb> bombes � des positions al�atoires 
   -- dans <Board>  
   procedure Poser_Bombes(Board : in out T_Board;
                          Nb    : in     Natural) is
   Gen : Generator;
   
   --Variables pour choisir aléatoirement deux positions afin de poser une bombe sur le terrain
   X : Integer := Board'First(1);
   Y : Integer := Board'First(2);
   
   nbBombesSurBoard : Integer := 1;
   
   begin
      Reset(Gen);
      
      --Boucle qui place les bombes dans le tableau
      while nbBombesSurBoard < Nb loop
      
         X := Integer(Float(Integer'Value(Argument(1))) * random(Gen)-0.5)+1;
		 Y := Integer(Float(Integer'Value(Argument(2))) * random(Gen)-0.5)+1;
		 
		 --Vérifie qu'il n'y ai pas déjà une bombe placée à l'endroit tiré
		 if Board(X, Y) = 0 then
		    --Les bombes sont noté "9"
		    Board(X, Y) := 9;
		    nbBombesSurBoard := nbBombesSurBoard + 1;
		 end if;
      end loop;
   end Poser_Bombes;
------------------------------------------------------------------------- 
   ----------------------------------------------------  
   -- Placer ici vos autres proc�dures et fonctions --
   ----------------------------------------------------
-------------------------------------------------------------------------
   -- Procédure qui place un drapeau à l'endroit indiqué par l'utilisateur
   procedure Poser_Drapeaux(Flags : in out T_Display; Li : in Integer; Co : in Integer; Nb_Drapeaux : in out Natural) is
   
   begin
      
      -- Si la case contient un drapeau le programme enlève le drapeau et incremente le nombre de drapeaux disponibles de un
      if Flags(Li, Co) then
         Flags(Li, Co) := False;
         Nb_Drapeaux := Nb_Drapeaux + 1;
      -- Vérifie que l'utilisateur dispose d'assez de drapeaux pour pouvoir en poser un et pose un drapeau
      elsif Nb_Drapeaux > 0 then
         Flags(Li, Co) := True;
         Nb_Drapeaux := Nb_Drapeaux - 1;
      end if;
      
   end Poser_Drapeaux;
   
   -- Procédure qui découvre les cases autour d'une case ne contenant aucune bombe ou affiche le nombre de bombes qu'une case touche
   procedure Decouvrir_Alentour(Visibilite : in out T_Display; Board : in out T_Board; Li : in Integer; Co : in Integer) is

      nb_Bombes_Autour : Integer := 0;

   begin

      -- Compte le nombre de cases possèdant une bombe autour et incremente un compteur
      for I in -1..1 loop
         for J in -1..1 loop
            if Li+I > 0 and Li+I < Board'Length(1)+1 and Co+J > 0 and Co+J < Board'Length(2)+1 then
               Put_Line("Ok"); Put(Li+I) ; Put(Co+J); New_Line;
               if Board(Li+I, Co+J) = 9 then
                  nb_Bombes_Autour := nb_Bombes_Autour + 1;
               end if;
            end if;
         end loop;
      end loop;
         
         -- Si le compteur est égal à 0 appel de la procédure pour découvrir les cases vides autour
         if nb_Bombes_Autour = 0 then
            Visibilite(Li, Co) := True;
      
            for I in -1..1 loop
               for J in -1..1 loop
                  if Li+I > 0 and Li+I < Board'Last(1)+1 and Co+J > 0 and Co+J < Board'Last(2)+1 then
                     if Visibilite(Li+I, Co+J) = False then
                        Decouvrir_Alentour(Visibilite, Board, Li+I, Co+J);
                     end if;
                  end if;
               end loop;
            end loop;

         -- Sinon on affiche le nombre de bombes autour
         else
            Visibilite(Li, Co) := True;
            Board(Li, Co) := nb_Bombes_Autour;
         end if;

   end Decouvrir_Alentour;

   -- Procédure qui ouvre une case et vérifie qu'il n'y ait pas de bombe
   procedure Ouvrir_Case(Visibilite : in out T_Display; Flags : in T_Display; Gagne : in out Boolean;
                         Perdu : in out Boolean; Board : in out T_Board; Li : in Integer; Co : in Integer; Nb_Bombes : in Natural) is
   
   
   Nb_Cases_Visibles : Integer := 0;
   
   begin
      -- Vérification qu'il n'y ai pas de bombes dans la case
      if Board(Li, Co) < 9 and Flags(Li, Co) = False then
         Decouvrir_Alentour(Visibilite, Board, Li, Co);         
         
         -- Vérifie le nombre de cases ouvertes pour savoir si la partie est terminée
         for i in Visibilite'Range(1) loop
            for j in Visibilite'Range(2) loop
               if Visibilite(i, j) then
			      Nb_Cases_Visibles := Nb_Cases_Visibles + 1;
			   end if;
			end loop;
         end loop;
         if Board'Length(1)*Board'Length(2)-Nb_Bombes = Nb_Cases_Visibles then
            Gagne := True;
         end if;
      elsif Board(Li, Co) = 9 then
         Perdu := True;
      end if;
         
   
   end Ouvrir_Case;

   -- Aire de jeu de dimensions pass�es en ligne de commande 
   Board : T_Board(1..Integer'Value(Argument(1)),1..Integer'Value(Argument(2)))
            := (others => (others => 0));
   -- Tableaux utilis�s pour la visualisation des cases ouvertes, ferm�es
   -- et avec drapeau         
   Visibilite, Flags : T_Display(Board'Range(1),Board'Range(2)) 
                        := (others => (others => False));
   -- Position d'une case de l'aire de jeu
   Li : Integer := Board'First(1);
   Co : Integer := Board'First(2);
   -- Nombres totaux de bombes et de drapeaux           
   Nb_Bombes, Nb_Drapeaux : Natural := 0;
   -- Variables d'�tat du jeu pour d�tecter la fin de partie
   Gagne, Perdu, Abandon : Boolean := False;
   -- Chronom�tre
   Start, Temps : Integer := 0;
   -- Choix de l'action utilisateur
   Choix : Natural := 0;

begin -- Demineur
   --------------------------------------------------------------
   -- VOUS NE DEVEZ RIEN MODIFIER POUR LE RENDU FINAL          --
   -- SAUF AUX ENDROITS INDIQU�S "� compl�ter" ou "� modifier" --
   --------------------------------------------------------------
   Put_Line("Dimension du plateau:" 
        & Integer'Image(Board'Length(1)) 
        & " x" 
        & Integer'Image(Board'Length(2))
       );
   Put("Nombre de bombes: ");
   if (Argument_Count = 2) then
      Get(Nb_Bombes);
      Poser_Bombes(Board,Nb_Bombes); -- bombes pos�es al�atoirement
   else 
      Nb_Bombes := (Argument_Count-2)/2;
      Put_Line(Integer'Image(Nb_Bombes));
      Poser_Bombes(Board); -- bombes pos�es selon arguments 
                           -- pass�s en ligne de commande
   end if;
   Nb_Drapeaux := Nb_Bombes;
   Start := 0; -- � modifier
   loop   
      Put("Action (0. Quitter / 1. Drapeau / 2. Ouvrir case): ");
      Get(Choix);
      case Choix is
         -- Abandon
         when 0 => Abandon := True;
         -- Placer ou enlever un drapeau
         when 1 => 
            Put("Position (Ligne,Colonne) = "); 
            Get(Li); 
            Get(Co);
            Poser_Drapeaux(Flags, Li, Co, Nb_Drapeaux);
         -- Ouvrir une case / perdu si elle contient une bombe
         when 2 =>  
            Put("Position (Ligne,Colonne) = ");
            Get(Li);
            Get(Co);
            Ouvrir_Case(Visibilite, Flags, Gagne, Perdu, Board, Li, Co, Nb_Bombes);
         -- Choix non-valides   
         when others => Put("Choix ind�fini!");
      end case;
      Affichage(Visibilite,Flags,Board);
      exit when (Gagne or Perdu) or Abandon;
   end loop; 
   if Abandon then
      Put_Line("Abandon!");
   elsif Gagne then
      Put_Line("Gagn�!");
      Temps := 0; -- � modifier
      Put_Line("Votre temps:" & Integer'Image(Temps));      
   elsif Perdu then
      Put_Line("Perdu!");
   end if;
   -- Affichage du plateau de jeu avec toutes les cases ouvertes
   Put_Line("Plateau de jeu ouvert");
   Visibilite := (others => (others => True));
   Flags := (others => (others => False));
   Affichage(Visibilite,Flags,Board);
end demineur;






