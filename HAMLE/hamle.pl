%geracao tabuleiro inicial (a resolver)

%criar relacao tamanho -> numero de pecas 
%gerar uma posicao do tabuleiro aleatoria
%escolher uma das direcoes, calcular distancia à margem e definir essa posicao com esse tamanho e como posicao ocupada
%fazer o mesmo para o resto das pecas

%usar predicado length para gerar lista de variaveis n/inicializadas ?

:-use_module(library(lists)).
:-use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(between)).
%1º passo
%-> resolver

%verificar células brancas interconectadas 
%começar na primeira branca, ir guardando velulas visitadas e tentar visitar todas as celulas brancas adjacente
%se no finaol o numero de celulas visitadas for !=  do numero de cels bracnas é pq nao existe

%verificar celulas pretas nao adjacentes
%-basicamente é aplicar o mesmo predicado que nas celulas brancas
%-mas verificar se o numero de celulas pretas interconectadas é 0

%recebe lista de cell_index e retorna lista de cell_index solucao

%tamanho maximo = 10
%index = linha[1,x]*10 + coluna
%==restricoes

%1) Todas as pecas têm de ser movidas
%2) Não pode haver peças adjacentes        [TODO]
%3) Celulas vazias estao interconectadas   [TODO]
%4) celulas nao podem sobrepor-se
hamle_solver(TabIni, TabSol, Tamanho):-  
        %        
        % gera tab solucao com vars n/instanciadas
        generate_tab(TabSol,Tamanho), 
        %
        %lista de pecas do tab
        get_pieces_list(TabIni,PList),
        %
        %lista de posicoes de pecas no tab
        get_pieces_pos_list(TabIni,TabIni, PosList),
        %
        %numero de pecas no tab
        length(PList, NumPecas),       
        %
        %generate list of directions for pieces with length = numPecas
        length(DirList,NumPecas),        
        %
        %[1,4]: Num of directions
        domain(DirList,1,4),
        %
        restrict_dirs(TabIni,TabSol,DirList,PList,PosList,Tamanho),
        
        
        %
        labeling([],DirList),
        do_labeling(TabSol),       
        write(TabSol).                           



%2º passo 
%gerar tabuleiro


%%%%%%%%%%%%%
restrict_dirs([],[],_).
restrict_dirs(Tab,Tsol,[Hdir|Tdir],[Hpeca|Tpeca],[Hpos|Tpos], TabSize):-
        %restringe dirs e obtem novas coos
        get_coos_dir(Hpos,Hpeca,Hdir,NewX,NewY),
        % posicao tem de estar livre  
        restrict_position(Tab,NewX,NewY,Hpeca),
        %               
        inside_bounds(NewX,NewY,TabSize), 
        % posicoes adjacentes têm de estar livres
        adjoins_free(Tsol,NewX,NewY),
        %
        restrict_dirs(Tab,Tsol,Tdir,Tpeca,Tpos,TabSize).       

                
adjoins_free(Tab,X,Y):-
        foreach(between(1,4,Dir), check_adjoin_dir(Tab,X,Y,Dir)).

                
check_adjoin_dir(Tab,X,Y,1):-
        (outside_bounds(Tab,X,Y-1);
        restrict_position(Tab,X,Y-1,0)),!.           

check_adjoin_dir(Tab,X,Y,2):-
        (outside_bounds(Tab,X,Y+1);
        restrict_position(Tab,X,Y+1,0)),!.

check_adjoin_dir(Tab,X,Y,3):-
        (outside_bounds(Tab,X+1,Y);
        restrict_position(Tab,X+1,Y,0)),!.

check_adjoin_dir(Tab,X,Y,4):-
        (outside_bounds(Tab,X-1,Y);
        restrict_position(Tab,X-1,Y,0)),!.       

%%%%
restrict_position(Tab,X,Y,Val):-
        element(X, Tab, Linha),
        element(Y, Linha, V),
        V #= Val.        
                
outside_bounds(X,Y,TabSize):-
       ( X < 1 ;  X > TabSize;
       Y < 1 ; Y > TabSize), !.        
                
inside_bounds(X,Y,TabSize):-
       ( X > 0 ;  X =< TabSize;
       Y > 0 ; Y =< TabSize), !.
        
%1-norte,2-sul,3-este,4 oeste.        
get_coos_dir([X,Y],ValPeca,Dir,NewX,NewY):-
        Dir #= 1,
        NewX is X,
        NewY is Y - ValPeca.               
                
get_coos_dir([X,Y],ValPeca,Dir,NewX,NewY):-
        Dir #= 2,
        NewX is X,
        NewY is Y + ValPeca. 
        
get_coos_dir([X,Y],ValPeca,Dir,NewX,NewY):-
        Dir #= 3,
        NewX is X + ValPeca,
        NewY is Y. 
        
get_coos_dir([X,Y],ValPeca,Dir,NewX,NewY):-
        Dir #= 4,
        NewX is X - ValPeca,
        NewY is Y.                         
       
        
%obter lista de posicoes 
get_pieces_pos_list(_,[],[]).         
get_pieces_pos_list(Tab,[Htab|Ttab],List):-
       nth1(Y,Tab,Htab),
       get_pieces_pos_list_aux(Htab,Htab,Y, List1),
       get_pieces_pos_list(Tab,Ttab,NewList),
       append(List1,NewList,List).
      
get_pieces_pos_list_aux([],_,_,[]).       
get_pieces_pos_list_aux([H|T],Linha,Y,List):-                     
        H > 0,
        !,       
        get_pieces_pos_list_aux(T,Linha,Y,List1), 
        nth1(X1,Linha,H),
        append([X1],[Y], Pos),
        append([Pos],List1,List). 

get_pieces_pos_list_aux([_|T],L,Y,List):-  
        get_pieces_pos_list_aux(T,L,Y,List). 
%%%%%%%%%%%%%%%%%%%%%%%        

fill_tab([],_).
fill_tab([H|T],Val):-
       fill_list(H,Val),
       fill_tab(T,Val). 

fill_list([],_).
fill_list([H|T],Val):-
      H is Val,
      fill_list(T,Val).  
                
matrix_get(Tab,X,Y,V):- nth1(Y,Tab,Linha),nth1(X,Linha,V).                                                                                     

%gera matriz tamanho Size x Size
generate_tab([],[]).
generate_tab([H|L],Size):-
        length(H,Size),
        generate_tab(L,Size).

%%%%%%%%%%%%%
get_pieces_list([],[]).
get_pieces_list([H|T],List):-
        get_pieces_list_aux(H,L1),
        get_pieces_list(T,L2),
        append(L1,L2,List).     
        
get_pieces_list_aux([],[]).
get_pieces_list_aux([H|T],List):-
        H > 0,
        !,
        get_pieces_list_aux(T,L1),
        append([H],L1,List).        
        
get_pieces_list_aux([_|T],L):-
        get_pieces_list_aux(T,L).
%%%%%%%%%%%%%


%retorna valor maximo de uma lista de listas
get_max_value([],Max,Max).
get_max_value([H|T],MaxActual,Max):-
        get_max_value_aux(H,MaxActual,MaxList),
        NextMax is max(MaxList,MaxActual),
        get_max_value(T,NextMax,Max).

get_max_value_aux([],Max,Max).
get_max_value_aux([H|T],MaxActual,Max):-
         NextMax is max(H,MaxActual),
         get_max_value_aux(T,NextMax,Max).

%efectua labeling a uma lista de listas
do_labeling([]).
do_labeling([H|T]):-
        labeling([],H),
        do_labeling(T).