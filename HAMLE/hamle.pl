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

main(TabIni,Tamanho):-
        write('Tabuleiro problema:'),nl,
        print_tab(TabIni,Tamanho),             
        hamle_solver(TabIni, TabSol,Tamanho),
        write('Tabuleiro solução:'),nl,
        print_tab(TabSol,Tamanho).  

hamle_solver(TabIni, Tamanho, Pieces):-
        %lista tabuleiro
        get_pieces_pos_list(TabIni,1,PosList),
        %
        convert_to_index_list(PosList,Tamanho,IndexList),   
        %retorna lista de pecas
        get_pieces_list(TabIni,Plist),            
        %devolve lista de listas com indices das posicoes possiveis para cada peca
        get_list_of_domains(PosList,Plist,Tamanho,DomainList),
        %
        %devolve lista de listas de indices para tabling
        convert_to_index_pos_list(DomainList,Tamanho,DomainIndexList),   
        write(DomainIndexList),nl,
        %tamanho da lista
        length(IndexList,Size),             
        %
        length(Pieces,Size),        
        %
        do_tabling(Pieces,DomainIndexList),
        
        all_different(Pieces),
        %write(Pieces),
        labeling([],Pieces).

do_tabling([],[]).
do_tabling([Hpiece|Tpiece],[Hdom|Tdom]):-
        do_tabling(Tpiece,Tdom),
        table([[Hpiece]],Hdom). 
        
convert_to_index_pos_list([],_,[]).
convert_to_index_pos_list([H|T],Tamanho,DomainIndex):-
        convert_to_index_pos_list(T,Tamanho, List),
        convert_to_index_list(H,Tamanho,List1),
        append([List1],List,DomainIndex).

convert_to_index_list([],_,[]).
convert_to_index_list([[X,Y]|Ttab],Size,[[Index] | List]):-
        convert_to_index_list(Ttab,Size,List),
        Index is X + (Y-1)*Size.   
    
get_list_of_domains([],[],_,[]).        
get_list_of_domains([HPos|TPos],[Hpeca|Tpeca], Line_Size, DomainList):-  
        get_list_of_domains(TPos,Tpeca,Line_Size,NewDomainL),
        get_coos(HPos,Hpeca,Line_Size,List),
        delete(List,[],NewList),
        append([NewList],NewDomainL,DomainList).                    
        
get_coos(Pos,Peca,Line_Size,[NewPos,NewPos2,NewPos3,NewPos4]):-
         get_coo_right(Pos, Peca, Line_Size, NewPos),    
         get_coo_left(Pos, Peca, NewPos2),
         get_coo_up(Pos, Peca, Line_Size, NewPos3),  
         get_coo_down(Pos, Peca, NewPos4),!.         
                                               
get_coo_right([X,Y],Peca,Line_Size,[NewX,Y]):-
        NewX is X + Peca,
        NewX =< Line_Size.

get_coo_right(_,_,_,[]).

get_coo_left([X,Y],Peca,[NewX,Y]):-
        NewX is X - Peca,
        NewX >= 1.        

get_coo_left(_,_,[]).

get_coo_up([X,Y],Peca,Line_Size,[X,NewY]):-
        NewY is Y + Peca,
        NewY =< Line_Size.

get_coo_up(_,_,_,[]).

get_coo_down([X,Y],Peca,[X,NewY]):-
        NewY is Y - Peca,
        NewY >= 1.  
        
get_coo_down(_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%CANCRO %%%%%%%%%%%%%%%%
/*hamle_solver(TabIni, TabSol, Tamanho):-  
        %      
        % gera tab solucao com vars n/instanciadas
        generate_tab(TabSol,Tamanho,Tamanho), !,
        %
        %lista de pecas do tab
        get_pieces_list(TabIni,PList),
        %
        %lista de posicoes de pecas no tab
        get_pieces_pos_list(TabIni,1, PosList),
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
        labeling([],DirList),  !.   % nao da mais solucoes
*/                                

%2º passo 
%gerar tabuleiro 

        
        
        
        
        
%%%%%%%%%%%%%
restrict_dirs(_,_,[],[],[],_).
restrict_dirs(Tab,Tsol,[Hdir|Tdir],[Hpeca|Tpeca],[Hpos|Tpos], TabSize):-
        %restringe dirs e obtem novas coos
        get_coos_dir(Hpos,Hpeca,Hdir,NewX,NewY),     
        %               
        inside_bounds(NewX,NewY,TabSize),
        %
        % posicao tem de estar livre  
        restrict_position(Tsol,NewX,NewY,Hpeca),
        %         
        % posicoes adjacentes têm de estar livres
        adjoins_free(Tsol,TabSize,NewX,NewY),
        %        
        restrict_dirs(Tab,Tsol,Tdir,Tpeca,Tpos,TabSize).

                
adjoins_free(Tab,TabSize,X,Y):-
        foreach(between(1,4,Dir), check_adjoin_dir(Tab,TabSize,X,Y,Dir)).

                
check_adjoin_dir(Tab,TabSize,X,Y,1):-
        NewY is Y-1,
        (outside_bounds(X,NewY,TabSize);
        restrict_position(Tab,X,NewY,0),!).           

check_adjoin_dir(Tab,TabSize, X,Y,2):-
        NewY is Y+1,
        (outside_bounds(X,NewY,TabSize);
        restrict_position(Tab,X,NewY,0),!).

check_adjoin_dir(Tab,TabSize,X,Y,3):-
        NewX is X+1,
        (outside_bounds(NewX,Y,TabSize);        
        restrict_position(Tab,NewX,Y,0),!).

check_adjoin_dir(Tab,TabSize,X,Y,4):-
        NewX is X-1,
        (outside_bounds(NewX,Y,TabSize);
        restrict_position(Tab,NewX,Y,0),!).       

%%%%
restrict_position(Tab,X,Y,Val):-        
        nth1(Y, Tab, Linha),
        element(X, Linha, V),
        write(V),nl,
        write(Val),nl,!,
        V #= Val.
                
                
outside_bounds(X,Y,TabSize):-
       X < 1 ;  X > TabSize;
       Y < 1 ; Y > TabSize.        
                
inside_bounds(X,Y,TabSize):-
       X > 0 ,  X =< TabSize,
       Y > 0 , Y =< TabSize.
        
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
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%obter lista de posicoes 
get_pieces_pos_list([],_,[]).         
get_pieces_pos_list([Htab|Ttab],Y,List):- 
       NewY is Y + 1,      
       get_pieces_pos_list(Ttab,NewY,NewList),
       findall([X,Y],(nth1(X,Htab, V),V > 0),List1),
       append(List1,NewList,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
generate_tab([],_,0).
generate_tab([H|L],Size,Counter):-
        length(H,Size),
        NextC is Counter -1,
        generate_tab(L,Size,NextC).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_tab([],_).%:- print_char_n_times('_', Size).
print_tab([H|T],Size):-
        print_tab(T,Size),
        print_line(H),        
        nl.        

print_line([]).
print_line([H|T]):-
        mostra(H),
        write('|'),
        print_line(T).

print_char_n_times(_,0).        
print_char_n_times(C,N):-
        N1 is N-1,
        write(C),
        print_char_n_times(C,N1).

mostra(H):-var(H), write('0').
mostra(H):-write(H).
