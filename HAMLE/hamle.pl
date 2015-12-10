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
        write('Predicado Printtab está errado'), 
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
        %write(DomainIndexList),nl,
        %tamanho da lista
        length(IndexList,Size),             
        %
        length(Pieces,Size),        
        %aplicacao de dominios
        do_tabling(Pieces,DomainIndexList),
        %
        restrict_adjoins(Pieces,Tamanho),
        %
        all_different(Pieces),
        %
        labeling([],Pieces),
        write(Pieces).

%criar no final forma de ver qual peca se moveu para tal sitio par anao haver confusoes de valores de pecas
%usar a posicao inicial vs posiciao final

restrict_adjoins([],_).
restrict_adjoins([H|T],Size):-
        restrict_adjoins(T,Size),
        restrict_aux(H,T,Size).

restrict_aux(_,[],_).
restrict_aux(H,[H1|T1],Size):-
        restrict_aux(H,T1,Size),
        H #\= H1 + 1,
        H #\= H1 - 1, 
        H #\= H1 + Size,
        H #\= H1 - Size.

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
       
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%obter lista de posicoes 
get_pieces_pos_list([],_,[]).         
get_pieces_pos_list([Htab|Ttab],Y,List):- 
       NewY is Y + 1,      
       get_pieces_pos_list(Ttab,NewY,NewList),
       findall([X,Y],(nth1(X,Htab, V),V > 0),List1),
       append(List1,NewList,List).



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
