%geracao tabuleiro inicial (a resolver)

%criar relacao tamanho -> numero de pecas 
%gerar uma posicao do tabuleiro aleatoria
%escolher uma das direcoes, calcular distancia à margem e definir essa posicao com esse tamanho e como posicao ocupada
%fazer o mesmo para o resto das pecas

%usar predicado length para gerar lista de variaveis n/inicializadas ?

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(between)).
:- use_module(library(random)).

%1º passo
%-> resolver

main(T,Tamanho):-
        %generate_tab(T,Tamanho,Tamanho),
        %problem_generator(T,Tamanho),
        write('Tabuleiro problema:'),nl,
        print_tab(T,Tamanho),           
        hamle_solver(T,Tamanho,TSol),
        write('Tabuleiro solução:'),nl,
        print_tab(TSol,Tamanho).

%%talvez imprimir isto entre trabuleiro problema e tabuleiro solução          
%%FAZER DISPLAY DAS DUAS LISTAS : PecasPOsINicial e PecasPOsFinal a fim de evidenciar "movimentos" realizados pelas pecas

%%%chamar isto dentro do hamle solver
convert_to_tab_sol(PosList,PiecesList,BoardSize,TSol):-
        generate_tab(TSol,BoardSize,BoardSize),
        fill_vars_tab(PosList,PiecesList,TSol,BoardSize).       

fill_vars_tab([],[],_,_).
fill_vars_tab([[X,Y]|T],[PH|PT],TSol,BoardSize):-
        fill_vars_tab(T,PT,TSol,BoardSize),
        nth1(Y,TSol,Linha),
        nth1(X,Linha, PH).      %atribui valor da peça

convert_index_to_pos([],_,[]).
convert_index_to_pos([H|T], BoardSize, List):-
        convert_index_to_pos(T,BoardSize,NewList),
        index_to_coo(H,BoardSize,Coo),
        append([Coo],NewList,List).
       
index_to_coo(I,BoardSize,[X,Y]):-
        Y is ceiling(I/BoardSize),
        Temp is mod(I,BoardSize),
        Temp is 0,
        X is BoardSize.         %converte indice em coo's X,Y  

index_to_coo(I,BoardSize,[X,Y]):-
        Y is ceiling(I/BoardSize),
        X is mod(I,BoardSize).                %converte indice em coo's X,Y
               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%heuristica de geracao de problema dinamico%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problem_generator(TabProb,Size):-
        Upper is 2*Size +1,       
        random(Size,Upper,NumPieces),!,  %num de pecas do tabuleiro problema  
        write('numPieces:'),
        write(NumPieces),nl,                                   
        fill_tab_random(TabProb,Size,NumPieces,0),!,
        fill_blanks(TabProb),
        !.
        
fill_tab_random(_,_,Num,Num).
fill_tab_random(Tab,Size,NumPieces,Val):-
        random(1,Size,Y),
        random(1,Size,X),
        nth1(Y,Tab,Line),
        nth1(X,Line,Value),
        (var(Value), %verifica se nao tem já valor atribuido
        get_upper_bound(X,Size,Upper),
        random(1,Upper,Value),  %atribui valor a peça
        NewVal is Val +1 ,        
        fill_tab_random(Tab,Size,NumPieces,NewVal));
        fill_tab_random(Tab,Size,NumPieces,Val).


get_upper_bound(X,Size,Upper):-
        X >= Size/2,
        Upper is X-1.
        
get_upper_bound(X,Size,Upper):-
        X < Size/2,
        Upper is Size-X.

fill_blanks([]).
fill_blanks([H|T]):-
        fill_blanks(T),
        fill_blanks_aux(H).

fill_blanks_aux([]).
fill_blanks_aux([H|T]):-        
        (var(H),
        H is 0, 
        fill_blanks_aux(T));
        fill_blanks_aux(T).              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hamle_solver(TabIni, Tamanho, Tsol):-
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
        labeling([],Pieces),!, 
        convert_index_to_pos(Pieces,Tamanho,PosSolList),        
        print_movimentos2(PosList, PosSolList,1), 
        convert_to_tab_sol(PosSolList,Plist,Tamanho,Tsol).

print_movimentos(PosList, PosSolList):-
        write('Posicao inicial das peças (por ordem crescente de índices): '), nl,
        print_line(PosList),
        write('Posicao final das peças (por ordem crescente de índices): '), nl,
        print_line(PosSolList).

print_movimentos2([],[],_).
print_movimentos2([H1|T1],[H2|T2], I):-             
        write('Peça #'),write(I),write('  '),write(H1), 
        write(' moveu-se para: '), write(H2),nl,
        NewI is I+1,
        print_movimentos2(T1,T2,NewI).
        

%criar no final forma de ver qual peca se moveu para tal sitio para dnao haver confusoes de valores de pecas
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
        NextC is Counter - 1,
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
        print_line(H),        
        nl,
        print_tab(T,Size).

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

mostra(0):-write(.).
mostra(H):-var(H), write('.').
mostra(H):-write(H).
