%geracao tabuleiro inicial (a resolver)

%criar relacao tamanho -> numero de pecas 
%gerar uma posicao do tabuleiro aleatoria
%escolher uma das direcoes, calcular distancia à margem e definir essa posicao com esse tamanho e como posicao ocupada
%fazer o mesmo para o resto das pecas

%usar predicado length para gerar lista de variaveis n/inicializadas ?

:-use_module(library(clpfd)).
%1º passo
%-> resolver

%verificar células brancas interconectadas 
%começar na primeira branca, ir guardando velulas visitadas e tentar visitar todas as celulas brancas adjacente
%se no finaol o numero de celulas visitadas for !=  do numero de cels bracnas é pq nao existe

%verificar celulas pretas nao adjacentes
%-basicamente é aplicar o mesmo predicado que nas celulas brancas
%-mas verificar se o numero de celulas pretas interconectadas é 0

%recebe tabuleiro problema e retorna tabuleiro sol e verifica se o actual é uma solucao
%==restricoes
%1) Todas as pecas têm de ser movidas
%2) Não pode haver peças adjacentes        [TODO]
%3) Celulas vazias estao interconectadas   [TODO]
%4) celulas nao podem sobrepor-se
hamle_solver(/*Tinicial,Tsol,*/Tamanho,PiecesPosList, PosListSolv):-
        length(PiecesPosList,Size),
        % cria lista de listas com posicoes solucao n/instanciadas
        length(PosListSolv,Size),
        generate_list(PosListSolv,2),
        %
        generate_domain(PosListSolv,1,Tamanho),
        %restricoes
        %4)
        %===============>> Nao se pode fazer isto com lista de listas
        %all_different(PosListSolv),
        %1)
        verify_lists(PosListSolv, PiecesPosList),
        %
        do_labeling(PosListSolv),       
        write(PosListSolv).                           

%gera lista de listas (coordenadas)
generate_list([],_).
generate_list([H|L],Size):-
        length(H,Size),
        generate_list(L,Size).

%gera dominio para cada coo
generate_domain([],_,_).
generate_domain([H|L],Min,Max):-
        domain(H,Min,Max),
        generate_domain(L,Min,Max).

%verifica se elemento na posicao x de ambas a lista é diferente
verify_lists([],[]).                            
verify_lists([L1h|L1t],[L2h|L2t]):-
        verify_coos(L1h,L2h),
        verify_lists(L1t,L2t).

verify_coos([H1,T1],[H2,T2]):-
        H1 #\= H2,
        T1 #\= T2.

%efectua labeling a uma lista de listas
do_labeling([]).
do_labeling([H|T]):-
        labeling([],H),
        do_labeling(T).


%2º passo 
%gerar tabuleiro

