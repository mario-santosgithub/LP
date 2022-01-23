+:- [codigo_comum].

% -----------------------------------------------------------------------------
% Prototipos dos predicados principais
% -----------------------------------------------------------------------------

% combinacoes_soma/4
% permutacoes_soma/4
% espaco_fila/3
% espacos_fila/3
% espacos_puzzle/2
% espacos_com_posicoes_comuns/3
% permutacoes_soma_espacos/2
% permutacao_possivel_espaco/4
% permutacoes_possiveis_espaco/4
% permutacoes_possiveis_espacos/2
% numeros_comuns/2
% atribui_comuns/1
% retira_impossiveis/2
% simplifica/2
% inicializa/2
% escolhe_menos_alternativas/2
% experimenta_perm/3


% -----------------------------------------------------------------------------
%                          Estrutura para espaco
% -----------------------------------------------------------------------------

% Criador
faz_espaco(Dados, espaco(Soma, Vars)) :- % cria o espaco 
    [Soma | Vars] = Dados,
    Vars \== [],
    number(Soma),
    is_list(Vars),
    Soma >= 0.

% Seletores
obter_vars(espaco(_, Vars), Vars). % obtem as variaveis do espaco
obter_soma(espaco(Soma, _), Soma). % obtem a soma do espaco

% -----------------------------------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs)
% N inteiro, Els lista de inteiros, Soma inteiro, Combs lista
% -----------------------------------------------------------------------------

combinacoes_soma(_, [], _, []).

combinacoes_soma(N, Els, Soma, Combs) :-
    bagof(Comb, combinacao(N, Els, Comb), L_Combs),
    bagof(Comb, (member(Comb, L_Combs), soma(Comb, Res), Res =:= Soma), Combs).


% -----------------------------------------------------------------------------
% soma(Lista, Res)  < Auxiliar >
% Soma todos os elementos da Lista e guarda o resultado em Res
% -----------------------------------------------------------------------------

soma([],0).
soma([H|T], Res):-
    soma(T, Res1),
    Res is H + Res1.


% -----------------------------------------------------------------------------    
% permutacoes_soma(N, Els, Soma, Perms)
% N inteiro, Els lista de inteiros, Soma inteiro, Perms lista
% -----------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, (member(Combinacao, Combs), permutation(Combinacao, X)),
     L_Perms),
    sort(L_Perms, Perms).


% -----------------------------------------------------------------------------
% arranja_fila(Fila, H_V, L_Esp)  < Auxiliar >               
% recebe uma fila e coloca a soma como primeiro indice
% -----------------------------------------------------------------------------

arranja_fila(Fila, H_V, L_Esp) :-
    arranja_fila(Fila, H_V, L_Esp, []).

arranja_fila([], _, L_Esp, L_Esp).

% Fila = [[5, 13], _, _, [6,9], _, _, _]
arranja_fila(Fila, H_V, L_Esp, New_Esp1) :- 
    H_V == 'h',
    [H1 | T1] = Fila,
    Primeira_Lista = H1,
    [_ | T2] = Primeira_Lista, 
    append(T2, T1, Aux1),   % Aux1 = [13, _, _, [6, 9], _, _, _]
    add_vars(T1, T2, Result),
    L_Result = [Result],
    append(Result, Aux2, Aux1),
    append(New_Esp1, L_Result, New_Esp2),
    arranja_fila(Aux2, H_V, L_Esp, New_Esp2).

arranja_fila(Fila, H_V, L_Esp, New_Esp1) :-
    H_V == 'v',
    [H1 | T1] = Fila,
    Primeira_Lista = H1,
    [H2 | _] = Primeira_Lista,
    L_H2 = [H2],
    append(L_H2, T1, Aux1),
    add_vars(T1, L_H2, Result),
    L_Result = [Result],
    append(Result, Aux2, Aux1),
    append(New_Esp1, L_Result, New_Esp2),
    arranja_fila(Aux2, H_V, L_Esp, New_Esp2).


% -----------------------------------------------------------------------------
% add_vars(Fila, L_Esp, Res)         < Auxiliar >
% Adiciona variaveis a uma lista, para criar o espaco na principal
% -----------------------------------------------------------------------------

add_vars(Fila1, L_Esp, Res) :-
    add_vars(Fila1, L_Esp, Res, 0).

add_vars([], L_Esp, L_Esp, _).

add_vars([H | T], L_Esp, Res, Estado) :-
    Res = L_Esp,
    nonvar(H); Estado =:= 1,
    add_vars(T, L_Esp, Res, 1).

add_vars([H | T], L_Esp, Res, Estado) :-
    Estado =:= 0,
    var(H),
    append(L_Esp, [H], Aux),
    add_vars(T, Aux, Res, Estado).


% -----------------------------------------------------------------------------
% espaco_fila(Fila, Esp, H_V)
% Fila e uma linha ou coluna de um puzzle, H_V um atomo h ou v
% -----------------------------------------------------------------------------

espaco_fila(Fila, Esp, H_V) :-
    exclude(is_list, Fila, Res),  % para o caso de so haverem listas
    Res \== [],
    bagof(X, (member(X, Fila), X \== [0,0]), Resto),
    arranja_fila(Resto, H_V, L_Esp),
    member(Dados_Esp, L_Esp),
    faz_espaco(Dados_Esp, Esp).

espaco_fila(Fila, Esp, _) :-
    exclude(is_list, Fila, Res),
    Res == [],
    Esp = [].


% -----------------------------------------------------------------------------
% espacos_fila(H_V, Fila, Espacos)
% Fila e uma linha ou coluna de um puzzle, H_V um atomo h ou v, Espacos uma
% lista de todos os espacoes de Fila
% -----------------------------------------------------------------------------

espacos_fila(H_V, Fila, Espacos) :-
    exclude(is_list, Fila, Res),
    Res \== [],
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos), !.

espacos_fila(_, Fila, Espacos) :-
    exclude(is_list, Fila, Res),
    Res == [],
    Espacos = [].


% -----------------------------------------------------------------------------
% add_espacos(Puzzle, Inicio, H_V, Espaco) < Auxiliar >
% adiciona espacos de espaco fila a uma lista
% -----------------------------------------------------------------------------

add_espacos([], Res, _, Res).

add_espacos([H | T], Inicio, H_V, Res) :-
    espacos_fila(H_V, H, Espaco),
    append(Inicio, Espaco, New_Espacos),
    add_espacos(T, New_Espacos, H_V, Res).


% -----------------------------------------------------------------------------
% espacos_puzzle(Puzzle, Espacos)
% Puzzle e um puzzle, Espacos e a lista de espacos de Puzzle
% -----------------------------------------------------------------------------

espacos_puzzle(Puzzle, Espacos) :-
    add_espacos(Puzzle, [], h, Espacos1),
    mat_transposta(Puzzle, Puzzlet),
    add_espacos(Puzzlet, [], v, Espacos2),
    append(Espacos1, Espacos2, Espacos).


% -----------------------------------------------------------------------------
% verifica_vars(Vars1, Vars2, Res)  < Auxiliar >
% Res = 'V' se existe alguma variavel em Vars1 que tambem pertenca a Vars2 ou 
%'F' em caso contrario
% -----------------------------------------------------------------------------

verifica_vars(Vars1, Vars2, Res) :-
    verifica_vars1(Vars1, Vars2, _, Res).

verifica_vars1([], _, Res, Res).

verifica_vars1([H | _], Vars2, Res, Final) :-
    Res \== 'V',
    findall(Var, (member(Var, Vars2), Var == H), Aux),
    Aux \== [],
    Res1 = 'V',
    verifica_vars1([], Vars2, Res1, Final).

verifica_vars1([H | T], Vars2, Res, Final) :-
    Aux = [],
    findall(Var, (member(Var, Vars2), Var == H), Aux),
    Aux == [],
    Res = 'F',
    verifica_vars1(T, Vars2, Res, Final).


% -----------------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos e uma lista de espacos, Esp um espaco, Esps_com uma lista de espacos
% com variaveis em comum com Esp
% -----------------------------------------------------------------------------

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(Espaco, (member(Espaco, Espacos), Espaco \== Esp), Espacos_M),
    obter_vars(Esp, Vars1),
    espacos_com_posicoes_comuns(Espacos_M, Vars1, Esps_com, []), !.

espacos_com_posicoes_comuns([], _, Esps_com, Esps_com).

espacos_com_posicoes_comuns([H | T], Vars1, Esps_com, Aux) :-   % H e um espaco
    obter_vars(H, Vars2),
    verifica_vars(Vars1, Vars2, Res),
    Res == 'V',
    append(Aux, [H], Espacos),
    espacos_com_posicoes_comuns(T, Vars1, Esps_com, Espacos).

espacos_com_posicoes_comuns([H | T], Vars1, Esps_com, Aux) :-   % H e um espaco
    obter_vars(H, Vars2),
    verifica_vars(Vars1, Vars2, Res),
    Res == 'F',
    espacos_com_posicoes_comuns(T, Vars1, Esps_com, Aux).


% -----------------------------------------------------------------------------
% permutacoes_soma_espacos(Espacos, Perms_soma) 
% Espacos e uma lista de espacos, Perms_soma as permutacoes para os espacos
% -----------------------------------------------------------------------------

permutacoes_soma_espacos(Espacos, Perms_soma) :-
    permutacoes_soma_espacos(Espacos, Perms_soma, []).

permutacoes_soma_espacos([], Perms_soma, Perms_soma).

permutacoes_soma_espacos([H | T], Perms_soma, Aux) :-
    obter_soma(H, Soma),
    obter_vars(H, Vars),
    PS = [H],
    length(Vars, N),
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
    append(PS, [Perms], Perm_Espaco),
    Perm_EspacoL = [Perm_Espaco],
    append(Aux, Perm_EspacoL, Res1),
    permutacoes_soma_espacos(T, Perms_soma, Res1).


% -----------------------------------------------------------------------------
% obter_permutacoes(Espaco, Espacos, Perms_Espaco)
% Espaco um espaco, Espacos uma lista de espacos e
% Perms_espaco as permutacoes do Espaco.
% -----------------------------------------------------------------------------

obter_permutacoes(Espaco, [H1 | _], Perms_Espaco) :-
    [ Espaco1 | PermsL] = H1,
    Espaco == Espaco1,
    [Perms | _ ] = PermsL, 
    Perms_Espaco = Perms, !.

obter_permutacoes(Espaco, [H1 | T], Perms_Espaco) :-
    [ Espaco1 | _] = H1,
    Espaco \== Espaco1,
    obter_permutacoes(Espaco, T, Perms_Espaco). 


% -----------------------------------------------------------------------------
% testa_elemento(El, Perms, Res)  < Auxiliar >
% Res e um atomo 'V' ou 'F' se o elemento estiver e alguma lista de Perms
% ------------------------------------------------------------------------------

testa_elemento(El, Perms, Res) :-           
    testa_elemento(El, Perms, _, Res),!.

testa_elemento(_, [], Res, Res).

testa_elemento(El, [H|_], Res, Final) :-
    Res \== 'V',
    member(El, H),
    Res1 = 'V',
    testa_elemento(El, [], Res1, Final).

testa_elemento(El, [H|T], _, Final) :-
    \+ member(El, H),
    Res1 = 'F',
    testa_elemento(El, T, Res1, Final).


% -----------------------------------------------------------------------------
% testa_permutacao(Permutacao1, Espacos_comuns, Perms_soma, Res) < Auxiliar >
% Res = 'V' se Permutacao1 e uma permutacao possivel
% -----------------------------------------------------------------------------

testa_permutacao(Permutacao1, Espacos_comuns, Perms_soma, Res) :-
    testa_permutacao(Permutacao1, Espacos_comuns, Perms_soma, _, Res),!.

testa_permutacao([], [], _, Res, Res).

testa_permutacao([H1|T1], [H2|T2], Perms_soma, Res, Final) :-
    Res \== 'F',
    obter_permutacoes(H2, Perms_soma, Perms),
    testa_elemento(H1, Perms, Res1),
    testa_permutacao(T1, T2, Perms_soma, Res1, Final).

testa_permutacao([_|T1], [_|T2], Perms_soma, Res, Final) :-
    Res == 'F',
    testa_permutacao(T1, T2, Perms_soma, Res, Final).


% -----------------------------------------------------------------------------
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm e uma permutacao, Esp um espaco, Espacos uma lista de espacos e
% Perms_soma uma lista de listas, tal que Perm seja possivel para o espaco Esp
% -----------------------------------------------------------------------------

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacao_possivel_espaco1(PermL, Esp, Espacos, Perms_soma),
    member(Perm, PermL).

permutacao_possivel_espaco1(Perm1, Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Espacos_com),
    obter_permutacoes(Esp, Perms_soma, Perm_Esp),
    permutacao_possivel_espaco1(Perm1, Perm_Esp, Espacos_com, Perms_soma, []),
     !.

permutacao_possivel_espaco1(Res, [], _, _, Res).

permutacao_possivel_espaco1(Perm, [H | T], Espacos_com, Perms_soma, Res) :-
    testa_permutacao(H, Espacos_com, Perms_soma, V_F),
    V_F == 'V',
    append(Res, [H], Res1),
    permutacao_possivel_espaco1(Perm, T, Espacos_com, Perms_soma, Res1). 

permutacao_possivel_espaco1(Perm, [H | T], Espacos_com, Perms_soma, Res) :-
    testa_permutacao(H, Espacos_com, Perms_soma, V_F),
    V_F == 'F',
    permutacao_possivel_espaco1(Perm, T, Espacos_com, Perms_soma, Res).


% -----------------------------------------------------------------------------
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Espacos e uma lista de espacos, Perms_soma, as permutacoes de cada espaco
% Esp um espaco especifico, e Perms_poss as permutacoes possiveis para Esp
% -----------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma),
     L_Perms),
    bagof(X, member(X, L_Perms), PermsL),
    obter_vars(Esp, Vars),
    append([Vars], [PermsL], Perms_poss).


% -----------------------------------------------------------------------------
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos e uma lista de espacos, Perms_poss_esps uma lista de permutacoes
% possiveis para cada espaco 
% -----------------------------------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espacos(Espacos, Espacos, Perms_soma, Perms_poss_esps,
         []).

permutacoes_possiveis_espacos([], _, _, Aux, Aux).

permutacoes_possiveis_espacos([H | T], Espacos, Perms_soma, Perms_poss_esps,
     Aux) :-
    permutacoes_possiveis_espaco(Espacos, Perms_soma, H, Perms_poss),
    append(Aux, [Perms_poss], Res),
    permutacoes_possiveis_espacos(T, Espacos, Perms_soma, Perms_poss_esps,
         Res).


% -----------------------------------------------------------------------------
% testa_elemento2(El, Pos, Perms, Res) < Auxiliar >
% Avalia se um numero pertence a todas as listas numa certa posicao
% -----------------------------------------------------------------------------

testa_elemento2(El, Pos, Perms, Res) :-           
    testa_elemento2(El, Pos, Perms, Res, _), !.  % Perms = [[7,5,1,3],[7,4,2,3]]

testa_elemento2(_, _, [], Res, Res).

testa_elemento2(El, Pos, [H | T], Res, V_F) :-
    V_F \== 'F',
    member(El, H),
    nth1(Pos, H, El1),
    El == El1,
    Res1 = 'V',
    testa_elemento2(El, Pos, T, Res, Res1).

testa_elemento2(El, Pos, [_| T], Res, _) :-
    Res1 = 'F',
    testa_elemento2(El, Pos, T, Res, Res1).


% -----------------------------------------------------------------------------
% numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms e uma lista de permutacoes, Numeros_comuns uma lista com conjuntos
% formada por (posicao, numero), que representa a posicao e o numero que e 
% comum a todas as permutacoes da lista Lst_Perms
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
% Funcao que cria a conjunto de numeros (Posicao, Numero)
% -----------------------------------------------------------------------------

faz_conjunto(Pos, El, (Pos, El)) :-
    El > 0,
    Pos > 0.

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    length(Lst_Perms, 1),
    [H | _] = Lst_Perms,
    numeros_comuns_unico(H, 1, Numeros_comuns, []), !.

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    [PrimeiraL| Res] = Lst_Perms,
    numeros_comuns(PrimeiraL, 1, Res, Numeros_comuns, []), !.

numeros_comuns_unico([], _, Aux, Aux).

numeros_comuns_unico([H|T], Pos, Numeros_comuns, Aux) :-
    nonvar(H),
    faz_conjunto(Pos, H, Conjunto),
    append(Aux, [Conjunto], Aux1),
    Pos1 is Pos + 1,  
    numeros_comuns_unico(T, Pos1, Numeros_comuns, Aux1).

numeros_comuns_unico([H|T], Pos, Numeros_comuns, Aux) :-
    var(H),
    Pos1 is Pos + 1,  
    numeros_comuns_unico(T, Pos1, Numeros_comuns, Aux).

numeros_comuns([], _, _, Aux, Aux).

numeros_comuns([H | T], N, Res, Numeros_comuns, Aux) :-
    testa_elemento2(H, N, Res, V_F),
    V_F == 'V',
    faz_conjunto(N, H, Conjunto),
    append(Aux, [Conjunto], Aux1),
    N_mais_um is N+1,
    numeros_comuns(T, N_mais_um, Res, Numeros_comuns, Aux1).

numeros_comuns([H | T], N, Res, Numeros_comuns, Aux) :-
    testa_elemento2(H, N, Res, V_F),
    V_F == 'F',
    N_mais_um is N+1,
    numeros_comuns(T, N_mais_um, Res, Numeros_comuns, Aux).


% -----------------------------------------------------------------------------
% adiciona_comuns(Vars, Numeros_comuns, Novas_vars)   < Auxiliar >
% Vars e uma lista de variaveis, Numeros_comuns uma lista com os numeros comuns 
% e Novas_vars, uma lista com os numeros atribuidos a sua posicao correta
% -----------------------------------------------------------------------------

adiciona_comuns(Vars, Numeros_comuns, Novas_vars) :-
    adiciona_comuns(Vars, Numeros_comuns, Novas_vars, _).

adiciona_comuns(_, [], Vars, Vars).

adiciona_comuns(Vars, [H | T], Novas_vars, _) :- % (_, _, _, _)  (1,7), (4,3)
    faz_conjunto(Pos, El, H),
    nth1(Pos, Vars, X),
    X = El,
    adiciona_comuns(Vars, T, Novas_vars, Vars).

% -----------------------------------------------------------------------------
% atribui_comuns(Perms_possiveis)
% Se todas as permutacoes de Perms_possiveis tiverem um numero que seja comum
% a todas as elas na mesma posicao, esse numero e substituido nas variaveis
% do espaco
% -----------------------------------------------------------------------------

atribui_comuns(Perms_possiveis) :-
    atribui_comuns(Perms_possiveis, Res, []),
    findall(X, member(X, Res), Perms_possiveis), !.

atribui_comuns([], Res, Res). 

atribui_comuns([H | T], Res, Aux) :- % H = [[_206,_212],[[1,2],[2,1]]]
    nth1(2, H, Perms),                     
    numeros_comuns(Perms, Numeros_comuns), 
    Numeros_comuns \== [],
    nth1(1, H, Vars),
    adiciona_comuns(Vars, Numeros_comuns, Novas_vars),
    append([Novas_vars], [Perms], Nova_Perm),
    append(Aux, [Nova_Perm], Aux1),
    atribui_comuns(T, Res, Aux1).

atribui_comuns([H | T], Res, Aux) :-
    nth1(2, H, Perms),
    numeros_comuns(Perms, Numeros_comuns),
    Numeros_comuns == [],
    append(Aux, [H], Aux1),
    atribui_comuns(T, Res, Aux1).


% -----------------------------------------------------------------------------
% elimina_perms(Numeros_comuns, Perms, Res) < Auxiliar >
% elimina as perms que nao respeitam os conjuntos de Numeros_comuns
% -----------------------------------------------------------------------------

elimina_perms(Numeros_comuns, Perms, Res) :-
    elimina_perms(Numeros_comuns, Perms, Res, Perms), !.

elimina_perms([], _, Aux, Aux).

elimina_perms([H|T], _, Res, Aux) :-
    faz_conjunto(Pos, El, H),
    findall(Perm, (member(Perm, Aux), nth1(Pos, Perm, El)), Aux1),
    elimina_perms(T, _, Res, Aux1).


% -----------------------------------------------------------------------------
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista com permutacoes possiveis, Novas_Perms_Possiveis
% uma lista com as permutacoes depois de serem tiradas as que nao respeitam
% valores que ja estejam definidos nas variaveis
% -----------------------------------------------------------------------------

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis, []), !.

retira_impossiveis([], Aux, Aux).

retira_impossiveis([H | T], Novas_Perms_Possiveis, Aux) :- 
    nth1(1, H, Vars),
    nth1(2, H, Perms),
    numeros_comuns([Vars], Numeros_comuns),
    Numeros_comuns \== [],
    elimina_perms(Numeros_comuns, Perms, Res),
    append([Vars], [Res], Nova_Perm),
    append(Aux, [Nova_Perm], Aux1),
    retira_impossiveis(T, Novas_Perms_Possiveis, Aux1).

retira_impossiveis([H | T], Novas_Perms_Possiveis, Aux) :- 
    nth1(1, H, Vars),
    numeros_comuns([Vars], Numeros_comuns),
    Numeros_comuns == [],
    append(Aux, [H], Aux1),
    retira_impossiveis(T, Novas_Perms_Possiveis, Aux1).


% -----------------------------------------------------------------------------
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista com permutacoes possiveis, Novas_Perms_Possiveis
% e uma lista com as permutacoes apos serem chamadas as funcoes atribui_comuns
% e retira_impossiveis
% -----------------------------------------------------------------------------

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis), 
    retira_impossiveis(Perms_Possiveis, Perms_Possiveis1),
    atribui_comuns(Perms_Possiveis1),
    retira_impossiveis(Perms_Possiveis1, Novas_Perms_Possiveis).


% -----------------------------------------------------------------------------
% inicializa(Puzzle, Perms_Possiveis)
% Puzzle e um puzzle, e as Perms_Possiveis e uma lista com as permutacoes
% possiveis para os espacos desse puzzle
% -----------------------------------------------------------------------------

inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis1),
    simplifica(Perms_Possiveis1, Perms_Possiveis).


% -----------------------------------------------------------------------------
% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis e uma lista de permutacoes possiveis, e Escolha uma
% permutacao dessa lista, para ser experimentada no proximo predicado 
% -----------------------------------------------------------------------------

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :- 
    bagof(Espaco, (member(Espaco, Perms_Possiveis), nth1(2, Espaco, Perms), length(Perms, 2)), EscolhaL),
    nth1(1, EscolhaL, Escolha), !.


% -----------------------------------------------------------------------------
% experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis)
% Escolha e uma permutacao escolhida em escolhe_menos_alternativas, 
% Perms_Possiveis uma lista de permutacoes, e Novas_Perms_Possiveis uma lista,
% em que a escolha substitui a permutacao que tinha as suas variaveis
% -----------------------------------------------------------------------------

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, L_Perms),
    member(Perm, L_Perms),
    Esp = Perm,
    append([Esp], [[Perm]], Nova_Perm),
    experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis, Nova_Perm, []), !.

experimenta_perm(_, [], Aux, _, Aux).

experimenta_perm(Escolha, [H | T], Novas_Perms_Possiveis, Nova_Perm, Aux) :-
    H == Escolha,
    append(Aux, [Nova_Perm], Aux1),
    experimenta_perm(Escolha, T, Novas_Perms_Possiveis, Nova_Perm, Aux1).

experimenta_perm(Escolha, [H | T], Novas_Perms_Possiveis, Nova_Perm, Aux) :-
    append(Aux, [H], Aux1),
    experimenta_perm(Escolha, T, Novas_Perms_Possiveis, Nova_Perm, Aux1).
