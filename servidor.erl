-module(servidor).

-export([start/0, stop/0]).

-export([loop/1]). %% Funciones exportadas por spawn

-define(SERVER_NAME, servidor).

%% Inicializa el nodo y recupera en una lista los usuarios
%% registrados con anterioridad
start() ->
    case(whereis(?SERVER_NAME)) of
        undefined ->
            erlang:set_cookie(node(),cookie),
            register(servidor, spawn(?MODULE, loop, [[]])),
            servidor ! recuperar,
            servidor_iniciado;
        _PidServidor ->
            io:format("Error: el servidor ya se encuentra funcionando~n"),
            error_servidor_ya_funcionando
    end.  

stop() -> 
    case(whereis(?SERVER_NAME)) of
        undefined ->
            io:format("Error: el servidor ya se encuentra parado~n"),
            error_servidor_ya_parado;
        _PidCliente ->
            servidor ! stop
    end.
    

%% Loop que se encarga de recibir peticiones de los clientes
%% y responderles correctamente
loop(ListaUsuarios)->
	receive
		{registrar, Usuario, Pid} ->
            {Mess, Lista} = registrar(Usuario,ListaUsuarios,Pid),
            Pid ! Mess,
            guardarLista(Lista),
            loop(Lista);
        recuperar ->
		  Usuarios = recuperarLista(),
          loop(Usuarios);
        {eliminar, Usuario, From} -> 
            {Message, Lista} = eliminar_cliente(Usuario, ListaUsuarios, From),
            From ! {ok, Message},
            loop(Lista);
        {enviar, Origen, Dest, Msg} -> 
            case find(Dest, ListaUsuarios) of 
                no_existe -> 
                    Origen ! {recibir, destinatario_erroneo};
                {ok, PidDest} -> 
                    Origen ! {recibir, ok},
                    {ok,Nombre_origen}= findNombre(pid_to_list(Origen), ListaUsuarios),
                    list_to_pid(PidDest) ! {recibir, Msg, Nombre_origen}
            end,
            loop(ListaUsuarios);
        stop -> 
            ok
    end.


%% Funciones internas

find(_, [])->
    no_existe;

find(Usuario, [{Usuario,Pid}|_])->
    {ok, Pid};

find(Usuario, [{_Usuario2,_}|L])->
    find(Usuario, L).

findNombre(_, [])->
    no_existe;

findNombre(Pid, [{Usuario,Pid}|_])->
    {ok, Usuario};

findNombre(Pid, [_|L])->
    findNombre(Pid, L).

registrar(Usuario, ListaUsuarios,Pid)->
    case (find(Usuario,ListaUsuarios)) of
        {ok, Pid_antiguo}->   
            case (pid_to_list(Pid)==Pid_antiguo) of
                true -> 
                    {{ok, usuario_ya_registrado}, ListaUsuarios};
                false -> 
                    {_Mess, Lista} = eliminar_cliente(Usuario,ListaUsuarios),
                    L = registroUsuario(Pid, Usuario, Lista),
                    {{ok, usuario_ya_registrado}, L}
            end;
        no_existe -> 
            L = registroUsuario(Pid,Usuario,ListaUsuarios),
            {ok, L}
    end.

%% Funcion auxiliar de registrar
registroUsuario(From, User, ListaUsuarios)->
    case lists:keymember(User, 1, ListaUsuarios) of
        true -> 
        	io:format("Error: usuario ~p [~p] ya registrado~n", [User, From]),
            From ! {stop, usuario_ya_registrado},
            ListaUsuarios;
        false ->
            io:format("Usuario ~p [~p] registrado en el servidor~n", [User, From]),
            From ! {ok},
            [{User, pid_to_list(From)} | ListaUsuarios]
    end.

eliminar_cliente(Usuario,Lista)->
    case file:delete("usuarios.log") of
        ok ->
            Usuarios = lists:keydelete(Usuario, 1, Lista),
            guardarLista(Usuarios),
            {ok, Usuarios};
        _ -> 
            {error, Lista}
    end.

eliminar_cliente(Usuario, Lista, From) ->
    case (find(Usuario,Lista)) of
        {ok, Pid}->
            case (Pid==pid_to_list(From)) of
                true -> eliminar_cliente(Usuario,Lista);                    
                false -> {error, Lista}
            end;
        no_existe -> {error,Lista}
    end.    

%% Persistencia
%% Almacenamos la lista de usuarios registrados en el fichero usuarios.log
guardarLista(L) ->
        {ok, S} = file:open("usuarios.log", write),
        lists:foreach( fun(X) -> io:format(S, "~p.~n",[X]) end, L),
        file:close(S). %% No hace falta que devuelva la lista

%% Cada vez que el servidor se inicia llama a esta función para tener
%% los usuarios activos
recuperarLista() ->
    case file:open("usuarios.log", read) of 
        {ok, Fd} -> 
            L = leer_termino(Fd),
            L;
        {error, _} -> 
            []
    end.

%% Funcion auxiliar de recuperarLista
leer_termino(Fd) -> %% Entendemos fd como descriptor de fichero
	case io:read(Fd, '') of
	   {ok, Term} -> 
            [Term] ++ leer_termino(Fd);
        _ -> []
	end.
