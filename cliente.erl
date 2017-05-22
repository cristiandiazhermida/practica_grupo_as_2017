-module(cliente).

-export([start/1,registrar/2,logout/0,eliminar/1,enviar/2]).

-define(PID_CLIENTE, pid_cliente).

%% Inicia el cliente. Si el usuario introducido esta registrado
%% automaticamente inicia sesion, sino lo registra en el servidor
%% e inicia sesion 
start(User) ->
    erlang:set_cookie(node(),cookie),
    case(whereis(?PID_CLIENTE)) of
        undefined ->
        	Pid= spawn(?MODULE, registrar, [server_node(), User]),
            register(?PID_CLIENTE, Pid),
            iniciando_sesion;
        _PidCliente ->
            io:format("Error: ya te encuentras registrado y con la sesion iniciada~n"),
            error_usuario_ya_registrado
    end.

enviar(Mensaje, Destinatario) -> 
    case (whereis(?PID_CLIENTE)) of 
        undefined -> 
            io:format("Error: no te encuentras actualmente logueado~n"), 
            error_sesion_no_iniciada;
        PidCliente -> 
            PidCliente ! {enviar, Mensaje, Destinatario, self()},
            receive
            	{ok, Mess} -> io:format("~p~n",[Mess]), mensaje_enviado;
            	{error, Mess} -> io:format("~p~n", [Mess]),
            					error_enviando_mensaje
            end
    end.

%% Funcion loop que envia y recibe los mensajes
cliente(Server_node)->
	receive
        {eliminar, User, From} ->
            {servidor, Server_node} ! {eliminar, User, self()},
            receive
                {ok, Mess} -> From ! {ok,Mess}
            after
            	2000 -> From ! {ok, "Error: el servidor no responde"}
            end,
            cliente(Server_node);
        {enviar, Message, Dest, From} ->
            {servidor, Server_node} ! {enviar, self(), Dest, Message},
            receive
            	{recibir, destinatario_erroneo} -> 
            		From ! {error, "Error: el destinatario indicado no existe"};
            	{recibir, ok} -> 
            		From ! {ok,"Enviado correctamente"}
            after
            	2000 ->  From ! {error, "Error: el servidor no responde"}
            end,
            cliente(Server_node);
        {recibir, Msg, Origen} ->
            io:format("~p dice:~n  ~p~n", [Origen, Msg]),
            cliente(Server_node)
    end.

eliminar(User) ->
    case whereis(?PID_CLIENTE) of
        undefined ->
            io:format("No te encuentras actualmente logueado~n");
        PidCliente -> 
            PidCliente ! {eliminar, User, self()},
            receive
                {ok, ok} -> io:format("Se ha eliminado el usuario satisfactoriamente~n"),
                    exit(PidCliente, ok),
                    usuario_eliminado;
                {ok, error} -> io:format("Error: no se ha eliminado el usuario~n"),
                        error_usuario_no_eliminado
            after
            	2000 -> io:format("Error: servidor caido~n"),
            			error_servidor_caido
            end           
    end.

logout()->
    case whereis(?PID_CLIENTE) of
        undefined ->
            io:format("Error: no te encuentras actualmente logueado~n"),
            error_sesion_no_iniciada;
        PidCliente -> 
            exit(PidCliente, ok),
            io:format("Se ha finalizado sesion correctamente~n"),
            sesion_finalizada
    end.

%% Funciones internas

server_node() ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom("servidor@" ++ HostName).

registrar(Server_node, User)->
    {servidor, Server_node} ! {registrar,User,self()},
    receive
        {ok, usuario_ya_registrado} ->
            io:format("Se ha iniciado sesion~n"),
            cliente(Server_node);
        ok ->
            io:format("Te has registrado correctamente, sesion iniciada~n"),
            cliente(Server_node)
    after
    	2000 ->
    		io:format("Error: Sin respuesta del servidor~n"),
    		exit(whereis(?PID_CLIENTE),ok)
    end.