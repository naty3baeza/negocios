-module(negociacion_mef).
-behaviour(gen_fsm).

%% API publica
-export([start/1, start_link/1, negociacion/2, acepta_negociacion/1,
        oferta/2, rechaza_oferta/2, lista/1, cancela/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4,
        % estados propios
        ocupada/2, ocupada/3, ocupada_espera/2, ocupada_espera/3, negociar/2,
        negociar/3, espera/2, lista/2, lista/3]).

-record(estado, {nombre="",
                 otra,
                 itemspropios=[],
                 otrositems=[],
                 monitor,
                 desde}).

%%% API PUBLICA
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% pide iniciar sesión. Retorna cuando/si la otra parte acepta participar
negociacion(MiPid, OtroPid) ->
  gen_fsm:sync_send_event(MiPid, {negociar, OtroPid}, 30000).

%% Acepta la oferta comercial de la otra jugadora
acepta_negociacion(MiPid) ->
  gen_fsm:sync_send_event(MiPid, acepta_negociar).

%% Envía un item a la tabla de negociaciones hechas
oferta(MiPid, Item) ->
  gen_fsm:send_event(MiPid, {oferta, Item}).

%% Cancela la oferta realizada
rechaza_oferta(MiPid, Item) ->
    gen_fsm:send_event(MiPid, {rechaza_oferta, Item}).

%% Notifica que estas lista para un trato. Pides a la otra
%% jugadora que también se declare lista, el trato esta hecho
lista(MiPid) ->
    gen_fsm:sync_send_event(MiPid, lista, infinity).

%% Cancelas la transaccion
cancela(MiPid) ->
      gen_fsm:sync_send_all_state_event(MiPid, cancela).

%% pide a la otra MEF iniciar la negociacion
%todas las llamadas son asincronas para evitar deadlocks
pide_negociar(OtroPid, MiPid) ->
    gen_fsm:send_event(OtroPid, {pide_negociar, MiPid}).

%% Reenvia el mensaje del cliente aceptando la negociacion
acepta_negociar(OtroPid, MiPid) ->
    gen_fsm:send_event(OtroPid, {acepta_negociar, MiPid}).

%% reenvia una oferta al cliente
hace_oferta(OtroPid, Item) ->
    gen_fsm:send_event(OtroPid, {hace_oferta, Item}).

%% reenvia una cancelacion de oferta a cliente
deshace_oferta(OtroPid, Item) ->
    gen_fsm:send_event(OtroPid, {deshace_oferta, Item}).

%% pregunta a la otra jugadora si esta lista para iniciar la negociacion
estas_lista(OtroPid) ->
    gen_fsm:send_event(OtroPid, estas_lista).

%% responde que de su lado no esta lista para negociar
%% esto no es en estado 'espera'
no_aun(OtroPid) ->
    gen_fsm:send_event(OtroPid, no_aun).

%% le dice a la otra MEF que la jugadora esta esperando
%% por el estado 'lista'. El estado deberia pasar a 'lista'
estoy_lista(OtroPid) ->
    gen_fsm:send_event(OtroPid, 'lista!').

%% Se da cuenta que la MEF esta en estado lista.
ack_trans(OtroPid) ->
    gen_fsm:send_event(OtroPid, ack).

%% pregunta si esta lista para hacer commit
pide_commit(OtroPid) ->
    gen_fsm:sync_send_event(OtroPid, pide_commit).

%% comienza el commit sincrono
hace_commit(OtroPid) ->
    gen_fsm:sync_send_event(OtroPid, hace_commit).

%% Hace que la otra MEF se entere que tu MEF cancelo el trato.
notifica_cancelacion(OtroPid) ->
    gen_fsm:send_all_state_event(OtroPid, cancela).

%% API del gen_fsm
init(Nombre) ->
    {ok, ocupada, #estado{nombre=Nombre}}.

%% el estado 'ocupada' es anterior a cualquier negociacion.
%% La otra jugadora pide por una negociacion. Basicamente
%% solo esperamos porque nuestra propia usuaria acepte negociar
%% y que almacene el OtroPid para futuros usos
ocupada({pide_negociar, OtroPid}, E=#estado{}) ->
  Ref = monitor(process, OtroPid),
  notifica(E, "~p pregunta si quiere comenzar a negociar", [OtroPid]),
  {next_state, ocupada_espera, E#estado{otra=OtroPid, monitor=Ref}};
  ocupada(Evento, Dato) ->
    inesperado(Evento, ocupada),
    {next_state, ocupada, Dato}.

%% llamada de negociacion entrante del usuario. Reenvia al otro lado
%% lo reenvia y almacena el Pid de la otra jugadora.
ocupada({negociar, OtroPid}, Desde, E=#estado{}) ->
  pide_negociar(OtroPid, self()),
  notifica(E, "pide a usuario ~p por un negocio", [OtroPid]),
  Ref = monitor(process, OtroPid),
  {next_state, ocupada_espera, E#estado{otra=OtroPid, monitor=Ref, desde=Desde}};
ocupada(Evento, _Desde, Dato) ->
  inesperado(Evento, ocupada),
  {next_state, ocupada, Dato}.


ocupada_espera({pide_negociar, OtroPid}, E=#estado{otra=OtroPid}) ->
  io:format(" ~p esta en ocupada-espera, con pide negociar con ~p~n",[self(), OtroPid]),
  gen_fsm:reply(E#estado.desde, ok),
  notifica(E, "comenzando negociacion", []),
  {next_state, negociar, E};
  %% La otra jugadora ha aceptado la oferta, cambia a estado negociar
ocupada_espera({acepta_negociar, OtroPid}, E=#estado{otra=OtroPid}) ->
    io:format(" ~p esta en ocupada-espera, con pide negociar con ~p~n",[self(), OtroPid]),
    gen_fsm:reply(E#estado.desde, ok),
    notifica(E, "comienza negociacion", []),
    {next_state, negociar, E};
ocupada_espera(Evento, Dato) ->
  inesperado(Evento, ocupada_espera),
  {next_state, ocupada_espera, Dato}.


ocupada_espera(acepta_negociar, _Desde, E=#estado{otra=OtroPid}) ->
  acepta_negociar(OtroPid, self()),
  notifica(E, "aceptando negociacion", []),
  {reply, ok, negociar, E};
ocupada_espera(Evento, _Desde, Dato) ->
  inesperado(Evento, ocupada_espera),
  {next_state, ocupada_espera, Dato}.


negociar({oferta, Item}, E=#estado{itemspropios=ItemsPropios}) ->
  hace_oferta(E#estado.otra, Item),
  notifica(E, "oferta  ~p", [Item]),
  {next_state, negociar, E#estado{itemspropios=agrega(Item, ItemsPropios)}};
%% El propio lado retractando un item de oferta
negociar({rechaza_oferta, Item}, E=#estado{itemspropios=ItemsPropios}) ->
  deshace_oferta(E#estado.otra, Item),
  notifica(E, "cancelando oferta en ~p", [Item]),
  {next_state, negociar, E#estado{itemspropios=quita(Item, ItemsPropios)}};
%% el otro lado ofreciendo items
negociar({hace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  notifica(E, "la otra jugadora ofreciendo ~p", [Item]),
  {next_state, negociar, E#estado{otrositems=agrega(Item, OtrosItems)}};
%% el otro lado rechaza un item de oferta
negociar({deshace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  notifica(E, "La otra jugadora cancela oferta en ~p", [Item]),
  {next_state, negociar, E#estado{otrositems=quita(Item, OtrosItems)}};


negociar(estas_lista, E=#estado{otra=OtroPid}) ->
  io:format("La otra jugadora esta lista para la negociacion~n"),
  notifica(E,
          "La otra jugadora esta lista para transferir bienes:~n"
          "Vos tenes ~p, La otra jugadora tiene ~p",
          [E#estado.otrositems, E#estado.itemspropios]),
  no_aun(OtroPid),
  {next_state, negociar, E};
negociar(Evento, Dato) ->
  inesperado(Evento, negociar),
  {next_state, negociar, Dato}.


negociar(lista, Desde, E = #estado{otra=OtroPid}) ->
  estas_lista(OtroPid),
  notifica(E, "preguntando si esta lista, esperando", []),
  {next_state, espera, E#estado{desde=Desde}};
negociar(Evento, _Desde, E) ->
  inesperado(Evento, negociar),
  {next_state, negociar, E}.


espera({hace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  gen_fsm:reply(E#estado.desde, oferta_modificada),
  notifica(E, "la otra jugadora ofrece ~p", [Item]),
  {next_state, negociar, E#estado{otrositems=agrega(Item, OtrosItems)}};

espera({deshace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  gen_fsm:reply(E#estado.desde, oferta_modificada),
  notifica(E, "La otra jugadora cancela oferta de ~p", [Item]),
  {next_state, negociar, E#estado{otrositems=quita(Item, OtrosItems)}};

espera(estas_lista, E=#estado{}) ->
  estoy_lista(E#estado.otra),
  notifica(E, "Me preguntaron si estoy lista y lo estoy. Esperando por la misma respuesta.", []),
  {next_state, espera, E};

espera(no_aun, E = #estado{}) ->
  notifica(E, "La otra jugadora no esta lista aun", []),
  {next_state, espera, E};

espera('lista!', E=#estado{}) ->
  estoy_lista(E#estado.otra),
  ack_trans(E#estado.otra),
  gen_fsm:reply(E#estado.desde, ok),
  notifica(E, "La otra jugadora esta lista. Pasando al estado lista", []),
  {next_state, lista, E};

espera(Evento, Dato) ->
  inesperado(Evento, espera),
  {next_state, espera, Dato}.

lista(ack, E=#estado{}) ->
  case prioridad(self(), E#estado.otra) of
    true ->
      try
        notifica(E, "preguntando por commit", []),
        lista_commit = pide_commit(E#estado.otra),
        notifica(E, "indicando commit", []),
        ok = hace_commit(E#estado.otra),
        notifica(E, "commiteando...", []),
        commit(E),
        {stop, normal, E}
      catch Class:Reason ->
        %% aborta! sea por falla de lista_commit o hace_commit
        notifica(E, "commit fallo", []),
        {stop, {Class, Reason}, E}
      end;
    false ->
      {next_state, lista, E}
    end;
lista(Evento, Dato) ->
  inesperado(Evento, lista),
  {next_state, lista, Dato}.

lista(pide_commit, _Desde, E) ->
  notifica(E, "responde al pedido de commit", []),
  {reply, lista_commit, lista, E};
lista(hace_commit, _Desde, E) ->
  notifica(E, "committeando...", []),
  commit(E),
  {stop, normal, ok, E};
lista(Evento, _Desde, Dato) ->
  inesperado(Evento, lista),
  {next_state, lista, Dato}.

%% La otra jugadora envia el evento de cancelacion
%% Hay que detenerse no importa lo que se estaba haciendo y terminar.
handle_event(cancela, _NombreEstado, E=#estado{}) ->
  notifica(E, "evento de cancelacion recibido", []),
  {stop, otra_cancelo, E};
handle_event(Evento, NombreEstado, Dato) ->
  inesperado(Evento, NombreEstado),
  {next_state, NombreEstado, Dato}.

%% El evento de cancelacion viene de la otra jugadora. Debemos advertir a la otra
%% jugadora que vamos a salir.
handle_sync_event(cancela, _Desde, _NombreEstado, E = #estado{}) ->
  notifica_cancelacion(E#estado.otra),
  notifica(E, "cancelando negociacion, envio evento de cancelacion", []),
  {stop, cancelo, ok, E};
%% No responder a eventos inesperados.
handle_sync_event(Evento, _Desde, NombreEstado, Dato) ->
  inesperado(Evento, NombreEstado),
  {next_state, NombreEstado, Dato}.

handle_info({'DOWN', Ref, process, Pid, Motivo}, _, E=#estado{otra=Pid, monitor=Ref}) ->
  notifica(E, "La otra jugadora murio", []),
  {stop, {other_down, Motivo}, E};
handle_info(Info, NombreEstado, Dato) ->
  inesperado(Info, NombreEstado),
  {next_state, NombreEstado, Dato}.

code_change(_OldVsn, NombreEstado, Dato, _Extra) ->
  {ok, NombreEstado, Dato}.

%% Transaccion completada
terminate(normal, lista, E=#estado{}) ->
  notifica(E, "dejando MEF.", []);
terminate(_Motivo, _NombreEstado, _DatoEstado) ->
  ok.
%% agrega un item a una lista de items
agrega(Item, Items) ->
    [Item | Items].

%% quitar un item de una lista de items
quita(Item, Items) ->
    Items -- [Item].

%% Envia una notificacion a las jugadoras.
%% la salida va por shell, esto es suficiente para propositos de prueba
notifica(#estado{nombre=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

%% la funcion 'inesperados' permite logear mensajes inesperados
inesperado(Msg, Estado) ->
    io:format("~p se recibio un evento inesperado ~p durante el estado ~p~n",
              [self(), Msg, Estado]).

prioridad(MiPid, OtroPid) when MiPid > OtroPid -> true;
prioridad(MiPid, OtroPid) when MiPid < OtroPid -> false.

commit(E = #estado{}) ->
  io:format("Transaccion completada para ~s. "
            "Items enviados son:~n~p,~n recibidos son:~n~p.~n",
            [E#estado.nombre, E#estado.itemspropios, E#estado.otrositems]).
