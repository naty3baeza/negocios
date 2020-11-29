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
gen_fsm:sync_send_event(MiPid, lista, infinito).

%% Cancelas la transaccion
cancela(MiPid) ->
gen_fsm:sync_send_all_state_event(MiPid, cancela).

%% pide a la otra MEF iniciar la negociacion
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

%% API del gen_fsm
init(Nombre) ->
{ok, ocupada, #estado{nombre=Nombre}}.

%% Envia una notificacion a las jugadoras.
%% la salida va por shell, esto es suficiente para propositos de prueba
notifica(#estado{nombre=N}, Str, Args) ->
io:format("~s: "++Str++"~n", [N|Args]).

%% la funcion 'inesperados' permite logear mensajes inesperados
inesperado(Msg, Estado) ->
io:format("~p se recibio un evento inesperado ~p durante el estado ~p~n",
[self(), Msg, Estado]).

ocupada({pide_negociar, OtroPid}, E=#estado{}) ->
  Ref = monitor(process, OtroPid),
  notifica(E, "~p pregunta si quiere comenzar a negociar", [OtroPid]),
  {siguiente_estado, ocupada_espera, E#estado{otra=OtroPid, monitor=Ref}};
  ocupada(Evento, Dato) ->
    inesperado(Evento, ocupada),
    {siguiente_estado, ocupada, Dato}.

ocupada({negociar, OtroPid}, Desde, E=#estado{}) ->
  pide_negociar(OtroPid, self()),
  notifica(E, "pide a usuario ~p por un negocio", [OtroPid]),
  Ref = monitor(process, OtroPid),
  {siguiente_estado, ocupada_espera, E#estado{otra=OtroPid, monitor=Ref, desde=Desde}};
ocupada(Evento, _Desde, Dato) ->
  inesperado(Evento, ocupada),
  {siguiente_estado, ocupada, Dato}.

ocupada_espera({pide_negociar, OtroPid}, E=#estado{otra=OtroPid}) ->
  gen_fsm:reply(E#estado.desde, ok),
  notifica(E, "comenzando negociacion", []),
  {siguiente_estado, negociar, E};
  %% La otra jugadora ha aceptado la oferta, cambia a estado negociar
ocupada_espera({acepta_negociar, OtroPid}, E=#estado{otra=OtroPid}) ->
    gen_fsm:reply(E#estado.desde, ok),
    notifica(E, "comienza negociacion", []),
    {siguiente_estado, negociar, E};
ocupada_espera(Evento, Dato) ->
  inesperado(Evento, ocupada_espera),
  {siguiente_estado, ocupada_espera, Data}.

ocupada_espera(acepta_negociar, _Desde, E=#estado{otra=OtroPid}) ->
  acepta_negociar(OtroPid, self()),
  notifica(E, "negociacion aceptada", []),
  {reply, ok, negociar, E};
ocupada_espera(Evento, _Desde, Datos) ->
  inesperado(Evento, ocupada_espera),
  {siguiente_estado, ocupada_espera, Datos}.

%% agrega un item a una lista de items
agrega(Item, Items) ->
[Item | Items].

%% quitar un item de una lista de items
quita(Item, Items) ->
Items -- [Item].

negociar({oferta, Item}, E=#estado{itemspropios=ItemsPropios}) ->
  hace_oferta(E#estado.otra, Item),
  notifica(E, "oferta  ~p", [Item]),
  {siguiente_estado, negociar, E#estado{itemspropios=agrega(Item, ItemsPropios)}};
%% El propio lado retractando un item de oferta
negociar({rechaza_oferta, Item}, E=#estado{itemspropios=ItemsPropios}) ->
  deshace_oferta(E#estado.otra, Item),
  notifica(E, "cancelando oferta en ~p", [Item]),
  {siguiente_estado, negociar, E#estado{itemspropios=quita(Item, ItemsPropios)}};
%% el otro lado ofreciendo items
negociar({oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  notifica(E, "la otra jugadora ofreciendo ~p", [Item]),
  {siguiente_estado, negociar, E#estado{otrositems=agrega(Item, OtrosItems)}};
%% el otro lado rechaza un item de oferta
negociar({deshace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  notifica(E, "La otra jugadora cancela oferta en ~p", [Item]),
  {siguiente_estado, negociar, E#estado{otrositems=quita(Item, OtrosItems)}};

negociar(estas_lista, E=#estado{otra=OtroPid}) ->
  io:format("La otra jugadora esta lista para la negociacion~n"),
  notifica(E, "La otra jugadora esta lista para transferir bienes:~n"
  "Vos tenes ~p, La otra jugadora tiene ~p",
  [E#estado.otrositems, E#estado.itemspropios]),
  no_aun(OtroPid),
  {siguiente_estado, negociar, E};
negociar(Evento, Dato) ->
  inesperado(Evento, negociar),
  {siguiente_estado, negociar, Dato}.

espera({hace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  gen_fsm:reply(E#estado.desde, oferta_modificada),
  notifica(E, "la otra jugadora ofrece ~p", [Item]),
  {siguiente_estado, negociar, E#estado{otrositems=agrega(Item, OtrosItems)}};
espera({deshace_oferta, Item}, E=#estado{otrositems=OtrosItems}) ->
  gen_fsm:reply(E#estado.desde, oferta_modificada),
  notifica(E, "La otra jugadora cancela oferta de ~p", [Item]),
  {siguiente_estado, negociar, E#estado{otrositems=quita(Item, OtrosItems)}};

espera(estas_lista, E=#estado{}) ->
  estoy_lista(E#estado.otra),
  notifica(E, "Me preguntaron si estoy lista y lo estoy. Esperando por la misma respuesta.", []),
  {siguiente_estado, espera, E};

espera(no_aun, E = #estado{}) ->
  notifica(E, "La otra jugadora no esta lista aun", []),
  {siguiente_estado, espera, E};

espera('lista!', E=#estado{}) ->
  estoy_lista(E#estado.otra),
  ack_trans(E#estado.otra),
  gen_fsm:reply(E#estado.desde, ok),
  notifica(E, "La otra jugadora esta lista. Pasando al estado lista", []),
  {siguiente_estado, lista, E};

espera(Evento, Dato) ->
  inesperado(Evento, espera),
  {siguiente_estado, espera, Dato}.

prioridad(MiPid, OtroPid) when MiPid > OtroPid -> true;
prioridad(MiPid, OtroPid) when MiPid < OtroPid -> false.
