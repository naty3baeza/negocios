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
