-module(test_negociacion).
-compile(export_all).

%% prueba de deadlocks en los estados lista
%% -- prueba de los mensajes por la condicion de carrera en el estado lista
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    {ok, Pid} = negociacion_mef:start_link("Andrea"),
    Parent ! Pid,
    io:format("Spawned Andrea: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    negociacion_mef:acepta_negociacion(Pid),
    timer:sleep(400),
    io:format("~p~n",[negociacion_mef:lista(Pid)]),
    timer:sleep(1000),
    negociacion_mef:oferta(Pid, "caballo"),
    negociacion_mef:oferta(Pid, "vaca"),
    timer:sleep(1000),
    io:format("'a' sincronizando~n"),
    sync2(),
    negociacion_mef:lista(Pid),
    timer:sleep(200),
    negociacion_mef:lista(Pid),
    timer:sleep(1000).

b(PidA, PidCliA) ->
    {ok, Pid} = negociacion_mef:start_link("Marian"),
    io:format("Spawned Marian: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    negociacion_mef:negociacion(Pid, PidA),
    negociacion_mef:oferta(Pid, "botas"),
    timer:sleep(200),
    negociacion_mef:rechaza_oferta(Pid, "botas"),
    timer:sleep(500),
    negociacion_mef:oferta(Pid, "cinto"),
    timer:sleep(1000),
    io:format("'b' sincronizando~n"),
    sync1(PidCliA),
    negociacion_mef:oferta(Pid, "caballo"), %% condicion de carrera!
    negociacion_mef:lista(Pid),
    timer:sleep(200),
    timer:sleep(1000).

%% fuerza una condicion de carrera en la negociacion
main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> c(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> d(S, PidC, PidCliC) end),
    receive PidD -> PidD end,
    PidCliC ! PidD.

c(Parent) ->
    {ok, Pid} = negociacion_mef:start_link("Pedro"),
    Parent ! Pid,
    receive PidD -> PidD end,
    io:format("Spawned Pedro: ~p~n", [Pid]),
    %sys:trace(Pid, true),
    sync2(),
    negociacion_mef:negociacion(Pid, PidD),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    negociacion_mef:rechaza_oferta(Pid, "auto"),
    negociacion_mef:oferta(Pid, "moto"),
    timer:sleep(600),
    negociacion_mef:cancela(Pid),
    timer:sleep(1000).

d(Parent, PidC, PidCliC) ->
    {ok, Pid} = negociacion_mef:start_link("Juana"),
    Parent ! Pid,
    io:format("Spawned Juana: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sync1(PidCliC),
    negociacion_mef:negociacion(Pid, PidC),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    negociacion_mef:rechaza_oferta(Pid, "auto"),
    negociacion_mef:oferta(Pid, "bicicleta"),
    timer:sleep(100),
    negociacion_mef:lista(Pid),
    timer:sleep(1000).

main_ef() ->
    S = self(),
    PidCliE = spawn(fun() -> e(S) end),
    receive PidE -> PidE end,
    spawn(fun() -> f(PidE, PidCliE) end).

e(Parent) ->
    {ok, Pid} = negociacion_mef:start_link("Andrea"),
    Parent ! Pid,
    io:format("Spawned Andrea: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    negociacion_mef:acepta_negociacion(Pid),
    timer:sleep(400),
    io:format("~p~n",[negociacion_mef:lista(Pid)]),
    timer:sleep(1000),
    negociacion_mef:oferta(Pid, "horse"),
    negociacion_mef:oferta(Pid, "sword"),
    timer:sleep(1000),
    io:format("'e' sincronizando~n"),
    sync2(),
    negociacion_mef:lista(Pid),
    timer:sleep(200),
    negociacion_mef:lista(Pid),
    timer:sleep(1000).

f(PidE, PidCliE) ->
    {ok, Pid} = negociacion_mef:start_link("Marian"),
    io:format("Spawned Marian: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    negociacion_mef:negociacion(Pid, PidE),
    negociacion_mef:oferta(Pid, "boots"),
    timer:sleep(200),
    negociacion_mef:rechaza_oferta(Pid, "boots"),
    timer:sleep(500),
    negociacion_mef:oferta(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("f sincronizando~n"),
    sync1(PidCliE),
    negociacion_mef:oferta(Pid, "horse"),
    timer:sleep(200),
    negociacion_mef:lista(Pid),
    timer:sleep(1000).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
