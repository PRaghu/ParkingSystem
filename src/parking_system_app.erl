%%%-------------------------------------------------------------------
%% @doc parking_system public API
%% @end
%%%-------------------------------------------------------------------

-module(parking_system_app).

-behaviour(application).

%% Application Callbacks
-export([start/2, stop/1]).

%% API
-export([start/0, stop/0]).

%%====================================================================
%% Application Callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
  parking_system_sup:start_link().

stop(_State) ->
  ok.

%%====================================================================
%% API
%%====================================================================

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(parking_system),
  ok.

-spec stop() -> ok.
stop() ->
  application:stop(parking_system).
