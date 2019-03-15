-module(parking_system_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT
-export([
  all/0, 
  init_per_suite/1, 
  end_per_suite/1
]).

%% Test Cases
-export([
  entry_exit/1,
  unused_callbacks/1
]).

-type config() :: proplists:proplist().

-export_type([config/0]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  [
    entry_exit,
    unused_callbacks
  ].

-spec init_per_suite(config()) -> ok.
init_per_suite(Config) ->
  _ = application:load(parking_system),
  {ok, ParkingConfig} = application:get_env(parking_system, config),
  UpdatedParkingConfig = 
    ParkingConfig#{
      two_wheeler_size => 10, 
      three_wheeler_size => 10,
      four_wheeler_size => 10
    },
  ok = application:set_env(parking_system, config, UpdatedParkingConfig),
  ok = parking_system_app:start(),
  [{parking_config, ParkingConfig} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  ParkingConfig = ?config(parking_config, Config),
  ok = application:set_env(parking_system, config, ParkingConfig),
  ok = parking_system_app:stop().

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec entry_exit(config()) -> any().
entry_exit(_Config) ->

  "successfully vehicle parked" = parking_system:entry(front, two_wheeler),
  _ = [parking_system:entry(rear, two_wheeler) || _X <- lists:seq(1, 9)],
  "currently parking slots not available" = parking_system:entry(front, two_wheeler),
  
  _ = [parking_system:entry(rear, three_wheeler) || _X <- lists:seq(1, 10)],
  _ = [parking_system:entry(front, four_wheeler) || _X <- lists:seq(1, 10)],

  "Huff, I'm fully loaded!"     = parking_system:entry(front, four_wheeler),
  "6 is now available to park"  = parking_system:exit(6, four_wheeler),
  "successfully vehicle parked" = parking_system:entry(rear, four_wheeler),

  "Sorry, please enter proper slot number" = parking_system:exit(-5, four_wheeler),
  "Sorry, please enter proper slot number" = parking_system:exit(15, four_wheeler),

  "Sorry, You look different!. I can accommodate parking space for only 4/ 3/ 2-wheelers." =
    parking_system:exit(5, six_wheeler),
  "Sorry, You look different!. I can accommodate parking space for only 4/ 3/ 2-wheelers." = 
    parking_system:entry(front, six_wheeler),
  
  "Sorry, You look different!. I can allow only front/ rear-gates" =
    parking_system:entry(wrong_entry, four_wheeler).

-spec unused_callbacks(config()) -> any().
unused_callbacks(_Config) ->
  {noreply, _State} = parking_system:handle_cast(request, self()).
