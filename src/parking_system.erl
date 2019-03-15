-module(parking_system).

-behaviour(gen_server).

%% API
-export([
  child_spec/0,
  start_link/0,
  entry/2,
  exit/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
child_spec() ->
  #{
    id      => ?MODULE,
    start   => {?MODULE, start_link, []},
    restart => permanent
  }.

-spec entry(Gate, VehicleType) -> Res when
  Gate        :: front | rear,
  VehicleType :: four_wheeler | three_wheeler | two_wheeler,
  Res         :: string().
entry(Gate, Vehicle) ->
  gen_server:call(?MODULE, {Gate, Vehicle}).

-spec exit(Slot, VehicleType) -> Res when
  Slot        :: integer(),
  VehicleType :: four_wheeler | three_wheeler | two_wheeler,
  Res         :: string().
exit(Slot, VehicleType) ->
  gen_server:call(?MODULE, {exit, Slot, VehicleType}).

-spec start_link() -> Res when
  Res :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, #{
    two_wheeler_size   := TwoWheelers,
    three_wheeler_size := ThreeWheelers,
    four_wheeler_size  := FourWheelers,
    vehicle_types      := VehicleTypes,
    gates              := Gates
  }} = application:get_env(parking_system, config),
  
  {ok, #{
    parking_config => new(TwoWheelers, ThreeWheelers, FourWheelers),
    vehicle_types  => VehicleTypes,
    gates          => Gates
  }}.

%% @hidden
handle_call({Gate, VehicleType}, _From, #{vehicle_types := VTypes, gates := Gates} = State) ->
  case {lists:member(Gate, Gates),  lists:member(VehicleType, VTypes)} of
    {true, true} ->
      entry_req(Gate, VehicleType, State);
    {true, false} ->
      reply(unknown_request, State);
    {false, _} ->
      reply(unknown_gate_request, State)
  end;
handle_call({exit, Slot, VehicleType}, _From, #{parking_config := #{size := #{max := Size}}} = 
  State) when (Slot > 0) and (Slot < Size) ->
    case lists:member(VehicleType, maps:get(vehicle_types, State)) of
      true ->
        exit_req(Slot, VehicleType, State);
      false ->
        reply(unknown_request, State)
    end;
handle_call({exit, _Slot, _VehicleType}, _From, State) ->
  reply(unknown_slot_request, State).
 
%% @hidden
handle_cast(_Request, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

entry_req(_Gate, _VehicleType, #{parking_config := #{status := fully_loaded}} = State) ->
  reply(parking_fully_loaded, State);
entry_req(Gate, VehicleType, #{parking_config := #{vehicles := Vehicles} = Config} = State) ->  
  case maps:get(VehicleType, Vehicles) of
    #{status := fully_loaded} ->
      reply(no_slots_available, State);
    _ ->   
      #{size := Size} = NewEntry = entry(Gate, VehicleType, Config),
      VehiclesDetails = maybe_update_vehicles_details(NewEntry),
      ParkingStatus   = maybe_update_parking_status(Size),
      
      NewState = State#{
        parking_config => #{
          vehicles => VehiclesDetails, 
          status => ParkingStatus, 
          size => Size
        }
      },
    
    {reply, "successfully vehicle parked", NewState}
  end.

entry(Gate, VehicleType, #{vehicles := Vehicles, size := Size} = Config) ->
  #{available_slots := Slots} = VehicelsDetails = maps:get(VehicleType, Vehicles),
  AvailableSlots = available_slots(Gate, Slots),
  Config#{
    vehicles => Vehicles#{
      VehicleType => VehicelsDetails#{
        available_slots => tl(AvailableSlots)
      }
    },
    size => Size#{
      current => maps:get(current, Size) + 1
      }
  }.

available_slots(front, Slots) -> lists:usort(Slots);
available_slots(rear, Slots)  -> lists:reverse(lists:usort(Slots)).

exit_req(Slot, VehicleType, #{parking_config := #{vehicles := Vehicles, 
                                size := #{current := Current} = Size
                            }} = State) ->
  #{VehicleType := 
    #{available_slots := AvailableSlots, max_slots := MaxSlots} = Vehicle
  } = Vehicles,

  case {Slot < MaxSlots, lists:member(Slot, AvailableSlots)} of
    {true, false}  ->
      NewState = State#{
        parking_config => #{
          size     => Size#{current => Current - 1},
          status   => vacant,
          vehicles => Vehicles#{
            VehicleType => Vehicle#{
              available_slots => lists:sort([Slot | AvailableSlots]), 
              status          => vacant
            }
          }
        }
      },
      {reply, erlang:integer_to_list(Slot) ++ " is now available to park", NewState};
    _ ->
      reply(unknown_slot_request, State)
  end.

new(TwoWheelerSize, ThreeWheelerSize, FourWheelerSize) ->
  #{
    vehicles => #{
      two_wheeler => #{
        available_slots => lists:seq(1, TwoWheelerSize), 
        status => vacant,
        max_slots => TwoWheelerSize
      },
      three_wheeler => #{
        available_slots => lists:seq(1, ThreeWheelerSize), 
        status => vacant,
        max_slots => TwoWheelerSize
      },
      four_wheeler => #{
        available_slots => lists:seq(1, FourWheelerSize), 
        status => vacant,
        max_slots => TwoWheelerSize
      }
    },
    size => #{
      max => TwoWheelerSize + ThreeWheelerSize + FourWheelerSize, 
      current => 0
    },
    status => vacant
  }.

maybe_update_vehicles_details(#{vehicles := Vehicles}) ->
  Fun = 
    fun
      (_VehicleType, #{available_slots := []} = Vehicle) -> 
        Vehicle#{available_slots => [], status => fully_loaded};
      (_VehicleType, Vehicle) ->
        Vehicle
    end,
  maps:map(Fun, Vehicles).

maybe_update_parking_status(#{max := Max, current := Max}) -> fully_loaded;
maybe_update_parking_status(_) -> vacant.

reply(parking_fully_loaded, State) ->
  {reply, "Huff, I'm fully loaded!", State};
reply(no_slots_available, State) ->
  {reply, "currently parking slots not available", State};
reply(unknown_request, State) ->
  Msg = "Sorry, You look different!. I can accommodate parking space for only 4/ 3/ 2-wheelers.",
  {reply, Msg, State};
reply(unknown_gate_request, State) ->
  Msg = "Sorry, You look different!. I can allow only front/ rear-gates",
  {reply, Msg, State};
reply(unknown_slot_request, State) ->
  {reply, "Sorry, please enter proper slot number", State}.
