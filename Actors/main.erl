-module(main).
-export([main/0, kill_spawn_loop/3, spawn_loop/4, kill_loop/2]).


%%%%%
%
% @params: CarList: list of cars
%          H: height of the chessboard
%          W: width of the chessboard
%
%  This function choose a random number of cars and kill and spawn the chosen number of cars every 10 seconds
kill_spawn_loop(CarList, H, W) -> 
    N_Cars = rand:uniform(4),
    NewList =  kill_loop(N_Cars, CarList),
    NewList2 = spawn_loop(N_Cars, NewList, H, W),
    timer:sleep(10000),
    kill_spawn_loop(NewList2, H, W).

%%%%%
%
% @params: N_Cars_to_Spawn: number of cars to spawn
%          CarList: list of cars
%          H: height of the chessboard
%          W: width of the chessboard
%
% @return: CarList: list of cars with the new ones
% This function spawns N_Cars_to_Spawn cars and adds them to CarList
% @dev: The cars are spawned with a delay of 5 seconds between them. 
%       After each car is spawned the render actor is notified to print the chessboard (Just to add some delay between the cars)
spawn_loop(N_Cars_to_Spawn, CarList, H, W) ->
    case N_Cars_to_Spawn > 0 of
        true ->
            {PID_M, Ref_monitor} = spawn_monitor(car, main, [H,W]),  
            io:format("MAIN: Spawned car with PID: ~p~n", [PID_M]),
            CarList2 = lists:append(CarList, [PID_M]),
            timer:sleep(5000),
            render ! {print},
            spawn_loop(N_Cars_to_Spawn-1, CarList2, H, W);
        false ->
            CarList
    end.

%%%%%
% @params: N_Cars_to_Kill: number of cars to kill
%          CarList: list of cars
% 
% @return: CarList: list of cars without the killed ones
% This function kills N_Cars_to_Kill cars from CarList. The car will be chosen randomly
kill_loop(N_Cars_to_Kill, CarList) ->
    case N_Cars_to_Kill > 0 of
        true ->
            %Choose randomly a car to kill
            PID_M2 = lists:nth(rand:uniform(length(CarList)), CarList),
            exit(PID_M2, kill),
            io:format("MAIN: Killed car with PID: ~p~n", [PID_M2]),
            CarList2 = [ PIDM || PIDM <- CarList, PIDM =/= PID_M2],
            kill_loop(N_Cars_to_Kill-1, CarList2);
        false ->
            CarList
    end.



main() ->
    H = 10,
    W = 10,
    Chessboard = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)]),
    PID_A = ambient:main(Chessboard), %spawn the ambient actor
    PID_W = wellknown:main(),
    PID_R = render:main([{X, Y} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)], dict:new()),
    List = spawn_loop(10, [], H, W),
    kill_spawn_loop(List, H, W).
    
                
                
                
                
    