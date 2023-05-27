%Render actors represent the ambient at the moment of the request. They are used to show the current state of the ambient to the user.
%Show in ASCII ART the ambient

% Path: Actors\render.erl
-module(render).
-export([main/2, render/3]).

%%%%
% @params: Chessboard: list of positions of the cars
%          DictToList: dict of cars
% @dev: This function prints the chessboard in ASCII ART 
%       O represents an empty cell
%       Num* represents  a car goal  
%       *Num* represents a cell with a car parked on a goal 
%       Num represents a cell with a car in  cell
print_chessboard([{X,Y}|T], DictToList) ->
    case Y == 0 of
        true -> io:format("\n");
        _ -> ok
    end,
    A = lists:keyfind({X,Y}, 2, DictToList),
    B = lists:keyfind({X,Y}, 3, DictToList),
    %The struct is {PID, {POS}, {GOAL}, [FRIENDS], ID}
    case {A,B} of 
        {false, false} -> io:format("O\t");
        {{_,{X,Y},_,_, ID}, {_,_,{X,Y}, _, ID}} -> io:format("*~p*\t", [ID]);
        {{_,{X,Y},_,_, ID}, _} -> io:format("~p\t", [ID]);
        {_ , {_,_,{X,Y},_, ID}} -> io:format("~p*\t", [ID])
    end,
    print_chessboard(T, DictToList );
print_chessboard([],_)-> io:format("\n").



render(Chessboard, Dict, N) ->
    % PID -> {{POS},{GOAL}, int}     DICT STRUCTURE
    receive
        {print} ->
            %Transform the dict to a list and flatten it using map
            DictToList = dict:to_list(Dict),
            DictToList2 = lists:map(fun({A, {B,C,D,E}}) -> io:format("R: Car ~p with PID ~p is Friend with ~p~n", [E,A,D]), {A,B,C,D,E} end, DictToList),
            
            print_chessboard(Chessboard, DictToList2),
            render(Chessboard, Dict, N);
        % position of car sent by detect
        {position, PID, X, Y} -> 
            case dict:find(PID, Dict) of 
                error -> 
                    %If it is not in the dict, it is a new car create it with undefined goal
                    Dict2 = dict:store(PID, {{X,Y}, {undefined, undefined}, [], N}, Dict),
                    monitor(process, PID),
                    render(Chessboard, Dict2, N+1);
                
                {ok, {{_,_},{X_Goal, Y_Goal}, FriendsList, ID}} ->
                    %If it is in the dict, update the position
                    Dict2 = dict:store(PID, {{X,Y},{X_Goal, Y_Goal}, FriendsList, ID}, Dict),
                    render(Chessboard, Dict2, N)
            end;
            
        % target position of goal sent by detect
        {target, PID, X, Y} ->    
            case dict:find(PID, Dict) of 
                error -> 
                    %If it is not in the dict, it is a new car create it with undefined position
                    Dict2 = dict:store(PID, {{undefined, undefined},{X,Y}, [], N}, Dict),
                    monitor(process, PID),
                    render(Chessboard, Dict2, N+1);
                {ok, {{X_Pos, Y_Pos}, {_,_}, FriendsList, ID}} ->
                    %If it is in the dict, update the goal
                    Dict2 = dict:store(PID, {{X_Pos, Y_Pos},{X,Y}, FriendsList, ID}, Dict),
                    render(Chessboard, Dict2, N)
            end;

        %Sent by ambient when car park or restart
        {parked, PID, X, Y, IsParked} -> 
            {_,_,_, Num} = dict:fetch(PID, Dict),
            io:format("RENDER: Car N_~p with PID ~p is parked at (~p, ~p): ~p~n", [Num, PID, X, Y, IsParked]),
            self() ! {print},
            render(Chessboard, Dict, N);
        
        %sent by friendship actor  
        {friends, PID, PIDLIST} -> 
            case dict:find(PID, Dict) of 
                error -> 
                    %If it is not in the dict, it is a new car create it with undefined position and goal
                    Dict2 = dict:store(PID, {{undefined, undefined}, {undefined, undefined}, PIDLIST, N}, Dict),
                    monitor(process, PID),
                    render(Chessboard, Dict2, N+1);

                {ok, {{X_Pos, Y_Pos}, {X_Goal, Y_Goal}, _, ID}} ->
                    Dict2 = dict:store(PID, {{X_Pos, Y_Pos},{X_Goal,Y_Goal}, PIDLIST ,ID}, Dict),
                    %io:format("RENDER: Car N_~p with PID: ~p is friend with ~p~n", [ID, PID, PIDLIST]),
                    render(Chessboard, Dict2, N)
            end;
        {'DOWN', _, _, PID, Reason } ->
                    {_,_,_, Num} = dict:fetch(PID, Dict),
                    io:format("RENDER: Died PID: ~p with N_~p, Reason: ~p~n", [PID, Num, Reason]),
                    %remove from dict
                    Dict2 = dict:erase(PID, Dict),
                    render(Chessboard, Dict2, N)
    end. 



main(Chessboard, Dict) ->
    PID_R = spawn(render, render, [Chessboard, Dict, 1]),
    register(render, PID_R),
    io:format("RENDER: Correctly registered ~p as 'render' ~n", [PID_R]),
    PID_R. 
