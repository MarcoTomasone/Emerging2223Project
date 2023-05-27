-module(car).
-export([main/2,friendship/2, friendship/3, state/4, state/7, detect/7, getFriends/4, generate_new_goal/5]).


    get_5_alive_friends(TotalFriends, PidList, RefList,  N) ->
        %take a random friend from the list, if the pid is alive monitor it and add it to ref list
        case {N, length(TotalFriends)} of
            {_, 0} -> {PidList, RefList};
            {0, _} -> {PidList, RefList};
            {_, _} -> 
                {PIDF, PIDS} = lists:nth(rand:uniform(length(TotalFriends)), TotalFriends),
                case is_process_alive(PIDF) of
                    true -> 
                        Ref = monitor(process, PIDF),
                        get_5_alive_friends(lists:delete({PIDF, PIDS}, TotalFriends), [{PIDF,PIDS} | PidList] , [Ref | RefList], N-1);
                    false -> 
                        %io:format("FRI: I'm ~p I'm not alive anymore~n", [PIDF]),
                        get_5_alive_friends(lists:delete({PIDF, PIDS}, TotalFriends), PidList, RefList, N)
                end
        end.


    receive_friends(FriendsList, RefList, L, PID_S, Ref) ->
        receive
            {myFriends, PIDSLIST, Ref} ->
                %io:format("FRI: I'm ~p I received a list of friends ~p~n", [self(), PIDSLIST]),
                %Demonitor the old friends from refList
                lists:foreach(fun(X) -> demonitor(X) end, RefList),
                %Add the new friends to the list
                TotalFriends = lists:usort(PIDSLIST ++ FriendsList),
                %Create a list choosing 5 random friends from TotalFriends and monitor all the friends in the list and save the ref in RefList
                {FriendList2, RefList2} = get_5_alive_friends(TotalFriends, [], [], 5),
                %io:format("FRI: I'm ~p my friends are:~p~n ", [RefList2, FriendList2]),
                
                PID_S ! {listModified, FriendList2},
                timer:sleep(5000),
                case length(FriendList2) < 5 of
                    true -> getFriends(FriendList2, RefList2, L, PID_S);
                    false -> {FriendList2, RefList2}
                end
        after 5000 -> 
            case length(FriendsList) < 5 of
                    true -> getFriends(FriendsList, RefList, L, PID_S);
                    false -> {FriendsList, RefList}
                end
        end.
        


    getFriends(FriendsList, RefList, L, PID_S) ->
        Lenght = length(L),
        case Lenght of
            0 -> 
                Ref = make_ref(),
                wellknown ! {getFriends, self(), PID_S, Ref},
                receive_friends(FriendsList, RefList, L, PID_S, Ref);
            5 -> 
                {FriendsList, RefList};
            _ -> 
                %pick a random friend from the list
                {PIDF, _} = lists:nth(rand:uniform(Lenght), L),
                %delete the friend from the list
                %io:format("FRI: I'm ~p I'm asking friends to ~p~n", [self(), PIDF]),
                L2 = [ {PIDFR, PIDSTATE} || {PIDFR, PIDSTATE} <- L, PIDFR =/= PIDF],
                Ref = make_ref(),
                PIDF ! {getFriends, self(), PID_S, Ref},
                receive_friends(FriendsList, RefList, L2, PID_S, Ref)
        end.
        
    friendship(FriendsList, RefList, PID_S) ->
        %io:format("FRI: start friendship with PID: ~p~n", [self()]),
        case length(FriendsList) of 
            5 ->
                %io:format("FRI: I'm ~p I have 5 friends:~p~n ", [self(), FriendsList]),
                receive 
                    {getFriends, PIDF, PIDS, Ref} ->
                        %io:format("FRI: I'm ~p receiving get friends from ~p~n", [self(), PIDF]),
                        PIDF ! {myFriends, FriendsList, Ref},
                        friendship(FriendsList, RefList, PID_S);
                    %case a friend dies
                    {'DOWN', _, _, PID, Reason } ->
                        io:format("FRI: Died PID: ~p, Reason: ~p~n", [PID, Reason]),
                        %delete the friend from the list
                        L2 = [ {PIDFR, PIDSTATE} || {PIDFR, PIDSTATE} <- FriendsList, PIDFR =/= PID],
                        friendship(L2, RefList, PID_S)
                end;
            _ ->
                {FriendList2, RefList2} = getFriends(FriendsList, RefList, FriendsList, PID_S),
                PID_S ! {listModified, FriendList2}, %Send to state the new list of friends
                friendship(FriendList2, RefList2, PID_S)

        end.

   
    %%%%
    % @param: PID_S: PID of the state actor
    % @dev: This function is used to link friendship actor to the state actor and to the detect actor.
    %       Now, F is linked to S and F is linked to D. Given that F and D are linked, S is linked to D.
    %       After that it calls friendship/3 that is the main function of the friendship actor
    friendship(PID_S, PID_D) ->
        link(PID_S),
        link(PID_D),
        friendship([], [], PID_S).


    state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W, FriendList) ->
        %io:format("Start State ~p~n", [self()]),
        receive
            {updateState, X, Y, IsFree} -> 
                %io:fwrite("Update Value for park (~p,~p), new value: ~p~n", [X,Y, IsFree]), %Update parkings  
                case {IsFree, dict:fetch({X,Y}, World_Knowledge)==IsFree} of
                    %It doesn't update the goal coordinates because this case doesn't affect s
                    {_, true} -> state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W, FriendList);
                    %If a park becames free, the state actor updates the knowledge of the world
                    {true, false}->
                            World_Knowledge2 = dict:store({X,Y}, IsFree, World_Knowledge),
                            lists:foreach(fun({_, PIDSTATE}) -> PIDSTATE ! {notifyStatus, X, Y, IsFree} end, FriendList),
                            state(PID_D, World_Knowledge2, X_Goal, Y_Goal, H, W, FriendList);
                    %If a park becames busy I have to check if it was the goal
                    {false,false} -> 
                            World_Knowledge2 = dict:store({X,Y}, IsFree, World_Knowledge),
                            lists:foreach(fun({_, PIDSTATE}) -> PIDSTATE ! {notifyStatus, X, Y, IsFree} end, FriendList),
                            case {X=:=X_Goal, Y =:= Y_Goal} of
                                %If the goal is busy, I have to generate a new goal   
                                {true,true} -> 
                                    %io:fwrite("DISCOVER I HAVE TO CREATE A NEW GOAL: ~p~n", [PID_D]),
                                    {X_Goal_New, Y_Goal_New} = generate_new_goal(H, W, X_Goal, Y_Goal, World_Knowledge2), 
                                    PID_D ! {updateGoal, X_Goal_New, Y_Goal_New},
                                    state(PID_D, World_Knowledge2, X_Goal_New, Y_Goal_New, H, W,FriendList);
                                %Else update the knowledge of the world
                                {_,_} -> 
                                    state(PID_D, World_Knowledge2, X_Goal, Y_Goal, H, W,FriendList)
                            end
                end;

            %Case Car Exit from the parking and needs new goal
            {askNewGoal, PID_D, Ref} -> 
                %io:format("Ask New Goal with Ref ~p~n",[Ref]),
                {X_Goal_New, Y_Goal_New} = generate_new_goal(H, W, X_Goal, Y_Goal, World_Knowledge), 
                PID_D ! {responseNewGoal, X_Goal_New, Y_Goal_New, Ref},
                state(PID_D, World_Knowledge, X_Goal_New, Y_Goal_New, H, W,FriendList);
            
            {notifyStatus, X, Y, IsFree} -> 
                %io:format("Send myself a message notify status ~p~n", [self()]),
                self() ! {updateState, X, Y, IsFree},
                state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W,FriendList);
                %io:format("CAR: Notify Status {~p,~p} : ~p ~n", [X,Y,IsFree]),
                
            {listModified, NewFriendList} -> 
                %io:format("Received new list of friends: ~p~n", [ NewFriendList]),
                render ! {friends, self(), NewFriendList}, %send to render the new list of friends
                state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W , NewFriendList);

            %Default 
            _ -> state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W,FriendList)
        end.

    %%%
    % Function used to initialize the state actor, receives the PID of the detect actor and starts the real detect actor 
    state(X_Goal, Y_Goal, H, W) -> 
        World_Knowledge = dict:from_list([{{X, Y}, undefined} || X <- lists:seq(0, H-1), Y <- lists:seq(0, W-1)]), 
        receive
            {pidDetect, PID_D} ->
                link(PID_D),
                state(PID_D, World_Knowledge, X_Goal, Y_Goal, H, W, [])
        end.


    %%%
    % Function used to generate random coordinates for the new goal, checks that the coordinates should be different from the old ones 
    % and that the cell is free 
    % @param H: Height of the world
    % @param W: Width of the world
    % @param Old_X: X coordinate of the old goal
    % @param Old_Y: Y coordinate of the old goal
    % @param World_Knowledge: Knowledge of the world
    % @return {X_Goal_New, Y_Goal_New}: New coordinates of the goal
    generate_new_goal(H, W, Old_X, Old_Y, World_Knowledge) ->
        {X_Goal_New, Y_Goal_New} = generate_coordinates(H, W),
        %If the coordinates I generate are free and are different from old ones I return them
        case dict:fetch({X_Goal_New, Y_Goal_New}, World_Knowledge) of
            %It's impossible to get an error because the dict is initialized with all the cells
            error -> exit(self(), "Error in generate new Goal");     
            false -> generate_new_goal(H, W, Old_X, Old_Y, World_Knowledge);
            %case true & undefined 
            _ -> 
                case {X_Goal_New =:= Old_X, Y_Goal_New =:= Old_Y} of
                    {true, true} -> generate_new_goal(H, W, Old_X, Old_Y, World_Knowledge);
                    {_,_} -> {X_Goal_New, Y_Goal_New}
                end
        end.


    %%%%
    % @param H: height of the chessboard
    % @param W: width of the chessboard
    % @return: a tuple {X,Y} with X and Y coordinates 
    generate_coordinates(H, W) ->
        X = rand:uniform(H)-1,
        Y = rand:uniform(W)-1,
        {X, Y}.

    
    %%%%
    % @param X:  actual X coordinate of the car
    % @param X_Goal: X coordinate of the goal
    % @param H: height of the chessboard
    % @return: 1 or -1
    % @dev: This function computes the movement along the X axis that minimizes the distance to the goal.
    %       If the distance is the same, the function returns 1. Since pacman effect is allowed (the car can move from the last cell to the  
    %       first one and viceversa), the function must consider the modulo operation.
    compute_X_movement(X, X_Goal, H) ->
        D_pos = abs(X_Goal - ((X+1) rem H)), 
        D_neg = abs(X_Goal - ((X-1) rem H)),
        %io:format("X: D_pos: ~p, D_neg: ~p~n", [D_pos, D_neg]),
        case D_pos =< D_neg of
            true -> 1;
            false -> -1
        end.

    %%%%
    % @param Y:  actual Y coordinate of the car
    % @param Y_Goal: Y coordinate of the goal
    % @param W: width of the chessboard
    % @return: 1 or -1
    % @dev: This function computes the movement along the Y axis that minimizes the distance to the goal.
    %       If the distance is the same, the function returns 1. Since pacman effect is allowed (the car can move from the last cell to the
    %       first one and viceversa), the function must consider the modulo operation.
    compute_Y_movement(Y, Y_Goal, W) ->
        D_pos = abs(Y_Goal - ((Y+1) rem W)), 
        D_neg = abs(Y_Goal - ((Y-1) rem W)),
        %io:format("Y: D_pos: ~p, D_neg: ~p~n", [D_pos, D_neg]),
        case D_pos =< D_neg of
            true -> 1;
            false -> -1
        end.

    %%%%
    % @param X:  actual X coordinate of the car
    % @param Y:  actual Y coordinate of the car
    % @param X_Goal: X coordinate of the goal
    % @param Y_Goal: Y coordinate of the goal
    % @param H: height of the chessboard
    % @param W: width of the chessboard
    % @return: {X,Y} coordinates of the next cell
    % @dev: This function computes the next cell to reach the goal. 
    %       If the car is already in the goal this function sends the message "park" to the ambient actor.
    %       If the car is not in the goal, the function computes the next cell to reach the goal. 
    %       If the car is not in the goal and it must move along both the axes, the function chooses randomly the axis and the direction.
    move(X, Y, {X_Goal, Y_Goal}, H, W) ->

        case {X =:= X_Goal, Y =:= Y_Goal} of
            {true, true} -> 
                {X, Y};
            {true, false}-> 
                {X, (Y + compute_Y_movement(Y, Y_Goal, W)) rem (W)};
            {false, true} -> 
                {(X + compute_X_movement(X, X_Goal, H)) rem (H), Y};
            {false, false} ->
                case random:uniform(2) of
                    1 -> {(X + compute_X_movement(X, X_Goal, H+1)) rem (H), Y};
                    2 -> {X, (Y + compute_Y_movement(Y, Y_Goal, W+1)) rem (W)}
                end
        end. 
    %%%%%%
    %@param X:  actual X coordinate of the car
    %@param Y:  actual Y coordinate of the car
    %
    detect(X, Y, X_Goal, Y_Goal, H, W, PID_S) ->
        %io:format("Start Detect with goal (~p,~p)~n", [X_Goal, Y_Goal]),
        timer:sleep(2000),
        {X_New, Y_New} = move(X, Y, {X_Goal, Y_Goal}, H, W),  
        render ! {position, PID_S, X_New, Y_New},
        Ref = make_ref(),
        %io:format("Ref ~p~n", [Ref]),
        ambient ! {isFree, self(), X_New, Y_New, Ref},
        receive 
            {updateGoal, X_Goal_New, Y_Goal_New} ->  
                render ! {target, PID_S, X_Goal_New, Y_Goal_New},
                %Handle the message from the ambient actor
                receive 
                    {status, Ref, IsFree} ->  
                        PID_S ! {updateState, X_New, Y_New, IsFree},
                        detect(X_New, Y_New, X_Goal_New, Y_Goal_New, H, W, PID_S)
                end;             
            {status, Ref, IsFree} -> 
                %io:format("Received status ~p with Ref ~p~n", [IsFree, Ref]),
                PID_S ! {updateState, X_New, Y_New, IsFree},
                case {X_New =:= X_Goal, Y_New =:= Y_Goal, IsFree} of
                    {true, true, true} ->
                        Park_Ref = make_ref(),
                        ambient ! {park, PID_S, X_New, Y_New, Park_Ref},
                        timer:sleep(rand:uniform(5)*1000),
                        ambient ! {leave, PID_S, Park_Ref},
                        PID_S ! {askNewGoal, self(), Park_Ref},
                        receive
                            {responseNewGoal, X_Goal_New, Y_Goal_New, Park_Ref} ->
                                %io:format("Received new goal (~p, ~p) with Ref: ~p~n", [X_Goal_New, Y_Goal_New, Park_Ref]),
                                render ! {target, PID_S, X_Goal_New, Y_Goal_New},
                                detect(X_New, Y_New, X_Goal_New, Y_Goal_New, H, W, PID_S)
                        end;
                    %If not in  goal
                    _ -> detect(X_New, Y_New, X_Goal, Y_Goal, H, W, PID_S) 
                end; 
            MSG -> io:format("D: ~p NO PATTERN MATCHING FOUND ~p~n", [self(), MSG])
        end.

    %The main actor creates other actors and re-creates them if they fail
    main(H, W) ->
        process_flag(trap_exit, true), 
        %io:format("X_Spawn: ~p, Y_Spawn: ~p~n", [X_Spawn, Y_Spawn]),
        %io:format("X_Goal: ~p, Y_Goal: ~p~n", [X_Goal, Y_Goal]),
        Spawn_loop = fun Spawn_loop() ->
            
            {X_Spawn, Y_Spawn} = generate_coordinates(H, W),
            {X_Goal, Y_Goal} = generate_coordinates(H, W),
            PID_S = spawn(?MODULE, state, [X_Goal, Y_Goal, H, W]),
            PID_D = spawn_link(?MODULE, detect, [X_Spawn, Y_Spawn, X_Goal, Y_Goal, H, W, PID_S]),
            PID_S ! {pidDetect, PID_D},
            PID_F  = spawn(?MODULE, friendship, [PID_S, PID_D]),
            render ! {target, PID_S, X_Goal, Y_Goal},
            render ! {position, PID_S, X_Spawn, Y_Spawn},
            receive
                {'EXIT', PID, Reason } ->
                    io:format("Died PID: ~p, Reason: ~p~n", [PID, Reason]),
                    Spawn_loop();
                X -> io:format("DIED ~p~n", [X])
            end
        end,
        Spawn_loop().
     

       


    

