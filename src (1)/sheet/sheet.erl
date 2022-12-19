https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-module(sheet).

%% API
-export([ sheet/0
        , cell/2
        , add_viewer/2
        , remove_viewer/2
        , get_viewers/1
        , set_value/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

sheet() ->
    P = spawn(fun() -> sheet_server(dict:new()) end),
    io:format("Started sheet process ~p~n", [P]),
    {ok, P}.

cell(S, A) -> rpc(S, {cell, A}).
add_viewer(C, P) -> async(C, {self(), {add_viewer, P}}).
remove_viewer(C, P) -> async(C, {self(), {remove_viewer, P}}).
get_viewers(C) -> rpc(C, get_viewers).
set_value(C, V) -> rpc(C, {set, V}), ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_viewers([], _) -> ok;
notify_viewers([H|T], Msg) ->
    async(H, Msg),
    notify_viewers(T, Msg).

make_updater(Sheet, Cell, Expression) ->
    case Expression of
        {formula, Fun, Deps} ->
            Pid = spawn(fun() -> update_server(Sheet, Cell, Fun, Deps, [], Deps) end),
            lists:map(fun(D) -> {ok, C} = cell(Sheet, D), add_viewer(C, Pid) end, Deps),
            Pid;
        
        Value ->
            Func = fun(X) -> X, Value end,
            spawn(fun() -> update_server(Sheet, Cell, Func, [], [], []) end)
    end.

%%%===================================================================
%%% Communication primitives
%%%===================================================================
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	    {Pid, Response} -> Response
    end.

async(Pid, Msg) -> Pid ! Msg.

reply(From, Msg) -> From ! {self(), Msg}.
reply_ok(From, Msg) -> From ! {self(), {ok, Msg}}.

%%%===================================================================
%%% Server loops
%%%===================================================================
sheet_server(Cells) ->
    receive
        {From, {cell, A}} ->
            case dict:is_key(A, Cells) of
                true ->
                    {ok, [Pid|[]]} = dict:find(A, Cells),
                    reply_ok(From, Pid),
                    sheet_server(Cells);
                false ->
                    Sheet = self(),
                    P = spawn(fun() ->
                        cell_server(undefined, Sheet, A, undefined, []) end),
                    NewCells = dict:append(A, P, Cells),
                    io:format("Created cell ~p with pid ~p~n", [A,P]),
                    reply_ok(From, P),
                    sheet_server(NewCells)
            end;
        
        stop -> ok;
        
        Unknown ->
        io:format("~p received an unknown message ~p~n", [self(), Unknown]),
        sheet_server(Cells)
    end.

cell_server(Updater, Sheet, Name, Value, Viewers) ->
    receive
        {_, {add_viewer, P}} ->
            notify_viewers([P], {updated, Name, Value, []}), 
            case lists:member(P, Viewers) of
                false ->
                    cell_server(Updater, Sheet, Name, Value, [P|Viewers]);
                true ->
                    cell_server(Updater, Sheet, Name, Value, Viewers)
            end;
        
        {_, {remove_viewer, P}} ->
            cell_server(Updater, Sheet, Name, Value, lists:delete(P, Viewers));
        
        {From, get_viewers} ->
            reply(From, Viewers),
            cell_server(Updater, Sheet, Name, Value, Viewers);
        
        {Updater, {updated, Result}} ->
            notify_viewers(Viewers, {updated, Name, Result, []}),
            cell_server(Updater, Sheet, Name, Result, Viewers);
        
        {From, {set, Expression}} ->
            case Updater of
                undefined -> ok;
                OldUpdater -> async(OldUpdater, stop)
            end,
            NewUpdater = make_updater(Sheet, self(), Expression),
            reply_ok(From, ok),
            cell_server(NewUpdater, Sheet, Name, Value, Viewers);
        
        stop ->
            case Updater of
                undefined -> ok;
                OldUpdater -> async(OldUpdater, stop)
            end;
        
        Unknown ->
        io:format("cell ~p received an unknown message ~p~n", [self(), Unknown]),
        cell_server(Updater, Sheet, Name, Value, Viewers)
    end.

update_server(_, Cell, Fun, Acc, [], []) ->
    Result = Fun(Acc),
    case Result of
        undefined -> reply(Cell, {updated, Result});
        error -> reply(Cell, {updated, Result});
        Value -> reply(Cell, {updated, {def, Value}}) 
    end;

update_server(Sheet, Cell, Fun, Deps, Acc, []) ->
    Result = Fun(Acc),
    lists:map(fun(D) -> {ok, C} = cell(Sheet, D), remove_viewer(C, self()) end, Deps),
    reply(Cell, {updated, {def, Result}}),
    update_server(Sheet, Cell, Fun, Deps, [], Deps);

update_server(Sheet, Cell, Fun, Deps, Acc, Queue) ->
    receive
        {updated, Name, {def, Value}, _} ->
            case lists:member(Name, Queue) of
                true ->
                    NewQueue = lists:delete(Name, Queue),
                    {ok, X} = cell(Sheet, Name),
                    remove_viewer(X, self()),
                    lists:map(fun(D) -> {ok, C} = cell(Sheet, D), add_viewer(C, self()) end, NewQueue),
                    update_server(Sheet, Cell, Fun, Deps, [Value|Acc], NewQueue);
                false ->
                    update_server(Sheet, Cell, Fun, Deps, Acc, Queue)
            end;
        
        {updated, _, undefined, _} ->
            reply(Cell, {updated, undefined}),
            update_server(Sheet, Cell, Fun, Deps, [], Deps);
        
        {updated, _, error, _} ->
            reply(Cell, {updated, error}),
            update_server(Sheet, Cell, Fun, Deps, [], Deps);
        
        stop -> ok;
        
        Unknown ->
        io:format("updater ~p received an unknown message ~p~n", [self(), Unknown]),
        update_server(Sheet, Cell, Fun, Deps, Acc, Queue)
    
    after 500 ->
        
        case Queue of
            Deps ->
                update_server(Sheet, Cell, Fun, Deps, [], Queue);
            Q ->
                reply(Cell, {updated, updating}),
                lists:map(fun(D) -> {ok, C} = cell(Sheet, D), add_viewer(C, self()) end, Q),
                update_server(Sheet, Cell, Fun, Deps, Acc, Q)
        end
    
    end.
