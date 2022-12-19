https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
%
% test.erl
%

-module(test).
-export([sum/0]).

print_viewer() ->
    receive
        {updated, Name, Value, _} ->
            io:format("~p = ~p.~n", [Name,Value]),
            print_viewer();
        _ ->
            print_viewer()
    end.

sum() ->
    {ok, Sheet} = sheet:sheet(),
    UI = spawn(fun print_viewer/0),
    {ok, Sum} = sheet:cell(Sheet, sum),
    sheet:add_viewer(Sum, UI),
    {ok, A} = sheet:cell(Sheet, a),
    {ok, B} = sheet:cell(Sheet, b),
    sheet:add_viewer(A, UI),
    sheet:add_viewer(B, UI),
    sheet:set_value(Sum, {formula, fun lists:sum/1, [a, b]}),
    sheet:set_value(A, 23),
    sheet:set_value(B, 19),
    sheet:set_value(B, undefined),
    sheet:set_value(A, 23),
    sheet:set_value(B, 19),
    sheet:set_value(A, undefined),
    sheet:set_value(A, 23),
    {Sheet, Sum, A, B, UI}.

