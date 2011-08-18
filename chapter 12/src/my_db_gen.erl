%%%----------------------------------------------------------------------------------
%%% File    : my_db_gen.erl
%%% Author  : Mirko Bonadei <mirko.bonadei@gmail.com>
%%% Description : Exercise N.1 of the chapter 12. Implement asimple DB,
%%                as described in chapter 5, but with the gen_server 
%%                behaviour.
%%%
%%% Created : 18 Aug 2011 by Mirko Bonadei <mirko.bonadei@gmail.com>
%%%----------------------------------------------------------------------------------
-module(my_db_gen).
-author("mirko.bonadei@gmail.com").
-behaviour(gen_server).

-export([
	 start/0,
	 stop/0,
	 write/2,
	 delete/1,
	 read/1,
	 match/1
	 ]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

-define(SERVER, ?MODULE).

%%=======================================================================================
%% API FUNCTIONS
%%=======================================================================================

%%---------------------------------------------------------------------------------------
%% Function: start() -> ok | {error, Reason}
%% Description: Starts a new my_db_gen server.
%%---------------------------------------------------------------------------------------
start() ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
	{ok, _Pid} ->
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

%%----------------------------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Stops my_db_gen server.
%%----------------------------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%----------------------------------------------------------------------------------------
%% Function: write(Key, Value) -> ok
%% Description: Adds a Key with a Value to the DB. If the Key is already inserted it 
%%              updates its value.
%%----------------------------------------------------------------------------------------
write(Key, Value) ->
    gen_server:cast(?SERVER, {insert, {Key, Value}}).

%%----------------------------------------------------------------------------------------
%% Function: delete(Key) -> ok
%% Description: deletes the Key and the bound Value in the DB
%%----------------------------------------------------------------------------------------
delete(Key) ->
    gen_server:cast(?SERVER, {delete, Key}).

%%----------------------------------------------------------------------------------------
%% Function: read(Key) -> {ok, Element}|{error, instance}
%% Description: Reads the Element associated to the Key if there is one
%%----------------------------------------------------------------------------------------
read(Key) ->
    gen_server:call(?SERVER, {read, Key}).

%%----------------------------------------------------------------------------------------
%% Function: match(Element) -> [] | [Key1, Key2, ..., KeyN]
%% Description: Returns the List of Keys bounded with the Key
%%----------------------------------------------------------------------------------------
match(Element) ->
    gen_server:call(?SERVER, {match, Element}).


%%========================================================================================
%% gen_server callbacks
%%========================================================================================
init([]) ->
    {ok, []}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({insert, {Key, Value}}, State) ->
    NewState = case lists:keysearch(Key, 1, State) of
		   {value, _} ->
		       lists:keyreplace(Key, 1, State, {Key, Value});
		   false ->
		       [{Key, Value}|State]
	       end,
    {noreply, NewState};
handle_cast({delete, Key}, State) ->
    NewState = lists:foldl(fun({KeyTmp, ValueTmp}, Result) ->
			case KeyTmp =/= Key of
			    true -> [{KeyTmp, ValueTmp}|Result];
			    false -> Result
			end
			end, [], State),
    {noreply, NewState}.


handle_call({read,Key}, _From, State) ->
    Reply = case lists:keysearch(Key, 1, State) of
	{value, {Key, Value}} ->
	    {ok,Value};
		false ->
	    {error, instance}
	    end,
    {reply, Reply, State};
handle_call({match, Element}, _From, State) ->
    Reply = lists:foldl(fun({KeyTmp,ValueTmp}, Result) ->
				   case ValueTmp == Element of
				       true -> [KeyTmp|Result];
				       false -> Result
				   end
			   end, [], State),
    {reply, Reply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.
