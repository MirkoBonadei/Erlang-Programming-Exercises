%%%-------------------------------------------------------------------
%%% File    : my_db_sup.erl
%%% Author  : Mirko Bonadei <mirko.bonadei@gmail.com>
%%% Description : 
%%%
%%% Created : 18 Aug 2011 by Mirko Bonadei <mirko.bonadei@gmail.com>
%%%-------------------------------------------------------------------
-module(my_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Server = {my_db_gen, {my_db_gen, start, []},
	      permanent, 30000, worker, [my_db_gen]},
    Children = [Server],
    RestartStrategy = {one_for_one, 5,3600},
    {ok,{RestartStrategy, Children}}.
