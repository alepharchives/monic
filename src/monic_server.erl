-module(monic_server).
-behavior(gen_server).
-include("monic.hrl").

% public API

-export([start_link/0]).

% gen_server API
-export([init/1, terminate/2, code_change/3,handle_call/3, handle_cast/2, handle_info/2]).

-record(group, {
          name,
          fd=nil,
          requests=[]
         }).

% public functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server functions

init([]) ->
    {ok, RootDir} = application:get_env(monic, "rootdir"),
    %State = init_workers(Name, Options),
    {ok, #state{}}.

handle_call({read, #handle{path=Path}}=Request, From, #state{idle=Idle}=State)->
    case lists:partition(fun({Path1, _}) -> Path1 == Path end, Idle) of
        {[], _} ->
            {noreply, State#state{requests=[{Request, From} | State#state.requests]}};
        {[W], Rest} ->
            monic_worker:start_work(W, {Request, From}),
            {noreply, State#state{idle=Rest,busy=[W|State#state.busy]}}
    end;
handle_call({write, _Bin}=Request, From, #state{idle=Idle}=State) ->
    case Idle of
        [] ->
            {noreply, State#state{requests=[{Request, From} | State#state.requests]}};
        [W|Rest] ->
            monic_worker:start_work(W, {Request, From}),
            {noreply, State#state{idle=Rest,busy=[W|State#state.busy]}}
    end;
handle_call(close, _From, State) ->
    shutdown_workers(State),
    {stop, normal, ok, State}.

handle_cast({done, Worker, From, Resp}, #state{requests=[]}=State) ->
    Busy = [B || B <- State#state.busy, B /= Worker],
    Idle = State#state.idle ++ [Worker],
    gen_server:reply(From, Resp),
    {noreply, State#state{idle=Idle, busy=Busy}};
handle_cast({done, {_, Pid}, From, Resp}, #state{requests=[R|Rest]}=State) ->
    gen_server:reply(From, Resp),
    monic_worker:start_work(Pid, R),
    {noreply, State#state{requests=Rest}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    shutdown_workers(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% private functions

init_workers(Name, Options) ->
    Max = get_value(max, Options, 1),
    Workers = lists:map(fun(N) ->
                                Path = filename:join(Name, integer_to_list(N)),
                                {ok, Worker} = monic_worker:start_link(self(), Path),
                                Worker end, lists:seq(1, Max)),
    #state{idle=Workers}.

shutdown_workers(#state{busy=Busy,idle=Idle}) ->
    lists:foreach(fun(Worker) -> monic_worker:close(Worker) end,
                  Busy ++ Idle),
    ok.

get_value(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        false -> Default;
        {_, Value} -> Value
    end.
