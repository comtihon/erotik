%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. май 2014 22:08
%%%-------------------------------------------------------------------
-module(me_connector).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/2, start/2]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
	socket :: port(),
	ssh_ref :: pid(),
	channel_ids :: dict()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Name :: string() | atom(), {atom(), Config :: proplists:proplist()}) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Config) when not is_atom(Name) ->
	start_link(list_to_atom(Name), Config);
start_link(Name, Config) ->
	gen_server:start_link({local, Name}, ?MODULE, Config, []).

start(Name, Config) when not is_atom(Name) ->
	start(list_to_atom(Name), Config);
start(Name, Config) ->
	gen_server:start({local, Name}, ?MODULE, Config, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Config :: proplists:proplist()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init(Params) ->
	gen_server:cast(self(), {connect, Params}),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call({command, List}, _From, State = #state{socket = S}) when S /= undefined ->  %working through api
	Reply = me_logic:send_command(S, List),
	{reply, Reply, State};
handle_call({command, List}, From, State = #state{ssh_ref = SSHRef, channel_ids = Dict}) -> %working through ssh
	ChannelId = me_logic:send_command(SSHRef, List),
	{noreply, State#state{channel_ids = dict:append(ChannelId, From, Dict)}};
handle_call({connect, {api, Config}}, _From, State) ->
	{Host, Port, Login, Password, Timeout} = parse_params(Config),
	% {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}], Timeout),  %TODO change to active true and stream dencoding
	{ok, Socket} = login(Login, Password, Host, Port, Timeout),
	{reply, ok, State#state{socket = Socket}};	
handle_call(disconnect, _From, State = #state{socket = Socket, ssh_ref = SShRef}) ->
	me_ssh:close(SShRef),
	me_api:close(Socket),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast({connect, {ssh, Config}}, State) ->
	{Host, Port, Login, Password, Timeout} = parse_params(Config),
	{ok, SSHRef} = me_ssh:connect(Host, Port, Login, Password, Timeout),
	{noreply, State#state{ssh_ref = SSHRef, channel_ids = dict:new()}};
handle_cast(halt, State) ->
	{stop, normal, State};
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info({ssh_cm, _, {data, ChannelId, _, Message}}, State = #state{channel_ids = Dict}) ->  %get part of ssh data
	{noreply, State#state{channel_ids = dict:append(ChannelId, binary_to_list(Message), Dict)}};
handle_info({ssh_cm, _, {closed, ChannelId}}, State = #state{channel_ids = Dict}) -> %ssh channel closed - return data to client
	case dict:find(ChannelId, Dict) of
		{ok, List} ->
			[From | Messages] = List,
			gen_server:reply(From, lists:concat(Messages)),
			{noreply, State#state{channel_ids = dict:erase(ChannelId, Dict)}};
		error -> {noreply, State}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, #state{socket = Socket, ssh_ref = SShRef}) ->
	me_ssh:close(SShRef),
	me_api:close(Socket),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_params(Config) ->
	Host = proplists:get_value(host, Config),
	Port = proplists:get_value(port, Config),
	Login = proplists:get_value(login, Config),
	Password = proplists:get_value(password, Config),
	Timeout = proplists:get_value(timeout, Config, infinity),
	{Host, Port, Login, Password, Timeout}.
	
login(Login, Password, Host, Port, Timeout) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}], Timeout),  %TODO change to active true and stream dencoding
	case me_logic:do_login(Socket, Login, Password) of
		ok ->
			io:format("Connected~n"),
			{ok, Socket};
		err ->
			ok = gen_tcp:close(Socket),
			timer:sleep(1000),
			io:format("Reconnecting~n"),
			login(Login, Password, Host, Port, Timeout)
	end.