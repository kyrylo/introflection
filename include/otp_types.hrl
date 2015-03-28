%% ===================================================================
%% Shorter definitions for gen_server and supervisor callback types.
%% Just to make dialyzer STFU.
%% ===================================================================

-type gs_args() :: term().
-type gs_state() :: term().
-type gs_reason() :: term().
-type gs_error() :: {already_started, pid} | term().

-type gs_start_link_reply() :: {ok, pid()} | ignore | {error, gs_error()}.

-type gs_init_reply() ::
        {ok, gs_state()}
      | {ok, gs_state(), timeout() | hibernate}
      | {stop, gs_reason()} | ignore.

-type gs_request() :: term().
-type gs_from() :: {pid(), term()}.
-type gs_reply() :: term().

-type gs_call_reply() ::
        {reply, gs_reply(), gs_state()}
      | {reply, gs_reply(), gs_state(), timeout() | hibernate}
      | {noreply, gs_state()}
      | {noreply, gs_state(), timeout() | hibernate}
      | {stop, gs_reason(), gs_reply(), gs_state()}
      | {stop, gs_reason(), gs_state()}.

-type gs_cast_reply() ::
        {noreply, gs_state()}
      | {noreply, gs_state(), timeout() | hibernate}
      | {stop, gs_reason(), gs_state()}.

-type gs_info_reply() ::
        {noreply, gs_state()}
      | {noreply, gs_state(), timeout() | hibernate}
      | {stop, gs_reason(), gs_state()}.

-type gs_terminate_reason() :: normal | shutdown | {shutdown, term()} | term().

-type gs_code_change_reply() :: {ok, gs_state()} | {error, gs_reason()}.

%% ===================================================================
%% Shorter definitions for gen_event
%% ===================================================================

-type ge_state() :: term().
-type ge_reason() :: term().
-type ge_event() :: term().
-type ge_request() :: term().

-type ge_args() :: term() | {term(), term()}.

-type ge_init_reply() ::
        {ok, ge_state()}
      | {ok, ge_state(), hibernate}
      | {error, ge_reason()}.

-type ge_event_reply() ::
        {ok, ge_state()}
      | {ok, ge_info_reply(), hibernate}
      | {swap_handler, term(), ge_state(), atom() | {atom(), term()}, term()}
      | remove_handler.

-type ge_call_reply() ::
        {ok, term(), ge_state()}
      | {ok, term(), ge_state(), hibernate}
      | {swap_handler, term(), term(), ge_state(), atom() | {atom(), term()}, term()}
      | {remove_handler, term()}.

-type ge_info_reply() ::
        {ok, term()}
      | {ok, term(), hibernate}
      | {swap_handler, term(), term(), atom() | {atom(), term()}, term()}
      | remove_handler.

-type(ge_code_change_reply() :: {ok, ge_state()}).

-type ge_terminate_reason() :: {stop, term()} | stop | remove_handler | term().

%% ===================================================================
%% Shorter definitions for application
%% ===================================================================

-type app_start_type() :: normal | {takeover, node()} | {failover, node()}.

-type app_start_args() :: term().

-type app_start_reply() :: {error, term()} | {ok, pid()}.

-type app_stop_state() :: term().

%% ===================================================================
%% Shorter definitions for supervisor
%% ===================================================================

-type sup_start_link_err() ::
        {already_started, pid()}
      | {shutdown, term()}
      | term().

-type sup_start_link_reply() ::
        {ok, pid()}
      | ignore
      | {error, sup_start_link_err()}.

-type sup_args() :: term().

-type sup_strat() ::
        one_for_all
      | one_for_one
      | rest_for_one
      | simple_one_for_one.

-type sup_child_spec() ::
        {term(),
         {module(), atom(), [term()] | undefined},
         permanent | transient | temporary,
         brutal_kill | timeout(),
         worker | supervisor,
         [module()] | dynamic}.

-type sup_init_reply() ::
        {ok, {{sup_strat(), non_neg_integer(), non_neg_integer()},
              [sup_child_spec()]}}.
      % | ignore.

-type sup_start_child_sup_ref() ::
        atom()
      | {atom(), node()}
      | {global, atom()}
      | {via, module(), any()}
      | pid().

-type sup_start_child_child() :: undefined | pid().

-type sup_start_child_err() ::
        already_present
      | {already_started, sup_start_child_child()}
      | term().

-type sup_start_child_reply()  ::
        {ok, sup_start_child_child()}
      | {ok, sup_start_child_child(), term()}
      | {error, sup_start_child_err()}.

%% ===================================================================
%% Other definitions
%% ===================================================================

-type socket() :: port().
