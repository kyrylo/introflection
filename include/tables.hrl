-type node_list() :: [node()].

-record(introflection_events, {id={now(), node()},
                               type :: non_neg_integer(),
                               ref  :: any()}).
