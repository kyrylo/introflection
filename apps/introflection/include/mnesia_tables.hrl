-record(introflection_modules, {object_id :: pos_integer(),
                                name      :: string(),
                                nesting   :: non_neg_integer(),
                                parent    :: pos_integer()}).

-record(introflection_events, {id={now(), node()},
                               type :: non_neg_integer(),
                               ref  :: any()}).
