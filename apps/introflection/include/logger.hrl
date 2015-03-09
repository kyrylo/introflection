-define(DEBUG(Format, Data), lager:debug(Format, Data)).
-define(DEBUG(Format), lager:debug(Format)).

-define(INFO(Format, Data), lager:info(Format, Data)).
-define(INFO(Format), lager:info(Format)).

-define(WARN(Format, Data), lager:warning(Format, Data)).
-define(WARN(Format), lager:warning(Format)).

-define(ERROR(Format, Data), lager:error(Format, Data)).
-define(ERROR(Format), lager:error(Format)).
