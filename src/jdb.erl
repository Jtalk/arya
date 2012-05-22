-module(jdb).

-export( [ appenv/3, getenv/2, report/2, report/3, configure/0]).

appenv( Name, Function, ErrorMessage) ->
	case application:get_env( Name) of
		{ ok, Value} ->
			Function(Value);
		Error ->
			report( 0, ErrorMessage, Error),
			Error
	end.
	
getenv( Name, ErrorMessage) ->
	appenv( Name, fun( Value) -> Value end, ErrorMessage).

report( Level, Report) ->
	{ ok,Default} = application:get_env( loglevel),
	if 
		Level == 0 ->
			error_logger:format( Report, self());
		Level =< Default ->
			error_logger:info_msg( Report, self());
		true ->
			false
	end.
	
report( Level, Report, Data) ->
	{ ok,Default} = application:get_env( loglevel),
	if 
		Level == 0 ->
			error_logger:format( Report, { self(), Data});
		Level =< Default ->
			error_logger:info_msg( Report, { self(), Data}),
			Data;
		true ->
			Data
	end.
		
configure() ->
	appenv( logfile, fun error_logger:logfile/1, "Unable to create config file"),
	appenv( is_tty, fun error_logger:tty/1, "Unable to read tty configuration").
	
	
	
	
	
	
	
	
	
	
	
	
	
	