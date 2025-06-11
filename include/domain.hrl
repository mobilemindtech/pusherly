
-record(user, {
		id :: string(),
		name :: string(),
		email :: string(),
		password_hash :: string(),
		type :: atom(),
		api_key :: string(),
		created_at :: calendar:datetime()
		}).


-record(channel, {
	id :: string(), 
	user_id :: string(),
	name :: string(), 
	description :: string(), 
	created_at :: calendar:datetime()
	}).

-type channel():: #channel{}.

-record(app,{
		id :: string(),
		name :: string(),
		user_id :: string(),
		channels :: list(channel()),
		firebase_key :: string(),
		apple_key :: string(),
		created_at :: calendar:datetime()
		}).

-record(session, {
		session_id :: string(),
		user_id :: string(),
		created_at :: calendar:datetime(),
		expires_at :: calendar:datetime()
		}).