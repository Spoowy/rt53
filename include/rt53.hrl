-define(RT53_URL, "https://route53.amazonaws.com").
-define(RT53_API, "2012-02-29").
-define(RT53_NS, "https://route53.amazonaws.com/doc/2012-02-29/").
-define(RT53_KEY_VAR, "AWS_ACCESS_KEY_ID").
-define(RT53_SECRET_VAR, "AWS_SECRET_ACCESS_KEY").


-type zone_info() :: [{atom(), [string()]}].

-type zone_list_metadata() :: [{atom(), string()}].
-type hosted_zone_list() :: {zone_list_metadata(), [zone_info()]}.

-type change_info() :: [{atom(), [string()]}].
-type nameservers() :: [string()].
-type new_zone_info() :: {zone_info(), change_info(), nameservers()}.
-type options() :: [{string(), term()}].
