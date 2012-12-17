RT53
====

An Erlang interface to the RT53 Amazon Web Service.

To Use:
-------

  Start the application:

    rt53:start().

  or
 
    rt53:start(Key, Secret).

  If the Key and Secret are not provided to rt53:start, they should be
  set as operating system environment variables with the names:

    AWS_ACCESS_KEY_ID
    AWS_SECRET_ACCESS_KEY

  (These are the same variables the erlcloud package uses.)

  Now you can do whatever:

    {MetaData, Zones} = rt53:list_hosted_zones().
    {ZoneInfo, _, _} = rt53:create_hosted_zone(Name, Comment).
    rt53:delete_hosted_zone(ZoneID).

    {_, Data} = rt53:list_resource_record_sets(ZoneID).
    rt53:change_resource_record_sets(ZoneID, basic, 
      [{create, "a.example.com.", cname, 600, "10.0.0.1"}]).


IMPORTANT NOTES
---------------

This is a partial implementation.  Only "basic" resource record sets
are implemented (they're all I need at the moment).  If you need
weighted, alias, latency, &c. RR sets, drop me a line.

Also, TXT records are poorly handled.  They can be created but parsing
them back out is ... icky.  

AUTHOR
------

Kevin Montuori <montuori@gmail.com> with help from my collegues Mark
Chandler and Jeremy Blacker.  Thanks go to Pearson for allowing this lib
to be made public.

LICENSE
-------

Freely redistributable and without warranty, though please indicate
any changes you've made to the code if you do redistribute.


SAMPLE SHELL SESSION
--------------------

Note that ZoneIDs can be specified as "ABC123" or
"/hostedzone/ABC123".  Likewise, get_change/1 accepts a bare change ID
or one prefixed with "/change/".  

The environment variables 

    AWS_ACCESS_KEY_ID=<key>
    AWS_SECRET_ACCESS_KEY=<secret>

have been set.

    > rt53:start().
    ok
    
    > rt53_auth:credentials().
    {"<key>",
     "<secret>"}
  
    > rt53_auth:authinfo().
    {"AWS3-HTTPS AWSAccessKeyId=<key>,Algorithm=HMACSHA1,Signature=<sig>",
     "Mon, 17 Dec 2012 21:28:40 GMT"}
    
    > rt53:list_hosted_zones().
    {[{marker,[]},
      {is_truncated,["false"]},
      {next_marker,[]},
      {max_items,["100"]}],
     [[{id,["/hostedzone/Z3..."]},
       {name,["quux.example.com."]},
       {caller_reference,["ba6d58a3-..."]},
       {comment,["testing domain"]},
       {resource_record_set_count,["2"]}],
      [{id,["/hostedzone/Z3..."]},
       {name,["bar.example.com."]},
       {caller_reference,["8D86C9A5-..."]},
       {comment,[]},
       {resource_record_set_count,["5"]}],
      [{id,["/hostedzone/Z1..."]},
       {name,["baz.example.com."]},
       {caller_reference,["289E160D-..."]},
       {comment,["a subdomain"]},
       {resource_record_set_count,["26"]}]]}
    
    > rt53:get_hosted_zone("/hostedzone/Z3...").
    {[{id,["/hostedzone/Z3"]},
      {name,["quux.example.com."]},
      {caller_reference,["ba6d58a3-9b84-4acb-93a9-a35591855295"]},
      {comment,["testing domain"]},
      {resource_record_set_count,["2"]}],
     {nameserver,["ns-697.awsdns-23.net","ns-339.awsdns-42.com",
                  "ns-1579.awsdns-05.co.uk","ns-1520.awsdns-62.org"]}}
    
    > rt53:create_hosted_zone("demo.example.com", "A Demonstration Subdomain").
    {[{id,["/hostedzone/Z2..."]},
      {name,["demo.example.com."]},
      {caller_reference,["f620e4a5-...]},
      {comment,["A Demonstration Subdomain"]},
      {resource_record_set_count,["2"]}],
     [{id,["/change/C2..."]},
      {status,["PENDING"]},
      {submitted_at,["2012-12-17T21:15:35.368Z"]}],
     ["ns-544.awsdns-04.net","ns-1156.awsdns-16.org",
      "ns-1938.awsdns-50.co.uk","ns-364.awsdns-45.com"]}
    
    > rt53:get_change("/change/C2...").
    [{id,["/change/C2..."]},
     {status,["INSYNC"]},
     {submitted_at,["2012-12-17T21:15:35.368Z"]}]
    
    > rt53:delete_hosted_zone("Z2...").
    [{id,["/change/C3..."]},
     {status,["PENDING"]},
     {submitted_at,["2012-12-17T21:17:31.330Z"]}]
    
    > rt53:list_resource_record_sets("/hostedzone/Z3...").
    {[{is_truncated,["false"]},
      {max_items,["100"]},
      {next_record_name,[]},
      {next_record_type,[]},
      {next_record_identifier,[]}],
     [[{name,["quux.example.com."]},
       {type,["SOA"]},
       {ttl,["900"]},
       {value,["ns-697.awsdns-23.net. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}],
      [{name,["quux.example.com."]},
       {type,["NS"]},
       {ttl,["172800"]},
       {value,["ns-697.awsdns-23.net.","ns-339.awsdns-42.com.",
               "ns-1579.awsdns-05.co.uk.","ns-1520.awsdns-62.org."]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}]]}
    
    > rt53:change_resource_record_sets("Z3...", basic, 
    >  [{create, "a.quux.example.com", cname, 600, "10.0.0.1"},
    >   {create, "b.quux.example.com", cname, 600, "10.0.0.2"}], 
    >  "An example comment").
    [{id,["/change/C3M..."]},
     {status,["PENDING"]},
     {submitted_at,["2012-12-17T21:21:32.479Z"]}]
    
    > rt53:list_resource_record_sets("/hostedzone/Z3...").
    {[{is_truncated,["false"]},
      {max_items,["100"]},
      {next_record_name,[]},
      {next_record_type,[]},
      {next_record_identifier,[]}],
     [[{name,["b.quux.example.com."]},
       {type,["CNAME"]},
       {ttl,["600"]},
       {value,["10.0.0.2"]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}],
      [{name,["a.quux.example.com."]},
       {type,["CNAME"]},
       {ttl,["600"]},
       {value,["10.0.0.1"]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}],
      [{name,["quux.example.com."]},
       {type,["SOA"]},
       {ttl,["900"]},
       {value,["ns-697.awsdns-23.net. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}],
      [{name,["quux.example.com."]},
       {type,["NS"]},
       {ttl,["172800"]},
       {value,["ns-697.awsdns-23.net.","ns-339.awsdns-42.com.",
               "ns-1579.awsdns-05.co.uk.","ns-1520.awsdns-62.org."]},
       {alias_target,[]},
       {hosted_zone_id,[]},
       {d_n_s_name,[]},
       {set_identifier,[]},
       {weight,[]},
       {region,[]}]]}
    
    > rt53:change_resource_record_sets("Z3...", basic,    
    >  [{delete, "a.quux.example.com", cname, 600, "10.0.0.1"},
    >   {delete, "b.quux.example.com", cname, 600, "10.0.0.2"}],
    >  "Removing CNAMEs").
    [{id,["/change/C2..."]},
     {status,["PENDING"]},
     {submitted_at,["2012-12-17T21:23:43.082Z"]}]
