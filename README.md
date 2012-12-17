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
