FSharp.Control.RabbitMQ  (working title: BigWig)
================================================

An AQMP implementation is based on messages.  These messages are both thought out and extensible.
It appears that a message based way of using Rabbit MQ would be beneficial.

For now we piggyback off of the C# RabbitMQ connector.  Replacing the RabbitMQ connector is 
currently out of scope.  However, this should allow us to have a RabbitMQ friendly way 
of configuring queues and queue topologies.

Beyond this, there are several useful things that could be done with plugins like "management" 
and "Shovel".  The tool of choice is the idiom of exploratory programming through type providers.


Status
------

Experimental


To Do
-----

* push definitions into the appropriate C# connector methods

* create queues, subscribe, and publish examples

* stabilise configuration form (decorator / file / programatic).  
 

Maintainer(s)
-------------

- [@sgtz](https://github.com/sgtz)

The default maintainer account for projects under "fsprojects" is [@fsgit](https://github.com/fsgit) - F# Community Project Incubation Space (repo management)


