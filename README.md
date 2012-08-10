docterl
=======

Distributed OCTree in ERLang

Variants:


docterl_ets
-----------

using ETS tables as datastore. Distribution is done via gen_event. Only one tree at a time is supported.

This variant does not offer any detection or handling of node failure or network partition.