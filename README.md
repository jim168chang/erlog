erlog
=====

Simple Erlang Logging
---------------------

This library is easy to use.

Start by calling

	erlog:start_link("conf/default.conf")

OR

	erlog:start_link()

Then you can call any of the logging functions
	
	log/2, log/3, log/4,
	debug/1, debug/2, debug/3,
	info/1, info/2, info/3,
	warning/1, warning/2, warning/3,
	error/1, error/2, error/3,
	critical/1, critical/2, critical/3,
	alert/1, alert/2, alert/3,
	reload_config/1

See the sample config file for more information
