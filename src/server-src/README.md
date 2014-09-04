Adding a service to the server is not really complicated, but you must work on
the .init version of main.ml and server.conf for permanent changes, that are
used by the config to generate the correct main.ml and server.conf (otherwise,
your changes will be overwritten at the next *make config*).

In **main.ml**, services are referenced by the *my_factory* variable. Services are
referenced by a (string, dynamic_service) tuple. Creating a new service can be
done easily by reusing *empty_dyn_service* and redefining dyn_handler. 
The dyn_hanlder is a function that takes an environment and a cgi, which will be
uses to get arguments and return the service response.
When a service has been defined, add it to my_factory with the name you want
(as the first argument of the couple).

In server.conf are defined URIs to access those services. Simply add a "uri"
record, with path to access the service and as handler the name you gave to
my_factory (tldr : copy one the service and just replace *path* and *handler*).

