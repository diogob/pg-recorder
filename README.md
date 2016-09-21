# pg-recorder

This will record all messages received from database notifications using user defined functions.

To start recording just connect to the same database where the PostgREST-WS backend is running:

    $ pg-recorder postgres://localhost/target_db -c target_channel
    
This will open a database session listening in the target channel and 
will send every payload received to a user defined function with the signature `target_channel(json)`

## Development

To run the tests using stack:

    $ stack test
