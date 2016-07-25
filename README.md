# act-recorder

This will record all messages received from the Act frontend middleware
to a PostgreSQL table that logs every UI event.

To start recording just connect to the same database where the PostgREST-WS backend is running:

    $ act-recorder postgres://localhost/target_db
    
This will open a database session listening in the same channel used byt Act frontend and 
will store every payload received in an appropriate table using the user identifier 
provided by the JWT.

## Development

To run the tests using stack:

    $ stack test act-recorder
