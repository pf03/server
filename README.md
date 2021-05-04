# server

h1 ORDER OF LAUNCH AND TESTNG (tested only for Windows)

1.	Run commands:
    git clone https://github.com/pf03/server.git
    cd server
    stack build
2. Move the resulting binary file (in the case of Windows server-exe.exe) to the ‘dist’ folder of the repository, which contains all the necessary additional files
3. Rename the config-example.json file to config.json and, if necessary, edit it
4. To initialize DB tables and bring them up to date, start the server with the ‘db-init’ flag. The rest of the command line options that might be useful for testing are listed below.
3. To update the token file ‘dist/curl/tokens.sh’, start the server with the ‘gen-tokens’ flag
4. API  functions could be tested using ‘curl’ requests in the corresponding folder
5. To test pure functions run ‘stack test’
6. Rules for registration of migrations:
    - file names are in the strict ‘1234_migration_name.sql’ format and in strict order, starting at ‘0000_migration_name.sql’;
    - the ‘drop.sql’ file is used to drop tables;
    - files with other names are ignored.
For migrations test:
    - drop the database using the db-drop command,
    - delete (rename) part of the migration files,
    - initialize the tables and apply the remaining migrations  in the migration folder with the ‘db-init’ command,
    - return migrations to the migrations folder
    - apply all migrations using ‘migrations’ command
***
COMMAND LINE ARGUMENTS

STARTING THE SERVER:
    - start with no arguments - start the server

MIGRATION:
    - ‘db-init’         - applying migrations to the local database, starting with zero migration
    - ‘migrations’      - applying migrations to the local database, starting with the first not applied migration(the one that is not in the migrations database table)
    - ‘db-drop’         - drop all database tables
    - ‘db-restart’      – ‘db-drop’ + ‘db-init’
    - ‘db-restart-force’- force ‘db-restart’
CURL:
    - ‘gen-tokens’      - update token file ‘dist/curl/tokens.sh’

***
MODULES

The server operation logic is divided into the following layers (presented in the corresponding folders in the ‘src’) from low to high:
1. Common       - common functions;
2. Interface    - classes of types that implement abstract access of higher layers to interfaces:
    2.1 MError  - error handling,
    2.2.MLog    - logging,
    2.3 MCache  - working with changing data in pure code,
    2.4 MDB     - work with PostgreSQL database;
3. Logic        - the main logic of the program, for convenience it is divided into:
    3.1 Pure    - pure functions,
    3.2 IO      - IO functions,
    3.3 DB      - functions for working with the database;
4. T            - one of the possible implementations of the interface - transformer T;
5. App          - application layer functions that have access to both the interface 
                  and its implementation.

* Lower layers should not import modules from higher layers.

