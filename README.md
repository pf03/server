# SERVER
*** 
## ORDER OF LAUNCH AND TESTING (tested only for Windows)

1.	Run commands:
   * git clone https://github.com/pf03/server.git
   * cd server
   * stack build
3. To run the binary file successfully, copy the following files and folders from repository into the same folder with the binary file:
* curl
* migrations
* photos
* config-example.json
3. Rename the config-example.json file to config.json and, if necessary, edit it
4. To initialize DB tables and bring them up to date, start the server with the `db-init` flag. The rest of the command line options that might be useful for testing are listed below.
3. To update the token file `dist/curl/tokens.sh`, start the server with the `gen-tokens` flag
4. API functions could be tested using `curl` requests in the corresponding folder
5. To test pure functions run `stack test`
6. Rules for registration of migrations:
* file names are in the strict `1234_migration_name.sql` format and in strict order, starting at `0000_migration_name.sql`;
* the `drop.sql` file is used to drop tables;
* files with other names are ignored.

For migrations test:
* drop the database using the db-drop command,
* delete (rename) part of the migration files,
* initialize the tables and apply the remaining migrations  in the migration folder with the `db-init` command,
* return migrations to the migrations folder
* apply all migrations using `migrations` command
***
## COMMAND LINE ARGUMENTS

#### STARTING THE SERVER:
  
* start with no arguments - start the server

#### MIGRATION:
  
* `db-init`         - applying migrations to the local database, starting with zero migration
* `migrations`      - applying migrations to the local database, starting with the first not applied migration(the one that is not in the migrations database table)
* `db-drop`         - drop all database tables
* `db-restart`      – `db-drop` + `db-init`
* `db-restart-force`- force `db-restart`

#### CURL:
  
* `gen-tokens`      - update token file `dist/curl/tokens.sh`

***
## MODULES

The server operation logic is divided into the following layers (presented in the corresponding folders in the `src`) from low to high:
1. Common       - common functions;
2. Interface    - classes of types that implement abstract access of higher layers to interfaces:
  * MError  - error handling,
  * MLog    - logging,
  * MCache  - working with changing data in pure code,
  * MDB     - work with PostgreSQL database;
3. Logic        - the main logic of the program, for convenience it is divided into:
  * Pure    - pure functions,
  * IO      - IO functions,
  * DB      - functions for working with the database;
4. T        - one of the possible implementations of the interface - transformer T;
5. App      - application layer functions that have access to both the interface and its implementation.

Lower layers should not import modules from higher layers.

## API

The server supports the following api functions (paths). Some paths are available only for admin (marked with ![](https://via.placeholder.com/15/ff0000/000000?text=+)). In this case, other users receive an `Unknown path` error. Some paths are available only for authorized users (![](https://via.placeholder.com/15/0000ff/000000?text=+)) or for all users (![](https://via.placeholder.com/15/00ff00/000000?text=+)). For testing each path, there is a separate file in the `curl` folder. The requests in the file are duplicated for different tokens (users). The `posts` request is tested with different filters.
All possible paths are written in the `Logic.Pure.API.router` function. All possible parameters for each path are written in the` Logic.Pure.Params.Internal.possibleParamDescs` function. All requests for creating, editing, deleting entities and uploading photos return the number of changed entities.

#### Paths list:

1.  Login  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `/login` - login
2. Files  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `/photos/upload` - upload photo, for all;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `photos/fileName` - download photo by `fileName`;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `photos` - returns list of photos filenames;  
3. DB  
* Insert  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `users/create` - create a user;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `authors/create` - create an author;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `categories/create` - create a category;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `tags/create` - create a tag;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `drafts/create` - create a draft;  
![](https://via.placeholder.com/15/ff0000/000000?text=+)  `drafts/n/publish` - publish a draft (delete draft and create post);  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `posts/n/comments/create` - create a comment to a post with `id = n`;  
* Update  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `users/n/edit` - edit user with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `user/edit` - edit current user;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `authors/n/edit` - edit author with `id = n`;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `categories/n/edit` - edit category with `id = n`;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `tags/n/edit` - edit tag with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `drafts/n/edit` - edit draft with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `posts/n/edit` - edit post with `id = n`;  
* Delete  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `users/n/delete` - delete user with `id = n`;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `authors/n/delete` - delete author with `id = n`;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `categories/n/delete` - delete category with `id = n`;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `tags/n/delete` - delete tag with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `drafts/n/delete` - delete draft with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `posts/n/delete` - delete post with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `comments/n/delete` - delete comment with `id = n`;  
* Select many  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `users` - select many users with pagination;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `authors` - select many authors with pagination;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `categories` - select many categories with pagination;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `tags` - select many tags with pagination;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `posts` - select many posts with pagination and filters;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `drafts` - select many drafts with pagination;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `posts/n/comments` - select many comments with pagination for post with `id = n`;  
* Select by id  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `users/n` - select user with `id = n`;  
![](https://via.placeholder.com/15/0000ff/000000?text=+) `user` - select current user;  
![](https://via.placeholder.com/15/ff0000/000000?text=+) `authors/n` - select author with `id = n`;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `categories/n` - select category with `id = n`;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `tags/n` - select tag with `id = n`;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `posts/n` - select post with `id = n`;  
![](https://via.placeholder.com/15/00ff00/000000?text=+)  `drafts/n` - select draft with `id = n`.  
