# OoDb

---

## The Problem
- SQL databases seem like they are more meant for writing queries by hand than being used cleanly within a program
- You have to choose between
  - Writing all sorts of queries and doing terrible casting by hand
  - Using a library that does a lot of stuff for you
    - Adds its own layer of configuration complexity to make it fit your database
    - If that database changes, you have to refactor all of this to match
    - It can be difficult/impossible to make these wrappers correctly model your tables
    - The only wrapper (at least that Ive worked with) that gets remotely close, is ruby's ActiveRecord
      - Only works for ruby
      - Has problems with windows
      - Is HEAVILY integrated with RAILS

---

## General Idea
- Write a file that specifies what your database should look like, as well as "endpoints" to access data (most of this is handled by default, with optional tweaks you can make)
- Run the command on it, and it generates a schema
- It uses the schema to generate tables and classes for the database
  - If you need to update your database, create a migration file, which will update the schema, as well as the tables/classes
- Run the server
- The server uses its own networking protocol (ideally I would like to make this encrypted), and "queries" are specified in a much more User.fromId(...) format
- The server actually runs on 2 ports (as of my idea atm), one with endpoints, and one that serves the schema
- A user connects to the schema port, and downloads the schema
- After downloading the schema, use the tool to turn the schema file into classes to interact with your unique database `db.getUser(id)`, `myUser.relatedTableEntry.edit.setField(x).save`
- Under the table, OoDb is caching objects and sending requests to the server to make sure your objects are up to date
  - `x = db.getUser(id); y = db.getUser(id); x.edit.setFirstName("ex").save`, this change reflects in both x and y, as they are actually the same object
  - The cache has some sort of `validUntil`, where it will make sure to update objects every so often (configurable)
- Requests to the server require sending (some sort of hash or token) for your schema, to make sure the clients schema is up to date
  - Possible optimization down the line is allowing out of date schemas, but they are only allowed to use part of the schema that is correct
- OoDb is written in scala, and the generated classes from schema will also be in scala first, but all (me or someone else) needs to do is write a `schema -> client side classes / db interaction` converter, and everything will work in that language (for every database to come)

---

## Some miscellaneous notes about optimizations
- If you know you will will want, say: A `User`, and their `Accounts`, it it much more efficient to do this all in one query
  - `db.getUser(id, accounts = true)` (the syntax there is definitely going to be different, and TBD), which will automatically grab the user, and all their accounts, and cache them locally
- Indexes are built into the table itself
  - Example of what a row might look like in memory: `id, firstNameStrRef, lastNameStrRef, accountsListRef`
  - accountsListRef points to a list in memory that basically does a "join" automatically for you, without having to do any lookup for you in a separate index
  - Guess what, say `Account` had a `comments`, and you wanted those too? When you look at the account that you had a reference to (without any additional searching), you get a reference to those comments too (without any additional searching)
- Maybe not as a first version thing, but I would like the db to also be able to handle tokens/security
  - You have some sort of `User.login(user, pass)` endpoint, which gives you a token
  - You can specify that queries to any table which is related to a user requires a `User` token to gain access to
  - Also would like to customize this, so you could also have something like allowing admins access to everything (you would need to still send a token, but the token associated with that user is an admin)
- Very TDB idea: schema could also generate an API HTTP server that interacts with the database, based on the endpoints specified
- 99.99999% (possibly 100%) of inserts into a pk_index are probably just adding the next `AUTO_INCREMENT`ed id, so a B+ Tree optimization on that, were it stores the right-most path, and only 1 read/write is necessary (excluding required splits)
- There will obviously need to be multi-threading involved, and configuration on how that happens