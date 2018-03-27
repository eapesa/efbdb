efbdb
=====

Firebase Realtime Database Client written in Erlang.

`efbdb` uses the concept of server-sent events to get realtime updates from Firebase DB.


Build
-----

To build `efbdb`, run the following on your terminal / command shell

```
$> rebar3 compile
```


Setup
-----

To include `efbdb` in your application, add the following in the `deps` of your
`rebar.config`

```
{efbdb, {git, "https://github.com/eapesa/efbdb.git", {branch, "master"}}}
```


Usage
-----

`efbdb` is an OTP application so it needs to be started either by adding it in
your `.app` file or run the following in your erlang shell:

```
$> application:ensure_all_started(efbdb).
```

To initialize `efbdb`, run the following:

```
$> {ok, Conn} = efbdb:connect(_FIREBASE_HOST_, _FIREBASE_NODE_, _OPTIONS_)
```

`efbdb:connect` has 3 arities.

* `efbdb:connect/1` expects only the firebase host with firebase node defaults to "/".
  Options discussed on the next section.

* `efbdb:connect/2` expects both firebase host and node.

* `efbdb:connect/3` expects the host, node and the options.


efbdb Options
-------------

`efbdb` options is a map that contains any or all of the items below:

* `firebase_secret :: binary` = defaults to blank if not specified

* `update_callback :: {atom, atom}` = callback function to handle additions and modifications on firebase db.
  Defaults to the native handler, `firebasedb:on_update/2`

* `remove_callback {atom, atom}` = callback function to handle deletion on firebase db.
  Defaults to the native handler, `firebasedb:on_remove/1`


Sample Usage
------------

```
$> {ok, Conn} = efbdb:connect("efbdb-1a361.firebaseio.com", "/", #{
      firebase_secret => <<"firebase_secret_key">>,
      update_callback => {firebasedb, on_update},
      remove_callback => {firebasedb, on_remove}
  }).
```
