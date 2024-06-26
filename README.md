Pinobot
=======

This is a NetHack monster information IRC bot.

Bot commands
--------

    NetHack:            @?monster
    NetHack:            @v?monster   (alias for just @?)
    UnNetHack:          @u?monster
    NetHack 3.4.3:      @V?monster
    UnNetHackPlus:      @u+?monster
    SporkHack:          @s?monster
    GruntHack:          @g?monster
    Slash'EM:           @l?monster
    Slash'EM Extended:  @le?monster
    SlashTHEM:          @lt?monster
    NetHack Brass:      @b?monster
    dNetHack:           @d?monster
    notdNethack:        @n?monster
    notnotdNethack:     @nn?monster
    EvilHack:           @e?monster
    XNetHack:           @x?monster
    SpliceHack:         @sp?monster
    Hack'EM:            @h?monster

How to run
----------

Pinobot is made up of two components:

  * `pinobot-frontend` : This part connects to IRC network.
  * `pinobot-monsterdb` : This part connects to `pinobot-frontend`.

This design allows you to independently upgrade `pinobot-monsterdb`, without
needing to disconnect Pinobot from IRC.

There is a file called `pinobot_config.toml` in the root of this repository.
When you run pinobot, it will look for this file to figure out where it should
connect, what nickname it should use, etc.

```shell
# Assuming cabal and ghc are installed.

# Terminal 1
$ cabal new-run pinobot-frontend

# Terminal 2, after `pinobot-frontend` has launched
$ cabal new-run pinobot-monsterdb
```

How to make Pinobot join and leave channels
-------------------------------------------

```shell
cabal new-run pinobot-join '#foobar'
```

```shell
cabal new-run pinobot-part '#foobar'
```

Patch
-----

The NetHack code that was used in the generation of monster data is given as a
patch in patch/unixmain.patch. You need to patch the file sys/unix/unixmain.c
with that patch file.

You don't need to mess with the patch file unless you are a developer and want
to hack with the data for some reason.

