Pinobot
=======

This is a NetHack monster information IRC bot written by Mikko Juola.

Bot commands
--------

    UnNetHack:          @?monster
    UnNetHack:          @u?monster
    Vanilla:            @v?monster
    Leaked Vanilla:     @V?monster
    UnNetHackPlus:      @u+?monster
    SporkHack:          @s?monster
    GruntHack:          @g?monster
    Slash'EM:           @l?monster
    Slash'EM Extended:  @le?monster
    SlashTHEM:          @lt?monster
    NetHack Brass:      @b?monster
    dNetHack:           @d?monster

Gives information about some monster. Replace 'monster' with the actual monster
name.

How to run
----------

The bot is composed of two operating system processes, pinobot-frontend and
pinobot-monsterdb. pinobot-frontend talks to the IRC server and has direct
communication with it and pinobot-monsterdb does all the logic, parsing and
handing out monster information. The idea is that Pinobot can be upgraded by
stopping pinobot-monsterdb and running an updated version of it.

Getting this thing to run should be as simple as just running pinobot-frontend
first and then pinobot-monsterdb.

Patch
-----

The NetHack code that was used in the generation of monster data is given as a
patch in patch/unixmain.patch. You need to patch the file sys/unix/unixmain.c
with that patch file.

You don't need to mess with the patch file unless you are a developer and want
to hack with the data for some reason.

