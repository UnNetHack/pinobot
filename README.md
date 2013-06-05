Pinobot
=======

This is a NetHack monster information IRC bot written by Mikko Juola.

Bot commands
--------

    UnNetHack:      @?monster
    UnNetHack:      @u?monster
    Vanilla:        @v?monster
    UnNetHackPlus:  @u+?monster
    SporkHack:      @s?monster
    GruntHack:      @g?monster
    Slash'EM:       @l?monster
    NetHack Brass:  @b?monster

Gives information about some monster. Replace 'monster' with the actual monster
name.

Configuration
-------------

Edit src/Main.hs. There is a line that looks like this:

    -- Add your channel to the S.fromList part.
    (_, part) <- IRC.initChannelsPart $ S.fromList [""]

Add the channel you want the bot join to the quotes. For example:

    (_, part) <- IRC.initChannelsPart $ S.fromList ["#unnethack"]

Similarly, you can change the IRC server by modifying the source below it. It
should be simple enough that you do not need to know Haskell.

Patch
-----

The NetHack code that was used in the generation of monster data is given as a
patch in patch/unixmain.patch. You need to patch the file sys/unix/unixmain.c
with that patch file.

You don't need to mess with the patch file unless you are a developer and want
to hack with the data for some reason.

