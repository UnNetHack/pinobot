Pinobot
=======

This is a NetHack monster information IRC bot written by Mikko Juola.

Bot commands
--------

    @?monster

Gives information about some monster. Replace 'monster' with the actual name.
The @? form gives information from UnNetHack.

    @v?monster

@v? prefix is the same as @? but the information comes from vanilla NetHack
3.4.3.

    @u?monster

@u? is the same as @? at the moment.

Configuration
-------------

Edit src/Main.hs. There is a line that looks like this:

    -- Add your channel to the S.fromList part.
    (_, part) <- IRC.initChannelsPart $ S.fromList [""]

Add the channel you want the bot join to the quotes. For example:

    (_, part) <- IRC.initChannelsPart $ S.fromList ["#unnethack"]

Similarly, you can change the IRC server by modifying the source below it. It
should be simple enough that you do not need to know Haskell.


