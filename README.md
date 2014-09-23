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
    Slash'EM extended:  @le?monster
    NetHack Brass:      @b?monster
    dNetHack:           @d?monster

Gives information about some monster. Replace 'monster' with the actual monster
name.

### Code commands:

Code commands let you run Haskell code.

    @>code                   Compile and run 'code'. E.g. @>5+5
    @>%fun x y z = x+y+z     Define a function named 'fun'.
    @>::fun :: a -> b -> c   Define a type for a function named 'fun'.
                             E.g. @>::fac :: Integer -> Integer
    @>!fun                   Forget function and type definitions for 'fun'.
    @>?fun                   Show definitions of 'fun'.

All sorts of Haskell libraries are available in the code as well as some
Pinobot code (notably all the monster data). See the file
arbicode-sandbox/PinobotSafe.hs for all the exports.

Examples:

    @>allMonsterNames Vanilla.variant
    Lists all monster names in NetHack 3.4.3

    @>monster UnNetHack.variant "giant ant"
    Return a monster value for "giant ant" from UnNetHack.

How to run
----------

The bot is composed of two operating system processes, pinobot-frontend and
pinobot-monsterdb. pinobot-frontend talks to the IRC server and has direct
communication with it and pinobot-monsterdb does all the logic, parsing and
handing out monster information. The idea is that Pinobot can be upgraded by
stopping pinobot-monsterdb and running an updated version of it.

Getting this thing to run should be as simple as just running pinobot-frontend
first and then pinobot-monsterdb.

Code execution interface
------------------------

There is also an executable called pinobot-arbicode in this package. This
module implements an interface that lets users define and run small snippets
(one-lines) of Haskell code. If you run it, like you would run
pinobot-monsterdb, the interface is presented to users.

However, for this to work correctly, some setting up is needed.

Create a directory 'arbicode-sandbox-bin' in the working directory of
pinobot-arbicode. Put the Glasgow Haskell Compile 'ghc' (as a symlink or
otherwise) inside this directory and make sure it is executable.

Next, make a symlink or copy lib/NetHack to arbicode-sandbox/NetHack. This is
needed to export Pinobot's NetHack modules to the code execution interface.

Now you should be able to run pinobot-arbicode. However, the first time you try
to compile something from IRC, the compilation may time out because all the
variant data is getting compiled. You can manually run ghc to make the first
compilation.

    cd arbicode-sandbox
    ghc Env.hs

Patch
-----

The NetHack code that was used in the generation of monster data is given as a
patch in patch/unixmain.patch. You need to patch the file sys/unix/unixmain.c
with that patch file.

You don't need to mess with the patch file unless you are a developer and want
to hack with the data for some reason.

