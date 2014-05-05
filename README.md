This repository contains code for playing the tabletop game "Dominion" on the
command-line with multiple players.

QUICKSTART

To choose which/how many people to play with and which cards to play with,
modify Config.hs. Then, starting at the root of the repository, you can run the
dominion server with:

    runhaskell Main.hs

When you run the server, it will tell you its server ID. Then, each player
(from the same machine) can connect to the server by running:

    ./client.sh serverID name

where name is their entry in the userNames list in Config.hs. Once everyone is
connect, the game will begin; you can type /help for a list of special
commands.
