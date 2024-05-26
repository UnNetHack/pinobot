# Known issues and things to follow up.

In DNethack-based variants, gargoyles may infer petrification resistance, but
Pinobot does not pick this up.

Pinobot may produce lines that are longer than 512 characters when taking into
account the entire raw IRC line sent to the server (limit is set by IRC RFC).
Lines might get cut off for particularly long monster names.

Latest Vanilla update (2024-05-26) has some suspect flag changes that suggests
there might have been a bug, or upstream made a mistake. Example: jellyfish
lost "passes bars" flag.

In DNethack, Mahadeva is not supposed to generate in "gehennom, dungeons" but
actually generates on the Astral Planes only.
