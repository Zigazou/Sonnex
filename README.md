Sonnex
======

Sonnex is an alternative to Soundex for french language

The string must contain only one word.
The Sonnex code contains the following characters:
    1 ← un, ein, in, ain
    2 ← en, an
    3 ← on
    a ← a, à, â
    b ← b, bb
    C ← ch
    d ← d, dd
    e ← e, eu
    é ← ê, é, è, ai, ei
    f ← f, ff, ph
    g ← gu
    i ← î, i, ille
    j ← j, ge
    k ← k, c, qu, ck
    l ← l, ll
    m ← m, mm
    n ← n, nn
    o ← o, ô
    p ← p, pp
    r ← r, rr
    s ← s, ss
    t ← t, tt
    u ← u, ù, û
    v ← v, w
    z ← z, s
    U ← ou

Examples
--------

Here are a few examples of sonnex results:

- balade | ballade → balad
- basilic | basilique → bazilik
- boulot | bouleau → bUlo
- cane | canne → kan
- censé | sensé → s2sé
- compte | comte | conte → k3t
- cygne | signe → sin
- date | datte → dat
- dessin | dessein → dés1
- différend | différent → difér2
- cric | crique → krik
- champ | chant → C2

Testing
-------

The `test` directory contains two files:

- `test-sonnex-homonymes.txt`: a list of french homonyms
- `test-sonnex-streets.txt`: a list of streets from Rouen (Normandy)

You can try these files against Sonnex by using the following command:

   cat test-sonnex-homonymes.txt | runhaskell test-sonnex.hs 


