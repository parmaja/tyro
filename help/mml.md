# Music Macro Language (MML) - A Fun Way to Make Music!

MML (Music Macro Language) is a simple way to write music as text! In Tyro, you can play MML tunes using `music.mml()` to make your games sound awesome. 

## How to Use MML in Tyro

In your Tyro code, you can play MML like this: 

```tyro
music.mml("c d e f g a b c5")
```

You can also give multiple parts (channels) at the same time! 

```tyro
music.mml("cdefg", "efgfe")
```

Each string becomes a different music channel that plays together! 

---

## Basic Notes

MML uses letters to represent musical notes, just like in music class! 

| Letter | Note |
| --- | --- |
| `c` | C (Do) |
| `d` | D (Re) |
| `e` | E (Mi) |
| `f` | F (Fa) |
| `g` | G (So) |
| `a` | A (La) |
| `b` | B (Ti) |
| `r` or `p` | Rest (Silence!) |

### Sharps (#)

Want a sharp note? Just add `#` after the letter! 

```tyro
music.mml("c c# d d# e")
```

This plays C, C-sharp, D, D-sharp, E!

---

## Note Lengths (How Long to Hold Notes)

By default, notes last for 1/4 (quarter note). But you can tell MML how long to hold each note by adding a number after it!

| Number | Length | Like a... |
| --- | --- | --- |
| `1` | Whole note | Very long! |
| `2` | Half note | Long |
| `4` | Quarter note | Normal (default) |
| `8` | Eighth note | Short |
| `16` | Sixteenth note | Very short |

**Examples:**

```tyro
music.mml("c4 d4 e4 f4")  -- 4 means quarter notes
music.mml("c2 d2")        -- Half notes
music.mml("c8 c8 d8 d8")  -- Eighth notes (like a fast march!)
```

### Dotted Notes (Extra Long!)

Add a dot `.` after a note to make it half again as long! You can even add more dots for even more length!

```tyro
music.mml("c4. d8 e4")  -- C is 1.5x longer than a quarter note
music.mml("c2..")        -- Super duper long dotted note!
```

---

## Octaves (High and Low Sounds)

Notes can be high or low. We control this with the octave command `o` and with `<` / `>` to go up or down!

| Command | What it does |
| --- | --- |
| `o4` | Set octave 4 (middle - this is the default!) |
| `>` | Go up to the next higher octave |
| `<` | Go down to the next lower octave |
| `+` | Make just this one note go up an octave |
| `-` | Make just this one note go down an octave |

**Examples:**

```tyro
music.mml("c4 d4 e4 o5 c4")  -- Goes up to higher octave
music.mml("c5 < c4 < c3")   -- Going down octaves
music.mml("c4 c+4 c4")       -- Only middle C goes up
```

---

## Tempo (How Fast or Slow)

Want your song to go faster or slower? Use the `t` command (tempo in beats per minute)!

```tyro
music.mml("t60 c4 d4 e4 f4")  -- Slow and chill (60 BPM)
music.mml("t120 c4 d4 e4 f4") -- Normal speed (120 BPM)
music.mml("t180 c4 d4 e4 f4") -- Super fast! (180 BPM)
```

The default tempo is 60 BPM. You can go from 1 up to 2000 BPM!

---

## Note Length Shortcut (l)

If you want all notes after to have the same length, use `l` to set the default length!

```tyro
music.mml("l8 c d e f g a b")  -- All are 8th notes!
music.mml("l4 c d l8 e f")     -- Back to quarter, then 8th notes
```

---

## Volume (How Loud!)

Use `v` to change the volume! Higher numbers are louder! 

```tyro
music.mml("v50 c4 d4 e4")   -- Quiet
music.mml("v100 c4 d4 e4")  -- Normal
music.mml("v200 c4 d4 e4")  -- Loud!
```

---

## Articulation (Smooth vs Bouncy Notes)

You can control how notes connect with the `m` (mode) commands! 

| Command | Name | What it sounds like |
| --- | --- | --- |
| `ml` | Legato | Notes flow smoothly together! |
| `mn` | Normal | Nice balanced notes (default) |
| `ms` | Staccato | Short and bouncy, like a robot! |

**Example:**

```tyro
music.mml("ml c4 d4 e4")  -- Smooth and connected
music.mml("ms c4 d4 e4")  -- Bouncy and short
```

---

## Other Cool Commands

| Command | What it does | Example |
| --- | --- | --- |
| `!` | Reset everything to defaults! | `c4 ! d4` |
| `;` | Stop the music here | `"c4 d4 e4;"` |
| `#` | Comment - anything after # is ignored | `"# This is my song! c4 d4"` |
| `i[Name]` | Pick an instrument! | `i[GrandPiano] c4 d4` |
| `n` | Play note by number (0-127) | `n60 n62 n64` |
| `q` | Play a specific frequency (Hz) | `q440 q523` (A and C) |
| `s` | Shift octave (can reset with `s0`) | `s2 c4` or `s0 c4` |
| `&` | Tie notes together | `c4 & c4` (connected notes) |

---

## Fun Examples for Kids!

### Tiny Scale
```tyro
music.mml("c4 d4 e4 f4 g4 a4 b4 c5")
```

### Super Simple Melody
```tyro
music.mml("l8 e e r e r c e g g2")
```

### Going Up and Down
```tyro
music.mml("c4 d4 e4 f4 g4 f4 e4 d4 c2")
```

### Fast and Bouncy!
```tyro
music.mml("t180 ms l8 c c d c g3")
```

### Two Part Harmony!
```tyro
music.mml("c4 e4 g4 e4 c4", "e4 g4 c5 g4 e4")
```

### Happy Little Tune
```tyro
music.mml("t120 l4 c4 d4 e4 c4 e4 g2 g2")
```

---

## Tips!

- Spaces don't matter! You can space things out to make it easier to read!
- Use rests `r` or `p` to give your music some breathing room!
- Try combining tempo, length, and articulation to create your own unique sound!
- Look at the examples in the `demos/` folder! `music.tyro` has a huge song (William Tell) and `music_demo.tyro` has simple ones!
- If you see `,` used in multi-channel songs in the demos, Tyro passes them as separate strings to `music.mml()` - like in `music.mml(part1, part2)`!

Have fun making music!