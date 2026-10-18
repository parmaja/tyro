# Tyro Fuse - Share Your Game with One Single Zip! 🎁

Fuse is a Tyro command that packs your game **and** everything it needs to run into **one single zip file**. Your friend does not need to install Tyro or copy lots of files - they just unzip your package, and the game is ready to play!

Think of it like putting your game, the Tyro engine, and a little instruction note into a gift box.

---

## The Command

```
tyro --fuse <your game> <name of the zip>
```

Two things go after `--fuse`:

| Part | What it is |
| --- | --- |
| `<your game>` | Your game **folder** or one **single `.tyro` file** |
| `<name of the zip>` | The zip file you want to create |

---

## Example 1: Pack a Whole Game Folder

Say your game lives in a folder called `mygame`, and inside it there is `mygame.tyro`:

```
mygame\
  mygame.tyro
  player.png
  jump.wav
```

Open a command prompt in your Tyro folder and type:

```
tyro --fuse mygame mygame.zip
```

Tyro will answer something like:

```
Tyro version 0.1
fusing mygame into mygame.zip
done: mygame.zip
```

That's it! `mygame.zip` now contains:

- your whole `mygame\` folder (all pictures, sounds and scripts),
- the Tyro engine files (so it can run by itself),
- a small file called `fuse.conf` that tells Tyro which script to start.

---

## Example 2: Pack One Single Script

Have only one script file? That works too!

```
tyro --fuse pong.tyro pong.zip
```

The script is put at the top of the zip, and Tyro knows to run it directly.

---

## Example 3: Put the Zip Somewhere Else

Want to save the zip into another folder? Just give the full path:

```
tyro --fuse mygame D:\MyGames\mygame.zip
```

The folder will be created if it does not exist yet.

---

## Example 4: The Shortcut Forms

These all do the same thing as `--fuse mygame mygame.zip`:

```
tyro --fuse=mygame mygame.zip
tyro --fuse:mygame mygame.zip
```

---

## What Finds the Main Script?

When you pack a **folder**, Tyro looks for the script to run, in this order:

1. `foldername.tyro` (a script with the same name as the folder - best choice!)
2. Any other `*.tyro` file
3. Any `*.lua` file

**Tip:** name your main script like your folder. If the folder is `mygame`, make the main script `mygame.tyro`. Then Tyro always knows what to start!

If Tyro cannot find any script, it will still pack your folder, but it will print a warning like:

```
no script found in: mygame
```

---

## What Is fuse.conf?

`fuse.conf` is a tiny note Tyro writes **inside the zip** for itself. You never have to write it - it is made automatically. It looks like this:

```
run=mygame.tyro
workpath=mygame
```

It simply says: *"Start this script, and use this folder as the working folder."*

You can open it (just for fun!) with any zip tool, but you do not need to change anything.

---

## What Is release.conf?

Next to `tyro.exe` there is a list of the engine files that go into every package - things like `tyro.exe`, the fonts and the `.dll` libraries. That list lives in `bin\release.conf`, one file per line.

Normally you do not need to touch it. Grown-ups or advanced users can edit it to add or remove engine files from the packages.

---

## When Something Goes Wrong

Tyro always tells you what happened:

| Message | What it means |
| --- | --- |
| `done: mygame.zip` | 🎉 Your zip was created! |
| `fuse failed: game path not found: ...` | Tyro could not find your game folder or script - check the spelling |
| `usage: tyro --fuse <path> <zip>` | You forgot one of the two parts after `--fuse` |
| `not found: ...` | A file listed in `release.conf` is missing (the zip is still made) |
| `no script found in: ...` | Your folder has no `.tyro` or `.lua` script to run |
| `empty game folder: ...` | Your folder is empty - put your game inside it first |

**Tip:** after making a zip, unzip it somewhere and try to play it. That is the best way to check that everything works!

---

## Fun Things to Try

### 🎮 Make a gift for a friend
```
tyro --fuse mygame mygame.zip
```
Then send `mygame.zip` to a friend. They unzip it and double-click `tyro.exe` to play.

### 📦 Pack all your games
Give every game its own zip:
```
tyro --fuse defender defender.zip
tyro --fuse kingvalley kingvalley.zip
tyro --fuse minerman minerman.zip
```

### 🧪 Test it like a real player
Unzip your package into a **new empty folder** and start it there. If the game runs, your package is perfect!

### 📝 One script, one zip
```
tyro --fuse hello.tyro hello.zip
```
The smallest possible package - great for sharing a single demo.

---

## Tips!

- The zip can take a moment to build - Tyro packs the engine, fonts and libraries too, so it is not tiny.
- A folder keeps its own name inside the zip: `mygame\` stays `mygame\`.
- A single script goes to the top of the zip, with no folder around it.
- Warnings do not stop the zip - read them, but the file is still created.
- An error means no zip was written - fix the problem and try again.
- You can also check `--help` to see the fuse command reminder: `tyro --fuse <path> <zip>`.

Have fun packing and sharing your games!
