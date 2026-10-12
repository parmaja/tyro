# radio_client

An internet radio client demo built with **RayLib** and MiniLib's **mnIceCasts**.
It deliberately avoids TyroLib so it can be used as a small, self-contained test
bed for `mnIceCasts`.

```
src/demos/radio/
  radio_client.lpr   the program
  radio_client.lpi   Lazarus project
```

## Build

`radio_client.lpi` already has the needed unit search paths:

* `..\..\raylib`                 - RayLib.pas / RayClasses.pas
* `..\..\..\..\minilib\lib`      - mnTypes, mnStreams, mnLibraries, ...
* `..\..\..\..\minilib\socket\source` - mnIceCasts, mnSockets, mnClients, ...

With Lazarus:

```
lazbuild src/demos/radio/radio_client.lpi
```

or with the command line compiler:

```
cd src/demos/radio
fpc -Mdelphi -Fu"..\..\raylib" ^
    -Fu"..\..\..\..\minilib\lib" ^
    -Fu"..\..\..\..\minilib\socket\source" ^
    -FU"lib\x86_64-win64" -FE"..\..\..\bin" radio_client.lpr
```

The executable is written to `bin/`, next to `raylib-6.0-64.dll`, which RayLib
loads dynamically.

## Use

* `Enter`      play the URL in the box
* `Space`/`P`  pause / resume
* `S`          stop
* `Up`/`Down`  volume
* `D`          dump the current raw buffer to `radio_dump_NNN.bin`
* `Esc`        quit

The default station is a public MP3 stream. Both MP3 and OGG streams work;
AAC/ADTS streams are downloaded but cannot be decoded by RayLib's audio backend
and are reported as such.

The **Save dump** button is the main debugging aid: it writes exactly what
`TmnIceCastClient` produced (after chunked / ICY metadata handling) so it can be
played or inspected offline.

## How playback works

A radio stream never ends, so the client buffer cannot be loaded into a RayLib
`Music` as a whole. The player:

1. waits until the client has buffered a pre-roll,
2. snapshots the buffer into a `Music` with `LoadMusicStreamFromMemory`,
3. when the decoder is close to catching up it trims the shared buffer, keeping
   an overlap of already decoded bytes, snapshots the tail and seeks into the
   overlap so playback continues seamlessly.

## Bugs found and fixed in mnIceCasts

Writing this demo exposed three bugs, all fixed in
`minilib/socket/source/mnIceCasts.pas`:

1. **Transient `icStopped` at startup.** The connection was created as
   `icStopped` and only set to `icConnecting` when the worker thread reached
   `Prepare`. A consumer polling `State` in that window saw `icStopped` and
   thought the stream had already ended. The connection now starts as
   `icConnecting`.
2. **Unconditional `TThread.Queue`.** `SetState`/`SetTitle` queued notifications
   even when no `OnStateChanged`/`OnTitleChanged` handler was assigned. An
   application with no message loop pumping `CheckSynchronize` (like the plain
   RayLib loop in this demo) accumulated queued records without bound. The
   notifications are now only queued when a handler is assigned.
3. **Needless delay before the first connect.** `ConnectWithRetries` always
   waited `ReconnectDelay` before the first attempt, adding a fixed delay to
   every `Open`. The wait now only applies to retries.
