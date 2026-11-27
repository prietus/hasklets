# hasklets

Desktop widgets for Wayland, written in Haskell with GTK 4 and cairo. They
sit on the desktop, above the wallpaper and below the windows, on any
compositor with wlr-layer-shell; made to go with
[rivermonad](https://github.com/prietus/rivermonad) and
[waymobar](https://github.com/prietus/waymobar).

![hasklets on a rivermonad desktop: weather with the week ahead, the journal, processes, disks, CPU, memory and network, and the media player, under waymobar and over the bgmonad wallpaper](docs/desktop.png)

*Move them around: drag a widget and it sticks to the sides of the others.*

![The widgets dragged around the desktop and landing on the grid: weather, processes, the journal, CPU, memory and network, and the MPD card with its VU meter moving to the music](docs/drag.gif)

* **Weather**: current conditions, high and low, and the next six hours,
  from [Open-Meteo](https://open-meteo.com) (no API key) or from your own
  wtr server (Apple's WeatherKit)
* **Weather, with forecast** (`forecast`, hidden by default): the same, plus
  the week ahead and the temperature of the next 24 hours as a curve
* **Weather, full** (`weather-full`, hidden by default): a taller card with
  the weather now, the curve of the next 24 hours, ten days ahead with their
  chance of rain and their range of temperatures, and tiles for what it feels
  like, humidity, wind, rain, UV, pressure, visibility, clouds, sun and moon;
  the days beyond the week and the tiles need a wtr server (its `detail` view)
* **CPU, memory and network**: two ring gauges and a minute of upload and
  download history, read from `/proc`
* **Media player**: cover art, title, artist, progress and previous /
  play-pause / next buttons for any MPRIS player, over D-Bus
* **Disks**: a ring for the disk your home directory is on, split into the
  four folders or files that take the most space in it (measured with `du`
  every ten minutes), and a bar with the used and total space of every
  other real filesystem
* **Processes**: the ones using the most CPU, with their memory
* **Calendar**: the events of the next two weeks from iCloud, Google, any
  CalDAV server or an iCal address
* **Journal** (`journal`, hidden by default): what is new in the systemd
  journal, instead of the journal itself
* **Backups** (`luna`, hidden by default): what
  [Luna](https://github.com/prietus/luna) says about your Borg repositories:
  how much they hold, the largest ones, and the backup that is running, has
  failed or comes next. It needs Luna 0.1.3 or newer
* **Music server** (`mpd` and `mopidy`, hidden by default): what an
  [MPD](https://www.musicpd.org) or a [Mopidy](https://mopidy.com) server is
  playing, asked to the server itself, so no player has to be open: cover,
  edition, format of the audio, outputs and the queue. Click the bar to move
  in the song or a song of the queue to play it, switch shuffle and repeat,
  and turn the wheel for the volume
* five themes: dark, light, Nord, Gruvbox and a translucent glass look
* configured with a JSON file that is reloaded when it changes, or from a
  settings window (right-click any widget)
* gi-gtk4 and [gtk4-layer-shell](https://github.com/wmww/gtk4-layer-shell);
  everything is drawn with cairo and pango

## Build

```sh
sudo pacman -S gtk4 gtk4-layer-shell gobject-introspection curl   # plus ghc and cabal-install
cd hasklets
cabal install --installdir ~/.local/bin
```

The first build compiles the GTK bindings and takes a while. `curl` is used
at run time for the weather, the calendars and cover art served over HTTP.

Start it from your rivermonad config:

```haskell
startupHook = spawnOnce "hasklets"
```

On other compositors, run `hasklets` from their autostart.

## Configuration

`hasklets` reads `~/.config/hasklets/config.json`, creating it with the
defaults the first time. The file is watched: save it and the widgets
rearrange themselves, without restarting and without losing the history of
the charts. If it does not parse, the error goes to stderr and the previous
configuration is kept.

```json
{
  "theme": "mocha",
  "scale": 1,
  "corner": "top-left",
  "margin": 24,
  "gap": 14,
  "columns": [["weather","system","mpris"],["disks","processes"]],
  "weather": {"latitude":40.42,"longitude":-3.70,"name":"Madrid","provider":"open-meteo"},
  "processes": {"rows":5}
}
```

| Field | Default | |
|---|---|---|
| `theme` | `"mocha"` | see [Themes](#themes) |
| `scale` | `1` | size of the widgets, 0.5 to 3: `1.25` makes them a quarter bigger |
| `corner` | `"top-left"` | or `"top-right"`, `"bottom-left"`, `"bottom-right"` |
| `opacity` | the theme's | how opaque the cards are, 0 to 1 (the themes use about 0.92). Lower it, to `0.6` say, for glass cards on a compositor that blurs what is behind them, like [river-fx](https://github.com/prietus/river-fx) |
| `margin` | `24` | distance from the screen edges, in logical pixels |
| `gap` | `14` | space between widgets |
| `columns` | see above | columns from left to right, each a list of `weather`, `forecast`, `weather-full`, `system`, `mpris`, `disks`, `processes`, `calendar`, `journal`, `luna`, `mpd`, `mopidy`; a widget that is not listed is hidden |
| `positions` | `{}` | widgets dragged out of their column, as `{"disks": [640, 300]}`: the distance from the two edges of the corner, in logical pixels |
| `weather.name` | `"Madrid"` | title of the weather widget |
| `weather.latitude`, `weather.longitude` | `40.42`, `-3.70` | forecast location; sent to the weather provider |
| `weather.provider` | `"open-meteo"` | or `"wtr"`: a wtr server (WeatherKit) at `weather.url`, with the token of `~/.config/wtr/api_token`; it has no hourly forecast, so the widget shows the next six days instead of the next six hours |
| `weather.rain` | `true` | rain falls over the weather cards while it rains; `false` keeps them still |
| `processes.rows` | `5` | processes shown, 1 to 10 |
| `journal.rows` | `4` | messages the journal card has room for, 1 to 8 |
| `mpd.host`, `mpd.port` | `"localhost"`, `6600` | the MPD server of the `mpd` widget |
| `mopidy.host`, `mopidy.port`, `mopidy.http` | `"localhost"`, `6600`, `6680` | the Mopidy server of the `mopidy` widget: its MPD protocol (the Mopidy-MPD extension) and its web API, where the covers come from |
| `mpd.queue`, `mopidy.queue` | `1` | songs of the queue listed after the one playing, 1 to 10; the card grows to hold them |
| `mpd.levels` | `0` | the port of an [mpdvu](https://github.com/prietus/mpdvu) running next to the MPD server (`6601`), for a VU meter of what is playing; `0` for none |
| `mpd.meter` | `"vu"` | what that meter is: `"vu"` for the level of each channel, or `"spectrum"` for a bar for each band of frequencies, which takes mpdvu 0.2.0 |
| `mopidy.goodies` | `true` | use [mopidy-goodies](https://github.com/prietus/mopidy-goodies) where the Mopidy server has it: the card then shows the format that really reaches the DAC, whether the path is bit-perfect, and the label, catalogue number and country of the album; `false` never asks for it |
| `mopidy.levels` | `"off"` | a meter in the Mopidy card, from the visualizer feed of mopidy-goodies: `"vu"` for the level of each channel, `"spectrum"` for a bar for each band of frequencies. The audio comes over the network (uncompressed, some 1.4 Mbit/s) and is worked out here |

Missing fields take their default.

### Themes

![The five themes: mocha, latte, nord, gruvbox and glass](docs/themes.png)

| `theme` | |
|---|---|
| `mocha` | Catppuccin Mocha: dark and flat, thin ring gauges (the default) |
| `latte` | Catppuccin Latte: the light counterpart |
| `nord` | Nord colors, squarer cards, arc gauges and bar charts |
| `gruvbox` | Gruvbox colors, nearly square corners, monospace text, bar charts |
| `glass` | translucent dark cards with large corners and arc gauges |

A theme is a record in `app/Theme.hs` with the colors, the corner radius,
the font and the style of gauges and charts; adding one is adding a record
to the `themes` list.

The themes use the [Inter](https://rsms.me/inter/) font (`inter-font` on
Arch), except the one in monospace; without it they are drawn in the
system's default font.

### Calendar

The calendar widget is hidden until it is added to a column. Its accounts
are not in `config.json` but in `~/.config/hasklets/accounts.json`, a file
only its owner can read, because they hold passwords; add them from the
settings window, or by hand:

```json
{"calendars": [
  {"provider": "icloud", "user": "me@icloud.com", "password": "abcd-efgh-ijkl-mnop"},
  {"provider": "google", "user": "me@gmail.com", "password": "abcdefghijklmnop"},
  {"provider": "caldav", "url": "https://cloud.example.org/remote.php/dav/", "user": "me", "password": "..."},
  {"provider": "ics", "url": "https://example.org/calendar.ics"}
]}
```

| `provider` | |
|---|---|
| `icloud` | every calendar of the account, with an [app-specific password](https://support.apple.com/102654) |
| `google` | the main calendar of the account, with an [app password](https://support.google.com/accounts/answer/185833); if Google rejects it, use the calendar's "secret address in iCal format" as an `ics` account |
| `caldav` | every calendar of an account on any CalDAV server (Nextcloud, Fastmail, Radicale...) |
| `ics` | a read-only iCal address; no user or password |

Passwords reach `curl` through its standard input, never on its command
line. Events are fetched every ten minutes. Repeating events are expanded
for daily, weekly, monthly and yearly rules; the rarer parts of a rule
(`BYMONTHDAY`, `BYSETPOS`...) are ignored.

### Journal

The journal widget is hidden until it is added to a column. A day of logs is
thousands of lines, nearly all of them the same few messages over and over,
so the card does not show the log: it groups the warnings and errors by who
logged them and by the shape of the message (its text without the numbers),
and gives a row to

* a message whose shape was first seen in the last 24 hours, with a full dot
  and its source in bold: a warning that has been coming for longer than
  that is known noise, and only counts in the total at the bottom;
* an error, a crash (`coredump`) or anything worse, new or not; one that
  keeps coming back has a hollow dot.

Each row has how many times the message was logged in the last day and how
long ago the last one was. Above them, a bar for every minute of the last
hour: as tall as the lines logged in it (on a logarithmic scale) and
coloured when one of them belongs to a row. Below, the units systemd has as
failed. With nothing to show, the card shrinks to the chart.

GDM logs under its own name, `gdm-wayland-session`, everything the session
writes to its standard output: the compositor and whatever it starts. Such a
line is given to the program it begins with (`waymobar: ...`) when there is
one.

It reads `journalctl`, and what is known comes from the last week of the
journal itself, so nothing is stored. To see the system's messages and not
only your own, your user has to be in the `wheel`, `adm` or
`systemd-journal` group.

### Placing widgets by hand

Drag a widget with the left button to put it anywhere on the screen. It
leaves its column and lands on a grid: columns a widget and a space apart,
and rows a quarter of that. Every widget is as tall as a whole number of
rows (four make a square), so however they are grouped they line up, with
the same space between them all. The place is saved in `positions`; the
settings window has a button that puts every dragged widget back in its
column. This needs a compositor with layer-shell.

### Settings window

`hasklets --config`, or a right-click on any widget, opens a window that
edits the same file: move widgets between columns or hide them, choose the
theme, the size, the corner and the spacing, add calendar accounts, look up the coordinates of a place by name (through
Open-Meteo's geocoding service) and set the number of processes. *Apply*
saves, and the running widgets pick the change up.

To have it float on rivermonad:

```haskell
myManageHook = composeOne
  [ appId =? "dev.hasklets.Settings" -?> doCenterFloat ]
```

## Development

Each widget is a module in `app/` with a loop that fetches its data on its
own thread and a `Render ()` that draws it; `Main.hs` puts them in the
window according to the configuration, `Draw.hs` has the shared drawing
primitives (card, gauge, sparkline, text) and `Theme.hs` the themes they
are styled with.
