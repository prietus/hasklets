# hasklets

Desktop widgets for Wayland, written in Haskell with GTK 4 and cairo. They
sit on the desktop, above the wallpaper and below the windows, on any
compositor with wlr-layer-shell; made to go with
[rivermonad](https://github.com/prietus/rivermonad) and
[waymobar](https://github.com/prietus/waymobar).

![hasklets on a rivermonad desktop: weather, CPU, memory and network, media player, disks and top processes](docs/desktop.png)

* **Weather**: current conditions, high and low, and the next six hours,
  from [Open-Meteo](https://open-meteo.com) (no API key)
* **CPU, memory and network**: two ring gauges and a minute of upload and
  download history, read from `/proc`
* **Media player**: cover art, title, artist, progress and previous /
  play-pause / next buttons for any MPRIS player, over D-Bus
* **Disks**: used and total space of every real filesystem
* **Processes**: the ones using the most CPU, with their memory
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
at run time for the weather and for cover art served over HTTP.

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
  "corner": "top-left",
  "margin": 24,
  "gap": 14,
  "columns": [["weather","system","mpris"],["disks","processes"]],
  "weather": {"latitude":37.38,"longitude":-6.05,"name":"Tomares"},
  "processes": {"rows":5}
}
```

| Field | Default | |
|---|---|---|
| `corner` | `"top-left"` | or `"top-right"`, `"bottom-left"`, `"bottom-right"` |
| `margin` | `24` | distance from the screen edges, in logical pixels |
| `gap` | `14` | space between widgets |
| `columns` | see above | columns from left to right, each a list of `weather`, `system`, `mpris`, `disks`, `processes`; a widget that is not listed is hidden |
| `weather.name` | `"Tomares"` | title of the weather widget |
| `weather.latitude`, `weather.longitude` | `37.38`, `-6.05` | forecast location; sent to api.open-meteo.com |
| `processes.rows` | `5` | processes shown, 1 to 10 |

Missing fields take their default.

### Settings window

`hasklets --config`, or a right-click on any widget, opens a window that
edits the same file: move widgets between columns or hide them, choose the
corner and the spacing, look up the coordinates of a place by name (through
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
window according to the configuration, and `Draw.hs` has the shared
drawing primitives (card, ring gauge, sparkline, text).
