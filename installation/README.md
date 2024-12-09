# Installation

To install the compiled executable in Mint (MATE) (tested in versions 20.1, 21.2 and 22) :

1. Create a directory named `lazmqttc` somewhere in the search path such as in `~/.local/bin`.
2. Copy the following files into that directory.
    1. the executable file `lazmqttc`,
    2. the image file `images/lazmqttc.png` ,
    3. the directory `languages`,
    
3. Copy the `lazmqttc.desktop` file to '~/.local/share/applications'.
4. Replace `$USER` in the `Exec=` and `Icon=` lines of the copied `lazmqttc.desktop` file to the correct user name. If the executable is renamed or if it is placed in a different directory than `~/.local/bin/lazmqttc` then the rest of those lines will also need to be adjusted.

For example, if the user in Mint is `mimi`, the two lines in `lazmqttc.desktop` should be

<pre>
Exec=/home/mimi/.local/bin/lazmqttc/lazmqttc
Icon=/home/mimi/.local/bin/lazmqttc/lazmqttc.png
</pre>

If need be, see [Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/latest/index.html) and [A. Registered Categories](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry) to change the category or categories under which the executable will appear in the menu.
