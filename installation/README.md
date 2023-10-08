# Installation

To install the compiled executable in Mint (MATE) (tested in versions 20.1 and 21.2) :

1. Create a directory named `lazmqttc` somewhere in the search path such as in `~/.local/bin`.
2. Into that directory, copy 
    1. the executable file `lazmqttc`,
    2. the image file `images/lazmqttc.png` ,
    3. the directory `languages`,
    
3. Copy the `lazmqttc.desktop` file to '~/.local/share/applications'.
4. Replace `$USER` in the `Exec=` and `Icon=` lines of the copied `lazmqttc.desktop` file to the correct user name. If the executable is renamed or if it is placed in a different directory than `~/.local/bin/lazmqttc` then the rest of those lines will also need to be adjusted.

Since my user name is `michel` on my Mint machine the two lines in my `lazmqttc.desktop` file are

<pre>
Exec=/home/michel/.local/bin/lazmqttc/lazmqttc
Icon=/home/michel/.local/bin/lazmqttc/lazmqttc.png
</pre>

If need be, see [Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/latest/index.html) and [A. Registered Categories](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry) to change the category or categories under which the executable will appear in the menu.
