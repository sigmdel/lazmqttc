# Resources

This directory contains three sample JSON broker definition files and a sample application JSON options file.


In Linux, these files would be saved in the user configuration directory:
<pre>  /home/&lt;<i>user</i>&gt;/.config/sigmdel/lazmqttc</pre>

In Windows 10, the files are saved in the local `AppData` folder:
<pre>  C:\Users\&lt;<i>user</i>&gt;\AppData\Local\sigmdel\lazmqttc</pre>

In both cases &lt;<i>user</i>&gt; is a placeholder for the name of the user.

## Broker Definition Files

### `default.json` 
Simple broker definition that subscribes to all topics. Assumes the broker is on the same machine as the application.

### `test_mosquitto.json`
Use this definition to test the application with the public MQTT broker at `mosquitto.org`.

### `tasmota.json`

Used to communicate with devices on which [Tasmota](https://github.com/arendst/Tasmota) is running. Its default publish topic will cause a `Status 5` command message to be sent to all devices with the `tasmotas` MQTT topic. They will respond with the corresponding status message which includes their IP address. Use the `cmnd/sonoffs/status` topic if the Tasmota firmware of some of the devices is older than version 7.1.0. 
  
The definition includes six subscribe topics, only one is used: `stat/+/STATUS5`. With that only replies to the published topic will be displayed. The other subscribe topics will be useful when investigating Tasmota and [Domoticz](https://www.domoticz.com/) MQTT messages.

## Default Options File

### `options.json`

This contains the default settings just as if the `Reset` button had been pressed in the **Options** edit window.

If the application does not find the options file in the configuration directory when it starts up, it will created one with default values. A new 'options.json` file is saved only when at least one parameter is changed.
