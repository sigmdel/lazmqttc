# lazmqttc: Lazarus MQTT Client

A basic MQTT client written in Free Pascal/Lazarus that can publish messages to a broker while being subscribed to one or more topics with the same broker. It uses the libmosquitto library to communicate with the MQTT broker.

![screenshot](images/test_mosquitto.jpg)

The screen capture shows the message sent to the public `test.moquitto.org` broker and it's reply. In this example, the client is subscribed to the same topic used to send the message, which in many cases would not be done.

## Requirements

The libmosquito library is needed. In Debian systems this means installing two packages:
-  libmosquitto1    
-  libmosquitto-dev 

The first, `libmosquitto1` will probably already be installed if the mosquitto-clients package is available on the system. In Debian-based systems these packages can be installed with a package manager such as [Synaptic](http://www.nongnu.org/synaptic/) or from the command line.

    $ sudo apt install libmosquitto1 libmosquitto-dev

Two Free Pascal units are required

- mosquitto.pas - conversion of the C mosquitto.h header to Pascal, provides the same API as the C version
- mqttclass.pas - Object Pascal wrapper class to ease the integration of libmosquitto into Object Oriented 

These files, found in the [mosquitto-p](mosquitto-p/) directory, are copied from the [GitHub repository](https://github.com/chainq/mosquitto-p) with the same name by Károly Balogh (chainq).

## Compiling

In principle creating this tool should be straightforward: clone this repository, start the Lazarus IDE, load the project, add an application icon if desired and compile the program. The application icon, called `lazmqttc.png`, is available in the `images`. The file `lazmqtt.lzp` is the LazPaint source for the image file. 

## Testing Environment

The project was built with Lazarus 2.0.12 (Free Pascall 3.2.0) on a Mint 20.1 system with version 1.6.9-1 of the mosquitto libraries. 

This utility was cobbled quickly to fulfill an immediate need: wrangling a number of IoT devices running Tasmota firmware. The code could use considerable improvement.

## Licence

The [Eclipse Mosquitto](https://github.com/eclipse/mosquitto) project is dual-licensed under the Eclipse Public License 2.0 and the
Eclipse Distribution License 1.0.

The content of the `mosquito-p` repository is covered by the ISC License ([SPDX](https://spdx.dev/): [ISC](https://spdx.org/licenses/ISC.html)).

The icon images were copied from the [Lazarus](https://www.lazarus-ide.org/) distribution which is provided under a modified LGPL licence (see COPYING.modifiedLGPL.txt in the Lazarus source tree. The source of the check list box editor (also covered by the modified LGPL licence) in the Lazarus IDE was borrowed to create the editor of subscribed topics. 

The **BSD Zero Clause** ([SPDX](https://spdx.dev/): [0BSD](https://spdx.org/licenses/0BSD.html)) licence applies to the original code in this repository.
