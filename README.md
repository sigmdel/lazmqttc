# lazmqttc: Lazarus MQTT Client

A basic MQTT client written in Free Pascal/Lazarus that can publish messages to a broker while being subscribed to one or more topics with the same broker. It uses the libmosquitto library to communicate with the MQTT broker.

![screenshot](images/test_mosquitto.jpg)

The screen capture shows the message sent to the public `test.moquitto.org` broker and it's reply. In this example, the client is subscribed to the same topic used to send the message, which in many cases would not be done.

<!-- TOC -->

- [1. Requirements](#1-requirements)
    - [1.1. Linux](#11-linux)
    - [1.2. Windows 10](#12-windows-10)
    - [1.3. Common](#13-common)
- [2. Compiling](#2-compiling)
- [3. Testing Environment](#3-testing-environment)
- [4. Broker Definitions](#4-broker-definitions)
- [5. Licence](#5-licence)

<!-- /TOC -->


## 1. Requirements

### 1.1. Linux

The libmosquito library is needed. In Debian systems this means installing two packages:
-  libmosquitto1    
-  libmosquitto-dev 

The first, `libmosquitto1` will probably already be installed if the mosquitto-clients package is available on the system. In Debian-based systems these packages can be installed with a package manager such as [Synaptic](http://www.nongnu.org/synaptic/) or from the command line.

    $ sudo apt install libmosquitto1 libmosquitto-dev

### 1.2. Windows 10


1. Get the latest binary package from [Eclipse mosquitto Download](https://mosquitto.org/download/). Version 2.0.10 is available as of May 5, 2021. Chose the appropriate 64-bit `mosquitto-2.0.10-install-windows-x64.exe` or `32-bit mosquitto-2.0.10-install-windows-x32.exe`. 

2. Click on the downloaded application to install the package. I did not install it as a service as I only want to test the `lazmqttc` in this environement. 

> - The 64-bit package was installed in `C:\Program Files\mosquitto\`. 
> - The 32-bit package will probably go into `C:\Program Files (x86)\mosquitto\`.
 
 3. Test the `mosquitto_sub` and `mosquitto_pub` utilities from the command line using a the host name or IP address of a reachable MQTT broker instead of &lt;<i>mqtt_broker</i>&gt;. 

<pre>
   C:\Users\michel>"c:\Program Files\mosquitto\mosquitto_sub" -h &lt;<i>mqtt_broker</i>&gt; -t "#"
</pre>

4. Copy the mosquitto libraries <pre>
        C:\Program Files\mosquitto\mosquitto.dll
        C:\Program Files\mosquitto\mosquitto_dynamic_security.dll
        C:\Program Files\mosquitto\libcrypto-1_1-x64.dll
        C:\Program Files\mosquitto\libssl-1_1-x64.dll
        C:\Program Files\mosquitto\mosquittopp.dll
</pre> to the same folder containing the compiled `lasmqtt.exe` executable. Depending on use, they may not all be necessary, but the first two are needed without doubt. 

>> It is left as an exercise for knowledgable Windows users to find a more elegant way of ensuring that the DLL's are found.


### 1.3. Common

Two Free Pascal units are required

- mosquitto.pas - conversion of the C mosquitto.h header to Pascal, provides the same API as the C version
- mqttclass.pas - Object Pascal wrapper class to ease the integration of libmosquitto into Object Oriented 

These files, found in the [mosquitto-p](mosquitto-p/) directory, are copied from the [GitHub repository](https://github.com/chainq/mosquitto-p) with the same name by Károly Balogh (chainq).

## 2. Compiling

In principle creating this tool should be straightforward: clone this repository, start the Lazarus IDE, load the project, add an application icon if desired and compile the program. The application icon, called `lazmqttc.png`, is available in the `images`. The file `lazmqtt.lzp` is the LazPaint source for the image file. 

## 3. Testing Environment

The project was built with Lazarus 2.0.12 (Free Pascall 3.2.0) on a Mint 20.1 system with version 1.6.9-1 of the mosquitto libraries. 

A cursory test was done with the same compiler in Windows 10.

This utility was cobbled quickly to fulfill an immediate need: wrangling a number of IoT devices running Tasmota firmware. The code could use considerable improvement.

## 4. Broker Definitions 

The MQTT broker definition are JSON formated text files. Here is an example of a definition used with an MQTT broker running on a [Domoticz](https://domoticz.com/) home automation server. The default publish message will show status 5 of all Tasmota devices which can be useful because that will display their IP address.

<pre>
{
  "Host" : "192.168.1.22",
  "Port" : 1883,
  "User" : "",
  "Password" : "",
  "SSL" : false,
  "SSLCert" : "",
  "KeepAlives" : 60,
  "ReconnectDelay" : 10,
  "ReconnectBackoff" : true,
  "AutoReconnect" : true,
  "PubTopic" : "cmnd/tasmotas/status",
  "PubPayload" : "5",
  "PubQoS" : 0,
  "PubRetain" : false,
  "SubTopics" : [
    {
      "Topic" : "stat/#",
      "QoS" : 0,
      "Use" : true
    },
    {
      "Topic" : "tele/#",
      "QoS" : 0,
      "Use" : false
    },
    {
      "Topic" : "domoticz/in",
      "QoS" : 0,
      "Use" : false
    },
    {
      "Topic" : "domoticz/out",
      "QoS" : 0,
      "Use" : false
    }
  ]
}   
</pre>       

In Linux, these definition files are saved in the `~/.config/sigmdel/lazmqttc`  
where `~` is the user home directory. So fully expanded the directory is
<pre>  /home/&lt;<i>user</i>&gt;/.config/sigmdel/lazmqttc</pre>

In Windows 10, the files are save in the local `AppData` folder :
<pre>  C:\Users\&lt;<i>user</i>&gt;\AppData\Local\sigmdel\lazmqttc</pre>


## 5. Licence

The [Eclipse Mosquitto](https://github.com/eclipse/mosquitto) project is dual-licensed under the Eclipse Public License 2.0 and the
Eclipse Distribution License 1.0.

The content of the `mosquito-p` repository is covered by the ISC License ([SPDX](https://spdx.dev/): [ISC](https://spdx.org/licenses/ISC.html)).

The icon images were copied from the [Lazarus](https://www.lazarus-ide.org/) distribution which is provided under a modified LGPL licence (see COPYING.modifiedLGPL.txt in the Lazarus source tree. The source of the check list box editor (also covered by the modified LGPL licence) in the Lazarus IDE was borrowed to create the  subscribed topics editor. 

The **BSD Zero Clause** ([SPDX](https://spdx.dev/): [0BSD](https://spdx.org/licenses/0BSD.html)) licence applies to the original code in this repository.
