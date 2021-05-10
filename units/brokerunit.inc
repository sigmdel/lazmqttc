// Default values for TBroker.Init
//  Set up as an include to simplify unit test.

const
  DEFAULT_MQTT_HOST = 'localhost';  // default broker host name / IP address
  DEFAULT_MQTT_PORT = 1883;         // default MQTT TCP port
  DEFAULT_USER = '';                // no security
  DEFAULT_PASSWORD = '';            // no security
  DEFAULT_SSL = false;              // no encryption
  DEFAULT_SSLCERT = '';             // no encryption
  DEFAULT_KEEPALIVES = 60;          // second delay between keep alive pings - min 5 secs
  DEFAULT_RECONNECTDELAY = 0;       // reconnection delay - default is 1 second
  DEFAULT_RECONNECTBACKOFF = false; // exponential reconnection delay
  DEFAULT_PUBLISH_TOPIC = '';       // no default publish topic
  DEFAULT_PUBLISH_PAYLOAD = '';     // no default publish payload
  DEFAULT_PUBQOS = 0;               // publish topic quality of service
  DEFAULT_PUBRETAIN = false;        // retail publish topic
  DEFAULT_AUTORECONNECT = true;     // reconnect after disconnection
  DEFAULT_FIRST_SUBTOPIC = '#';     // subscribe to all topics
  DEFAULT_FIRST_SUBQOS = 0;         // subscribed quality of service 0
  DEFAULT_FIRST_SUBUSE = True;      // activate this subscribe topic

