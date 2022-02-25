unit stringres;

{$mode objfpc}{$H+}

interface

uses
 mqttclass;

resourcestring
  mssNoClient = 'No MQTT Client';
  mssConnecting = 'Attempting to connect';
  mssConnected = 'Connected';
  mssReconnecting = 'Attempting to reconnect';
  mssDisconnected = 'Disconnected';
  sFalse = 'false';
  sTrue = 'true';
  sMsgFormatWithTopic = '%s[%s] - %s';
  sMsgFormatNoTopic = '%s%s';
  sConnect = 'Connect';
  sDisconnect = 'Disconnect';
  sAppCaption = '%s (ver. %d.%d.%d)';
  sSubscribeColumnTitle = 'Subscribe Topics';

  sNoClientError = 'The MQTT client has not been created';
  sNotConnectedError = 'The MQTT client is not connected';
  sNoTopicError = 'A topic must be specified';
  sNoPayloadWarning = 'OK but the message had no payload';
  sRetainRemovedFormat = 'Retain flag removed for topic %s';
  sMQTTErrorFormat = 'MQTT Error: %d';
  sOK = 'OK';

  sNoEncryptionKey = 'Password encryption key is empty - program halted';

  // sbrorkeredit
  sPageNames = 'Broker'#10'Security'#10'Encryption'#10'Publish Topic'#10'Subscribe Topics'#10;
  sQoSHint = '0 - At most once'#10'1 - At least once'#10'2 - Exactly once'#10;
  cbsLoseChanges = 'Close without saving changes';
  cbsDeleteQuery = 'Delete <%s>';
  sJSONFiles = 'JSON files';

  // topicsgrids
  sUseTitle = 'Use';
  sTopicTitle = 'Topic';
  sQosTitle = 'QoS';
  sQos = 'Quality of Service';

  // report
  sMessageSent =  'Message published';
  sMessageNotSent =  'Message not published';


var
  statusStr: array[TMQTTConnectionState] of string;
  falsetruestr: array[boolean] of string;
  FileDialogFilter: string;

implementation

initialization
  statusStr[mqttNone] := mssNoClient;
  statusStr[mqttConnecting] := mssConnecting;
  statusStr[mqttConnected] := mssConnected;
  statusStr[mqttReconnecting] := mssReconnecting;
  statusStr[mqttDisconnected] := mssDisconnected;
  falsetruestr[false] := sFalse;
  falsetruestr[true] := sTrue;
  FileDialogFilter := sJSONFiles + '|*.json';
end.

