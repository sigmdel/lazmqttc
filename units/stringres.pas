unit stringres;

{$mode objfpc}{$H+}

interface

uses
 mqttclass;

resourcestring
  mssNoClient = 'No Mqtt Client';
  mssConnecting = 'Attempting to connect';
  mssConnected = 'Connected';
  mssReconnecting = 'Attempting to reconnect';
  mssDisconnected = 'Disconnected';
  sFalse = 'false';
  sTrue = 'true';
  srxMsgFormat = '[%s] - %s';
  sConnect = 'Connect';
  sDisconnect = 'Disconnect';
  sVersionFormat = '(version %d.%d.%d)';
  sSubscribeColumnTitle = 'Subscribe Topics';

  sNoClientError = 'The Mqtt client has not been created';
  sNotConnectedError = 'The Mqtt client is not connected';
  sNoTopicError = 'A topic must be specified';
  sNoPayloadWarning = 'OK but the message had no payload';
  sRetainRemovedFormat = 'Retain flag removed for topic %s';
  sMQTTErrorFormat = 'MQTT Error: %d';
  sOK = 'OK';

  // sbrorkeredit
  sQoSHint = '0 - At most once'#10'1 - At least once'#10'2 - Exactly once';
  cbsLoseChanges = 'Close without saving changes';
  cbsDeleteQuery = 'Delete <%s>';

  // topicsgrids
  sUseTitle = 'Use';
  sTopicTitle = 'Topic';
  sQosTitle = 'QoS';

var
  statusStr: array[TMQTTConnectionState] of string;
  falsetruestr: array[boolean] of string;

implementation

initialization
  statusStr[mqttNone] := mssNoClient;
  statusStr[mqttConnecting] := mssConnecting;
  statusStr[mqttConnected] := mssConnected;
  statusStr[mqttReconnecting] := mssReconnecting;
  statusStr[mqttDisconnected] := mssDisconnected;
  falsetruestr[false] := sFalse;
  falsetruestr[true] := sTrue;
end.

