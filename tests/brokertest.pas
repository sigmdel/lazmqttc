unit brokertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  brokerunit, fpJson;

{$i ../units/brokerunit.inc}

type
  TTestBroker= class(TTestCase)
  private
    FBroker: TBroker;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestDefaults;
    procedure TestSubscribed;
    procedure TestAddSubTopic;
    procedure TestDeleteSubTopic;
    procedure TestAssignBroker;
    procedure TestSaveToJSON;
    procedure TestLoadFromJSON;
    procedure TestLoadSaveFile;
  end;

implementation

const
  sTopicKey = 'Topic';
  sUseKey = 'Use';
  sQoSKey = 'QoS';
  sHostKey = 'Host';
  sPortKey = 'Port';
  sUserKey = 'User';
  sPasswordKey = 'Password';
  sSSLKey = 'SSL';
  sSSLCertKey = 'SSLCert';
  sKeepAlivesKey = 'KeepAlives';
  sReconnectDelayKey = 'ReconnectDelay';
  sReconnectBackoffKey = 'ReconnectBackoff';
  sPubTopicKey = 'PubTopic';
  sPubPayloadKey = 'PubPayload';
  sPubQoSKey = 'PubQoS';
  sPubRetainKey = 'PubRetain';
  sSubTopicsKey = 'SubTopics';


procedure TTestBroker.SetUp;
begin
  FBroker := TBroker.create;
end;

procedure TTestBroker.TearDown;
begin
  freeandnil(FBroker);
end;

procedure TTestBroker.TestHookUp;
begin
  AssertTrue(assigned(FBroker));
end;

procedure TTestBroker.TestDefaults;
begin
  AssertEquals('default Host', DEFAULT_MQTT_HOST, FBroker.host);
  AssertEquals('default Port', DEFAULT_MQTT_PORT, FBroker.port);
  AssertEquals('default User', '', FBroker.user);
  AssertEquals('default Password', '', FBroker.password);
  AssertEquals('default SSL', DEFAULT_SSL, FBroker.SSL);
  AssertEquals('default SSLCert', DEFAULT_SSLCert, FBroker.SSLCert);

  AssertEquals('default KeepAlives', DEFAULT_KEEPALIVES, FBroker.KeepAlives);
  AssertEquals('default ReconnectDelay', DEFAULT_RECONNECTDELAY, FBroker.ReconnectDelay);
  AssertEquals('default ReconnectBackoff', DEFAULT_RECONNECTBACKOFF, FBroker.ReconnectBackoff);

  AssertEquals('default PubTopic', DEFAULT_PUBLISH_TOPIC, FBroker.PubTopic);
  AssertEquals('default PubQoS', DEFAULT_PUBQOS, FBroker.PubQoS);
  AssertEquals('default PubRetain', DEFAULT_PUBRETAIN, FBroker.PubRetain);

  AssertEquals('default first SubTopic', DEFAULT_FIRST_SUBTOPIC, FBroker.SubTopics[0].Topic);
  AssertEquals('default first SubTopic QoS', DEFAULT_FIRST_SUBQOS, FBroker.SubTopics[0].QoS);
  AssertEquals('default first SubTopic use', DEFAULT_FIRST_SUBUSE, FBroker.SubTopics[0].Use);

  AssertEquals('SubTopics Count', 1, FBroker.SubTopicsCount);
  AssertEquals('SubTopics Use Count', 1, FBroker.SubTopicsUseCount);
end;

procedure TTestBroker.TestSubscribed;
var
  n: integer;
begin
  n := FBroker.SubTopicsUseCount;
  FBroker.SubTopics[0].Use := false;
  AssertFalse('Topic not subscribed', FBroker.SubTopics[0].Use);
  AssertEquals('SubTopics Use Count (n-1)', n-1, FBroker.SubTopicsUseCount);

  FBroker.SubTopics[0].Use := true;
  AssertTrue('Topic subscribed', FBroker.SubTopics[0].Use);
  AssertEquals('SubTopics Use Count (n)', n, FBroker.SubTopicsUseCount);
end;

procedure TTestBroker.TestAddSubTopic;
const
  secondtopic = 'tele/#';
  thirdtopic = 'status';
begin
  FBroker.AddSubTopic(secondtopic, 1, false);
  AssertEquals('Second subscribe topic', secondtopic, FBroker.SubTopics[1].topic);
  AssertEquals('Second subscribe QoS ', 1, FBroker.SubTopics[1].QoS);
  AssertFalse('Second soubscribe Use', FBroker.SubTopics[1].Use);

  FBroker.AddSubTopic(thirdtopic);
  AssertEquals('Third subscribe topic', thirdtopic, FBroker.SubTopics[2].topic);
  AssertEquals('Third subscribe QoS ', 0, FBroker.SubTopics[2].QoS);
  AssertTrue('Third soubscribe Use', FBroker.SubTopics[2].Use);

  AssertEquals('SubTopics Count', 3, FBroker.SubTopicsCount);
  AssertEquals('SubTopics Use Count', 2, FBroker.SubTopicsUseCount);
end;

procedure TTestBroker.TestDeleteSubTopic;
const
  secondtopic = 'tele/#';
  thirdtopic = 'status';
begin
  FBroker.AddSubTopic(secondtopic);
  FBroker.AddSubTopic(thirdtopic);
  AssertEquals('a- SubTopics.count', 3, FBroker.SubTopicsCount);
  AssertEquals('a- subscribe topic 0', '#', FBroker.SubTopics[0].topic);
  AssertEquals('a- subscribe topic 1', secondtopic, FBroker.SubTopics[1].topic);
  AssertEquals('a- subscribe topic 2', thirdtopic, FBroker.SubTopics[2].topic);

  FBroker.DeleteSubTopic(0);
  AssertEquals('b- SubTopics.count', 2, FBroker.SubTopicsCount);
  AssertEquals('b- subscribe topic 0', secondtopic, FBroker.SubTopics[0].topic);
  AssertEquals('b- subscribe topic 1', thirdtopic, FBroker.SubTopics[1].topic);

  FBroker.DeleteSubTopic(thirdtopic);
  AssertEquals('c- SubTopics.count', 1, FBroker.SubTopicsCount);
  AssertEquals('c- subscribe topic 0', secondtopic, FBroker.SubTopics[0].topic);

  FBroker.DeleteSubTopic(secondtopic);
  AssertEquals('d- SubTopics.count', 0, FBroker.SubTopicsCount);
end;


procedure TTestBroker.TestAssignBroker;
const
  firsttopic = 'cmnd';
  secondtopic = 'tele/#';
  thirdtopic = 'status';
var
  aBroker: TBroker;
begin
  FBroker.KeepAlives := 30;
  FBroker.ReconnectBackoff := true;
  FBroker.DeleteSubTopic(0);
  FBroker.AddSubTopic(firsttopic, 0, false);
  FBroker.AddSubTopic(secondtopic, 2);
  FBroker.AddSubTopic(thirdtopic, 1, false);

  aBroker := TBroker.create;
  try
    aBroker.assign(FBroker);

    AssertEquals('aBroker SubTopics Count', FBroker.SubTopicsCount, aBroker.SubTopicsCount);
    AssertEquals('aBroker SubTopics Use Count', FBroker.SubTopicsUseCount, aBroker.SubTopicsUseCount);

    AssertEquals('aBroker[0].topic', FBroker.SubTopics[0].topic, aBroker.SubTopics[0].topic);
    AssertEquals('aBroker[1].topic', FBroker.SubTopics[1].topic, aBroker.SubTopics[1].topic);
    AssertEquals('aBroker[2].topic', FBroker.SubTopics[2].topic, aBroker.SubTopics[2].topic);

    AssertTrue('FBroker.isEqual', FBroker.isEqual(aBroker));
    AssertTrue('aBroker.isEqual', aBroker.isEqual(FBroker));
  finally
    aBroker.free;
  end;
end;


procedure TTestBroker.TestSaveToJSON;
var
  jo: TJSONObject;
  E, F: TJSONEnum;
  i: integer;
begin
  jo := TJSONObject.create;
  try
    FBroker.SaveToJSON(jo);
    for E in jo do begin
      case E.key of
        sHostKey : AssertEquals(sHostKey, FBroker.Host, E.Value.AsString);
        sPortKey : AssertEquals(sPortKey, FBroker.Port, E.Value.AsInteger);
        sUserKey : AssertEquals(sUserKey, FBroker.User, E.Value.AsString);
        sPasswordKey : AssertEquals(sPasswordKey, FBroker.Password, E.Value.AsString);
        sPubTopicKey : AssertEquals(sPubTopicKey, FBroker.PubTopic, E.Value.AsString);
        sPubPayloadKey : AssertEquals(sPubPayloadKey, FBroker.PubPayload, E.Value.AsString);
        sPubQoSKey : AssertEquals(sPubQoSKey, FBroker.PubQoS, E.Value.AsInteger);
        sPubRetainKey : AssertEquals(sPubRetainKey, FBroker.PubRetain, E.Value.AsBoolean);
        sSSLKey : AssertEquals(sSSLKey, FBroker.SSL, E.Value.AsBoolean);
        sSSLCertKey : AssertEquals(sSSLCertKey, FBroker.SSLCert, E.Value.AsString);
        sKeepAlivesKey : AssertEquals(sKeepAlivesKey, FBroker.KeepAlives, E.Value.AsInteger);
        sReconnectDelayKey : AssertEquals(sReconnectDelayKey, FBroker.ReconnectDelay, E.Value.AsInteger);
        sReconnectBackoffKey : AssertEquals(sReconnectBackoffKey, FBroker.ReconnectBackoff, E.Value.AsBoolean);
        sSubTopicsKey : begin
           for i := 0 to E.Value.Count-1 do begin
             for F in E.Value.Items[i] do begin
               case F.key of
                 sTopicKey : AssertEquals(Format('Subtopic[%d]', [i]), FBroker.SubTopics[i].topic, F.Value.AsString);
                 sQoSKey : AssertEquals(Format('Subtopic[%d]', [i]), FBroker.SubTopics[i].qos, F.Value.AsInteger);
                 sUseKey: AssertEquals(Format('Subscribed[%d]', [i]), FBroker.SubTopics[i].Use, F.Value.AsBoolean);
                 else
                   Fail('Unknown key "%s" in SubTopic[%d]', [F.key, i])
               end;
             end;
           end; // for i
        end; // SubTopics begin *)
      else
        Fail('Unknown key "%s" in JSON', [E.key])
      end; // case E
    end; // For E
  finally
    freeandnil(jo);
  end;
end;

procedure  TTestBroker.TestLoadFromJSON;
var
  jo: TJSONObject;
  LBroker: TBroker;
begin
  jo := TJSONObject.create;
  LBroker := TBroker.create;
  try
    FBroker.SaveToJSON(jo);
    LBroker.Clear;
    LBroker.LoadFromJSON(jo);

    AssertEquals('Host', DEFAULT_MQTT_HOST, LBroker.host);
    AssertEquals('Port', DEFAULT_MQTT_PORT, LBroker.port);
    AssertEquals('User', '', LBroker.user);
    AssertEquals('Password', '', LBroker.password);
    AssertEquals('SSL', DEFAULT_SSL, LBroker.SSL);
    AssertEquals('SSLCert', DEFAULT_SSLCert, LBroker.SSLCert);

    AssertEquals('KeepAlives', DEFAULT_KEEPALIVES, LBroker.KeepAlives);
    AssertEquals('ReconnectDelay', DEFAULT_RECONNECTDELAY, LBroker.ReconnectDelay);
    AssertEquals('ReconnectBackoff', DEFAULT_RECONNECTBACKOFF, LBroker.ReconnectBackoff);

    AssertEquals('PubTopic', DEFAULT_PUBLISH_TOPIC, LBroker.PubTopic);
    AssertEquals('PubPayload', DEFAULT_PUBLISH_PAYLOAD, LBroker.PubPayload);

    AssertEquals('PubQoS', DEFAULT_PUBQOS, LBroker.PubQoS);
    AssertEquals('PubRetain', DEFAULT_PUBRETAIN, LBroker.PubRetain);

    AssertEquals('first SubTopic', DEFAULT_FIRST_SUBTOPIC, LBroker.SubTopics[0].Topic);
    AssertEquals('first SubTopic QoS', DEFAULT_FIRST_SUBQOS, LBroker.SubTopics[0].QoS);
    AssertEquals('first SubTopic use', DEFAULT_FIRST_SUBUSE, LBroker.SubTopics[0].Use);

    AssertEquals('SubTopics Count', 1, LBroker.SubTopicsCount);
    AssertEquals('SubTopics Use Count', 1, LBroker.SubTopicsUseCount);
  finally
    freeandnil(jo);
    freeandnil(LBroker);
  end;
end;

procedure TTestBroker.TestLoadSaveFile;
var
  aFilename: string;
begin
  aFilename := GetTempFilename();
  if (aFilename = '') then
    aFilename := Format('%~stempxx', [extractfilepath(Paramstr(0))]);
  aFilename := aFilename + '.json';
  FBroker.SaveToFile(aFilename);
  try
    FBroker.Clear;
    FBroker.LoadFromFile(aFilename);
    AssertEquals('loaded Host', DEFAULT_MQTT_HOST, FBroker.host);
    AssertEquals('loaded Port', DEFAULT_MQTT_PORT, FBroker.port);
    AssertEquals('loaded User', '', FBroker.user);
    AssertEquals('loaded Password', '', FBroker.password);
    AssertEquals('loaded SSL', DEFAULT_SSL, FBroker.SSL);
    AssertEquals('loaded SSLCert', DEFAULT_SSLCert, FBroker.SSLCert);

    AssertEquals('loaded KeepAlives', DEFAULT_KEEPALIVES, FBroker.KeepAlives);
    AssertEquals('loaded ReconnectDelay', DEFAULT_RECONNECTDELAY, FBroker.ReconnectDelay);
    AssertEquals('loaded ReconnectBackoff', DEFAULT_RECONNECTBACKOFF, FBroker.ReconnectBackoff);

    AssertEquals('loaded PubTopic', DEFAULT_PUBLISH_TOPIC, FBroker.PubTopic);
    AssertEquals('loaded PubQoS', DEFAULT_PUBQOS, FBroker.PubQoS);
    AssertEquals('loaded PubRetain', DEFAULT_PUBRETAIN, FBroker.PubRetain);

    AssertEquals('loaded first SubTopic', DEFAULT_FIRST_SUBTOPIC, FBroker.SubTopics[0].Topic);
    AssertEquals('loaded first SubTopic QoS', DEFAULT_FIRST_SUBQOS, FBroker.SubTopics[0].QoS);
    AssertEquals('loaded first SubTopic use', DEFAULT_FIRST_SUBUSE, FBroker.SubTopics[0].Use);

    AssertEquals('SubTopics Count', 1, FBroker.SubTopicsCount);
    AssertEquals('SubTopics Use Count', 1, FBroker.SubTopicsUseCount);
  finally
    DeleteFile(aFilename);
  end;
end;

initialization
  RegisterTest(TTestBroker);
end.

