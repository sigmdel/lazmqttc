unit brokertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  brokerunit;

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
    procedure TestAssignToStringList;
  end;

implementation

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
  AssertEquals('default PubTopic', '', FBroker.PubTopic);
  AssertEquals('default first SubTopic', DEFAULT_FIRST_SUBSCRIBE_TOPIC, FBroker.SubTopics[0]);
  AssertEquals('default first topic Subscribed', DEFAULT_SUBSCRIBED_FIRST_TOPIC, FBroker.Subscribed[0]);
  AssertEquals('(subscribe) Count', 1, FBroker.count);
  AssertEquals('susbscribedCount', 1, FBroker.SubscribedCount);
end;

procedure TTestBroker.TestSubscribed;
begin
  FBroker.Subscribed[0] := false;
  AssertFalse('Topic not subscribed', FBroker.Subscribed[0]);
  FBroker.Subscribed[0] := true;
  AssertTrue('Topic subscribed', FBroker.Subscribed[0]);
end;

procedure TTestBroker.TestAddSubTopic;
const
  secondtopic = 'tele/#';
  thirdtopic = 'status';
begin
  FBroker.AddSubTopic(secondtopic, false);
  AssertEquals('Second subscribe topic', secondtopic, FBroker.SubTopics[1]);
  AssertFalse('Second topic subscribed', FBroker.Subscribed[1]);

  FBroker.AddSubTopic(thirdtopic);
  AssertEquals('Third subscribe topic', thirdtopic, FBroker.SubTopics[2]);
  AssertTrue('Third topic subscribed', FBroker.Subscribed[2]);

  AssertEquals('count', 3, FBroker.count);
  AssertEquals('SubscribedCount', 2, FBroker.SubscribedCount);
end;

procedure TTestBroker.TestDeleteSubTopic;
const
  secondtopic = 'tele/#';
  thirdtopic = 'status';
begin
  FBroker.AddSubTopic(secondtopic);
  FBroker.AddSubTopic(thirdtopic);
  AssertEquals('a- SubTopics.count', 3, FBroker.count);
  AssertEquals('a- subscribe topic 0', '#', FBroker.SubTopics[0]);
  AssertEquals('a- subscribe topic 1', secondtopic, FBroker.SubTopics[1]);
  AssertEquals('a- subscribe topic 2', thirdtopic, FBroker.SubTopics[2]);

  FBroker.DeleteSubTopic(0);
  AssertEquals('b- SubTopics.count', 2, FBroker.count);
  AssertEquals('b- subscribe topic 0', secondtopic, FBroker.SubTopics[0]);
  AssertEquals('b- subscribe topic 1', thirdtopic, FBroker.SubTopics[1]);

  FBroker.DeleteSubTopic(thirdtopic);
  AssertEquals('c- SubTopics.count', 1, FBroker.count);
  AssertEquals('c- subscribe topic 0', secondtopic, FBroker.SubTopics[0]);

  FBroker.DeleteSubTopic(secondtopic);
  AssertEquals('d- SubTopics.count', 0, FBroker.count);
end;


procedure TTestBroker.TestAssignBroker;
const
  firsttopic = 'cmnd';
  secondtopic = 'tele/#';
  thirdtopic = 'status';
var
  aBroker: TBroker;
begin
  FBroker.DeleteSubTopic(0);
  FBroker.AddSubTopic(firsttopic, false);
  FBroker.AddSubTopic(secondtopic);
  FBroker.AddSubTopic(thirdtopic, false);

  aBroker := TBroker.create;
  try
    aBroker.assign(FBroker);

    AssertEquals('aBroker.count', 3, aBroker.count);
    AssertEquals('aBroker.SubscribedCount', 1, aBroker.SubscribedCount);

    AssertEquals('aBroker[0]', firsttopic, aBroker[0]);
    AssertEquals('aBroker[1]', secondtopic, aBroker[1]);
    AssertEquals('aBroker[2]', thirdtopic, aBroker[2]);
  finally
    aBroker.free;
  end;
end;

procedure TTestBroker.TestAssignToStringList;
const
  secondtopic = 'tele/#';
  thirdtopic = 'status';
var
  sl: TStringList;
begin
  FBroker.AddSubTopic(secondtopic);
  FBroker.AddSubTopic(thirdtopic, false);

  sl := TStringList.create;
  try
    FBroker.assignTo(sl);
    AssertEquals('sl.count', 3, sl.count);
    AssertEquals('sl[0]', DEFAULT_FIRST_SUBSCRIBE_TOPIC, sl[0]);
    AssertEquals('sl.objects[0]', DEFAULT_SUBSCRIBED_FIRST_TOPIC, assigned(sl.objects[0]));

    AssertEquals('sl[1]', secondtopic, sl[1]);
    AssertTrue('sl.objects[1] assigned', assigned(sl.objects[1]));
    AssertEquals('sl[2]', thirdtopic, sl[2]);
    AssertFalse('sl.object[2] not assigned', assigned(sl.objects[2]));
  finally
    freeandnil(sl);
  end;
end;



initialization
  RegisterTest(TTestBroker);
end.

