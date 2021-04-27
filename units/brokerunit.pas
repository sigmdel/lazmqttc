unit brokerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, jsonscanner, JSONParser;

{$i brokerunit.inc} // Get the default values for TBroker.Init

type
  
  { TBroker }

  TBroker = class
  private
    FModified: boolean;
    FHost: string;
    FPort: integer;
    FUser: string;
    FPassword: string;
    FPubTopic: string;
    FSubTopics: TStrings;
    function GetCount: integer;
    function GetSubscribed(index: integer): boolean;
    function GetSubscribedCount: integer;
    function GetSubTopics(index: integer): string;
    procedure SetHost(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: integer);
    procedure SetPubTopic(AValue: string);
    procedure SetSubscribed(index: integer; AValue: boolean);
    procedure SetSubTopics(index: integer; AValue: string);
    procedure SetUser(AValue: string);
  public
    constructor create;
    destructor destroy; override;

      { Add an MQTT subscribe topic. The subscribed parameter is a flag
        used to determine if the client will subscribe or not to the topic}
    procedure AddSubTopic(const aTopic: string; subscribed: boolean = true);

      { Copy all the properties from the source TBroker to self }
    procedure Assign(Source: TBroker);

      { Copy all pertinent properties to the target object.
        If the target is a TBroker then all properties are copied.
        If the target is a TStrings then all subscribe topics and
        and subscription flags are assigned to the target}
    procedure AssignTo(Target: TObject);
    procedure Clear;
    property Count: integer read GetCount;
    procedure DeleteSubTopic(index: integer);
    procedure DeleteSubTopic(const aTopic: string);
    procedure ExchangeSubTopics(Index1, Index2: Integer);
    procedure Init;
    procedure LoadFromFile(const aFilename: string);
    procedure LoadFromJSON(aJSON: TJSONObject);
    procedure SaveToFile(const aFileName: string);
    procedure SaveToJSON(aJSON: TJSONObject);
    procedure SubTopicsClear;
    property Host: string read FHost write SetHost;
    property Modified: boolean read FModified write FModified;
    property Password: string read FPassword write SetPassword;
    property Port: integer read FPort write SetPort;
    property PubTopic: string read FPubTopic write SetPubTopic;
    property SubscribedCount: integer read GetSubscribedCount;
    property Subscribed[index: integer]: boolean read GetSubscribed write SetSubscribed;
    property SubTopics[index: integer]: string read GetSubTopics write SetSubTopics; default;
    property User: string read FUser write SetUser;
  end;

var
  Broker: TBroker;

implementation

uses
  StdCtrls, CheckLst;

// JSON keys, do not translate these.
const
  sHostKey = 'Host';
  sPortKey = 'Port';
  sUserKey = 'User';
  sPasswordKey = 'Password';
  sPubTopicKey = 'PubTopic';
  sSubTopicsKey = 'SubTopics';
  sTopicKey = 'topic';
  sUseKey = 'use';

{ TBroker }

constructor TBroker.create;
begin
  inherited;
  FSubTopics := TStringList.create;
  TStringList(FSubTopics).Duplicates := dupIgnore;
  TStringList(FSubTopics).Sorted := false;
  TStringList(FSubTopics).OwnsObjects := false;
  Init;
end;

destructor TBroker.destroy;
begin
  FSubTopics.free;
  inherited destroy;
end;

procedure TBroker.AddSubTopic(const aTopic: string; subscribed: boolean);
begin
  FSubTopics.AddObject(aTopic, TObject(ptrint(ord(subscribed))));
  FModified := true;
end;

procedure TBroker.Assign(Source: TBroker);
var
  i: integer;
begin
  Clear;
  FHost := Source.FHost;
  FPort := Source.FPort;
  FPubTopic := Source.PubTopic;
  FUser := Source.User;
  FPassword := Source.Password;
  for i := 0 to Source.FSubTopics.Count-1 do
     FSubTopics.AddObject(Source.FSubTopics[i], Source.FSubTopics.objects[i]);
  FModified := false;
end;

procedure TBroker.AssignTo(Target: TObject);
var
  i: integer;
begin
  if Target is TBroker then
    TBroker(Target).Assign(self)
  else if Target is TCustomListBox then begin
     AssignTo(TCustomListBox(Target).Items);
     if Target is TCustomCheckListBox then begin
       for i := 0 to FSubTopics.Count-1 do
         TCustomCheckListBox(Target).Checked[i] := Subscribed[i];
     end;
  end
  else if Target is TStrings then begin
    TStrings(Target).Clear;
    for i := 0 to FSubTopics.Count-1 do
       TStrings(Target).AddObject(FSubTopics[i], FSubTopics.objects[i]);
  end else
    raise Exception.Create('Not yet implemented');
end;

procedure TBroker.Clear;
begin
  FHost := '';
  FPort := DEFAULT_MQTT_PORT;
  FPubTopic := '';
  FUser := '';
  FPassword := '';
  FSubTopics.Clear;
  FModified := false;
end;

procedure TBroker.DeleteSubTopic(index: integer);
begin
  FSubTopics.Delete(index);
  FModified := true;
end;

procedure TBroker.DeleteSubTopic(const aTopic: string);
begin
  DeleteSubTopic(FSubTopics.IndexOf(aTopic));
end;

procedure TBroker.ExchangeSubTopics(Index1, Index2: Integer);
begin
  FSubTopics.Exchange(Index1, Index2);
end;

procedure TBroker.Init;
begin
  FHost := DEFAULT_MQTT_HOST;
  FPort := DEFAULT_MQTT_PORT;
  FUser := DEFAULT_USER;
  FPassword := DEFAULT_PASSWORD;
  FPubTopic := DEFAULT_PUBLISH_TOPIC;
  AddSubTopic(DEFAULT_FIRST_SUBSCRIBE_TOPIC, DEFAULT_SUBSCRIBED_FIRST_TOPIC);
end;

procedure TBroker.LoadFromFile(const aFilename: string);
Var
  stream : TFileStream;
  P : TJSONParser;
  D : TJSONData;
begin
  if aFilename = '' then exit;
  Clear;
  stream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
  {$IF FPC_FULLVERSION>=30002}
    P:=TJSONParser.Create(stream, []);
    try
      P.Options:=P.Options+[joStrict];
  {$ELSE}
    P:=TJSONParser.Create(stream);
    try
  {$ENDIF}
      D:=P.Parse;
    finally
      P.Free;
    end;
  finally
    stream.Free;
  end;
  LoadFromJSON(D as TJSONObject);
  D.free;
end;



procedure TBroker.LoadFromJSON(aJSON: TJSONObject);
var
  E, F: TJSONEnum;
  i: integer;
  topic: string;
  use: boolean;
begin
  for E in aJSON do begin
    case E.key of
      sHostKey : Host := E.Value.AsString;
      sPortKey : Port := E.Value.AsInteger;
      sUserKey : User := E.Value.AsString;
      sPasswordKey : Password := E.Value.AsString;
      sPubTopicKey : PubTopic := E.Value.AsString;
      sSubTopicsKey : begin
         SubTopicsClear;
         for i := 0 to E.Value.Count-1 do begin
           topic := '';
           use := false;
           for F in E.Value.Items[i] do begin
             case F.key of
               '': ; // do nothing
               sTopicKey : topic := F.Value.AsString;
               sUseKey: use := F.Value.AsInteger = 1;
             end;
           end;
           if topic <> '' then
             AddSubTopic(topic, use);
         end; // for i
      end; // SubTopics begin
    end; // case E
  end; // For E
end;

procedure TBroker.SaveToFile(const aFilename: string);
var
  aJSON: TJSONObject;
  jsonText: string;
  stream: TFileStream;
begin
  if aFilename = '' then exit;
  jsonText := '';
  aJSON := TJSONObject.create;
  try
    SaveToJSON(aJSON);
    jsonText := aJSON.AsJSON;
  finally
    freeandnil(aJSON);
  end;
  if jsonText = '' then exit;
  stream := TFileStream.Create(AFileName,fmCreate);
  try
    stream.WriteBuffer(jsonText[1], Length(jsonText));
  finally
    stream.Free;
  end;
end;

procedure TBroker.SaveToJSON(aJSON: TJSONObject);
var
  arrayJSON: TJSONArray;
  i: integer;

  function saveTopic(index: integer): TJSONObject;
  begin
    result := TJSONObject.create;
    try
      result.Add(sTopicKey, SubTopics[index]);
      result.Add(sUseKey, ord(Subscribed[index]));
    except
      freeandnil(result);
      raise;
    end;
  end;

begin
  AJSON.Add(sHostKey, Host);
  AJSON.Add(sPortKey, Port);
  AJSON.Add(sUserKey, User);
  AJSON.Add(sPasswordKey, Password);
  AJSON.Add(sPubTopicKey, PubTopic);
  arrayJSON := TJSONArray.create;
  try
    for i := 0 to Count-1 do
       arrayJSON.add(saveTopic(i));
  except
    freeandnil(arrayJSON);
    raise
  end;
  AJSON.Add(sSubTopicsKey, arrayJSON);
end;

function TBroker.GetCount: integer;
begin
  result := FSubTopics.Count;
end;

function TBroker.GetSubscribed(index: integer): boolean;
begin
  result := assigned(FSubTopics.Objects[index]);
end;

function TBroker.GetSubscribedCount: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to FSubTopics.count-1 do
     if GetSubscribed(i) then
       inc(result);
end;

function TBroker.GetSubTopics(index: integer): string;
begin
  result := FSubTopics[index];
end;

procedure TBroker.SetHost(AValue: string);
begin
  if FHost=AValue then Exit;
  FHost:=AValue;
  FModified := true;
end;

procedure TBroker.SetPassword(AValue: string);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
  FModified := true;
end;

procedure TBroker.SetPort(AValue: integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
  FModified := true;
end;

procedure TBroker.SetPubTopic(AValue: string);
begin
  if FPubTopic=AValue then Exit;
  FPubTopic:=AValue;
  FModified := true;
end;

procedure TBroker.SetSubscribed(index: integer; AValue: boolean);
begin
  if GetSubscribed(index) = AValue then Exit;
  FSubTopics.objects[index] := TObject(ptrint(ord(AValue)));
  FModified := true;
end;

procedure TBroker.SetSubTopics(index: integer; AValue: string);
begin
  if GetSubTopics(index) = AValue then Exit;
  FSubTopics[index] := Avalue;
  FModified := true;
end;

procedure TBroker.SetUser(AValue: string);
begin
  if FUser=AValue then Exit;
  FUser:=AValue;
  FModified := True;
end;

procedure TBroker.SubTopicsClear;
begin
  if FSubTopics.Count > 0 then begin
    FSubTopics.Clear;
    FModified := true;
  end;
end;

initialization
  Broker := TBroker.create;
finalization
  Broker.free;
end.

