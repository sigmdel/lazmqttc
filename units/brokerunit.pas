unit brokerunit;

interface

uses SysUtils, Classes, fpJSON;

{$i brokerunit.inc} // Get the default values for TBroker.Init

Type
  { TSubTopicItem }

  TSubTopicItem = class(TObject)
  Private
    FTopic: String;
    FQoS: Integer;
    Fuse: Boolean;
  Public
    Constructor Create(const aTopic: string; aQoS: integer = 0; aUse: boolean = true); virtual;
    Constructor Create(aSubTopicItem: TSubTopicItem); virtual;
    Constructor CreateFromJSON(AJSON: TJSONData); virtual;
    Procedure Assign(source: TSubTopicItem); virtual;
    Function isEqual(anItem: TSubTopicItem): boolean; virtual;
    Procedure LoadFromJSON(AJSON: TJSONData); virtual;
    Function SaveToJSON: TJSONObject; overload;
    Procedure SaveToJSON(AJSON: TJSONObject); overload; virtual;
    Property Topic: String Read FTopic Write FTopic;
    Property QoS: Integer Read FQoS Write FQoS;
    Property Use: Boolean Read Fuse Write FUse;
  end;

  TSubTopics = Array of TSubTopicItem;

  { TBroker }

  TBroker = class(TObject)
  Private
    FHost: String;
    FPort: Integer;
    FUser: String;
    FPassword: String;
    FSSL: Boolean;
    FSSLCert: String;
    FKeepAlives: Integer;
    FReconnectDelay: Integer;
    FReconnectBackoff: Boolean;
    FPubTopic: String;
    FPubPayload: String;
    FPubQoS: integer;
    FPubRetain: boolean;
    FAutoReconnect: boolean;
    FSubTopics: TSubTopics;
    function GetSubTopicsCount: integer;
    function GetSubTopicsUseCount: integer;
  Protected
    Procedure Init;
    Procedure SetSubTopics(AValue: TSubTopics); virtual;
  Public
      { Creates a default MQTT broker class, see brokerunit.inc for it's definition }
    Constructor Create;

      { Creates an MQTT broker that is an exact copy of aBroker }
    Constructor CreateFrom(aBroker: TBroker);

      { Creates an MQTT broker from a JSON definition. See LoadFromJSON and LoadFromFile }
    Constructor CreateFromJSON(AJSON: TJSONData); virtual;

    Destructor Destroy; override;

      { Deep copy of aBroker's definition }
    Procedure Assign(aBroker: TBroker); virtual;

      { Adds a subscribe topic to the SubTopics array }
    Procedure AddSubTopic(aTopic: TSubTopicItem); virtual;

      { Creates and TSubTopicItem, populates it with the given fields and adds it to the SubTopics array }
    Procedure AddSubTopic(const aTopic: string; aQoS: integer = 0; aUse: boolean = true); virtual;

      { Removes all subscribed topics, and erases all the other fields }
    Procedure Clear;

      { Removes all subscribed topics leaving other properties unchanged }
    Procedure ClearSubTopics;

      { Returns the TSubTopicItem specified by its index from the SubTopics array.
        Returns nil if the index is out of range. }
    Function ExtractSubTopic(index: integer): TSubTopicItem; overload; virtual;

      { Returns the TSubTopicItem specified by its topic from the SubTopics array.
        Returns nil if aTopic is not found. }
    Function ExtractSubTopic(const aTopic: string): TSubTopicItem; overload; virtual;

      { Destroys the TSubTopicItem specified by its index and removes it from the SubTopics array.
        Returns false if index is out of range, true otherwise. }
    Function DeleteSubTopic(index: integer): boolean; overload;

      { Destroys the TSubTopicItem specified by its topic and removes it from the SubTopics array.
        Returns false if aTopic is not found, true otherwise. }
    Function DeleteSubTopic(const aTopic: string): boolean; overload;

      { Returns the index of the given topic in the SubTopics array - first
        index is 0.}
    Function IndexOf(const aTopic: string): integer; virtual;

      { Returns true if aBroker has exactly the values for all fields
        including the TSubTopicItems in the SubTopics array }
    Function isEqual(aBroker: TBroker): boolean; virtual;

      { Clears all properties and populates them from values found in
        the given file. It must be a correctly formated JSON file }
    Procedure LoadFromFile(const aFilename: string); virtual;

      { Clears all properties and populates them from values found in
        the JSON object }
    Procedure LoadFromJSON(AJSON: TJSONData); virtual;

      { Saves all properties in a JSON formated text file }
    Procedure SaveToFile(const aFilename: string); virtual;

      { Returns a JSON object containing the values of all properties }
    Function SaveToJSON: TJSONObject; overload;

      { Saves the values of all properties in a JSON object}
    Procedure SaveToJSON(AJSON: TJSONObject); overload; virtual;

      { If true then attempts at reconnecting will be made if connection to the broker is lost }
    Property AutoReconnect: boolean Read FAutoReconnect Write FAutoReconnect;

      { Host name or IP address of the broker. Can use local domain host names in Linux systems}
    Property Host: String Read FHost Write FHost;

      { Broker password. Used in conjunction with the User property to connect to a secured MQTT broker }
    Property Password: String Read FPassword Write FPassword;

      { MQTT TCP Port used by the broker}
    Property Port: Integer Read FPort Write FPort;

      { Time between pings sent by the broker to keep the connection alive }
    Property KeepAlives: Integer Read FKeepAlives Write FKeepAlives;

      { Default publish message topic }
    Property PubTopic: String Read FPubTopic Write FPubTopic;

    Property PubPayload: String Read FPubPayload Write FPubPayload;

      { Default publish topic quality of service  }
    Property PubQoS: integer Read FPubQoS write FPubQoS;

      { Default publish message retain flag }
    Property PubRetain: boolean read FPubRetain write FPubRetain;

       { Number of seconds between reconnect attempts }
    Property ReconnectDelay: Integer Read FReconnectDelay Write FReconnectDelay;

       { Increase reconnect delay exponentially }
    Property ReconnectBackoff: Boolean Read FReconnectBackoff Write FReconnectBackoff;

       { Broker uses SSL encryption - NOT IMPLEMENTED}
    Property SSL: Boolean Read FSSL Write FSSL;

       { SSL certificate - NOT IMPLEMENTED }
    Property SSLCert: String Read FSSLCert Write FSSLCert;

       { Number of TSubTopicItems (subscribe topic records) in the SubTopics array }
    Property SubTopicsCount: integer read GetSubTopicsCount;

       { Number of active subscribe topics.}
    Property SubTopicsUseCount: integer read GetSubTopicsUseCount;

       { Array of TSubTopicItems }
    Property SubTopics: TSubTopics Read FSubTopics Write SetSubTopics;

       { Broker user account. Used in conjunction with the Password
         property to connect to a secured MQTT broker }
    Property User: String Read FUser Write FUser;
  end;


Var
  Broker: TBroker;

implementation

uses
  pwd, JSONParser;

// JSON keys, do not translate these.
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
  sAutoReconnectKey = 'AutoReconnect';
  sPubTopicKey = 'PubTopic';
  sPubPayloadKey = 'PubPayload';
  sPubQoSKey = 'PubQoS';
  sPubRetainKey = 'PubRetain';
  sSubTopicsKey = 'SubTopics';


{ TSubTopics functions }

Procedure ClearArray(Var anArray: TSubTopics);
var
  I: integer;
begin
  For I := 0 to Length(anArray)-1 do
    FreeAndNil(anArray[I]);
  SetLength(anArray,0);
End;

Function CopyTSubTopics(source: TSubTopics): TSubTopics;
var
  I: integer;
begin
  result := nil;
  SetLength(result, length(source));
  for I := 0 to Length(source)-1 do
    result[i] := TSubTopicItem.Create(source[I])
end;

Procedure CopyTSubTopics(source: TSubTopics; var target: TSubTopics); overload;
begin
  ClearArray(target);
  target := CopyTSubTopics(source);
end;

Procedure CopyTSubTopics(source: TSubTopics; target: TObject);
var
  I: integer;
begin
  if target is TStrings then begin
    TStrings(target).Clear;
    for I := 0 to Length(source) do
      TStrings(target).Add(source[i].Topic);
  end
  else
    raise Exception.CreateFmt('Cannot copy TSubTopics to %s', [target.ClassName]);
end;

Function CreateTSubTopics(AJSON: TJSONData): TSubTopics;
var
  I: integer;
begin
  result := nil;
  SetLength(Result, AJSON.Count);
  For I := 0 to AJSON.Count-1 do
    Result[i] := TSubTopicItem.CreateFromJSON(AJSON.Items[i]);
End;

Procedure SaveTSubTopicsToJSON(AnArray: TSubTopics; AJSONArray: TJSONArray);
var
  I: integer;
begin
  For I := 0 to Length(AnArray)-1 do
    AJSONArray.Add(AnArray[i].SaveToJSON);
end;

Function SaveTSubTopicsToJSON(AnArray: TSubTopics): TJSONArray;
begin
  Result := TJSONArray.Create;
  Try
    SaveTSubTopicsToJSON(AnArray,Result);
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;

function SameTopics(Array1, Array2: TSubTopics): boolean;
var
  I: integer;
begin
  // this assumes the order of SubTopics is important
  result := false;
  if length(Array1) <> length(Array2) then
    exit;
  for I := 0 to length(Array1)-1 do
    if not Array1[I].isEqual(Array2[I]) then
      exit;
  (*
  for I := 0 to length(Array1)-1 do begin
    n := indexOf(Array[2].Topic); ????
    if not Array[I].isEqual(Array2[n]) then
      exit;
   end;
  *)
  result := true;
end;

{ TSubTopicItem }

constructor TSubTopicItem.Create(const aTopic: string; aQoS: integer; aUse: boolean);
begin
  if (trim(aTopic) = '') then
    Raise Exception.Create('Cannot subscribe to an empty topic');
  inherited Create;
  FTopic := aTopic;
  FQoS := aQoS;
  FUse := aUse;
end;

constructor TSubTopicItem.Create(aSubTopicItem: TSubTopicItem);
begin
  inherited Create;
  assign(aSubTopicItem);
end;

constructor TSubTopicItem.CreateFromJSON(AJSON: TJSONData);
begin
  inherited Create();
  LoadFromJSON(AJSON);
end;

procedure TSubTopicItem.Assign(source: TSubTopicItem);
begin
  FTopic := source.Topic;
  FQoS := source.QoS;
  FUse := source.Use;
end;

function TSubTopicItem.isEqual(anItem: TSubTopicItem): boolean;
begin
  result := (FTopic = anItem.Topic) and (FQoS = anItem.FQoS) and (FUse = anItem.Use);
end;

procedure TSubTopicItem.LoadFromJSON(AJSON: TJSONData);
var
  E: TJSONEnum;
begin
  for E in AJSON do begin
    case E.Key of
      sTopicKey: Topic := E.Value.AsString;
      sQoSKey:   QoS := E.Value.AsInteger;
      sUseKey:   Use := E.Value.AsBoolean;
    end;
  end;
end;

function TSubTopicItem.SaveToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TSubTopicItem.SaveToJSON(AJSON: TJSONObject);
begin
  AJSON.Add(sTopicKey, Topic);
  AJSON.Add(sQoSKey, QoS);
  AJSON.Add(sUseKey, Use);
end;

{ TBroker }

constructor TBroker.Create;
begin
  inherited Create;
  init;
end;

constructor TBroker.CreateFrom(aBroker: TBroker);
begin
  Create();
  Assign(aBroker);
end;

constructor TBroker.CreateFromJSON(AJSON: TJSONData);
begin
  Create();
  LoadFromJSON(AJSON);
end;

destructor TBroker.Destroy;
begin
  ClearArray(FSubTopics);
  inherited;
end;

procedure TBroker.Assign(aBroker: TBroker);
begin
  FHost := aBroker.Host;
  FPort := aBroker.Port;
  FUser := aBroker.User;
  FPassword := aBroker.Password;
  FSSL := aBroker.SSL;
  FSSLCert := aBroker.SSLCert;
  FKeepAlives := aBroker.KeepAlives;
  FReconnectDelay := aBroker.ReconnectDelay;
  FReconnectBackoff := aBroker.ReconnectBackoff;
  FAutoReconnect := aBroker.AutoReconnect;
  FPubTopic := aBroker.PubTopic;
  FPubPayload := aBroker.PubPayload;
  FPubQoS := aBroker.PubQoS;
  FPubRetain := aBroker.PubRetain;
  CopyTSubTopics(aBroker.SubTopics, FSubTopics);
end;

procedure TBroker.AddSubTopic(aTopic: TSubTopicItem);
begin
  with aTopic do AddSubTopic(Topic, QoS, Use);
end;

procedure TBroker.AddSubTopic(const aTopic: string; aQoS: integer = 0; aUse: boolean = true);
var
  n: integer;
begin
  n := IndexOf(aTopic);
  if n < 0 then begin
    SetLength(FSubTopics, succ(length(FSubTopics)));
    n := pred(Length(FSubTopics));
    FSubTopics[n] := TSubTopicItem.create(aTopic, aQoS, aUse);
  end
  else begin
    FSubTopics[n].FTopic := aTopic;
    FSubTopics[n].FQoS := aQoS;
    FSubTopics[n].FUse := aUse;
  end;
end;

procedure TBroker.Clear;
begin
  FHost := '';
  FPort := DEFAULT_MQTT_PORT;
  FUser := '';
  FPassword := '';
  FSSL := false;
  FSSLCert := '';
  FKeepAlives := 60;
  FReconnectDelay := 0;
  FReconnectBackoff := false;
  FAutoReconnect := true;
  FPubTopic := '';
  FPubPayload := '';
  FPubQoS := 0;
  FPubRetain := false;
  ClearSubTopics;
end;

procedure TBroker.ClearSubTopics;
begin
  ClearArray(FSubTopics);
end;

function TBroker.DeleteSubTopic(index: integer): boolean;
var
  aSubTopicItem: TSubTopicItem;
begin
  aSubTopicItem := ExtractSubTopic(index);
  result := assigned(aSubTopicItem);
  freeAndNil(aSubTopicItem);
end;

function TBroker.DeleteSubTopic(const aTopic: string): boolean;
begin
  result := DeleteSubTopic(IndexOf(aTopic));
end;

function TBroker.ExtractSubTopic(index: integer): TSubTopicItem;
var
  last: integer;
begin
  if (index < 0) or (index >= length(FSubTopics)) then
    result := nil
  else begin
    result := FSubTopics[index];
    last := length(FSubTopics) - 1;
    if index < last then
      move(FSubTopics[index+1], FSubTopics[index], (last - index)*sizeof(TSubTopicItem));
    setlength(FSubTopics, last);
  end;
end;

function TBroker.ExtractSubTopic(const aTopic: string): TSubTopicItem;
begin
  result := ExtractSubTopic(IndexOf(aTopic));
end;

function TBroker.IndexOf(const aTopic: string): integer;
begin
  for result := 0 to length(FSubTopics)-1 do
    if FSubTopics[result].Topic = aTopic then
      exit;
  result := -1;
end;

procedure TBroker.Init;
begin
  FHost := DEFAULT_MQTT_HOST;
  FPort := DEFAULT_MQTT_PORT;
  FUser := DEFAULT_USER;
  FPassword := DEFAULT_PASSWORD;
  FSSL := DEFAULT_SSL;
  FSSLCert := DEFAULT_SSLCERT;
  FKeepalives := DEFAULT_KEEPALIVES;
  FReconnectDelay := DEFAULT_RECONNECTDELAY;
  FReconnectBackoff := DEFAULT_RECONNECTBACKOFF;
  FAutoReconnect := DEFAULT_AUTORECONNECT;
  FPubTopic := DEFAULT_PUBLISH_TOPIC;
  FPubPayload := DEFAULT_PUBLISH_PAYLOAD;
  FPubQoS := DEFAULT_PUBQOS;
  FPubRetain := DEFAULT_PUBRETAIN;
  ClearSubTopics;
  AddSubTopic(
    DEFAULT_FIRST_SUBTOPIC,
    DEFAULT_FIRST_SUBQOS,
    DEFAULT_FIRST_SUBUSE);
end;

function TBroker.isEqual(aBroker: TBroker): boolean;
begin
  result :=
     (FHost = aBroker.FHost) and
     (FPort = aBroker.Port) and
     (FUser = aBroker.User) and
     (FPassword = aBroker.Password) and
     (FSSL = aBroker.SSL) and
     (FSSLCert = aBroker.SSLCert) and
     (FKeepAlives = aBroker.KeepAlives) and
     (FReconnectDelay = aBroker.ReconnectDelay) and
     (FReconnectBackoff = aBroker.ReconnectBackoff) and
     (FAutoReconnect = aBroker.AutoReconnect) and
     (FPubTopic = aBroker.PubTopic) and
     (FPubPayload = aBroker.PubPayload) and
     (FPubQoS = aBroker.PubQoS) and
     (FPubRetain = aBroker.PubRetain) and
     SameTopics(FSubTopics, aBroker.SubTopics);
end;

procedure TBroker.LoadFromFile(const aFilename: string);
Var
  stream : TFileStream;
  jd : TJSONData;
begin
  if aFilename = '' then exit;
  jd := nil;
  stream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    jd := getJSON(stream);
    LoadFromJSON(jd);
  finally
    jd.free;
    stream.free;
  end;
end;

procedure TBroker.LoadFromJSON(AJSON: TJSONData);
var
  E: TJSONEnum;
begin
  Clear;
  for E in AJSON do begin
    case E.Key of
      sHostKey: Host := E.Value.AsString;
      sPortKey: Port := E.Value.AsInteger;
      sUserKey: User := E.Value.AsString;
      sPasswordKey: Password := Decrypt(E.Value.AsString);
      sSSLKey: SSL := E.Value.AsBoolean;
      sSSLCertKey: SSLCert := E.Value.AsString;
      sKeepAlivesKey: KeepAlives := E.Value.AsInteger;
      sReconnectDelayKey: ReconnectDelay := E.Value.AsInteger;
      sReconnectBackoffKey: ReconnectBackoff := E.Value.AsBoolean;
      sAutoReconnectKey: AutoReconnect := E.Value.AsBoolean;
      sPubTopicKey: PubTopic := E.Value.AsString;
      sPubPayloadKey: PubPayload := E.Value.AsString;
      sPubQoSKey: PubQoS := E.Value.AsInteger;
      sPubRetainKey: PubRetain := E.Value.AsBoolean;
      sSubTopicsKey: SubTopics := CreateTSubTopics(E.Value);
       (*
       else: Warning or Error for unknown key ??
       *)
    end;
  end;
end;

function TBroker.GetSubTopicsCount: integer;
begin
  result := Length(FSubTopics);
end;

function TBroker.GetSubTopicsUseCount: integer;
var
  I: integer;
begin
  result := 0;
  for I := 0 to length(FSubTopics)-1 do
    if FSubTopics[I].Use then
      inc(result);
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

function TBroker.SaveToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TBroker.SaveToJSON(AJSON: TJSONObject);
begin
  AJSON.Add(sHostKey, Host);
  AJSON.Add(sPortKey, Port);
  AJSON.Add(sUserKey, User);
  AJSON.Add(sPasswordKey, Encrypt(Password));
  AJSON.Add(sSSLKey, SSL);
  AJSON.Add(sSSLCertKey, SSLCert);
  AJSON.Add(sKeepAlivesKey, KeepAlives);
  AJSON.Add(sReconnectDelayKey, ReconnectDelay);
  AJSON.Add(sReconnectBackoffKey, ReconnectBackoff);
  AJSON.Add(sAutoReconnectKey, AutoReconnect);
  AJSON.Add(sPubTopicKey, PubTopic);
  AJSON.Add(sPubPayloadKey, PubPayload);
  AJSON.Add(sPubQoSKey,  PubQoS);
  AJSON.Add(sPubRetainKey,  PubRetain);
  AJSON.Add(sSubTopicsKey, SaveTSubTopicsToJSON(SubTopics));
end;

procedure TBroker.SetSubTopics(AValue: TSubTopics);
begin
  if (FSubTopics=AValue) then exit;
  ClearArray(FSubTopics);
  FSubTopics := AValue;
end;

initialization
  Broker := TBroker.create;
finalization
  Broker.free;
end.


end.

