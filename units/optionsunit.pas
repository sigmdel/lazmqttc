unit optionsunit;

interface

uses SysUtils, Classes, Dialogs, fpJSON;

{$i options.inc} // Get the default values for TOptions.Init

Type
  
  { TOptions }

  TOptions = class(TObject)
  Private
    FMessagesMaxLines: Integer;
    FAutoClearOnPublish : boolean;
    FAutoconnectOnPublish : Boolean;
    FAutoconnectDelay : Integer;
    FPubMsgHeader : String;
    FSubMsgHeader : String;
    FCopyPubMessages : Boolean;
    FShowTopics : Boolean;
  Protected
    Procedure Init;
  Public
      { Creates a default TOptions class, see options.inc for it's definition }
    Constructor Create;

      { Creates a TOptions that is an exact copy of aOptions }
    Constructor CreateFrom(aOptions: TOptions);

      { Creates a TOptions from a JSON definition. See LoadFromJSON and LoadFromFile }
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;

      { Deep copy of aOptions's definition }
    Procedure Assign(aOptions: TOptions); virtual;

      { Restores default values }
    Procedure Clear; virtual;

      { Returns true if aOptions has exactly the values for all fields }
    Function isEqual(aOptions: TOptions): boolean; virtual;

      { Clears all properties and populates them from values found in
        the given file. It must be a correctly formated JSON file }
    Procedure LoadFromFile(const aFilename: string); virtual;

      { Clears all properties and populates them from values found in
        the JSON object }
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;

      { Saves all properties in a JSON formated text file }
    Procedure SaveToFile(const aFilename: string); virtual;

      { Returns a JSON object containing the values of all properties }
    Function SaveToJSON : TJSONObject; overload;

       { Saves the values of all properties in a JSON object}
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;

    Property MessagesMaxLines: Integer Read FMessagesMaxLines Write FMessagesMaxLines;
    Property AutoClearOnPublish: boolean Read FAutoClearOnPublish Write FAutoClearOnPublish;
    Property AutoconnectOnPublish : Boolean Read FAutoconnectOnPublish Write FAutoconnectOnPublish;
    Property AutoconnectDelay : Integer Read FAutoconnectDelay Write FAutoconnectDelay;
    Property PubMsgHeader : String Read FPubMsgHeader Write FPubMsgHeader;
    Property SubMsgHeader : String Read FSubMsgHeader Write FSubMsgHeader;
    Property CopyPubMessages : Boolean Read FCopyPubMessages Write FCopyPubMessages;
    Property ShowTopics : Boolean Read FShowTopics Write FShowTopics;
  end;

var
  options: TOptions;

implementation

resourcestring
   rsLoadError = 'Error reading options.json: %s';


const
  sMessagesMaxLines = 'MessagesMaxLines';
  sAutoclearOnPublish = 'AutoclearOnPublish';
  sAutoconnectOnPublish = 'AutoconnectOnPublish';
  sAutoconnectDelay = 'AutoconnectDelay';
  sPubMsgHeader = 'PubMsgHeader';
  sSubMsgHeader = 'SubMsgHeader';
  sCopyPubMessages = 'CopyPubMessages';
  sShowTopics = 'ShowTopics';

constructor TOptions.Create;
begin
  inherited Create();
  init();
end;

constructor TOptions.CreateFrom(aOptions: TOptions);
begin
  Create();
  Assign(aOptions);
end;

constructor TOptions.CreateFromJSON(AJSON: TJSONData);
begin
  Create();
  LoadFromJSON(AJSON);
end;

procedure TOptions.Assign(aOptions: TOptions);
begin
  FMessagesMaxLines := aOptions.MessagesMaxLines;
  FAutoclearOnPublish := aOptions.AutoclearOnPublish;
  FAutoconnectOnPublish := aOptions.AutoconnectOnPublish;
  FAutoconnectDelay := aOptions.AutoconnectDelay;
  FPubMsgHeader := aOptions.PubMsgHeader;
  FSubMsgHeader := aOptions.SubMsgHeader;
  FCopyPubMessages := aOptions.CopyPubMessages;
  FShowTopics := aOptions.ShowTopics;
end;

procedure TOptions.Clear;
begin
  init();
end;

procedure TOptions.Init;
begin
  FMessagesMaxLines := DEFAULT_MESSAGES_MAXLINES;
  FAutoclearOnPublish := DEFAULT_AUTOCLEAR_ONPUBLISH;
  FAutoconnectOnPublish := DEFAULT_AUTOCONNECT_ONPUBLISH;
  FAutoconnectDelay := DEFAULT_AUTOCONNECT_DELAY;
  FPubMsgHeader := DEFAULT_PUBMSG_HEADER;
  FSubMsgHeader := DEFAULT_SUBMSG_HEADER;
  FCopyPubMessages := DEFAULT_COPY_PUBMESSAGES;
  FShowTopics := DEFAULT_SHOW_TOPICS;
end;

function TOptions.isEqual(aOptions: TOptions): boolean;
begin
  result := (FMessagesMaxLines = aOptions.MessagesMaxLines) and
    (FAutoclearOnPublish = aOptions.AutoclearOnPublish) and
    (FAutoconnectOnPublish = aOptions.AutoconnectOnPublish) and
    (FAutoconnectDelay = aOptions.AutoconnectDelay) and
    (FPubMsgHeader = aOptions.PubMsgHeader) and
    (FSubMsgHeader = aOptions.SubMsgHeader) and
    (FCopyPubMessages = aOptions.CopyPubMessages) and
    (FShowTopics = aOptions.ShowTopics);
end;

procedure TOptions.LoadFromJSON(AJSON: TJSONData);
var
  E : TJSONEnum;
begin
  for E in AJSON do begin
    case E.Key of
      sMessagesMaxLines:     MessagesMaxLines := E.Value.AsInteger;
      sAutoclearOnPublish:   AutoclearOnPublish := E.Value.AsBoolean;
      sAutoconnectOnPublish: AutoconnectOnPublish := E.Value.AsBoolean;
      sAutoconnectDelay:     AutoconnectDelay := E.Value.AsInteger;
      sPubMsgHeader:         PubMsgHeader := E.Value.AsString;
      sSubMsgHeader:         SubMsgHeader := E.Value.AsString;
      sCopyPubMessages:      CopyPubMessages := E.Value.AsBoolean;
      sShowTopics:           ShowTopics := E.Value.AsBoolean;
    (*
     else: Warning or Error for unknown key ??
    *)
    end;
  end;
end;


(*
procedure TOptions.LoadFromFile(const aFilename: string);
Var
  stream : TFileStream;
  jd : TJSONData;
begin
  if aFilename = '' then exit;
  jd := nil;
  try
    stream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    try
      jd := getJSON(stream);
      LoadFromJSON(jd);
    finally
      jd.free;
      stream.free;
    end;
  except
    MessageDlg('lazmqttc', 'options.json not found', mtInformation, [mbOk], 0);
    //ShowMessage('oops, not found');
  end;
end;
*)

procedure TOptions.LoadFromFile(const aFilename: string);
Var
  stream : TFileStream;
  jd : TJSONData;
begin
  if aFilename = '' then exit;
  if not fileExists(AFilename) then
    SaveToFile(aFilename);
  jd := nil;
  try
    stream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    try
      jd := getJSON(stream);
      LoadFromJSON(jd);
    finally
      jd.free;
      stream.free;
    end;
  except
    on E : Exception do MessageDlg('lazmqttc', format(rsLoadError, [E.Message]), mtInformation, [mbOk], 0);
  end;
end;

function TOptions.SaveToJSON: TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TOptions.SaveToJSON(AJSON: TJSONObject);
begin
  AJSON.Add(sMessagesMaxLines, MessagesMaxLines);
  AJSON.Add(sAutoclearOnPublish, AutoclearOnPublish);
  AJSON.Add(sAutoconnectOnPublish, AutoconnectOnPublish);
  AJSON.Add(sAutoconnectDelay, AutoconnectDelay);
  AJSON.Add(sPubMsgHeader, PubMsgHeader);
  AJSON.Add(sSubMsgHeader, SubMsgHeader);
  AJSON.Add(sCopyPubMessages, CopyPubMessages);
  AJSON.Add(sShowTopics, ShowTopics);
end;

procedure TOptions.SaveToFile(const aFilename: string);
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


initialization
  Options := TOptions.create;
finalization
  Options.free;
end.

