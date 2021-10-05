(*
  String Encryption and Decryption Functions using Free Pascal Blowfish unit

  Based on
    Blowfish, the cryptography unit by leledumbo (June 24, 2012)
    @ http://pascalgeek.blogspot.com/2012/06/encryption-decryption-and-asynchronous.html
*)

unit pwd;

{$mode objfpc}{$H+}

interface

uses
  brokeredit, report;

const
  DEFAULT_KEY = 'CdBZoYYSWjPiZV8fxbN62a4TK8cSDNDKQoAmA73t3qX';

var
  DefaultKey: string = DEFAULT_KEY;

function Encrypt(value: string; encode: boolean = true; const aKey: string = ''): string;
function Decrypt(value: string; encoded: boolean = true; const aKey: string = ''): string;

implementation

uses
  SysUtils, Classes, base64, BlowFish, startup;

function Encrypt(value: string;  encode: boolean; const aKey: string): string;
var
  Key: string;
  ss: TStringStream;
  en: TBlowFishEncryptStream;
begin
  result := '';
  if value = '' then
    exit;
  if aKey = '' then
    Key := DefaultKey
  else
    Key := aKey;

  ss := TStringStream.Create('');
  try
    en := TBlowFishEncryptStream.Create(Key, ss);
    try
      en.WriteAnsiString(value);  // must be freed before ss is read!!!
    finally
      en.Free;
    end;
  finally
    result := ss.DataString;
    ss.free;
    if encode then
      result := EncodeStringBase64(result);
  end;
end;

function Decrypt(value: string; encoded: boolean; const aKey: string): string;
var
  Key, src: string;
  ss: TStringStream;
  de: TBlowFishDecryptStream;
begin
  result := '';
  if value = '' then
    exit;
  if aKey = '' then
    Key := DefaultKey
  else
    Key := aKey;

  if encoded then
    src := DecodeStringBase64(value)
  else
    src := value;
  ss := TStringStream.Create(src);
  try
    de := TBlowFishDeCryptStream.Create(Key, ss);
    try
      result :=  de.ReadAnsiString;
    finally
      de.free;
    end;
  finally
    ss.free;
  end;
end;

procedure GetDefaultKey;
var
  fname: string;
  inf: textfile;
begin
  fname := ExtractFilePath(configfile) + 'key.txt';
  {$i-}
  assign(inf, fname);
  reset(inf);
  {$i+}
  if (IoResult=0) then begin
    read(inf, DefaultKey);
    close(inf);
  end;
end;

initialization
  GetDefaultKey;
end.

