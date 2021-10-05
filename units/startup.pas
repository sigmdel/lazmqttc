unit startup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

var
  configfile: string; // see initialization
  optionsfile: string;

implementation

const
  CONFIGFILENAME = 'default.json';
  OPTIONSFILENAME = 'options.json';

  function Vendor: string;
  begin
    result := 'sigmdel';
  end;

  function  GetAppName: string;
  begin
    result := changefileext(extractfilename(paramstr(0)), '');
  end;

  initialization
    OnGetVendorName := @Vendor;
    OnGetApplicationName := @GetAppName;
    configfile := GetAppConfigDir(false);
    ForceDirectories(configfile);   // create config directory, report error if false ?
    optionsfile := IncludeTrailingPathDelimiter(configfile) + OPTIONSFILENAME;
    configfile := IncludeTrailingPathDelimiter(configfile) + CONFIGFILENAME;
end.

