unit Lb.SearchFile;

interface

uses
//  System.SysUtils,
//  System.Variants,
//  System.Classes,
//  System.Generics.Collections,
//  System.IOUtils,

  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Generics.Collections,
  Lb.Params;

type
  TInfoFile = class(TObject)
  private
    FFileName: String;
    FPath: String;
    FDateTime: TDateTime;
  public
    procedure SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
    property FileName: String read FFileName;
    property Path: String read FPath;
    property DateTime: TDateTime read FDateTime;
  end;

  TInfoFiles = class(TObjectList<TInfoFile>)
  private
    FOnEventParams: TNotifyEventParams;
    procedure DoEventParams(const APathFileName: String; ADateTime: TDateTime);
  public
    function IndexOfDateTime(const ADateTime: TDateTime): Integer;
    procedure SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
    property OnEventParams: TNotifyEventParams write FOnEventParams;
  end;

implementation

uses
  System.Threading;

procedure SetSearchFile(const AInfoFiles: TInfoFiles; const APath: String);
var
  xS: String;
  i: Integer;
  xSDA: TStringDynArray;
  xParams: TParams;
begin
  if not Assigned(AInfoFiles) then
    Exit;
  // Файлы
  xSDA := TDirectory.GetFiles(APath, '*.*');
  for xS in xSDA do
  begin
    var xFile := xSDA[i];
    var xLastWriteTime := TFile.GetLastWriteTime(xFile);
    AInfoFiles.SetInfoFile(xFile,xLastWriteTime);
  end;
  // Папки
  xSDA := TDirectory.GetDirectories(APath);
  for xS in xSDA do
    SetSearchFile(AInfoFiles,xS);
end;

{ TInfoFile }

procedure TInfoFile.SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
begin
  FPath     := ExtractFilePath(APathFileName);
  FFileName := ExtractFileName(APathFileName);
  FDateTime := ADateTime;
end;

{ TInfoFiles }

procedure TInfoFiles.DoEventParams(const APathFileName: String; ADateTime: TDateTime);
var
  xParams: TParams;
begin
  if Assigned(FOnEventParams) then
  begin
    try
      xParams := TParams.Create;
      xParams.ParamByName('path_file_name').AsString := APathFileName;
      xParams.ParamByName('date_time').AsDateTime := ADateTime;
      FOnEventParams(Self,xParams);
    finally
      FreeAndNil(xParams);
    end;
  end;
end;

function TInfoFiles.IndexOfDateTime(const ADateTime: TDateTime): Integer;
var
  xInfoFile: TInfoFile;
  i, Count: Integer;
begin
  Result := -1;
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xInfoFile := Self.Items[i];
      if xInfoFile.DateTime < ADateTime then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure TInfoFiles.SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
var
  xInfoFile: TInfoFile;
begin
  xInfoFile := TInfoFile.Create;
  xInfoFile.SetInfoFile(APathFileName,ADateTime);
  var xIndex :=  Self.IndexOfDateTime(ADateTime);
  if xIndex >= 0 then
  begin
    xIndex := IndexOfDateTime(ADateTime);
    Self.Insert(xIndex,xInfoFile);
    DoEventParams(APathFileName,ADateTime);
  end
  else
  begin
    Self.Add(xInfoFile);
    DoEventParams(APathFileName,ADateTime);
  end;
end;

end.
