unit Lb.Search.Files;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TDirect = class;
  TDirectList = TObjectList<TDirect>;

  TDirect = class(TObject)
  public type

    TFile = class(TObject)
    private
      FName: String;
      FFullName: String;
      procedure SetFullName(const Value: String);
    public
      constructor Create;
      destructor Destroy; override;
      property Name: String read FName;
      property FullName: String read FFullName write SetFullName;
    end;

    TFileList = TObjectList<TFile>;

  private
    FPath: String;
    FName: String;
    FFiles: TFileList;
    FDirects: TDirectList;
    procedure SetPath(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Path: String read FPath write SetPath;
    property Name: String read FName;
    property Directs: TDirectList read FDirects;
    property Files: TFileList read FFiles;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TDirect.TFile }

constructor TDirect.TFile.Create;
begin
  FFullName := '';
  FName := '';
end;

destructor TDirect.TFile.Destroy;
begin

  inherited;
end;

procedure TDirect.TFile.SetFullName(const Value: String);
begin
  FFullName := Value;
  FName := ExtractFileName(FFullName);
end;

{ TDirect }

constructor TDirect.Create;
begin
  FPath := '';
  FFiles := TFileList.Create;
  FDirects := TDirectList.Create;
end;

destructor TDirect.Destroy;
begin
  FreeAndNil(FDirects);
  FreeAndNil(FFiles);
  inherited;
end;

procedure TDirect.SetPath(const Value: String);

  function GetPos: Integer;
  var
    xC: Char;
    i, L: Integer;
  begin
    Result := 1;
    L := Length(Value);
    for i := L downto 1 do
    begin
      xC := Value[i];
      if xC = PathDelim then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

  function GetParserName: String;
  var
    xIndex: Integer;
  begin
    Result := '';
    xIndex := GetPos;
    if xIndex > 1 then
      Result := Copy(Value,xIndex + 1,Value.Length - xIndex);
  end;

var
  xFile: TDirect.TFile;
  xSDA: TStringDynArray;
  i: Integer;
  xS: String;
  xDirect: TDirect;
begin
  FPath := Value;
  FName := GetParserName;
  if not FPath.IsEmpty then
  begin
    // Файл
    xSDA := TDirectory.GetFiles(FPath, '*.*');
    for i := 0 to High(xSDA) do
    begin
      xFile := TDirect.TFile.Create;
      xFile.FullName := xSDA[i];
      FFiles.Add(xFile);
    end;
    // Папка
    xSDA := TDirectory.GetDirectories(FPath);
    for xS in xSDA do
    begin
      xDirect := TDirect.Create;
      xDirect.Path := xS;
      FDirects.Add(xDirect);
    end;

  end;
end;



end.
