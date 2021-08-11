unit Lb.ConfigIniFile;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TCustomIniFile = class(TObject)
  private
    FStrings: TStrings;
    function GetIndexOf(const AValue: String; AOffSet: Integer = 0): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Strings: TStrings read FStrings;
    function ReadString(const ASection, AKey: String; const ADefault: String = ''): String;
    procedure WriteString(ASection, AKey, AValue: String);
  public
    function ReadInteger(const ASection, AKey: String; const ADefault: Int64 = 0): Int64;
    function ReadDouble(const ASection, AKey: String; const ADefault: Double = 0): Double;
    function ReadBoolean(const ASection, AKey: String; const ADefault: Boolean = False): Boolean;
    function ReadDate(const ASection, AKey: String; const ADefault: TDateTime = 0): TDateTime;
    function ReadTime(const ASection, AKey: String; const ADefault: TDateTime = 0): TDateTime;
    function ReadDateTime(const ASection, AKey: String; const ADefault: TDateTime = 0): TDateTime;
  public
    procedure WriteInteger(const ASection, AKey: String; const AValue: Int64);
    procedure WriteDouble(const ASection, AKey: String; const AValue: Double);
    procedure WriteBoolean(const ASection, AKey: String; const AValue: Boolean);
    procedure WriteDate(const ASection, AKey: String; const AValue: TDateTime);
    procedure WriteTime(const ASection, AKey: String; const AValue: TDateTime);
    procedure WriteDateTime(const ASection, AKey: String; const AValue: TDateTime);
  end;

  TIniFile = class(TCustomIniFile)
  private
    FFileName: String;
    procedure SetLoad;
    procedure SetSave;
  public
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;
  end;

  TMemIniFile = class(TCustomIniFile)
  private
    FFileName: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open;
    procedure UpDateFile;
    property FileName: String read FFileName write FFileName;
  end;

implementation

{ TCustomIniFile }

constructor TCustomIniFile.Create;
begin
  FStrings := TStringList.Create;
end;

destructor TCustomIniFile.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

function TCustomIniFile.GetIndexOf(const AValue: String;
  AOffSet: Integer): Integer;
var
  xS: String;
  xValSection, xValKey: String;
  i, Count: Integer;
begin
  Result := -1;
  Count := FStrings.Count;
  if (Count > 0) and (AOffSet >= 0) and (AOffSet < Count) then
    for i := AOffSet to Count - 1 do
    begin
      xS := FStrings[i];
      xS := Trim(xS);
      xValSection := '[' + Trim(AValue) + ']';
      if SameText(xValSection,xS) then
      begin
        Result := i;
        Break;
      end
      else
      begin
        xValKey := Trim(AValue) + '=';
        if Pos(xValKey,xS) = 1 then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
end;

function TCustomIniFile.ReadString(const ASection, AKey, ADefault: String): String;

  function GetPosComment(AValue: String): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to AValue.Length do
    begin
      if (AValue[i] = ';') or (AValue[i] = '#') then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

  function GetStrToValue(const AKey, AStringValue: String): String;
  var
    xValue: String;
    xLengthKey, xLengthValue: Integer;
  begin
    xLengthKey := Length(AKey + '=');
    xLengthValue := Length(AStringValue) - xLengthKey - GetPosComment(AStringValue);
    xValue := Copy(AStringValue,xLengthKey + 1,xLengthValue);
    Result := xValue;
  end;

var
  xValue: String;
  xIndSection, xIndKey: Integer;
begin
  Result := ADefault;
  xIndSection := GetIndexOf(ASection);
  if xIndSection >= 0 then
  begin
    xIndKey := GetIndexOf(AKey,xIndSection);
    if xIndKey >= 0 then
    begin
      xValue := GetStrToValue(AKey,FStrings[xIndKey]);
      Result := xValue;
    end;
  end;
end;

procedure TCustomIniFile.WriteString(ASection, AKey, AValue: String);
var
  xValue: String;
  xIndSection, xIndKey: Integer;
begin
  xIndSection := GetIndexOf(ASection);
  if xIndSection >= 0 then
  begin
    xIndKey := GetIndexOf(AKey,xIndSection);
    xValue := AKey + '=' + AValue;
    if xIndKey >= 0 then
      FStrings[xIndKey] := xValue
    else
      FStrings.Insert(xIndSection + 1,xValue);
  end
  else
  begin
    FStrings.Add('[' + ASection + ']');
    FStrings.Add(AKey + '=' + AValue);
  end;
end;

function TCustomIniFile.ReadInteger(const ASection, AKey: String;
  const ADefault: Int64): Int64;
var
  xValue: String;
begin
  xValue := Self.ReadString(ASection,AKey);
  Result := StrToInt64Def(xValue,0);
end;

function TCustomIniFile.ReadDouble(const ASection, AKey: String;
  const ADefault: Double): Double;
var
  xValue: String;
  xOldDecimalSeparator: Char;
begin
  xValue := Self.ReadString(ASection,AKey);
  xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloatDef(xValue,0);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

function TCustomIniFile.ReadBoolean(const ASection, AKey: String;
  const ADefault: Boolean): Boolean;
var
  xVal: Integer;
  xValue: String;
begin
  xValue := Self.ReadString(ASection,AKey);
  if not xValue.IsEmpty then
  begin
    case xValue[1] of
      'F','f': Result := False;
      'T','t': Result := True;
    else
      xVal := StrToInt64Def(xValue,0);
      Result := Boolean(xVal);
    end;
  end
  else
    Result := ADefault;
end;

function TCustomIniFile.ReadDate(const ASection, AKey: String;
  const ADefault: TDateTime): TDateTime;
var
  xValue: String;
begin
  xValue := Self.ReadString(ASection,AKey);
  Result := StrToDateDef(xValue,ADefault);
end;

function TCustomIniFile.ReadTime(const ASection, AKey: String;
  const ADefault: TDateTime): TDateTime;
var
  xValue: String;
begin
  xValue := Self.ReadString(ASection,AKey);
  Result := StrToTimeDef(xValue,ADefault);
end;

function TCustomIniFile.ReadDateTime(const ASection, AKey: String;
  const ADefault: TDateTime): TDateTime;
var
  xValue: String;
begin
  xValue := Self.ReadString(ASection,AKey);
  Result := StrToDateTimeDef(xValue,ADefault);
end;

procedure TCustomIniFile.WriteInteger(const ASection, AKey: String;
  const AValue: Int64);
begin
  WriteString(ASection,AKey,IntToStr(AValue));
end;

procedure TCustomIniFile.WriteDouble(const ASection, AKey: String;
  const AValue: Double);
var
  xOldDecimalSeparator: Char;
begin
  xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  WriteString(ASection,AKey,FloatToStr(AValue));
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

procedure TCustomIniFile.WriteBoolean(const ASection, AKey: String;
  const AValue: Boolean);
var
  xValue: String;
begin
  if AValue then
    xValue := 'true'
  else
    xValue := 'false';
  WriteString(ASection,AKey,xValue);
end;

procedure TCustomIniFile.WriteDate(const ASection, AKey: String;
  const AValue: TDateTime);
begin
  WriteString(ASection,AKey,DateToStr(AValue));
end;

procedure TCustomIniFile.WriteTime(const ASection, AKey: String;
  const AValue: TDateTime);
begin
  WriteString(ASection,AKey,TimeToStr(AValue));
end;

procedure TCustomIniFile.WriteDateTime(const ASection, AKey: String;
  const AValue: TDateTime);
begin
  WriteString(ASection,AKey,DateTimeToStr(AValue));
end;

{ TIniFile }

constructor TIniFile.Create(const AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  SetLoad;
end;

destructor TIniFile.Destroy;
begin
  SetSave;
  inherited;
end;

procedure TIniFile.SetLoad;
begin
  FStrings.LoadFromFile(FFileName);
end;

procedure TIniFile.SetSave;
begin
  FStrings.SaveToFile(FFileName);
end;


{ TMemIniFile }

constructor TMemIniFile.Create;
begin
  inherited Create;
  FFileName := '';
end;

destructor TMemIniFile.Destroy;
begin
  Self.UpDateFile;
  inherited;
end;

procedure TMemIniFile.Open;
begin
  FStrings.LoadFromFile(FFileName,TEncoding.UTF8);
end;

procedure TMemIniFile.UpDateFile;
begin
  FStrings.SaveToFile(FFileName,TEncoding.UTF8);
end;

end.
