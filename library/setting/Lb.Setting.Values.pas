unit Lb.Setting.Values;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

type
  TConfig = class(TObject)
  public type
    ///<summary>Значение работы</summary>
    TValue = class(TObject)
    private
      FConfig: TConfig;
    private
      FName: String;
      FValue: String;
      function GetAsDouble: Double;
      function GetAsInteger: Integer;
      function GetAsString: String;
      procedure SetAsDouble(const Value: Double);
      procedure SetAsInteger(const Value: Integer);
      procedure SetAsString(const Value: String);
    protected
      property Config: TConfig read FConfig;
      property Value: String read FValue write FValue;
    public
      constructor Create(const AConfig: TConfig); virtual;
      destructor Destroy; override;
      property Name: String read FName write FName;
    public
      property AsString: String read GetAsString write SetAsString;
      property AsInteger: Integer read GetAsInteger write SetAsInteger;
      property AsDouble: Double read GetAsDouble write SetAsDouble;
      property ValueString: String read FValue;
    end;
    TValueList = TObjectList<TValue>;

    ///<summary>Секция</summary>
    TSection = class(TObject)
    private
      FConfig: TConfig;
    private
      FName: String;
      FValues: TValueList;
      function GetValueByName(AName: String): TConfig.TValue;
    protected
      property Config: TConfig read FConfig;
      function GetCreateValue(const AName: String): TConfig.TValue;
      function GetIndexOf(const AName: String): Integer;
    public
      constructor Create(const AConfig: TConfig); virtual;
      destructor Destroy; override;
      property Name: String read FName write FName;
      property ValueByName[AName: String]: TConfig.TValue read GetValueByName;
      property Values: TValueList read FValues;
    end;
    TSectionList = TObjectList<TSection>;

  private
    FName: String;
    FSections: TSectionList;
    function GetCreateSection(const AName: String): TConfig.TSection;
    function GetIndexOf(const AName: String): Integer;
    function GetSectionByName(AName: String): TConfig.TSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    property SectionByName[AName: String]: TConfig.TSection read GetSectionByName;
    property Sections: TSectionList read FSections;
    property Name: String read FName write FName;
  end;

type
  TConfigStrings = class(TConfig)
  private
    FSource: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    property Source: TStrings read FSource;
  end;

  TConfigIniFile = class(TConfigStrings)
  private
    FFileName: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    property FileName: String read FFileName write FFileName;
  end;


function GetConfigIniFile(const APath, AConfigName: String): TConfig; overload;
function GetConfigIniFile(const AConfigName: String): TConfig; overload;

implementation

type
  TConfigList = TObjectList<TConfig>;

var
  localConfigs: TConfigList = nil;

type
  TConfigs = record
    class procedure SetInitialization; static;
    class procedure SetFinalization; static;
    class function GetIndexOf(const AName: String): Integer; static;
    class function GetCreateConfigIniFile(const APath, AConfigName: String): TConfig; static;
    class function GetCreateConfigStrings(const AConfigName: String; const AStrings: TStrings): TConfig; static;
  end;

function GetConfigIniFile(const APath, AConfigName: String): TConfig;
begin
  Result := TConfigs.GetCreateConfigIniFile(APath,AConfigName);
end;

function GetConfigIniFile(const AConfigName: String): TConfig;
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'config' + PathDelim;
  CreateDir(xPath);
  Result := TConfigs.GetCreateConfigIniFile(xPath,AConfigName);
end;

{ TConfigs }

class procedure TConfigs.SetInitialization;
begin
  if not Assigned(localConfigs) then
    localConfigs := TConfigList.Create;
end;

class procedure TConfigs.SetFinalization;
begin
  if Assigned(localConfigs) then
    FreeAndNil(localConfigs);
  localConfigs := nil;
end;

class function TConfigs.GetIndexOf(const AName: String): Integer;
begin
  Result := -1;
  if localConfigs.Count > 0 then
    for var i := 0 to localConfigs.Count - 1 do
    begin
      var xConfig := localConfigs[i];
      if SameText(xConfig.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

class function TConfigs.GetCreateConfigIniFile(const APath, AConfigName: String): TConfig;
var
  xConfig: TConfig;
  xIndex: Integer;
begin
  xIndex := TConfigs.GetIndexOf(AConfigName);
  if xIndex < 0 then
  begin
    xConfig := TConfigIniFile.Create;
    xConfig.Name := AConfigName;
    TConfigIniFile(xConfig).FileName := APath + AConfigName;
    localConfigs.Add(xConfig);
  end
  else
    xConfig := localConfigs[xIndex];
  Result := xConfig;
end;


class function TConfigs.GetCreateConfigStrings(const AConfigName: String; const AStrings: TStrings): TConfig;
var
  xConfig: TConfig;
  xIndex: Integer;
begin
  xIndex := TConfigs.GetIndexOf(AConfigName);
  if xIndex < 0 then
  begin
    xConfig := TConfigStrings.Create;
    xConfig.Name := AConfigName;
    TConfigStrings(xConfig).Source.Assign(AStrings);
    localConfigs.Add(xConfig);
  end
  else
    xConfig := localConfigs[xIndex];
  Result := xConfig;
end;


function GetStrToNR(const S: String): String;
begin
  var xS := '';
  for var xC in S do
  begin
    case xC of
      #10: xS := xS + '\n';
      #13: xS := xS + '\r';
    else
      xS := xS + xC;
    end;
  end;
  Result := xS;
end;

function GetNRToStr(const S: String): String;
begin
  var xI := 1;
  var xS := '';
  while True do
  begin
    var xC := S[xI];
    if xC = '\' then
    begin
      case S[xI + 1] of
        'n': xS := xS + #10;
        'r': xS := xS + #13;
      end;
    end;
    Inc(xI);
    if xI >= S.Length then
      Break;
  end;
  Result := xS;
end;

{ TConfig.TValue }

constructor TConfig.TValue.Create(const AConfig: TConfig);
begin
  FName := '';
  FValue := '';
  FConfig := AConfig;
end;

destructor TConfig.TValue.Destroy;
begin
  inherited;
end;

function TConfig.TValue.GetAsString: String;
begin
  Result := GetNRToStr(FValue);
end;

procedure TConfig.TValue.SetAsString(const Value: String);
begin
  FValue := GetStrToNR(Value);
  if Assigned(FConfig) then
    FConfig.Close;
end;

function TConfig.TValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(FValue,0);
end;

procedure TConfig.TValue.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);
  if Assigned(FConfig) then
    FConfig.Close;
end;


function TConfig.TValue.GetAsDouble: Double;
begin
  var xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloatDef(FValue,0);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

procedure TConfig.TValue.SetAsDouble(const Value: Double);
begin
  var xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  FValue := FloatToStr(Value);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
  if Assigned(FConfig) then
    FConfig.Close;
end;

{ TConfig.TSection }

constructor TConfig.TSection.Create(const AConfig: TConfig);
begin
  FConfig := AConfig;
  FValues := TValueList.Create;
end;

destructor TConfig.TSection.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TConfig.TSection.GetCreateValue(const AName: String): TConfig.TValue;
var
  xIndex: Integer;
  xValue: TConfig.TValue;
begin
  xIndex := GetIndexOf(AName);
  if xIndex < 0 then
  begin
    xValue := TConfig.TValue.Create(Self.Config);
    xValue.Name := AName;
    FValues.Add(xValue);
  end
  else
    xValue := FValues[xIndex];
  Result := xValue;
end;

function TConfig.TSection.GetIndexOf(const AName: String): Integer;
begin
  Result := -1;
  if FValues.Count > 0 then
    for var i := 0 to FValues.Count - 1 do
    begin
      var xValue := FValues[i];
      if SameText(xValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TConfig.TSection.GetValueByName(AName: String): TConfig.TValue;
begin
  var xValue := GetCreateValue(AName);
  Result :=  xValue;
end;

{ TConfig }

constructor TConfig.Create;
begin
  FSections := TSectionList.Create;
end;

destructor TConfig.Destroy;
begin
  FreeAndNil(FSections);
  inherited;
end;

function TConfig.GetCreateSection(const AName: String): TConfig.TSection;
var
  xIndex: Integer;
  xSection: TConfig.TSection;
begin
  xIndex := GetIndexOf(AName);
  if xIndex < 0 then
  begin
    xSection := TConfig.TSection.Create(Self);
    xSection.Name := AName;
    FSections.Add(xSection);
  end
  else
    xSection := FSections[xIndex];
  Result := xSection;
end;

function TConfig.GetIndexOf(const AName: String): Integer;
begin
  Result := -1;
  if FSections.Count > 0 then
    for var i := 0 to FSections.Count - 1 do
    begin
      var xSection := FSections[i];
      if SameText(xSection.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TConfig.GetSectionByName(AName: String): TConfig.TSection;
begin
  var xSection := GetCreateSection(AName);
  Result := xSection;
end;

{ TConfigStrings }

constructor TConfigStrings.Create;
begin
  inherited;
  FSource := TStringList.Create;
end;

destructor TConfigStrings.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

procedure TConfigStrings.Open;
var
  xSection: TConfig.TSection;
begin
  xSection := nil;
  for var xS in FSource do
  begin
    if not xS.IsEmpty then
    begin
      if (xS[1] = '[') and (xS[xS.Length] = ']') then
      begin
        var xStrName := Copy(xS,2,xS.Length - 2);
        xSection := Self.SectionByName[xStrName];
      end
      else
      begin
        var xInd := Pos('=',xS);
        if xInd > 0 then
          if Assigned(xSection) then
          begin
            var xStrName := Copy(xS,1,xInd - 1);
            var xValue := xSection.ValueByName[xStrName];
            xValue.Value := Copy(xS,xInd + 1,xS.Length);
          end;
      end;
    end;
  end;
end;

procedure TConfigStrings.Close;
begin
  FSource.Clear;
  for var xSection in FSections do
  begin
    FSource.Add('[' + xSection.Name + ']');
    for var xValue in xSection.Values do
      FSource.Add(xValue.Name + '=' + xValue.Value);
  end;
end;

{ TConfigIniFile }

constructor TConfigIniFile.Create;
begin
  inherited;

end;

destructor TConfigIniFile.Destroy;
begin

  inherited;
end;

procedure TConfigIniFile.Open;
begin
  if FileExists(FFileName) then
  begin
    FSource.LoadFromFile(FFileName,TEncoding.UTF8);
    inherited Open;
  end;
end;

procedure TConfigIniFile.Close;
begin
  inherited Close;
  FSource.SaveToFile(FFileName,TEncoding.UTF8);
end;

initialization
  TConfigs.SetInitialization;

finalization
  TConfigs.SetFinalization;

end.
