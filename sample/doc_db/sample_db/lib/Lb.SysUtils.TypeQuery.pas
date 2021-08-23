unit Lb.SysUtils.TypeQuery;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;


{$IFDEF DEBUG}
  //{$DEFINE DB_PARAMS}
  //{$DEFINE DB_QUERY_FOLDER}
{$ENDIF}


type
  ///<summary>Объект используется для передачи данных</summary>
  TParams = class(TObject)
  public type
    TTypeValue = (tvNull,tvString,tvInteger,tvInt64,tvDouble,tvBoolean,tvDate,tvTime,tvDateTime,tvStream);

    TValue = class(TObject)
    private
      FName: String;
      FValue: Variant;
      FMemory: TMemoryStream;
      FTypeValue: TTypeValue;
      FParams: TParams;
      function GetValue: Variant;
      procedure SetValue(const Value: Variant);
    protected
      function GetIsNull: Boolean;
    private
      function GetAsString: String;
      function GetAsInteger: Integer;
      function GetAsInt64: Int64;
      function GetAsDouble: Double;
      function GetAsBoolean: Boolean;
      function GetAsDate: TDateTime;
      function GetAsTime: TDateTime;
      function GetAsDateTime: TDateTime;
      procedure SetAsString(const Value: String);
      procedure SetAsInteger(const Value: Integer);
      procedure SetAsInt64(const Value: Int64);
      procedure SetAsDouble(const Value: Double);
      procedure SetAsBoolean(const Value: Boolean);
      procedure SetAsDate(const Value: TDateTime);
      procedure SetAsTime(const Value: TDateTime);
      procedure SetAsDateTime(const Value: TDateTime);
    private
      function GetValueString: String;
      function GetValueNumber: Double;
        function GetStream: TStream;
    public
      constructor Create(const AParams: TParams);
      destructor Destroy; override;
      property Name: String read FName write FName;
      property IsNull: Boolean read GetIsNull;
      property TypeValue: TTypeValue read FTypeValue;
      property Value: Variant read GetValue write SetValue;
    public
      procedure SetValueString(const Value: String; const ATypeValue: TTypeValue);
      procedure SetValueNumber(const Value: Double; const ATypeValue: TTypeValue);
      procedure SetValueStream(const Value: TStream);
      property ValueString: String read GetValueString;
      property ValueNumber: Double read GetValueNumber;
    public
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
      property AsString: String read GetAsString write SetAsString;
      property AsInteger: Integer read GetAsInteger write SetAsInteger;
      property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
      property AsDouble: Double read GetAsDouble write SetAsDouble;
      property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
      property AsDate: TDateTime read GetAsDate write SetAsDate;
      property AsTime: TDateTime read GetAsTime write SetAsTime;
      property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
      property Stream: TStream read GetStream;
    public
      function ToString: String; override;
    end;
    TValueList = TObjectList<TValue>;

  private
    FItems: TValueList;
    function GetValueByName(const AName: String): TValue;
    function GetValues(Name: String): TParams.TValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property ValueByName[const AName: String]: TValue read GetValueByName;
    property Count: Integer read GetCount;
  public
    function IndexOf(const AName: String): Integer;
    function IsNameField(const AName: String): Boolean;
    property Items: TValueList read FItems;
    property Values[Name: String]: TParams.TValue read GetValues;
  public
    class function StrToTypeValue(const ATypeValue: TTypeValue): String; static;
    class function TypeValueToStr(const AValue: String): TParams.TTypeValue; static;
  end;

  TQueryParams = class(TParams);

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  FireDAC.Stan.Param,
  Lb.SysUtils.ISO860;

{$IFDEF DB_PARAMS}
procedure SetLogParams(const AParams: TParams);
var
  xValue: TParams.TValue;
  i, Count: Integer;
begin
  TLogger.LogForm('set_log_params:','Count = ' + IntToStr(AParams.Count));
  Count := AParams.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := AParams.Items[i];
      TLogger.LogText(' >> ' + xValue.ToString);
    end;
end;
{$ENDIF}

{ TParams.TValue }

constructor TParams.TValue.Create(const AParams: TParams);
begin
  FTypeValue := TParams.TTypeValue.tvNull;
  FParams := AParams;
  FMemory := TMemoryStream.Create;
end;

destructor TParams.TValue.Destroy;
begin
  FreeAndNil(FMemory);
  inherited;
end;

function TParams.TValue.GetValue: Variant;
begin
  //Result := FParams.Values[FName].Value;
  Result := FValue;
end;


procedure TParams.TValue.SetValue(const Value: Variant);
begin
  //FParams.Values[FName].Value := Value;
  FValue := Value;
end;


function TParams.TValue.GetIsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TParams.TValue.GetStream: TStream;
begin
  Result := FMemory;
end;

function TParams.TValue.ToString: String;
var
  xTypeS: String;
begin
  case FTypeValue of
    tvNull: xTypeS := 'null';
    tvString: xTypeS := 'string';
    tvInteger: xTypeS := 'integer';
    tvInt64: xTypeS := 'int64';
    tvDouble: xTypeS := 'double';
    tvBoolean: xTypeS := 'boolean';
    tvDate: xTypeS := 'date';
    tvTime: xTypeS := 'time';
    tvDateTime: xTypeS := 'date_time';
  end;
  Result := Name + ':' + xTypeS + ' = ' + VarToStrDef(FValue,'');
end;

function TParams.TValue.GetAsString: String;
begin
  Result := '';
  if not Self.IsNull then
    Result := Self.Value;
{$IFDEF DB_PARAMS}
  TLogger.Log('TParams.TValue.GetAsString [' + FName + ']');
  SetLogParams(FParams);
{$ENDIF}
end;

procedure TParams.TValue.SetAsString(const Value: String);
begin
  FTypeValue := TParams.TTypeValue.tvString;
  FValue := Value;
{$IFDEF DB_PARAMS}
  TLogger.Log('TParams.TValue.SetAsString [' + FName + '] ' + Value);
  SetLogParams(FParams);
{$ENDIF}
end;

function TParams.TValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Self.Value,0);
end;

procedure TParams.TValue.SetAsInteger(const Value: Integer);
begin
  FTypeValue := TParams.TTypeValue.tvInteger;
  FValue := IntToStr(Value);
end;

function TParams.TValue.GetAsInt64: Int64;
begin
  Result := StrToInt64Def(Self.Value,0);
end;

procedure TParams.TValue.SetAsInt64(const Value: Int64);
begin
  FTypeValue := TParams.TTypeValue.tvInt64;
  FValue := IntToStr(Value);
end;

function TParams.TValue.GetAsDouble: Double;
begin
  Result := StrToFloatDef(Self.Value,0)
end;

procedure TParams.TValue.SetAsDouble(const Value: Double);
begin
  FTypeValue := TParams.TTypeValue.tvDouble;
  FValue := FloatToStr(Value);
end;

function TParams.TValue.GetAsBoolean: Boolean;
begin
  Result := False;
  if not Self.IsNull then
  begin
    case FTypeValue of
      tvString: Result :=  not Self.AsString.IsEmpty;
      tvInteger: Result := Self.AsInteger > 0;
      tvInt64: Result := Self.AsInt64 > 0;
      tvDouble: Result := Self.AsDouble > 0;
      tvBoolean: Result := FValue;
      tvDate: Result := Self.AsDate > 0;
      tvTime: Result := Self.AsTime > 0;
      tvDateTime: Result := Self.AsDateTime > 0;
    end;
  end;
end;

procedure TParams.TValue.SetAsBoolean(const Value: Boolean);
begin
  FTypeValue := TParams.TTypeValue.tvBoolean;
  FValue := Value;
end;

function TParams.TValue.GetAsDate: TDateTime;
var
  xS: String;
begin
  Result := 0;
  if not Self.IsNull then
  begin
    xS := VarToStrDef(FValue,'');
    Result := GetStrISO860ToDate(xS);
  end;
end;

procedure TParams.TValue.SetAsDate(const Value: TDateTime);
begin
  FTypeValue := TParams.TTypeValue.tvDate;
  FValue := GetDateToStrISO860(Value);
end;

function TParams.TValue.GetAsTime: TDateTime;
var
  xS: String;
begin
  Result := 0;
  if not Self.IsNull then
  begin
    xS := VarToStrDef(FValue,'');
    Result := GetStrISO860ToTime(xS);
  end;
end;

procedure TParams.TValue.SetAsTime(const Value: TDateTime);
begin
  FTypeValue := TParams.TTypeValue.tvTime;
  FValue := GetTimeToStrISO860(Value);
end;

function TParams.TValue.GetAsDateTime: TDateTime;
var
  xS: String;
begin
  Result := 0;
  if Self.IsNull then
  begin
    xS := VarToStrDef(FValue,'');
    Result := GetStrISO860ToDateTime(xS);
  end;
end;

procedure TParams.TValue.SetAsDateTime(const Value: TDateTime);
begin
  FTypeValue := TParams.TTypeValue.tvDateTime;
  FValue := GetDateTimeToStrISO860(Value);
end;

function TParams.TValue.GetValueString: String;
begin
  Result := VarToStrDef(FValue,'');
end;

procedure TParams.TValue.SetValueString(const Value: String; const ATypeValue: TTypeValue);
begin
  FTypeValue := ATypeValue;
  FValue := Value;
end;

function TParams.TValue.GetValueNumber: Double;

  function GetIntToBool(const Value: Boolean): Integer;
  begin
    Result := Integer(Value);
  end;

begin
  Result := 0;
  if not IsNull then
    case FTypeValue of
      tvInteger: Result := FValue;
      tvInt64: Result := FValue;
      tvDouble: Result := FValue;
      tvBoolean: Result := GetIntToBool(FValue);
    end;
end;

procedure TParams.TValue.SetValueNumber(const Value: Double; const ATypeValue: TTypeValue);
begin
  FTypeValue := ATypeValue;
  case ATypeValue of
    tvInteger: FValue := Trunc(Value);
    tvInt64: FValue := Trunc(Value);
    tvDouble: FValue := Value;
    tvBoolean: FValue := Boolean(Trunc(Value));
  end;
end;

procedure TParams.TValue.SetValueStream(const Value: TStream);
begin
  FTypeValue := TParams.TTypeValue.tvStream;
  FMemory.LoadFromStream(Value);
end;

procedure TParams.TValue.LoadFromStream(Stream: TStream);
begin
  FTypeValue := TParams.TTypeValue.tvStream;
  FMemory.LoadFromStream(Stream);
end;

procedure TParams.TValue.SaveToStream(Stream: TStream);
begin
  FMemory.SaveToStream(Stream);
end;

{ TParams }

constructor TParams.Create;
begin
  inherited Create;
  FItems := TValueList.Create;
end;

destructor TParams.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TParams.Clear;
begin
  FItems.Clear;
end;

function TParams.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TParams.GetValueByName(const AName: String): TValue;
var
  xIndex: Integer;
  xValue: TParams.TValue;
begin
  if AName.IsEmpty then
    raise Exception.Create('Error Message: Имя параметра пустое');
  xIndex := Self.IndexOf(AName);
  if xIndex >= 0 then
    xValue := FItems[xIndex]
  else
  begin
    xValue := TValue.Create(Self);
    xValue.Name := AName;
    FItems.Add(xValue);
  end;
  Result := xValue;
end;


function TParams.IndexOf(const AName: String): Integer;
var
  i, Count: Integer;
  xValue: TParams.TValue;
begin
  Result := -1;
  Count := FItems.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := FItems[i];
      if SameText(xValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TParams.IsNameField(const AName: String): Boolean;
var
  xIndex: Integer;
begin
  xIndex := Self.IndexOf(AName);
  Result := (xIndex >= 0);
end;

function TParams.GetValues(Name: String): TParams.TValue;
var
  xIndex: Integer;
  xValue: TParams.TValue;
begin
{$IFDEF DB_PARAMS}
  TLogger.Log('TParams.GetValues');
  SetLogParams(Self);
{$ENDIF}

  xIndex := Self.IndexOf(Name);
  if xIndex < 0 then
  begin
    xValue := TParams.TValue.Create(Self);
    xValue.Name := Name;
    FItems.Add(xValue);
  end
  else
    xValue := FItems[xIndex];
  Result := xValue;
end;


class function TParams.StrToTypeValue(const ATypeValue: TTypeValue): String;
var
  xS: String;
begin
  case ATypeValue of
    tvString: xS := 'string';
    tvInteger: xS := 'integer';
    tvInt64: xS := 'int64';
    tvDouble: xS := 'double';
    tvBoolean: xS := 'boolean';
    tvDate: xS := 'date';
    tvTime: xS := 'time';
    tvDateTime: xS := 'datetime';
    tvStream: xS := 'stream';
  else
    xS := 'null';
  end;
  Result := xS;
end;

class function TParams.TypeValueToStr(const AValue: String): TParams.TTypeValue;
var
  xS: String;
begin
  xS := AnsiLowerCase(AValue);
  if SameText(xS,'string') then
    Result := TParams.TTypeValue.tvString
  else if SameText(xS,'integer') then
    Result := TParams.TTypeValue.tvInteger
  else if SameText(xS,'int64') then
    Result := TParams.TTypeValue.tvInt64
  else if SameText(xS,'double') then
    Result := TParams.TTypeValue.tvDouble
  else if SameText(xS,'boolean') then
    Result := TParams.TTypeValue.tvBoolean
  else if SameText(xS,'date') then
    Result := TParams.TTypeValue.tvDate
  else if SameText(xS,'time') then
    Result := TParams.TTypeValue.tvTime
  else if SameText(xS,'datetime') then
    Result := TParams.TTypeValue.tvDateTime
  else
    Result := TParams.TTypeValue.tvNull;
end;

end.
