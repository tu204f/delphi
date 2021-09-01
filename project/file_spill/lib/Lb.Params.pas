unit Lb.Params;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Тип параметра</summary>
  TParamType = (ptNull,ptStrign,ptInteger,ptInt64,ptDouble,ptDate,ptTime,ptDateTime,ptBoolean);

  ///<summary>Параметры</summary>
  TParam = class(TObject)
  private
    FName: String;
    FValue: Variant;
    FDataType: TParamType;
  private {todo: Нужно переписать все}
    function GetAsStrign: String;
    function GetAsInteger: Integer;
    function GetAsInt64: Int64;
    function GetAsDouble: Double;
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsBoolean: Boolean;
  private
    procedure SetAsString(const AValue: String);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsInt64(const AValue: Int64);
    procedure SetAsDouble(const AValue: Double);
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsTime(const AValue: TDateTime);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsBoolean(const AValue: Boolean);
  protected
    function GetText: String;
    procedure SetText(const AValue: String);
  protected
    property Value: Variant read FValue write FValue;
    property Text: String read GetText write SetText;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: String read FName write FName;
    property DataType: TParamType read FDataType write FDataType;
    property AsString: String read GetAsStrign write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;
  TParamList = TObjectList<TParam>;

  ///<summary>Список параметров</summary>
  TParams = class(TParamList)
  protected
    function GetCreateParam: TParam;
  public
    function IndexOfName(const AName: String): Integer;
    function ParamByName(const AName: String): TParam;
    procedure SetLoadParamStrings(const ASource: TStrings);
    procedure SetSaveParamStrings(const ASource: TStrings);    
  end;

  TNotifyEventParams = procedure(Sander: TObject; AParams: TParams) of object;
  TCallBackParams = procedure(AParams: TParams);

implementation

resourcestring
  ERROR_NAME_TYPE_PARAM = 'Неверный тип данных';
  ERROR_PARAM_BY_NAME = 'Не верное имя параметра';


function GetParamTypeToStr(const AParamType: TParamType): String;
begin
  case AParamType of
    ptStrign: Result := 'string';
    ptInteger: Result := 'integer';
    ptInt64: Result := 'int64';
    ptDouble: Result := 'double';
    ptDate: Result := 'date';
    ptTime: Result := 'time';
    ptDateTime: Result := 'datetime';
    ptBoolean: Result := 'boolean';
  else
    Result := 'null';
  end;
end;

function GetStrToParamType(const AParamType: String): TParamType;
var
  xS: String;
begin
  xS := AnsiLowerCase(Trim(AParamType));
  if SameText(xS,'string') then
    Result := TParamType.ptStrign
  else if SameText(xS,'integer') then
    Result := TParamType.ptInteger
  else if SameText(xS,'int64') then
    Result := TParamType.ptInt64
  else if SameText(xS,'double') then
    Result := TParamType.ptDouble
  else if SameText(xS,'date') then
    Result := TParamType.ptDate
  else if SameText(xS,'time') then
    Result := TParamType.ptTime
  else if SameText(xS,'datetime') then
    Result := TParamType.ptDateTime
  else if SameText(xS,'boolean') then
    Result := TParamType.ptBoolean
  else
    Result := TParamType.ptNull;
end;

{ TParam }

constructor TParam.Create;
begin
  FName := '';
  FValue := varNull;
end;

destructor TParam.Destroy;
begin

  inherited;
end;

function TParam.GetText: String;
begin
  Result := Self.Name + ':' + GetParamTypeToStr(FDataType) + '=' + Self.AsString;
end;

procedure TParam.SetText(const AValue: String);
begin
  {todo: читаем список фалов}
end;

function TParam.GetAsStrign: String;
begin
  Result := VarToStrDef(FValue,'');
end;

function TParam.GetAsInteger: Integer;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsInt64: Int64;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsDouble: Double;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsDate: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsTime: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsDateTime: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := FValue;
end;

function TParam.GetAsBoolean: Boolean;
begin
  if VarIsNull(FValue) then
    Result := False
  else
    Result := FValue;
end;

procedure TParam.SetAsString(const AValue: String);
begin
  if FDataType = ptNull then
    FDataType := ptStrign;
  if (FDataType = ptStrign) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsInteger(const AValue: Integer);
begin
  if FDataType = ptNull then
    FDataType := ptInteger;
  if (FDataType = ptInteger) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsInt64(const AValue: Int64);
begin
  if FDataType = ptNull then
    FDataType := ptInt64;
  if (FDataType = ptInt64) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDouble(const AValue: Double);
begin
  if FDataType = ptNull then
    FDataType := ptDouble;
  if (FDataType = ptDouble) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDate(const AValue: TDateTime);
begin
  if FDataType = ptNull then
    FDataType := ptDate;
  if (FDataType = ptDate) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsTime(const AValue: TDateTime);
begin
  if FDataType = ptNull then
    FDataType := ptTime;
  if (FDataType = ptTime) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDateTime(const AValue: TDateTime);
begin
  if FDataType = ptNull then
    FDataType := ptDateTime;
  if (FDataType = ptDateTime) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsBoolean(const AValue: Boolean);
begin
  if FDataType = ptNull then
    FDataType := ptBoolean;
  if (FDataType = ptBoolean) then
    FValue := AValue
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

{ TParams }

function TParams.IndexOfName(const AName: String): Integer;
var
  i, iCount: Integer;
begin
  Result := -1;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xParam := Self.Items[i];
      if SameText(xParam.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TParams.ParamByName(const AName: String): TParam;
begin
  var xIndex := Self.IndexOfName(AName);
  if xIndex >= 0 then
    Result := Self.Items[xIndex]
  else
  begin
    var xParam := Self.GetCreateParam;
    xParam.Name := AName;
    Result := xParam;
  end;
end;

function TParams.GetCreateParam: TParam;
begin
  var xParam := TParam.Create;
  Result := xParam;
  Self.Add(xParam);
end;

procedure TParams.SetLoadParamStrings(const ASource: TStrings);
var
  i, iCount: Integer;
begin
  ASource.Clear;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xParam := Self.Items[i];
      ASource.Add(xParam.Text);
    end;
end;

procedure TParams.SetSaveParamStrings(const ASource: TStrings);
begin
  for var xS in ASource do
  begin
    var xParam := Self.GetCreateParam;
    xParam.Text := xS;
  end;
end;

end.
