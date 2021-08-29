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
    procedure SetAsString(const Value: String);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsBoolean(const Value: Boolean);
    function GetText: String;
  protected
    property Value: Variant read FValue write FValue;
    property Text: String read GetText;
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
  public
    function IndexOfName(const AName: String): Integer;
    function ParamByName(const AName: String): TParam;
    procedure SetLoadParamStrings(const ASource: TStrings);
    procedure SetSaveParamStrings(const ASource: TStrings);    
  end;

  TNotifyEventParams = procedure(Sander: TObject; AParams: TParams) of object;

implementation

resourcestring
  ERROR_NAME_TYPE_PARAM = 'Неверный тип данных';
  ERROR_PARAM_BY_NAME = 'Не верное имя параметра';


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
  Result := Self.Name + '=' + Self.AsString;
end;

function TParam.GetAsStrign: String;
begin
  Result := VarToStrDef(FValue,'');
end;

function TParam.GetAsInteger: Integer;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsInt64: Int64;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsDouble: Double;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsDate: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsTime: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsDateTime: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := 0;
end;

function TParam.GetAsBoolean: Boolean;
begin
  if VarIsNull(FValue) then
    Result := FValue
  else
    Result := False;
end;

procedure TParam.SetAsString(const Value: String);
begin
  if (FDataType = ptNull) or (FDataType = ptStrign) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsInteger(const Value: Integer);
begin
  if (FDataType = ptNull) or (FDataType = ptInteger) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsInt64(const Value: Int64);
begin
  if (FDataType = ptNull) or (FDataType = ptInt64) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDouble(const Value: Double);
begin
  if (FDataType = ptNull) or (FDataType = ptDouble) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDate(const Value: TDateTime);
begin
  if (FDataType = ptNull) or (FDataType = ptDate) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsTime(const Value: TDateTime);
begin
  if (FDataType = ptNull) or (FDataType = ptTime) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsDateTime(const Value: TDateTime);
begin
  if (FDataType = ptNull) or (FDataType = ptDateTime) then
    FValue := Value
  else
    raise Exception.Create('Error Message: ' + ERROR_NAME_TYPE_PARAM);
end;

procedure TParam.SetAsBoolean(const Value: Boolean);
begin
  if (FDataType = ptNull) or (FDataType = ptBoolean) then
    FValue := Value
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
    raise Exception.Create('Error Message: ' + ERROR_PARAM_BY_NAME);
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

end;

end.
