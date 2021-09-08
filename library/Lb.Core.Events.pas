(******************************************************************************)
(* Глобальное управление событиями в приложение                               *)
(******************************************************************************)
unit Lb.Core.Events;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

type
  ///<summary>Параметрами для передачи</summary>
  TParamValue = record
    Key: String;
    Value: Variant;
    constructor Create(const AKey: String; const AValue: Variant);
    function IsNull: Boolean;
  end;


  ///<summary>Массив параметров</summary>
  TParamValues = class(TList<TParamValue>)
  private
    function GetValues(Name: String): Variant;
    procedure SetValues(Name: String; const Value: Variant);
  private
    function GetParamValues(Name: String): TParamValue;
    procedure SetParamValues(Name: String; const Value: TParamValue);
  protected
    property ParamValues[Name: String]: TParamValue read GetParamValues write SetParamValues;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfName(AName: String): Integer;
    ///<summary>Получение значения по имени параметра</summary>
    property Values[Name: String]: Variant read GetValues write SetValues;
    ///<summary>Содержит все передоваемые парамерты</summary>
    function ToString: String; override;
    ///<summary>Копирование объекта</summary>
    procedure SetAssign(const AParams: TParamValues);
  private
    function GetAsInteger(Name: String): Integer;
    function GetAsString(Name: String): String;
    function GetAsDouble(Name: String): Double;
    function GetAsBoolean(Name: String): Boolean;
    function GetAsInt64(Name: String): Int64;
    procedure SetAsInteger(Name: String; const AValue: Integer);
    procedure SetAsString(Name: String; const AValue: String);
    procedure SetAsDouble(Name: String; const AValue: Double);
    procedure SetAsBoolean(Name: String; const AValue: Boolean);
    procedure SetAsInt64(Name: String; const AValue: Int64);
  public
    property AsInteger[Name: String]: Integer read GetAsInteger write SetAsInteger;
    property AsString[Name: String]: String read GetAsString write SetAsString;
    property AsDouble[Name: String]: Double read GetAsDouble write SetAsDouble;
    property AsBoolean[Name: String]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInt64[Name: String]: Int64 read GetAsInt64 write SetAsInt64;
  end;

  ///<summary>Событие с параметром</summary>
  TNotifyEventParams = procedure(Sender: TObject; const AParamValues: TParamValues) of object;
  TNotifyEventParamStrings = procedure(Sender: TObject; const AParams: TStrings) of object;



  ///<summary>Параметрируем событие</summary>
  TEventParam = class(TObject)
  private
    FName: String;
    FSender: TObject;
    FParams: TParamValues;
    FParamStrings: TStrings;
    FEvent: TNotifyEventParams;
    FEventStrings: TNotifyEventParamStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetEvent(AParams: array of TParamValue); overload;
    procedure SetEvent(AParams: TStrings); overload;
    property Params: TParamValues read FParams;
    property Name: String read FName write FName;
    property Sender: TObject read FSender write FSender;
    property Event: TNotifyEventParams read FEvent write FEvent;
    property EventStrings: TNotifyEventParamStrings read FEventStrings write FEventStrings;
  end;

  /// <summary>Список событий </summary>
  TNotifyEventList = TObjectList<TEventParam>;

  ///<summary>Список событий с параметрами</summary>
  TNotifyEvents = class(TObject)
  private
    FEvents: TNotifyEventList;
    function GetCount: Integer;
  protected
    function GetCreateEventParam: TEventParam;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(Name: String): Integer;
    ///<summary>Генерируем событие</summary>
    ///<param name='AName'>Имя события</param>
    ///<param name='ASender'>Объект, который генерировал событие</param>
    ///<param name='AParams'>Передаваемые параметры</param>
    ///<remarks>
    /// ApplicationEvents.SetEvent('sys_body_close',Self,[]);
    ///</remarks>
    procedure SetEvent(AName: String; ASender: TObject; AParams: array of TParamValue); overload;
    procedure SetEvent(AName: String; ASender: TObject; AParams: TStrings); overload;
    ///<summary>Подписаться на событие</summary>
    ///<param name='AName'>Имя события</param>
    ///<remarks>
    /// ApplicationEvents.GetEvents('sys_body_close').Event := EventSysBodyClose;
    ///</remarks>
    function GetEvents(AName: String): TEventParam;
    property Count: Integer read GetCount;
  end;

function ApplicationEvents: TNotifyEvents;

implementation

var
  localNotifyEvents: TNotifyEvents = nil;

function ApplicationEvents: TNotifyEvents;
begin
  if not Assigned(localNotifyEvents) then
    localNotifyEvents := TNotifyEvents.Create;
  Result := localNotifyEvents;
end;

{ TParamValue }

constructor TParamValue.Create(const AKey: String; const AValue: Variant);
begin
  Key := AKey;
  Value := AValue;
end;

function TParamValue.IsNull: Boolean;
begin
  Result := VarIsNull(Self.Value);
end;

{ TParamValues }

constructor TParamValues.Create;
begin
  inherited Create;
end;

destructor TParamValues.Destroy;
begin
  inherited;
end;

function TParamValues.IndexOfName(AName: String): Integer;
var
  xParam:  TParamValue;
  i, Count: Integer;
begin
  Result := -1;
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xParam := Self.Items[i];
      if SameText(xParam.Key,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;


function TParamValues.GetValues(Name: String): Variant;
var
  xInd: Integer;
  xParam:  TParamValue;
begin
  xInd := IndexOfName(Name);
  if xInd >= 0 then
  begin
    xParam := Self.Items[xInd];
    Result := xParam.Value;
  end
  else
    Result := varNull;
end;

procedure TParamValues.SetValues(Name: String; const Value: Variant);
var
  xInd: Integer;
  xParam:  TParamValue;
begin
  xInd := IndexOfName(Name);
  if xInd >= 0 then
  begin
    xParam := Self.Items[xInd];
    xParam.Value := Value;
  end;
end;

function TParamValues.ToString: String;
var
  xStr: TStrings;
  xValue: TParamValue;
begin
  xStr := TStringList.Create;
  try
    xStr.Add('param_value: count = ' + IntToStr(Self.Count));
    if Self.Count > 0 then
      for xValue in Self do
        xStr.Add(xValue.Key + '=' + VarToStrDef(xValue.Value,'default'));
    Result := xStr.Text;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TParamValues.SetAssign(const AParams: TParamValues);
var
  i, iCount: Integer;
  xParamValue: TParamValue;
begin
  Self.Clear;
  if Assigned(AParams) then
  begin
    iCount := AParams.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParamValue := AParams[i];
        Self.Add(xParamValue);
      end;
  end;
end;

function TParamValues.GetParamValues(Name: String): TParamValue;
var
  xIndex: Integer;
  xParamValue: TParamValue;
begin
  FillChar(xParamValue,SizeOf(TParamValue),0);
  xIndex := IndexOfName(Name);
  if xIndex >= 0 then
    Result := Self.Items[xIndex]
  else
    Result := xParamValue;
end;

procedure TParamValues.SetParamValues(Name: String; const Value: TParamValue);
var
  xIndex: Integer;
begin
  xIndex := IndexOfName(Name);
  if xIndex >= 0 then
    Self.Items[xIndex] := Value
  else
    Self.Add(Value);
end;

function TParamValues.GetAsInteger(Name: String): Integer;
var
  xParamValue: TParamValue;
begin
  xParamValue := ParamValues[Name];
  if xParamValue.IsNull then
    Result := 0
  else
    Result := xParamValue.Value;
end;

procedure TParamValues.SetAsInteger(Name: String; const AValue: Integer);
var
  xParamValue: TParamValue;
begin
  xParamValue.Key := Name;
  xParamValue.Value := AValue;
  ParamValues[Name] := xParamValue;
end;

function TParamValues.GetAsString(Name: String): String;
var
  xParamValue: TParamValue;
begin
  xParamValue := ParamValues[Name];
  if xParamValue.IsNull then
    Result := ''
  else
    Result := VarToStrDef(xParamValue.Value,'');
end;


procedure TParamValues.SetAsString(Name: String; const AValue: String);
var
  xParamValue: TParamValue;
begin
  xParamValue.Key := Name;
  xParamValue.Value := AValue;
  ParamValues[Name] := xParamValue;
end;


function TParamValues.GetAsDouble(Name: String): Double;
var
  xParamValue: TParamValue;
begin
  xParamValue := ParamValues[Name];
  if xParamValue.IsNull then
    Result := 0
  else
    Result := xParamValue.Value;
end;

procedure TParamValues.SetAsDouble(Name: String; const AValue: Double);
var
  xParamValue: TParamValue;
begin
  xParamValue.Key := Name;
  xParamValue.Value := AValue;
  ParamValues[Name] := xParamValue;
end;

function TParamValues.GetAsBoolean(Name: String): Boolean;
var
  xParamValue: TParamValue;
begin
  xParamValue := ParamValues[Name];
  if xParamValue.IsNull then
    Result := False
  else
    Result := xParamValue.Value;
end;

procedure TParamValues.SetAsBoolean(Name: String; const AValue: Boolean);
var
  xParamValue: TParamValue;
begin
  xParamValue.Key := Name;
  xParamValue.Value := AValue;
  ParamValues[Name] := xParamValue;
end;

function TParamValues.GetAsInt64(Name: String): Int64;
var
  xParamValue: TParamValue;
begin
  xParamValue := ParamValues[Name];
  if xParamValue.IsNull then
    Result := 0
  else
    Result := xParamValue.Value;
end;

procedure TParamValues.SetAsInt64(Name: String; const AValue: Int64);
var
  xParamValue: TParamValue;
begin
  xParamValue.Key := Name;
  xParamValue.Value := AValue;
  ParamValues[Name] := xParamValue;
end;

{ TEventParam }

constructor TEventParam.Create;
begin
  FName := '';
  FSender := nil;
  FEvent := nil;
  FParams := TParamValues.Create;
  FParamStrings := TStringList.Create;
end;

destructor TEventParam.Destroy;
begin
  FreeAndNil(FParamStrings);
  FreeAndNil(FParams);
  inherited;
end;

procedure TEventParam.SetEvent(AParams: array of TParamValue);
var
  xParam: TParamValue;
begin
  if Assigned(FEvent) then
  begin
    FParams.Clear;
    for xParam in AParams do
      FParams.Add(xParam);
    FEvent(FSender,FParams);
  end;
end;

procedure TEventParam.SetEvent(AParams: TStrings);
begin
  if Assigned(FEventStrings) then
  begin
    FParamStrings.Assign(AParams);
    FEventStrings(FSender,FParamStrings);
  end;
end;

{ TNotifyEvents }

constructor TNotifyEvents.Create;
begin
  FEvents := TNotifyEventList.Create;
end;

destructor TNotifyEvents.Destroy;
begin
  FreeAndNil(FEvents);
  if Assigned(localNotifyEvents) then
    localNotifyEvents := nil;
  inherited;
end;

procedure TNotifyEvents.Clear;
begin
  FEvents.Clear;
end;

function TNotifyEvents.GetCount: Integer;
begin
  Result := FEvents.Count;
end;

function TNotifyEvents.GetCreateEventParam: TEventParam;
var
  xEvent: TEventParam;
begin
  xEvent := TEventParam.Create;
  FEvents.Add(xEvent);
  Result := xEvent;
end;

function TNotifyEvents.IndexOf(Name: String): Integer;
var
  xEvent: TEventParam;
  i, Count: Integer;
begin
  Result := -1;
  Count := FEvents.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xEvent := FEvents[i];
      if SameText(xEvent.Name,Name) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure TNotifyEvents.SetEvent(AName: String; ASender: TObject; AParams: array of TParamValue);
var
  xIndex: Integer;
  xEvent: TEventParam;
begin
  xIndex := IndexOf(AName);
  if xIndex >= 0 then
  begin
    xEvent := FEvents[xIndex];
    xEvent.Sender := ASender;
    xEvent.SetEvent(AParams);
  end;
end;

procedure TNotifyEvents.SetEvent(AName: String; ASender: TObject; AParams: TStrings);
var
  xIndex: Integer;
  xEvent: TEventParam;
begin
  xIndex := IndexOf(AName);
  if xIndex >= 0 then
  begin
    xEvent := FEvents[xIndex];
    xEvent.Sender := ASender;
    xEvent.SetEvent(AParams);
  end;
end;


function TNotifyEvents.GetEvents(AName: String): TEventParam;
var
  xInd: Integer;
  xEvent: TEventParam;
begin
  xInd := IndexOf(AName);
  if xInd < 0 then
  begin
    xEvent := GetCreateEventParam;
    xEvent.Name := AName;
  end
  else
    xEvent := FEvents[xInd];
  Result := xEvent;
end;



initialization
  ApplicationEvents;

finalization
  if Assigned(localNotifyEvents) then
    FreeAndNil(localNotifyEvents);

end.
