unit Lb.DB.Connection;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Data.DB,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,

  FireDAC.Comp.Client,
  FireDAC.Phys.DB2Def,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.PG,
  FireDAC.Phys.ODBCBase,
  FireDAC.Phys.DB2,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite,

  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  {$IF DECLARED(TFmxObject)}
  FireDAC.FMXUI.Wait,
  {$ELSE}
  FireDAC.VCLUI.Wait,
  {$ENDIF}
  FireDAC.Comp.UI;

type
  TArrString = TArray<String>;
  TArrVariant = TArray<Variant>;
  TArrFieldType = TArray<TFieldType>;

  TValueParam = class(TObject)
  public type
    TTypeSQL = (Insert,Update);
  private
    FCount: Integer;
    FCapacity: Integer;
    FFields: TArrString;
    FParams: TArrVariant;
    FTypes: TArrFieldType;
  protected
    FResultCount: Integer;
    FResultParams: TArrVariant;
    FResultTypes: TArrFieldType;
    procedure SetAddResultParam(AParam: Variant; AType: TFieldType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetClear;
    procedure SetAdd(const AField: String; AParam: Variant; AType: TFieldType);
    function GetInsertSQL(const ANameTable: String): String;
    property Count: Integer read FCount;
    property Capacity: Integer write FCapacity;
  public
    class function GetFieldTypeToStr(AFieldType: TFieldType): String;
    property ResultParams: TArrVariant read FResultParams;
    property ResultTypes: TArrFieldType read FResultTypes;
  end;

  /// <summary>
  /// Объект установки соединиее с базей
  /// </summary>
  /// <remarks>
  /// Упрощает соединение с базой данных, и выполнение запросов
  /// </remarks>
  TDBConnection = class(TObject)
  private
    FActiveExec: Boolean;
    FParams: TStrings;
    function GetConnected: Boolean;
  protected
    FConnection: TFDConnection;
    FTransaction: TFDTransaction;
    FQuery: TFDQuery;
    procedure DoMessageError(const AMsg: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    /// <summary>
    /// Определяет соединения по умолчанию
    /// </summary>
    procedure DefaultConnection(const AFileName: String = 'default.db');
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    property Params: TStrings read FParams write FParams;
    property Connected: Boolean read GetConnected;
  public
    // Отправляем запрос
    function GetExecSQL(const ASQL: String): Boolean; overload;
    function GetExecSQL(const ASQL: String; const AParams: array of Variant): Boolean; overload;
    function GetExecSQL(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): Boolean; overload;
    // Отправляем запрос и получаем значение одного поля и первой строки
    function GetExecSQLScalar(const ASQL: String): Variant; overload;
    function GetExecSQLScalar(const ASQL: String; const AParams: array of Variant): Variant; overload;
    function GetExecSQLScalar(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): Variant; overload;
    // Запрос с получением данных
    function GetSelect(const ASQL: String): TDataSet; overload;
    function GetSelect(const ASQL: String; const AParams: array of Variant): TDataSet; overload;
    function GetSelect(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): TDataSet; overload;
    // Для создания TDateSet – подзапроса
    function GetSelectCreateDataSet(const ASQL: String): TDataSet; overload;
    function GetSelectCreateDataSet(const ASQL: String; const AParams: array of Variant): TDataSet; overload;
    function GetSelectCreateDataSet(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): TDataSet; overload;
    /// <summary>
    /// Показывает выполяется ли сейчас запрос или нет
    /// </summary>
    /// <remarks>
    /// Использовать в случание много поточного запроса
    /// </remarks>
    property ActiveExec: Boolean read FActiveExec;
  public
    /// <summary>
    /// Проверяет установлено соединения
    /// </summary>
    class function GetIsConnected: Boolean;
    /// <summary>
    /// Устанавливаем соединение с базой
    /// </summary>
    /// <param name = "AParams">Параметр установки соединение с базой</param>
    /// <remarks>
    /// Все настройки и запросы по умолчанию работают с SQLite3
    /// </remarks>
    class function GetConnectionParam(const AParams: TStrings): TDBConnection;
    /// <summary>
    /// Создание объекта соединения
    /// </summary>
    class function GetCreateDBConnection(const AParam: TStrings): TDBConnection;
    /// <summary>
    /// Разорвать соединение с базой
    /// </summary>
    class procedure SetDisConnected(ADBConnection: TDBConnection = nil);
  end;

var
  DBConnection: TDBConnection = nil;

procedure SetConnectionParam(const AParams: TStrings);

implementation

uses Lb.Logger;

var
  CountConnect: Integer = 0;
  DPhysDB2: TFDPhysDB2DriverLink = nil;
  DPhysPg: TFDPhysPgDriverLink = nil;
  DPhysSQLite: TFDPhysSQLiteDriverLink = nil;
  {$IF DECLARED(TFmxObject)}
  FDGUIxWaitCursor: TFDGUIxWaitCursor = nil;
  {$ELSE}
  FDGUIxWaitCursor: TFDGUIxWaitCursor = nil;
  {$ENDIF}

procedure SetConnectionParam(const AParams: TStrings);
var
  xS: String;
begin
  if not Assigned(DBConnection) then
  begin
    xS := 'Не определён объект соединения.[DBConnection]';
    raise Exception.Create('Error Message: ' + xS);
  end;
  try
    if Assigned(AParams) then
    begin
      DBConnection.Params.Assign(AParams);
      DBConnection.Open;
    end;
  except
    on E: Exception  do Exception.Create('Error Message: ' + E.Message);
  end;
end;

procedure SetInitializationDriverLink;
begin
  if not Assigned(FDGUIxWaitCursor) then
    FDGUIxWaitCursor := TFDGUIxWaitCursor.Create(nil);
  // Инициализация соединенеи с DB2
  if not Assigned(DPhysDB2) then DPhysDB2 := TFDPhysDB2DriverLink.Create(nil);
  // Инициализация соединенеи с PostgreSQL
  if not Assigned(DPhysPg) then
  begin
    DPhysPg := TFDPhysPgDriverLink.Create(nil);
    DPhysPg.VendorLib := 'driver\libpq.dll';
  end;
  // Инициализация соединенеи с SQLite3
  if not Assigned(DPhysSQLite) then
    DPhysSQLite := TFDPhysSQLiteDriverLink.Create(nil);
end;

procedure SetFinalizationDriverLink;
begin
  if Assigned(FDGUIxWaitCursor) then FreeAndNil(FDGUIxWaitCursor);
  if Assigned(DPhysDB2) then FreeAndNil(DPhysDB2);
  if Assigned(DPhysPg) then FreeAndNil(DPhysPg);
  if Assigned(DPhysSQLite) then FreeAndNil(DPhysSQLite);
end;

{ TValueParam }

constructor TValueParam.Create;
begin
  SetClear;
end;

destructor TValueParam.Destroy;
begin

  inherited;
end;

procedure TValueParam.SetClear;
begin
  FCapacity := 250;
  FCount := 0;
  FResultCount := 0;

  SetLength(FFields,FCapacity);
  SetLength(FParams,FCapacity);
  SetLength(FTypes,FCapacity);

  SetLength(FResultParams,0);
  SetLength(FResultTypes,0);
end;

procedure TValueParam.SetAdd(const AField: String; AParam: Variant; AType: TFieldType);
var
  xInd: Integer;
begin
  Inc(FCount);
  xInd := FCount - 1;
  FFields[xInd] := AField;
  FParams[xInd] := AParam;
  FTypes[xInd] := AType;
end;

procedure TValueParam.SetAddResultParam(AParam: Variant; AType: TFieldType);

  function GetCorFieldType(const AFieldType: TFieldType): TFieldType;
  begin
    case AFieldType of
      ftGuid, ftWideString: Result := TFieldType.ftString;
    else
      Result := AFieldType;
    end;
  end;

begin
  Inc(FResultCount);
  SetLength(FResultParams,FResultCount);
  SetLength(FResultTypes,FResultCount);
  FResultParams[FResultCount - 1] := AParam;
  FResultTypes[FResultCount - 1] := GetCorFieldType(AType);
end;


function TValueParam.GetInsertSQL(const ANameTable: String): String;
var
  xSQL: String;
  xField, xFieldParam: String;
  i: Integer;
begin
  xField := '';
  xFieldParam := '';
  FResultCount := 0;
  for i := 0 to Self.Count - 1  do
  begin
    if not VarIsNull(FParams[i]) then
    begin
      xField := xField + FFields[i] + ',';
      xFieldParam := xFieldParam + ':' + FFields[i] + ',';
      SetAddResultParam(FParams[i],FTypes[i]);
    end;
  end;
  xField := Copy(xField,1,Length(xField) - 1);
  xFieldParam := Copy(xFieldParam,1,Length(xFieldParam) - 1);
  xSQL := 'insert into ' + ANameTable + ' (' + xField + ') values (' + xFieldParam + ');';
  Result := xSQL;
end;

class function TValueParam.GetFieldTypeToStr(AFieldType: TFieldType): String;
begin
  case AFieldType of
    ftUnknown: Result := 'ftUnknown';
    ftString: Result := 'ftString';
    ftSmallint: Result := 'ftSmallint';
    ftInteger: Result := 'ftInteger';
    ftWord: Result := 'ftWord';
    ftBoolean: Result := 'ftBoolean';
    ftFloat: Result := 'ftFloat';
    ftCurrency: Result := 'ftCurrency';
    ftBCD: Result := 'ftBCD';
    ftDate: Result := 'ftDate';
    ftTime: Result := 'ftTime';
    ftDateTime: Result := 'ftDateTime';
    ftBytes: Result := 'ftBytes';
    ftVarBytes: Result := 'ftVarBytes';
    ftAutoInc: Result := 'ftAutoInc';
    ftBlob: Result := 'ftBlob';
    ftMemo: Result := 'ftMemo';
    ftGraphic: Result := 'ftGraphic';
    ftFmtMemo: Result := 'ftFmtMemo';
    ftParadoxOle: Result := 'ftParadoxOle';
    ftDBaseOle: Result := 'ftDBaseOle';
    ftTypedBinary: Result := 'ftTypedBinary';
    ftCursor: Result := 'ftCursor';
    ftFixedChar: Result := 'ftFixedChar';
    ftWideString: Result := 'ftWideString';
    ftLargeint: Result := 'ftLargeint';
    ftADT: Result := 'ftADT';
    ftArray: Result := 'ftArray';
    ftReference: Result := 'ftReference';
    ftDataSet: Result := 'ftDataSet';
    ftOraBlob: Result := 'ftOraBlob';
    ftOraClob: Result := 'ftOraClob';
    ftVariant: Result := 'ftVariant';
    ftInterface: Result := 'ftInterface';
    ftIDispatch: Result := 'ftIDispatch';
    ftGuid: Result := 'ftGuid';
    ftTimeStamp: Result := 'ftTimeStamp';
    ftFMTBcd: Result := 'ftFMTBcd';
    ftFixedWideChar: Result := 'ftFixedWideChar';
    ftWideMemo: Result := 'ftWideMemo';
    ftOraTimeStamp: Result := 'ftOraTimeStamp';
    ftOraInterval: Result := 'ftOraInterval';
    ftLongWord: Result := 'ftLongWord';
    ftShortint: Result := 'ftShortint';
    ftByte: Result := 'ftByte';
    ftExtended: Result := 'Extended';
    ftConnection: Result := 'ftConnection';
    ftParams: Result := 'ftConnection';
    ftStream: Result := 'ftStream';
    ftTimeStampOffset: Result := 'ftTimeStampOffset';
    ftObject: Result := 'ftObject';
    ftSingle: Result := 'ftSingle';
  else
    Result := 'nun';
  end;
end;


{ TDBConnection }

constructor TDBConnection.Create;
begin
  // Принициализации управления драйвера
  if CountConnect <= 0 then
  begin
    SetInitializationDriverLink;
    Inc(CountConnect);
  end
  else
    Inc(CountConnect);
  inherited Create;
  FParams := TStringList.Create;
  FConnection := TFDConnection.Create(nil);
  FTransaction := TFDTransaction.Create(nil);
  FQuery := TFDQuery.Create(nil);
  FConnection.Transaction := FTransaction;
  FConnection.UpdateTransaction := FTransaction;
  FQuery.Connection := FConnection;
  FQuery.Transaction := FTransaction;
  FQuery.UpdateTransaction := FTransaction;
  FActiveExec := False;

end;

destructor TDBConnection.Destroy;
begin
  Self.Close;
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  FreeAndNil(FParams);
  if CountConnect <= 1 then
    SetFinalizationDriverLink
  else
    Dec(CountConnect);
  inherited;
end;

procedure TDBConnection.DoMessageError(const AMsg: String);
begin
  // Отправляем сообщение об ошибками
  // raise Exception.Create(AMsg);
  TLogger.Log(AMsg);
end;

procedure TDBConnection.Open;
begin
  try
    FConnection.Params.Assign(FParams);
    FConnection.Open;
  except
    on E: Exception do
      raise Exception.Create('Error Message [TDBConnection.Connect]' +
        E.Message);
  end;
end;

procedure TDBConnection.Close;
begin
  FConnection.Close;
end;

procedure TDBConnection.DefaultConnection(const AFileName: String);
begin
  with Params do
  begin
    Clear;
    Add('DriverID=SQLite');
    Add('database=' + AFileName);
  end;
end;

function TDBConnection.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TDBConnection.GetExecSQL(const ASQL: String): Boolean;
begin
  Result := Self.GetExecSQL(ASQL, [], []);
end;

function TDBConnection.GetExecSQL(const ASQL: String;
  const AParams: array of Variant): Boolean;
begin
  Result := Self.GetExecSQL(ASQL, AParams, []);
end;

function TDBConnection.GetExecSQL(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType): Boolean;
begin
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');

  Result := False;
  if not Self.Connected then
    Exit;

  try

    FActiveExec := True;
    try
      FConnection.StartTransaction;
      FConnection.ExecSQL(ASQL, AParams, ATypes);
      Result := True;
      FConnection.Commit;
    finally
      FActiveExec := False;
    end;

  except
    on E: Exception do
    begin
      FConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]'
        + sLineBreak + ASQL + sLineBreak + E.Message);
      Result := False;
    end;
  end;

end;

function TDBConnection.GetExecSQLScalar(const ASQL: String): Variant;
begin
  Result := Self.GetExecSQLScalar(ASQL,[],[]);
end;

function TDBConnection.GetExecSQLScalar(const ASQL: String;
  const AParams: array of Variant): Variant;
begin
  Result := Self.GetExecSQLScalar(ASQL,AParams,[]);
end;

function TDBConnection.GetExecSQLScalar(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType): Variant;
begin
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');

  if not Self.Connected then
    Exit;

  try

    FActiveExec := True;
    try
      FConnection.StartTransaction;
      Result := FConnection.ExecSQLScalar(ASQL, AParams, ATypes);
      FConnection.Commit;
    finally
      FActiveExec := False;
    end;

  except
    on E: Exception do
    begin
      FConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]' + E.Message);
      Result := False;
    end;
  end;
end;

function TDBConnection.GetSelect(const ASQL: String): TDataSet;
begin
  Result := Self.GetSelect(ASQL, [], []);
end;

function TDBConnection.GetSelect(const ASQL: String;
  const AParams: array of Variant): TDataSet;
begin
  Result := Self.GetSelect(ASQL, AParams, []);
end;

function TDBConnection.GetSelect(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType): TDataSet;
begin
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');

  Result := nil;
  if not Self.Connected then
    Exit;

  try
    FActiveExec := True;
    try
      // FConnection.StartTransaction;
      FQuery.Open(ASQL, AParams, ATypes);
      Result := FQuery;
      // FConnection.Commit;
    finally
      FActiveExec := False;
    end;
  except
    on E: Exception do
    begin
      // FConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]' + E.Message);
      Result := nil;
    end;
  end;
end;

function TDBConnection.GetSelectCreateDataSet(const ASQL: String): TDataSet;
begin
  Result := GetSelectCreateDataSet(ASQL,[],[]);
end;

function TDBConnection.GetSelectCreateDataSet(const ASQL: String;
  const AParams: array of Variant): TDataSet;
begin
  Result := GetSelectCreateDataSet(ASQL,AParams,[]);
end;

function TDBConnection.GetSelectCreateDataSet(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType): TDataSet;
var
  xQuery: TFDQuery;
begin
  Result := nil;
  if not Self.Connected then
    Exit;

  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');
  xQuery := TFDQuery.Create(nil);
  try
    FActiveExec := True;
    try
      xQuery.Connection := FConnection;
      xQuery.Transaction := FTransaction;
      xQuery.UpdateTransaction := FTransaction;
      xQuery.Open(ASQL,AParams,ATypes);
    finally
      FActiveExec := False;
    end;
    Result := xQuery;
  except
    on E: Exception do
    begin
      DoMessageError('Error Message [GetSelectCreateDataSet]' + E.Message);
      Result := nil;
      FreeAndNil(xQuery);
    end;
  end;
end;

procedure TDBConnection.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TDBConnection.Commit;
begin
  FConnection.Commit;
end;

procedure TDBConnection.Rollback;
begin
  FConnection.Rollback;
end;

(*******************************************************************************
  Управляющие процедуры соединением
*******************************************************************************)

class function TDBConnection.GetIsConnected: Boolean;
begin
  Result := DBConnection.Connected;
end;

class function TDBConnection.GetConnectionParam(const AParams: TStrings): TDBConnection;
begin
  DBConnection.Params.Assign(AParams);
  DBConnection.Open;
  Result := DBConnection;
end;

class function TDBConnection.GetCreateDBConnection(const AParam: TStrings): TDBConnection;
var
  xDBConnection: TDBConnection;
begin
  xDBConnection := TDBConnection.Create;
  xDBConnection.Params.Assign(AParam);
  xDBConnection.Open;
  Result := xDBConnection;
end;

class procedure TDBConnection.SetDisConnected(ADBConnection: TDBConnection);
begin
  if Assigned(ADBConnection) then
    ADBConnection.Close;
end;

initialization
  DBConnection := TDBConnection.Create;

finalization
  FreeAndNil(DBConnection);

end.
