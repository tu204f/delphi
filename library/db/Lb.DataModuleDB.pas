(*******************************************************************************
  Для работы с различными базами данных.
*******************************************************************************)
unit Lb.DataModuleDB;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Variants,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.Comp.Client,
  FireDAC.Phys.DB2Def,
  FireDAC.Phys,
  FireDAC.Phys.ODBCBase,
  FireDAC.Phys.DB2,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.PG,
  FireDAC.DApt,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.UI.Intf,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  {$IFDEF FMX}
  FMX.Types, FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  FireDAC.Phys.SQLiteWrapper.Stat;
  {$ELSE}
  Vcl.ExtCtrls, FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  FireDAC.Phys.SQLiteWrapper.Stat;
  {$ENDIF}

const
  ///<summary>Время ожидание выполнения запроса</summary>
  WAITING_TIME = 600;

type
  {todo: Доработать модуль что можно было работать с разными транзакциями}

  TReturning = record
    Name: String;
    ValueType: TFieldType;
    Value: Variant;
  end;

  TReturningList = class(TList<TReturning>)
  public
    function GetIndexOfName(const AName: String): Integer;
    function GetIsName(const AName: String): Boolean;
    function GetValueByName(const AName: String): Variant;
  end;

  TOnDataSetCallBack = procedure(Sender: TObject; ATransID: Integer; ADataSet: TDataSet) of object;
  TOnEventDataSet = procedure(Sender: TObject; ADataSet: TDataSet) of object;

  /// <summary>
  /// Модуль установки соединение
  /// </summary>
  TDataModuleDB = class(TDataModule)
    FDConnection: TFDConnection;
    FDCommand: TFDCommand;
    FDPhysDB2DriverLink: TFDPhysDB2DriverLink;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FDTransaction: TFDTransaction;
    FDQuery: TFDQuery;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDUpdateSQL: TFDUpdateSQL;
    FDPhysPgDriverLink: TFDPhysPgDriverLink;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;
    ///<summary>Ответ на асинхронный запрос</summary>
    procedure AsyncQueryAfterOpen(DataSet: TDataSet);
  public type
    ///<summary>Объект для асинхронного объекта</summary>
    TAsyncQuery = class(TObject)
    private
      FQuery: TFDQuery;
      FWaitingTime: Integer;
      FEventDataSet: TOnEventDataSet;
      FDataModuleDB: TDataModuleDB;
      function GetTag: Integer;
    protected
      procedure DoAsyncQuery(ADataSet: TDataSet);
    public
      constructor Create(const AQuery: TFDQuery); virtual;
      destructor Destroy; override;
      ///<summary>Время ожидание получение ответа не получено</summary>
      function IsWaitingTime: Boolean;
      property Query: TFDQuery read FQuery;
      property Tag: Integer read GetTag;
      property EventDataSet: TOnEventDataSet write FEventDataSet;
      property DataModuleDB: TDataModuleDB read FDataModuleDB write FDataModuleDB;
    end;
    TAsyncQueryList = TObjectList<TAsyncQuery>;

  private
    FActiveExec: Boolean;
    FParams: TStrings;
    function GetConnected: Boolean;
    procedure DoMessageError(const AMsg: String);
  private
    FAsyncQueryTimer: TTimer;
    FQuerys: TAsyncQueryList;
    function GetGeneratorTag: Integer;
    function GetIndexOfDataSet(ATag: Integer): Integer;
    procedure AsyncQueryTimerOnTimer(Sender: TObject);
    procedure SetInitializationAsyncQueryTimer;
  private
    FReturnings: TReturningList;
    function GetReturningIndexOf(const AName: String): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    ///<summary>Используем для инициализации SQLite</summary>
    procedure DefaultConnection(const AFileName: String = 'default.db');
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    property Params: TStrings read FParams write FParams;
    property Connected: Boolean read GetConnected;
    property ActiveExec: Boolean read FActiveExec;
  public {здесь рузельтат запроса хранится}
    function ReturningByName(const AName: String): Variant;
    /// <summary>
    /// здесь рузельтат запроса хранится, Типа Insert, Update и Delete
    /// </summary>
    property Returnings: TReturningList read FReturnings;
  public
    // Отправляем запрос
    function GetExecSQL(const ASQL: String): Boolean; overload;
    function GetExecSQL(const ASQL: String; const AParams: array of Variant): Boolean; overload;
    function GetExecSQL(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): Boolean; overload;
    // Отправляем запрос командами
    function GetCommandSQL(const ASQL: String): Integer; overload;
    function GetCommandSQL(const ASQL: String; const AParams: array of Variant): Integer; overload;
    /// <summary>
    /// Для тех запросов, когда производится возврат результат
    /// Insert, UpDate и Delete -> RETURNING
    /// </summary>
    function GetCommandSQL(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): Integer; overload;
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
  public
    {TODO: Возможна ошибочная работа в асинхронном режиме}
    // Для создания TDateSet – подзапроса
    procedure SetAsycSelectCreateDataSet(const ASQL: String; ACallBackDataSet: TOnEventDataSet); overload;
    procedure SetAsycSelectCreateDataSet(const ASQL: String; const AParams: array of Variant; ACallBackDataSet: TOnEventDataSet); overload;
    ///<summary>Асинхронный запрос в базу</summary>
    ///<returns>Возвращаем номер индекса запроса</returns>
    procedure SetAsycSelectCreateDataSet(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType; ACallBackDataSet: TOnEventDataSet); overload;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TReturningList }

function TReturningList.GetIndexOfName(const AName: String): Integer;
var
  xReturning: TReturning;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xReturning := Self.Items[i];
      if SameText(xReturning.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TReturningList.GetIsName(const AName: String): Boolean;
begin
  Result := (GetIndexOfName(AName) >= 0);
end;

function TReturningList.GetValueByName(const AName: String): Variant;
var
  xIndex: Integer;
begin
  xIndex := GetIndexOfName(AName);
  if xIndex >= 0 then
    Result := Self.Items[xIndex].Value
  else
    Result := null;
end;

{ TDataModuleDB.TAsyncQuery }

constructor TDataModuleDB.TAsyncQuery.Create(const AQuery: TFDQuery);
begin
  FDataModuleDB := nil;
  FWaitingTime := WAITING_TIME;
  FQuery := AQuery;
end;

destructor TDataModuleDB.TAsyncQuery.Destroy;
begin
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  inherited;
end;

function TDataModuleDB.TAsyncQuery.GetTag: Integer;
begin
  Result := -1;
  if Assigned(FQuery) then
    Result := FQuery.Tag;
end;

function TDataModuleDB.TAsyncQuery.IsWaitingTime: Boolean;
begin
  Result := FWaitingTime <= 0;
  Dec(FWaitingTime);
end;

procedure TDataModuleDB.TAsyncQuery.DoAsyncQuery(ADataSet: TDataSet);
begin
  if Assigned(FEventDataSet) then
    FEventDataSet(FDataModuleDB,ADataSet);
end;

{ TDataModuleDB }

constructor TDataModuleDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitializationAsyncQueryTimer;
  FParams := TStringList.Create;
  FQuerys := TAsyncQueryList.Create;
  FReturnings := TReturningList.Create;
end;

destructor TDataModuleDB.Destroy;
begin
  FreeAndNil(FAsyncQueryTimer);
  FreeAndNil(FReturnings);
  FreeAndNil(FQuerys);
  FreeAndNil(FParams);
  inherited;
end;

procedure TDataModuleDB.SetInitializationAsyncQueryTimer;
begin
  FAsyncQueryTimer := TTimer.Create(nil);
  FAsyncQueryTimer.Interval := 1000;
  FAsyncQueryTimer.Enabled := True;
  FAsyncQueryTimer.OnTimer := AsyncQueryTimerOnTimer;
end;

procedure TDataModuleDB.DoMessageError(const AMsg: String);
begin
  // Генерация сообщение
  raise Exception.Create(AMsg);
end;

function TDataModuleDB.GetConnected: Boolean;
begin
  Result := FDConnection.Connected;
end;

procedure TDataModuleDB.Open;
begin
  try
    FDConnection.Params.Assign(FParams);
    FDConnection.Open;
    FQuerys.Clear;
  except
    on E: Exception do
      raise Exception.Create('Error Message [TDBConnection.Connect]' + E.Message);
  end;
end;

procedure TDataModuleDB.Close;
begin
  FDConnection.Close;
end;

procedure TDataModuleDB.DefaultConnection(const AFileName: String);
begin
  with Params do
  begin
    Clear;
    Add('DriverID=SQLite');
    Add('database=' + AFileName);
  end;
  Self.Open;
end;

procedure TDataModuleDB.StartTransaction;
begin
  FDConnection.StartTransaction;
end;

procedure TDataModuleDB.Commit;
begin
  FDConnection.Commit;
end;

procedure TDataModuleDB.Rollback;
begin
  FDConnection.Rollback;
end;


function TDataModuleDB.GetExecSQL(const ASQL: String): Boolean;
begin
  Result := Self.GetExecSQL(ASQL,[],[]);
end;

function TDataModuleDB.GetExecSQL(const ASQL: String;
  const AParams: array of Variant): Boolean;
begin
  Result := Self.GetExecSQL(ASQL,AParams,[]);
end;

function TDataModuleDB.GetExecSQL(const ASQL: String;
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
      FDConnection.StartTransaction;
      FDConnection.ExecSQL(ASQL, AParams, ATypes);
      Result := True;
      FDConnection.Commit;
    finally
      FActiveExec := False;
    end;
  except
    on E: Exception do
    begin
      FDConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]'
        + sLineBreak + ASQL + sLineBreak + E.Message);
      Result := False;
    end;
  end;
end;

function TDataModuleDB.GetReturningIndexOf(const AName: String): Integer;
var
  i, Count: Integer;
  xReturning: TReturning;
begin
  Result := -1;
  Count := FReturnings.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xReturning := FReturnings[i];
      if SameText(AName,xReturning.Name) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TDataModuleDB.ReturningByName(const AName: String): Variant;
var
  xInd: Integer;
  xReturning: TReturning;
begin
  Result := Null;
  xInd := GetReturningIndexOf(AName);
  if xInd >= 0 then
  begin
    xReturning := FReturnings[xInd];
    Result := xReturning.Value;
  end;
end;

function TDataModuleDB.GetCommandSQL(const ASQL: String): Integer;
begin
  Result := Self.GetCommandSQL(ASQL,[],[]);
end;

function TDataModuleDB.GetCommandSQL(const ASQL: String; const AParams: array of Variant): Integer;
begin
  Result := Self.GetCommandSQL(ASQL,AParams,[]);
end;

function TDataModuleDB.GetCommandSQL(const ASQL: String; const AParams: array of Variant; const ATypes: array of TFieldType): Integer;
var
  xQuery: TFDQuery;
  i, xCount: Integer;
  xParam: TFDParam;
  xReturning: TReturning;
begin
  Result := -1;
  if not Self.Connected then
    Exit;
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');
  xQuery := TFDQuery.Create(nil);
  try
    FActiveExec := True;
    try
      xQuery.Connection := FDConnection;
      xQuery.Transaction := FDTransaction;
      xQuery.UpdateTransaction := FDTransaction;
      Result := xQuery.ExecSQL(ASQL,AParams,ATypes);

      FReturnings.Clear;
      xCount := xQuery.ParamCount;
      if xCount > 0 then
        for i := 0 to xCount - 1 do
        begin
          xParam := xQuery.Params.Items[i];
          xReturning.Name := xParam.Name;
          xReturning.ValueType := xParam.DataType;
          xReturning.Value := xParam.Value;
          FReturnings.Add(xReturning);
        end;

    finally
      FActiveExec := False;
    end;
  except
    on E: Exception do
    begin
      DoMessageError('Error Message [GetSelectCreateDataSet]' + E.Message);
      Result := -1;
      FreeAndNil(xQuery);
    end;
  end;
end;

function TDataModuleDB.GetExecSQLScalar(const ASQL: String): Variant;
begin
  Result := Self.GetExecSQLScalar(ASQL, [],[]);
end;

function TDataModuleDB.GetExecSQLScalar(const ASQL: String;
  const AParams: array of Variant): Variant;
begin
  Result := Self.GetExecSQLScalar(ASQL, AParams,[]);
end;

function TDataModuleDB.GetExecSQLScalar(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType): Variant;
begin
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');

  if not Self.Connected then
    Exit;

  try

    FActiveExec := True;
    try
      FDConnection.StartTransaction;
      Result := FDConnection.ExecSQLScalar(ASQL, AParams, ATypes);
      FDConnection.Commit;
    finally
      FActiveExec := False;
    end;

  except
    on E: Exception do
    begin
      FDConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]' + E.Message);
      Result := False;
    end;
  end;
end;

function TDataModuleDB.GetGeneratorTag: Integer;
var
  xSysTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  xSysTime := System.SysUtils.Time;
  DecodeTime(xSysTime,Hour,Min,Sec,MSec);
  Result := Hour * Min * Sec * MSec;
end;

function TDataModuleDB.GetSelect(const ASQL: String): TDataSet;
begin
  Result := Self.GetSelect(ASQL,[],[]);
end;

function TDataModuleDB.GetSelect(const ASQL: String;
  const AParams: array of Variant): TDataSet;
begin
  Result := Self.GetSelect(ASQL,AParams,[]);
end;

function TDataModuleDB.GetSelect(const ASQL: String;
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
      FDConnection.StartTransaction;
      FDQuery.Open(ASQL, AParams, ATypes);
      Result := FDQuery;
      FDConnection.Commit;
    finally
      FActiveExec := False;
    end;
  except
    on E: Exception do
    begin
      FDConnection.Rollback;
      DoMessageError('Error Message [TDBConnection.GetSelect]' + E.Message);
      Result := nil;
    end;
  end;
end;

function TDataModuleDB.GetSelectCreateDataSet(const ASQL: String): TDataSet;
begin
  Result := Self.GetSelectCreateDataSet(ASQL,[],[]);
end;

function TDataModuleDB.GetSelectCreateDataSet(const ASQL: String;
  const AParams: array of Variant): TDataSet;
begin
  Result := Self.GetSelectCreateDataSet(ASQL,AParams,[]);
end;

function TDataModuleDB.GetSelectCreateDataSet(const ASQL: String;
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
      xQuery.Connection := FDConnection;
      xQuery.Transaction := FDTransaction;
      xQuery.UpdateTransaction := FDTransaction;
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

function TDataModuleDB.GetIndexOfDataSet(ATag: Integer): Integer;
var
  i, Count: Integer;
begin
  Result := -1;
  Count := FQuerys.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if FQuerys[i].Tag = ATag then
      begin
        Result := i;
        Break;
      end;
end;

procedure TDataModuleDB.AsyncQueryAfterOpen(DataSet: TDataSet);
var
  xInd: Integer;
  xAsyncQuery: TAsyncQuery;
begin
  xInd := GetIndexOfDataSet(DataSet.Tag);
  if xInd >= 0 then
  begin
    // Послать сообщение
    xAsyncQuery := FQuerys[xInd];
    xAsyncQuery.DoAsyncQuery(DataSet);
    FQuerys.Delete(xInd);
    if Assigned(xAsyncQuery) then
      FreeAndNil(xAsyncQuery);
  end;
end;

procedure TDataModuleDB.AsyncQueryTimerOnTimer(Sender: TObject);
var
  i, Count: Integer;
  xAsyncQuery: TAsyncQuery;
begin
  // Удаление выполняемого запроса по истечению времени
  Count := FQuerys.Count;
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      xAsyncQuery := FQuerys[i];
      if xAsyncQuery.IsWaitingTime then
      begin
        xAsyncQuery.DoAsyncQuery(nil);
        FQuerys.Delete(i);
      end;
    end;
end;

procedure TDataModuleDB.SetAsycSelectCreateDataSet(const ASQL: String; ACallBackDataSet: TOnEventDataSet);
begin
  Self.SetAsycSelectCreateDataSet(ASQL,[],[],ACallBackDataSet);
end;

procedure TDataModuleDB.SetAsycSelectCreateDataSet(const ASQL: String;
  const AParams: array of Variant; ACallBackDataSet: TOnEventDataSet);
begin
  Self.SetAsycSelectCreateDataSet(ASQL,AParams,[],ACallBackDataSet);
end;

procedure TDataModuleDB.SetAsycSelectCreateDataSet(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType; ACallBackDataSet: TOnEventDataSet);
var
  xQuery: TFDQuery;
  xAsyncQuery: TAsyncQuery;
begin
  // Возращаем номер транзации
  if not Self.Connected then
    Exit;
  if ASQL.IsEmpty then
    raise Exception.Create('Error Message: SQL – запрос не может быть пустым');

  xQuery := TFDQuery.Create(nil);
  xQuery.ResourceOptions.CmdExecMode := amAsync;
  xAsyncQuery := TAsyncQuery.Create(xQuery);
  xAsyncQuery.DataModuleDB := Self;
  xAsyncQuery.EventDataSet := ACallBackDataSet;

  FQuerys.Add(xAsyncQuery);
  xQuery.Tag := GetGeneratorTag;
  try
    FActiveExec := True;
    try
      xQuery.Connection := FDConnection;
      xQuery.Transaction := FDTransaction;
      xQuery.UpdateTransaction := FDTransaction;
      xQuery.AfterOpen := AsyncQueryAfterOpen;
      xQuery.Open(ASQL,AParams,ATypes);
    finally
      FActiveExec := False;
    end;
  except
    on E: Exception do
    begin
      DoMessageError('Error Message [GetSelectCreateDataSet]' + E.Message);
      //FreeAndNil(xQuery);
    end;
  end;
end;



end.
