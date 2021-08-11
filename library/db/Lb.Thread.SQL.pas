(*******************************************************************************
  Отработка SQL - зарпосов в отдельном потоке
*******************************************************************************)
unit Lb.Thread.SQL;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  FMX.Types,
  Lb.Memory.DB,
  Lb.Local.DB;

{$IFDEF NOT_WORK_CODE}
type

  /// <summary>
  /// <para>Базовый поток для, где используется два фильтра</para>
  /// <para>текущая история болезни и текущю дату</para>
  /// </summary>
  TCustom2XThread = class(TCustomThreadDB)
  private
    FCurrentDate: TDateTime;
    FKeyHistory: String;
  protected
    function GetScriptSQL: String; override;
  public
    property KeyHistory: String read FKeyHistory write FKeyHistory;
    property CurrentDate:  TDateTime read FCurrentDate write FCurrentDate;
  end;

  /// <summary>
  /// Поток выгружает список пациентов полате
  /// </summary>
  TPatientWardThread = class(TCustomThreadDB)
  private
    FWardName: String;
  protected
    function GetScriptSQL: String; override;
  public
    constructor Create;
    property WardName: String read FWardName write FWardName;
  end;

  /// <summary>
  /// Поток расписание обхода
  /// </summary>
  TBypassingsThread = class(TCustom2XThread)
  public
    constructor Create;
  end;

  /// <summary>
  /// Загрузка распорядка дня
  /// </summary>
  TRuotioneDayThread = class(TCustom2XThread)
  public
    constructor Create;
  end;

  /// <summary>
  /// Назначение иследования
  /// </summary>
  TAssignedExamThread = class(TCustom2XThread)
  public
    constructor Create;
  end;

  /// <summary>
  /// Лекарственная терапия
  /// </summary>
  TMedicationsThread = class(TCustomThreadDB)
  private
    FKeyHistory: String;
    FDeportamintKey: String;
    FFromDate: TDateTime;
  protected
    function GetScriptSQL: String; override;
  public
    constructor Create;
    property KeyHistory: String read FKeyHistory write FKeyHistory;
    property DeportamintKey: String read FDeportamintKey write FDeportamintKey;
    property FromDate: TDateTime read FFromDate write FFromDate;
  end;

  /// <summary>
  /// Параметрические данные
  /// </summary>
  TParametricDataThread = class(TCustomThreadDB)
  private
    FKeyHistory: String;
  protected
    function GetScriptSQL: String; override;
  public
    constructor Create;
    property KeyHistory: String read FKeyHistory write FKeyHistory;
  end;

  /// <summary>
  /// Запрашиваем значение принадлежность пациента к КНЦ
  /// </summary>
  TActiveNKCThread = class(TCustomThreadDB)
  private
    FKeyHistory: String;
  protected
    function GetScriptSQL: String; override;
  public
    constructor Create;
    property KeyHistory: String read FKeyHistory write FKeyHistory;
  end;

type
  /// <summary>
  /// Информация о пациентах
  /// </summary>
  /// <remarks>
  ///  <para>DeportamintKey - Ключ департамента</para>
  ///  <para>Key            - Ключ пациента</para>
  ///  <para>HistoryNumber  - Номер истори болезни</para>
  ///  <para>HospatDate     - Дата гопитализации</para>
  ///  <para>LastName      : String;</para>
  ///  <para>FirstName     : String;</para>
  ///  <para>MiddlenName   : String;</para>
  ///  <para>Closed         - Еще госпитализирван</para>
  /// </remarks>
  TPatient = record
    DeportamintKey: String;
    Key: String;
    HistoryNumber: String;
    HospatDate: String;
    HospatDateFirst: String;
    LastName: String;
    FirstName: String;
    MiddlenName: String;
    Closed: String;
  public
    function GetToString: String;
    procedure SetWriteSetting;
  end;
  TPatientList = TList<TPatient>;


type
  TOnEventInfoSQL = procedure(Sender: TObject; AMsg: String) of object;
  
  /// <summary>
  ///  Менаджер потоков
  /// </summary>
  TManagerThreadSQL = class(TObject)
  private
    FWardName: String;
    FKeyHistory: String;
    FCurrentDate: TDateTime;
    FDeportamintKey: String;
    FFromDate: TDateTime;
  private
    FTimer: TTimer;
    function GetTimerInterval: Integer;
    procedure TimerOnTimer(Sender: TObject);
  private
    FActive: Boolean;
    FOnBegin: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    procedure DoBegin;
    procedure DoEnd;
  private
    FOnEventInfoSQL: TOnEventInfoSQL;
    procedure DoEventInfoSQL(const AMsg: String);
    procedure PatientWardOnTerminate(Sender: TObject);
    procedure BypassingsOnTerminate(Sender: TObject);
    procedure RuotioneDayOnTerminate(Sender: TObject);
    procedure AssignedExamOnTerminate(Sender: TObject);
    procedure MedicationsOnTerminate(Sender: TObject);
    procedure ParametricDataOnTerminate(Sender: TObject);
    procedure ActiveNKCOnTerminate(Sender: TObject);
  protected
    procedure SetStartPatientWard(const AWardName: String);
    procedure SetStartBypassings(const AKeyHistory: String; const ACurrentDate: TDateTime);
    procedure SetStartRuotioneDay(const AKeyHistory: String; const ACurrentDate: TDateTime);
    procedure SetStartAssignedExam(const AKeyHistory: String; const ACurrentDate: TDateTime);
    procedure SetStartMedications(const AKeyHistory, ADeportamintKey: String; const AFromDate: TDateTime);
    procedure SetStartParametricData(const AKeyHistory: String);
    procedure SetStartActiveNKC(const AKeyHistory: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    property WardName: String read FWardName write FWardName;
    property OnEventInfoSQL: TOnEventInfoSQL write FOnEventInfoSQL;
    property OnBegin: TNotifyEvent write FOnBegin;
    property OnEnd: TNotifyEvent write FOnEnd;
  private
    FDateRuotioneDay: TDateTime;
  public
    /// <summary>
    /// Дата для распорядка дня
    /// </summary>
    property DateRuotioneDay: TDateTime write FDateRuotioneDay;
    /// <summary>
    /// Список значене работы
    /// </summary>
    property Active: Boolean read FActive;
  end;

{$ENDIF}


implementation

uses
  Lb.SysUtils,
  Lb.Setting,
  Lb.Logger;

{$IFDEF NOT_WORK_CODE}

{ TCustom2XThread }

function TCustom2XThread.GetScriptSQL: String;
var
  xSQL: String;
  xKeyHistory, xDate: String;
begin
  xKeyHistory := '''' + FKeyHistory + '''';
  xDate := '''' + FormatDateTime('yyyy-mm-dd',FCurrentDate) + '''';
  xSQL := GetBaseScriptSQL;
  xSQL := StringReplace(xSQL,'&key_history',xKeyHistory,[rfReplaceAll]);
  xSQL := StringReplace(xSQL,'&date',xDate,[rfReplaceAll]);
  Result := xSQL;
end;

{ TPatientWardThread }

constructor TPatientWardThread.Create;
begin
  inherited Create;
  FNameScript := 'patient_ward';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

function TPatientWardThread.GetScriptSQL: String;
var
  xSQL: String;
begin
  xSQL := GetBaseScriptSQL;
  xSQL := StringReplace(xSQL,'&wardname',FWardName,[rfReplaceAll]);
  Result := xSQL;
end;


{ TBypassingsThread }

constructor TBypassingsThread.Create;
begin
  inherited Create;
  FNameScript := 'doctor_bypass';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

{ TRuotioneDayThread }

constructor TRuotioneDayThread.Create;
begin
  inherited Create;
  FNameScript := 'research';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

{ TAssignedExamThread }

constructor TAssignedExamThread.Create;
begin
  inherited Create;
  FNameScript := 'assigned_exam';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

{ TMedicationsThread }

constructor TMedicationsThread.Create;
begin
  inherited Create;
  FNameScript := 'medications';
  {$IFDEF REAL}
  FDBConnection := DBConnectionApteka;
  {$ENDIF}
end;

function TMedicationsThread.GetScriptSQL: String;
var
  xSQL: String;
  xKeyHistory, xDeportamintKey, xDate: String;
begin
  xKeyHistory := '''' + FKeyHistory + '''';
  xDeportamintKey := '''' + FDeportamintKey + '''';
  xDate := '''' + FormatDateTime('yyyy-mm-dd',FFromDate) + '''';
  xSQL := GetBaseScriptSQL;
  xSQL := StringReplace(xSQL,'&key_history',xKeyHistory,[rfReplaceAll]);
  xSQL := StringReplace(xSQL,'&deportamint_key',xDeportamintKey,[rfReplaceAll]);
  xSQL := StringReplace(xSQL,'&from_date',xDate,[rfReplaceAll]);
  Result := xSQL;
end;

{ TParametricDataThread }

constructor TParametricDataThread.Create;
begin
  inherited Create;
  FNameScript := 'parametric';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

function TParametricDataThread.GetScriptSQL: String;
var
  xSQL: String;
  xKeyHistory: String;
begin
  xKeyHistory := '''' + FKeyHistory + '''';
  xSQL := GetBaseScriptSQL;
  xSQL := StringReplace(xSQL,'&key_h',xKeyHistory,[rfReplaceAll]);
  Result := xSQL;
end;

{ TActiveNKCThread }

constructor TActiveNKCThread.Create;
begin
  inherited Create;
  FNameScript := 'active_knc';
  {$IFDEF REAL}
  FDBConnection := DBConnectionMedStat;
  {$ENDIF}
end;

function TActiveNKCThread.GetScriptSQL: String;
var
  xSQL: String;
  xKeyHistory: String;
begin
  xKeyHistory := '''' + FKeyHistory + '''';
  xSQL := GetBaseScriptSQL;
  xSQL := StringReplace(xSQL,'&key_history',xKeyHistory,[rfReplaceAll]);
  Result := xSQL;
end;

{ TPatient }

function TPatient.GetToString: String;
begin
  Result := Self.LastName + ' ' + Self.FirstName + ' ' + MiddlenName;
end;

procedure TPatient.SetWriteSetting;
begin
  try
    TSetting.WriteString('config.patient.deportamint_key',Self.DeportamintKey);
    TSetting.WriteString('config.patient.key',Self.Key);
    TSetting.WriteString('config.patient.history_number',Self.HistoryNumber);
    TSetting.WriteString('config.patient.hospat_date',Self.HospatDate);
    TSetting.WriteString('config.patient.last_name',Self.LastName);
    TSetting.WriteString('config.patient.first_name',Self.FirstName);
    TSetting.WriteString('config.patient.middlen_name',Self.MiddlenName);
    TSetting.WriteString('config.patient.closed',Self.Closed);
  except
  end;
end;

{ TManagerThreadSQL }

constructor TManagerThreadSQL.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Interval := GetTimerInterval;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;
  FDateRuotioneDay := 0;
  FActive := False;
end;

destructor TManagerThreadSQL.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

procedure TManagerThreadSQL.SetStartPatientWard(const AWardName: String);
begin
  with TPatientWardThread.Create do
  begin
    WardName := AWardName;
    OnTerminate := PatientWardOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartBypassings(const AKeyHistory: String;
  const ACurrentDate: TDateTime);
begin
  with TBypassingsThread.Create do
  begin
    KeyHistory := AKeyHistory;
    CurrentDate := ACurrentDate;
    OnTerminate := BypassingsOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartRuotioneDay(const AKeyHistory: String;
  const ACurrentDate: TDateTime);
begin
  with TRuotioneDayThread.Create do
  begin
    KeyHistory := AKeyHistory;
    CurrentDate := ACurrentDate;
    OnTerminate := RuotioneDayOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartAssignedExam(const AKeyHistory: String;
  const ACurrentDate: TDateTime);
begin
  with TAssignedExamThread.Create do
  begin
    KeyHistory := AKeyHistory;
    CurrentDate := ACurrentDate;
    OnTerminate := AssignedExamOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartMedications(const AKeyHistory,
  ADeportamintKey: String; const AFromDate: TDateTime);
begin
  with TMedicationsThread.Create do
  begin
    KeyHistory := AKeyHistory;
    DeportamintKey := ADeportamintKey;
    FromDate := AFromDate;
    OnTerminate := MedicationsOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartParametricData(const AKeyHistory: String);
begin
  with TParametricDataThread.Create do
  begin
    KeyHistory := AKeyHistory;
    OnTerminate := ParametricDataOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.SetStartActiveNKC(const AKeyHistory: String);
begin
  with TActiveNKCThread.Create do
  begin
    KeyHistory := AKeyHistory;
    OnTerminate := ActiveNKCOnTerminate;
    Start;
  end;
end;

procedure TManagerThreadSQL.DoEventInfoSQL(const AMsg: String);
begin
  if Assigned(FOnEventInfoSQL) then
    FOnEventInfoSQL(Self,AMsg);
end;

procedure TManagerThreadSQL.TimerOnTimer(Sender: TObject);
begin
   FTimer.Interval := GetTimerInterval;
   FTimer.Enabled := False;
   DoEventInfoSQL('Перезапуск');
   Self.Start;
end;

function TManagerThreadSQL.GetTimerInterval: Integer;
const
  ONE_MINUTE = 60000;
var
  xMin: Integer;
begin
  {$IFDEF DEBUG}
  xMin := TSetting.ReadInteger('config.debug.interval',1000);
  {$ELSE}
  xMin := TSetting.ReadInteger('config.sys.interval',15);
  xMin := xMin * ONE_MINUTE;
  {$ENDIF}
  Result := xMin;  
end;

procedure TManagerThreadSQL.Start;
begin
  if not Self.Active then
  begin
    DoBegin;
    if not FWardName.IsEmpty then
    begin
      DoEventInfoSQL('Загружаем данные.');
      SetStartPatientWard(FWardName);
    end
    else
      raise Exception.Create('Error Message: Не определить номер палаты');
  end;
end;

procedure TManagerThreadSQL.DoBegin;
begin
  FActive := True;
  if Assigned(FOnBegin) then
    FOnBegin(Self);
end;

procedure TManagerThreadSQL.DoEnd;
begin
  FActive := False;
  if Assigned(FOnEnd) then
    FOnEnd(Self);
end;

procedure TManagerThreadSQL.PatientWardOnTerminate(Sender: TObject);
var
  xInd: Integer;
  xPatient: TPatient;
  xTable: TMemTable;
  xPatients: TPatientList;
begin
  if Assigned(Sender) then
  begin

    xTable := TCustomThreadDB(Sender).Table;
    xTable.First;
    xPatients := TPatientList.Create;
    try
      while not xTable.Eof do
      begin
        xPatient.DeportamintKey  := xTable.FieldByName('DEPARTMENTKEY').AsString;
        xPatient.Key             := xTable.FieldByName('KEY').AsString;
        xPatient.HistoryNumber   := xTable.FieldByName('HISTORYNUMBER').AsString;
        xPatient.HospatDate      := xTable.FieldByName('HOSPATDATE').AsString;
        xPatient.HospatDateFirst := xTable.FieldByName('HOSPATDATE_FIRST').AsString;
        xPatient.LastName        := xTable.FieldByName('LASTNAME').AsString;
        xPatient.FirstName       := xTable.FieldByName('FIRSTNAME').AsString;
        xPatient.MiddlenName     := xTable.FieldByName('MIDDLENAME').AsString;
        xPatient.Closed          := xTable.FieldByName('CLOSED').AsString;
        xPatients.Add(xPatient);
        xTable.Next;
      end;
      // ---------------------------------------------------------------------
      if xPatients.Count = 0 then
      begin
        { TODO: Нет выбранного пациента }
        Exit;
      end;
      // ---------------------------------------------------------------------
      xInd := TSetting.ReadInteger('config.sys.bed',0);
      if xInd >= xPatients.Count then
        xInd := xPatients.Count - 1;
      xPatient := xPatients[xInd];
      // ---------------------------------------------------------------------
      FKeyHistory := xPatient.Key;

      if FDateRuotioneDay = 0 then
      begin
        FCurrentDate := Date;
        FDateRuotioneDay := FCurrentDate;
      end;

      FDeportamintKey := xPatient.DeportamintKey;
      FFromDate := StrToDateTime(xPatient.HospatDateFirst);
      xPatient.SetWriteSetting;

    finally
      FreeAndNil(xPatients);
    end;
    {TODO: если данные успешно загружанны}
    SetStartBypassings(FKeyHistory,FCurrentDate);
  end;
  DoEventInfoSQL('Загружаем данные..');
end;

procedure TManagerThreadSQL.BypassingsOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    {TODO: если данные успешно загружанны}
    SetStartRuotioneDay(FKeyHistory,FDateRuotioneDay);
  end;
  DoEventInfoSQL('Загружаем данные...');
end;

procedure TManagerThreadSQL.RuotioneDayOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    {TODO: если данные успешно загружанны}
    SetStartAssignedExam(FKeyHistory,FCurrentDate);
  end;
  DoEventInfoSQL('Загружаем данные....');
end;

procedure TManagerThreadSQL.AssignedExamOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    {TODO: если данные успешно загружанны}
    SetStartMedications(FKeyHistory,FDeportamintKey,FFromDate);
  end;
  DoEventInfoSQL('Загружаем данные.....');
end;

procedure TManagerThreadSQL.MedicationsOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    {TODO: если данные успешно загружанны}
    SetStartParametricData(FKeyHistory);
  end;
  DoEventInfoSQL('Загружаем данные......');
end;

procedure TManagerThreadSQL.ParametricDataOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    {TODO: если данные успешно загружанны}
    SetStartActiveNKC(FKeyHistory);
  end;
  DoEventInfoSQL('Загружаем данные.......');
end;

procedure TManagerThreadSQL.ActiveNKCOnTerminate(Sender: TObject);
begin
  if Assigned(Sender) then
  begin
    FTimer.Enabled := True;
    DoEnd;
  end;
  DoEventInfoSQL('Загружаем данные.......');
end;

{$ENDIF}

end.
