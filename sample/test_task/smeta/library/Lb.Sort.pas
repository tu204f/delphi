unit Lb.Sort;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Lb.SysUtils,
  Lb.StringsFile,
  SQLiteWrap,
  Vcl.ExtCtrls;

type
  TSort = class;

  ///<summary>Поток преобразования</summary>
  TSortIndex = class(TBaseThread)
  public const
    SIZE_BUFFER_DATA_BASE = 100000;
  private
    FStringsFile: TStringsFile;
    FParamValues: TParamValueList;
  protected
    FFileNameDataBase: String;
    FSLDB: TSQLiteDatabase;
    procedure SetFileNameDataBase(const Value: String);
  protected
    procedure SetParamValues;
    procedure DoWork; override;
  private
    FMaxSize: Int64;
    FPosition: Int64;
    FOnBlockParamValueBegin: TNotifyEvent;
    FOnBlockParamValueEnd: TNotifyEvent;
    procedure DoBlockParamValueBegin;
    procedure DoBlockParamValueEnd;
  public
    constructor Create;
    destructor Destroy; override;
    property StringsFiles: TStringsFile write FStringsFile;
    property FileNameDataBase: String write SetFileNameDataBase;
    property OnBlockParamValueBegin: TNotifyEvent write FOnBlockParamValueBegin;
    property OnBlockParamValueEnd: TNotifyEvent write FOnBlockParamValueEnd;
  public
    property MaxSize: Int64 read FMaxSize;
    property Position: Int64 read FPosition;
  end;

  // Поток формирование результата
  TSortResult  = class(TBaseThread)
  private
    FFileNameResult: String;
    FFileNameDataBase: String;
    FSLDB: TSQLiteDatabase;
    procedure SetFileNameDataBase(const Value: String);
  private
    FRow: Integer;
    FRowCount: Integer;
  protected
    procedure DoWork; override;
  public
    constructor Create;
    destructor Destroy; override;
    property FileNameDataBase: String write SetFileNameDataBase;
    property FileNameResult: String write FFileNameResult;
    property Row: Integer read FRow;
    property RowCount: Integer read FRowCount;
  end;

  TParam = record
    TypeEvent: Integer;
    TypeObject: Integer;
    Index: Integer;
    Max: Integer;
  private
    function GetProcent: Integer;
    function GetToString: String;
  public
    property Procent: Integer read GetProcent;
    property ToString: String read GetToString;
  end;

  TOnSortChange = procedure(Sender: TObject; const AParam: TParam) of object;

  TSort = class(TObject)
  private
    FParam: TParam;
  private
    FFileNameResult: String;
    FFileNameDataBase: String;
    FSortIndex  : TSortIndex;
    FSortResult : TSortResult;
    FStringsFile: TStringsFile;
    FOnSortChange: TOnSortChange;
    FOnEndSort: TNotifyEvent;
  private
    FTimer: TTimer;
    procedure TimerOnTimer(Sender: TObject);
  protected
    procedure DoSortChange(const AParam: TParam);
  private
    procedure SortOnBegin(Sender: TObject);
    procedure SortOnEnd(Sender: TObject);
    procedure SortOnBlockParamValueBegin(Sender: TObject);
    procedure SortOnBlockParamValueEnd(Sender: TObject);
    procedure SortResultOnBegin(Sender: TObject);
    procedure SortResultOnEnd(Sender: TObject);
    function GetIsActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(const AFileName, AFileNameResult, AFileNameDataBase: String);
    procedure Stop;
    property IsActive: Boolean read GetIsActive;
    property OnSortChange: TOnSortChange write FOnSortChange;
    property OnEndSort: TNotifyEvent write FOnEndSort;
  end;

implementation

{ TSortIndex }

constructor TSortIndex.Create;
begin
  inherited Create;
  FParamValues := TParamValueList.Create;
  FSLDB := nil;
end;

destructor TSortIndex.Destroy;
begin
  if Assigned(FSLDB) then
    FreeAndNil(FSLDB);
  FreeAndNil(FParamValues);
  inherited;
end;

procedure TSortIndex.SetFileNameDataBase(const Value: String);

  procedure SetCreateTable;
  var
    xSQL: TStrings;
  begin
    xSQL := TStringList.Create;
    try
      with xSQL do
      begin
        Add('create table if not exists param_values(');
        Add(' number integer,');
        Add(' value text');
        Add(')');
      end;
      FSLDB.ExecSQL(xSQL.Text);
    finally
      FreeAndNil(xSQL);
    end;
  end;

  procedure SetCreateIndex;
  var
    xSQL: String;
  begin
    xSQL := 'create index if not exists index_param_values ' +
            'on param_values (number asc, value asc)';
    FSLDB.ExecSQL(xSQL);
  end;

  procedure SetDeleteTable;
  var
    xSQL: String;
  begin
    xSQL := 'delete from param_values';
    FSLDB.ExecSQL(xSQL);
  end;

begin
  FFileNameDataBase := Value;
  FSLDB := TSQLiteDatabase.Create(FFileNameDataBase);
  SetCreateTable;
  SetCreateIndex;
  SetDeleteTable;
end;

procedure TSortIndex.DoBlockParamValueBegin;
begin
  if Assigned(FOnBlockParamValueBegin) then FOnBlockParamValueBegin(Self);
end;

procedure TSortIndex.DoBlockParamValueEnd;
begin
  if Assigned(FOnBlockParamValueEnd) then FOnBlockParamValueEnd(Self);
end;

procedure TSortIndex.SetParamValues;
var
  i, Count: Integer;
  xParamValue: TParamValue;
  xQuery: TStrings;
begin
  Synchronize(DoBlockParamValueBegin);
  Count := FParamValues.Count;
  if Count > 0 then
  begin
    {todo: Есть куча недостатков, работы библиотеки нужно доработать}
    {todo: Формирование куча в память, формирование}
    xQuery := TStringList.Create;
    try
      xQuery.Add('insert into param_values(number, value) values');
      for i := 0 to Count - 2 do
      begin

        if Self.Terminated then
        begin
          xQuery.Clear;
          Break;
        end;

        xParamValue := FParamValues[i];
        xQuery.Add('(' + IntToStr(xParamValue.Number) + ',''' + xParamValue.Text + '''),');
      end;

      xParamValue := FParamValues[Count - 1];
      xQuery.Add('(' + IntToStr(xParamValue.Number) + ',''' + xParamValue.Text + ''')');

      if xQuery.Count > 0 then
        FSLDB.ExecSQL(xQuery.Text);
    finally
      FreeAndNil(xQuery);

    end;

  end;
  Synchronize(DoBlockParamValueEnd);
end;

procedure TSortIndex.DoWork;
var
  xS: String;
  xParamValue: TParamValue;
begin
  if Assigned(FStringsFile) then
  begin

    FPosition := 0;
    FMaxSize := FStringsFile.MaxSize;

    FParamValues.Clear;
    xS := FStringsFile.First;
    xParamValue.Value := xS;
    FParamValues.Add(xParamValue);
    while not FStringsFile.EOF do
    begin
      if Self.Terminated then
        Break;

      FPosition := FStringsFile.Position;

      xS := FStringsFile.Next;
      if xS.IsEmpty then
      begin
        Break;
      end
      else
      begin
        xParamValue.Value := xS;

        FParamValues.Add(xParamValue);
        if FParamValues.Count > TSortIndex.SIZE_BUFFER_DATA_BASE then
        begin
          Self.SetParamValues;
          FParamValues.Clear;
        end;
      end;
    end;
    if FParamValues.Count > 0 then
      Self.SetParamValues;
  end;
end;

{ TSortResult }

constructor TSortResult.Create;
begin
  inherited Create;
  FSLDB := nil;
end;

destructor TSortResult.Destroy;
begin
  if Assigned(FSLDB) then
    FreeAndNil(FSLDB);
  inherited;
end;

procedure TSortResult.SetFileNameDataBase(const Value: String);
begin
  FFileNameDataBase := Value;
  FSLDB := TSQLiteDatabase.Create(FFileNameDataBase);
end;

procedure TSortResult.DoWork;

  function GetIsTable: Boolean;
  begin
    {todo: Нужно реализовать проверку на наличие таблицы}
    Result := True;
  end;

  function GetCountRow: Integer;
  var
    xSQL: String;
  begin
    xSQL := 'select count(*) cnt from param_values';
    Result := FSLDB.GetTableValue(xSQL);
  end;

var
  xS: String;
  xSQL: String;
  xParamValue: TParamValue;
  xTable: TSQLiteTable;
  xFS: TFileStream;
  xBytes: TBytes;
begin
  FRow := 0;
  FRowCount := GetCountRow;
  if FRowCount > 0 then
  begin
    xSQL := 'select "number","value" from "param_values" order by "value" asc, "number" asc';
    xTable := FSLDB.GetTable(xSQL);
    try
      xFS := TFileStream.Create(FFileNameResult,fmCreate);
      try
        xTable.Reset;
        while not xTable.EOF do
        begin

          if Self.Terminated then
            Break;

          xParamValue.Number := xTable.FieldAsInteger(0);
          xParamValue.Text   := xTable.FieldAsString(1);
          xS := xParamValue.Value + sLineBreak;
          xBytes := TEncoding.UTF8.GetBytes(xS);
          xFS.Write(xBytes,Length(xBytes));
          xTable.Next;
          Inc(FRow);
        end;
      finally
        FreeAndNil(xFS);
      end;
    finally
      FreeAndNil(xTable);
    end;
  end;
end;

{ TParam }

function TParam.GetProcent: Integer;
begin
  Result := 0;
  if Self.Max > 0 then
    Result := Trunc((Self.Index/Self.Max) * 100);
end;

function TParam.GetToString: String;
begin
  Result :=
       'TypeEvent: ' + IntToStr(Self.TypeEvent) +
    '; TypeObject: ' + IntToStr(Self.TypeObject) +
     '; Состояние: ' + IntToStr(Self.Procent);
end;

{ TSort }

constructor TSort.Create;
begin
  FSortIndex   := nil;
  FSortResult  := nil;
  FStringsFile := nil;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerOnTimer;
end;

destructor TSort.Destroy;
begin
  if Assigned(FStringsFile) then
    FreeAndNil(FStringsFile);

  if Assigned(FSortResult) then
  begin
    FSortResult.Terminate;
    FSortResult := nil;
  end;

  if Assigned(FSortIndex) then
  begin
    FSortIndex.Terminate;
    FSortIndex := nil;
  end;

  FreeAndNil(FTimer);

  inherited;
end;


procedure TSort.Start(const AFileName, AFileNameResult, AFileNameDataBase: String);
begin
  FFileNameResult := AFileNameResult;
  FFileNameDataBase := AFileNameDataBase;

  FSortIndex := TSortIndex.Create;
  FStringsFile := TStringsFile.Create(AFileName);

  FSortIndex.FileNameDataBase := AFileNameDataBase;
  FSortIndex.StringsFiles     := FStringsFile;
  FSortIndex.Start;

  FSortIndex.OnBegin                := SortOnBegin;
  FSortIndex.OnBlockParamValueBegin := SortOnBlockParamValueBegin;
  FSortIndex.OnBlockParamValueEnd   := SortOnBlockParamValueEnd;
  FSortIndex.OnEnd                  := SortOnEnd;
end;

procedure TSort.Stop;
begin

  if Assigned(FSortResult) then
  begin
    FSortResult.Terminate;
    FSortResult := nil;
  end;

  if Assigned(FSortIndex) then
  begin
    FSortIndex.Terminate;
    FSortIndex := nil;
  end;

end;

procedure TSort.SortOnBegin(Sender: TObject);
begin
  FParam.TypeEvent  := 0;
  FParam.TypeObject := 0;
  FParam.Index      := 0;
  FParam.Max        := 0;

  DoSortChange(FParam);
  FTimer.Enabled    := True;
end;

procedure TSort.SortOnBlockParamValueBegin(Sender: TObject);
begin
  FParam.TypeEvent  := 1;
  FParam.TypeObject := 0;

  DoSortChange(FParam);
end;

procedure TSort.SortOnBlockParamValueEnd(Sender: TObject);
begin
  FParam.TypeEvent  := 2;
  FParam.TypeObject := 0;

  FParam.Index := FParam.Max;

  DoSortChange(FParam);
end;

procedure TSort.SortOnEnd(Sender: TObject);
begin
  FSortIndex := nil;

  FParam.TypeEvent  := 3;
  FParam.TypeObject := 0;

  FSortResult := TSortResult.Create;
  FSortResult.FileNameDataBase := FFileNameDataBase;
  FSortResult.FileNameResult   := FFileNameResult;
  FSortResult.OnBegin  := SortResultOnBegin;
  FSortResult.OnEnd    := SortResultOnEnd;
  FSortResult.Start;

  DoSortChange(FParam);
end;


procedure TSort.SortResultOnBegin(Sender: TObject);
begin
  FParam.TypeEvent  := 4;
  FParam.TypeObject := 1;
  FParam.Index      := 0;
  FParam.Max        := 0;

  DoSortChange(FParam);
end;

procedure TSort.SortResultOnEnd(Sender: TObject);
begin
  FSortResult := nil;

  FParam.TypeEvent  := 5;
  FParam.TypeObject := 1;

  FParam.Index := FParam.Max;

  DoSortChange(FParam);
  FTimer.Enabled := False;

  if Assigned(FOnEndSort) then FOnEndSort(Self);

  if Assigned(FStringsFile) then
  begin
    FreeAndNil(FStringsFile);
    FStringsFile := nil;
  end;

end;

procedure TSort.DoSortChange(const AParam: TParam);
begin
  if Assigned(FOnSortChange) then FOnSortChange(Self,AParam);
end;

function TSort.GetIsActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TSort.TimerOnTimer(Sender: TObject);
begin

  if Assigned(FSortIndex) then
  begin
    FParam.Index := FSortIndex.Position;
    FParam.Max   := FSortIndex.MaxSize;
    DoSortChange(FParam);
  end;

  if Assigned(FSortResult) then
  begin
    FParam.Index := FSortResult.Row;
    FParam.Max   := FSortResult.RowCount;
    DoSortChange(FParam);
  end;

end;



end.
