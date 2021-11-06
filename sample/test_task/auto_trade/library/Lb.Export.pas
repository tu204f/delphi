unit Lb.Export;

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
  ///<summary>Базовый поток для экспотра</summary>
  TCustomExport = class(TBaseThread)
  public const
    SIZE_BUFFER_DATA_BASE = 10;
  private
    FStringsFile: TStringsFile;
    FParamValues: TStrings;
  protected
    FFileNameDataBase: String;
    FSLDB: TSQLiteDatabase;
    procedure SetFileNameDataBase(const Value: String);
  protected
    procedure DoWork; override;
  private
    FMaxSize: Int64;
    FPosition: Int64;
    FOnBlockParamValueBegin: TNotifyEvent;
    FOnBlockParamValueEnd: TNotifyEvent;
    procedure DoBlockParamValueBegin;
    procedure DoBlockParamValueEnd;
    function GetProcent: Integer;
 protected
    procedure SetParserRow(S: String; AParams: TStrings);
    function GetInsertValue(S: String): String;
    procedure SetParamValues; virtual;
    procedure SetCreateTable; virtual;
    procedure SetDeleteTable; virtual;
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
    property Procent: Integer read GetProcent;
  end;

  TExportCrossBrendFull = class(TCustomExport)
  protected
    procedure SetParamValues; override;
    procedure SetCreateTable; override;
    procedure SetDeleteTable; override;
  end;


implementation

{ TCustomExport }

constructor TCustomExport.Create;
begin
  inherited Create;
  FParamValues := TStringList.Create;
  FSLDB := nil;
end;

destructor TCustomExport.Destroy;
begin
  if Assigned(FSLDB) then
    FreeAndNil(FSLDB);
  FreeAndNil(FParamValues);
  inherited;
end;

procedure TCustomExport.SetCreateTable;
begin

end;

procedure TCustomExport.SetDeleteTable;
begin

end;

procedure TCustomExport.SetFileNameDataBase(const Value: String);
begin
  FFileNameDataBase := Value;
  FSLDB := TSQLiteDatabase.Create(FFileNameDataBase);
  SetCreateTable;
  SetDeleteTable;
end;

procedure TCustomExport.DoBlockParamValueBegin;
begin
  if Assigned(FOnBlockParamValueBegin) then FOnBlockParamValueBegin(Self);
end;

procedure TCustomExport.DoBlockParamValueEnd;
begin
  if Assigned(FOnBlockParamValueEnd) then FOnBlockParamValueEnd(Self);
end;

procedure TCustomExport.SetParamValues;
begin

end;

procedure TCustomExport.SetParserRow(S: String; AParams: TStrings);
var
  xC: Char;
  tmpS: String;
begin
  if not Assigned(AParams) then
    Exit;

  tmpS := '';
  AParams.Clear;
  for xC in S do
  begin
    if xC = ';' then
    begin
      AParams.Add(tmpS);
      tmpS := '';
    end
    else
      tmpS := tmpS + xC;
  end;
  if not tmpS.IsEmpty then
    AParams.Add(tmpS);
end;

procedure TCustomExport.DoWork;
var
  xS: String;
begin
  if Assigned(FStringsFile) then
  begin

    FPosition := 0;
    FMaxSize := FStringsFile.MaxSize;

    FParamValues.Clear;
    xS := FStringsFile.First;
    FParamValues.Add(xS);
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
        FParamValues.Add(xS);
        if FParamValues.Count > TCustomExport.SIZE_BUFFER_DATA_BASE then
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

function TCustomExport.GetInsertValue(S: String): String;
var
  xS: String;
  i, iCount: Integer;
  xParams: TStrings;
begin
  xParams := TStringList.Create;
  try
    SetParserRow(S,xParams);

    iCount := xParams.Count;
    if iCount > 0 then
    begin
      xS := '(''';
      for i := 0 to iCount - 2 do
        xS := xS + xParams[i] + ''',''';
      xS := xS + xParams[iCount - 1] + ''')';
    end;
    Result := xS;
  finally
    FreeAndNil(xParams);
  end;
end;

function TCustomExport.GetProcent: Integer;
var
  xValue: Integer;
begin
  Result := 0;
  if FMaxSize > 0 then
  begin
    xValue := Trunc((Self.Position/Self.MaxSize) * 100);
    Result := xValue;
  end;
end;

{ TExportCrossBrendFull }

procedure TExportCrossBrendFull.SetCreateTable;
var
  xSQL: TStrings;
begin
  xSQL := TStringList.Create;
  try
    with xSQL do
    begin
      Add('create table if not exists tb_brend_full(');
      Add(' brend text,');
      Add(' number text,');
      Add(' cross_brend text,');
      Add(' cross_number text');
      Add(')');
    end;
    FSLDB.ExecSQL(xSQL.Text);
  finally
    FreeAndNil(xSQL);
  end;
end;

procedure TExportCrossBrendFull.SetDeleteTable;
var
  xSQL: String;
begin
  xSQL := 'delete from tb_brend_full';
  FSLDB.ExecSQL(xSQL);
end;

procedure TExportCrossBrendFull.SetParamValues;
var
  i, Count: Integer;
  xQuery: TStrings;
  xS: String;
begin
  Synchronize(DoBlockParamValueBegin);
  Count := FParamValues.Count;
  if Count > 0 then
  begin
    {todo: Есть куча недостатков, работы библиотеки нужно доработать}
    {todo: Формирование куча в память, формирование}
    xQuery := TStringList.Create;
    try
      xQuery.Add('insert into tb_brend_full(brend, number, cross_brend, cross_number) values');
      for i := 0 to Count - 2 do
      begin

        if Self.Terminated then
        begin
          xQuery.Clear;
          Break;
        end;

        xS := GetInsertValue(FParamValues[i]);
        xQuery.Add(xS + ',');
      end;

      xS := GetInsertValue(FParamValues[Count - 1]);
      xQuery.Add(xS);

      if xQuery.Count > 0 then
        FSLDB.ExecSQL(xQuery.Text);
    finally
      FreeAndNil(xQuery);

    end;

  end;
  Synchronize(DoBlockParamValueEnd);
end;


end.
