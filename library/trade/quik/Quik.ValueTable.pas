unit Quik.ValueTable;

interface

{Сохранение данных в файл на диске}
{$DEFINE FILE_CSV}

{Сохронять данные в SQLite - нужно продумать}

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Quik.SysUtils;

type
  TRow = class(TValueList)
  public
    procedure SetCol(const ACol: Integer; const AValue: TValue);
  end;
  TRowList = TObjectList<TRow>;

  TCell = class(TObject)
  private
    FValue: TValue;
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
    function GetAsDate: TDateTime;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsTime: TDateTime;
    function GetAsInt64: Int64;
  protected
    property Value: TValue read GetValue write SetValue;
  public
    procedure Clear;
    property AsString: String read GetAsString;
    property AsDouble: Double read GetAsDouble;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsDate: TDateTime read GetAsDate;
    property AsTime: TDateTime read GetAsTime;
  end;

  /// <summary>
  /// Таблица
  /// </summary>
  /// <remarks>
  /// Установить обратный перебор
  /// </remarks>
  TQuikTable = class(TObject)
  private
    FName: String;
    FRows: TRowList;
    FCell: TCell;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetCells(Col, Row: Integer): TCell;
  private
    function GetValues(Col, Row: Integer): TValue;
  private
    function GetCellByName(const ANameField: String; const ARow: Integer): TCell;
  private
    function GetByName(const ANameField: String): TCell;
  private
    function GetCount: Integer;
  protected
    procedure SetClear;
    function GetRow(const ARow: Integer): TRow;
    procedure SetCells(const ACol, ARow: Integer; const AValue: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetValueBlock(ARow1, ACol1, ARow2, ACol2: Integer; ABlocks: TBlocks);
    property Name: String read FName write FName;
    property Cells[Col, Row: Integer]: TCell read GetCells;
    property CellByName[const ANameField: String; const ARow: Integer]: TCell read GetCellByName;
    property ByName[const ANameField: String]: TCell read GetByName;
    property Values[Col, Row: Integer]: TValue read GetValues;
    property ColCount: Integer read GetColCount;
    property RowCount: Integer read GetRowCount;
    property Count: Integer read GetCount;
  private
    FRowID: Integer;
    function IndexOfName(const ANameField: String): Integer;
  public
    procedure Fisrt;
    procedure Next;

    procedure Last;
    procedure Prior;

    function EOF: Boolean;
    function BOF: Boolean;

    property RowID: Integer read FRowID write FRowID;
    function AsInteger(const ANameField: String): Integer;
    function AsDouble(const ANameField: String): Double;
    function AsString(const ANameField: String): String;
  public
    /// <summary>
    /// Проверяем содержится ли данный параметр в строке
    /// </summary>
    function GetIsRowSearchParam(const ARow: Integer; const AValue: Variant): Boolean;
    function GetRowsSearchParam(const AValue: Variant): String;
    /// <summary>
    /// Если пустая таблица
    /// </summary>
    function IsEmpty: Boolean;
    /// <summary>
    /// Получить список всех полей
    /// </summary>
    procedure SetNameFields(const AStrings: TStrings);
  end;
  TQuikTableList = TObjectList<TQuikTable>;

  TPointCells = record
    Col1, Row1: Integer;
    Col2, Row2: Integer;
  public
    constructor Create(const ACol1, ARow1, ACol2, ARow2: Integer);
    function ToString: String;
  end;

  ///<summary>Событье дабовление блока</summary>
  ///<summary>
  ///   <para>APointCells - координаты ячейки</para>
  ///   <para>AQuikTable  - Таблица куда дабовляется блок</para>
  ///</summary>
  TOnAddValueBlock = procedure(Sender: TObject; APointCells: TPointCells; AQuikTable: TQuikTable) of object;

  /// <summary>
  /// Менаджер таблицы
  /// </summary>
  TQuikManagerTable = class(TObject)
  private
    FValueBlocks: TValueBlockList;
    FTables: TQuikTableList;
    FOnAddValueBlock: TOnAddValueBlock;
  protected
    procedure SetAddTable(const AName: String);
    ///<summary>Таблица инициализации систем таблицы</summary>
    procedure SetInitialization; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfTable(const AName: String): Integer;
    property Tables: TQuikTableList read FTables;
    procedure SetNameTables(const AStrings: TStrings);
    procedure SetValueBlock(AValueBlock: TValueBlock);
  public
    property OnAddValueBlock: TOnAddValueBlock write FOnAddValueBlock;
  end;

procedure SetStringToQuikTable(AQuikTable: TQuikTable; AStrings: TStrings);

implementation

uses Lb.Logger;

procedure SetStringToQuikTable(AQuikTable: TQuikTable; AStrings: TStrings);
var
  xS: String;
  xRow, xCol, xRowCount, xColCount: Integer;
begin
  if not Assigned(AQuikTable) then Exit;
  if not Assigned(AStrings) then Exit;
  xRowCount := AQuikTable.RowCount;
  xColCount := AQuikTable.ColCount;
  if (xRowCount > 0) and (xColCount > 0) then
  begin
    for xRow := 0 to xRowCount - 1 do
    begin
      xS := '';
      for xCol := 0 to xColCount - 1 do
        xS := xS + AQuikTable.Cells[xCol,xRow].AsString + ';';
      AStrings.Add(xS);
    end;
  end;
end;

{ TCell }

function TCell.GetAsDouble: Double;
begin
  Result := 0;
  case FValue.TypeValue of
    TDT_FLOAT: Result := FValue.Value;
    TDT_STRING: Result := StrToIntDef(Self.AsString,0);
    TDT_INTEGER: Result := FValue.Value;
  end;
end;

function TCell.GetAsInt64: Int64;
begin
  Result := 0;
  case FValue.TypeValue of
    TDT_FLOAT: Result := Trunc(FValue.Value);
    TDT_STRING: Result := StrToIntDef(Self.AsString,0);
    TDT_INTEGER: Result := FValue.Value;
  end;
end;

function TCell.GetAsInteger: Integer;
begin    
  Result := 0;
  case FValue.TypeValue of
    TDT_FLOAT: Result := Trunc(FValue.Value);
    TDT_STRING: Result := StrToIntDef(FValue.Value,0);
    TDT_INTEGER: Result := FValue.Value;
  end;
end;

function TCell.GetAsString: String;
begin
  Result := '';
  if (FValue.TypeValue = TDT_FLOAT) or
     (FValue.TypeValue = TDT_STRING) or
     (FValue.TypeValue = TDT_INTEGER) then
      Result := VarToStrDef(FValue.Value,'');
end;

function TCell.GetAsTime: TDateTime;
var
  xS: String;
begin
  xS := Self.AsString;
  if not xS.IsEmpty then
    xS := Copy(xS,1,2) + ':' + Copy(xS,3,2)  + ':' + Copy(xS,5,2);
  Result := StrToTimeDef(xS,0);
end;

procedure TCell.Clear;
begin
  FillChar(FValue,SizeOf(FValue),0);
end;

function TCell.GetAsDate: TDateTime;
var
  xS: String;
begin
  xS := Self.AsString;
  if not xS.IsEmpty then
    xS := Copy(xS,7,2) + '.' + Copy(xS,5,2)  + '.' + Copy(xS,1,4);
  Result := StrToDateDef(xS,0);
end;

function TCell.GetValue: TValue;
begin
  Result := FValue;
end;

procedure TCell.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

{ TRow }

procedure TRow.SetCol(const ACol: Integer; const AValue: TValue);
begin
  if Self.Count >= ACol then
  begin
    Self.Items[ACol - 1] := AValue;
  end
  else if Self.Count < ACol then
  begin
    while Self.Count <= ACol do
      Self.Add(TValue.Create(TDT_NULL,Unassigned));
    Self.Items[ACol - 1] := AValue;
  end;
end;

{ TQuikTable }

constructor TQuikTable.Create;
begin
  FRows := TRowList.Create;
  FCell := TCell.Create;
end;

destructor TQuikTable.Destroy;
begin
  FreeAndNil(FCell);
  FreeAndNil(FRows);
  inherited;
end;

procedure TQuikTable.SetClear;
begin
  FRows.Clear;
end;

procedure TQuikTable.SetNameFields(const AStrings: TStrings);
var
  i, Count: Integer;
begin
  if not Self.IsEmpty then
  begin
    if Assigned(AStrings) then
    begin
      AStrings.Clear;
      Count := Self.ColCount;
      if Count > 0 then
        for i := 0 to Count - 1 do
          AStrings.Add(Self.Cells[i,0].AsString)
    end;
  end;
end;

function TQuikTable.GetRow(const ARow: Integer): TRow;
var
  xRow: TRow;
begin
  xRow := nil;
  if FRows.Count >= ARow then
  begin
    xRow := FRows[ARow - 1];
  end
  else
    while FRows.Count < ARow do
    begin
      xRow := TRow.Create;
      FRows.Add(xRow);
    end;
  Result := xRow;
end;

procedure TQuikTable.SetCells(const ACol, ARow: Integer; const AValue: TValue);
var
  xRow: TRow;
begin
  xRow := GetRow(ARow);
  if Assigned(xRow) then
    xRow.SetCol(ACol,AValue)
  else
    raise Exception.Create('Error Message: Не верно определена строка');
end;

procedure TQuikTable.SetValueBlock(ARow1, ACol1, ARow2, ACol2: Integer; ABlocks: TBlocks);
var
  xValue: TValue;
  i, Count, xRow, xCol: Integer;
begin
  if Assigned(ABlocks) then
  begin
    Count := ABlocks.Count;
    if Count > 0 then
    begin
      xRow := ARow1;
      xCol := ACol1;
      for i := 0 to Count - 1 do
      begin
        xValue := ABlocks.Items[i];
        SetCells(xCol,xRow,xValue);
        Inc(xCol);
        if xCol > ACol2 then
        begin
          xCol := ACol1;
          Inc(xRow);
        end;
      end;
    end;
  end;
end;

function TQuikTable.GetColCount: Integer;
begin
  Result := 0;
  if FRows.Count > 0 then
    Result := FRows[0].Count;
end;

function TQuikTable.GetCount: Integer;
var
  xCount: Integer;
begin
  Result := 0;
  xCount := FRows.Count - 1;
  if xCount >= 0 then
    Result := xCount;
end;

function TQuikTable.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TQuikTable.GetCellByName(const ANameField: String; const ARow: Integer): TCell;
var
  xCol: Integer;
begin
  xCol := Self.IndexOfName(ANameField);
  if xCol >= 0 then
    Result := Self.Cells[xCol,ARow]
  else
  begin
    FCell.Clear;
    Result := FCell;
  end;
end;

function TQuikTable.GetByName(const ANameField: String): TCell;
begin
  Result := GetCellByName(ANameField,Self.RowID);
end;


function TQuikTable.GetCells(Col, Row: Integer): TCell;
var
  xRow: TRow;
begin
  FCell.Clear;
  if (Row >= 0) and (Row < FRows.Count) then
  begin
    xRow := FRows[Row];
    if Assigned(xRow) then
      FCell.Value := xRow[Col];
  end;
  Result := FCell;
end;

function TQuikTable.GetValues(Col, Row: Integer): TValue;
var
  xRow: TRow;
  xValue: TValue;
begin
  xValue.TypeValue := TDT_NULL;
  xValue.Value := Unassigned;
  if (Row >= 0) and (FRows.Count > Row) then
  begin
    xRow := FRows[Row];
    if Assigned(xRow) then
    begin
      xValue := xRow[Col];
      Result := xValue;
    end;
  end;
  Result := xValue;
end;

function TQuikTable.IndexOfName(const ANameField: String): Integer;
var
  xS: String;
  i, Count: Integer;
begin
  Result := -1;
  Count := Self.ColCount;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xS := Self.Cells[i,0].AsString;
      if SameText(xS,ANameField) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TQuikTable.IsEmpty: Boolean;
begin
  Result := (Self.RowCount = 0) and (Self.ColCount = 0);
end;

procedure TQuikTable.Last;
begin
  FRowID := Self.RowCount - 1;
end;

procedure TQuikTable.Fisrt;
begin
  FRowID := 1;
end;

procedure TQuikTable.Next;
begin
  Inc(FRowID);
end;

procedure TQuikTable.Prior;
begin
  Dec(FRowID);
end;

function TQuikTable.EOF: Boolean;
begin
  Result := FRowID >= Self.RowCount;
end;

function TQuikTable.BOF: Boolean;
begin
  Result := FRowID <= 0;
end;

function TQuikTable.AsInteger(const ANameField: String): Integer;
var
  xCol: Integer;
  xValue: TValue;
begin
  Result := 0;
  xCol := IndexOfName(ANameField);
  if xCol >= 0 then
  begin
    xValue := Self.Values[xCol,FRowID];
    case xValue.TypeValue of
      TDT_FLOAT: Result := Trunc(xValue.Value);
      TDT_STRING: Result := StrToIntDef(xValue.Value,0);
      TDT_INTEGER: Result := xValue.Value;
    end;
  end;
end;

function TQuikTable.AsDouble(const ANameField: String): Double;
var
  xCol: Integer;
  xValue: TValue;
begin
  Result := 0;
  xCol := IndexOfName(ANameField);
  if xCol >= 0 then
  begin
    xValue := Self.Values[xCol,FRowID];
    case xValue.TypeValue of
      TDT_FLOAT: Result := xValue.Value;
      TDT_STRING: Result := StrToFloatDef(xValue.Value,0);
      TDT_INTEGER: Result := xValue.Value;
    end;
  end;
end;

function TQuikTable.AsString(const ANameField: String): String;
var
  xCol: Integer;
  xValue: TValue;
begin
  Result := '';
  xCol := IndexOfName(ANameField);
  if xCol >= 0 then
  begin
    xValue := Self.Values[xCol,FRowID];
    if (xValue.TypeValue = TDT_FLOAT) or
       (xValue.TypeValue = TDT_STRING) or
       (xValue.TypeValue = TDT_INTEGER) then
      Result := VarToStrDef(xValue.Value,'');
  end;
end; 


function TQuikTable.GetIsRowSearchParam(const ARow: Integer; const AValue: Variant): Boolean;
var
  xS, xValue: String;
  i, iCount: Integer;
begin
  Result := False;
  xValue := VarToStrDef(AValue,'');
  if not xValue.IsEmpty then
  begin
    iCount := Self.ColCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xS := Self.Cells[i,ARow].AsString;
        if SameText(xS,xValue) then
        begin  
          Result := True;
          Break;
        end;
      end;
  end;
end;

function TQuikTable.GetRowsSearchParam(const AValue: Variant): String;
var
  xS: String;
  xFields: TStrings;
  xCol, xColCount: Integer;
  xRow, xRowCount: Integer;
begin
  xS := '';
  xColCount := Self.ColCount;
  xRowCount := Self.RowCount;
  if xRowCount > 0 then
  begin
    xFields := TStringList.Create;
    SetNameFields(xFields);
    try
      for xRow := 1 to xRowCount - 1 do
        if GetIsRowSearchParam(xRow,AValue) then
        begin  
          for xCol := 0 to xColCount - 1 do
            xS := xS + xFields[xCol] + '=' + Self.Cells[xCol,xRow].AsString + ';';  
          Result := xS;
          Break;
        end;
    finally
      FreeAndNil(xFields);
    end;
  end;
end;

{ TPointCells }

constructor TPointCells.Create(const ACol1, ARow1, ACol2, ARow2: Integer);
begin
  Col1 := ACol1;
  Col2 := ACol2;
  Row1 := ARow1;
  Row2 := ARow2;
end;

function TPointCells.ToString: String;
begin
  Result := '[' + Col1.ToString + ',' + Row1.ToString + '] - [' +
    Col2.ToString + ',' + Row2.ToString + ']';
end;

{ TQuikManagerTable }

constructor TQuikManagerTable.Create;
begin
  FValueBlocks := TValueBlockList.Create;
  FTables := TQuikTableList.Create;
  SetInitialization;
end;

destructor TQuikManagerTable.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FValueBlocks);
  inherited;
end;

function TQuikManagerTable.IndexOfTable(const AName: String): Integer;
var
  i, Count: Integer;
begin
  Result := -1;
  Count := FTables.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if SameText(FTables[i].Name,AName) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TQuikManagerTable.SetAddTable(const AName: String);
var
  xQuikTable: TQuikTable;
begin
  xQuikTable := TQuikTable.Create;
  xQuikTable.Name := AName;
  FTables.Add(xQuikTable);
end;

procedure TQuikManagerTable.SetInitialization;
begin
  SetAddTable('security');
  SetAddTable('trades');
  SetAddTable('orders');
  SetAddTable('stop_orders');
end;

procedure TQuikManagerTable.SetNameTables(const AStrings: TStrings);
var
  i, Count: Integer;
begin
  if Assigned(AStrings) then
  begin
    AStrings.Clear;
    Count := FTables.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        AStrings.Add(FTables[i].Name);
  end;
end;

procedure TQuikManagerTable.SetValueBlock(AValueBlock: TValueBlock);
var
  xIndex: Integer;
  xQuikTable: TQuikTable;
  xBlocks: TBlocks;
  xPointCells: TPointCells;
begin
  {todo: Каждая таблица это отдельный поток}
  {todo: Прописать потоковыую зависемость работы с данными}
  xIndex := IndexOfTable(AValueBlock.NameTable);
  if xIndex < 0 then
  begin
    xQuikTable := TQuikTable.Create;
    xQuikTable.Name := AValueBlock.NameTable;
    FTables.Add(xQuikTable);
  end
  else
    xQuikTable := FTables[xIndex];

  {todo: то что должно отрабатываться в отдельном потоке}
  xBlocks := TBlocks.Create;
  try
    // Парсим данные по объектам
    SetParserBlockData(AValueBlock.Data,AValueBlock.Size,xBlocks);
    // Пармис данные по значению
    xQuikTable.SetValueBlock(
        AValueBlock.Row1,
        AValueBlock.Col1,
        AValueBlock.Row2,
        AValueBlock.Col2,
        xBlocks);

    xPointCells.Row1 := AValueBlock.Row1;
    xPointCells.Col1 := AValueBlock.Col1;
    xPointCells.Row2 := AValueBlock.Row2;
    xPointCells.Col2 := AValueBlock.Col2;


    if Assigned(FOnAddValueBlock) then
      FOnAddValueBlock(Self,xPointCells,xQuikTable);
  finally
    FreeAndNil(xBlocks);
  end;
end;


end.
