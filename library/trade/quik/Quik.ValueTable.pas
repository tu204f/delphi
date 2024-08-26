unit Quik.ValueTable;

{$I quik_connect.inc}

interface

{Сохранение данных в файл на диске}
{$DEFINE FILE_CSV}

{Сохронять данные в SQLite - нужно продумать}

uses
  System.SysUtils,
  //System.Variants,
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
  private
    function GetAsDate: TDateTime;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsTime: TDateTime;
    function GetAsInt64: Int64;
  protected
    function IsNullValue: Boolean;
    property Value: TValue read FValue write FValue;
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
    function GetIndexName(const AIndex: Integer): TCell;
  private
    FOnChange: TNotifyEvent;
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
    property IndexName[const AIndex: Integer]: TCell read GetIndexName;
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

    function AsByIndexInteger(const ACol: Integer): Integer;
    function AsByIndexDouble(const ACol: Integer): Double;
    function AsByIndexString(const ACol: Integer): String;

  public
    /// <summary>
    /// Проверяем содержится ли данный параметр в строке
    /// </summary>
    function GetIsRowSearchParam(const ARow: Integer; const AValue: String): Boolean;
    function GetRowsSearchParam(const AValue: Variant): String;
    /// <summary>
    /// Если пустая таблица
    /// </summary>
    function IsEmpty: Boolean;
    /// <summary>
    /// Получить список всех полей
    /// </summary>
    procedure SetNameFields(const AStrings: TStrings);
    /// <summary>
    /// Событие обновление данных
    /// </summary>
    property OnChange: TNotifyEvent write FOnChange;
  end;

  TQuikTableList = class(TObjectList<TQuikTable>)
  public
    function IndexOfTableName(const ATableName: String): Integer;
    function GetTableName(const ATableName: String): TQuikTable;
  end;

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


function GetSecCodeToTable(const ASecCode, ANameTable: String; const ATable: TQuikTable): Boolean;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

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

function GetSecCodeToTable(const ASecCode, ANameTable: String; const ATable: TQuikTable): Boolean;
var
  xSecCode: String;
  i, iCount: Integer;
begin
  Result := False;
  iCount := ATable.RowCount;
  if iCount > 0 then
  begin
    ATable.Fisrt;
    for i := 0 to iCount - 1 do
    begin
      xSecCode := ATable.ByName[ANameTable].AsString;
      if SameText(ASecCode,xSecCode) then
      begin
        Result := True;
        Break;
      end;
      ATable.Next;
    end;
  end;
end;

{ TCell }

function TCell.GetAsDouble: Double;
begin
  Result := 0;
  if IsNullValue then
  begin
    case FValue.TypeValue of
      TTypeData.tdtFloat: Result := FValue.AsDouble;
      TTypeData.tdtString: Result := StrToFloatDef(Self.AsString,0);
      TTypeData.tdtInteger: Result := FValue.AsInteger;
    end;
  end;
end;

function TCell.GetAsInt64: Int64;
begin
  Result := 0;
  if IsNullValue then
  begin
    Result := 0;
    case FValue.TypeValue of
      TTypeData.tdtFloat: Result := Trunc(FValue.AsDouble);
      TTypeData.tdtString: Result := StrToIntDef(Self.AsString,0);
      TTypeData.tdtInteger: Result := FValue.AsInteger;
    end;
  end;
end;

function TCell.GetAsInteger: Integer;
begin    
  Result := 0;
  if IsNullValue then
  begin
    case FValue.TypeValue of
      TTypeData.tdtFloat: Result := Trunc(FValue.AsDouble);
      TTypeData.tdtString: Result := StrToIntDef(FValue.AsString,0);
      TTypeData.tdtInteger: Result := FValue.AsInteger;
    end;
  end;
end;

function TCell.GetAsString: String;
begin
  Result := '';
  if IsNullValue then
  begin
    if (FValue.TypeValue = TTypeData.tdtFloat) or
       (FValue.TypeValue = TTypeData.tdtString) or
       (FValue.TypeValue = TTypeData.tdtInteger) then
        Result := FValue.AsString;
  end;
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

function TCell.IsNullValue: Boolean;
begin
  Result := Assigned(FValue);
end;

procedure TCell.Clear;
begin
  FValue := nil;
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

{ TRow }

procedure TRow.SetCol(const ACol: Integer; const AValue: TValue);
begin
  if Self.Count >= ACol then
  begin
    Self.Items[ACol - 1].Copy(AValue);
  end
  else if Self.Count < ACol then
  begin
    while Self.Count <= ACol do
      Self.Add(TValue.Create);
    Self.Items[ACol - 1].Copy(AValue);
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
  {$IFDEF LOG_QUIK_TABLE}
  TLogger.LogTree(0,'TQuikTable.SetValueBlock');
  {$ENDIF}
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
        {$IFDEF LOG_QUIK_TABLE};
        TLogger.LogTreeText(3,Format('[%d,%d] = %s',[xRow,xCol,xValue.AsString]));
        {$ENDIF}
        SetCells(xCol,xRow,xValue);
        Inc(xCol);
        if xCol > ACol2 then
        begin
          xCol := ACol1;
          Inc(xRow);
        end;
      end;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
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

function TQuikTable.GetIndexName(const AIndex: Integer): TCell;
begin
  Result := Self.Cells[AIndex,Self.RowID];
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
begin
  Result := nil;
  if (Row >= 0) and (FRows.Count > Row) then
  begin
    xRow := FRows[Row];
    if Assigned(xRow) then
      Result := xRow[Col];
  end;
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
      TTypeData.tdtFloat  : Result := Trunc(xValue.AsDouble);
      TTypeData.tdtString : Result := StrToIntDef(xValue.AsString,0);
      TTypeData.tdtInteger: Result := xValue.AsInteger;
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
      TTypeData.tdtFloat  : Result := xValue.AsDouble;
      TTypeData.tdtString : Result := StrToFloatDef(xValue.AsString,0);
      TTypeData.tdtInteger: Result := xValue.AsInteger;
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
    if (xValue.TypeValue = TTypeData.tdtFloat) or
       (xValue.TypeValue = TTypeData.tdtString) or
       (xValue.TypeValue = TTypeData.tdtInteger) then
      Result := xValue.AsString;
  end;
end;

function TQuikTable.AsByIndexInteger(const ACol: Integer): Integer;
var
  xValue: TValue;
begin
  Result := 0;
  if ACol >= 0 then
  begin
    xValue := Self.Values[ACol,FRowID];
    case xValue.TypeValue of
      TTypeData.tdtFloat: Result := Trunc(xValue.AsDouble);
      TTypeData.tdtString: Result := StrToIntDef(xValue.AsString,0);
      TTypeData.tdtInteger: Result := xValue.AsInteger;
    end;
  end;
end;

function TQuikTable.AsByIndexDouble(const ACol: Integer): Double;
var
  xValue: TValue;
begin
  Result := 0;
  if ACol >= 0 then
  begin
    xValue := Self.Values[ACol,FRowID];
    case xValue.TypeValue of
      TTypeData.tdtFloat: Result := xValue.AsDouble;
      TTypeData.tdtString: Result := StrToFloatDef(xValue.AsString,0);
      TTypeData.tdtInteger: Result := xValue.AsInteger;
    end;
  end;
end;

function TQuikTable.AsByIndexString(const ACol: Integer): String;
var
  xValue: TValue;
begin
  Result := '';
  if ACol >= 0 then
  begin
    xValue := Self.Values[ACol,FRowID];
    if (xValue.TypeValue = TTypeData.tdtFloat) or
       (xValue.TypeValue = TTypeData.tdtString) or
       (xValue.TypeValue = TTypeData.tdtInteger) then
      Result := xValue.AsString;
  end;
end;

function TQuikTable.GetIsRowSearchParam(const ARow: Integer; const AValue: String): Boolean;
var
  xS, xValue: String;
  i, iCount: Integer;
begin
  Result := False;
  xValue := AValue;
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

{ TQuikTableList }

function TQuikTableList.IndexOfTableName(const ATableName: String): Integer;
var
  xTable: TQuikTable;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTable := Self.Items[i];
      if SameText(ATableName,xTable.Name) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TQuikTableList.GetTableName(const ATableName: String): TQuikTable;
var
  xIndex: Integer;
begin
  xIndex := IndexOfTableName(ATableName);
  if xIndex >= 0 then
  begin
    Result := Items[xIndex];
  end
  else
    raise Exception.Create('Error Message: Таблица не найдена');
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
