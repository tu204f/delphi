(*******************************************************************************
  Все данные храниться в оперативной
*******************************************************************************)
unit Lb.Memory.DB;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TTypeField = (tfString, tfInteger, tfInt64, tfFloat, tfBoolean,
    tfDate, tfTime, tfDateTime);

  TMemCell = record
    Active: Boolean;
  private
    FValue: String;
    procedure SetValue(const AValue: String);
  public    
    property Value: String read FValue write SetValue;  
  end;

  TCellList = TList<TMemCell>;

  /// <summary>
  /// Поле значение, которые хранит данные
  /// </summary>
  TMemField = class(TCellList)
  private
    FRowID: Integer;
    FNameField: String;
    FTypeField: TTypeField;
    function GetActive: Boolean;
    function GetAsString: String;
    function GetAsInteger: Integer;
    function GetAsInt64: Int64;
    function GetAsFloat: Double;
    function GetAsBool: Boolean;
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    function GetAsDateTime: TDateTime;
    procedure SetAsString(const Value: String);
    procedure SetAsInteger(const Value: Integer);
    procedure SetRowID(const Value: Integer);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsBool(const Value: Boolean);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
  public
    constructor Create;
    /// <summary>
    /// Производим дабовление ячейки
    /// </summary>
    /// <remarks>
    /// Производим дабовление новый строки
    /// </remarks>
    procedure AddCell;
    /// <summary>
    /// Дабовление значения нового ячейки
    /// </summary>
    procedure AddValue(const AValue: String);
    /// <summary>
    /// Наименование поля
    /// </summary>
    property NameField: String read FNameField write FNameField;
    /// <summary>
    /// Тип поля
    /// </summary>
    property TypeField: TTypeField read FTypeField write FTypeField;
  public
    /// <summary>
    /// Индекс - из какой строки данные брать
    /// </summary>
    property RowID: Integer read FRowID write SetRowID;
    property Active: Boolean read GetActive;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBool: Boolean read GetAsBool write SetAsBool;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;
  TMemFieldList = TObjectList<TMemField>;

  /// <summary>
  /// Список полей
  /// </summary>
  TMemFields = class(TObject)
  private
    FList: TMemFieldList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TMemField;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const ANameField: String; ATypeField: TTypeField = tfString): TMemField;
    function IndexOf(const ANameField: String): Integer;
    property Items[Index: Integer]: TMemField read GetItems;
    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///  Хранение информации в памяти
  /// </summary>
  TMemTable = class(TObject)
  private
    FFields: TMemFields;
    FRowID: Integer;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetEof: Boolean;
    function GetCells(Col, Row: Integer): String;
  protected
    procedure SaveString(AString: TStrings);
    procedure LoadString(AString: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FieldActive: Boolean;
    function FieldByName(const ANameField: String): TMemField;
    procedure First;
    procedure Next;
    procedure AddRow;
    property Eof: Boolean read GetEof;
    property RowID: Integer read FRowID write FRowID;
    property Fields: TMemFields read FFields;
    property RowCount: Integer read GetRowCount;
    property ColCount: Integer read GetColCount;
    property Cells[Col, Row: Integer]: String read GetCells;
  public
    function SaveFile(const AFileName: String): Boolean;
    procedure LoadFile(const AFileName: String);
    procedure SaveStream(AStream: TStream);
    procedure LoadStream(AStream: TStream);
  end;


implementation

{ TMemCell }

procedure TMemCell.SetValue(const AValue: String);
begin
  Active := True;
  FValue := AValue;
end;

{ TMemField }

constructor TMemField.Create;
begin
  inherited Create;
  FRowID := 0;
end;

procedure TMemField.SetRowID(const Value: Integer);
begin
  if (Value >= 0) and (Value < Self.Count) then
    FRowID := Value;
end;

procedure TMemField.AddCell;
var
  xCell: TMemCell;
begin
  FillChar(xCell,SizeOf(xCell),0);
  Add(xCell);
end;

procedure TMemField.AddValue(const AValue: String);
var
  xCell: TMemCell;
begin
  FillChar(xCell,SizeOf(xCell),0);
  xCell.Value := AValue;
  xCell.Active := False;
  Add(xCell);
end;
 
function TMemField.GetActive: Boolean;
begin
  Result := Self.Items[FRowID].Active;
end;

// ---------------------------------------------------------------------------

function TMemField.GetAsString: String;
begin
  Result := Self.Items[FRowID].Value;
end;

function TMemField.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Self.AsString,0);
end;

function TMemField.GetAsInt64: Int64;
begin
  Result := StrToInt64Def(Self.AsString,0);
end;

function TMemField.GetAsFloat: Double;
begin
  Result := StrToFloatDef(Self.AsString,0);
end;

function TMemField.GetAsBool: Boolean;
begin
  Result := Boolean(Self.AsInteger);
end;

function TMemField.GetAsDate: TDateTime;
begin
  Result := StrToDateDef(Self.AsString,0);
end;

function TMemField.GetAsTime: TDateTime;
begin
  Result := StrToTimeDef(Self.AsString,0);
end;

function TMemField.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(Self.AsString,0);
end;

// --------------------------------------------------------------------------

procedure TMemField.SetAsString(const Value: String);
var
  xCell: TMemCell;
begin
  xCell := Self.Items[RowID];
  xCell.Active := True;
  xCell.Value := Value;
  Self.Items[RowID] := xCell;
end;

procedure TMemField.SetAsInteger(const Value: Integer);
begin
  Self.AsString := IntToStr(Value);
end;

procedure TMemField.SetAsInt64(const Value: Int64);
begin
  Self.AsString := IntToStr(Value);
end;

procedure TMemField.SetAsFloat(const Value: Double);
begin
  Self.AsString := FloatToStr(Value);
end;

procedure TMemField.SetAsBool(const Value: Boolean);
begin
  Self.AsInteger := Integer(Value);
end;

procedure TMemField.SetAsDate(const Value: TDateTime);
begin
  Self.AsString := DateToStr(Value);
end;

procedure TMemField.SetAsTime(const Value: TDateTime);
begin
  Self.AsString := TimeToStr(Value);
end;

procedure TMemField.SetAsDateTime(const Value: TDateTime);
begin
  Self.AsString := DateTimeToStr(Value);
end;

{ TMemFields }

constructor TMemFields.Create;
begin
  FList := TMemFieldList.Create;
end;

destructor TMemFields.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TMemFields.Clear;
begin
  FList.Clear;
end;

function TMemFields.Add(const ANameField: String; ATypeField: TTypeField): TMemField;
var
  xField: TMemField;
begin
  xField := TMemField.Create;
  with xField do
  begin
    NameField := ANameField;
    TypeField := ATypeField;
  end;
  Result := xField;
  FList.Add(xField);
end;

function TMemFields.IndexOf(const ANameField: String): Integer;
var
  i, Count: Integer;
begin
  Result := -1;
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1  do
      if SameText(Self.Items[i].NameField,ANameField) then
      begin
        Result := i;
        Break;
      end;
end;

function TMemFields.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMemFields.GetItems(Index: Integer): TMemField;
begin
  if (Index >= 0) and (Index < Flist.Count) then
    Result := FList[Index]
  else
    raise Exception.Create('Error Message: Вышли за пределы массива');
end;

{ TMemTable }

constructor TMemTable.Create;
begin
  FFields := TMemFields.Create;
end;

destructor TMemTable.Destroy;
begin
  FreeAndNil(FFields);
  inherited;
end;

procedure TMemTable.Clear;
var
  i, Count: Integer;
begin
  Count := FFields.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
      FFields.Items[i].Clear;
end;

function TMemTable.FieldActive: Boolean;
var
  xField: TMemField;
  i, Count: Integer;
begin
  Result := False;
  Count := FFields.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xField := FFields.Items[i];
      xField.RowID := FRowID;
      if xField.Active then
      begin
        Result := True;
        Break;
      end;      
    end;
end;

function TMemTable.FieldByName(const ANameField: String): TMemField;
var
  xField: TMemField;
  xInd: Integer;
begin
  xInd := FFields.IndexOf(ANameField);
  if xInd >= 0 then
  begin
    xField := FFields.Items[xInd];
    xField.RowID := FRowID;
    Result := xField;
  end
  else
    raise Exception.Create('Error Message: Поле не найдено + ' + ANameField);
end;

procedure TMemTable.AddRow;
var
  i, Count: Integer;
begin
  Count := FFields.Count;
  if Count > 0 then
  begin
    Inc(FRowID);
    for i := 0 to Count - 1 do
      FFields.Items[i].AddCell;
  end;
end;

function TMemTable.GetColCount: Integer;
begin
  Result := FFields.Count;
end;

function TMemTable.GetRowCount: Integer;
begin
  Result := 0;
  if FFields.Count > 0 then
    Result := FFields.Items[0].Count;
end;

procedure TMemTable.First;
begin
  FRowID := 0;
end;

procedure TMemTable.Next;
var
  i, Count: Integer;
begin
  Inc(FRowID);
  Count := FFields.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
      FFields.Items[i].RowID := FRowID;
end;

function TMemTable.GetEof: Boolean;
begin
  Result := FRowID >= Self.RowCount;
end;

function TMemTable.SaveFile(const AFileName: String): Boolean;
var
  xFS: TFileStream;
begin
  try
    xFS := TFileStream.Create(AFileName,fmCreate);
    try
      SaveStream(xFS);
      Result := True;
    finally
      FreeAndNil(xFS);
    end;
  except
    Result := False;
  end;
end;

procedure TMemTable.SaveStream(AStream: TStream);
var
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    SaveString(xStr);
    xStr.SaveToStream(AStream);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMemTable.SaveString(AString: TStrings);

  function GetUnPrinTable(S: String): String;
  var
    tmpS: String;
    i, L: Integer;
  begin
    tmpS := '';
    L := Length(S);
    if L >= 1 then
      for i := 1 to L do
        if (S[i] = #13) or (S[i] = #10) then
          tmpS := tmpS + '\'
        else
          tmpS := tmpS + S[i];
    Result := tmpS;
  end;

var
  xS: String;
  xField: TMemField;
  i, Count: Integer;
begin
  if Assigned(AString) then
  begin
    // Загрузка списка значения
    xS := '';
    Count := FFields.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xField := FFields.Items[i];
        xS := xS + xField.NameField + ';'
      end;
    AString.Add(xS);
    // Значение таблицы
    Self.First;
    while not Self.Eof do
    begin
      xS := '';
      Count := FFields.Count;
      if Count > 0 then
        for i := 0 to Count - 1 do
        begin
          xField := FFields.Items[i];
          xS := xS + GetUnPrinTable(xField.AsString) + ';'
        end;
      AString.Add(xS);
      Self.Next;
    end;
  end;
end;

procedure TMemTable.LoadFile(const AFileName: String);
var
  xFS: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    try
      xFS := TFileStream.Create(AFileName,fmOpenReadWrite);
      try
        LoadStream(xFS);
      finally
        FreeAndNil(xFS);
      end;
    except
    end;
  end;
end;

procedure TMemTable.LoadStream(AStream: TStream);
var
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    xStr.LoadFromStream(AStream);
    LoadString(xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMemTable.LoadString(AString: TStrings);

  procedure SetParser(const S: String; AStrings: TStrings);
  var
//    B: Boolean;
    tmpS: String;
    i, L: Integer;
  begin
//    B := False;
    if Assigned(AStrings) then
    begin
      L := S.Length;
      tmpS := '';
      for i := 1 to L do
      begin
        if S[i] = ';' then
        begin
          AStrings.Add(tmpS);
          tmpS := '';
        end
        else
          tmpS := tmpS + S[i];

      end;
      AStrings.Add(tmpS);
    end;
  end;

  procedure SetFields(S: String);
  var
    xStr: TStrings;
    i, Count: Integer;
  begin
    xStr := TStringList.Create;
    try
      SetParser(S,xStr);
      FFields.Clear;
      Count := xStr.Count;
      if Count > 0 then
        for i := 0 to Count - 1 do
          FFields.Add(xStr[i]);
    finally
      FreeAndNil(xStr);
    end;
  end;

  procedure SetAddRow(S: String);
  var
    xStr: TStrings;
    xField: String;
    i, Count: Integer;
  begin
    {TODO: Требуетс учеть возможность поевление ошибок}
    xStr := TStringList.Create;
    try
      SetParser(S,xStr);
      Count := FFields.Count;
      if Count > 0 then
        for i := 0 to Count - 1 do
        begin
          xField := xStr[i];
          FFields.Items[i].AddValue(xField);
        end;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  i, Count: Integer;
begin
  if Assigned(AString) then
  begin
    Count := AString.Count;
    if Count > 0 then
    begin
      SetFields(AString[0]);
      for i := 1 to Count - 1 do
        SetAddRow(AString[i]);
    end;
  end;
end;

function TMemTable.GetCells(Col, Row: Integer): String;
var
  xMemField: TMemField;
begin
  Result := '';
  if (Col >= 0) and (Col < FFields.Count) then
  begin
    xMemField := FFields.Items[Col];
    if (Row >= 0) and (Row < xMemField.Count) then
    begin
      xMemField.RowID := Row;
      Result := xMemField.AsString;    
    end;
  end;
end;

end.
