unit UnitGridFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  VirtualTrees,
  Vcl.ImgList,
  System.UITypes,
  Data.DB;

{$IFDEF DEBUG}
  {$DEFINE DB_LOG}
  //Лог формирование дерева данных
  {$DEFINE LOG_TREE}
{$ENDIF}

// Возможность создания дерева
{$DEFINE GRID_TREE}

const
  /// <summary>
  /// Максимальный уровень в ложения
  /// </summary>
  CHILD_MAX = 100;

type
  TGridFrame = class;
(*******************************************************************************
  Строки и значения
*******************************************************************************)
  TValue = record
    Name: String;
    Value: Variant;
  public
    constructor Create(const AName: String; const AValue: Variant);
    function ToString: String;
  end;

  /// <summary>
  /// Список значений
  /// </summary>
  TValueList = TList<TValue>;

  /// <summary>
  /// Строка значение
  /// </summary>
  TRow = class(TObject)
  private
    FValues: TValueList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TValue;
    function GetAsValue(AField: String): Variant;
    procedure SetAsValue(AField: String; const Value: Variant);
    function GetAsString(AField: String): String;
    procedure SetAsString(AField: String; const Value: String);
    function GetAsInteger(AField: String): Integer;
    procedure SetAsInteger(AField: String; const Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function AddValue(const AValue: TValue): Integer; overload;
    function AddValue(const AName: String; const AValue: Variant): Integer; overload;
    function IndexOf(const AName: String): Integer;
    property Items[Index: Integer]: TValue read GetItems;
    property Count: Integer read GetCount;
  public
    property AsValue[AField: String]: Variant read GetAsValue write SetAsValue;
    property AsInteger[AField: String]: Integer read GetAsInteger write SetAsInteger;
    property AsString[AField: String]: String read GetAsString write SetAsString;
  end;
  TRowList = TObjectList<TRow>;

  /// <summary>
  /// Список строк
  /// </summary>
  TRows = class(TObject)
  private
    FRows: TRowList;
    FGridFrame: TGridFrame;
    function GetCount: Integer;
    function GetItems(Index: Integer): TRow;
  public
    constructor Create(const AGridFrame: TGridFrame);
    destructor Destroy; override;
    procedure Clear;
    function Add: TRow;
    property Items[Index: Integer]: TRow read GetItems;
    property Count: Integer read GetCount;
  end;

(*******************************************************************************
  Колонки и сортировка
*******************************************************************************)
  TColumn = class;

  /// <summary>
  /// Колонка
  /// </summary>
  TColumn = class(TObject)
  private
    FTitle: String;
    FField: String;
    FChild: Integer;
    FWidth: Integer;
    FVisible: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Child: Integer read FChild write FChild;
    property Title: String read FTitle write FTitle;
    property Field: String read FField write FField;
    property Width: Integer read FWidth write FWidth;
    property Visible: Boolean read FVisible write FVisible;
  end;
  TColumnList = TObjectList<TColumn>;

  /// <summary>
  /// Сипсок колонон
  /// </summary>
  TColumns = class(TObject)
  private
    FColumns: TColumnList;
    FGridFrame: TGridFrame;
    function GetItems(Index: Integer): TColumn;
  public
    constructor Create(const AGridFrame: TGridFrame);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const ATitle, AFieldName: String; AChild: Integer; AWidth: Integer; AVisible: Boolean = True);
    function AddColumn: TColumn;
    procedure SetShowColumn;
    property Items[Index: Integer]: TColumn read GetItems;
  end;

(*******************************************************************************
  Табличные данные представляем, как дерево
*******************************************************************************)

  TValueTree = record
    Field: String;
    Child: Integer;
  public
    constructor Create(const AField: String; const AChild: Integer);
  end;
  TValueTreeList = TList<TValueTree>;

  TNode = class;
  TNodes = class;

  TNode = class(TObject)
  private
    FValue: String;
    FRowID: Integer;
    FNodes: TNodes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Value: String read FValue write FValue;
    property RowID: Integer read FRowID write FRowID;
    property Nodes: TNodes read FNodes;
  public
    procedure Assign(const ANode: TNode);
  end;
  TNodeList = TObjectList<TNode>;

  TNodes = class(TObject)
  private
    FNodes: TNodeList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TNode;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetCreateNode: TNode;
    function IndexOf(const AValue: String): Integer;
    function IndexOfRowID(const ARowID: Integer): Integer;
    property Items[Index: Integer]: TNode read GetItems;
    property Count: Integer read GetCount;
  end;

  /// <summary>
  /// Древа видно представление данных
  /// </summary>
  /// <remarks>
  /// Child - уровен вложения
  /// </remarks>
  TTree = class(TObject)
  private
    FNodes: TNodes;
    FValues: TValueTreeList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TValueTree;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AField: String; AChild: Integer);
    property Items[Index: Integer]: TValueTree read GetItems;
    property Count: Integer read GetCount;
  public
    property Nodes: TNodes read FNodes;
  end;

(*******************************************************************************
  Фреме для вывода таблицы
*******************************************************************************)

  PVirtualData = ^TVirtualData;
  /// <summary>
  /// Данные организации работы дерева
  /// </summary>
  TVirtualData = record
    Root: Integer;    // Root - индекс
    RowID: Integer;   // Индекс данных
    Index: Integer;   // Индекс строки в узле
    Child: Integer;   // Порядок узле
  end;

  /// <summary>
  /// Для объекта сортировки
  /// </summary>
  TValueSort = record
    Value: Variant;
    RowID: Integer;
  end;
  TValueSortList = TList<TValueSort>;

  TInfo = record
    Columns: TColumns;
    Rows: TRows;
    Tree: TTree;
    Column: Integer;
  public
    constructor Create(AColumns: TColumns; ARows: TRows; ATree: TTree; AColumn: Integer);
  end;

  /// <summary>
  /// Процедура обратного вызова заголовка
  /// </summary>
  TOnCallBackInfoGrid = procedure(const AInfo: TInfo) of object;

  /// <summary>
  /// Процедура обратного вызова
  /// </summary>
  TOnCallBackPaintText = procedure(
    AInfo: TInfo;
    ATargetCanvas: TCanvas;
    AVirtualData: TVirtualData
  ) of object;

  /// <summary>
  /// Процедура обратного вызова для разрешения иконки
  /// </summary>
  TOnCallBackImageIndex = procedure(
    const AInfo: TInfo;
    const AKind: TVTImageKind;
    const AVirtualData: TVirtualData;
    const AImageList: TCustomImageList;
    var AImageIndex: TImageIndex
  ) of object;


  /// <summary>
  /// Изменение объектов
  /// </summary>
  TOnCallBackVirtualData = procedure(
    const AInfo: TInfo;
    const AVirtualData: TVirtualData
  ) of object;


  /// <summary>
  /// Где расположен фремер для вывода табличной информации
  /// </summary>
  TGridFrame = class(TFrame)
    VST: TVirtualStringTree;
    ImageList: TImageList;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTClick(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
  private
    FColumns: TColumns;
    FRows: TRows;
    FTree: TTree;
    function GetRowCount: Integer;
  private
    FOnCallBackPaintText: TOnCallBackPaintText;
    FOnCallBackHeaderAscending: TOnCallBackInfoGrid;
    FOnCallBackHeaderDescending: TOnCallBackInfoGrid;
    FOnCallBackImageIndex: TOnCallBackImageIndex;
    FOnCallBackChange: TOnCallBackVirtualData;
    FOnCallBackClick: TOnCallBackVirtualData;
    procedure LocalCallBackHeaderAscending(const AInfo: TInfo);
    procedure LocalCallBackHeaderDescending(const AInfo: TInfo);
  protected
    procedure SetShowSVT;
    procedure SetSortTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetShow;
    property Tree: TTree read FTree;
    property Columns: TColumns read FColumns;
    property Rows: TRows read FRows;
    property RowCount: Integer read GetRowCount;
  public
    /// <summary>
    /// Оформление работы с текста
    /// </summary>
    property OnCallBackPaintText: TOnCallBackPaintText write FOnCallBackPaintText;
    /// <summary>
    /// Сортировка строки - восходящий
    /// </summary>
    property OnCallBackHeaderAscending: TOnCallBackInfoGrid write FOnCallBackHeaderAscending;
    /// <summary>
    /// Сортировка строки - По убыванию
    /// </summary>
    property OnCallBackHeaderDescending: TOnCallBackInfoGrid write FOnCallBackHeaderDescending;
    /// <summary>
    /// Процедура обратного вызова для разрешения иконки
    /// </summary>
    property OnCallBackImageIndex: TOnCallBackImageIndex write FOnCallBackImageIndex;
    property OnCallBackChange: TOnCallBackVirtualData write FOnCallBackChange;
    property OnCallBackClick: TOnCallBackVirtualData write FOnCallBackClick;
  end;

/// <summary>
/// Функция сравнивает
/// </summary>
function GetSameValueSort(const AValue1, AValue2: TValueSort): Integer;

/// <summary>
/// Процедура сортировки
/// </summary>
procedure SetValueSort(const AValues: TValueSortList);

function GetValue(const AName: String; ADataSet: TDataSet): TValue;


implementation

{$R *.dfm}

uses
{$IFDEF DB_LOG}
  Lb.Logger,
{$ENDIF}
  System.Math;

(*******************************************************************************
  Процедура сортировка
*******************************************************************************)

function GetSameValueSort(const AValue1, AValue2: TValueSort): Integer;

  function GetSameValueText(const AValue1, AValue2: Variant): Integer;
  var
    xS1, xS2: String;
  begin
    xS1 := VarToStrDef(AValue1,'');
    xS2 := VarToStrDef(AValue2,'');
    Result := AnsiCompareText(xS1,xS2);
  end;

  function VarToInt(const Value: Variant): Integer;
  begin
    Result := 0;
    if not VarIsNull(Value) then
      Result := Value;
  end;

  function GetSameValueInt(const AValue1, AValue2: Variant): Integer;
  var
    xV1, xV2: Int64;
  begin
    xV1 := VarToInt(AValue1);
    xV2 := VarToInt(AValue2);
    if xV1 > xV2 then
      Result := 1
    else if xV1 < xV2  then
      Result := -1
    else
      Result := 0;
  end;

  function VarToFloat(const Value: Variant): Double;
  begin
    Result := 0;
    if not VarIsNull(Value) then
      Result := Value;
  end;

  function GetSameValueDouble(const AValue1, AValue2: Variant): Integer;
  var
    xV1, xV2: Double;
  begin
    xV1 := VarToFloat(AValue1);
    xV2 := VarToFloat(AValue2);
    if SameValue(xV1,xV2) then
      Result := 0
    else if xV1 < xV2  then
      Result := -1
    else
      Result := 1;
  end;

  function VarToDate(const Value: Variant): TDateTime;
  begin
    Result := 0;
    if not VarIsNull(Value) then
      Result := Value;
  end;

  function GetSameValueDate(const AValue1, AValue2: Variant): Integer;
  var
    xV1, xV2: Double;
  begin
    xV1 := VarToDate(AValue1);
    xV2 := VarToDate(AValue2);
    if SameValue(xV1,xV2) then
      Result := 0
    else if xV1 < xV2  then
      Result := -1
    else
      Result := 1;
  end;

var
  basicType: Integer;
begin
  // Функция сравнивает
  {todo: Расширеть возможность для других типов данных}
  basicType := VarType(AValue1.Value) and VarTypeMask;

  case basicType of
    //varEmpty     : typeString := 'varEmpty';
    //varNull      : typeString := 'varNull';
    varSmallInt  : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    varInteger   : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    // ---------------------------------------------------------------------
    varSingle    : Result := GetSameValueDouble(AValue1.Value,AValue2.Value);
    varDouble    : Result := GetSameValueDouble(AValue1.Value,AValue2.Value);
    varCurrency  : Result := GetSameValueDouble(AValue1.Value,AValue2.Value);
    // ---------------------------------------------------------------------
    varDate      : Result := GetSameValueDate(AValue1.Value,AValue2.Value);
    // ---------------------------------------------------------------------
    varOleStr    : Result := GetSameValueText(AValue1.Value,AValue2.Value);
    //varDispatch  : typeString := 'varDispatch';
    //varError     : typeString := 'varError';
    varBoolean   : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    // varVariant   : typeString := 'varVariant';
    // varUnknown   : typeString := 'varUnknown';
    varByte      : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    varWord      : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    varLongWord  : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    varInt64     : Result := GetSameValueInt(AValue1.Value,AValue2.Value);
    // varStrArg    : typeString := 'varStrArg';
    varString    : Result := GetSameValueText(AValue1.Value,AValue2.Value);
    // varAny       : typeString := 'varAny';
    // varTypeMask  : typeString := 'varTypeMask';
  else
    Result := GetSameValueText(AValue1.Value,AValue2.Value);
  end;
end;

(*******************************************************************************
  Процедура для сортировки
*******************************************************************************)

procedure SetValueSort(const AValues: TValueSortList);

  function SCompare(AIndex1, AIndex2: Integer): Integer;
  var
    xValue1, xValue2: TValueSort;
  begin
    xValue1 := AValues[AIndex1];
    xValue2 := AValues[AIndex2];
    Result := GetSameValueSort(xValue1,xValue2);
  end;

  procedure ExchangeItems(const AIndex1, AIndex2: Integer);
  var
    xValue1, xValue2, xTmp: TValueSort;
  begin
    xValue1 := AValues[AIndex1];
    xValue2 := AValues[AIndex2];
    xTmp := xValue1;
    xValue1 := xValue2;
    xValue2 := xTmp;
    AValues[AIndex1] := xValue1;
    AValues[AIndex2] := xValue2;
  end;


  procedure QuickSort(L, R: Integer);
  var
    i, j, p: Integer;
  begin
    repeat
      i := L;
      j := R;
      p := (L + R) shr 1;
      repeat
        while SCompare(i,p) < 0 do Inc(i);
        while SCompare(j,p) > 0 do Dec(j);
        if i <= j then
        begin
          if i <> j then
            ExchangeItems(i, j);
          if p = i then
            p := j
          else if p = j then
            p := i;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if L < j then QuickSort(L,j);
      L := i;
    until i >= R;
  end;

var
  L, R: Integer;
begin
  // Процедура сортировки
  L := 0;
  R := AValues.Count - 1;
  QuickSort(L, R);
end;

function GetValue(const AName: String; ADataSet: TDataSet): TValue;
begin
  Result.Name := AName;
  Result.Value := ADataSet.FieldByName(AName).AsVariant;
end;

(*******************************************************************************
  Процедуры обратного вызова для сортировки
*******************************************************************************)

procedure SetSort(const AValues: TValueSortList; AInfo: TInfo);
var
  xValue: TValueSort;
  xNode: TNode;
  xRow: TRow;
  xCol: TColumn;
  i, Count: Integer;
begin
  // Формируем списк - для сортировки
  AValues.Clear;
  Count := AInfo.Tree.Nodes.Count;
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      xNode := AInfo.Tree.Nodes.Items[i];
      xRow :=  AInfo.Rows.Items[xNode.RowID];
      xCol := AInfo.Columns.Items[AInfo.Column];
      xValue.Value := xRow.AsValue[xCol.Field];
      xValue.RowID := xNode.RowID;
      AValues.Add(xValue);
    end;
    SetValueSort(AValues);
  end;
end;

procedure TGridFrame.LocalCallBackHeaderAscending(const AInfo: TInfo);

  function GetNodeToRowID(const ARowID: Integer): TNode;
  var
    xInd: Integer;
  begin
    Result := nil;
    xInd := AInfo.Tree.Nodes.IndexOfRowID(ARowID);
    if xInd >= 0 then
      Result := AInfo.Tree.Nodes.Items[xInd];
  end;

var
  xNode, xNewNode: TNode;
  xNodes: TNodes;
  xValue: TValueSort;
  xValues: TValueSortList;
  i, Count: Integer;
begin
  xValues := TValueSortList.Create;
  try
    SetSort(xValues,AInfo);
    // -------------------------------------------
    Count := xValues.Count;
    if Count > 0 then
    begin
      xNodes := TNodes.Create;
      try
        if AInfo.Tree.Nodes.Count = Count then
        begin
          // --------------------------
          // Старый массив
          for i := 0 to Count - 1 do
          begin
            xValue := xValues.Items[i];
            xNode := GetNodeToRowID(xValue.RowID);
            xNewNode := xNodes.GetCreateNode;
            xNewNode.Assign(xNode);
          end;
          // --------------------------
          // Новый массив
          for i := 0 to Count - 1 do
          begin
            xNewNode := xNodes.Items[i];
            xNode := AInfo.Tree.Nodes.Items[i];
            xNode.Assign(xNewNode);
          end;
        end;
      finally
        FreeAndNil(xNodes);
      end;
    end;
    // --------------------------------------------
  finally
    FreeAndNil(xValues);
  end;
end;

procedure TGridFrame.LocalCallBackHeaderDescending(const AInfo: TInfo);

  function GetNodeToRowID(const ARowID: Integer): TNode;
  var
    xInd: Integer;
  begin
    Result := nil;
    xInd := AInfo.Tree.Nodes.IndexOfRowID(ARowID);
    if xInd >= 0 then
      Result := AInfo.Tree.Nodes.Items[xInd];
  end;

var
  xNode, xNewNode: TNode;
  xNodes: TNodes;
  xValue: TValueSort;
  xValues: TValueSortList;
  i, Count: Integer;
begin
  xValues := TValueSortList.Create;
  try
    SetSort(xValues,AInfo);
    // -------------------------------------------
    Count := xValues.Count;
    if Count > 0 then
    begin
      xNodes := TNodes.Create;
      try
        if AInfo.Tree.Nodes.Count = Count then
        begin
          // --------------------------
          // Старый массив
          for i := Count - 1 downto 0 do
          begin
            xValue := xValues.Items[i];
            xNode := GetNodeToRowID(xValue.RowID);
            xNewNode := xNodes.GetCreateNode;
            xNewNode.Assign(xNode);
          end;
          // --------------------------
          // Новый массив
          for i := 0 to Count - 1 do
          begin
            xNewNode := xNodes.Items[i];
            xNode := AInfo.Tree.Nodes.Items[i];
            xNode.Assign(xNewNode);
          end;
        end;
      finally
        FreeAndNil(xNodes);
      end;
    end;
    // --------------------------------------------
  finally
    FreeAndNil(xValues);
  end;
end;

(******************************************************************************)


{ TValue }

constructor TValue.Create(const AName: String; const AValue: Variant);
begin
  Name := AName;
  Value := AValue;
end;

function TValue.ToString: String;
begin
  Result := Name + ' :: ' + VarToStrDef(Value,'');
end;

{ TRow }

constructor TRow.Create;
begin
  FValues := TValueList.Create;
end;

destructor TRow.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TRow.Clear;
begin
  FValues.Clear;
end;

function TRow.AddValue(const AValue: TValue): Integer;
begin
  Result := FValues.Add(AValue);
end;

function TRow.AddValue(const AName: String; const AValue: Variant): Integer;
begin
  Result := FValues.Add(TValue.Create(AName,AValue));
end;

function TRow.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TRow.GetItems(Index: Integer): TValue;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues[Index];
end;

function TRow.IndexOf(const AName: String): Integer;
var
  xValue: TValue;
  i, Count: Integer;
begin
  Result := -1;
  Count := FValues.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := FValues[i];
      if SameText(xValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TRow.GetAsValue(AField: String): Variant;
var
  xInd: Integer;
  xValue: TValue;
begin
  xInd := IndexOf(AField);
  if xInd >= 0 then
  begin
    xValue := FValues[xInd];
    Result := xValue.Value;
  end;
end;

procedure TRow.SetAsValue(AField: String; const Value: Variant);
var
  xInd: Integer;
  xValue: TValue;
begin
  xInd := IndexOf(AField);
  if xInd >= 0 then
  begin
    xValue := FValues[xInd];
    xValue.Value := Value;
    FValues[xInd] := xValue;
  end;
end;

function TRow.GetAsString(AField: String): String;
begin
  Result := VarToStrDef(Self.AsValue[AField],'');
end;

procedure TRow.SetAsString(AField: String; const Value: String);
begin
  Self.AsValue[AField] := Value;
end;

function TRow.GetAsInteger(AField: String): Integer;
var
  xValue: Variant;
begin
  Result := 0;
  xValue := AsValue[AField];
  if not VarIsNull(xValue) then
    Result := xValue;
end;

procedure TRow.SetAsInteger(AField: String; const Value: Integer);
begin
  Self.AsValue[AField] := Value;
end;


//function TRow.AsInteger(const AField: String): Integer;
//var
//  xValue: Variant;
//begin
//  Result := 0;
//  xValue := AsValue(AField);
//  if not VarIsNull(xValue) then
//    Result := xValue;
//end;
//
//function TRow.AsString(const AField: String): String;
//begin
//  Result := VarToStrDef(AsValue(AField),'');
//end;
//
//function TRow.AsValue(const AField: String): Variant;
//var
//  xInd: Integer;
//  xValue: TValue;
//begin
//  xInd := IndexOf(AField);
//  if xInd >= 0 then
//  begin
//    xValue := FValues[xInd];
//    Result := xValue.Value;
//  end;
//end;

{ TRows }

constructor TRows.Create(const AGridFrame: TGridFrame);
begin
  FGridFrame := AGridFrame;
  FRows := TRowList.Create;
end;

destructor TRows.Destroy;
begin
  FreeAndNil(FRows);
  inherited;
end;

procedure TRows.Clear;
begin
  FRows.Clear;
end;

function TRows.Add: TRow;
var
  xRow: TRow;
begin
  xRow := TRow.Create;
  Result := xRow;
  FRows.Add(xRow);
end;

function TRows.GetCount: Integer;
begin
  Result := FRows.Count;
end;

function TRows.GetItems(Index: Integer): TRow;
begin
  if (Index >= 0) and (Index < FRows.Count) then
    Result := FRows[Index]
  else
    raise Exception.Create('Error Message: [TRows.GetItems] Вышел запределы границы массива');
end;

{ TColumn }

constructor TColumn.Create;
begin
  FVisible := True;
  FTitle := '';
  FField := '';
  FChild := -1;
  FWidth := 0;
end;

destructor TColumn.Destroy;
begin

  inherited;
end;

{ TColumns }

constructor TColumns.Create(const AGridFrame: TGridFrame);
begin
  FGridFrame := AGridFrame;
  if not Assigned(AGridFrame) then
    raise Exception.Create('Error Message: Объект колонки не определен');
  FColumns := TColumnList.Create;
end;

destructor TColumns.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

function TColumns.GetItems(Index: Integer): TColumn;
begin
  if (Index >= 0) and (Index < FColumns.Count) then
    Result := FColumns[Index]
  else
    raise Exception.Create('Error Message: [TColumns.GetItems]. Вышли за предел массива');
end;

procedure TColumns.Clear;
begin
  FColumns.Clear;
  if Assigned(FGridFrame) then
    FGridFrame.VST.Header.Columns.Clear;
end;

procedure TColumns.Add(const ATitle, AFieldName: String; AChild: Integer; AWidth: Integer; AVisible: Boolean);
var
  xColumn: TColumn;
begin
  xColumn := AddColumn;
  // --------------------------------------------------------------------------
  // Создание таблицы
  with xColumn do
  begin
    Title := ATitle;
    Field := AFieldName;
    Child := AChild;
    Width := AWidth;
    Visible := AVisible;
  end;
end;

function TColumns.AddColumn: TColumn;
var
  xColumn: TColumn;
begin
  xColumn := TColumn.Create;
  Result := xColumn;
  FColumns.Add(xColumn);
end;

procedure TColumns.SetShowColumn;
var
  xColumn: TColumn;
  i, Count: Integer;
begin
  Count := FColumns.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xColumn := FColumns[i];
      if Assigned(FGridFrame) then
        with FGridFrame.VST.Header.Columns.Add do
        begin
          Text := xColumn.Title;
          Width := xColumn.Width;
        end;
    end;
end;

{ TValueTree }

constructor TValueTree.Create(const AField: String; const AChild: Integer);
begin
  Field := AField;
  Child := AChild;
end;

{ TNode }

constructor TNode.Create;
begin
  FValue := '';
  FRowID := 0;
  FNodes := TNodes.Create;
end;

destructor TNode.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

procedure TNode.Clear;
begin
  FNodes.Clear;
end;

procedure TNode.Assign(const ANode: TNode);
var
  xNode, xNewNode: TNode;
  i, Count: Integer;
begin
  Self.Clear;
  FValue := ANode.Value;
  FRowID := ANode.RowID;
  Count := ANode.Nodes.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xNode := ANode.Nodes.Items[i];
      xNewNode := FNodes.GetCreateNode;
      xNewNode.Assign(xNode);
    end;
end;

{ TNodes }

constructor TNodes.Create;
begin
  FNodes := TNodeList.Create;
end;

destructor TNodes.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

procedure TNodes.Clear;
begin
  FNodes.Clear;
end;

function TNodes.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

function TNodes.GetCreateNode: TNode;
var
  xNode: TNode;
begin
  xNode := TNode.Create;
  FNodes.Add(xNode);
  Result := xNode;
end;

function TNodes.GetItems(Index: Integer): TNode;
begin
  if (Index >= 0) and (Index < FNodes.Count) then
    Result := FNodes[Index]
  else
    raise Exception.Create('Error Message: [TNodes.GetItems]. Вышли за предел массива');
end;

function TNodes.IndexOf(const AValue: String): Integer;
var
  xNode: TNode;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FNodes.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xNode := FNodes.Items[i];
      if SameText(xNode.Value,AValue) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TNodes.IndexOfRowID(const ARowID: Integer): Integer;
var
  xNode: TNode;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FNodes.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xNode := FNodes.Items[i];
      if xNode.RowID = ARowID then
      begin
        Result := i;
        Break;
      end;
    end;
end;

{ TTree }

constructor TTree.Create;
begin
  FNodes := TNodes.Create;
  FValues := TValueTreeList.Create;
end;

destructor TTree.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FNodes);
  inherited;
end;

procedure TTree.Clear;
begin
  FValues.Clear;
end;

procedure TTree.Add(const AField: String; AChild: Integer);
begin
  FValues.Add(TValueTree.Create(AField,AChild));
end;

function TTree.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TTree.GetItems(Index: Integer): TValueTree;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues[Index]
  else
    raise Exception.Create('Error Message: [TTree.GetItems]. Вышли за предел массива');
end;

{ TInfo }

constructor TInfo.Create(AColumns: TColumns; ARows: TRows; ATree: TTree; AColumn: Integer);
begin
  Columns := AColumns;
  Rows := ARows;
  Tree := ATree;
  Column := AColumn;
end;

{ TGridFrame }

constructor TGridFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VST.NodeDataSize := SizeOf(TVirtualData);
  FColumns := TColumns.Create(Self);
  FRows := TRows.Create(Self);
  FTree := TTree.Create;
  // ---------------------------------------------------
  FOnCallBackPaintText := nil;
  FOnCallBackHeaderAscending := LocalCallBackHeaderAscending;
  FOnCallBackHeaderDescending := LocalCallBackHeaderDescending;
  FOnCallBackImageIndex := nil;
end;

destructor TGridFrame.Destroy;
begin
  FreeAndNil(FTree);
  FreeAndNil(FRows);
  FreeAndNil(FColumns);
  inherited;
end;

function TGridFrame.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

procedure TGridFrame.SetShowSVT;

  procedure SetChildVirtualNode(const ARoot, AChild: Integer; const AVirtualNode: PVirtualNode; const ANode: TNode);
  var
    xNode: TNode;
    xVirtualNode: PVirtualNode;
    xVirtualData: PVirtualData;
    i, iCount: Integer;
  begin
    iCount := ANode.Nodes.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xNode := ANode.Nodes.Items[i];
        xVirtualNode := VST.AddChild(AVirtualNode);
        xVirtualData := VST.GetNodeData(xVirtualNode);
        with xVirtualData^ do
        begin
          Root := ARoot;
          RowID := xNode.RowID;
          Index := i;
          Child := AChild + 1;
        end;
        SetChildVirtualNode(ARoot,xVirtualData^.Child,xVirtualNode,xNode);
      end;
  end;

var
  i, iCount: Integer;
  j, jCount: Integer;
  xNode: TNode;
  xVirtualNode: PVirtualNode;
  xVirtualData: PVirtualData;
begin
  iCount := FTree.Nodes.Count;
  if iCount > 0 then
  begin
    VST.BeginUpdate;
    try
      VST.Clear;
      for i := 0 to iCount - 1 do
      begin
        xVirtualNode := VST.AddChild(nil);
        //-------------------------------------------
        xNode := FTree.Nodes.Items[i];
        xVirtualData := VST.GetNodeData(xVirtualNode);
        with xVirtualData^ do
        begin
          Root := i;
          RowID := xNode.RowID;
          Index := i;
          Child := 0;
        end;
        // ------------------------------------------

        SetChildVirtualNode(i,0,xVirtualNode,xNode);
        VST.Selected[xVirtualNode];
      end;
    finally
      VST.EndUpdate;
    end;
  end;
end;

procedure TGridFrame.SetShow;
begin
  SetSortTree;
  SetShowSVT;
end;

procedure TGridFrame.SetSortTree;

  function GetNode(const AValue: String; const ARowID: Integer; const ANodes: TNodes): TNode;
  var
    xInd: Integer;
    xNode: TNode;
  begin
    xInd := ANodes.IndexOf(AValue);
    if xInd < 0 then
    begin
      xNode := ANodes.GetCreateNode;
      xNode.Value := AValue;
      xNode.RowID := ARowID;
    end
    else
      xNode := ANodes.Items[xInd];
    Result := xNode;
  end;

  procedure SetRowTree(const ARowID: Integer; const ARow: TRow);
  var
    xS: String;
    xValueTree: TValueTree;
    i, iCount: Integer;
    xNode: TNode;
  begin
    xNode := nil;
    iCount := FTree.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xValueTree := FTree.Items[i];
        xS := ARow.AsString[xValueTree.Field];
        if i = 0 then
        begin
          xNode := GetNode(xS,ARowID,FTree.Nodes)
        end
        else
        begin
          if Assigned(xNode) then
            xNode := GetNode(xS,ARowID,xNode.Nodes)
          else
            Break;
        end;
      end;
    end
    else
    begin
      xNode := FTree.Nodes.GetCreateNode;
      xNode.RowID := ARowID;
    end;
  end;

var
  xRow: TRow;
  i, iCount: Integer;
begin
  iCount := FRows.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xRow := FRows.Items[i];
      SetRowTree(i,xRow);
    end;
end;

procedure TGridFrame.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  xVirtualData: PVirtualData;
begin
  if Assigned(Node) then
  begin
    xVirtualData := VST.GetNodeData(Node);
    if Odd(xVirtualData^.Root) then
    begin
      TargetCanvas.Brush.Color := $00CACACA;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TGridFrame.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  xVirtualData: PVirtualData;
begin
  if Assigned(Node) then
  begin
    xVirtualData := VST.GetNodeData(Node);
    if Assigned(FOnCallBackImageIndex) then
    begin
      FOnCallBackImageIndex(
        TInfo.Create(FColumns,FRows,FTree,Column),
        Kind,
        xVirtualData^,
        nil,
        ImageIndex
      );
    end;
  end;
end;

procedure TGridFrame.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

  function GetColumn(AVirtualData: TVirtualData): String;
  var
    xCol: TColumn;
    xInd: Integer;
    xRow: TRow;
  begin
    Result := '';
    xRow := FRows.Items[AVirtualData.RowID];
    xCol := FColumns.Items[Column];
    xInd := xRow.IndexOf(xCol.Field);
    if AVirtualData.Child = xCol.Child then
    begin
      if xCol.Visible then
      begin
        if xInd >= 0 then
          Result := VarToStrDef(xRow.Items[xInd].Value,'')
        else
          if SameText('$inc',xCol.Field) then
            Result := IntToStr(AVirtualData.Index + 1);
      end
      else
        Result := '';
    end;
  end;

var
  xVirtualData: PVirtualData;
begin
  if Column >= 0 then
  begin
    if Assigned(Node) then
    begin
      xVirtualData := VST.GetNodeData(Node);
      CellText := GetColumn(xVirtualData^);
    end;
  end;
end;

procedure TGridFrame.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xVirtualData: PVirtualData;
begin
  if Assigned(Node) then
  begin
     xVirtualData := VST.GetNodeData(Node);
    if Assigned(FOnCallBackPaintText) then
      FOnCallBackPaintText(Tinfo.Create(FColumns,FRows,FTree,Column),TargetCanvas,xVirtualData^);
  end;
end;

procedure TGridFrame.VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  // Выбор сортировки по одному полю
  Sender.SortColumn := HitInfo.Column;
  case Sender.SortDirection of
    sdAscending: begin
      Sender.SortDirection := sdDescending;
      if Assigned(FOnCallBackHeaderAscending) then
      begin
        FOnCallBackHeaderAscending(TInfo.Create(FColumns,FRows,FTree,HitInfo.Column));
        Self.SetShowSVT;
      end;
    end;
    sdDescending: begin
      Sender.SortDirection := sdAscending;
      if Assigned(FOnCallBackHeaderDescending) then
      begin
        FOnCallBackHeaderDescending(TInfo.Create(FColumns,FRows,FTree,HitInfo.Column));
        Self.SetShowSVT;
      end;
    end;
  end;
end;

procedure TGridFrame.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  xVirtualData: PVirtualData;
begin
  if Assigned(Node) then
  begin
    xVirtualData := VST.GetNodeData(Node);
    if Assigned(FOnCallBackChange) then
    begin
      FOnCallBackChange(
        TInfo.Create(FColumns,FRows,FTree,-1),
        xVirtualData^
      );
    end;
  end;
end;

procedure TGridFrame.VSTClick(Sender: TObject);
var
  xNode: PVirtualNode;
  xVirtualData: PVirtualData;
begin
  xNode := VST.FocusedNode;
  if Assigned(xNode) then
  begin
    xVirtualData := VST.GetNodeData(xNode);
    if Assigned(FOnCallBackClick) then
    begin
      FOnCallBackClick(
        TInfo.Create(FColumns,FRows,FTree,-1),
        xVirtualData^
      );
    end;
  end;
end;

procedure TGridFrame.Clear;
begin
  VST.Clear;
  FRows.Clear;
  FTree.Nodes.Clear;
end;


end.
