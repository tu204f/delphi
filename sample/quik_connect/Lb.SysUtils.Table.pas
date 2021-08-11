unit Lb.SysUtils.Table;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Quik.ValueTable,
  Quik.Manager.DDE;

type
  ///<summary>Таблица Финансовых инструментов</summary>
  TSecurity = class(TQuikTable)
  private
    function GetLast: Double;
    function GetCellByNameAndCode(const ANameField, ASecCode: String): TCell;
    function GetBid: Double;
    function GetMinStep: Double;
    function GetOffer: Double;
  public
    procedure SetRowSecCode(const ASecCode: String);
    property CellByNameAndCode[const ANameField: String; const ASecCode: String]: TCell read GetCellByNameAndCode;
  public
    property Last: Double read GetLast;
    property Bid: Double read GetBid;
    property Offer: Double read GetOffer;
    property MinStep: Double read GetMinStep;
  end;

  {$DEFINE SOURCE_MA}
  ///<summary>Источник данных</summary>
  TSourceCandel = class(TQuikTable)
  private
    function GetDate: TDateTime;
    function GetTime: TDateTime;
    function GetClose: Double;
    function GetHigh: Double;
    function GetLow: Double;
    function GetOpen: Double;
  {$IFDEF SOURCE_MA}
  private
    function GetMovingAverage: Double;
  {$ENDIF}
  public
    procedure SetLast(const ACount: Integer = 0);
    property Date: TDateTime read GetDate;
    property Time: TDateTime read GetTime;
    property Open: Double read GetOpen;
    property High: Double read GetHigh;
    property Low: Double read GetLow;
    property Close: Double read GetClose;
  {$IFDEF SOURCE_MA}
  public
    property MovingAverage: Double read GetMovingAverage;
  {$ENDIF}
  end;

var
  Security: TSecurity = nil;
  Trades: TQuikTable = nil;
  Orders: TQuikTable = nil;
  SourceCandel: TSourceCandel = nil;

procedure SetInitializationTable;
function GetIsQuikTable: Boolean;

implementation

function GetQuikTableByName(const ANameTable: String): TQuikTable;
begin
  Result := nil;
  var xInd := QuikManagerTable.IndexOfTable(ANameTable);
  if xInd >= 0 then
    Result := QuikManagerTable.Tables[xInd];
end;

procedure SetInitializationTable;
begin
  Security := TSecurity(GetQuikTableByName('security'));
  Trades := GetQuikTableByName('orders');
  Orders := GetQuikTableByName('trades');
  SourceCandel := TSourceCandel(GetQuikTableByName('source_code_1'));
  //SourceCode2 := GetQuikTableByName('source_code_2');
end;

function GetIsQuikTable: Boolean;
begin
  Result :=
    Assigned(Security) and
    Assigned(Trades) and
    Assigned(Orders) and
    Assigned(SourceCandel);
    //and Assigned(SourceCode2);
end;

procedure SetRowSecCode(const ATable: TQuikTable; const AClassCode, ASecCode: String);
var
  xClassCode, xSecCode: String;
begin
  ATable.Fisrt;
  while not ATable.EOF do
  begin
    xClassCode := ATable.AsString('CLASS_CODE');
    xSecCode := ATable.AsString('CODE');
    if SameText(AClassCode,xClassCode) and SameText(ASecCode,xSecCode) then
      Break;
    ATable.Next;
  end;
end;

{ TSecurity }

function TSecurity.GetCellByNameAndCode(const ANameField, ASecCode: String): TCell;
begin
  Self.SetRowSecCode(ASecCode);
  Result := Self.ByName[ANameField];
end;

procedure TSecurity.SetRowSecCode(const ASecCode: String);
begin
  {todo: Перебираем - все строки кода, другой способ поиска}
  Self.Fisrt;
  while not Self.EOF do
  begin
    var xSecCode := Self.AsString('CODE');
    if SameText(ASecCode,xSecCode) then
      Break;
    Self.Next;
  end;
end;

function TSecurity.GetLast: Double;
begin
  Result := Self.ByName['LAST'].AsDouble;
end;

function TSecurity.GetBid: Double;
begin
  Result := Self.ByName['BID'].AsDouble;
end;

function TSecurity.GetOffer: Double;
begin
  Result := Self.ByName['OFFER'].AsDouble;
end;

function TSecurity.GetMinStep: Double;
begin
  Result := Self.ByName['SEC_PRICE_STEP'].AsDouble;
end;

{ TSourceCandel }

procedure TSourceCandel.SetLast(const ACount: Integer);
begin
  if Self.Count > 0 then
    Self.RowID := (Self.Count - 1) - ACount
  else
    Self.Last;
end;

function TSourceCandel.GetDate: TDateTime;
begin
  Result := Self.ByName['DATE'].AsDate;
end;

function TSourceCandel.GetTime: TDateTime;
begin
  Result := Self.ByName['TIME'].AsTime;
end;

function TSourceCandel.GetOpen: Double;
begin
  Result := Self.ByName['OPEN'].AsDouble;
end;

function TSourceCandel.GetHigh: Double;
begin
  Result := Self.ByName['HIGH'].AsDouble;
end;

function TSourceCandel.GetLow: Double;
begin
  Result := Self.ByName['LOW'].AsDouble;
end;

function TSourceCandel.GetClose: Double;
begin
  Result := Self.ByName['CLOSE'].AsDouble;
end;

{$IFDEF SOURCE_MA}
function TSourceCandel.GetMovingAverage: Double;
begin
  Result := Self.ByName['MA'].AsDouble;
end;
{$ENDIF}

initialization

finalization

end.
