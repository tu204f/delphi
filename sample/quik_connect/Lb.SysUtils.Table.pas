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
  TTabSecurity = class(TQuikTable)
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

  // дописываем поля - по надобности

  ///<summary>Таблица сделок</summary>
  TTabTrades = class(TQuikTable)
  private
  public
  end;

  ///<summary>Таблица заявок</summary>
  TTabOrders= class(TQuikTable)
  private
  public
  end;

  ///<summary>Стакан</summary>
  TTabBook = class(TQuikTable)
  private
  public
  end;

var
  Security: TTabSecurity = nil;
  Trades: TQuikTable = nil;
  Orders: TQuikTable = nil;

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
  Security := TTabSecurity(GetQuikTableByName('security'));
  Trades := GetQuikTableByName('orders');
  Orders := GetQuikTableByName('trades');
  //SourceCandel := TSourceCandel(GetQuikTableByName('source_code_1'));
  //SourceCode2 := GetQuikTableByName('source_code_2');
end;

function GetIsQuikTable: Boolean;
begin
  Result :=
    Assigned(Security) and
    Assigned(Trades) and
    Assigned(Orders);
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

{ TTabSecurity }

function TTabSecurity.GetCellByNameAndCode(const ANameField, ASecCode: String): TCell;
begin
  Self.SetRowSecCode(ASecCode);
  Result := Self.ByName[ANameField];
end;

procedure TTabSecurity.SetRowSecCode(const ASecCode: String);
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

function TTabSecurity.GetLast: Double;
begin
  Result := Self.ByName['LAST'].AsDouble;
end;

function TTabSecurity.GetBid: Double;
begin
  Result := Self.ByName['BID'].AsDouble;
end;

function TTabSecurity.GetOffer: Double;
begin
  Result := Self.ByName['OFFER'].AsDouble;
end;

function TTabSecurity.GetMinStep: Double;
begin
  Result := Self.ByName['SEC_PRICE_STEP'].AsDouble;
end;

initialization

finalization

end.
