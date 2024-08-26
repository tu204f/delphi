unit Lb.Status.Quik;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Status,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable,
  QuikTransOrder;

type
  ///<summary>
  /// Отслеживание состояние рынка — торговый терминал Quik
  ///</summary>
  TQuikStatus = class(TCustomStatus)
  private
    FRSIQuikTable: TQuikTable;
    FSecurityTable: TQuikTable;
    FQtyTable: TQuikTable;
  protected
    procedure DoStart; override;
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetOperationTrade(ASide: TQBTypeSide;
      APrice: Double; AQty: Double; ALine: TTypeLine): String; override;
  end;

implementation

uses
  System.Math,
  Lb.Logger;

{ TQuikStatus }

constructor TQuikStatus.Create;
begin
  inherited;

end;

destructor TQuikStatus.Destroy;
begin

  inherited;
end;

procedure TQuikStatus.DoStart;
begin
  inherited DoStart;
  FRSIQuikTable  := QuikManagerTable.Tables.GetTableName(ParamApplication.QuikTableRSI);
  FSecurityTable := QuikManagerTable.Tables.GetTableName('security');
  FQtyTable      := QuikManagerTable.Tables.GetTableName('qty');
end;

procedure TQuikStatus.DoSelected;
begin
  MinStep := 0;

  FRSIQuikTable.Fisrt;
  FastRSI := RoundTo(FRSIQuikTable.IndexName[8].AsDouble,-2);
  SlowRSI := RoundTo(FRSIQuikTable.IndexName[9].AsDouble,-2);

  if GetSecCodeToTable(ParamApplication.SecCode,'CODE',FSecurityTable) then
  begin
    Bid := FSecurityTable.ByName['BID'].AsDouble;
    Ask := FSecurityTable.ByName['OFFER'].AsDouble;
    MinStep := FSecurityTable.ByName['SEC_PRICE_STEP'].AsDouble;
  end;

  // Включена вертуализация сделки
  if not ParamApplication.IsVirtualChecked then
  begin
    // Размер позиции
    if GetSecCodeToTable(ParamApplication.SecCode,'SECCODE',FQtyTable) then
      Qty := FQtyTable.ByName['TOTAL_NET'].AsDouble;

    if Qty > 0 then
      Side := TQBTypeSide.tsBuy
    else if Qty < 0 then
      Side := TQBTypeSide.tsSell;
  end;

  DoParams;
  inherited DoSelected;
  DoUpDate;
end;

function TQuikStatus.GetOperationTrade(ASide: TQBTypeSide; APrice, AQty: Double; ALine: TTypeLine): String;

  function _BuySellToSide(const ASide: TQBTypeSide): Char;
  begin
    case ASide of
      TQBTypeSide.tsBuy: Result := 'B';
      TQBTypeSide.tsSell: Result := 'S';
    else
      Result := #0;
    end;
  end;

  function _MktPrice(const ABuySell: Char; const APrice: Double): Double;
  begin
    case ABuySell of
      'B': Result := APrice + 100 * MinStep;
      'S': Result := APrice - 100 * MinStep;
    else
      Result := APrice;
    end;
  end;

var
  xMsg: String;
  xBuySell: Char;
  xSyncOrder: TCustomSyncOrder;
begin
  Result := inherited GetOperationTrade(ASide, APrice, AQty, ALine);
  if not ParamApplication.IsVirtualChecked then
  begin
    GetConnectQUIK(ParamApplication.PathQuik);
    xBuySell := _BuySellToSide(ASide);
    if CharInSet(xBuySell,['B','S']) then
    begin
      xSyncOrder := TCustomSyncOrder.Create;
      try
        xSyncOrder.SecCode   := ParamApplication.SecCode;
        xSyncOrder.ClassCode := ParamApplication.ClassCode;
        xSyncOrder.TrdaccID  := ParamApplication.TrdaccID;
        xSyncOrder.GetNewOrder(
          _MktPrice(xBuySell,APrice),
          Trunc(AQty),
          xBuySell,
          xMsg
        );
        DoInfoMsg(xMsg);
      finally
        FreeAndNil(xSyncOrder);
      end;
    end;
  end;

end;

end.
