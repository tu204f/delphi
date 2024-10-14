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
    FCurrentTime: TDateTime;
    FRSIQuikTable: TQuikTable;
    FSecurityTable: TQuikTable;
    FQtyTable: TQuikTable;
  protected
    procedure DoStart; override;
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetOperationTrade(AParamStatus: TParamStatus): String; override;
  end;

implementation

uses
  System.Math,
  Lb.Logger;

{ TQuikStatus }

constructor TQuikStatus.Create;
begin
  inherited;
  FTypePlatform := TTypePlatform.tpQuik;
end;

destructor TQuikStatus.Destroy;
begin

  inherited;
end;

procedure TQuikStatus.DoStart;
begin
  inherited DoStart;
  FCurrentTime := 0;

  FRSIQuikTable := nil;
  FSecurityTable := nil;
  FQtyTable := nil;

  with QuikManagerTable.Tables do
  begin
    if IsTableName(ParamPlatform.QuikTableRSI) then
      FRSIQuikTable  := GetTableName(ParamPlatform.QuikTableRSI);
    if IsTableName('security') then
      FSecurityTable := GetTableName('security');
    if IsTableName('qty') then
      FQtyTable := QuikManagerTable.Tables.GetTableName('qty');
  end;
end;

procedure TQuikStatus.DoSelected;
var
  xCurrentTime: TDateTime;
begin
  if not Assigned(FRSIQuikTable) then
  begin
    Stop;
    Exit;
  end;

  MinStep := 0;
  IsNewCandel := False;

  FRSIQuikTable.Fisrt;
  FastRSI := RoundTo(FRSIQuikTable.IndexName[8].AsDouble,-2);
  SlowRSI := RoundTo(FRSIQuikTable.IndexName[9].AsDouble,-2);
  xCurrentTime := FRSIQuikTable.ByName['Time'].AsTime;
  if FCurrentTime = 0 then
  begin
    FCurrentTime := xCurrentTime;
  end
  else if FCurrentTime <> xCurrentTime then
  begin
    FCurrentTime := xCurrentTime;
    DoNewCandel;
  end;


  if GetSecCodeToTable(ParamPlatform.SecCode,'CODE',FSecurityTable) then
  begin
    Bid := FSecurityTable.ByName['BID'].AsDouble;
    Ask := FSecurityTable.ByName['OFFER'].AsDouble;
    MinStep := FSecurityTable.ByName['SEC_PRICE_STEP'].AsDouble;
  end;

  // Включена вертуализация сделки
  if not ParamPlatform.IsVirtualChecked then
  begin
    // Размер позиции
    if GetSecCodeToTable(ParamPlatform.SecCode,'SECCODE',FQtyTable) then
      Position.Qty := FQtyTable.ByName['TOTAL_NET'].AsDouble;

    if Position.Qty > 0 then
      Position.Side := TQBTypeSide.tsBuy
    else if Position.Qty < 0 then
    begin
      Position.Side := TQBTypeSide.tsSell;
      Position.Qty  := -1 * Position.Qty;
    end;
  end;

  DoParams;
  inherited DoSelected;
  DoUpDate;
end;

function TQuikStatus.GetOperationTrade(AParamStatus: TParamStatus): String;

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
  Result := inherited GetOperationTrade(AParamStatus);
  if not ParamPlatform.IsVirtualChecked then
  begin
    GetConnectQUIK(ParamPlatform.PathQuik);
    xBuySell := _BuySellToSide(AParamStatus.Side);
    if CharInSet(xBuySell,['B','S']) then
    begin
      xSyncOrder := TCustomSyncOrder.Create;
      try
        xSyncOrder.SecCode   := ParamPlatform.SecCode;
        xSyncOrder.ClassCode := ParamPlatform.ClassCode;
        xSyncOrder.TrdaccID  := ParamPlatform.TrdaccID;
        xSyncOrder.GetNewOrder(
          _MktPrice(xBuySell,AParamStatus.Price),
          Trunc(AParamStatus.Qty),
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
