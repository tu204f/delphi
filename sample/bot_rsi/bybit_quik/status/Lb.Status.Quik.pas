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
  /// ������������ ��������� ����� � �������� �������� Quik
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
  FRSIQuikTable  := QuikManagerTable.Tables.GetTableName(ParamApplication.QuikTableRSI);
  FSecurityTable := QuikManagerTable.Tables.GetTableName('security');
  FQtyTable      := QuikManagerTable.Tables.GetTableName('qty');
end;

procedure TQuikStatus.DoSelected;
var
  xCurrentTime: TDateTime;
begin
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


  if GetSecCodeToTable(ParamApplication.SecCode,'CODE',FSecurityTable) then
  begin
    Bid := FSecurityTable.ByName['BID'].AsDouble;
    Ask := FSecurityTable.ByName['OFFER'].AsDouble;
    MinStep := FSecurityTable.ByName['SEC_PRICE_STEP'].AsDouble;
  end;

  // �������� ������������� ������
  if not ParamApplication.IsVirtualChecked then
  begin
    // ������ �������
    if GetSecCodeToTable(ParamApplication.SecCode,'SECCODE',FQtyTable) then
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
  if Date > (StrToDate('01.09.2024') + 30)  then
    Exit;

  Result := inherited GetOperationTrade(AParamStatus);
  if not ParamApplication.IsVirtualChecked then
  begin
    GetConnectQUIK(ParamApplication.PathQuik);
    xBuySell := _BuySellToSide(AParamStatus.Side);
    if CharInSet(xBuySell,['B','S']) then
    begin
      xSyncOrder := TCustomSyncOrder.Create;
      try
        xSyncOrder.SecCode   := ParamApplication.SecCode;
        xSyncOrder.ClassCode := ParamApplication.ClassCode;
        xSyncOrder.TrdaccID  := ParamApplication.TrdaccID;
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
