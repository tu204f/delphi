(******************************************************************************)
(* Фиксация подтвержденных сделок                                             *)
(******************************************************************************)
unit Lb.Platform.Trading;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  ///<summary>Виртуальная торговля</summary>
  TPlatformTrading = class(TObject)
  public type
    ///<summary>Сделка</summary>
    TTrade = record
      Time: Int64;
      Price: Double;
      Qty: Double;
      Side: TTypeBuySell;
    end;
    TTradeList = TList<TTrade>;

    TPosition = class(TObject)
    private
      FOpenTime: Int64;
      FCloseTime: Int64;
      FSide: TTypeBuySell;
      FOpenPrice: Double;
      FClosePrice: Double;
      FMovingPrice: Double;
      FQty: Double;
      FValue: Double;
      FTrades: TTradeList;
      FPlatformTrading: TPlatformTrading;
    private
      FHistory: TTradeList;
    protected
      procedure ExecuteTrade;
    public
      constructor Create(const APlatformTrading: TPlatformTrading); virtual;
      destructor Destroy; override;
      procedure OpenTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeBuySell);
      function GetProfit(const APrice: Double = 0): Double;
      function ToString: string; override;
      property History: TTradeList read FHistory;
    public
      property OpenTime: Int64 read FOpenTime;
      property CloseTime: Int64 read FCloseTime;
      property Side: TTypeBuySell read FSide;
      property OpenPrice: Double read FOpenPrice;
      property ClosePrice: Double read FClosePrice;
      property MovingPrice: Double read FMovingPrice;
      property Qty: Double read FQty;
      property Value: Double read FValue;
    end;
    TPositionList = TObjectList<TPosition>;

  private
    FPosition: TPosition;
    FPositions: TPositionList;
    function GetIsPosition: Boolean;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure OpenTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeBuySell); overload;
    procedure OpenTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell); overload;
    procedure CloseTrade(const ATime: Int64; const APrice: Double);
    property Positions: TPositionList read FPositions;
    property CurrentPosition: TPosition read FPosition;
  end;

implementation

{ TPlatformTrading.TPosition }

constructor TPlatformTrading.TPosition.Create(const APlatformTrading: TPlatformTrading);
begin
  FPlatformTrading := APlatformTrading;
  FTrades := TTradeList.Create;
  FHistory:= TTradeList.Create;
end;

destructor TPlatformTrading.TPosition.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FTrades);
  inherited;
end;

procedure TPlatformTrading.TPosition.ExecuteTrade;
var
  xTrade: TPlatformTrading.TTrade;
begin
  FMovingPrice := 0;
  FQty := 0;
  FValue := 0;
  if FTrades.Count > 0 then
  begin
    for var i := 0 to FTrades.Count - 1 do
    begin
      xTrade := FTrades[i];
      FQty := FQty + xTrade.Qty;
      FValue := FValue + xTrade.Price * xTrade.Qty;
    end;
    if FQty = 0 then
      FMovingPrice := FValue/FQty
    else
      FMovingPrice := 0;
  end;
end;

function TPlatformTrading.TPosition.GetProfit(const APrice: Double): Double;

  function _GetProfit: Double;
  var
    xValue: Double;
    xTrade: TPlatformTrading.TTrade;
    i, iCount: Integer;
  begin
    xValue := 0;
    iCount := FHistory.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTrade := FHistory[i];
        xValue := xValue + xTrade.Price * xTrade.Qty;
      end;
    Result := xValue;
  end;

begin
  if FQty = 0 then
    Result := _GetProfit
  else
  begin
    if APrice = 0 then
       raise Exception.Create('Error Message: Неуказаная цена');

    case FSide of
      tsBuy : Result := APrice * FQty - _GetProfit;
      tsSell: Result := _GetProfit + APrice * FQty;
    end;
  end;
end;

procedure TPlatformTrading.TPosition.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xQty: Double;
  xTrade: TPlatformTrading.TTrade;
  xHistory: TPlatformTrading.TTrade;
begin
  xHistory.Time := ATime;
  xHistory.Price := APrice;
  xHistory.Qty := AQty;
  xHistory.Side := ASide;
  FHistory.Add(xHistory);

  if FTrades.Count = 0 then
  begin
    FOpenTime := ATime;
    FCloseTime := 0;
    FOpenPrice := APrice;
    FClosePrice := 0;
    FMovingPrice := 0;
    xTrade.Time := ATime;
    xTrade.Price := APrice;
    xTrade.Qty := AQty;
    xTrade.Side := ASide;
    FTrades.Add(xTrade);
    FSide := ASide;
  end
  else
  begin
    if ASide = FSide then
    begin
      xTrade.Time := ATime;
      xTrade.Price := APrice;
      xTrade.Qty := AQty;
      xTrade.Side := ASide;
      FTrades.Add(xTrade);
    end
    else
    begin
      xQty := AQty;
      xTrade := FTrades[0];
      while xQty > 0 do
      begin
        xQty := xQty - xTrade.Qty;
        if xQty > 0 then
        begin
          FTrades.Delete(0);
          if FTrades.Count > 0 then
            xTrade := FTrades[0]
          else
            Break;
        end
        else
        begin
          xTrade.Qty := -1 * xQty;
          FTrades[0] := xTrade;
          Break;
        end;
      end;


      if xQty > 0 then
      begin

        var xIndex := FHistory.Count - 1;
        xHistory := FHistory[xIndex];
        xHistory.Qty := xTrade.Qty;
        FHistory[xIndex] := xHistory;

        FPlatformTrading.FPosition := nil;
        FPlatformTrading.OpenTrade(
          ATime,
          APrice,
          xQty,
          ASide
        );

        FCloseTime := ATime;
        FClosePrice := APrice;
      end;


    end;
  end;
  ExecuteTrade;
end;

function TPlatformTrading.TPosition.ToString: string;
begin
  var xS := '';
  xS :=
    OpenTime.ToString + ';' +
    CloseTime.ToString + ';' +
    'BuySell: ' + GetStrToSide(Side) + ';' +
    OpenPrice.ToString + ';' +
    ClosePrice.ToString + ';' +
    MovingPrice.ToString + ';' +
    'Qty: ' + Qty.ToString + ';' +
    Value.ToString + ';';
  Result := xS;
end;

{ TPlatformTrading }

constructor TPlatformTrading.Create;
begin
  FPosition := nil;
  FPositions := TPositionList.Create;
end;

destructor TPlatformTrading.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

function TPlatformTrading.GetIsPosition: Boolean;
begin

end;

procedure TPlatformTrading.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
begin
  if not Assigned(FPosition) then
  begin
    FPosition := TPosition.Create(Self);
    FPositions.Add(FPosition);
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
  end
  else
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
end;

procedure TPlatformTrading.OpenTrade(const ATime: TDateTime; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  OpenTrade(xTime,APrice,AQty,ASide);
end;

procedure TPlatformTrading.CloseTrade(const ATime: Int64; const APrice: Double);
begin
  if Assigned(FPosition) then
  begin
    if FPosition.Qty > 0 then
      FPosition.OpenTrade(
        ATime,
        APrice,
        FPosition.Qty,
        GetCrossSide(FPosition.Side)
      );
    FPosition := nil;
  end;
end;

end.
