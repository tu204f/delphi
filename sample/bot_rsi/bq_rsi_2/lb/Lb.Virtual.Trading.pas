unit Lb.Virtual.Trading;

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
  TVirtualTrading = class(TObject)
  public type
    ///<summary>Сделка</summary>
    TTrade = record
      Time: Int64;
      Price: Double;
      Qty: Double;
      Side: TTypeSide;
    end;
    TTradeList = TList<TTrade>;

    TPosition = class(TObject)
    private
      FSide: TTypeSide;
      FPrice: Double;
      FQty: Double;
      FValue: Double;
      FTrades: TTradeList;
      FVirtualTrading: TVirtualTrading;
    protected
      procedure ExecuteTrade;
    public
      constructor Create(const AVirtualTrading: TVirtualTrading); virtual;
      destructor Destroy; override;
      procedure AddTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeSide);
      property Trades: TTradeList read FTrades;
      property Side: TTypeSide read FSide;
      property Price: Double read FPrice;
      property Qty: Double read FQty;
      property Value: Double read FValue;
    end;
    TPositionList = TObjectList<TPosition>;

  private
    FPosition: TPosition;
    FPositions: TPositionList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeSide); overload;
    procedure AddTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeSide); overload;
    property Positions: TPositionList read FPositions;
  end;

implementation

{ TVirtualTrading.TPosition }

constructor TVirtualTrading.TPosition.Create(const AVirtualTrading: TVirtualTrading);
begin
  FVirtualTrading := AVirtualTrading;
  FTrades := TTradeList.Create;
end;

destructor TVirtualTrading.TPosition.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TVirtualTrading.TPosition.ExecuteTrade;
var
  xTrade: TVirtualTrading.TTrade;
begin
  FPrice := 0;
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
      FPrice := FValue/FQty
    else
      FPrice := 0;
  end;
end;

procedure TVirtualTrading.TPosition.AddTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeSide);
var
  xQty: Double;
  xTrade: TVirtualTrading.TTrade;
begin
  if FTrades.Count = 0 then
  begin
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
        if xQty >= 0 then
          FTrades.Delete(0);
      end;
      if xQty < 0 then
      begin
        FVirtualTrading.AddTrade(
          ATime,
          APrice,
          Abs(xQty),
          ASide
        );
      end;
    end;
  end;
  ExecuteTrade;
end;

{ TVirtualTrading }

constructor TVirtualTrading.Create;
begin
  FPosition := nil;
  FPositions := TPositionList.Create;
end;

destructor TVirtualTrading.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

procedure TVirtualTrading.AddTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeSide);
begin
  if Assigned(FPosition) then
  begin
    FPosition := TPosition.Create(Self);
    FPositions.Add(FPosition);
    FPosition.AddTrade(ATime,APrice,AQty,ASide);
  end
  else
  begin
    if FPosition.FTrades.Count = 0 then
    begin
      FPosition := TPosition.Create(Self);
      FPositions.Add(FPosition);
      FPosition.AddTrade(ATime,APrice,AQty,ASide);
    end
    else
      FPosition.AddTrade(ATime,APrice,AQty,ASide);
  end;
end;

procedure TVirtualTrading.AddTrade(const ATime: TDateTime; const APrice,
  AQty: Double; ASide: TTypeSide);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := (xHour * 3600 + xMin * 60 * xSec) * xMSec;
  AddTrade(xTime,APrice,AQty,ASide);
end;

end.
