(******************************************************************************)
(* Фиксация торговой операции независимо от торговой платформы                *)
(* Журнал торговых операция                                                   *)
(******************************************************************************)
unit Lb.Journal.Trading;

interface

{$I debug.inc}

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
  TJournalTrading = class;

  ///<summary>Напров</summary>
  TInfoPositionTrading = record
    Profit: Double;
    Price: Double;
    Qty: Double;
    Count: Integer;
    Side: TTypeBuySell;
  end;

  ///<summary>Сделка</summary>
  TJournalTrade = record
    Time: Int64;          // Время
    Price: Double;        // Цена
    Qty: Double;          // Количество
    Side: TTypeBuySell;   // Напровление сделки
  public
    function ToString: string;
  end;
  TJournalTradeList = TList<TJournalTrade>;

  ///<summary>Позиция</summary>
  TJournalPosition = class(TObject)
  private
    FOpenTime: Int64;
    FCloseTime: Int64;
    FSide: TTypeBuySell;
    FOpenPrice: Double;
    FClosePrice: Double;
    FMovingPrice: Double;
    FQty: Double;
    FValue: Double;
    FTrades: TJournalTradeList;
    FBufferTrading: TJournalTrading;
  private
    FProfit: Double;
    FMinProfit: Double;
    FMaxProfit: Double;
  private
    FHistory: TJournalTradeList;
  protected
    procedure ExecuteTrade;
  public
    constructor Create(const ABufferTrading: TJournalTrading); virtual;
    destructor Destroy; override;
    procedure OpenTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeBuySell);

    procedure SetProfit(const APrice: Double = 0);

    function ToString: string; override;
    function IsActive: Boolean;
    property History: TJournalTradeList read FHistory;
  public
    property OpenTime: Int64 read FOpenTime;
    property CloseTime: Int64 read FCloseTime;
    property Side: TTypeBuySell read FSide;
    property OpenPrice: Double read FOpenPrice;
    property ClosePrice: Double read FClosePrice;
    property MovingPrice: Double read FMovingPrice;
    property Qty: Double read FQty;
    property Value: Double read FValue;
  public
    property Profit: Double read FProfit;
    property MinProfit: Double read FMinProfit;
    property MaxProfit: Double read FMaxProfit;
  end;
  TJournalPositionList = TObjectList<TJournalPosition>;


  ///<summary>Виртуальная торговля</summary>
  TJournalTrading = class(TObject)
  private
    FPosition: TJournalPosition;
    FPositions: TJournalPositionList;
    function GetIsPosition: Boolean;
    function GetProfitClosePosition: Double;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure OpenTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeBuySell); overload;
    procedure OpenTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell); overload;

    procedure CloseTrade(const ATime: Int64; const APrice: Double); overload;
    procedure CloseTrade(const ATime: TDateTime; const APrice: Double); overload;

    procedure ReverseTrade(const ATime: Int64; const APrice: Double); overload;
    procedure ReverseTrade(const ATime: TDateTime; const APrice: Double); overload;

    procedure SaveTrading(const AFileName: String);

    ///<summary>
    /// Список позиции
    ///</summary>
    property Positions: TJournalPositionList read FPositions;
    property CurrentPosition: TJournalPosition read FPosition;
    ///<summary>
    /// Есть открытая текущая позиция
    ///</summary>
    property IsPosition: Boolean read GetIsPosition;
  public
    property ProfitClosePosition: Double read GetProfitClosePosition;
    ///<summary>
    /// Информация по позиции инструмента
    ///</summary>
    function GetInfoPositionTrading(const AAsk, ABid: Double): TInfoPositionTrading;
  end;

implementation


{ TJournalTrade }

function TJournalTrade.ToString: string;
var
  xS: String;
begin
  xS :=
    Time.ToString + ';' +
    Price.ToString + ';' +
    Qty.ToString + ';' +
    GetStrToSide(Side);
  Result := xS;
end;

{ TJournalPosition }

constructor TJournalPosition.Create(const ABufferTrading: TJournalTrading);
begin
  FBufferTrading := ABufferTrading;
  FTrades := TJournalTradeList.Create;
  FHistory:= TJournalTradeList.Create;
end;

destructor TJournalPosition.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FTrades);
  inherited;
end;

procedure TJournalPosition.ExecuteTrade;
var
  xTrade: TJournalTrade;
begin
{$IFDEF DBG_POS_EXECUTE_TRADE}
  TLogger.Log('TPlatformTrading.TPosition.ExecuteTrade');
{$ENDIF}
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
{$IFDEF DBG_POS_EXECUTE_TRADE}
    TLogger.LogTreeText(3,'>> Qty:' + FQty.ToString);
    TLogger.LogTreeText(3,'>> Value: ' + FValue.ToString);
{$ENDIF}
    if FQty > 0 then
      FMovingPrice := FValue/FQty
    else
      FMovingPrice := 0;
  end;
{$IFDEF DBG_POS_EXECUTE_TRADE}
  TLogger.LogTreeText(3,'>> MovingPrice:' + FMovingPrice.ToString);
{$ENDIF}
end;

procedure TJournalPosition.SetProfit(const APrice: Double);
const
  SIZE_ROUND = 10000;

  function _GetValue: Double;
  var
    xValue: Double;
    xTrade: TJournalTrade;
    i, iCount: Integer;
  begin
    xValue := 0;
    iCount := FHistory.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTrade := FHistory[i];
        case xTrade.Side of
          tsBuy : xValue := xValue - xTrade.Price * xTrade.Qty;
          tsSell: xValue := xValue + xTrade.Price * xTrade.Qty;
        end;
      end;
    Result := xValue;
  end;

  procedure _ExtremesProfit(const AProfit: Double);
  begin
    if AProfit > FMaxProfit then
      FMaxProfit := AProfit;
    if AProfit < FMinProfit then
      FMinProfit := AProfit;
  end;

var
  xValue: Double;
begin
  xValue := 0;
  if FQty = 0 then
  begin
    xValue := _GetValue;
  end
  else
  begin
    if APrice = 0 then
    begin
      FProfit := 0;
      _ExtremesProfit(FProfit);
      Exit;
    end;

    case FSide of
      tsBuy : xValue := APrice * FQty + _GetValue;
      tsSell: xValue := _GetValue - APrice * FQty;
    end;
  end;
  FProfit := Round(xValue * SIZE_ROUND)/SIZE_ROUND;
  _ExtremesProfit(FProfit);
end;

function TJournalPosition.IsActive: Boolean;
begin
  Result := FTrades.Count > 0;
end;

procedure TJournalPosition.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xQty: Double;
  xTrade: TJournalTrade;
  xHistory: TJournalTrade;
begin
{$IFDEF DBG_POS_OPEN_TRADE}
  TLogger.Log('TPlatformTrading.TPosition.OpenTrade:');
  TLogger.LogTreeText(3,'>> Time:' + ATime.ToString);
  TLogger.LogTreeText(3,'>> Price:' + APrice.ToString);
  TLogger.LogTreeText(3,'>> Qty:' + AQty.ToString);
  TLogger.LogTreeText(3,'>> Side:' + GetStrToSide(ASide));
{$ENDIF}
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
{$IFDEF DBG_POS_OPEN_TRADE}
    TLogger.LogText('Записываем сделку');
{$ENDIF}
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
{$IFDEF DBG_POS_OPEN_TRADE}
    TLogger.LogText('Записываем сделку. Направление сделки совпадает');
{$ENDIF}
    end
    else
    begin
      xQty := AQty;
      xTrade := FTrades[0];
      while xQty > 0 do
      begin
        xQty := xQty - xTrade.Qty;
        if xQty >= 0 then
        begin
          FTrades.Delete(0);
          if FTrades.Count > 0 then
            xTrade := FTrades[0]
          else
          begin
            if Assigned(FBufferTrading) then
              FBufferTrading.FPosition := nil;
            FCloseTime := ATime;
            FClosePrice := APrice;
            Break;
          end;
        end
        else
        begin
          xTrade.Qty := -1 * xQty;
          FTrades[0] := xTrade;
          Break;
        end;
      end;
{$IFDEF DBG_POS_OPEN_TRADE}
      TLogger.LogText('Принцип первый пришел, первый ушёл');
      TLogger.LogTreeText(3,'>> xQty:' + xQty.ToString);
      TLogger.LogTreeText(3,'>> Trades.Count:' + FTrades.Count.ToString);
{$ENDIF}
      if xQty > 0 then
      begin

        var xIndex := FHistory.Count - 1;
        xHistory := FHistory[xIndex];
        xHistory.Qty := xTrade.Qty;
        FHistory[xIndex] := xHistory;

        FBufferTrading.FPosition := nil;
        FBufferTrading.OpenTrade(
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

function TJournalPosition.ToString: string;
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

{ TJournalTrading }


constructor TJournalTrading.Create;
begin
  FPosition := nil;
  FPositions := TJournalPositionList.Create;
end;

destructor TJournalTrading.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

function TJournalTrading.GetIsPosition: Boolean;
begin
  Result := False;
  if Assigned(FPosition) then
    Result := FPosition.IsActive;
end;

procedure TJournalTrading.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
begin
  if not Assigned(FPosition) then
  begin
    FPosition := TJournalPosition.Create(Self);
    FPositions.Add(FPosition);
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
  end
  else
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
end;

procedure TJournalTrading.OpenTrade(const ATime: TDateTime; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  OpenTrade(xTime,APrice,AQty,ASide);
end;


function TJournalTrading.GetProfitClosePosition: Double;
var
  xValue: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  xValue := 0;
  iCount := FPositions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := FPositions[i];
      xValue := xValue + xPosition.Profit;
    end;
  Result := xValue;
end;

procedure TJournalTrading.CloseTrade(const ATime: Int64; const APrice: Double);
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

procedure TJournalTrading.CloseTrade(const ATime: TDateTime; const APrice: Double);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  Self.CloseTrade(xTime,APrice);
end;

procedure TJournalTrading.ReverseTrade(const ATime: Int64; const APrice: Double);
var
  xQty: Double;
  xSide: TTypeBuySell;
begin
  if Assigned(FPosition) then
  begin
    xQty := 2 * FPosition.Qty;
    xSide := GetCrossSide(FPosition.Side);
    OpenTrade(
      ATime,
      APrice,
      xQty,
      xSide
    );
  end;
end;

procedure TJournalTrading.ReverseTrade(const ATime: TDateTime; const APrice: Double);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  Self.ReverseTrade(xTime,APrice);
end;

procedure TJournalTrading.SaveTrading(const AFileName: String);
var
  xStr: TStrings;
  xTrade: TJournalTrade;
  xPosition: TJournalPosition;
begin
  xStr := TStringList.Create;
  try
    for xPosition in FPositions do
    begin
      xStr.Add(xPosition.ToString);
      for xTrade in xPosition.History do
        xStr.Add('>>' + xTrade.ToString);
    end;
    xStr.SaveToFile(AFileName);
  finally
    FreeAndNil(xStr);
  end;
end;

function TJournalTrading.GetInfoPositionTrading(const AAsk, ABid: Double): TInfoPositionTrading;
var
  xInfo: TInfoPositionTrading;
begin
  FillChar(xInfo,SizeOf(xInfo),0);
  if Assigned(CurrentPosition) then
  begin
    xInfo.Side := CurrentPosition.Side;
    xInfo.Price := CurrentPosition.MovingPrice;
    case xInfo.Side of
      tsBuy: begin
        CurrentPosition.SetProfit(AAsk);
        xInfo.Profit := CurrentPosition.Profit;
      end;
      tsSell: begin
        CurrentPosition.SetProfit(ABid);
        xInfo.Profit := CurrentPosition.Profit;
      end;
    end;
    xInfo.Qty := CurrentPosition.Qty;
    xInfo.Count := CurrentPosition.History.Count;
  end;
  Result := xInfo;
end;

end.
