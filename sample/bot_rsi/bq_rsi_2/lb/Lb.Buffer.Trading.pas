(******************************************************************************)
(* Фиксация торговой операции независимо от торговой платформы                *)
(******************************************************************************)
unit Lb.Buffer.Trading;

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
  TBufferTrading = class;

  ///<summary>Напров</summary>
  TInfoPositionTrading = record
    Profit: Double;     //
    Price: Double;      //
    Qty: Double;        //
    Count: Integer;     //
    Side: TTypeBuySell; //
  end;

  ///<summary>Сделка</summary>
  TBufferTrade = record
    Time: Int64;
    Price: Double;
    Qty: Double;
    Side: TTypeBuySell;
  public
    function ToString: string;
  end;
  TBufferTradeList = TList<TBufferTrade>;

  ///<summary>Позиция</summary>
  TBufferPosition = class(TObject)
  private
    FOpenTime: Int64;
    FCloseTime: Int64;
    FSide: TTypeBuySell;
    FOpenPrice: Double;
    FClosePrice: Double;
    FMovingPrice: Double;
    FQty: Double;
    FValue: Double;
    FTrades: TBufferTradeList;
    FBufferTrading: TBufferTrading;
  private
    FHistory: TBufferTradeList;
  protected
    procedure ExecuteTrade;
  public
    constructor Create(const ABufferTrading: TBufferTrading); virtual;
    destructor Destroy; override;
    procedure OpenTrade(const ATime: Int64; const APrice, AQty: Double; ASide: TTypeBuySell);
    function GetProfit(const APrice: Double = 0): Double;
    function ToString: string; override;
    function IsActive: Boolean;
    property History: TBufferTradeList read FHistory;
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
  TBufferPositionList = TObjectList<TBufferPosition>;


  ///<summary>Виртуальная торговля</summary>
  TBufferTrading = class(TObject)
  private
    FPosition: TBufferPosition;
    FPositions: TBufferPositionList;
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
    property Positions: TBufferPositionList read FPositions;
    property CurrentPosition: TBufferPosition read FPosition;
    ///<summary>
    /// Есть открытая текущая позиция
    ///</summary>
    property IsPosition: Boolean read GetIsPosition;
  public
    property ProfitClosePosition: Double read GetProfitClosePosition;
    function GetInfoPositionTrading(const AAsk, ABid: Double): TInfoPositionTrading;
  end;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

{ TBufferTrade }

function TBufferTrade.ToString: string;
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

{ TBufferPosition }

constructor TBufferPosition.Create(const ABufferTrading: TBufferTrading);
begin
  FBufferTrading := ABufferTrading;
  FTrades := TBufferTradeList.Create;
  FHistory:= TBufferTradeList.Create;
end;

destructor TBufferPosition.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FTrades);
  inherited;
end;

procedure TBufferPosition.ExecuteTrade;
var
  xTrade: TBufferTrade;
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

function TBufferPosition.GetProfit(const APrice: Double): Double;
const
  SIZE_ROUND = 10000;

  function _GetValue: Double;
  var
    xValue: Double;
    xTrade: TBufferTrade;
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
      Result := 0;
      Exit;
    end;

    case FSide of
      tsBuy : xValue := APrice * FQty + _GetValue;
      tsSell: xValue := _GetValue - APrice * FQty;
    end;
  end;
  Result := Round(xValue * SIZE_ROUND)/SIZE_ROUND;
end;

function TBufferPosition.IsActive: Boolean;
begin
  Result := FTrades.Count > 0;
end;

procedure TBufferPosition.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xQty: Double;
  xTrade: TBufferTrade;
  xHistory: TBufferTrade;
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

function TBufferPosition.ToString: string;
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

{ TBufferTrading }


constructor TBufferTrading.Create;
begin
  FPosition := nil;
  FPositions := TBufferPositionList.Create;
end;

destructor TBufferTrading.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

function TBufferTrading.GetIsPosition: Boolean;
begin
  Result := False;
  if Assigned(FPosition) then
    Result := FPosition.IsActive;
end;

procedure TBufferTrading.OpenTrade(const ATime: Int64; const APrice,
  AQty: Double; ASide: TTypeBuySell);
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.OpenTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + ATime.ToString);
  TLogger.LogTreeText(3,'>> APrice: ' + APrice.ToString);
  TLogger.LogTreeText(3,'>> AQty: ' + AQty.ToString);
  TLogger.LogTreeText(3,'>> ASide: ' + GetStrToSide(ASide));
  {$ENDIF}
  if not Assigned(FPosition) then
  begin
    FPosition := TBufferPosition.Create(Self);
    FPositions.Add(FPosition);
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
  end
  else
    FPosition.OpenTrade(ATime,APrice,AQty,ASide);
end;

procedure TBufferTrading.OpenTrade(const ATime: TDateTime; const APrice,
  AQty: Double; ASide: TTypeBuySell);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.OpenTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + DateTimeToStr(ATime));
  {$ENDIF}
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  OpenTrade(xTime,APrice,AQty,ASide);
end;


function TBufferTrading.GetProfitClosePosition: Double;
var
  xValue: Double;
  xPosition: TBufferPosition;
  i, iCount: Integer;
begin
  xValue := 0;
  iCount := FPositions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := FPositions[i];
      xValue := xValue + xPosition.GetProfit;
    end;
  Result := xValue;
end;

procedure TBufferTrading.CloseTrade(const ATime: Int64; const APrice: Double);
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.CloseTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + ATime.ToString);
  TLogger.LogTreeText(3,'>> APrice: ' + APrice.ToString);
  {$ENDIF}
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

procedure TBufferTrading.CloseTrade(const ATime: TDateTime; const APrice: Double);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.CloseTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + DateTimeToStr(ATime));
  {$ENDIF}
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  Self.CloseTrade(xTime,APrice);
end;

procedure TBufferTrading.ReverseTrade(const ATime: Int64; const APrice: Double);
var
  xQty: Double;
  xSide: TTypeBuySell;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.ReverseTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + ATime.ToString);
  TLogger.LogTreeText(3,'>> APrice: ' + APrice.ToString);
  {$ENDIF}
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

procedure TBufferTrading.ReverseTrade(const ATime: TDateTime; const APrice: Double);
var
  xTime: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TBufferTrading.ReverseTrade');
  TLogger.LogTreeText(3,'>> ATime: ' + DateTimeToStr(ATime));
  {$ENDIF}
  DecodeTime(ATime,xHour, xMin, xSec, xMSec);
  xTime := xHour * 3600 + xMin * 60  + xSec;//   xMSec;
  Self.ReverseTrade(xTime,APrice);
end;

procedure TBufferTrading.SaveTrading(const AFileName: String);
var
  xStr: TStrings;
  xTrade: TBufferTrade;
  xPosition: TBufferPosition;
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

function TBufferTrading.GetInfoPositionTrading(const AAsk, ABid: Double): TInfoPositionTrading;
var
  xInfo: TInfoPositionTrading;
begin
  FillChar(xInfo,SizeOf(xInfo),0);
  if Assigned(CurrentPosition) then
  begin
    xInfo.Side := CurrentPosition.Side;
    xInfo.Price := CurrentPosition.MovingPrice;
    case xInfo.Side of
      tsBuy: xInfo.Profit := CurrentPosition.GetProfit(AAsk);
      tsSell: xInfo.Profit := CurrentPosition.GetProfit(ABid);
    end;
    xInfo.Qty := CurrentPosition.Qty;
    xInfo.Count := CurrentPosition.History.Count;
  end;
  Result := xInfo;
end;

end.
