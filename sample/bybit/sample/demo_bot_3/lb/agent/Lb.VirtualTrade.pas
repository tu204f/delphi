unit Lb.VirtualTrade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.Trade;

type
  TTypeLine = (tlNull,tlLine1,tlLine2,tlLine3,tlLine4,tlLine5,tlLine6,tlCloseLine);
  TTypeStr = TTypeSide;

  ///<summary>Виртуальная сделка</summary>
  TVirtualTrade = record
  public
    Symbol: String;      // Наименование инструмента
    Side: TTypeSide;     // Напровление
    Qty: Double;         // Количество
    TypeLine: TTypeLine;
    TypeStr: TTypeStr;
  public
    ValueRSI: Double;    // Короткая значение RSI
  private
    FPrice: Double;
    FMaxPrice: Double;
    FMinPrice: Double;
    function GetValue: Double;
    procedure SetPrice(const Value: Double);
    function GetProfitMax: Double;
    function GetProfitMin: Double;
  public
    procedure Default;
    property MaxPrice: Double write FMaxPrice;
    property MinPrice: Double write FMinPrice;
    ///<summary>Значение работы</summary>
    property Price: Double read FPrice write SetPrice;
    ///<summary>Объем значение работы</summary>
    property Value: Double read GetValue;
    property ProfitMax: Double read GetProfitMax;
    property ProfitMin: Double read GetProfitMin;
  public
    function ToStr: String;
  end;

  ///<summary>Списко вертуальных сделок</summary>
  TVirtualTradeList = TList<TVirtualTrade>;

procedure SetVirtualOrderSelectedOrder(
  const APlaceOrder: TParamOrder;
  const AValueRSI: Double;
  ATypeLine: TTypeLine;
  ATypeStr: TTypeStr
);

function GetVirtualTrades: TVirtualTradeList;

implementation

uses
  System.SyncObjs,
  System.DateUtils;

var
  localVirtualTrades: TVirtualTradeList = nil;

function GetVirtualTrades: TVirtualTradeList;
begin
  if not Assigned(localVirtualTrades) then
    localVirtualTrades := TVirtualTradeList.Create;
  Result := localVirtualTrades;
end;

procedure SaveTrade(AVirtualTrade: TVirtualTrade);
var
  F: TextFile;
  xS: String;
  xPath: String;
begin
  xS := AVirtualTrade.ToStr;
  xPath := ExtractFilePath(ParamStr(0)) + 'trades.csv';
  AssignFile(f,xPath);
  if FileExists(xPath) then
    Append(F)
  else
    Rewrite(F);
  WriteLn(F, xS);
  CloseFile(F);
end;

procedure SetVirtualOrderSelectedOrder(
  const APlaceOrder: TParamOrder;
  const AValueRSI: Double;
  ATypeLine: TTypeLine;
  ATypeStr: TTypeStr
  );
var
  xVirtualTrade: TVirtualTrade;
begin
  with xVirtualTrade do
  begin
    Default;
    Symbol := APlaceOrder.Symbol; // Наименование инструмента
    Side := APlaceOrder.Side;     // Напровление
    Price := APlaceOrder.Price;   // Цена
    Qty := APlaceOrder.Qty;       // Количество
    ValueRSI := AValueRSI;        // Длиное значение RSI
    TypeLine := ATypeLine;
    TypeStr := ATypeStr;
  end;
  GetVirtualTrades.Add(xVirtualTrade);
  SaveTrade(xVirtualTrade);
end;

{ TVirtualTrade }

procedure TVirtualTrade.Default;
begin
  Symbol := '';            // Наименование инструмента
  Side := TTypeSide.tsBuy; // Напровление
  Qty := 0;                // Количество
  ValueRSI := 0;           // Длиное значение RSI
  FPrice := 0;             // Цена
  MaxPrice := 0;
  MinPrice := 0;
end;

function TVirtualTrade.GetValue: Double;
begin
  Result := Qty * Price;
end;

procedure TVirtualTrade.SetPrice(const Value: Double);
begin
  FPrice := Value;

  if FMaxPrice = 0 then
    FMaxPrice := FPrice
  else
    if FMaxPrice < FPrice then
      FMaxPrice := FPrice;

  if FMinPrice = 0 then
    FMinPrice := FPrice
  else
    if FMinPrice > FPrice then
      FMinPrice := FPrice;
end;


function TVirtualTrade.GetProfitMax: Double;
begin
  Result := 0;
  if FMaxPrice > 0 then
  begin
    case Side of
      tsBuy: Result := FMaxPrice - FPrice;
      tsSell: Result := FPrice - FMinPrice;
    end;
  end;
end;

function TVirtualTrade.GetProfitMin: Double;
begin
  Result := 0;
  if FMinPrice > 0 then
  begin
    case Side of
      tsBuy: Result := FPrice - FMinPrice;
      tsSell: Result := FMinPrice - Price;
    end;
  end;
end;

function TVirtualTrade.ToStr: String;
var
  xS: String;
begin
  xS := Symbol + ';' +             // Наименование инструмента
    GetStrToTypeSide(Side) + ';' + // Напровление
    Price.ToString + ';' +         // Цена
    Qty.ToString + ';' +           // Количество
    ValueRSI.ToString + ';';       // Короткая значение RSI   \


    case TypeLine of
      tlLine1: xS := xS + 'line1;';
      tlLine2: xS := xS + 'line2;';
      tlLine3: xS := xS + 'line3;';
      tlLine4: xS := xS + 'line4;';
      tlLine5: xS := xS + 'line5;';
      tlCloseLine: xS := xS + 'close_line;';
    end;


    case TypeStr of
      tsBuy: xS := xS + 'buy;';
      tsSell: xS := xS + 'sell;';
    end;

  Result := xS;
end;

initialization

finalization
  FreeAndNil(localVirtualTrades);

end.
