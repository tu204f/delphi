unit Lb.VirtualTrade;

interface

//{$DEFINE TEST_VT}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
{$IFDEF TEST_VT}
  System.IniFiles;
{$ELSE}
  System.IniFiles,
  Lb.SysUtils;
{$ENDIF}


type
{$IFDEF TEST_VT}
  TQBTypeSide = (
    tsBuy,
    tsSell
  );
{$ENDIF}

  ///<summary>
  /// Состояние сделки
  ///</summary>
  TTypeTrade = (ttOpen,ttClose);

  ///<summary>
  /// Параметры открытой операции
  ///</summary>
  TParamTrade = record
    Time: TDateTime;
    Symbol: String;
    Side: TQBTypeSide;
    Qty: Double;
    Price: Double;
    OrderLinkId: String;
    TypeTrade: TTypeTrade; // Статус сделки
    Profit: Double;        // Профит на прибаль
  private
    function GetToStr: String;
  public
    property ToStr: String read GetToStr;
  end;
  TParamTradeList = TList<TParamTrade>;

  ///<summary>
  /// Позиция торговых операции
  ///</summary>
  TPostionTrade = class(TObject)
  private
    FNumber: Integer;
    FQty: Double;
    FSide: TQBTypeSide;
    FProfit: Double;
    FParamTrades: TParamTradeList;
  protected
    procedure AddParamTrade(const AParamTrade: TParamTrade);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure OpenTrade(const AParamTrade: TParamTrade);
    procedure CloseTrade(const AParamTrade: TParamTrade);

    property Qty: Double read FQty;
    property Side: TQBTypeSide read FSide;

    property Profit: Double read FProfit;
    property Items: TParamTradeList read FParamTrades;
    property Number: Integer read FNumber write FNumber;
  end;
  TPostionTradeList = TObjectList<TPostionTrade>;

  ///<summary>
  /// Виртуальная сделка
  ///</summary>
  TVirtualTrades = class(TObject)
  private
    FPostionTrade: TPostionTrade;
    FPostionTrades: TPostionTradeList;
    FOnChange: TNotifyEvent;
    function GetCount: Integer;
    function GetItems(Index: Integer): TPostionTrade;
    function GetQty: Double;
    function GetSide: TQBTypeSide;
  protected
    procedure DoChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddOrder(
      ASymbol: String;     // Торговый символ
      ASide: TQBTypeSide;  // Напровление торгового оперций
      AQty: Double;        // Количество
      APrice: Double;      // Цена
      AOrderLinkId: String // Напровление объекта
    );
    property Items[Index: Integer]: TPostionTrade read GetItems;
    property Count: Integer read GetCount;

    property Qty: Double read GetQty;
    property Side: TQBTypeSide read GetSide;

    procedure Save(const AFileName: String);
    property OnChange: TNotifyEvent write FOnChange;
  end;


///<summary>
/// Для вертуальных операция, повторяем структура основной стратегии
///</summary>
function Virtual_SelectedOrder(
  ASymbol: String;     // Торговый символ
  ASide: TQBTypeSide;  // Напровление торгового оперций
  AQty: Double;        // Количество
  APrice: Double;      // Цена
  AOrderLinkId: String // Напровление объекта
): String;


///<summary>
/// Списко вертуальный сделок
///</summary>
function GetVirtualTrades: TVirtualTrades;

{$IFDEF TEST_VT}
function GetStrToTypeSide(const ASide: TQBTypeSide): String;
function GetCrossTypeSide(const ASide: TQBTypeSide): TQBTypeSide;
{$ENDIF}

function GetStrToTypeTrade(const  AValue: TTypeTrade): String;

implementation

uses
  Lb.Logger;

{$IFDEF TEST_VT}
function GetStrToTypeSide(const ASide: TQBTypeSide): String;
begin
  case ASide of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  else
    Result := 'not TTypeSide';
  end;
end;

function GetCrossTypeSide(const ASide: TQBTypeSide): TQBTypeSide;
begin
  case ASide of
    tsBuy: Result := TQBTypeSide.tsSell;
    tsSell: Result := TQBTypeSide.tsBuy;
  else
    raise Exception.Create('Error Message: GetCrossTypeSide');
  end;
end;

{$ENDIF}

function GetStrToTypeTrade(const  AValue: TTypeTrade): String;
begin
  case AValue of
    ttOpen: Result := 'Open';
    ttClose: Result := 'Close';
  else
    Result := 'not TTypeTrade';
  end;
end;

var
  localVirtualTrades: TVirtualTrades = nil;

function GetVirtualTrades: TVirtualTrades;
begin
  if not Assigned(localVirtualTrades) then
    localVirtualTrades := TVirtualTrades.Create;
  Result := localVirtualTrades;
end;

function Virtual_SelectedOrder(
  ASymbol: String;     // Торговый символ
  ASide: TQBTypeSide;  // Напровление торгового оперций
  AQty: Double;        // Количество
  APrice: Double;      // Цена
  AOrderLinkId: String // Напровление объекта
): String;
begin
  try
    GetVirtualTrades.AddOrder(ASymbol, ASide, AQty, APrice, AOrderLinkId);
    Result := 'OK.';
  except
    Result := 'Error.';
  end;
end;

{ TParamTrade }

function TParamTrade.GetToStr: String;
var
  xS: String;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';
  xS := '';
  xS := xS + DateTimeToStr(Time,xF) + ';';
  xS := xS + Symbol + ';';
  xS := xS + GetStrToTypeSide(Side) + ';';
  xS := xS + FloatToStr(Qty,xF) + ';';
  xS := xS + FloatToStr(Price,xF) + ';';
  xS := xS + OrderLinkId + ';';
  case TypeTrade of
    ttOpen : xS := xS + 'open;;';
    ttClose: xS := xS + 'close;' + Profit.ToString + ';';
  end;
  Result := xS;
end;

{ TPostionTrade }


constructor TPostionTrade.Create;
begin
  FProfit := 0;
  FQty := 0;
  FSide := TQBTypeSide.tsBuy;
  FParamTrades := TParamTradeList.Create;
end;

destructor TPostionTrade.Destroy;
begin
  FreeAndNil(FParamTrades);
  inherited;
end;

procedure TPostionTrade.OpenTrade(const AParamTrade: TParamTrade);
var
  xParamTrade: TParamTrade;
begin
  xParamTrade := AParamTrade;
  xParamTrade.Profit := 0;
  Self.AddParamTrade(xParamTrade);
end;


procedure TPostionTrade.CloseTrade(const AParamTrade: TParamTrade);

  function _GetRoundDouble(const AValue: Double): Double;
  begin
    Result := Round(AValue * 10000)/10000;
  end;

var
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  Self.AddParamTrade(AParamTrade);
  // ---------------------------
  // Вычисляем профит
  FProfit := 0;
  iCount := FParamTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParamTrade := FParamTrades[i];
      case xParamTrade.Side of
        TQBTypeSide.tsBuy : FProfit := FProfit - xParamTrade.Price * xParamTrade.Qty;
        TQBTypeSide.tsSell: FProfit := FProfit + xParamTrade.Price * xParamTrade.Qty;
      end;
      if xParamTrade.TypeTrade = TTypeTrade.ttClose then
      begin
        if i = (iCount - 1) then
        begin
          xParamTrade.Profit := FProfit;
          FParamTrades.Items[i] := xParamTrade;
        end;
      end
      else
        xParamTrade.Profit := 0;
    end;
  // -------------------------------
  // Проверяем все правильно закрыта позицияя
  FQty := _GetRoundDouble(FQty);
  if FQty < 0 then
    raise Exception.Create('Error Message: Такого не может быть');
end;

procedure TPostionTrade.AddParamTrade(const AParamTrade: TParamTrade);
var
  xParamTrade: TParamTrade;
begin
  xParamTrade := AParamTrade;
  if FQty = 0 then
  begin
    FQty := xParamTrade.Qty;
    FSide := xParamTrade.Side;
  end
  else
  begin
    case FSide of
      TQBTypeSide.tsBuy: begin
        case xParamTrade.Side of
          TQBTypeSide.tsBuy: FQty := FQty + xParamTrade.Qty;
          TQBTypeSide.tsSell: FQty := FQty - xParamTrade.Qty;
        end;
      end;
      TQBTypeSide.tsSell: begin
        case xParamTrade.Side of
          TQBTypeSide.tsBuy: FQty := FQty - xParamTrade.Qty;
          TQBTypeSide.tsSell: FQty := FQty + xParamTrade.Qty;
        end;
      end;
    end;
  end;
  FParamTrades.Add(AParamTrade);
end;

{ TVirtualTrades }

constructor TVirtualTrades.Create;
begin
  FPostionTrade := nil;
  FPostionTrades := TPostionTradeList.Create;
end;

destructor TVirtualTrades.Destroy;
begin
  FreeAndNil(FPostionTrades);
  inherited;
end;


procedure TVirtualTrades.Clear;
begin
  FPostionTrades.Clear;
end;


procedure TVirtualTrades.AddOrder(ASymbol: String; ASide: TQBTypeSide;
  AQty: Double; APrice: Double; AOrderLinkId: String);

  function _TypeTradeAndQty(const AOrderLinkID: String): TTypeTrade;
  begin
    var xC := AOrderLinkID[1];
    if CharInSet(xC,['C','c']) then
      Result := TTypeTrade.ttClose
    else
      Result := TTypeTrade.ttOpen;
  end;

  function _GetRoundDouble(const AValue: Double): Double;
  begin
    Result := Round(AValue * 10000)/10000;
  end;

  procedure _AddPostion(APostionTrade: TPostionTrade; ATypeTrade: TTypeTrade;
    ASymbol: String; ASide: TQBTypeSide; AQty: Double; APrice: Double;
    AOrderLinkId: String);
  var
    xParamTrade: TParamTrade;
  begin
    xParamTrade.Time        := Date + Time;
    xParamTrade.Symbol      := ASymbol;
    xParamTrade.Side        := ASide;
    xParamTrade.Qty         := AQty;
    xParamTrade.Price       := APrice;
    xParamTrade.OrderLinkId := AOrderLinkID;
    xParamTrade.TypeTrade   := ATypeTrade;
    case ATypeTrade of
      ttOpen: APostionTrade.OpenTrade(xParamTrade);
      ttClose: APostionTrade.CloseTrade(xParamTrade);
    end;
  end;

var
  xQty: Double;
begin
  if Assigned(FPostionTrade) then
  begin

    xQty := 0;
    case FPostionTrade.Side of
      TQBTypeSide.tsBuy: begin
        case ASide of
          TQBTypeSide.tsBuy: xQty := FPostionTrade.Qty + AQty;
          TQBTypeSide.tsSell: xQty := FPostionTrade.Qty - AQty;
        end;
      end;
      TQBTypeSide.tsSell: begin
        case ASide of
          TQBTypeSide.tsBuy: xQty := FPostionTrade.Qty - AQty;
          TQBTypeSide.tsSell: xQty := FPostionTrade.Qty + AQty;
        end;
      end;
    end;

    xQty := _GetRoundDouble(xQty);
    if xQty = 0 then
    begin
      _AddPostion(FPostionTrade,TTypeTrade.ttClose,ASymbol,ASide,FPostionTrade.Qty,APrice,AOrderLinkId);
      FPostionTrade := nil;
    end else if xQty < 0 then
    begin
      _AddPostion(FPostionTrade,TTypeTrade.ttClose,ASymbol,ASide,FPostionTrade.Qty,APrice,AOrderLinkId);
      FPostionTrade := TPostionTrade.Create;
      _AddPostion(FPostionTrade,TTypeTrade.ttOpen,ASymbol,ASide,Abs(xQty),APrice,AOrderLinkId);
      FPostionTrades.Add(FPostionTrade);
    end else
    begin
      _AddPostion(FPostionTrade,TTypeTrade.ttOpen,ASymbol,ASide,AQty,APrice,AOrderLinkId);
    end;
  end
  else
  begin
    FPostionTrade := TPostionTrade.Create;
    _AddPostion(FPostionTrade,TTypeTrade.ttOpen,ASymbol,ASide,AQty,APrice,AOrderLinkId);
    FPostionTrades.Add(FPostionTrade);
  end;
  DoChange;
end;


function TVirtualTrades.GetCount: Integer;
begin
  Result := FPostionTrades.Count;
end;

function TVirtualTrades.GetItems(Index: Integer): TPostionTrade;
begin
  Result := FPostionTrades.Items[Index];
end;

procedure TVirtualTrades.Save(const AFileName: String);
var
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    for var xPostionTrade in FPostionTrades do
      for var xParamTrade in xPostionTrade.Items do
        xStr.Add(xParamTrade.ToStr);
    xStr.SaveToFile(AFileName);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TVirtualTrades.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Save('result_trade.csv');
end;

function TVirtualTrades.GetQty: Double;
begin
  if Assigned(FPostionTrade) then
    Result := FPostionTrade.Qty
  else
    Result := 0;
end;

function TVirtualTrades.GetSide: TQBTypeSide;
begin
  if Assigned(FPostionTrade) then
    Result := FPostionTrade.Side
  else
    Result := TQBTypeSide.tsBuy;
end;

initialization


finalization
  FreeAndNil(localVirtualTrades);

end.
