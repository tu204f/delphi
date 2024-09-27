unit Lb.PositionTrade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice;

{$DEFINE TEST_DEBUG}
  
type
  ///<summary>
  /// Напровление тренда
  ///</summary>
  TTypeTrande   = (ttNull, ttLong, ttShort);

  ///<summary>
  /// Состояние позиции
  ///</summary>
  TTypePosition = (tpNull, tpOpen, tpClose);

  ///<summary>
  /// Сделка
  ///</summary>
  TTrade = record
    Date: TDateTime;
    Time: TDateTime;
    Price: Double;
    Qty: Double;
    BuySell: Char;
  private
    function GetValue: Double;
  public
    function GetToStr: String;
    property Value: Double read GetValue;
  end;
  TTradeList = TList<TTrade>;

  ///<summary>
  /// Позиция
  ///</summary>
  TPositionTrade = class(TObject)
  public type
    TParam = record
      Price: Double;
      Qty: Double;
    private
      function GetBuySell: Char;
    public
      property BuySell: Char read GetBuySell;
    end;
  private
    FParam: TParam;
    FTypePosition: TTypePosition;
    FTrades: TTradeList;
    FBufferTrades: TTradeList;
    function GetDate: TDateTime;
    function GetTime: TDateTime;
    function GetQty: Double;
    function GetBuySell: Char;
    function GetPrice: Double;
  protected
    procedure SetTradeData;
  public
    constructor Create;
    destructor Destroy; override;
{$IFDEF TEST_DEBUG}   
    procedure OpenTrade(ADate, ATime: TDateTime; APrice: Double; AQty: Double; ABuySell: Char);
    procedure CloseTrade(ADate, ATime: TDateTime; APrice: Double);
{$ELSE}
    procedure OpenTrade(ACandel: TCandel; AQty: Double; ABuySell: Char);
    procedure CloseTrade(ACandel: TCandel);
{$ENDIF} 
    procedure SetUpDate(ACandel: TCandel);
    property Trades: TTradeList read FTrades;
    property BufferTrades: TTradeList read FBufferTrades;
  public
    property Date: TDateTime read GetDate;
    property Time: TDateTime read GetTime;
    property Price: Double read GetPrice;
    property Qty: Double read GetQty;
    property BuySell: Char read GetBuySell;
    property TypePosition: TTypePosition read FTypePosition;
  end;


implementation

{ TTrade }

function TTrade.GetValue: Double;
begin
  Result := Self.Price * Self.Qty;
end;

function TTrade.GetToStr: String;
var
  xS: String;
begin
  xS := DateToStr(Date) + ' ' + TimeToStr(Time) + '; ';
  xS := xS + 'Price: ' + Price.ToString + '; ';
  xS := xS + 'Qty: ' + Qty.ToString + '; ';
  xS := xS + 'BuySell: ' + BuySell + '; ';
  Result := xS;
end;

{ TPositionTrade.TParam }

function TPositionTrade.TParam.GetBuySell: Char;
begin
  if Qty > 0  then
    Result := 'B'
  else if Qty < 0 then
    Result := 'S'
  else
    Result := #0;
end;

{ TPositionTrade }

constructor TPositionTrade.Create;
begin
  FTypePosition := TTypePosition.tpNull;
  FTrades := TTradeList.Create;
  FBufferTrades := TTradeList.Create;
  FillChar(FParam,SizeOf(FParam),0);
end;

destructor TPositionTrade.Destroy;
begin
  FreeAndNil(FBufferTrades);
  FreeAndNil(FTrades);
  inherited;
end;

function _CrossBuySell(ABuySell: Char): Char;
begin
  case ABuySell of
    'B': Result := 'S';
    'S': Result := 'B';
  end;
end;

{$IFDEF TEST_DEBUG}  
procedure TPositionTrade.OpenTrade(ADate, ATime: TDateTime; APrice: Double; AQty: Double; ABuySell: Char);
{$ELSE}
procedure TPositionTrade.OpenTrade(ACandel: TCandel; AQty: Double; ABuySell: Char);
{$ENDIF}

  function _BufferTrade(AQty: Double; ABuySell: Char): Double;
  var
    xTrade: TTrade;
    xQty: Double;
  begin
    xQty := AQty;
    if not (Self.BuySell = ABuySell) then
    begin
      while xQty > 0 do
      begin
        xTrade := FBufferTrades[0];
        xQty := xQty - xTrade.Qty;
        FBufferTrades.Delete(0);
      end;
    end;
    Result := xQty;
  end;

var
  xQty: Double;
  xTrade: TTrade;
begin
  FTypePosition := TTypePosition.tpOpen;
  if FBufferTrades.Count = 0 then
  begin
    xTrade.Date    := ADate;
    xTrade.Time    := ATime;
    xTrade.Price   := APrice;
    xTrade.Qty     := AQty;
    xTrade.BuySell := ABuySell;
    FTrades.Add(xTrade);
    FBufferTrades.Add(xTrade);
  end
  else 
  begin
    xQty := _BufferTrade(AQty, ABuySell); 
    if xQty > 0 then
    begin
      xTrade.Date    := ADate;
      xTrade.Time    := ATime;
      xTrade.Price   := APrice;
      xTrade.Qty     := xQty;
      xTrade.BuySell := ABuySell;
      FTrades.Add(xTrade);
      FBufferTrades.Add(xTrade);
    end
    else if xQty < 0 then
    begin
      FBufferTrades.Clear;
      xTrade.Date    := ADate;
      xTrade.Time    := ATime;
      xTrade.Price   := APrice;
      xTrade.Qty     := Abs(xQty);
      xTrade.BuySell := ABuySell;
      FTrades.Add(xTrade);
      FBufferTrades.Add(xTrade);   
    end
    else
    begin
      FBufferTrades.Clear;
      FTypePosition := TTypePosition.tpClose;
    end;
  end;
  SetTradeData;
end;

{$IFDEF TEST_DEBUG} 
procedure TPositionTrade.CloseTrade(ADate, ATime: TDateTime; APrice: Double);
{$ELSE}
procedure TPositionTrade.CloseTrade(ACandel: TCandel);
{$ENDIF}
var
  xTrade: TTrade;
begin
  xTrade.Date    := ADate;
  xTrade.Time    := ATime;
  xTrade.Price   := APrice;
  xTrade.Qty   := Self.Qty;
  xTrade.BuySell := _CrossBuySell(Self.BuySell);
  FTrades.Add(xTrade);
  FBufferTrades.Clear;
  SetTradeData;
end;

procedure TPositionTrade.SetTradeData;
var
  xValue: Double;
  xTrade: TTrade;
  i, iCount: Integer;
begin
  xValue := 0;
  FParam.Qty := 0;
  iCount := FBufferTrades.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xTrade := FBufferTrades[i];
      xValue := xValue + xTrade.Value;
      FParam.Qty := FParam.Qty + xTrade.Qty;
    end;
    if FParam.Qty = 0 then
      FParam.Price := 0
    else
      FParam.Price := xValue/FParam.Qty;
  end;
end;

procedure TPositionTrade.SetUpDate(ACandel: TCandel);
begin
  SetTradeData;
end;

function TPositionTrade.GetDate: TDateTime;
begin
  Result := 0;
  if FTrades.Count > 0 then
    Result := FTrades[0].Date;
end;

function TPositionTrade.GetTime: TDateTime;
begin
  Result := 0;
  if FTrades.Count > 0 then
    Result := FTrades[0].Time;
end;

function TPositionTrade.GetQty: Double;
begin
  Result := Abs(FParam.Qty);
end;

function TPositionTrade.GetBuySell: Char;
begin
  Result := FParam.BuySell;
end;

function TPositionTrade.GetPrice: Double;
begin
  Result := FParam.Price;
end;

initialization

finalization

end.
