unit Lb.Trade.Data;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice;

type
  ///<summary>
  /// Сделка
  ///</summary>
  TTrade = class(TObject)
  private
    FPrice: Double;
    FBuySell: Char;
    FProfitFirst: Double;
    FProfitFirstMax: Double;
    FProfitFirstMin: Double;
    FProfit: Double;
    FProfitMax: Double;
    FProfitMin: Double;
    FIsFirst: Boolean;
    FQty: Double;
    FValueRSI: Double;
    function GetValue: Double;
  private
    FOpenDate: TDateTime;
    FOpenTime: TDateTime;
    FCloseDate: TDateTime;
    FCloseTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>
    /// Первая свяча, обновление
    ///</summary>
    procedure SetUpDataFirst(const ACandel: TCandel);
    ///<summary>
    /// Обновленеи свячи
    ///</summary>
    procedure SetUpData(const ACandel: TCandel);
    ///<summary>
    /// Цена - сделки
    ///</summary>
    property Price: Double read FPrice write FPrice;
    ///<summary>
    /// Напровление сделки
    ///</summary>
    property BuySell: Char read FBuySell write FBuySell;
    ///<summary>
    /// Количество - объем сделки
    ///</summary>
    property Qty: Double read FQty write FQty;
    property ValueRSI: Double read FValueRSI write FValueRSI;
    property Value: Double read GetValue;
  public
    property ProfitFirst: Double read FProfitFirst;
    property ProfitFirstMax: Double read FProfitFirstMax;
    property ProfitFirstMin: Double read FProfitFirstMin;
    property Profit: Double read FProfit;
    property ProfitMax: Double read FProfitMax;
    property ProfitMin: Double read FProfitMin;
    property IsFirst: Boolean read FIsFirst;
  public
    procedure SetOpenDateTime(const ADate, ATime: TDateTime);
    procedure SetCloseDateTime(const ADate, ATime: TDateTime);
    property OpenDate: TDateTime read FOpenDate;
    property OpenTime: TDateTime read FOpenTime;
    property CloseDate: TDateTime read FCloseDate;
    property CloseTime: TDateTime read FCloseTime;
  end;
  TTradeList = TObjectList<TTrade>;

implementation

function GetRoundTo(const AValue: Double): Double;
const
  R = 100000;
begin
  Result := Round(AValue * R)/R;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FPrice := 0;
  FBuySell := #0;
  FProfitMax := 0;
  FProfitMin := 9999999;
  FIsFirst := False;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.SetCloseDateTime(const ADate, ATime: TDateTime);
begin
  FCloseDate := ADate;
  FCloseTime := ATime;
end;

procedure TTrade.SetOpenDateTime(const ADate, ATime: TDateTime);
begin
  FOpenDate := ADate;
  FOpenTime := ATime;
end;

function TTrade.GetValue: Double;
begin
  Result := GetRoundTo(FPrice * FQty);
end;

procedure TTrade.SetUpData(const ACandel: TCandel);
var
  xValueMin, xValueMax: Double;
begin
  xValueMin := 0;
  xValueMax := 0;

  if BuySell = #0 then
    Exit;
  case FBuySell of
    'B': begin
      xValueMin := GetRoundTo(FQty * (ACandel.Low - FPrice));
      xValueMax := GetRoundTo(FQty * (ACandel.High - FPrice));
      FProfit   := GetRoundTo(FQty * (ACandel.Close - FPrice));
    end;
    'S': begin
      xValueMin := GetRoundTo(FQty * (FPrice - ACandel.High));
      xValueMax := GetRoundTo(FQty * (FPrice - ACandel.Low));
      FProfit   := GetRoundTo(FQty * (FPrice - ACandel.Close));
    end;
  end;

  if xValueMax > FProfitMax then
    FProfitMax := xValueMax;
  if xValueMin < FProfitMin then
    FProfitMin := xValueMin;
end;

procedure TTrade.SetUpDataFirst(const ACandel: TCandel);
begin
  if BuySell = #0 then
    Exit;
  FIsFirst := True;
  case FBuySell of
    'B': begin
      FProfitFirst    := GetRoundTo(FQty * (ACandel.Close - FPrice));
      FProfitFirstMin := GetRoundTo(FQty * (ACandel.Low - FPrice));
      FProfitFirstMax := GetRoundTo(FQty * (ACandel.High - FPrice));
    end;
    'S': begin
      FProfitFirst    := GetRoundTo(FQty * (FPrice - ACandel.Close));
      FProfitFirstMin := GetRoundTo(FQty * (FPrice - ACandel.High));
      FProfitFirstMax := GetRoundTo(FQty * (FPrice - ACandel.Low));
    end;
  end;
  FProfitMax := FProfitFirstMax;
  FProfitMin := FProfitFirstMin;
end;

end.
