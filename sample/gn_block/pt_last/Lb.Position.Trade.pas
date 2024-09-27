unit Lb.Position.Trade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice.Tiket;

type
  ///<summary>
  /// Примерный индикатор принятие решения, для открытие позиции
  ///</summary>
  TIndicator = class(TObject)
  private
    FCountPridel: Integer;
    FTikets: TTiketList;
    FHighPrice: Double;
    FLowPrice: Double;
    FWillR: Double;
    function GetBuySell: Char;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetTitketLast(ATiket: TTiket);
    property Tikets: TTiketList read FTikets;
    ///<summary>
    /// Количество свечей (под контроль)
    ///</summary>
    property CountPridel: Integer read FCountPridel write FCountPridel;
    property WillR: Double read FWillR;
    property HighPrice: Double read FHighPrice;
    property LowPrice: Double read FLowPrice;
    ///<summary>
    /// Напровленеи сделки (под контроль)
    ///</summary>
    property BuySell: Char read GetBuySell;

  public
    function ToString: string; override;
  end;

  ///<summary>
  /// Напровление позиции
  ///</summary>
  TPosition = class(TObject)
  public type
    TTrd = class(TObject)
      Date: TDateTime;
      Time: TDateTime;
      Price: Double;
      Qty: Double;
      BuySell: Char;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Default;
    end;
    TTrdList = TObjectList<TTrd>;
  private
    FTrds: TTrdList;
    FHistory: TTrdList;
    FQty: Double;
    FPrice: Double;
    FValue: Double;
    procedure SetCalc;
    function GetBuySell: Char;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ATiket: TTiket; ABuySell: Char; AQty: Double);
    property Qty: Double read FQty;
    property Price: Double read FPrice;
    property BuySell: Char read GetBuySell;
  end;
  TPositionList = TObjectList<TPosition>;

  TStatusTrade = (
    stNull,     // Нейтральная состояние
    stOpen,     // Нужно отрыть позицию
    stClose     // Закрываем позиция
  );

  ///<summary>
  /// Вертуальный терейдор
  ///</summary>
  TTrade = class(TObject)
  private
    FStatus: TStatusTrade;
    FBuySell: Char;
  private
    FIndicator: TIndicator;
    FCurrentPosition: TPosition;
    FPositions: TPositionList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetTitketLast(ATiket: TTiket);
    property Indicator: TIndicator read FIndicator;
  end;

implementation

uses
{$IFDEF DEBUG}
  UnitMainForm,
{$ENDIF}
  System.Math;

{ TIndicator }

constructor TIndicator.Create;
begin
  FCountPridel := 10000;
  FWillR := -1;
  FTikets := TTiketList.Create;
end;

destructor TIndicator.Destroy;
begin
  FreeAndNil(FTikets);
  inherited;
end;

procedure TIndicator.SetTitketLast(ATiket: TTiket);
var
  iCount: Integer;
  xTiket: TTiket;
begin
  FTikets.Add(ATiket);
  if FTikets.Count >= FCountPridel then
    FTikets.Delete(0);

  iCount := FTikets.Count;
  if iCount > 0 then
  begin
    xTiket := FTikets[0];
    FHighPrice := xTiket.Last;
    FLowPrice := xTiket.Last;
    for var i := 1 to iCount - 1 do
    begin
      xTiket := FTikets[i];
      if FHighPrice < xTiket.Last then
        FHighPrice := xTiket.Last;
      if FLowPrice > xTiket.Last then
        FLowPrice := xTiket.Last;
    end;

    if (FHighPrice > FLowPrice) then
    begin
      FWillR := (ATiket.Last - FLowPrice)/(FHighPrice - FLowPrice);
      FWillR := RoundTo(100 * FWillR,-2);
    end;

  end
  else
    FWillR := -1;
end;

function TIndicator.GetBuySell: Char;
begin
  Result := 'N';
  if (FWillR > 0) and (FWillR < 50) then
    Result := 'B'
  else if FWillR >= 50 then
    Result := 'S';
end;

function TIndicator.ToString: string;
var
  xS: String;
begin
  xS :=
    'Count = ' + FTikets.Count.ToString + '; ' +
    'R% = ' + FWillR.ToString + '; ' +
    'HighPrice = ' + FHighPrice.ToString + '; ' +
    'LowPrice = ' + FLowPrice.ToString + '; ' +
    'BuySell = ' + BuySell;
  Result := xS;
end;

{ TPosition.TTrd }

constructor TPosition.TTrd.Create;
begin
  Default;
end;

destructor TPosition.TTrd.Destroy;
begin

  inherited;
end;

procedure TPosition.TTrd.Default;
begin
  Date := 0;
  Time := 0;
  Price := 0;
  Qty  := 0;
  BuySell := #0;
end;

{ TPosition }

constructor TPosition.Create;
begin
  FTrds := TTrdList.Create;
  FHistory := TTrdList.Create;
end;

destructor TPosition.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FTrds);
  inherited;
end;

procedure TPosition.Clear;
begin
  FQty   := 0;
  FPrice := 0;
  FValue := 0;
  FTrds.Clear;
end;

procedure TPosition.SetCalc;
var
  i, iCount: Integer;
  xT: TPosition.TTrd;
begin
  FQty   := 0;
  FValue := 0;
  iCount := FTrds.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xT := FTrds[i];
      FQty := FQty + xT.Qty;
      FValue := FValue + xT.Price * xT.Qty;
    end;
  if FQty > 0 then
    FPrice := FValue/FQty
  else
    FPrice := 0;
end;

function TPosition.GetBuySell: Char;
begin
  if FTrds.Count > 0 then
    Result := FTrds[0].BuySell
  else
    Result := 'N';
end;

procedure TPosition.Add(ATiket: TTiket; ABuySell: Char; AQty: Double);

  function _GetRoundQty(const AValue: Double): Double;
  begin
    Result := Round(100 * AValue)/100;
  end;

var
  xQty: Double;
  xT: TPosition.TTrd;
begin

  // Фиксируем сделку для истории
  xT := TPosition.TTrd.Create;
  xT.Date    := ATiket.Date;
  xT.Time    := ATiket.Time;
  xT.Price   := ATiket.Last;
  xT.Qty     := AQty;
  xT.BuySell := ABuySell;
  FHistory.Add(xT);

  // Для определения состояние позиции
  if (Self.BuySell = ABuySell) or (Self.BuySell = 'N') then
  begin
    xT := TPosition.TTrd.Create;
    xT.Date    := ATiket.Date;
    xT.Time    := ATiket.Time;
    xT.Price   := ATiket.Last;
    xT.Qty     := AQty;
    xT.BuySell := ABuySell;
    FTrds.Add(xT);
  end
  else
  begin
    xQty := AQty;
    while xQty > 0 do
    begin
      xT := FTrds[0];
      if xT.Qty > xQty then
      begin
        xT.Qty := _GetRoundQty(xT.Qty - xQty);
        xQty := 0;
      end
      else
      begin
        xQty := _GetRoundQty(xQty - xT.Qty);
        FTrds.Delete(0);
      end;
    end;
  end;
  SetCalc;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FStatus := TStatusTrade.stNull;
  FIndicator := TIndicator.Create;
  FCurrentPosition := nil;
  FPositions := TPositionList.Create;
end;

destructor TTrade.Destroy;
begin
  FreeAndNil(FPositions);
  FreeAndNil(FIndicator);
  inherited;
end;

procedure TTrade.SetTitketLast(ATiket: TTiket);
begin
{$IFDEF DEBUG}
  SetLog('TTrade.SetTitketLast:');
  SetLog(' >> ' + ATiket.ToStr);
{$ENDIF}
  FIndicator.SetTitketLast(ATiket);
{$IFDEF DEBUG}
  SetLog(' >> ' + FIndicator.ToString);
{$ENDIF}

end;

end.
