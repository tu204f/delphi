unit Lb.TradeMan;

interface

{$IFDEF DEBUG}
  {$DEFINE DBG_O_POS}
  {$DEFINE DBG_C_POS}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.SysUtils.Candel;

type
(******************************************************************************)
(* ������ �������� ��������� �������� ��������,                              *)
(* � �������� ���������� ��������                                             *)
(******************************************************************************)

  ///<summary>��������� �������</summary>
  TTradeMan = class(TObject)
  public type

    ///<summary>�������� ������ ��� ������</summary>
    TCustomTrade = class(TObject)
    private
      FTradeMan: TTradeMan;
    public
      constructor Create(const ATradeMan: TTradeMan); virtual;
      destructor Destroy; override;
      procedure UpDataPrice(const APrice: Double); virtual; abstract;
    end;

    ///<summary>���������� �������</summary>
    TPosition = class(TCustomTrade)
    private
      FActive      : Boolean; // �������� ������
      FBuySell     : Char;    // ����������� �������
      FOpenPrice   : Double;  // ���� �������
      FQuantity    : Double;  // ���������� ���������� �����
      FTrailingStop: Double;  // ��� ������� �������� ������ � ���������
      FStopPrice   : Double;  // ���� �������� �������
    public
      constructor Create(const ATradeMan: TTradeMan); override;
      procedure Start(AOpenPrice: Double; AQuantity: Double; ABuySell: Char; ATrailingStop: Double);
      procedure Stop;
      procedure UpDataPrice(const APrice: Double); override;
      property OpenPrice: Double read FOpenPrice;
      property Quantity: Double read FQuantity;
      property BuySell: Char read FBuySell;
      property TrailingStop: Double read FTrailingStop;
      property StopPrice: Double read FStopPrice;
      property Active: Boolean read FActive;
    end;

    ///<summary>������ ��� �������� �������</summary>
    ///<remarks>��������� �������, ��������</remarks>
    TOpen = class(TCustomTrade)
    public const
      PRICE_UP   = 0; // ���� ������
      PRICE_DOWN = 1; // ���� ������
    private
      FBuySell: Char;
      FPrice: Double;
      FMaxPrice, FMinPrice: Double;
    protected
      procedure DoOpenPosition(const APrice: Double; const AStatus: Integer);
    public
      constructor Create(const ATradeMan: TTradeMan); override;
      destructor Destroy; override;
      procedure UpDataPrice(const APrice: Double); override;
      property MaxPrice: Double read FMaxPrice write FMaxPrice;
      property MinPrice: Double read FMinPrice write FMinPrice;
    public
      property BuySell: Char read FBuySell;
      property Price: Double read FPrice;
    end;

    ///<summary>�������� �������</summary>
    TClose = class(TCustomTrade)
    private
      FPrice: Double;
      FOpen: TOpen;
      FPosition: TPosition;
      function GetProfitPrice(const APrice: Double): Double;
    protected
      procedure DoClosePosition(const APrice: Double);
    public
      constructor Create(const ATradeMan: TTradeMan); override;
      destructor Destroy; override;
      procedure UpDataPrice(const APrice: Double); override;
      procedure CandelNew(const APrice: Double);
      ///<summary>������� - ���������</summary>
      property Open: TOpen read FOpen write FOpen;
      ///<summary>�������</summary>
      property Position: TPosition read FPosition write FPosition;
      property Price: Double read FPrice;
    end;

  public type
    TType�rossing    = (tcUp, tcDonw);
    TTypeTrade       = (ttTrend, ttContrTrend);
    TTypeTradeStatus = (tsOpen,  tsWait, tsClose);
  private
    FLeverage      : Integer; // ������ ���������� �����
    FCapital       : Double;
    FDeposit       : Double;
    FLimitDeposit  : Double;
    FTrailingStop  : Double;
    FPeriod        : Integer;
    FMinStepQuntity: Double;
    FTypeTrade     : TTypeTrade;
    function GetMaxPrice: Double;
    function GetMinPrice: Double;
  private {����������}
    FPlusCount: Integer;
    FMinusCount: Integer;
    FMinusProfit: Double;
  private
    FOpen    : TOpen;
    FClose   : TClose;
    FPosition: TPosition;
    procedure SetDeposit(const Value: Double);
  protected
    FOnOpenPosition: TNotifyEvent;
    FOnClosePosition: TNotifyEvent;
    procedure DoOpenPosition;
    procedure DoClosePosition;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>��� ���������� ���������� ��������</summary>
    procedure SetInputBlock(const ACandels: TCandels);

    ///<summary>��������� ������� �������� ��� �������� �������</summary>
    procedure SetPriceLast(const APrice: Double); overload;

    ///<summary>�������� ����� �����</summary>
    procedure CandelNew(const APrice: Double);

    ///<summary>����������� ��� ����������</summary>
    property MinStepQuntity: Double read FMinStepQuntity write FMinStepQuntity;
  public {����������� ���������}
    ///<summary>������ ��������</summary>
    ///<remarks>��������� ������ ������� �� ���� ������ ��������</remarks>
    property Deposit: Double read FDeposit write SetDeposit;
    ///<summary>������ ��� ������, �������� �� ��������� 10</summary>
    property Period: Integer read FPeriod write FPeriod;
    ///<summary>�������������� ��������<summary>
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    ///<summary>�������� � �����, ���������� ����</summary>
    property TrailingStop: Double read FTrailingStop write FTrailingStop;
    ///<summary>������ ���������� �����</summary>
    property Leverage: Integer read FLeverage write FLeverage;
    ///<summary>���� ������� �������</summary>
    function IsPosition: Boolean;
  public
    ///<summary>�������</summary>
    property Capital: Double read FCapital;
    ///<summary>������������ ����, �� ������ Period</summary>
    property MaxPrice: Double read GetMaxPrice;
    ///<summary>����������� ����, �� ������ Period</summary>
    property MinPrice: Double read GetMinPrice;
    ///<summary>������ �������</summary>
    property Position: TPosition read FPosition;
  public
    property PlusCount: Integer read FPlusCount;
    property MinusCount: Integer read FMinusCount;
    property MinusProfit: Double read FMinusProfit;
  public
    property OnOpenPosition: TNotifyEvent write FOnOpenPosition;
    property OnClosePosition: TNotifyEvent write FOnClosePosition;
  end;

implementation

{ TTradeMan.TCustomTrade }

constructor TTradeMan.TCustomTrade.Create(const ATradeMan: TTradeMan);
begin
  FTradeMan := ATradeMan;
end;

destructor TTradeMan.TCustomTrade.Destroy;
begin

  inherited;
end;

{ TTradeMan.TPosition }

constructor TTradeMan.TPosition.Create(const ATradeMan: TTradeMan);
begin
  inherited Create(ATradeMan);
  FActive := False;
end;

procedure TTradeMan.TPosition.Start(AOpenPrice, AQuantity: Double; ABuySell: Char; ATrailingStop: Double);
begin
  FActive       := True;
  FBuySell      := ABuySell;
  FOpenPrice    := AOpenPrice;
  FQuantity     := AQuantity;
  FTrailingStop := ATrailingStop;
end;

procedure TTradeMan.TPosition.Stop;
begin
  FActive := False;
end;

procedure TTradeMan.TPosition.UpDataPrice(const APrice: Double);

  procedure _StopPriceNull(AStopPrice: Double);
  begin
    if FStopPrice = 0 then
      FStopPrice := AStopPrice;
  end;

var
  xStopPrice: Double;
begin


  case FBuySell of
    'B': begin
      xStopPrice := APrice - FTrailingStop;
      _StopPriceNull(xStopPrice);
      if xStopPrice > FStopPrice then
        FStopPrice := xStopPrice;
    end;
    'S': begin
      xStopPrice := APrice + FTrailingStop;
      _StopPriceNull(xStopPrice);
      if xStopPrice < FStopPrice then
        FStopPrice := xStopPrice;
    end;
  end;
end;


{ TTradeMan.TOpen }

constructor TTradeMan.TOpen.Create(const ATradeMan: TTradeMan);
begin
  inherited Create(ATradeMan);
  FMaxPrice := 0;
  FMinPrice := 0;
  FBuySell  := #0;
end;

destructor TTradeMan.TOpen.Destroy;
begin

  inherited;
end;

procedure TTradeMan.TOpen.UpDataPrice(const APrice: Double);
begin
  if APrice >= FMaxPrice then
  begin
    DoOpenPosition(APrice,PRICE_UP);
  end else if APrice <= FMinPrice then
  begin
    DoOpenPosition(APrice, PRICE_DOWN);
  end;
end;

procedure TTradeMan.TOpen.DoOpenPosition(const APrice: Double; const AStatus: Integer);
begin
  FPrice := 0;
  case AStatus of
    PRICE_UP  : FBuySell := 'B';
    PRICE_DOWN: FBuySell := 'S';
  end;

  if CharInSet(FBuySell,['B','S']) then
    FPrice := APrice;

  if Assigned(FTradeMan) then
    FTradeMan.DoOpenPosition;
end;

{ TTradeMan.TClose }

constructor TTradeMan.TClose.Create(const ATradeMan: TTradeMan);
begin
  inherited Create(ATradeMan);
  FOpen := nil;
end;

destructor TTradeMan.TClose.Destroy;
begin

  inherited;
end;

function TTradeMan.TClose.GetProfitPrice(const APrice: Double): Double;
begin
  Result := 0;
  case FOpen.BuySell of
    'B': Result := APrice - FOpen.Price;
    'S': Result := FOpen.Price - APrice;
  end;
end;

procedure TTradeMan.TClose.UpDataPrice(const APrice: Double);
begin
  if not Assigned(FOpen) then
    Exit;

  if not Assigned(FPosition) then
    Exit;

  if FPosition.StopPrice <= 0 then
    Exit;

  case FOpen.BuySell of
    'B': if (FPosition.StopPrice > APrice) then
    begin
      DoClosePosition(APrice);
    end;
    'S': if (FPosition.StopPrice < APrice) then
    begin
      DoClosePosition(APrice);
    end;
  end;
end;

procedure TTradeMan.TClose.CandelNew(const APrice: Double);
var
  xProfit: Double;
begin
  if Assigned(FOpen) then
  begin
    xProfit := GetProfitPrice(APrice);
    if (xProfit > 0) then
      DoClosePosition(APrice);
  end
  else
    raise Exception.Create('Error Message. ������� ������ ���� ������');
end;

procedure TTradeMan.TClose.DoClosePosition(const APrice: Double);
begin
  FPrice := APrice;
  FTradeMan.DoClosePosition;
end;

{ TTradeMan }

constructor TTradeMan.Create;
begin
  FLeverage       := 1;
  FLimitDeposit   := 0;
  FDeposit        := 0;
  FCapital        := 0;
  FTypeTrade      := TTypeTrade.ttTrend;
  FPeriod         := 10;
  FPosition       := nil;
  FMinStepQuntity := 0.001;

  // ����������
  FPlusCount := 0;
  FMinusCount := 0;
  FMinusProfit := 0;

  FPosition := TPosition.Create(Self);

  FOpen  := TOpen.Create(Self);

  FClose := TClose.Create(Self);
  FClose.Open := FOpen;
  FClose.Position := FPosition;
end;

destructor TTradeMan.Destroy;
begin
  if Assigned(FClose) then
    FreeAndNil(FClose);
  if Assigned(FOpen) then
    FreeAndNil(FOpen);
  if Assigned(FPosition) then
    FreeAndNil(FPosition);
  inherited;
end;


procedure TTradeMan.SetDeposit(const Value: Double);
begin
  FDeposit := Value;
  FLimitDeposit := Value;
end;

procedure TTradeMan.SetInputBlock(const ACandels: TCandels);
(******************************************************************************)
(* ��� ������� ���������� ��������, ��������� ��� �� ���������                *)
(******************************************************************************)
  procedure _LimitValueMaxAndMinBlock(
    ACandels: TCandels;
    APeriod: Integer;
    var AMaxValue, AMinValue: Double
  );
  var
    xCandel: TCandel;
    xPeriod: Integer;
    i, iCount: Integer;
  begin
    xPeriod := APeriod;
    AMaxValue := 0;
    AMinValue := 0;
    iCount := ACandels.Count;
    if iCount <= APeriod then
      Exit;

    if xPeriod > 0 then
    begin
      xCandel := ACandels[0];
      AMaxValue := xCandel.High;
      AMinValue := xCandel.Low;
      for i := 1 to xPeriod - 1 do
      begin
        xCandel := ACandels[i];
        if AMaxValue < xCandel.High then
          AMaxValue := xCandel.High;
        if AMinValue > xCandel.Low then
          AMinValue := xCandel.Low;
      end;
    end;
  end;

var
  xMaxPrice, xMinPrice: Double;
begin
  _LimitValueMaxAndMinBlock(
    ACandels,
    FPeriod,
    xMaxPrice,
    xMinPrice
  );
  FOpen.MaxPrice := xMaxPrice;
  FOpen.MinPrice := xMinPrice;
end;

procedure TTradeMan.SetPriceLast(const APrice: Double);
begin
  if (FOpen.MaxPrice = 0) or (FOpen.MinPrice = 0) then
    Exit;
  if IsPosition then
  begin
    FPosition.UpDataPrice(APrice);
    FClose.UpDataPrice(APrice);
  end
  else
  begin
    FOpen.UpDataPrice(APrice);
  end;
end;

procedure TTradeMan.CandelNew(const APrice: Double);
begin
  if IsPosition then
    FClose.CandelNew(APrice);
end;

procedure TTradeMan.DoOpenPosition;

  function _FormatQuantity(const AQuantity: Double): Double;
  var
    xCountTiket: Integer;
  begin
    xCountTiket := Trunc(AQuantity/FMinStepQuntity);
    Result := xCountTiket * FMinStepQuntity;
  end;

var
  xQuantity: Double;
begin

  if IsPosition then
    raise Exception.Create('Error Message: �� ����� ���� ������');

  if FLeverage <= 0 then
    raise Exception.Create('Error Message: �� ����� ���� ������');

  // ����������, ����� �������� �������
  // � ����������� �� ������������ ��������
  xQuantity := (FLeverage * FDeposit)/FOpen.Price;
  xQuantity := _FormatQuantity(xQuantity);

  // ������ �� ����� �������� �������
  if xQuantity = 0 then
    Exit;

  if not FPosition.Active then
  begin
    FPosition.Start(
      FOpen.Price,
      xQuantity,
      FOpen.BuySell,
      FTrailingStop
    );
    FPosition.UpDataPrice(FOpen.Price);
  end;

  if Assigned(FOnOpenPosition) then
    FOnOpenPosition(Self);
end;

function TTradeMan.GetMaxPrice: Double;
begin
  Result := FOpen.MaxPrice;
end;

function TTradeMan.GetMinPrice: Double;
begin
  Result := FOpen.MinPrice;
end;

procedure TTradeMan.DoClosePosition;
var
  xCapital: Double;
begin

  if not IsPosition then
    raise Exception.Create('Error Message: �� ����� ���� ������');

  // ��������� �������
  xCapital := 0;
  case FPosition.BuySell of
    'B': xCapital := FPosition.Quantity * (FClose.Price  - FPosition.OpenPrice);
    'S': xCapital := FPosition.Quantity * (FPosition.OpenPrice - FClose.Price);
  end;

  // ����������
  if xCapital > 0 then
    Inc(FPlusCount)
  else
    Inc(FMinusCount);
  if FMinusProfit > xCapital then
    FMinusProfit := xCapital;

  // ���� ����� ������ ������������ �������� �� �����
  FDeposit := FDeposit + xCapital;
  if FDeposit > FLimitDeposit then
    FDeposit := FLimitDeposit;

  FCapital := FCapital + xCapital;

  FPosition.Stop;

  if Assigned(FOnClosePosition) then
    FOnClosePosition(Self);
end;

function TTradeMan.IsPosition: Boolean;
begin
  Result := FPosition.Active;
end;

end.
