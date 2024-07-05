unit Lb.SysUtils;

interface

{$IFDEF DEBUG}
//  {$DEFINE DB_LOG}
{$ENDIF}

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
  /// ��� ���������
  ///</summary>
  TType�riterion = (tcOpen, tcClose);

  ///<summary>
  /// ��� �����������
  ///</summary>
  TTypeSide = (tsBuy,tsSell);

  ///<summary>
  /// ��� �������
  ///</summary>
  TTypePosition = (tpNull, tpOpen, tpClose);

type
  TTrader = class;
  TPositionTrade = class;

  ///<summary>
  /// ������� �������� �� �������� �������
  ///</summary>
  TEventOperationTrade = procedure(
    ATrader: TTrader;               // ��������� �� ��������
    AType�riterion: TType�riterion; // ����������� �������
    ATypeSide: TTypeSide;           // ����������� ������
    AQty: Double                    // ����������
  ) of object;

  ///<summary>
  /// �������� ������� �������
  ///</summary>
  T�riterion = class(TObject)
  public
    RSI: Double;                   // �������� ����������
    ReActiveRSI: Double;           // �������� ����������, ��� ��������� ��������
    Qty: Double;                   // ����������
    IsActive: Boolean;             // ��������� ��������
    Type�riterion: TType�riterion; // ���� ��������
  public
    constructor Create; virtual;
    procedure Default;
  end;
  T�riterionList = TObjectList<T�riterion>;

  ///<summary>�������</summary>
  TTrader = class(TObject)
  private
    FID: Integer;
    FSide: TTypeSide;
    F�riterions: T�riterionList;
  private
    FOnOperationTrade: TEventOperationTrade;
  protected
    procedure DoEventOperationTrade(AType�riterion: TType�riterion; ATypeSide: TTypeSide; AQty: Double);  virtual;
    procedure DoOperationTrade(A�riterion: T�riterion);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpDate(const AValueRSI: Double);
    ///<summary>���������� ���������</summary>
    procedure Create�riterion(const ACount: Integer = 1);
    ///<summary>������ ���������</summary>
    property �riterions: T�riterionList read F�riterions;
    ///<summary>����������� ������</summary>
    property Side: TTypeSide read FSide write FSide;
    ///<summary>����� ��������</summary>
    property ID: Integer read FID write FID;
    ///<summary>������� ��������</summary>
    property OnOperationTrade: TEventOperationTrade write FOnOperationTrade;
  end;
  TTraderList = TObjectList<TTrader>;

  ///<summary>������ - ��������</summary>
  TTrade = record
    Date : TDateTime;
    Time : TDateTime;
    Price: Double;
    Qty  : Double;
    Side : TTypeSide;
  private
    function GetValue: Double;
  public
    property Value: Double read GetValue;
  end;
  TTradeList = TList<TTrade>;

  ///<summary>������� ��������</summary>
  TPositionTrade = class(TObject)
  private
    FTrader: TTrader;
    FCloseTrade: TTrade;
    FTrades: TTradeList;
    FTypePosition: TTypePosition;
    function GetSide: TTypeSide;
    function GetQty: Double;
    function GetValue: Double;
    function GetPrice: Double;
    function GetProfit: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>�������� �������</summary>
    procedure SetOpenTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
    ///<summary>�������� �������</summary>
    procedure SetCloseTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
    ///<summary>��� �������</summary>
    property TypePosition: TTypePosition read FTypePosition;
    ///<summary>��� �������� �������� ��������</summary>
    property Trader: TTrader read FTrader write FTrader;
    ///<summary>������ ����������� ������</summary>
    property Trades: TTradeList read FTrades;
    ///<summary>������ �������� �������</summary>
    property CloseTrade: TTrade read FCloseTrade;
  public
    property Price: Double read GetPrice;
    property Value: Double read GetValue;
    property Qty  : Double read GetQty;
    property Side : TTypeSide read GetSide;
    property Profit: Double read GetProfit;
  end;
  TPositionTradeList = TObjectList<TPositionTrade>;

  ///<summary>������� �������</summary>
  TWorkTrader = class(TTrader)
  private
    FCandel: TCandel;
    FActivePosition: TPositionTrade;
    FPositionTrades: TPositionTradeList;
    function GetProfit: Double;
    function GetPositivCountProfit: Double;
  protected
    procedure DoEventOperationTrade(AType�riterion: TType�riterion; ASide: TTypeSide; AQty: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpDateCandel(ACandel: TCandel; AValueRSI: Double);
    property ActivePosition: TPositionTrade read FActivePosition;
    property PositionTrades: TPositionTradeList read FPositionTrades;
    ///<summary>���������� ������� �� �������� ��������</summary>
    property Profit: Double read GetProfit;
  public
    ///<summary>���������� ���������� �������</summary>
    property PositivCountProfit: Double read GetPositivCountProfit;
  end;
  TWorkTraderList = TObjectList<TWorkTrader>;


function GetTypeSideToStr(const ASide: TTypeSide): String;

implementation

uses
  UnitMainForm;

function GetTypeSideToStr(const ASide: TTypeSide): String;
begin
  Result := '';
  case ASide of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  end;
end;

function GetUpRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam < AValue;
end;

function GetDownRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam > AValue;
end;

{ T�riterion }

constructor T�riterion.Create;
begin
  Default;
end;

procedure T�riterion.Default;
begin
  RSI := 0;
  ReActiveRSI := 0;
  Qty := 0;
  IsActive := False;
end;

{ TTrader }

constructor TTrader.Create;
begin
  F�riterions := T�riterionList.Create;
end;

destructor TTrader.Destroy;
begin
  F�riterions.Clear;
  FreeAndNil(F�riterions);
  inherited;
end;

procedure TTrader.DoEventOperationTrade(AType�riterion: TType�riterion; ATypeSide: TTypeSide; AQty: Double);
begin
  if Assigned(FOnOperationTrade) then
    FOnOperationTrade(Self,AType�riterion,FSide,AQty);
end;

procedure TTrader.DoOperationTrade(A�riterion: T�riterion);

  function _CrossSide(ASide: TTypeSide): TTypeSide;
  begin
    if ASide = tsBuy then
      Result := tsSell
    else
      Result := tsBuy;
  end;

begin
  case A�riterion.Type�riterion of
    tcOpen: begin
      DoEventOperationTrade(A�riterion.Type�riterion,FSide,A�riterion.Qty);
    end;
    tcClose: begin
      DoEventOperationTrade(A�riterion.Type�riterion,_CrossSide(FSide),A�riterion.Qty);
    end;
  end;
end;

procedure TTrader.Create�riterion(const ACount: Integer);
var
  x�riterion: T�riterion;
  i: Integer;
begin
  // ��������� �������� �� ��������
  F�riterions.Clear;
  for i := 0 to ACount do
  begin
    x�riterion := T�riterion.Create;
    x�riterion.Default;
    if i = (ACount - 1) then
      x�riterion.Type�riterion := TType�riterion.tcOpen
    else
      x�riterion.Type�riterion := TType�riterion.tcClose;
    F�riterions.Add(x�riterion);
  end;
end;

procedure TTrader.SetUpDate(const AValueRSI: Double);

  procedure _Active�riterion(A�riterion: T�riterion);
  begin
    // ���������� ��������� ��������
    case FSide of
      tsBuy: begin
        case A�riterion.Type�riterion of
          tcOpen: if GetDownRSI(AValueRSI,A�riterion.RSI) then
          begin
            DoOperationTrade(A�riterion);
            A�riterion.IsActive := False;
          end;
          tcClose: if GetUpRSI(AValueRSI,A�riterion.RSI) then
          begin
            DoOperationTrade(A�riterion);
            A�riterion.IsActive := False;
          end;
        end;
      end;
      tsSell: begin
        case A�riterion.Type�riterion of
          tcOpen: if GetUpRSI(AValueRSI, A�riterion.RSI) then
          begin
            DoOperationTrade(A�riterion);
            A�riterion.IsActive := False;
          end;
          tcClose: if GetDownRSI(AValueRSI, A�riterion.RSI) then
          begin
            DoOperationTrade(A�riterion);
            A�riterion.IsActive := False;
          end;
        end;
      end;
    end;
  end;

  procedure _ReActive�riterion(A�riterion: T�riterion);
  begin
    // ���������� ����������� ��������
    case FSide of
      tsBuy: begin
        case A�riterion.Type�riterion of
          tcOpen: if GetUpRSI(AValueRSI, A�riterion.ReActiveRSI) then
            A�riterion.IsActive := True;
          tcClose: if GetDownRSI(AValueRSI, A�riterion.ReActiveRSI) then
            A�riterion.IsActive := True;
        end;
      end;
      tsSell: begin
        case A�riterion.Type�riterion of
          tcOpen: if GetDownRSI(AValueRSI, A�riterion.ReActiveRSI) then
            A�riterion.IsActive := True;
          tcClose: if A�riterion.ReActiveRSI > AValueRSI then
            A�riterion.IsActive := True;
        end;
      end;
    end;
  end;

var
  x�riterion: T�riterion;
  i, iCount: Integer;
begin
  iCount := F�riterions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      x�riterion := F�riterions[i];
      if x�riterion.IsActive then
        _Active�riterion(x�riterion)
      else
        _ReActive�riterion(x�riterion);
    end;
end;

{ TTrade }

function TTrade.GetValue: Double;
begin
  Result := Price * Qty;
end;

{ TPositionTrade }

constructor TPositionTrade.Create;
begin
  FTrades := TTradeList.Create;
  FTypePosition := TTypePosition.tpNull;
end;

destructor TPositionTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

function TPositionTrade.GetSide: TTypeSide;
begin
  if FTrades.Count > 0 then
    Result := FTrades[0].Side
  else
    raise Exception.Create('Error Message: ��� ����������� ���������� ����������� �������');
end;

function TPositionTrade.GetPrice: Double;
begin
  Result := Self.Value/Self.Qty;
end;


function TPositionTrade.GetQty: Double;
var
  xQty: Double;
begin
  if FTrades.Count > 0 then
  begin
    xQty := 0;
    for var xTrade in FTrades do
      xQty := xQty + xTrade.Qty;
    Result := xQty;
  end
  else
    raise Exception.Create('Error Message: ��� ����������� ���������� ������ �������');
end;

function TPositionTrade.GetValue: Double;
var
  xValue: Double;
begin
  if FTrades.Count > 0 then
  begin
    xValue := 0;
    for var xTrade in FTrades do
      xValue := xValue + xTrade.Value;
    Result := xValue;
  end
  else
    raise Exception.Create('Error Message: ��� ����������� ���������� ����� �������');
end;

function TPositionTrade.GetProfit: Double;
begin
  Result := 0;
  if FTypePosition = TTypePosition.tpClose then
  begin
    case Side of
      tsBuy: Result := FCloseTrade.Value - Self.Value;
      tsSell: Result := Self.Value - FCloseTrade.Value;
    end;
  end;
end;

procedure TPositionTrade.SetOpenTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);

  function _GetQty: Double;
  begin
    Result := 0;
    if FTrades.Count > 0 then
      Result := FTrades[FTrades.Count - 1].Qty;
  end;

var
  xTrade: TTrade;
begin
  if FTrades.Count < 1 then
  begin
    with xTrade do
    begin
      Date := ADate;
      Time := ATime;
      Price:= APrice;
      Qty  := _GetQty + AQty;
      Side := ASide;
    end;
    FTrades.Add(xTrade);
    FTypePosition := TTypePosition.tpOpen;
  end;
end;

procedure TPositionTrade.SetCloseTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
begin
  if FTypePosition = TTypePosition.tpOpen then
  begin
    with FCloseTrade do
    begin
      Date := ADate;
      Time := ATime;
      Price:= APrice;
      Qty  := AQty;
      Side := ASide;
    end;
    FTypePosition := TTypePosition.tpClose;
  end;
end;

{ TWorkTrader }

constructor TWorkTrader.Create;
begin
  inherited;
  FActivePosition := nil;
  FPositionTrades := TPositionTradeList.Create;
end;

destructor TWorkTrader.Destroy;
begin
  FPositionTrades.Clear;
  FreeAndNil(FPositionTrades);
  inherited;
end;

procedure TWorkTrader.DoEventOperationTrade(AType�riterion: TType�riterion;
  ASide: TTypeSide; AQty: Double);
begin
  inherited;
  if Assigned(FActivePosition) then
  begin
    case AType�riterion of
      tcOpen: FActivePosition.SetOpenTrade(FCandel.Date, FCandel.Time, FCandel.Close, AQty, ASide);
      tcClose: begin
        FActivePosition.SetCloseTrade(FCandel.Date, FCandel.Time, FCandel.Close, FActivePosition.Qty, ASide);
        FActivePosition := nil;
      end;
    end;
  end
  else
    if AType�riterion = tcOpen then
    begin
      FActivePosition := TPositionTrade.Create;
      FActivePosition.Trader := TTrader(Self);
      FActivePosition.SetOpenTrade(FCandel.Date, FCandel.Time, FCandel.Close, AQty, ASide);
      FPositionTrades.Add(FActivePosition);
    end;
end;

procedure TWorkTrader.SetUpDateCandel(ACandel: TCandel; AValueRSI: Double);
begin
  {$IFDEF DB_LOG}
  Log('TWorkTrader.SetUpDateCandel:');
  {$ENDIF}
  FCandel := ACandel;
  SetUpDate(AValueRSI);
end;

function TWorkTrader.GetProfit: Double;
var
  xSum: Double;
  i, iCount: Integer;
  xPositionTrade: TPositionTrade;
begin
  xSum := 0;
  iCount := FPositionTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPositionTrade := FPositionTrades[i];
      xSum := xSum + xPositionTrade.Profit;
    end;
  Result := xSum;
end;


function TWorkTrader.GetPositivCountProfit: Double;
var
  xCount: Integer;
  i, iCount: Integer;
  xPositionTrade: TPositionTrade;
begin
  xCount := 0;
  iCount := FPositionTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPositionTrade := FPositionTrades[i];
      if xPositionTrade.Profit > 0 then
        xCount := xCount + 1;
    end;
  Result := xCount;
end;

end.
