(******************************************************************************)
(* ������ ������ �������� ��������                                            *)
(******************************************************************************)
unit Lb.Journal.Trading.V2;

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
  TJournalPosition = class;
  TJournalManager = class;

  ///<summary>����� ������� ��������� ����� ������</summary>
  TEventOnNewPositionTrade = procedure(ASander: TObject; ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList) of object;

  ///<summary>������</summary>
  TJournalTrade = class(TObject)
  private
    FTime: TDateTime;
    FPrice: Double;
    FQty: Double;
    FSide: TTypeBuySell;
    FCandels: TCandelList;
    function GetValue: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Time: TDateTime read FTime write FTime;
    property Price: Double read FPrice write FPrice;
    property Qty: Double read FQty write FQty;
    property Side: TTypeBuySell read FSide write FSide;
    property Value: Double read GetValue;
    property Candels: TCandelList read FCandels;
  end;

  ///<summary>������ ������</summary>
  TJournalTradeList = TObjectList<TJournalTrade>;

  ///<summary>������� ������� �������</summary>
  ///<remarks>
  /// ������� ��������� �������� ���� ���� ���� ���� �� ������� �������
  ///</remarks>
  TJournalPosition = class(TObject)
  private
    FPrice: Double;
    FQty: Double;
    FTrades: TJournalTradeList;
    function GetIsActive: Boolean;
    function GetQty: Double;
    function GetSide: TTypeBuySell;
  private
    FProfit: Double;
    FMaxProfit: Double;
    FMinProfit: Double;
    FProfits: TDoubleList;
  private
    FManager: TJournalManager;
    FOnNewPositionTrade: TEventOnNewPositionTrade;
  protected
    ///<summary>�������� ���������� �� ������</summary>
    procedure SetUpDate;
    procedure DoNewPositionTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>������ ������</summary>
    procedure OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

    ///<summary>�������� �������</summary>
    procedure CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);

    ///<summary>��������� ������ ��������</summary>
    function GetProfit(const APrice: Double = 0): Double;

    ///<summary>������ ������</summary>
    property Trades: TJournalTradeList read FTrades;
    property IsActive: Boolean read GetIsActive;
    property Price: Double read FPrice;
    property Qty: Double read GetQty;
    property Side: TTypeBuySell read GetSide;
    property OnNewPositionTrade: TEventOnNewPositionTrade write FOnNewPositionTrade;
    property Manager: TJournalManager read FManager write FManager;
  end;

  ///<summary>������ �������</summary>
  TJournalPositionList = TObjectList<TJournalPosition>;

  ///<summary>�������� �������</summary>
  TJournalManager = class(TObject)
  private
    FPositions: TJournalPositionList;
    function GetCurrentPosition: TJournalPosition;
    function GetIsCurrentPosition: Boolean;
  protected
    procedure DoNewTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>������ ������</summary>
    procedure OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

    ///<summary>�������� �������</summary>
    procedure CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);

    ///<summary>��������� ������������� �������</summary>
    property IsCurrentPosition: Boolean read GetIsCurrentPosition;
    ///<summary>������� �������</summary>
    property CurrentPosition: TJournalPosition read GetCurrentPosition;
    ///<summary>������ �������</summary>
    property Positions: TJournalPositionList read FPositions;
  end;

implementation

{ TJournalTrade }

constructor TJournalTrade.Create;
begin
  FCandels := TCandelList.Create;
end;

destructor TJournalTrade.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

function TJournalTrade.GetValue: Double;
begin
  Result := FPrice * FQty;
end;

{ TJournalPosition }

constructor TJournalPosition.Create;
begin
  FManager := nil;
  FTrades := TJournalTradeList.Create;

  FMaxProfit := 0;
  FMinProfit := 0;
  FProfits:= TDoubleList.Create;
end;

destructor TJournalPosition.Destroy;
begin
  FreeAndNil(FProfits);
  FreeAndNil(FTrades);
  inherited;
end;

procedure TJournalPosition.DoNewPositionTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
begin
  if Assigned(FOnNewPositionTrade) then
    FOnNewPositionTrade(Self, ATime, APrice, AQty, ASide, ACandels);
  if Assigned(FManager) then
    FManager.DoNewTrade(ATime, APrice, AQty, ASide, ACandels)
end;

procedure TJournalPosition.OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

  procedure _CreateJournalTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  var
    xTrade: TJournalTrade;
  begin
    xTrade := TJournalTrade.Create;
    xTrade.Time := ATime;
    xTrade.Price := APrice;
    xTrade.Qty := AQty;
    xTrade.Side := ASide;
    xTrade.Candels.CopyCandels(ACandels);

    FTrades.Add(xTrade);
    SetUpDate;
  end;

begin
  if Self.Qty = 0 then
  begin
    _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels)
  end
  else
  begin
    if Self.Side = ASide then
    begin
      _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
    end
    else
    begin
      if Self.Qty >= AQty then
      begin
        _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
      end
      else
      begin
        // ��������� ����� �������
        var xQty := AQty - Self.Qty;
        _CreateJournalTrade(ATime, APrice, Self.Qty, ASide, ACandels);
        DoNewPositionTrade(ATime, APrice, xQty, ASide, ACandels)
      end
    end;
  end;
end;

procedure TJournalPosition.SetUpDate;

  procedure _CalcQty;
  begin
    FQty := 0;
    for var xTrade in FTrades do
    begin
      case xTrade.Side of
        tsBuy: FQty := FQty + xTrade.Qty;
        tsSell: FQty := FQty - xTrade.Qty;
      end;
    end;
    FQty := GetRound(FQty);
  end;

  procedure _CalcPrice;
  var
    xSumValue: Double;
    xQty: Double;
    xSide: TTypeBuySell;
    i, iCount: Integer;
    xJournalTrade: TJournalTrade;
  begin
    FPrice := 0;
    if IsActive then
    begin
      iCount := FTrades.Count;
      if iCount > 0 then
      begin
        xSumValue := 0;
        xQty := FQty;
        xSide := GetSide;
        for i := (iCount - 1) downto 0 do
        begin
          xJournalTrade := FTrades[i];
          if xJournalTrade.Side = xSide then
          begin
            var tmpQty := xQty - xJournalTrade.Qty;

            if tmpQty > 0 then
            begin
              xQty := tmpQty;
              xSumValue := xSumValue + xJournalTrade.Value;
            end;

            if tmpQty = 0 then
            begin
              xSumValue := xSumValue + xJournalTrade.Value;
              Break;
            end;

            if tmpQty < 0 then
            begin
              xSumValue := xSumValue + xJournalTrade.Price * xQty;
              Break;
            end;

          end;
        end;
        FPrice := xSumValue/FQty;
      end;
    end;
  end;

begin
  _CalcQty;
  _CalcPrice;
end;

function TJournalPosition.GetIsActive: Boolean;
begin
  Result := not (FQty = 0);
end;


function TJournalPosition.GetQty: Double;
begin
  Result := Abs(FQty);
end;

function TJournalPosition.GetSide: TTypeBuySell;
begin
  if FQty > 0 then
    Result := TTypeBuySell.tsBuy
  else if FQty < 0 then
    Result := TTypeBuySell.tsSell
  else
    Result := TTypeBuySell.tsNull;
end;

function TJournalPosition.GetProfit(const APrice: Double): Double;

  function _ProfitSumm: Double;
  var
    xSum: Double;
  begin
    xSum := 0;
    for var xTrade in FTrades do
    begin
      case xTrade.Side of
        tsBuy: xSum := xSum - xTrade.Value;
        tsSell: xSum := xSum + xTrade.Value;
      end;
    end;
    Result := xSum;
  end;

var
  xProfit: Double;
begin

  if FTrades.Count > 0 then
  begin
    if FQty = 0 then
    begin
      xProfit := _ProfitSumm;
    end
    else if FPrice > 0 then
    begin
      case Self.Side of
        tsBuy: xProfit := _ProfitSumm + APrice * Qty;
        tsSell: xProfit := _ProfitSumm - APrice * Qty;
      else
        xProfit := 0;
      end;
    end
    else
      xProfit := 0;
  end
  else
    xProfit := 0;

  if FMaxProfit < xProfit then
    FMaxProfit := xProfit;

  if FMinProfit > xProfit then
    FMinProfit := xProfit;

  if not SameValue(FProfit,xProfit,0.01) then
  begin
    FProfits.Add(xProfit);
    FProfit := xProfit;
  end;

  Result := xProfit;
end;

procedure TJournalPosition.CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
begin
  Self.OpenTrade(
    ATime,
    APrice,
    Self.Qty,
    GetCrossSide(Self.Side),
    ACandels
  );
end;

{ TJournalManager }

constructor TJournalManager.Create;
begin
  FPositions := TJournalPositionList.Create;
end;

destructor TJournalManager.Destroy;
begin
  FPositions.Clear;
  FreeAndNil(FPositions);
  inherited;
end;

function TJournalManager.GetCurrentPosition: TJournalPosition;
var
  iCount: Integer;
  xPosition: TJournalPosition;
begin
  Result := nil;
  iCount := FPositions.Count;
  if iCount > 0 then
  begin
    xPosition := FPositions[iCount - 1];
    Result := xPosition;
  end;
end;

function TJournalManager.GetIsCurrentPosition: Boolean;
var
  xJournalPosition: TJournalPosition;
begin
  Result := False;
  xJournalPosition := GetCurrentPosition;
  if Assigned(xJournalPosition) then
    Result := xJournalPosition.IsActive;
end;

procedure TJournalManager.OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
var
  xJournalPosition: TJournalPosition;
begin
  if IsCurrentPosition then
  begin
    Self.CurrentPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels)
  end
  else
  begin
    xJournalPosition := TJournalPosition.Create;
    xJournalPosition.Manager := Self;
    FPositions.Add(xJournalPosition);
    xJournalPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels);
  end;
end;

procedure TJournalManager.CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
begin
  if IsCurrentPosition then
    Self.CurrentPosition.CloseTrade(ATime, APrice, ACandels)
end;

procedure TJournalManager.DoNewTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
var
  xJournalPosition: TJournalPosition;
begin
  xJournalPosition := TJournalPosition.Create;
  xJournalPosition.Manager := Self;
  FPositions.Add(xJournalPosition);
  xJournalPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels);
end;

end.