unit UnitCharTradeFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  Lb.JournalTrades;

type
  TTypeLinePosition = (lpTop, lpBottom, lpOpen);
  TOnEventDecision = procedure(Sender: TObject; APrice: Double; ALinePosition: TTypeLinePosition) of object;

  ///<summary>����������� ������������� ������</summary>
  TCharTradeFrame = class(TFrame)
    RectanglePrice: TRectangle;
    GridPanelLayout: TGridPanelLayout;
    TextOpenPrice: TText;
    TextUpDatePrice: TText;
    Rectangle: TRectangle;
    RectangleChart: TRectangle;
    RectangleText: TRectangle;
    LineTop: TLine;
    LineBottom: TLine;
    LineOpen: TLine;
  private
    FLimitValue: Integer;
    FDecisionValue: Integer;
  private
    FStepPrice: Double; // ��� ����
    FOpenPrice: Double; // ���� �������� ������
    FCurrentPrice: Double;
    FMaxPrice: Double;  // ������������ ����
    FMinPrice: Double;  // ����������� ����
    FBuySell: Char;
    function GetMaxPrice(const APrice: Double): Double;
    function GetMinPrice(const APrice: Double): Double;
    function GetPositionToPrice(const APrice: Double): Double;
    procedure SetPositionOpenPrice(const APrice: Double);
    procedure SetPositionUpDatePrice(const APrice: Double);
    procedure SetPositionRectanglePrice(const APriceTop, APriceBottom: Double);
    function GetDelta: Double;
  private
    procedure LineInitilization;
    procedure PriceLinePosition(const APrice: Double; ALinePosition: TTypeLinePosition);
  protected
    FOnLimitPrice: TOnEventDecision;
    procedure DoDecisionPrice;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>���� �������� ������</summary>
    ///<param name="APrice">���� ������</param>
    ///<param name="ABuySell">����������� ������</param>
    ///<param name="AStepDelta">��� �����������</param>
    procedure Open(const APrice: Double; const ABuySell: Char; const AStepPrice: Double);
    ///<summary>���������� ��������� ������</summary>
    procedure UpDate(const APrice: Double);
    ///<summary>������� ����� ����� �������� � ��������</summary>
    property Delta: Double read GetDelta;
    ///<summary>��������� �������� �������</summary>
    ///<remarks>������� �� ����������</remarks>
    property OnEventDecision: TOnEventDecision write FOnLimitPrice;
  public {���������� ������� ��������� ����}
    property LimitValue: Integer read FLimitValue write FLimitValue;
    property DecisionValue: Integer read FDecisionValue write FDecisionValue;
  end;

implementation

{$R *.fmx}

{ TCharTradeFrame }

constructor TCharTradeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLimitValue := 100;
  FDecisionValue := 20;
  FStepPrice := 0.001;

  TextOpenPrice.Visible := False;
  TextUpDatePrice.Visible := False;
  RectanglePrice.Visible := False;
  LineInitilization;
end;

destructor TCharTradeFrame.Destroy;
begin
  inherited;
end;


procedure TCharTradeFrame.LineInitilization;

  procedure _Line(const ALine: TLine);
  begin
    ALine.Visible := False;
    ALine.Stroke.Thickness := 2;
    ALine.Height := 2;
  end;

begin
  _Line(LineTop);
  _Line(LineBottom);


  LineTop.Stroke.Color := TAlphaColorRec.Blue;
  LineBottom.Stroke.Color := TAlphaColorRec.Blue;

  _Line(LineOpen);
end;

function TCharTradeFrame.GetDelta: Double;
begin
  Result := 0;
  case FBuySell of
    'B' : Result := FCurrentPrice - FOpenPrice;
    'S' : Result := FOpenPrice - FCurrentPrice;
  end;
end;

function TCharTradeFrame.GetPositionToPrice(const APrice: Double): Double;
begin
  Result := Self.Height * (1 - (APrice - FMinPrice) /(FMaxPrice - FMinPrice));
end;

function TCharTradeFrame.GetMaxPrice(const APrice: Double): Double;
begin
  // ��������� ������� ������
  Result := APrice +  FLimitValue * FStepPrice;
end;

function TCharTradeFrame.GetMinPrice(const APrice: Double): Double;
begin
  // ��������� ������ ������ ����
  Result := APrice - FLimitValue * FStepPrice;
end;

procedure TCharTradeFrame.Open(const APrice: Double; const ABuySell: Char; const AStepPrice: Double);
begin
  FStepPrice := AStepPrice;

  FOpenPrice := APrice;
  FBuySell := ABuySell;

  {todo: ���� �������� �������}
  FMaxPrice := GetMaxPrice(FOpenPrice);
  FMinPrice := GetMinPrice(FOpenPrice);

  PriceLinePosition(FOpenPrice,TTypeLinePosition.lpOpen);
  PriceLinePosition(FOpenPrice + FDecisionValue * FStepPrice, TTypeLinePosition.lpTop);
  PriceLinePosition(FOpenPrice - FDecisionValue * FStepPrice, TTypeLinePosition.lpBottom);
end;


procedure TCharTradeFrame.SetPositionRectanglePrice(const APriceTop, APriceBottom: Double);
var
  xTopY, xBottomY: Double;
begin
  xTopY    := GetPositionToPrice(APriceTop);
  xBottomY := GetPositionToPrice(APriceBottom);

  RectanglePrice.SetBounds(
    0,
    xTopY,
    RectangleChart.Width,
    (xBottomY - xTopY)
  );

  RectanglePrice.Visible := True;
end;

procedure TCharTradeFrame.SetPositionOpenPrice(const APrice: Double);
var
  xPriceY: Double;
begin
  xPriceY := GetPositionToPrice(APrice) - 21/2;
  TextOpenPrice.Text := APrice.ToString;

  TextOpenPrice.SetBounds(
    5,
    xPriceY,
    RectangleText.Width,
    21
  );

  TextOpenPrice.Visible := True;
end;

procedure TCharTradeFrame.SetPositionUpDatePrice(const APrice: Double);
var
  xPriceY: Double;
begin
  xPriceY := GetPositionToPrice(APrice) - 21/2;
  TextUpDatePrice.Text := APrice.ToString;

  TextUpDatePrice.SetBounds(
    5,
    xPriceY,
    RectangleText.Width,
    21
  );
  TextUpDatePrice.Visible := True;
end;

procedure TCharTradeFrame.PriceLinePosition(const APrice: Double; ALinePosition: TTypeLinePosition);

  procedure _LinePosition(const APrice: Double; const ALine: TLine);
  var
    xPriceY: Double;
  begin
    xPriceY := GetPositionToPrice(APrice) - 1;

    ALine.SetBounds(
      0,
      xPriceY,
      RectangleChart.Width,
      2
    );

    ALine.Visible := True;
  end;

begin
  case ALinePosition of
    lpTop   : _LinePosition(APrice,LineTop);
    lpBottom: _LinePosition(APrice,LineBottom);
    lpOpen  : _LinePosition(APrice,LineOpen);
  end;
end;

procedure TCharTradeFrame.UpDate(const APrice: Double);
var
  xPriceTop, xPriceBottom: Double;
begin
  FCurrentPrice := APrice;

  SetPositionOpenPrice(FOpenPrice);
  SetPositionUpDatePrice(APrice);


  if FOpenPrice > APrice then
  begin
    xPriceTop := FOpenPrice;
    xPriceBottom := APrice;

    // ���� �����������
    case FBuySell of
      'B': RectanglePrice.Fill.Color := TAlphaColorRec.Red;
      'S': RectanglePrice.Fill.Color := TAlphaColorRec.Green;
    end;

    SetPositionRectanglePrice(xPriceTop, xPriceBottom);
  end else if FOpenPrice < APrice then
  begin
    xPriceTop := APrice;
    xPriceBottom := FOpenPrice;

    // ���� �����������
    case FBuySell of
      'B': RectanglePrice.Fill.Color := TAlphaColorRec.Green;
      'S': RectanglePrice.Fill.Color := TAlphaColorRec.Red;
    end;

    SetPositionRectanglePrice(xPriceTop, xPriceBottom);
  end else
  begin
    TextOpenPrice.Visible := False;
    TextUpDatePrice.Visible := False;
    RectanglePrice.Visible := False;
  end;

  DoDecisionPrice;
//  DoLimitProfit;
end;

procedure TCharTradeFrame.DoDecisionPrice;
var
  xMaxP, xMinP: Double;
begin
  {todo: ������ ������� ������}
  xMaxP := FOpenPrice + FDecisionValue * FStepPrice;
  xMinP := FOpenPrice - FDecisionValue * FStepPrice;
  if (FCurrentPrice > xMaxP) then
  begin
    // ----------------------------
    // ����������� �������� �������
    // ������ �� ����������
    FOpenPrice := xMaxP;
    FMaxPrice := GetMaxPrice(FOpenPrice);
    FMinPrice := GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self,xMaxP,TTypeLinePosition.lpTop);
  end else if (FCurrentPrice < xMinP) then
  begin
    // ---------------------------
    // ����������� ������� �������
    // ������ �� ����������
    FOpenPrice := xMinP;
    FMaxPrice := GetMaxPrice(FOpenPrice);
    FMinPrice := GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self,xMinP,TTypeLinePosition.lpBottom);
  end;
end;

end.
