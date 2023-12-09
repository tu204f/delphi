(******************************************************************************)
(* Вариант до работки приложения, предоставление графика из цены              *)
(******************************************************************************)
unit UnitChartFrame;

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
  Lb.SysUtils.Candel,
  UnitCandelFrame, FMX.Layouts;

type
  ///<summary>Список свечей</summary>
  TCandelFrameList = TObjectList<TCandelFrame>;

  ///<summary>Кастом форма для программирования</summary>
  TChartFrame = class(TFrame)
    Timer: TTimer;
    LayoutChart: TLayout;
    LayoutPositionTrade: TLayout;
    LineSupport: TLine;
    LineResistance: TLine;
    LineOpen: TLine;
    LineClose: TLine;
    RectangleBody: TRectangle;
    LineStop: TLine;
    procedure TimerTimer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FMaxPrice, FMinPrice: Double;
    FIsUpData: Boolean;
    FCandels: TCandels;
    FCandelFrames: TCandelFrameList;
    FCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetLimit(var AMaxPrice, AMinPrice: Double);
  protected
    property MaxPrice: Double read FMaxPrice;
    property MinPrice: Double read FMinPrice;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build;
    property Candels: TCandels read FCandels;
    property Capacity: Integer read FCapacity write SetCapacity;
  public
    ///<summary>Поддержка и сопротивление</summary>
    procedure SupportAndResistance(const ASupport, AResistance: Double);
    ///<summary>Цена открытие</summary>
    procedure OpenAndClosePrice(const AOpen, AClose, AStop: Double; const ABuySell: Char);
  end;

implementation

{$R *.fmx}

uses Lb.SysUtils;

{ TChartFrame }

constructor TChartFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCandelFrames := TCandelFrameList.Create;
  FCandels := TCandels.Create;
  FIsUpData := False;

  LineSupport.Visible := False;
  LineResistance.Visible := False;

  LineOpen.Visible := False;
  LineClose.Visible := False;
  LineStop.Visible := False;

  RectangleBody.Visible := False;
end;

destructor TChartFrame.Destroy;
begin
  FreeAndNil(FCandels);
  FreeAndNil(FCandelFrames);
  inherited;
end;

procedure TChartFrame.FrameResize(Sender: TObject);
begin
  // Изменились размеры формы
  FIsUpData := True;
end;

procedure TChartFrame.SetCapacity(const Value: Integer);
var
  xCandelFrame: TCandelFrame;
begin
  FCapacity := Value;
  FCandelFrames.Clear;
  if FCapacity > 0 then
    for var i := 0 to FCapacity - 1 do
    begin
      xCandelFrame := TCandelFrame.Create(nil);
      xCandelFrame.Parent := LayoutChart;
      FCandelFrames.Add(xCandelFrame);
    end;
end;

procedure TChartFrame.SetLimit(var AMaxPrice, AMinPrice: Double);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  AMaxPrice := 0;
  AMinPrice := 0;
  iCount := FCandels.Count;
  if FCapacity < iCount then
    iCount := FCapacity;
  if iCount > 0 then
  begin
    xCandel := FCandels[0];
    AMaxPrice := xCandel.High;
    AMinPrice := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FCandels[i];
      if xCandel.High > AMaxPrice then
        AMaxPrice := xCandel.High;
      if xCandel.Low < AMinPrice then
        AMinPrice := xCandel.Low;
    end;
  end;
end;

procedure TChartFrame.TimerTimer(Sender: TObject);

  {todo: Вынести в само стоятельную процедуру}
  procedure _Build;
  var
    i, iCount: Integer;
    xDeltaW: Double;
    xCandelFrame: TCandelFrame;
    xX, xY, xW, xH: Double;
    xCandel: TCandel;
  begin
    // Постоянная перерисовка графиика
    iCount := FCandelFrames.Count;
    if iCount > 0 then
    begin
      xDeltaW := LayoutChart.Width/(iCount + 1);
      for i := 0 to iCount - 1 do
      begin
        xCandelFrame := FCandelFrames[i];
        if (FCandels.Count > 0) and (i < FCandels.Count) then
        begin
          xCandel := FCandels[i];
          xCandelFrame.MaxPrice := FMaxPrice;
          xCandelFrame.MinPrice := FMinPrice;
          xCandelFrame.Candel := xCandel;
          xCandelFrame.Build;
        end;
        xX := (iCount - i) * xDeltaW;
        xY := 0;
        xW := xDeltaW;
        xH := LayoutChart.Height;
        xCandelFrame.SetBounds(xX, xY, xW, xH);
      end;
    end;
  end;

begin
  if FIsUpData then
  begin
    _Build;
    FIsUpData := False;
  end;
end;

procedure TChartFrame.Build;
begin
  FIsUpData := True;
  SetLimit(FMaxPrice, FMinPrice);
end;

procedure TChartFrame.SupportAndResistance(const ASupport, AResistance: Double);

  function _WidthLine: Double;
  begin
    Result := LayoutChart.Width/3;
  end;

  procedure _Line(const ALine: TLine; const APrice: Double);
  var
    xX, xY, xWidth: Double;
  begin
    if not ALine.Visible then
      ALine.Visible := True;

    xWidth := _WidthLine;

    ALine.BringToFront;
    ALine.Width := xWidth;
    xY := PriceToY(LayoutChart.Height,APrice,FMaxPrice,FMinPrice);
    xX := LayoutChart.Width - ALine.Width;
    ALine.SetBounds(
      xX,
      xY,
      xWidth,
      2
    );
  end;

begin
  LineOpen.Visible := False;
  LineClose.Visible := False;
  LineStop.Visible := False;
  RectangleBody.Visible := False;

  _Line(LineSupport,ASupport);
  _Line(LineResistance,AResistance);
end;


procedure TChartFrame.OpenAndClosePrice(const AOpen, AClose, AStop: Double; const ABuySell: Char);

  function _WidthLine: Double;
  begin
    Result := LayoutChart.Width/3;
  end;

  procedure _Line(const ALine: TLine; const APrice: Double);
  var
    xX, xY, xWidth: Double;
  begin
    if not ALine.Visible then
      ALine.Visible := True;

    xWidth := _WidthLine;

    ALine.BringToFront;
    ALine.Width := xWidth;
    xY := PriceToY(LayoutChart.Height,APrice,FMaxPrice,FMinPrice);
    xX := LayoutChart.Width - ALine.Width;
    ALine.SetBounds(
      xX,
      xY,
      xWidth,
      2
    );
  end;

  function _Max(const AValue1, AValue2: Double): Double;
  begin
    if AValue1 > AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function _Min(const AValue1, AValue2: Double): Double;
  begin
    if AValue1 < AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  procedure _RectangleBody(const AOpen, AClose: Double; const ABuySell: Char);
  var
    xX1, xY1, xY2: Double;
    xTopPrice, xBottomPrice: Double;
  begin
    RectangleBody.Visible := True;

    case ABuySell of
      'B': begin
        if AOpen < AClose then
          RectangleBody.Fill.Color := TAlphaColorRec.Green
        else
          RectangleBody.Fill.Color := TAlphaColorRec.Red
      end;
      'S': begin
        if AOpen < AClose then
          RectangleBody.Fill.Color := TAlphaColorRec.Red
        else
          RectangleBody.Fill.Color := TAlphaColorRec.Green
      end;
    end;

    xTopPrice    := _Max(AOpen,AClose);
    xBottomPrice := _Min(AOpen,AClose);

    xX1 := 5;
    xY1 := PriceToY(LayoutChart.Height,xTopPrice,   FMaxPrice,FMinPrice);
    xY2 := PriceToY(LayoutChart.Height,xBottomPrice,FMaxPrice,FMinPrice);

    RectangleBody.SetBounds(
      xX1,
      xY1,
      LayoutPositionTrade.Width - 10,
      xY2 - xY1
    );

  end;

begin
  LineSupport.Visible := False;
  LineResistance.Visible := False;

  _Line(LineOpen,AOpen);
  _Line(LineClose,AClose);
  _Line(LineStop,AStop);

  _RectangleBody(AOpen, AClose, ABuySell);
end;


end.
