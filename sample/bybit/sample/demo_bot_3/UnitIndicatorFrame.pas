unit UnitIndicatorFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Indicator,
  Lb.Mode;

type
  TEventOnMode = procedure(Sender: TObject; AStartTime: String; APrice, AStopLoss: Double; AMode: TMode) of object;

  TIndicatorFrame = class(TFrame)
    StrGrid: TStringGrid;
    procedure StrGridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
  private
    FOldStartTime: String;
    FEventOnMode: TEventOnMode;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  protected
    procedure DoMode(AStartTime: String; APrice, AStopLoss: Double; AMode: TMode);
  public
    RSI: TRSI;
    ADR: TADR;
    RVI: TRVI;
    CurrentCandel: TCandelObject;
    BybitKline: TBybitKline;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UpDataTimer: Boolean;
    property OnMode: TEventOnMode write FEventOnMode;
  end;

implementation

{$R *.fmx}

{ TIndicatorFrame }

constructor TIndicatorFrame.Create(AOwner: TComponent);

  procedure _SetAddCol(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AStrGrid);
    xCol.Header := AHeader;
    xCol.Parent := AStrGrid;
    xCol.Width := AWidth;
  end;

  procedure _SetHeaders(const AGrid: TStringGrid);
  begin
    _SetAddCol(AGrid,'ID',50);
    _SetAddCol(AGrid,'Time',120);
    _SetAddCol(AGrid,'Open');
    _SetAddCol(AGrid,'High');
    _SetAddCol(AGrid,'Low');
    _SetAddCol(AGrid,'Close');
    _SetAddCol(AGrid,'Vol');
    _SetAddCol(AGrid,'RSI',125);
    _SetAddCol(AGrid,'Avg.RSI',125);
    _SetAddCol(AGrid,'ADR',125);
    _SetAddCol(AGrid,'K%',125);
    _SetAddCol(AGrid,'D%',125);
  end;

begin
  inherited;
  CurrentCandel := nil;

  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
  _SetHeaders(StrGrid);

  RSI := TRSI.Create;
  ADR := TADR.Create;
  RVI := TRVI.Create;
  FOldStartTime := '';
end;

destructor TIndicatorFrame.Destroy;
begin
  FreeAndNil(RVI);
  FreeAndNil(RSI);
  FreeAndNil(ADR);
  FreeAndNil(BybitKline);
  inherited;
end;

function TIndicatorFrame.UpDataTimer: Boolean;
begin
  Result := True;
  try
    BybitKline.Category := TTypeCategory.tcLinear;
    BybitKline.Symbol := 'BTCUSDT';
    BybitKline.Interval := TTypeInterval.ti_15;
    BybitKline.Limit := 150;
    BybitKline.Selected;
  except
    Result := False;
  end;
end;

procedure TIndicatorFrame.BybitKlineOnEventEndLoading(Sender: TObject);

  procedure _Mode;
  var
    xK, xD: Double;
    xCandel: TCandelObject;
  begin
    xCandel := BybitKline.CandelObjects[0];
    xK := RVI.K[0];
    xD := RVI.D[0];
    if xK > xD then
    begin
      // Продать
      DoMode(
        xCandel.startTime,
        xCandel.Close,
        ADR.Values[0],
        TMode.tmBuy
      );
    end
    else if xK < xD then
    begin
      // Купить
      DoMode(
        xCandel.startTime,
        xCandel.Close,
        ADR.Values[0],
        TMode.tmSell
      );
    end;
  end;

var
  xCandel: TCandelObject;
  i, iCount: Integer;
begin
  iCount := BybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    RSI.Period := 14;
    RSI.AvgPeriod := 3;
    RSI.SetCandels(BybitKline.CandelObjects);

    ADR.Period := 15;
    ADR.SetCandels(BybitKline.CandelObjects);

    RVI.Period := 10;
    RVI.PeriodSignal := 3;
    RVI.SetCandels(BybitKline.CandelObjects);

    CurrentCandel := BybitKline.CandelObjects[0];

    StrGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xCandel := BybitKline.CandelObjects[i];
      StrGrid.Cells[0,i]  := (i + 1).ToString;
      StrGrid.Cells[1,i]  := DateTimeToStr(xCandel.DateTime);
      StrGrid.Cells[2,i]  := xCandel.openPrice;
      StrGrid.Cells[3,i]  := xCandel.highPrice;
      StrGrid.Cells[4,i]  := xCandel.lowPrice;
      StrGrid.Cells[5,i]  := xCandel.closePrice;
      StrGrid.Cells[6,i]  := xCandel.volume;
      StrGrid.Cells[7,i]  := RSI.Values[i].ToString;
      StrGrid.Cells[8,i]  := RSI.AvgValues[i].ToString;
      StrGrid.Cells[9,i]  := ADR.Values[i].ToString;
      StrGrid.Cells[10,i] := RVI.K[i].ToString;
      StrGrid.Cells[11,i] := RVI.D[i].ToString;
    end;

    // Напровление
    if not SameText(FOldStartTime,CurrentCandel.startTime) then
      _Mode;
  end
  else
    StrGrid.RowCount := 0;
end;

procedure TIndicatorFrame.DoMode(AStartTime: String; APrice, AStopLoss: Double; AMode: TMode);
begin
  if Assigned(FEventOnMode) then
    FEventOnMode(Self, AStartTime, APrice, AStopLoss, AMode);
end;

procedure TIndicatorFrame.StrGridDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  xValueK, xValueD: Double;
  xRowColor: TBrush;
begin
  if (StrGrid.Columns[10] = Column) or (StrGrid.Columns[11] = Column) then
    if (Row >= 0) and (Row < RVI.K.Count) then
    begin
      xRowColor := TBrush.Create(TBrushKind.Solid,TAlphaColors.Alpha);
      try
        xValueK := RVI.K[Row];
        xValueD := RVI.D[Row];
        if xValueK > xValueD then
          xRowColor.Color := TAlphaColors.Green
        else
          xRowColor.Color := TAlphaColors.Red;
        Canvas.FillRect(Bounds, 0, 0, [], 1, xRowColor);
        Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
      finally
        FreeAndNil(xRowColor);
      end;
    end;
end;

end.
