unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline, FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,
  Lb.Indicator;

type
  TMainForm = class(TForm)
    ButtonSS: TButton;
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonSSClick(Sender: TObject);
    procedure StrGridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
  private
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  public
    RSI: TRSI;
    ADR: TADR;
    RVI: TRVI;
    Stochastic: TStochastic;
    BybitKline: TBybitKline;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BybitKlineParam;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

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
    _SetAddCol(AGrid,'stoc.K%');
    _SetAddCol(AGrid,'stoc.D%');
  end;

begin
  inherited;
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
  _SetHeaders(StrGrid);

  RSI := TRSI.Create;
  ADR := TADR.Create;
  RVI := TRVI.Create;
  Stochastic := TStochastic.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Stochastic);
  FreeAndNil(RVI);
  FreeAndNil(RSI);
  FreeAndNil(ADR);
  FreeAndNil(BybitKline);
  inherited;
end;


procedure TMainForm.BybitKlineParam;
begin
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_15;
  BybitKline.Limit := 1000;
  BybitKline.Selected;
end;

procedure TMainForm.ButtonSSClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    ButtonSS.Text := 'Стоп';
    Timer.Enabled := False;
  end
  else
  begin
    ButtonSS.Text := 'Старт';
    Timer.Enabled := True;
    BybitKlineParam;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  BybitKlineParam;
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xCandel: TCandelObject;
  i, iCount: Integer;
begin
  iCount := BybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    RSI.Period    := 14;
    RSI.AvgPeriod := 3;
    RSI.SetCandels(BybitKline.CandelObjects);

    ADR.Period := 15;
    ADR.SetCandels(BybitKline.CandelObjects);

    RVI.Period := 25;
    RVI.PeriodSignal := 5;
    RVI.SetCandels(BybitKline.CandelObjects);

    Stochastic.SetCandels(BybitKline.CandelObjects);

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
      StrGrid.Cells[12,i] := Stochastic.K[i].ToString;
      StrGrid.Cells[13,i] := Stochastic.D[i].ToString;
    end;
  end
  else
    StrGrid.RowCount := 0;
end;

procedure TMainForm.StrGridDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  xValue, xValueK, xValueD: Double;
  xRowColor: TBrush;
begin

  if StrGrid.Columns[8] = Column then
    if (Row >= 0) and (Row < RSI.AvgValues.Count) then
    begin
      xRowColor := TBrush.Create(TBrushKind.Solid,TAlphaColors.Alpha);
      try
        xValue := RSI.AvgValues[Row];
        if xValue > 50 then
          xRowColor.Color := TAlphaColors.Green
        else
          xRowColor.Color := TAlphaColors.Red;
        Canvas.FillRect(Bounds, 0, 0, [], 1, xRowColor);
        Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
      finally
        FreeAndNil(xRowColor);
      end;
    end;

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

  if (StrGrid.Columns[12] = Column) or (StrGrid.Columns[13] = Column) then
    if (Row >= 0) and (Row < Stochastic.K.Count) then
    begin
      xRowColor := TBrush.Create(TBrushKind.Solid,TAlphaColors.Alpha);
      try
        xValueK := Stochastic.K[Row];

        if xValueK < 30 then
          xRowColor.Color := TAlphaColors.Green
        else if xValueK > 70 then
          xRowColor.Color := TAlphaColors.Red
        else
          xRowColor.Color := TAlphaColors.White;

        Canvas.FillRect(Bounds, 0, 0, [], 1, xRowColor);
        Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
      finally
        FreeAndNil(xRowColor);
      end;
    end;

  if (StrGrid.Columns[13] = Column) then
    if (Row >= 0) and (Row < Stochastic.K.Count) then
    begin
      xRowColor := TBrush.Create(TBrushKind.Solid,TAlphaColors.Alpha);
      try
        xValueD := Stochastic.D[Row];
        if xValueD < 30 then
          xRowColor.Color := TAlphaColors.Green
        else if xValueD > 70 then
          xRowColor.Color := TAlphaColors.Red
        else
          xRowColor.Color := TAlphaColors.White;

        Canvas.FillRect(Bounds, 0, 0, [], 1, xRowColor);
        Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
      finally
        FreeAndNil(xRowColor);
      end;
    end;

end;


end.
