unit UnitTestMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  Lb.Candel.SysUtils,
  Lb.Candel.Source,
  FMX.Edit,
  BotTrade, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Layouts,
  FMX.Objects,
  System.Generics.Collections;

type
  TLines = TObjectList<TLine>;

  TMainForm = class(TForm)
    ButtonLoad: TButton;
    ButtonStartStop: TButton;
    EditInfo: TEdit;
    Timer: TTimer;
    CandelGrid: TStringGrid;
    PositionGrid: TStringGrid;
    PositionsGrid: TStringGrid;
    LayoutCandels: TLayout;
    RectangleChart: TRectangle;
    LineOpen: TLine;
    LineProfit: TLine;
    LineStop: TLine;
    CheckBox1: TCheckBox;
    Text1: TText;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIndexCandel: Integer;
    procedure StartWork;
    procedure StopWork;
  protected
    Lines: TLines;
    DefaultBot: TDefaultBot;
    procedure ShowGrid;
    procedure ShowCandels;
  public
    SourceCandel: TSourceCandel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses Lb.Logger;

var
  IndexStringColumn: Integer = 0;

  localStep: Integer = 50;
  localProfit: Integer = 100;
  localStop: Integer = 100;
  localMaxValue: Integer = 500;



procedure CreateStringColumn(const ATitle: String; AGrid: TStringGrid);
var
  xColumn: TStringColumn;
begin
  Inc(IndexStringColumn);
  xColumn := TStringColumn.Create(AGrid);
  xColumn.Name := 'column_name_' + IntToStr(IndexStringColumn);
  xColumn.Header := ATitle;
  xColumn.Parent := AGrid;
end;

procedure StringGridColumn(AGrid: TStringGrid; AColumns: TArray<String>);
begin
  for var xTitle in AColumns do
    CreateStringColumn(xTitle,AGrid);
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _GridTitle(const AGrid: TStringGrid);
  begin
    AGrid.RowCount := 0;
    StringGridColumn(AGrid,['OpenDate','OpenTime','CloseDate','CloseTime',
      'Open','Close','Quantity','Direction','Status','Profit','Stop','Result']);
  end;

begin
  inherited Create(AOwner);

  Lines := TLines.Create;

  SourceCandel := TSourceCandel.Create;
  DefaultBot := TDefaultBot.Create;

  CandelGrid.RowCount := 0;
  StringGridColumn(CandelGrid,['Дата','Время','Open','High','Low','Close','Vol']);

  _GridTitle(PositionGrid);
  _GridTitle(PositionsGrid);

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(DefaultBot);
  FreeAndNil(SourceCandel);

  FreeAndNil(Lines);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Тестирование базовой стратегии';
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'source' + PathDelim + 'candel.csv';
  SourceCandel.SetLoadFile(xPath);
  EditInfo.Text := 'Количество свечей: Count: ' + IntToStr(SourceCandel.Candels.Count);
end;

procedure TMainForm.StartWork;
begin
  if SourceCandel.Candels.Count > 0 then
  begin
    FIndexCandel := 0;
    ButtonStartStop.Text := 'Стоп';
    Timer.Enabled := True;

    Text1.Text := 'Profit: ' + IntToStr(localProfit) + '; Stop: ' + IntToStr(localStop);
    DefaultBot.Init(
      localProfit,
      localStop
    );
  end;
end;

procedure TMainForm.StopWork;
begin
  ButtonStartStop.Text := 'Старт';
  Timer.Enabled := False;

  // Подведение итогов

  if CheckBox1.IsChecked then
  begin
    var xResult := DefaultBot.GetProfitResult;
    if xResult > 0 then
    begin
      var xS := IntToStr(localProfit) + ';' + IntToStr(localStop) + ';' + FloatToStr(DefaultBot.GetProfitResult) + ';';
      TLogger.LogText(xS);
    end;

    if localStop < localMaxValue then
    begin
      localStop := localStop + localStep;
    end
    else
    begin
      localProfit := localProfit + localStep;
      localStop   := localStep;
    end;

    if localProfit < localMaxValue  then
      StartWork;

  end;
end;

procedure TMainForm.ButtonStartStopClick(Sender: TObject);
begin
  if Timer.Enabled then
    StopWork
  else
    StartWork;
end;

procedure TMainForm.ShowCandels;
const
  STEP_VALUE = 200;

  procedure _GetMaxAndMinValue(var AValueMax, AValueMin: Double);
  begin
    AValueMax := 0;
    AValueMin := 0;
    if DefaultBot.Position.Profit > DefaultBot.Position.Stop then
    begin
      AValueMax := DefaultBot.Position.Profit + STEP_VALUE;
      AValueMin := DefaultBot.Position.Stop - STEP_VALUE;
    end
    else if DefaultBot.Position.Profit < DefaultBot.Position.Stop then
    begin
      AValueMin := DefaultBot.Position.Profit - STEP_VALUE;
      AValueMax := DefaultBot.Position.Stop + STEP_VALUE;
    end;
  end;

  function _ControlY(const AHeight: Single; const APrice, AValueMax, AValueMin: Double): Double;
  var
    xDelta: Double;
  begin
    Result := 0;
    if (AHeight > 0) and (AValueMax <> AValueMin) then
    begin
      xDelta := (AValueMax - AValueMin)/AHeight;
      Result := AHeight - (APrice - AValueMin)/xDelta;
    end;
  end;


  procedure _BarLine(AValueMax, AValueMin: Double);
  var
    xLine: TLine;
    xCandel: TCandel;
    i, iCount: Integer;
    xX, xY, xW, xH: Single;
  begin
    Lines.Clear;
    iCount := DefaultBot.Candels.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xCandel := DefaultBot.Candels.Items[i];

        xLine := TLine.Create(Self);
        xLine.LineType := TLineType.Left;
        xLine.Parent := RectangleChart;
        xLine.Stroke.Color := TAlphaColorRec.Blue;
        xLine.Stroke.Thickness := 2;

        xX := (i + 1) * 2;
        xY := _ControlY(RectangleChart.Height, xCandel.High, AValueMax, AValueMin);
        xW := 2;
        xH := _ControlY(RectangleChart.Height, xCandel.Low, AValueMax, AValueMin) - xY;
        xLine.SetBounds(xX,xY,xW,xH);

        Lines.Add(xLine);
      end;
    end;
  end;

var
  xX, xY, xW, xH: Single;
  xValueMax, xValueMin: Double;
begin
  _GetMaxAndMinValue(xValueMax, xValueMin);

  // Линия профит
  xX := 0;
  xY := _ControlY(RectangleChart.Height, DefaultBot.Position.Profit, xValueMax, xValueMin);
  xW := RectangleChart.Width;
  xH := 2;
  LineProfit.SetBounds(xX,xY,xW,xH);

  // Линия стоп
  xX := 0;
  xY := _ControlY(RectangleChart.Height, DefaultBot.Position.Stop, xValueMax, xValueMin);
  xW := RectangleChart.Width;
  xH := 2;
  LineStop.SetBounds(xX,xY,xW,xH);

  // Линия открытие
  xX := 0;
  xY := _ControlY(RectangleChart.Height, DefaultBot.Position.Open, xValueMax, xValueMin);
  xW := RectangleChart.Width;
  xH := 2;
  LineOpen.SetBounds(xX,xY,xW,xH);

  _BarLine(xValueMax, xValueMin);
end;

procedure TMainForm.ShowGrid;
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  iCount := DefaultBot.Candels.Count;
  if iCount > 0 then
  begin
    CandelGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xCandel := DefaultBot.Candels.Items[i];
      CandelGrid.Cells[0,i] := DateToStr(xCandel.Date);
      CandelGrid.Cells[1,i] := TimeToStr(xCandel.Time);
      CandelGrid.Cells[2,i] := FloatToStr(xCandel.Open);
      CandelGrid.Cells[3,i] := FloatToStr(xCandel.High);
      CandelGrid.Cells[4,i] := FloatToStr(xCandel.Low);
      CandelGrid.Cells[5,i] := FloatToStr(xCandel.Close);
      CandelGrid.Cells[6,i] := FloatToStr(xCandel.Vol);
    end;
  end
  else
    CandelGrid.RowCount := 0;
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _CurrentShowGrid;
  var
    xTradePosition: TTradePosition;
  begin
    PositionGrid.RowCount := 1;
    xTradePosition := DefaultBot.Position;
    PositionGrid.Cells[0,0] := DateToStr(xTradePosition.OpenDate);
    PositionGrid.Cells[1,0] := TimeToStr(xTradePosition.OpenTime);
    PositionGrid.Cells[2,0] := DateToStr(xTradePosition.CloseDate);
    PositionGrid.Cells[3,0] := TimeToStr(xTradePosition.CloseTime);
    PositionGrid.Cells[4,0] := FloatToStr(xTradePosition.Open);
    PositionGrid.Cells[5,0] := FloatToStr(xTradePosition.Close);
    PositionGrid.Cells[6,0] := IntToStr(xTradePosition.Quantity);
    PositionGrid.Cells[7,0] := GetDirectionToStr(xTradePosition.Direction);
    PositionGrid.Cells[8,0] := GetStatusToStr(xTradePosition.Status);
    PositionGrid.Cells[9,0] := FloatToStr(xTradePosition.Profit);
    PositionGrid.Cells[10,0] := FloatToStr(xTradePosition.Stop);
    PositionGrid.Cells[11,0] := FloatToStr(xTradePosition.GetProfitPerUnit);
  end;

  procedure _ShowGrid;
  var
    xTradePosition: TTradePosition;
  begin
    var xInd := 0;
    PositionsGrid.RowCount := DefaultBot.Positions.Count;
    for xTradePosition in DefaultBot.Positions do
    begin
      with PositionsGrid do
      begin
        Cells[0,xInd] := DateToStr(xTradePosition.OpenDate);
        Cells[1,xInd] := TimeToStr(xTradePosition.OpenTime);
        Cells[2,xInd] := DateToStr(xTradePosition.CloseDate);
        Cells[3,xInd] := TimeToStr(xTradePosition.CloseTime);
        Cells[4,xInd] := FloatToStr(xTradePosition.Open);
        Cells[5,xInd] := FloatToStr(xTradePosition.Close);
        Cells[6,xInd] := IntToStr(xTradePosition.Quantity);
        Cells[7,xInd] := GetDirectionToStr(xTradePosition.Direction);
        Cells[8,xInd] := GetStatusToStr(xTradePosition.Status);
        Cells[9,xInd] := FloatToStr(xTradePosition.Profit);
        Cells[10,xInd] := FloatToStr(xTradePosition.Stop);
        Cells[11,xInd] := FloatToStr(xTradePosition.GetProfitPerUnit);
      end;
      Inc(xInd);
    end;
  end;

  procedure _NextCandel;
  begin
    Inc(FIndexCandel);
    if FIndexCandel >= SourceCandel.Candels.Count then
    begin
      DefaultBot.ExecutionStop;
      StopWork;
    end;
  end;

  function _Progress: String;
  begin
    var Value := Trunc((FIndexCandel/SourceCandel.Candels.Count) * 100);
    Result := '[' + IntToStr(Value) + ']';
  end;

var
  xCandel: TCandel;
begin
  try
    if FIndexCandel >= 187 then
    begin
      with TStringList.Create do
        Free;
    end;

    xCandel := SourceCandel.Candels[FIndexCandel];
    EditInfo.Text := _Progress + xCandel.ToString;

    DefaultBot.ExecutionLastCandel(xCandel);
    ShowGrid;
    ShowCandels;

    _NextCandel;

    _CurrentShowGrid;
    _ShowGrid;
  except
    StopWork;
  end;
end;


end.
