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
  Lb.Indicator,
  Lb.SaveHistory;

type
  TMainForm = class(TForm)
    ButtonSS: TButton;
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonSSClick(Sender: TObject);
  private
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  public
    RSI: TRSI;
    Momentum: TMomentum;
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
    _SetAddCol(AGrid,'Momentum',125);
  end;

begin
  inherited;
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
  _SetHeaders(StrGrid);

  RSI := TRSI.Create;
  Momentum := TMomentum.Create;
end;

destructor TMainForm.Destroy;
begin

  FreeAndNil(Momentum);
  FreeAndNil(RSI);
  FreeAndNil(BybitKline);
  inherited;
end;


procedure TMainForm.BybitKlineParam;
begin
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol := 'ETHUSDT';
  BybitKline.Interval := TTypeInterval.ti_5;
  BybitKline.Limit := 1000;
  BybitKline.Start(1000);
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
  xSource: TStrings;
var
  xS: String;
  xFileName: String;
  xCandel: TCandelObject;
  i, iCount: Integer;
begin
  xSource := TStringList.Create;

  iCount := BybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    RSI.Period    := 14;
    RSI.AvgPeriod := 3;
    RSI.SetCandels(BybitKline.CandelObjects);

    Momentum.Period := 14;
    Momentum.SetCandels(BybitKline.CandelObjects);

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
      StrGrid.Cells[9,i]  := Momentum.Values[i].ToString;

      xS := '';
      xS := xS + (i + 1).ToString + ';';
      xS := xS + DateTimeToStr(xCandel.DateTime) + ';';
      xS := xS + xCandel.Open.ToString + ';';
      xS := xS + xCandel.High.ToString + ';';
      xS := xS + xCandel.Low.ToString + ';';
      xS := xS + xCandel.Close.ToString + ';';
      xS := xS + xCandel.Vol.ToString + ';';
      xS := xS + RSI.Values[i].ToString + ';';
      xS := xS + RSI.AvgValues[i].ToString + ';';
      xS := xS + Momentum.Values[i].ToString + ';';

      xSource.Add(xS);
    end;

    xFileName := ExtractFilePath(ParamStr(0)) + 'candel.csv';
    xSource.SaveToFile(xFileName);

    Timer.Enabled := False;
  end
  else
    StrGrid.RowCount := 0;

  FreeAndNil(xSource);
end;

end.
